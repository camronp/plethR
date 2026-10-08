# Lung health score and treatment efficacy.
#
# The lung health score summarizes how far an animal's breathing is from healthy
# controls, in the direction disease moves each parameter:
#   1. Each parameter is standardized against healthy animals at the same
#      timepoint (or, with reference = "baseline", each animal's change from its
#      own baseline is compared with the change in healthy animals).
#   2. The direction of each parameter is learned from untreated diseased animals.
#   3. The score is the mean signed z-score: 0 = like healthy controls, higher =
#      more disease-like.
# Treated animals are never used to build the score, and each untreated diseased
# animal is scored with directions learned from the other diseased animals, so the
# disease group's score is not inflated (which would make any treatment look good).

`%||%` <- function(a, b) if (is.null(a)) b else a

# Standardized values (sessions x parameters) relative to healthy animals.
lung_z <- function(sessions, healthy, parameters, reference, baseline_timepoint) {
  s <- sessions
  vals <- as.matrix(s[, parameters, drop = FALSE])
  if (reference == "baseline") {
    if (is.null(baseline_timepoint)) baseline_timepoint <- levels(s$timepoint)[1]
    base <- s[s$timepoint == baseline_timepoint, c("subject", parameters), drop = FALSE]
    b <- as.matrix(base[match(s$subject, base$subject), parameters, drop = FALSE])
    vals <- vals - b
  }
  is_h <- as.character(s$group) %in% healthy
  z <- vals
  for (j in seq_along(parameters)) {
    v <- vals[, j]
    mu <- tapply(v[is_h], s$timepoint[is_h], mean, na.rm = TRUE)
    dev <- v[is_h] - mu[as.character(s$timepoint[is_h])]
    sdp <- stats::sd(dev, na.rm = TRUE) * sqrt((sum(!is.na(dev)) - 1) / max(1, sum(!is.na(dev)) - length(mu)))
    center <- mu[as.character(s$timepoint)]
    z[, j] <- if (is.finite(sdp) && sdp > 0) (v - center) / sdp else NA_real_
  }
  colnames(z) <- parameters
  z
}

# Direction and size of the disease effect per parameter, from a set of rows.
lung_weights <- function(z, rows, min_effect) {
  eff <- colMeans(z[rows, , drop = FALSE], na.rm = TRUE)
  data.frame(parameter = colnames(z), effect = eff, direction = sign(eff),
             used = !is.na(eff) & abs(eff) >= min_effect & eff != 0, stringsAsFactors = FALSE)
}

lung_apply <- function(z, rows, w) {
  if (!any(w$used)) return(rep(NA_real_, length(rows)))
  zz <- sweep(z[rows, w$used, drop = FALSE], 2, w$direction[w$used], `*`)
  rowMeans(zz, na.rm = TRUE)
}

#' Lung Health Score
#'
#' Combines the chosen respiratory parameters into one score per animal and
#' session: 0 means like healthy controls, higher means more like untreated
#' diseased animals. See Details for how bias is avoided.
#'
#' Each parameter is standardized against healthy animals at the same timepoint
#' (`reference = "healthy"`), or each animal's change from its own baseline is
#' compared with the change in healthy animals (`reference = "baseline"`, which
#' also removes differences between animals that existed before disease). The
#' direction of each parameter is learned from untreated diseased animals after
#' disease onset. Each untreated diseased animal is scored with directions learned
#' from the other diseased animals (leave-one-out), so their scores are not
#' inflated. Treated animals are never used to build the score.
#'
#' @param sessions Output of [summarize_sessions()] with groups assigned.
#' @param healthy Healthy control groups.
#' @param disease Untreated diseased groups.
#' @param parameters Parameters that define lung health. Defaults to all.
#' @param reference `"healthy"` or `"baseline"` (see Details).
#' @param baseline_timepoint Baseline for `reference = "baseline"`. Defaults to the first timepoint.
#' @param onset_timepoint First timepoint at which disease is expected (used to learn
#'   directions). Defaults to the second timepoint.
#' @param min_effect Only use parameters whose disease effect is at least this many
#'   healthy standard deviations (0 = use all chosen parameters).
#' @return `sessions` with a `lung_score` column (attribute `"parameters"` set to
#'   `"lung_score"`, so it works with [summarize_groups()] and [plot_timecourse()])
#'   and attribute `"lung_weights"` describing each parameter.
#' @export
lung_health_score <- function(sessions, healthy, disease, parameters = NULL, reference = c("healthy", "baseline"),
                              baseline_timepoint = NULL, onset_timepoint = NULL, min_effect = 0) {
  reference <- match.arg(reference)
  if (is.null(parameters)) parameters <- attr(sessions, "parameters")
  if (length(healthy) == 0 || length(disease) == 0) stop("Choose healthy and untreated disease groups.")
  if (length(intersect(healthy, disease))) stop("A group cannot be both healthy and diseased.")
  lv <- levels(sessions$timepoint)
  if (is.null(onset_timepoint)) onset_timepoint <- lv[min(2, length(lv))]
  z <- lung_z(sessions, healthy, parameters, reference, baseline_timepoint)
  post <- match(as.character(sessions$timepoint), lv) >= match(onset_timepoint, lv)
  is_d <- as.character(sessions$group) %in% disease
  learn <- which(is_d & post)
  w <- lung_weights(z, learn, min_effect)

  score <- rep(NA_real_, nrow(sessions))
  others <- which(!is_d)
  score[others] <- lung_apply(z, others, w)
  for (a in unique(sessions$subject[is_d])) {
    rows <- which(sessions$subject == a)
    w_loo <- lung_weights(z, setdiff(learn, rows), min_effect)
    score[rows] <- lung_apply(z, rows, w_loo)
  }
  out <- sessions
  out$lung_score <- score
  attr(out, "parameters") <- "lung_score"
  attr(out, "lung_weights") <- w
  attr(out, "lung_settings") <- list(healthy = healthy, disease = disease, parameters = parameters, reference = reference,
                                     baseline_timepoint = baseline_timepoint %||% lv[1], onset_timepoint = onset_timepoint,
                                     min_effect = min_effect, z = z)
  out
}

#' Machine Learning Disease Score
#'
#' An alternative to [lung_health_score()]: a classification model is trained to
#' tell untreated diseased animals from healthy controls, and its predicted
#' log-odds of disease becomes the score (0 = like healthy controls, higher = more
#' disease-like). It can weight parameters and their combinations as the data
#' suggest, but needs more animals than the simple lung health score to be reliable.
#'
#' To keep the score honest:
#' * The model is trained only on sessions from disease onset on, where the class of
#'   each animal is constant, so it cannot learn early vs late sessions.
#' * Every healthy and untreated animal is scored by a model trained without it
#'   (grouped cross-validation), for all of its sessions.
#' * Treated animals are never used in training; they are scored by the final model.
#' * Log-odds are centered on healthy animals at each timepoint.
#'
#' The result works with [treatment_efficacy()], [summarize_groups()],
#' [plot_timecourse()] and [plot_animal_trends()] like a lung health score.
#'
#' @inheritParams lung_health_score
#' @param model Model code from [ml_model_names()]; elastic net (`"en"`) is a
#'   regularized logistic regression and is recommended for small groups.
#' @param folds Number of animal-held-out folds.
#' @param tuning,seed,parallel,progress Passed to [ml_fit()].
#' @return `sessions` with a `lung_score` column holding the centered log-odds of
#'   disease, and the attributes of [lung_health_score()] (used for the
#'   per-parameter breakdown) plus `"ml_disease"` with the model's held-out performance.
#' @export
ml_disease_score <- function(sessions, healthy, disease, parameters = NULL, onset_timepoint = NULL, model = "en",
                             folds = 5, tuning = "quick", seed = 1, parallel = FALSE, progress = NULL) {
  ml_check_packages()
  base <- lung_health_score(sessions, healthy, disease, parameters, onset_timepoint = onset_timepoint)
  st <- attr(base, "lung_settings")
  lv <- levels(sessions$timepoint)
  post <- match(as.character(sessions$timepoint), lv) >= match(st$onset_timepoint, lv)
  train_s <- sessions[as.character(sessions$group) %in% c(healthy, disease) & post, , drop = FALSE]
  labels <- c("Disease", "Healthy")
  d <- ml_prepare(train_s, positive = disease, negative = healthy, labels = labels, parameters = st$parameters, include_day = FALSE)
  fit <- ml_fit(d, models = model, validation = "animal", folds = folds, tuning = tuning, seed = seed,
                parallel = parallel, progress = progress)
  if (!model %in% names(fit$fits)) stop("The disease model could not be fitted: ", paste(unlist(fit$errors), collapse = "; "))
  final <- fit$fits[[model]]
  wf <- workflows::add_model(workflows::add_recipe(workflows::workflow(), workflows::extract_preprocessor(final)),
                             workflows::extract_spec_parsnip(final))

  # All sessions of every animal, in the model's input format.
  all_d <- data.frame(subject = sessions$subject, group = as.character(sessions$group), timepoint = as.character(sessions$timepoint),
                      outcome = factor(NA, levels = labels), sessions[, st$parameters, drop = FALSE],
                      check.names = FALSE, stringsAsFactors = FALSE)
  ok <- stats::complete.cases(all_d[, st$parameters, drop = FALSE])
  prob <- rep(NA_real_, nrow(all_d))
  predict_p <- function(f, rows) stats::predict(f, all_d[rows, , drop = FALSE], type = "prob")[[".pred_Disease"]]

  # Held-out predictions for training animals: refit without each fold's animals.
  set.seed(seed)
  train_animals <- unique(d$subject)
  cls <- tapply(as.character(d$outcome), d$subject, `[`, 1)[train_animals]
  fold_of <- stats::setNames(integer(length(train_animals)), train_animals)
  v <- fit$settings$folds
  for (k in unique(cls)) {
    a <- sample(train_animals[cls == k])
    fold_of[a] <- rep_len(seq_len(v), length(a))
  }
  for (k in seq_len(v)) {
    held <- names(fold_of)[fold_of == k]
    if (!length(held)) next
    f_k <- parsnip::fit(wf, d[!d$subject %in% held, , drop = FALSE])
    rows <- which(all_d$subject %in% held & ok)
    if (length(rows)) prob[rows] <- predict_p(f_k, rows)
  }
  rows <- which(!all_d$subject %in% train_animals & ok)
  if (length(rows)) prob[rows] <- predict_p(final, rows)

  prob <- pmin(pmax(prob, 0.001), 0.999)
  lo <- log(prob / (1 - prob))
  is_h <- as.character(sessions$group) %in% healthy
  center <- tapply(lo[is_h], sessions$timepoint[is_h], mean, na.rm = TRUE)
  ctr <- center[as.character(sessions$timepoint)]
  ctr[is.na(ctr)] <- mean(lo[is_h], na.rm = TRUE)
  out <- base
  out$lung_score <- lo - ctr
  st$score_type <- "ml"
  st$ml_model <- model
  attr(out, "lung_settings") <- st
  attr(out, "ml_disease") <- list(model = model, metrics = fit$metrics, tuning = fit$tuning, importance = fit$importance)
  out
}

#' Is a Treatment Working? Treatment Efficacy from Lung Health
#'
#' Compares treated animals with untreated diseased animals and healthy controls
#' on the lung health score from [lung_health_score()], averaged over a window
#' (e.g. after treatment starts), and for each parameter separately.
#'
#' Rescue is the share of the disease effect removed by treatment:
#' `(untreated - treated) / (untreated - healthy) * 100`, so 0% = no effect and
#' 100% = treated animals look like healthy controls.
#'
#' @param scored Output of [lung_health_score()].
#' @param treated Treated groups.
#' @param from,to First and last timepoint of the evaluation window. Default: from
#'   the onset timepoint to the end.
#' @param test `"parametric"` (Welch's t-tests) or `"nonparametric"` (Mann-Whitney).
#' @return A list with `verdict` (text), `summary` (one row per role), `animals`
#'   (score per animal), `comparisons`, `rescue`, `per_parameter` (rescue and test
#'   per parameter) and `model` (mixed model of treated vs untreated over time).
#' @export
treatment_efficacy <- function(scored, treated, from = NULL, to = NULL, test = c("parametric", "nonparametric")) {
  test <- match.arg(test)
  st <- attr(scored, "lung_settings")
  if (is.null(st)) stop("Run lung_health_score() first.")
  healthy <- st$healthy; disease <- st$disease
  if (length(treated) == 0) stop("Choose the treated group(s).")
  if (length(intersect(treated, c(healthy, disease)))) stop("Treated groups must differ from healthy and untreated groups.")
  lv <- levels(scored$timepoint)
  if (is.null(from)) from <- st$onset_timepoint
  if (is.null(to)) to <- lv[length(lv)]
  window <- lv[match(from, lv):match(to, lv)]

  role_of <- function(g) ifelse(g %in% healthy, "Healthy", ifelse(g %in% disease, "Untreated", ifelse(g %in% treated, "Treated", NA)))
  s <- scored[as.character(scored$timepoint) %in% window, , drop = FALSE]
  s$role <- factor(role_of(as.character(s$group)), levels = c("Healthy", "Untreated", "Treated"))
  s <- s[!is.na(s$role) & !is.na(s$lung_score), , drop = FALSE]
  animals <- stats::aggregate(lung_score ~ subject + group + role, data = s, FUN = mean)

  vals <- split(animals$lung_score, animals$role)
  m <- vapply(vals, function(x) if (length(x)) mean(x) else NA_real_, numeric(1))
  sem <- vapply(vals, function(x) if (length(x) > 1) stats::sd(x) / sqrt(length(x)) else NA_real_, numeric(1))
  summary <- data.frame(role = names(vals), n = lengths(vals), mean_score = m, sem = sem, row.names = NULL)
  cmp <- function(a, b) pair_test(vals[[a]], vals[[b]], test)
  comparisons <- data.frame(
    comparison = c("Treated vs Untreated", "Treated vs Healthy", "Untreated vs Healthy"),
    difference = c(m["Treated"] - m["Untreated"], m["Treated"] - m["Healthy"], m["Untreated"] - m["Healthy"]),
    p = c(cmp("Treated", "Untreated"), cmp("Treated", "Healthy"), cmp("Untreated", "Healthy")),
    row.names = NULL)
  disease_effect <- m["Untreated"] - m["Healthy"]
  rescue <- if (is.finite(disease_effect) && disease_effect > 0) unname((m["Untreated"] - m["Treated"]) / disease_effect * 100) else NA_real_

  # Per parameter: signed z averaged per animal over the window.
  z <- st$z[as.character(scored$timepoint) %in% window, , drop = FALSE]
  w <- attr(scored, "lung_weights")
  zs <- sweep(z, 2, ifelse(w$direction == 0, 1, w$direction), `*`)
  sw <- scored[as.character(scored$timepoint) %in% window, , drop = FALSE]
  role_w <- role_of(as.character(sw$group))
  per_parameter <- do.call(rbind, lapply(colnames(zs), function(p) {
    a <- stats::aggregate(zs[, p] ~ sw$subject + role_w, FUN = mean)
    names(a) <- c("subject", "role", "z")
    mm <- tapply(a$z, a$role, mean)
    de <- mm["Untreated"] - mm["Healthy"]
    data.frame(parameter = p, disease_effect = unname(de), used = w$used[w$parameter == p],
               rescue = if (is.finite(de) && de >= 0.2) unname((mm["Untreated"] - mm["Treated"]) / de * 100) else NA_real_,
               p_treated_vs_untreated = pair_test(a$z[a$role == "Treated"], a$z[a$role == "Untreated"], test),
               stringsAsFactors = FALSE)
  }))
  per_parameter <- per_parameter[order(-abs(per_parameter$disease_effect)), , drop = FALSE]
  rownames(per_parameter) <- NULL

  # Mixed model: treated vs untreated over time, random intercept per animal.
  mm_data <- s[s$role %in% c("Untreated", "Treated"), , drop = FALSE]
  mm_data$role <- droplevels(mm_data$role)
  mm_data$timepoint <- droplevels(mm_data$timepoint)
  model <- tryCatch({
    form <- if (nlevels(mm_data$timepoint) > 1) lung_score ~ role * timepoint else lung_score ~ role
    ctr <- list(role = "contr.sum")
    if (nlevels(mm_data$timepoint) > 1) ctr$timepoint <- "contr.sum"
    fit <- nlme::lme(form, random = ~ 1 | subject, data = mm_data, contrasts = ctr)
    a <- stats::anova(fit, type = "marginal")
    a <- a[rownames(a) != "(Intercept)", , drop = FALSE]
    data.frame(term = gsub("role", "Treatment", gsub("timepoint", "Time", gsub(":", " \u00d7 ", rownames(a)))),
               F = a[["F-value"]], p = a[["p-value"]], row.names = NULL)
  }, error = function(e) NULL)

  p_tu <- comparisons$p[1]; p_th <- comparisons$p[2]; p_uh <- comparisons$p[3]
  verdict <- if (!is.finite(disease_effect) || disease_effect <= 0 || (!is.na(p_uh) && p_uh >= 0.05 && disease_effect < 0.5)) {
    list(level = "unclear", text = "The untreated group is not clearly different from healthy controls on these parameters, so treatment efficacy cannot be judged. Choose parameters affected by the disease or check the time window.")
  } else if (!is.na(p_tu) && p_tu < 0.05 && m["Treated"] < m["Untreated"]) {
    if (!is.na(p_th) && p_th >= 0.05 && rescue >= 70) {
      list(level = "full", text = sprintf("The treatment is working: treated animals have better lung health than untreated animals (p = %s) and are not distinguishable from healthy controls (%.0f%% rescue).", format.pval(p_tu, digits = 2), rescue))
    } else {
      list(level = "partial", text = sprintf("The treatment improves lung health (p = %s vs untreated) but does not fully restore it (%.0f%% rescue).", format.pval(p_tu, digits = 2), rescue))
    }
  } else if (!is.na(rescue) && rescue < 0) {
    list(level = "worse", text = sprintf("Treated animals score worse than untreated animals (%.0f%% rescue, p = %s).", rescue, format.pval(p_tu, digits = 2)))
  } else {
    list(level = "none", text = sprintf("No significant evidence that the treatment improves lung health (%.0f%% rescue, p = %s vs untreated). With small groups only large effects are detectable.", rescue, format.pval(p_tu, digits = 2)))
  }

  list(verdict = verdict, summary = summary, animals = animals, comparisons = comparisons, rescue = rescue,
       per_parameter = per_parameter, model = model, window = window, test = test)
}

#' Plot Treatment Effect per Parameter
#'
#' Rescue (share of the disease effect removed by treatment) for each parameter.
#'
#' @param efficacy Output of [treatment_efficacy()].
#' @return A ggplot object.
#' @export
plot_treatment_parameters <- function(efficacy) {
  d <- efficacy$per_parameter
  d <- d[!is.na(d$rescue), , drop = FALSE]
  if (nrow(d) == 0) stop("No parameter has a disease effect of at least 0.2 SD to rescue.")
  d$shown <- pmax(pmin(d$rescue, 200), -100)
  d$parameter <- factor(d$parameter, levels = d$parameter[order(d$shown)])
  d$sig <- ifelse(!is.na(d$p_treated_vs_untreated) & d$p_treated_vs_untreated < 0.05, "p < 0.05 vs untreated", "not significant")
  ggplot2::ggplot(d, ggplot2::aes(x = .data$shown, y = .data$parameter, fill = .data$sig)) +
    ggplot2::geom_vline(xintercept = c(0, 100), linetype = c("solid", "dashed"), color = c("grey40", "#2E8B57")) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f%%", .data$rescue), hjust = ifelse(.data$shown >= 0, -0.15, 1.15)), size = 3.2) +
    ggplot2::scale_fill_manual(values = c("p < 0.05 vs untreated" = "#1F5F8B", "not significant" = "#A9B8C6")) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.12)) +
    ggplot2::labs(title = "Treatment effect on each parameter",
                  x = "Rescue (% of disease effect removed; 100% = like healthy)", y = NULL,
                  caption = "Only parameters with a disease effect of at least 0.2 SD are shown; values beyond -100% or 200% are capped") +
    theme_plethr()
}
