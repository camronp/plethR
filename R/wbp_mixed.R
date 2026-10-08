# Mixed-model time course with covariates, and sample-size planning.

#' Mixed-Model Time Course
#'
#' Fits `value ~ group * timepoint + covariates + (1 | animal)` to every session
#' and compares each group with the reference group at every timepoint using
#' model-estimated marginal means (emmeans). Using all sessions with a random
#' effect per animal handles repeated measurements correctly, and covariates
#' adjust for differences between animals.
#'
#' Covariates:
#' * `"baseline"`: each animal's value at the first timepoint (then left out as an
#'   outcome). Adjusts for differences that existed before treatment (ANCOVA).
#' * `"sex"`: needs `sex` (a named vector, animal -> "F"/"M").
#' * `"weight"`: body weight at each session (column `Weight`, see [add_body_weight()]).
#'
#' @param sessions Output of [summarize_sessions()] with groups assigned.
#' @param parameter Parameter to model.
#' @param reference Reference group.
#' @param covariates Any of `"baseline"`, `"sex"`, `"weight"`.
#' @param sex Named character vector of sex per animal (needed for `"sex"`).
#' @param log_transform Model log(value); differences become ratios.
#' @param p_adjust Correction across all timepoint-by-group comparisons.
#' @return A list with `anova` (marginal F-tests), `means` (estimated means with
#'   95\% CI per group and timepoint), `contrasts` (difference from the reference
#'   per timepoint: estimate, 95\% CI, \% difference, p, adjusted p, stars),
#'   `settings` and `n` (animals per group).
#' @export
mixed_timecourse <- function(sessions, parameter, reference, covariates = character(0), sex = NULL,
                             log_transform = FALSE, p_adjust = "holm") {
  if (!requireNamespace("emmeans", quietly = TRUE)) stop("The mixed-model time course needs the emmeans package: install.packages(\"emmeans\")")
  d <- data.frame(subject = sessions$subject, group = droplevels(as.factor(sessions$group)),
                  timepoint = droplevels(sessions$timepoint), y = sessions[[parameter]], stringsAsFactors = FALSE)
  if (log_transform) d$y <- ifelse(d$y > 0, log(d$y), NA_real_)
  if (!reference %in% levels(d$group)) stop("Reference group not found.")
  d$group <- stats::relevel(d$group, ref = reference)
  rhs <- "group * timepoint"
  used <- character(0)
  if ("baseline" %in% covariates) {
    b_tp <- levels(d$timepoint)[1]
    b <- d[d$timepoint == b_tp, c("subject", "y")]
    d$baseline <- b$y[match(d$subject, b$subject)]
    d <- d[d$timepoint != b_tp, , drop = FALSE]
    d$timepoint <- droplevels(d$timepoint)
    d$baseline <- d$baseline - mean(d$baseline, na.rm = TRUE)
    rhs <- paste(rhs, "+ baseline"); used <- c(used, paste0("baseline (", b_tp, ")"))
  }
  if ("sex" %in% covariates) {
    if (is.null(sex)) stop("Sex is needed for the sex covariate (study design file).")
    d$sex <- factor(unname(sex[d$subject]))
    if (nlevels(droplevels(d$sex)) >= 2) { rhs <- paste(rhs, "+ sex"); used <- c(used, "sex") }
  }
  if ("weight" %in% covariates) {
    if (!"Weight" %in% names(sessions)) stop("Body weight is needed for the weight covariate (study design file).")
    w <- sessions$Weight[match(paste(d$subject, d$timepoint), paste(sessions$subject, as.character(sessions$timepoint)))]
    d$weight <- w - mean(w, na.rm = TRUE)
    rhs <- paste(rhs, "+ weight"); used <- c(used, "body weight")
  }
  d <- d[stats::complete.cases(d), , drop = FALSE]
  if (nlevels(d$timepoint) < 2) stop("At least two timepoints are needed.")
  form <- stats::as.formula(paste("y ~", rhs))
  fit <- nlme::lme(form, random = ~ 1 | subject, data = d, method = "REML", na.action = stats::na.omit)

  # Marginal F-tests with sum-to-zero contrasts (same model, different coding).
  ctr <- list(group = "contr.sum", timepoint = "contr.sum")
  if ("sex" %in% names(d) && nlevels(droplevels(d$sex)) >= 2) ctr$sex <- "contr.sum"
  fit_sum <- nlme::lme(form, random = ~ 1 | subject, data = d, method = "REML", contrasts = ctr)
  a <- stats::anova(fit_sum, type = "marginal")
  a <- a[rownames(a) != "(Intercept)", , drop = FALSE]
  terms <- gsub("group", "Group", gsub("timepoint", "Time", gsub(":", " \u00d7 ", rownames(a))))
  anova <- data.frame(term = terms, num_df = a[["numDF"]], den_df = a[["denDF"]], F = a[["F-value"]], p = a[["p-value"]])

  em <- emmeans::emmeans(fit, ~ group | timepoint, data = d)
  ms <- as.data.frame(summary(em, infer = c(TRUE, FALSE)))
  means <- data.frame(group = ms$group, timepoint = ms$timepoint, mean = ms$emmean, lower = ms$lower.CL, upper = ms$upper.CL)
  ref_idx <- which(levels(d$group) == reference)
  ct <- as.data.frame(summary(emmeans::contrast(em, method = "trt.vs.ctrl", ref = ref_idx, adjust = "none"), infer = c(TRUE, TRUE)))
  grp <- sub(paste0(" - ", reference, "$"), "", as.character(ct$contrast))
  grp <- gsub("^\\(|\\)$", "", grp)
  ref_mean <- means$mean[means$group == reference][match(as.character(ct$timepoint), as.character(means$timepoint[means$group == reference]))]
  if (log_transform) {
    means[, c("mean", "lower", "upper")] <- exp(means[, c("mean", "lower", "upper")])
    pct <- (exp(ct$estimate) - 1) * 100; lo <- (exp(ct$lower.CL) - 1) * 100; hi <- (exp(ct$upper.CL) - 1) * 100
  } else {
    pct <- ct$estimate / abs(ref_mean) * 100; lo <- ct$lower.CL / abs(ref_mean) * 100; hi <- ct$upper.CL / abs(ref_mean) * 100
  }
  contrasts <- data.frame(timepoint = factor(as.character(ct$timepoint), levels = levels(d$timepoint)), group = grp,
                          estimate = ct$estimate, lower = ct$lower.CL, upper = ct$upper.CL,
                          pct_difference = pct, pct_lower = lo, pct_upper = hi, p = ct$p.value, stringsAsFactors = FALSE)
  contrasts$p_adj <- stats::p.adjust(contrasts$p, method = p_adjust)
  contrasts$stars <- p_stars(contrasts$p_adj)
  n <- tapply(d$subject, d$group, function(s) length(unique(s)))
  list(anova = anova, means = means, contrasts = contrasts, n = n,
       settings = list(parameter = parameter, reference = reference, covariates = used, log_transform = log_transform,
                       p_adjust = p_adjust, formula = paste("value ~", rhs, "+ (1 | animal)")))
}

#' Plot a Mixed-Model Time Course
#'
#' Model-estimated group means with 95\% confidence intervals at each timepoint;
#' stars mark timepoints where a group differs from the reference (adjusted p < 0.05).
#'
#' @param mt Output of [mixed_timecourse()].
#' @param colors Named group colors.
#' @return A ggplot object.
#' @export
plot_mixed_timecourse <- function(mt, colors = NULL) {
  m <- mt$means
  lv <- levels(m$timepoint)
  m$x <- match(as.character(m$timepoint), lv)
  if (is.null(colors)) colors <- plethr_colors(levels(m$group))
  colors <- colors[names(colors) %in% m$group]
  pos <- ggplot2::position_dodge(width = 0.3)
  p <- ggplot2::ggplot(m, ggplot2::aes(x = .data$x, y = .data$mean, color = .data$group, group = .data$group)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lower, ymax = .data$upper), width = 0.25, linewidth = 0.5, position = pos) +
    ggplot2::geom_line(linewidth = 0.9, position = pos) +
    ggplot2::geom_point(size = 2.3, position = pos)
  s <- mt$contrasts[!is.na(mt$contrasts$p_adj) & mt$contrasts$p_adj < 0.05, , drop = FALSE]
  if (nrow(s)) {
    top <- max(m$upper, na.rm = TRUE); rng <- diff(range(c(m$lower, m$upper), na.rm = TRUE))
    s$x <- match(as.character(s$timepoint), lv)
    s$y <- top + rng * (0.06 + 0.07 * (match(s$group, names(colors)) - 1))
    p <- p + ggplot2::geom_text(data = s, ggplot2::aes(x = .data$x, y = .data$y, label = .data$stars, color = .data$group),
                                inherit.aes = FALSE, size = 4.5, fontface = "bold", show.legend = FALSE)
  }
  st <- mt$settings
  rotate <- length(lv) > 6
  p + ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_x_continuous(breaks = seq_along(lv), labels = lv, expand = ggplot2::expansion(add = 0.4)) +
    ggplot2::labs(title = paste(wbp_feature_name(st$parameter), "(mixed model)"), x = NULL,
                  y = wbp_axis_label(st$parameter),
                  caption = paste0("Model-estimated means \u00b1 95% CI; ", st$formula,
                                   if (length(st$covariates)) paste0("; adjusted for ", paste(st$covariates, collapse = ", ")) else "",
                                   "; * adjusted p < 0.05 vs ", st$reference)) +
    theme_plethr() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = if (rotate) 45 else 0, hjust = if (rotate) 1 else 0.5))
}

#' Sample Size Planning from Pilot Data
#'
#' For every parameter and group, uses the pilot (current) data to estimate how
#' many animals per group would detect the difference from the reference group
#' with the chosen power, and what difference the current group size can detect.
#'
#' Pilot effect sizes are imprecise and tend to be overestimated when chosen
#' because they look large; prefer planning for the smallest difference that
#' would matter biologically (`effect_pct`).
#'
#' @param values Output of [summarize_subjects()] (one value per animal).
#' @param reference Reference group.
#' @param power Target power.
#' @param alpha Significance level.
#' @param n_comparisons Number of planned comparisons; alpha is divided by this (Bonferroni).
#' @param effect_pct Optional difference to detect, in \% of the reference mean. Default: the observed difference.
#' @return A data frame per parameter and group: means, SD, Cohen's d, the effect
#'   used, animals per group needed, current n and power, and the smallest
#'   difference detectable at the current n.
#' @export
sample_size_plan <- function(values, reference, power = 0.8, alpha = 0.05, n_comparisons = 1, effect_pct = NULL) {
  sig <- alpha / n_comparisons
  out <- lapply(split(values, values$parameter), function(d) {
    ref <- d$value[d$group == reference & !is.na(d$value)]
    lapply(setdiff(unique(as.character(d$group)), reference), function(g) {
      x <- d$value[d$group == g & !is.na(d$value)]
      if (length(ref) < 2 || length(x) < 2) return(NULL)
      sdp <- sqrt(((length(ref) - 1) * stats::var(ref) + (length(x) - 1) * stats::var(x)) / (length(ref) + length(x) - 2))
      diff <- mean(x) - mean(ref)
      delta <- if (is.null(effect_pct)) abs(diff) else abs(effect_pct / 100 * mean(ref))
      n_now <- 2 / (1 / length(ref) + 1 / length(x))
      need <- if (sdp > 0 && delta > 0) tryCatch(ceiling(stats::power.t.test(delta = delta, sd = sdp, sig.level = sig, power = power)$n),
                                               error = function(e) NA_real_) else NA_real_
      pw <- if (sdp > 0 && delta > 0) stats::power.t.test(n = n_now, delta = delta, sd = sdp, sig.level = sig)$power else NA_real_
      det <- if (sdp > 0) stats::power.t.test(n = n_now, sd = sdp, sig.level = sig, power = power)$delta / abs(mean(ref)) * 100 else NA_real_
      data.frame(parameter = d$parameter[1], group = g, mean_reference = mean(ref), mean_group = mean(x), sd_pooled = sdp,
                 observed_pct = diff / abs(mean(ref)) * 100, cohens_d = diff / sdp,
                 planned_pct = delta / abs(mean(ref)) * 100, n_per_group_needed = need,
                 n_current = round(n_now, 1), power_current = pw, detectable_pct_current = det, stringsAsFactors = FALSE)
    })
  })
  out <- do.call(rbind, unlist(out, recursive = FALSE))
  if (is.null(out)) stop("Not enough animals to estimate variability.")
  out <- out[order(out$n_per_group_needed), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "settings") <- list(power = power, alpha = alpha, n_comparisons = n_comparisons, effect_pct = effect_pct, reference = reference)
  out
}

#' Plot Power against Group Size
#'
#' @param plan Output of [sample_size_plan()].
#' @param parameter Parameter to show (one curve per group compared with the reference).
#' @param colors Named group colors.
#' @param max_n Largest group size on the x axis.
#' @return A ggplot object.
#' @export
plot_power_curve <- function(plan, parameter, colors = NULL, max_n = 40) {
  st <- attr(plan, "settings")
  d <- plan[plan$parameter == parameter, , drop = FALSE]
  if (nrow(d) == 0) stop("No planning results for ", parameter, ".")
  sig <- st$alpha / st$n_comparisons
  curves <- do.call(rbind, lapply(seq_len(nrow(d)), function(i) {
    delta <- d$planned_pct[i] / 100 * abs(d$mean_reference[i])
    n <- 2:max_n
    pw <- vapply(n, function(k) stats::power.t.test(n = k, delta = delta, sd = d$sd_pooled[i], sig.level = sig)$power, numeric(1))
    data.frame(group = d$group[i], n = n, power = pw, label = sprintf("%s (%.0f%% difference)", d$group[i], d$planned_pct[i]))
  }))
  if (is.null(colors)) colors <- plethr_colors(unique(curves$group))
  lab_cols <- stats::setNames(colors[match(unique(curves$group), names(colors))], unique(curves$label))
  ggplot2::ggplot(curves, ggplot2::aes(x = .data$n, y = .data$power, color = .data$label)) +
    ggplot2::geom_hline(yintercept = st$power, linetype = "dashed", color = "grey50") +
    ggplot2::geom_vline(data = d, ggplot2::aes(xintercept = .data$n_current), linetype = "dotted", color = "grey60") +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_color_manual(values = lab_cols) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(x * 100, "%")) +
    ggplot2::labs(title = paste("Power to detect differences in", wbp_feature_name(parameter)), x = "Animals per group", y = "Power",
                  caption = sprintf("Two-sided t-test, alpha = %g%s; dashed line = target power (%.0f%%), dotted = current group size; variability from the current data",
                                    st$alpha, if (st$n_comparisons > 1) sprintf(" / %d comparisons", st$n_comparisons) else "", st$power * 100)) +
    theme_plethr(11) +
    ggplot2::theme(legend.position = "top")
}
