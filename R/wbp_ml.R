# Machine learning from session-level WBP data, built on tidymodels. Adapted from
# the CP05 infection-prediction pipeline (centered/scaled predictors, upsampling,
# tuning by ROC AUC), with validation that keeps each animal's sessions together.
#
# Prediction goals:
# * infected vs uninfected  (ml_prepare_infection)
# * acute vs chronic phase  (ml_prepare_phase)
# * disease severity        (ml_prepare_severity; regression on a measured value)
# * any two sets of groups  (ml_prepare)
#
# The modeling packages are optional (Suggests); ml_check_packages() reports any
# that are missing.

ml_packages <- c("parsnip", "recipes", "rsample", "tune", "workflows", "yardstick",
                 "dials", "themis", "ranger", "xgboost", "glmnet", "kknn", "discrim", "MASS")

#' Check the Packages Needed for Machine Learning
#'
#' @return Invisibly, the names of missing packages. Errors with an install
#'   command if any are missing.
#' @export
ml_check_packages <- function() {
  missing <- ml_packages[!vapply(ml_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Machine learning needs these packages: ", paste(missing, collapse = ", "),
         ". Install them with install.packages(c(", paste(sprintf('"%s"', missing), collapse = ", "), ")).")
  }
  invisible(missing)
}

#' Machine Learning Model Names
#'
#' @param type `"classification"` (categories, e.g. infected vs uninfected),
#'   `"regression"` (a number, e.g. severity) or `"all"`.
#' @return A named character vector: short codes (names) and full model names.
#' @examples
#' ml_model_names()
#' ml_model_names("regression")
#' @export
ml_model_names <- function(type = c("classification", "regression", "all")) {
  type <- match.arg(type)
  all <- c(log = "Logistic regression", lm = "Linear regression", knn = "K-nearest neighbors",
           lda = "Linear discriminant analysis", qda = "Quadratic discriminant analysis",
           en = "Elastic net", rf = "Random forest", bt = "Gradient-boosted trees")
  switch(type,
         classification = all[c("log", "knn", "lda", "qda", "en", "rf", "bt")],
         regression = all[c("lm", "knn", "en", "rf", "bt")],
         all = all)
}

ml_type <- function(object) {
  s <- if (inherits(object, "plethr_ml")) object$settings else attr(object, "ml")
  if (is.null(s$type)) "classification" else s$type
}

#' Add Days Post-Infection to Session Data
#'
#' @param sessions Output of [summarize_sessions()].
#' @param infection_timepoint The first session after infection (or treatment).
#' @param offset Days between infection and that session. With the default of 0,
#'   day 0 is the first post-infection session.
#' @return `sessions` with columns `dpi` (days post-infection; negative before
#'   infection) and `post_infection` (logical).
#' @export
add_dpi <- function(sessions, infection_timepoint, offset = 0) {
  lv <- levels(sessions$timepoint)
  if (!infection_timepoint %in% lv) stop("Timepoint '", infection_timepoint, "' not found.")
  at_inf <- sessions[sessions$timepoint == infection_timepoint, c("subject", "day")]
  fallback <- stats::median(at_inf$day)
  inf_day <- at_inf$day[match(sessions$subject, at_inf$subject)]
  inf_day[is.na(inf_day)] <- fallback
  sessions$dpi <- sessions$day - inf_day + offset
  sessions$post_infection <- match(as.character(sessions$timepoint), lv) >= match(infection_timepoint, lv)
  sessions
}

# Common builder: id columns, outcome, predictors, optional covariates.
ml_build <- function(s, outcome, parameters, include_day, covariate, covariate_name, info) {
  d <- data.frame(subject = s$subject, group = as.character(s$group), timepoint = as.character(s$timepoint),
                  outcome = outcome, s[, parameters, drop = FALSE], check.names = FALSE, stringsAsFactors = FALSE)
  if (include_day) d$day <- s$day
  if (!is.null(covariate)) d[[covariate_name]] <- factor(unname(covariate[d$group]))
  n0 <- nrow(d)
  d <- d[stats::complete.cases(d), , drop = FALSE]
  rownames(d) <- NULL
  type <- if (is.numeric(d$outcome)) "regression" else "classification"
  attr(d, "ml") <- c(info, list(type = type, parameters = parameters,
                                labels = if (type == "classification") levels(d$outcome) else NULL,
                                include_day = include_day, covariate = covariate, covariate_name = covariate_name,
                                timepoints = levels(droplevels(s$timepoint)), dropped_rows = n0 - nrow(d)))
  d
}

check_labels <- function(labels) {
  if (length(labels) != 2 || identical(labels[1], labels[2])) stop("The two classes need different names.")
}

#' Prepare Session Data: Any Two Sets of Groups
#'
#' Builds one row per animal and session with the outcome (positive vs negative
#' class), the respiratory parameters as predictors, and optional covariates.
#'
#' @param sessions Output of [summarize_sessions()] with groups assigned.
#' @param positive Groups forming the positive class.
#' @param negative Groups forming the negative class. Defaults to all other groups.
#' @param labels Names of the positive and negative classes.
#' @param parameters Respiratory parameters to use as predictors. Defaults to all.
#' @param exclude_timepoints Timepoints to leave out.
#' @param include_day Add study day as a predictor.
#' @param covariate Optional named character vector mapping each group to the level
#'   of an extra categorical predictor, e.g. genotype `c("Infected WT" = "WT", ...)`.
#' @param covariate_name Name of that predictor.
#' @return A data frame with id columns `subject`, `group`, `timepoint`, the factor
#'   `outcome` (positive class first) and the predictors. Rows with missing
#'   predictor values are dropped.
#' @export
ml_prepare <- function(sessions, positive, negative = NULL, labels = c("Positive", "Negative"),
                       parameters = NULL, exclude_timepoints = NULL, include_day = TRUE,
                       covariate = NULL, covariate_name = "covariate") {
  if (is.null(parameters)) parameters <- attr(sessions, "parameters")
  groups <- as.character(unique(sessions$group))
  if (is.null(negative)) negative <- setdiff(groups, positive)
  if (length(positive) == 0 || length(negative) == 0) stop("Both classes need at least one group.")
  if (length(intersect(positive, negative))) stop("A group cannot be in both classes.")
  check_labels(labels)
  s <- sessions[as.character(sessions$group) %in% c(positive, negative), , drop = FALSE]
  s <- s[!as.character(s$timepoint) %in% exclude_timepoints, , drop = FALSE]
  outcome <- factor(ifelse(as.character(s$group) %in% positive, labels[1], labels[2]), levels = labels)
  ml_build(s, outcome, parameters, include_day, covariate, covariate_name,
           list(task = "groups", positive = positive, negative = negative, exclude_timepoints = exclude_timepoints))
}

#' Prepare Session Data: Infected vs Uninfected
#'
#' Sessions of infected groups from the infection timepoint on are labeled
#' infected and sessions of control groups uninfected. By default, sessions before
#' infection are left out.
#'
#' Labeling pre-infection sessions of infected animals as uninfected
#' (`pre_infection = "uninfected"`) makes the label change over time within those
#' animals, so a model can score well by detecting early vs late sessions (age,
#' habituation to the chamber) rather than infection. On CP05 such a placebo
#' (uninfected controls labeled "infected" after the same timepoint) reached an
#' animal-level ROC AUC of about 0.75. If you use it, compare with that placebo.
#'
#' @inheritParams ml_prepare
#' @param infected Groups that were infected.
#' @param controls Groups that were never infected. Defaults to all other groups.
#' @param infection_timepoint The first session after infection.
#' @param pre_infection `"exclude"` (default) leaves all pre-infection sessions out;
#'   `"uninfected"` labels pre-infection sessions of infected animals as uninfected.
#' @export
ml_prepare_infection <- function(sessions, infected, controls = NULL, infection_timepoint,
                                 pre_infection = c("exclude", "uninfected"), labels = c("Infected", "Uninfected"),
                                 parameters = NULL, include_day = TRUE, covariate = NULL, covariate_name = "covariate") {
  pre_infection <- match.arg(pre_infection)
  if (is.null(parameters)) parameters <- attr(sessions, "parameters")
  if (is.null(controls)) controls <- setdiff(as.character(unique(sessions$group)), infected)
  if (length(infected) == 0 || length(controls) == 0) stop("Choose at least one infected and one control group.")
  check_labels(labels)
  s <- add_dpi(sessions[as.character(sessions$group) %in% c(infected, controls), , drop = FALSE], infection_timepoint)
  if (pre_infection == "exclude") s <- s[s$post_infection, , drop = FALSE]
  outcome <- factor(ifelse(as.character(s$group) %in% infected & s$post_infection, labels[1], labels[2]), levels = labels)
  ml_build(s, outcome, parameters, include_day, covariate, covariate_name,
           list(task = "infection", positive = infected, negative = controls, infection_timepoint = infection_timepoint,
                pre_infection = pre_infection,
                exclude_timepoints = if (pre_infection == "exclude") levels(s$timepoint)[seq_len(match(infection_timepoint, levels(s$timepoint)) - 1)] else NULL))
}

#' Prepare Session Data: Acute vs Chronic Phase
#'
#' Uses post-infection sessions of the chosen groups. Sessions up to `acute_days`
#' days post-infection are acute; later sessions are chronic. Study day is never
#' used as a predictor here, because it defines the outcome.
#'
#' Phase is completely tied to time, so a model can separate "acute" from
#' "chronic" sessions by picking up age or growth rather than disease. Fit the
#' same model to control animals labeled by the same cutoff: if it separates
#' them just as well, the signal is time, not disease phase.
#'
#' @inheritParams ml_prepare_infection
#' @param groups Groups to use (normally the infected groups).
#' @param acute_days Last day post-infection counted as acute.
#' @param offset Days between infection and the infection timepoint session.
#' @export
ml_prepare_phase <- function(sessions, groups, infection_timepoint, acute_days = 14, offset = 0,
                             labels = c("Acute", "Chronic"), parameters = NULL,
                             covariate = NULL, covariate_name = "covariate") {
  if (is.null(parameters)) parameters <- attr(sessions, "parameters")
  check_labels(labels)
  s <- add_dpi(sessions[as.character(sessions$group) %in% groups, , drop = FALSE], infection_timepoint, offset)
  s <- s[s$post_infection, , drop = FALSE]
  outcome <- factor(ifelse(s$dpi <= acute_days, labels[1], labels[2]), levels = labels)
  if (length(unique(outcome)) < 2) stop("All sessions fall in one phase; change the acute cutoff.")
  ml_build(s, outcome, parameters, include_day = FALSE, covariate, covariate_name,
           list(task = "phase", positive = groups, negative = NULL, infection_timepoint = infection_timepoint,
                acute_days = acute_days, offset = offset,
                exclude_timepoints = levels(s$timepoint)[seq_len(match(infection_timepoint, levels(sessions$timepoint)) - 1)]))
}

#' Prepare Session Data: Disease Severity
#'
#' Joins a measured severity value (e.g. lung CFU, histology score, weight loss)
#' to the session data for regression. Values can be given per animal (applied to
#' all of its sessions) or per animal and timepoint.
#'
#' @inheritParams ml_prepare
#' @param severity Data frame with columns `subject`, `value` and optionally `timepoint`.
#' @param groups Groups to include. Defaults to all groups with severity values.
#' @param outcome_name Name of the severity measure, for labels.
#' @param log_outcome Model log10(value); recommended for CFU.
#' @export
ml_prepare_severity <- function(sessions, severity, groups = NULL, outcome_name = "Severity", log_outcome = FALSE,
                                parameters = NULL, exclude_timepoints = NULL, include_day = TRUE,
                                covariate = NULL, covariate_name = "covariate") {
  if (is.null(parameters)) parameters <- attr(sessions, "parameters")
  if (!all(c("subject", "value") %in% names(severity))) stop("The severity table needs columns 'subject' and 'value'.")
  severity$subject <- trimws(as.character(severity$subject))
  severity$value <- suppressWarnings(as.numeric(severity$value))
  s <- sessions
  if (!is.null(groups)) s <- s[as.character(s$group) %in% groups, , drop = FALSE]
  s <- s[!as.character(s$timepoint) %in% exclude_timepoints, , drop = FALSE]
  key <- function(x) tolower(trimws(as.character(x)))
  if ("timepoint" %in% names(severity) && any(!is.na(severity$timepoint) & nzchar(as.character(severity$timepoint)))) {
    v <- severity$value[match(paste(key(s$subject), key(s$timepoint)), paste(key(severity$subject), key(severity$timepoint)))]
  } else {
    v <- severity$value[match(key(s$subject), key(severity$subject))]
  }
  if (log_outcome) v <- ifelse(!is.na(v) & v > 0, log10(v), NA_real_)
  keep <- !is.na(v)
  if (sum(keep) < 10) stop("Fewer than 10 sessions matched a severity value. Check that animal names match the sheet names.")
  s <- s[keep, , drop = FALSE]
  ml_build(s, v[keep], parameters, include_day, covariate, covariate_name,
           list(task = "severity", outcome_name = if (log_outcome) paste0("log10 ", outcome_name) else outcome_name,
                log_outcome = log_outcome, exclude_timepoints = exclude_timepoints,
                unmatched = setdiff(unique(severity$subject), unique(sessions$subject))))
}

ml_specs <- function(type) {
  loadNamespace("discrim")  # registers the MASS engines for discriminant analysis
  mode <- type
  spec <- function(x, engine, ...) parsnip::set_mode(parsnip::set_engine(x, engine, ...), mode)
  out <- list(
    knn = spec(parsnip::nearest_neighbor(neighbors = tune::tune()), "kknn"),
    rf = spec(parsnip::rand_forest(mtry = tune::tune(), trees = tune::tune(), min_n = tune::tune()), "ranger"),
    bt = spec(parsnip::boost_tree(mtry = tune::tune(), trees = tune::tune(), learn_rate = tune::tune()), "xgboost")
  )
  if (type == "classification") {
    c(out, list(
      log = spec(parsnip::logistic_reg(), "glm"),
      lda = spec(parsnip::discrim_linear(), "MASS"),
      qda = spec(parsnip::discrim_quad(), "MASS"),
      en = spec(parsnip::logistic_reg(penalty = tune::tune(), mixture = tune::tune()), "glmnet")
    ))
  } else {
    c(out, list(
      lm = spec(parsnip::linear_reg(), "lm"),
      en = spec(parsnip::linear_reg(penalty = tune::tune(), mixture = tune::tune()), "glmnet")
    ))
  }
}

# Tuning grids. "thorough" reproduces the grids of the original CP05 pipeline.
ml_grid <- function(model, n_predictors, tuning) {
  mt <- dials::mtry(range = c(1, min(10, n_predictors)))
  if (tuning == "quick") {
    switch(model,
      knn = dials::grid_regular(dials::neighbors(range = c(1, 20)), levels = 10),
      en = dials::grid_regular(dials::penalty(range = c(-3, 0)), dials::mixture(range = c(0, 1)), levels = 5),
      rf = dials::grid_regular(mt, dials::trees(range = c(100, 500)), dials::min_n(range = c(2, 20)), levels = 3),
      bt = dials::grid_regular(mt, dials::trees(range = c(100, 500)), dials::learn_rate(range = c(-3, -1)), levels = 3),
      NULL)
  } else {
    switch(model,
      knn = dials::grid_regular(dials::neighbors(range = c(1, 20)), levels = 20),
      en = dials::grid_regular(dials::penalty(range = c(0.02, 3), trans = scales::transform_identity()),
                               dials::mixture(range = c(0, 1)), levels = 10),
      rf = dials::grid_regular(mt, dials::trees(range = c(50, 1000)), dials::min_n(range = c(5, 25)), levels = 10),
      bt = dials::grid_regular(mt, dials::trees(range = c(100, 1000)), dials::learn_rate(range = c(-10, -1)), levels = 5),
      NULL)
  }
}

metric_cols <- c("roc_auc", "accuracy", "sensitivity", "specificity", "rsq", "rmse", "mae", "r")

# Classification metrics; the positive class is the first factor level.
ml_metrics <- function(truth, prob, cls) {
  cls <- factor(cls, levels = levels(truth))
  data.frame(
    roc_auc = if (length(unique(truth)) == 2) yardstick::roc_auc_vec(truth, prob) else NA_real_,
    accuracy = yardstick::accuracy_vec(truth, cls),
    sensitivity = yardstick::sens_vec(truth, cls),
    specificity = yardstick::spec_vec(truth, cls),
    rsq = NA_real_, rmse = NA_real_, mae = NA_real_, r = NA_real_,
    n = length(truth)
  )
}

# Regression metrics. R-squared is 1 - SSE/SST, so 0 = no better than predicting the mean.
ml_metrics_reg <- function(obs, pred) {
  data.frame(
    roc_auc = NA_real_, accuracy = NA_real_, sensitivity = NA_real_, specificity = NA_real_,
    rsq = yardstick::rsq_trad_vec(obs, pred),
    rmse = yardstick::rmse_vec(obs, pred),
    mae = yardstick::mae_vec(obs, pred),
    r = if (stats::sd(pred) > 0) stats::cor(obs, pred) else NA_real_,
    n = length(obs)
  )
}

ml_importance <- function(model, fit, best) {
  eng <- workflows::extract_fit_engine(fit)
  feats <- NULL
  imp <- tryCatch(switch(model,
    rf = { v <- eng$variable.importance; feats <- names(v); unname(v) },
    bt = { v <- xgboost::xgb.importance(model = eng); feats <- v$Feature; v$Gain },
    log = , lm = { co <- stats::coef(eng)[-1]; feats <- names(co); abs(unname(co)) },
    en = { co <- as.matrix(stats::coef(eng, s = best$penalty))[-1, 1]; feats <- names(co); abs(unname(co)) },
    NULL), error = function(e) NULL)
  if (is.null(imp) || !length(imp)) return(NULL)
  imp[!is.finite(imp)] <- NA
  out <- data.frame(model = model, feature = feats, importance = imp, stringsAsFactors = FALSE)
  out <- out[!is.na(out$importance), , drop = FALSE]
  if (nrow(out) && max(out$importance) > 0) out$importance <- out$importance / max(out$importance) * 100
  out[order(-out$importance), , drop = FALSE]
}

#' Train and Evaluate Prediction Models
#'
#' For a categorical outcome, fits up to seven classification models (logistic
#' regression, k-nearest neighbors, linear and quadratic discriminant analysis,
#' elastic net, random forest, gradient-boosted trees), upsampling the smaller
#' class and tuning by ROC AUC. For a numeric outcome (severity), fits up to five
#' regression models (linear regression, k-nearest neighbors, elastic net, random
#' forest, gradient-boosted trees), tuning by RMSE. Predictors are always centered
#' and scaled.
#'
#' Two validation schemes are available:
#' * `"animal"` (recommended): cross-validation in which all sessions of an animal
#'   are held out together, so models are always evaluated on animals they have
#'   never seen. Performance is computed from the out-of-fold predictions.
#' * `"study"`: leave-one-study-out, for data from [ml_prepare_library()]: each
#'   study is predicted by models trained on the other studies.
#' * `"random"`: the original CP05 approach. Rows (animal sessions) are split
#'   80/20 at random and models are tuned by 10-fold cross-validation on the
#'   training rows. Because sessions of the same animal end up in both training
#'   and test data, models can recognize individual animals and performance is
#'   usually optimistic.
#'
#' @param data Output of [ml_prepare()], [ml_prepare_infection()],
#'   [ml_prepare_phase()] or [ml_prepare_severity()].
#' @param models Model codes from [ml_model_names()]. Models that do not fit the
#'   outcome type are skipped.
#' @param validation `"animal"`, `"study"` or `"random"`.
#' @param folds Number of cross-validation folds for `"animal"` validation.
#' @param tuning `"quick"` (small grids) or `"thorough"` (the original CP05 grids; slow).
#' @param upsample Upsample the smaller class in training data (classification only).
#' @param seed Random seed.
#' @param progress Optional function called as `progress(i, n, model_name)`.
#' @param parallel Tune on several CPU cores (needs the future package).
#' @return An object of class `plethr_ml`: a list with `metrics` (per model, at
#'   session and animal level), `predictions` (held-out predictions with ids),
#'   `tuning` (best parameters), `importance`, `fits` (final models trained on all
#'   rows, for prediction), `errors`, `data` and `settings`.
#' @export
ml_fit <- function(data, models = NULL, validation = c("animal", "random", "study"),
                   folds = 5, tuning = c("quick", "thorough"), upsample = TRUE, seed = 1, progress = NULL,
                   parallel = FALSE) {
  ml_check_packages()
  validation <- match.arg(validation)
  tuning <- match.arg(tuning)
  info <- attr(data, "ml")
  type <- if (is.numeric(data$outcome)) "regression" else "classification"
  info$type <- type
  available <- names(ml_model_names(type))
  models <- if (is.null(models)) available else intersect(models, available)
  if (length(models) == 0) stop("None of the selected models can be used for this outcome.")
  classif <- type == "classification"
  labels <- if (classif) levels(data$outcome) else NULL
  pos_col <- if (classif) paste0(".pred_", labels[1]) else ".pred"
  id_cols <- intersect(c("subject", "group", "timepoint", "study"), names(data))
  set.seed(seed)
  if (parallel && requireNamespace("future", quietly = TRUE)) {
    old_plan <- future::plan(future::multisession, workers = max(1, parallel::detectCores() - 1))
    on.exit(future::plan(old_plan), add = TRUE)
  }

  rec <- recipes::recipe(outcome ~ ., data = data)
  rec <- recipes::update_role(rec, dplyr::all_of(id_cols), new_role = "id")
  rec <- recipes::step_dummy(rec, recipes::all_nominal_predictors())
  rec <- recipes::step_zv(rec, recipes::all_predictors())
  rec <- recipes::step_normalize(rec, recipes::all_numeric_predictors())
  if (classif && upsample) rec <- themis::step_upsample(rec, "outcome", over_ratio = 1, seed = seed)
  n_pred <- ncol(recipes::bake(recipes::prep(rec, training = data), new_data = NULL, recipes::all_predictors()))

  # Stratify folds by outcome only when the outcome is constant within each animal.
  per_animal <- classif && all(tapply(as.character(data$outcome), data$subject, function(x) length(unique(x))) == 1)
  if (validation == "study") {
    if (!"study" %in% names(data)) stop("Study-level validation needs data from several studies (see ml_prepare_library()).")
    v <- length(unique(data$study))
    if (v < 2) stop("Leave-one-study-out validation needs at least 2 studies.")
    resamples <- rsample::group_vfold_cv(data, group = "study", v = v)
    train <- data
    test <- NULL
  } else if (validation == "animal") {
    n_animals <- length(unique(data$subject))
    v <- if (per_animal) min(folds, min(table(data$outcome[!duplicated(data$subject)]))) else min(folds, n_animals)
    v <- max(2, v)
    resamples <- if (per_animal) rsample::group_vfold_cv(data, group = "subject", v = v, strata = "outcome")
                 else rsample::group_vfold_cv(data, group = "subject", v = v)
    train <- data
    test <- NULL
  } else {
    split <- rsample::initial_split(data, prop = 0.8, strata = if (classif) "outcome" else NULL)
    train <- rsample::training(split)
    test <- rsample::testing(split)
    resamples <- rsample::vfold_cv(train, v = 10, strata = if (classif) "outcome" else NULL)
  }
  mset <- if (classif) yardstick::metric_set(yardstick::roc_auc, yardstick::accuracy)
          else yardstick::metric_set(yardstick::rmse, yardstick::rsq_trad)
  select_metric <- if (classif) "roc_auc" else "rmse"
  specs <- ml_specs(type)[models]
  names_all <- ml_model_names("all")

  metrics <- list(); preds <- list(); tuned <- list(); imps <- list(); fits <- list(); errors <- list()
  for (i in seq_along(models)) {
    m <- models[i]
    if (is.function(progress)) progress(i, length(models), names_all[[m]])
    res <- tryCatch({
      wf <- workflows::add_recipe(workflows::add_model(workflows::workflow(), specs[[m]]), rec)
      grid <- ml_grid(m, n_pred, tuning)
      if (is.null(grid)) {
        rs <- tune::fit_resamples(wf, resamples, metrics = mset, control = tune::control_resamples(save_pred = TRUE))
        best <- NULL
        final_wf <- wf
      } else {
        rs <- tune::tune_grid(wf, resamples, grid = grid, metrics = mset, control = tune::control_grid(save_pred = TRUE))
        best <- tune::select_best(rs, metric = select_metric)
        final_wf <- tune::finalize_workflow(wf, best)
        # Permutation importance is only needed for the final model (slow during tuning).
        if (m == "rf") {
          final_wf <- workflows::update_model(final_wf, parsnip::set_engine(workflows::extract_spec_parsnip(final_wf), "ranger", importance = "permutation"))
        }
      }
      cv <- tune::collect_metrics(rs)
      if (!is.null(best)) cv <- merge(cv, best[, setdiff(names(best), ".config"), drop = FALSE])
      cv_score <- cv$mean[cv$.metric == select_metric][1]

      if (validation %in% c("animal", "study")) {
        p <- if (is.null(best)) tune::collect_predictions(rs) else tune::collect_predictions(rs, parameters = best)
        p <- p[order(p$.row), , drop = FALSE]
        held <- if (classif) cbind(data[p$.row, id_cols], outcome = p$outcome, prob = p[[pos_col]], pred = p$.pred_class)
                else cbind(data[p$.row, id_cols], outcome = p$outcome, prob = NA_real_, pred = p$.pred)
      } else {
        test_fit <- parsnip::fit(final_wf, train)
        if (classif) {
          pr <- stats::predict(test_fit, test, type = "prob")
          held <- cbind(test[, id_cols], outcome = test$outcome, prob = pr[[pos_col]],
                        pred = factor(ifelse(pr[[pos_col]] >= 0.5, labels[1], labels[2]), levels = labels))
        } else {
          pr <- stats::predict(test_fit, test)
          held <- cbind(test[, id_cols], outcome = test$outcome, prob = NA_real_, pred = pr$.pred)
        }
      }
      final_fit <- parsnip::fit(final_wf, data)
      list(held = held, best = best, cv_score = cv_score, fit = final_fit)
    }, error = function(e) e)

    if (inherits(res, "error")) {
      errors[[m]] <- conditionMessage(res)
      next
    }
    held <- res$held
    held$model <- m
    preds[[m]] <- held
    if (classif) {
      animal <- stats::aggregate(prob ~ subject + outcome, data = held, FUN = mean)
      animal$pred <- factor(ifelse(animal$prob >= 0.5, labels[1], labels[2]), levels = labels)
      m_ses <- ml_metrics(held$outcome, held$prob, held$pred)
      m_ani <- ml_metrics(animal$outcome, animal$prob, animal$pred)
    } else {
      animal <- stats::aggregate(cbind(outcome, pred) ~ subject, data = held, FUN = mean)
      m_ses <- ml_metrics_reg(held$outcome, held$pred)
      m_ani <- ml_metrics_reg(animal$outcome, animal$pred)
    }
    metrics[[m]] <- rbind(cbind(model = m, level = "session", m_ses, cv_score = res$cv_score),
                          cbind(model = m, level = "animal", m_ani, cv_score = NA_real_))
    tuned[[m]] <- if (is.null(res$best)) data.frame(model = m, parameters = "none")
                  else data.frame(model = m, parameters = paste(sprintf("%s = %s", names(res$best)[names(res$best) != ".config"],
                                                                        signif(unlist(res$best[names(res$best) != ".config"]), 3)),
                                                                collapse = ", "))
    imps[[m]] <- ml_importance(m, res$fit, res$best)
    fits[[m]] <- butcher_fit(res$fit)
  }

  if (length(metrics) == 0) stop("No model could be fitted: ", paste(unlist(errors), collapse = "; "))
  metrics <- do.call(rbind, metrics)
  rownames(metrics) <- NULL
  metrics$model_name <- unname(names_all[metrics$model])
  structure(list(
    metrics = metrics,
    predictions = do.call(rbind, preds),
    tuning = do.call(rbind, tuned),
    importance = do.call(rbind, imps),
    fits = fits,
    errors = errors,
    data = data,
    settings = c(info, list(validation = validation, folds = if (validation %in% c("animal", "study")) v else 10,
                            tuning = tuning, upsample = classif && upsample, seed = seed, models = models,
                            n_train = nrow(train), n_test = if (is.null(test)) NA else nrow(test)))
  ), class = "plethr_ml")
}

# Drop the training data stored inside fitted workflows to keep saved models small.
butcher_fit <- function(fit) {
  fit$pre$mold$predictors <- fit$pre$mold$predictors[0, , drop = FALSE]
  fit$pre$mold$outcomes <- fit$pre$mold$outcomes[0, , drop = FALSE]
  fit
}

ml_outcome_label <- function(object) {
  s <- object$settings
  if (ml_type(object) == "regression") s$outcome_name %||% "outcome" else paste(s$labels[1], "vs", s$labels[2])
}

#' @export
print.plethr_ml <- function(x, ...) {
  s <- x$settings
  cat("plethR prediction models:", ml_outcome_label(x), "\n")
  cat("Validation:", switch(s$validation, animal = paste0("animals held out (", s$folds, "-fold)"), study = paste0("studies held out (", s$folds, " studies)"), "random 80/20 rows"), "\n")
  m <- x$metrics
  cols <- if (ml_type(x) == "regression") c("rsq", "rmse", "mae", "r") else c("roc_auc", "accuracy", "sensitivity", "specificity")
  m[, cols] <- round(m[, cols], 3)
  print(m[, c("model_name", "level", cols, "n")], row.names = FALSE)
  if (length(x$errors)) cat("Failed:", paste(names(x$errors), collapse = ", "), "\n")
  invisible(x)
}

#' Predict New Animals
#'
#' Applies trained models to new session data, e.g. a new experiment processed
#' with [read_wbp()] and [summarize_sessions()].
#'
#' @param object Output of [ml_fit()].
#' @param sessions Session data for the new animals (groups are not needed).
#' @param model Model code to use, e.g. `"rf"`.
#' @param covariate If the models were trained with a covariate, a named vector
#'   mapping each new animal (subject) to its covariate level.
#' @param exclude_timepoints Timepoints to leave out. Defaults to the ones excluded in training.
#' @return A list with `sessions` (prediction for each animal and session: the
#'   probability of the first class, or the predicted value) and `animals` (mean
#'   per animal, with the predicted class for classification).
#' @export
ml_predict <- function(object, sessions, model, covariate = NULL,
                       exclude_timepoints = object$settings$exclude_timepoints) {
  ml_check_packages()
  s <- object$settings
  classif <- ml_type(object) == "classification"
  if (!model %in% names(object$fits)) stop("Model ", model, " was not trained.")
  sessions <- sessions[!as.character(sessions$timepoint) %in% exclude_timepoints, , drop = FALSE]
  missing <- setdiff(s$parameters, names(sessions))
  if (length(missing)) stop("The new data is missing these parameters: ", paste(missing, collapse = ", "))
  d <- data.frame(subject = sessions$subject, group = if ("group" %in% names(sessions)) as.character(sessions$group) else NA_character_,
                  timepoint = as.character(sessions$timepoint),
                  outcome = if (classif) factor(NA, levels = s$labels) else NA_real_,
                  sessions[, s$parameters, drop = FALSE], check.names = FALSE, stringsAsFactors = FALSE)
  if (isTRUE(s$include_day)) d$day <- sessions$day
  if (!is.null(s$covariate)) {
    if (is.null(covariate)) stop("These models use ", s$covariate_name, "; give its level for each new animal.")
    d[[s$covariate_name]] <- factor(unname(covariate[d$subject]), levels = sort(unique(unname(s$covariate))))
  }
  keep <- stats::complete.cases(d[, setdiff(names(d), c("group", "outcome")), drop = FALSE])
  d <- d[keep, , drop = FALSE]
  tp <- factor(d$timepoint, levels = unique(as.character(sessions$timepoint)))
  if (classif) {
    pr <- stats::predict(object$fits[[model]], d, type = "prob")
    out <- data.frame(subject = d$subject, timepoint = tp, prob = pr[[paste0(".pred_", s$labels[1])]])
    animals <- stats::aggregate(prob ~ subject, data = out, FUN = mean)
    animals$n_sessions <- as.vector(table(out$subject)[animals$subject])
    animals$predicted <- ifelse(animals$prob >= 0.5, s$labels[1], s$labels[2])
    names(animals)[names(animals) == "prob"] <- paste0("p_", s$labels[1])
  } else {
    pr <- stats::predict(object$fits[[model]], d)
    out <- data.frame(subject = d$subject, timepoint = tp, prob = pr$.pred)
    animals <- stats::aggregate(prob ~ subject, data = out, FUN = mean)
    animals$n_sessions <- as.vector(table(out$subject)[animals$subject])
    names(animals)[names(animals) == "prob"] <- paste0("predicted_", make.names(s$outcome_name %||% "value"))
  }
  list(sessions = out, animals = animals)
}

# ---- Plots -------------------------------------------------------------------

#' Plot Model Performance
#'
#' ROC AUC (classification) or held-out R-squared (regression) for every model,
#' at session and animal level.
#'
#' @param object Output of [ml_fit()].
#' @return A ggplot object.
#' @export
plot_ml_performance <- function(object) {
  m <- object$metrics
  reg <- ml_type(object) == "regression"
  m$score <- if (reg) m$rsq else m$roc_auc
  ses <- m[m$level == "session", ]
  m$model_name <- factor(m$model_name, levels = ses$model_name[order(ses$score)])
  m$level <- factor(ifelse(m$level == "session", "Each session", "Each animal (mean of its sessions)"),
                    levels = c("Each session", "Each animal (mean of its sessions)"))
  val <- if (object$settings$validation == "animal") "held-out animals" else "random 20% test rows"
  lo <- if (reg) min(-0.25, floor(min(m$score, na.rm = TRUE) * 4) / 4) else 0
  ggplot2::ggplot(m, ggplot2::aes(x = .data$score, y = .data$model_name, color = .data$level)) +
    ggplot2::geom_vline(xintercept = if (reg) 0 else 0.5, linetype = "dashed", color = "grey55") +
    ggplot2::geom_point(size = 3.5, position = ggplot2::position_dodge(width = 0.5)) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$score)), position = ggplot2::position_dodge(width = 0.5),
                       hjust = -0.45, size = 3.3, show.legend = FALSE) +
    ggplot2::scale_x_continuous(limits = c(lo, 1.08)) +
    ggplot2::scale_color_manual(values = c("#1F5F8B", "#D55E00")) +
    ggplot2::labs(title = paste("Model performance:", ml_outcome_label(object)),
                  x = if (reg) "Held-out R\u00b2 (0 = no better than the mean, 1 = perfect)" else "ROC AUC (0.5 = chance, 1 = perfect)",
                  y = NULL, caption = paste0("Evaluated on ", val)) +
    theme_plethr()
}

#' Plot ROC Curves
#'
#' @param object Output of [ml_fit()] for a categorical outcome.
#' @param level `"session"` or `"animal"`.
#' @param models Models to show. Defaults to all.
#' @return A ggplot object.
#' @export
plot_ml_roc <- function(object, level = c("session", "animal"), models = NULL) {
  level <- match.arg(level)
  if (ml_type(object) == "regression") stop("ROC curves are for categorical outcomes; use plot_ml_observed() for severity.")
  p <- object$predictions
  if (!is.null(models)) p <- p[p$model %in% models, , drop = FALSE]
  if (level == "animal") p <- stats::aggregate(prob ~ subject + outcome + model, data = p, FUN = mean)
  curves <- do.call(rbind, lapply(split(p, p$model), function(d) {
    rc <- yardstick::roc_curve(d, truth = "outcome", "prob")
    data.frame(model = d$model[1], fpr = 1 - rc$specificity, tpr = rc$sensitivity)
  }))
  auc <- object$metrics[object$metrics$level == level, ]
  curves$label <- factor(sprintf("%s (AUC %.2f)", ml_model_names("all")[curves$model], auc$roc_auc[match(curves$model, auc$model)]))
  ggplot2::ggplot(curves, ggplot2::aes(x = .data$fpr, y = .data$tpr, color = .data$label)) +
    ggplot2::geom_abline(linetype = "dashed", color = "grey60") +
    ggplot2::geom_path(linewidth = 0.9) +
    ggplot2::coord_equal() +
    ggplot2::scale_color_manual(values = unname(plethr_colors(levels(curves$label)))) +
    ggplot2::labs(title = paste("ROC curves:", if (level == "session") "each session" else "each animal"),
                  x = "False positive rate (1 \u2212 specificity)", y = "True positive rate (sensitivity)") +
    theme_plethr() +
    ggplot2::theme(legend.position = "right")
}

#' Plot a Confusion Matrix
#'
#' @param object Output of [ml_fit()] for a categorical outcome.
#' @param model Model code.
#' @param level `"session"` or `"animal"`.
#' @return A ggplot object.
#' @export
plot_ml_confusion <- function(object, model, level = c("session", "animal")) {
  level <- match.arg(level)
  if (ml_type(object) == "regression") stop("Confusion matrices are for categorical outcomes.")
  p <- object$predictions[object$predictions$model == model, , drop = FALSE]
  labels <- object$settings$labels
  if (level == "animal") {
    p <- stats::aggregate(prob ~ subject + outcome, data = p, FUN = mean)
    p$pred <- factor(ifelse(p$prob >= 0.5, labels[1], labels[2]), levels = labels)
  }
  tab <- as.data.frame(table(Truth = p$outcome, Predicted = p$pred))
  tab$pct <- tab$Freq / stats::ave(tab$Freq, tab$Truth, FUN = sum) * 100
  tab$correct <- tab$Truth == tab$Predicted
  ggplot2::ggplot(tab, ggplot2::aes(x = .data$Predicted, y = .data$Truth, fill = ifelse(.data$correct, .data$pct, -.data$pct))) +
    ggplot2::geom_tile(color = "white", linewidth = 1) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%d\n(%.0f%%)", .data$Freq, .data$pct)), size = 4.5, lineheight = 0.9) +
    ggplot2::scale_fill_gradient2(low = "#B2182B", mid = "#F7F7F7", high = "#2166AC", limits = c(-100, 100), guide = "none") +
    ggplot2::scale_y_discrete(limits = rev(labels)) +
    ggplot2::labs(title = paste0(ml_model_names("all")[[model]], ": ", if (level == "session") "sessions" else "animals"),
                  x = "Predicted", y = "Actual", caption = "Percent of each actual class") +
    theme_plethr() +
    ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank())
}

#' Plot Observed vs Predicted Values
#'
#' For severity (regression) models: held-out predictions against measured values.
#'
#' @param object Output of [ml_fit()] for a numeric outcome.
#' @param model Model code.
#' @param level `"session"` or `"animal"` (means per animal).
#' @param colors Named group colors.
#' @return A ggplot object.
#' @export
plot_ml_observed <- function(object, model, level = c("session", "animal"), colors = NULL) {
  level <- match.arg(level)
  if (ml_type(object) != "regression") stop("Observed vs predicted plots are for numeric outcomes.")
  p <- object$predictions[object$predictions$model == model, , drop = FALSE]
  if (level == "animal") p <- stats::aggregate(cbind(outcome, pred) ~ subject + group, data = p, FUN = mean)
  if (is.null(colors)) colors <- plethr_colors(unique(p$group))
  met <- object$metrics[object$metrics$model == model & object$metrics$level == level, ]
  rng <- range(c(p$outcome, p$pred), na.rm = TRUE)
  ggplot2::ggplot(p, ggplot2::aes(x = .data$outcome, y = .data$pred, color = .data$group)) +
    ggplot2::geom_abline(linetype = "dashed", color = "grey55") +
    ggplot2::geom_point(size = if (level == "animal") 3 else 2, alpha = if (level == "animal") 1 else 0.6) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::coord_equal(xlim = rng, ylim = rng) +
    ggplot2::labs(title = paste0(ml_model_names("all")[[model]], ": ", if (level == "session") "each session" else "each animal"),
                  x = paste("Measured", object$settings$outcome_name), y = paste("Predicted", object$settings$outcome_name),
                  caption = sprintf("Held-out R\u00b2 = %.2f, r = %.2f, RMSE = %.3g; dashed line = perfect prediction",
                                    met$rsq, met$r, met$rmse)) +
    theme_plethr() +
    ggplot2::theme(legend.position = "right")
}

#' Plot Predictions over Time
#'
#' Mean held-out prediction (probability of the first class, or predicted value)
#' for each group at each timepoint.
#'
#' @param object Output of [ml_fit()].
#' @param model Model code.
#' @param colors Named group colors.
#' @return A ggplot object.
#' @export
plot_ml_over_time <- function(object, model, colors = NULL) {
  reg <- ml_type(object) == "regression"
  p <- object$predictions[object$predictions$model == model, , drop = FALSE]
  lv <- object$settings$timepoints
  lv <- lv[lv %in% p$timepoint]
  val <- if (reg) p$pred else p$prob
  p$val <- val
  s <- p %>%
    dplyr::group_by(.data$group, .data$timepoint) %>%
    dplyr::summarize(mean = mean(.data$val), sem = if (dplyr::n() > 1) stats::sd(.data$val) / sqrt(dplyr::n()) else 0,
                     n = dplyr::n(), .groups = "drop")
  s$x <- match(s$timepoint, lv)
  if (is.null(colors)) colors <- plethr_colors(unique(s$group))
  colors <- colors[names(colors) %in% s$group]
  lab <- object$settings$labels
  g <- ggplot2::ggplot(s, ggplot2::aes(x = .data$x, y = .data$mean, color = .data$group, group = .data$group))
  if (!reg) g <- g + ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey55")
  g <- g +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$mean - .data$sem, ymax = .data$mean + .data$sem, fill = .data$group),
                         alpha = 0.15, color = NA) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::scale_color_manual(values = colors, breaks = names(colors)) +
    ggplot2::scale_fill_manual(values = colors, guide = "none") +
    ggplot2::scale_x_continuous(breaks = seq_along(lv), labels = lv, expand = ggplot2::expansion(add = 0.4))
  if (!reg) g <- g + ggplot2::scale_y_continuous(labels = function(x) paste0(x * 100, "%")) + ggplot2::coord_cartesian(ylim = c(0, 1))
  g + ggplot2::labs(
      title = if (reg) paste0("Predicted ", object$settings$outcome_name, " over time") else paste0("Predicted probability of \"", lab[1], "\" over time"),
      subtitle = ml_model_names("all")[[model]], x = NULL,
      y = if (reg) paste("Predicted", object$settings$outcome_name) else paste0("P(", lab[1], ")"),
      caption = paste0("Mean \u00b1 SEM of held-out predictions across animals", if (!reg) "; dashed line = 50%" else "")) +
    theme_plethr() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = if (length(lv) > 6) 45 else 0, hjust = if (length(lv) > 6) 1 else 0.5))
}

#' Plot Variable Importance
#'
#' @param object Output of [ml_fit()].
#' @param model Model code (`"rf"`, `"bt"`, `"en"`, `"log"` or `"lm"`).
#' @param n Number of features to show.
#' @return A ggplot object.
#' @export
plot_ml_importance <- function(object, model, n = 15) {
  imp <- object$importance
  imp <- imp[imp$model == model, , drop = FALSE]
  if (is.null(imp) || nrow(imp) == 0) stop("No variable importance for ", ml_model_names("all")[[model]], ".")
  imp <- utils::head(imp[order(-imp$importance), , drop = FALSE], n)
  imp$feature <- factor(imp$feature, levels = rev(imp$feature))
  how <- switch(model, rf = "permutation importance", bt = "gain", "|standardized coefficient|")
  ggplot2::ggplot(imp, ggplot2::aes(x = .data$importance, y = .data$feature)) +
    ggplot2::geom_col(fill = "#1F5F8B", width = 0.7) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(title = paste0("Variable importance: ", ml_model_names("all")[[model]]),
                  x = paste0("Relative importance (", how, ", max = 100)"), y = NULL) +
    theme_plethr()
}
