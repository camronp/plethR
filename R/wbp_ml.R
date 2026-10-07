# Machine learning classification of animals (e.g. infected vs uninfected) from
# session-level WBP data, built on tidymodels. Adapted from the CP05
# infection-prediction pipeline (7 models, centered/scaled predictors, upsampling,
# tuning by ROC AUC), with validation that keeps each animal's sessions together.
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
#' @return A named character vector: short codes (names) and full model names.
#' @examples
#' ml_model_names()
#' @export
ml_model_names <- function() {
  c(log = "Logistic regression", knn = "K-nearest neighbors", lda = "Linear discriminant analysis",
    qda = "Quadratic discriminant analysis", en = "Elastic net", rf = "Random forest",
    bt = "Gradient-boosted trees")
}

#' Prepare Session Data for Classification
#'
#' Builds one row per animal and session with the outcome (positive vs negative
#' class), the respiratory parameters as predictors, and optional covariates.
#'
#' @param sessions Output of [summarize_sessions()] with groups assigned.
#' @param positive Groups forming the positive class (e.g. the infected groups).
#' @param negative Groups forming the negative class. Defaults to all other groups.
#' @param labels Names of the positive and negative classes.
#' @param parameters Respiratory parameters to use as predictors. Defaults to all.
#' @param exclude_timepoints Timepoints to leave out, e.g. the pre-infection
#'   baseline, when positive animals are not yet affected.
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
  if (identical(labels[1], labels[2])) stop("The two classes need different names.")

  s <- sessions[as.character(sessions$group) %in% c(positive, negative), , drop = FALSE]
  s <- s[!as.character(s$timepoint) %in% exclude_timepoints, , drop = FALSE]
  d <- data.frame(subject = s$subject, group = as.character(s$group), timepoint = as.character(s$timepoint),
                  outcome = factor(ifelse(as.character(s$group) %in% positive, labels[1], labels[2]), levels = labels),
                  s[, parameters, drop = FALSE], check.names = FALSE, stringsAsFactors = FALSE)
  if (include_day) d$day <- s$day
  if (!is.null(covariate)) {
    d[[covariate_name]] <- factor(unname(covariate[d$group]))
  }
  n0 <- nrow(d)
  d <- d[stats::complete.cases(d), , drop = FALSE]
  rownames(d) <- NULL
  attr(d, "ml") <- list(parameters = parameters, labels = labels, positive = positive, negative = negative,
                        include_day = include_day, covariate = covariate, covariate_name = covariate_name,
                        exclude_timepoints = exclude_timepoints, timepoints = levels(droplevels(s$timepoint)),
                        dropped_rows = n0 - nrow(d))
  d
}

ml_specs <- function() {
  loadNamespace("discrim")  # registers the MASS engines for discriminant analysis
  list(
    log = parsnip::set_mode(parsnip::set_engine(parsnip::logistic_reg(), "glm"), "classification"),
    knn = parsnip::set_mode(parsnip::set_engine(parsnip::nearest_neighbor(neighbors = tune::tune()), "kknn"), "classification"),
    lda = parsnip::set_mode(parsnip::set_engine(parsnip::discrim_linear(), "MASS"), "classification"),
    qda = parsnip::set_mode(parsnip::set_engine(parsnip::discrim_quad(), "MASS"), "classification"),
    en = parsnip::set_mode(parsnip::set_engine(parsnip::logistic_reg(penalty = tune::tune(), mixture = tune::tune()), "glmnet"),
                           "classification"),
    rf = parsnip::set_mode(parsnip::set_engine(parsnip::rand_forest(mtry = tune::tune(), trees = tune::tune(), min_n = tune::tune()),
                                               "ranger"), "classification"),
    bt = parsnip::set_mode(parsnip::set_engine(parsnip::boost_tree(mtry = tune::tune(), trees = tune::tune(),
                                                                   learn_rate = tune::tune()), "xgboost"), "classification")
  )
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

# Binary classification metrics; the positive class is the first factor level.
ml_metrics <- function(truth, prob, cls) {
  d <- data.frame(truth = truth, prob = prob, cls = factor(cls, levels = levels(truth)))
  data.frame(
    roc_auc = if (length(unique(d$truth)) == 2) yardstick::roc_auc_vec(d$truth, d$prob) else NA_real_,
    accuracy = yardstick::accuracy_vec(d$truth, d$cls),
    sensitivity = yardstick::sens_vec(d$truth, d$cls),
    specificity = yardstick::spec_vec(d$truth, d$cls),
    n = nrow(d)
  )
}

ml_importance <- function(model, fit, best) {
  eng <- workflows::extract_fit_engine(fit)
  feats <- NULL
  imp <- tryCatch(switch(model,
    rf = { v <- eng$variable.importance; feats <- names(v); unname(v) },
    bt = { v <- xgboost::xgb.importance(model = eng); feats <- v$Feature; v$Gain },
    log = { co <- stats::coef(eng)[-1]; feats <- names(co); abs(unname(co)) },
    en = { co <- as.matrix(stats::coef(eng, s = best$penalty))[-1, 1]; feats <- names(co); abs(unname(co)) },
    NULL), error = function(e) NULL)
  if (is.null(imp) || !length(imp)) return(NULL)
  imp[!is.finite(imp)] <- NA
  out <- data.frame(model = model, feature = feats, importance = imp, stringsAsFactors = FALSE)
  out <- out[!is.na(out$importance), , drop = FALSE]
  if (nrow(out) && max(out$importance) > 0) out$importance <- out$importance / max(out$importance) * 100
  out[order(-out$importance), , drop = FALSE]
}

#' Train and Evaluate Classification Models
#'
#' Fits up to seven models (logistic regression, k-nearest neighbors, linear and
#' quadratic discriminant analysis, elastic net, random forest, gradient-boosted
#' trees). Predictors are centered and scaled and the minority class is upsampled
#' in training data. Tunable models are tuned by ROC AUC.
#'
#' Two validation schemes are available:
#' * `"animal"` (recommended): cross-validation in which all sessions of an animal
#'   are held out together, so models are always evaluated on animals they have
#'   never seen. Performance is computed from the out-of-fold predictions.
#' * `"random"`: the original CP05 approach. Rows (animal sessions) are split
#'   80/20 at random and models are tuned by 10-fold cross-validation on the
#'   training rows. Because sessions of the same animal end up in both training
#'   and test data, models can recognize individual animals and performance is
#'   usually optimistic.
#'
#' @param data Output of [ml_prepare()].
#' @param models Model codes from [ml_model_names()].
#' @param validation `"animal"` or `"random"`.
#' @param folds Number of cross-validation folds for `"animal"` validation.
#' @param tuning `"quick"` (small grids) or `"thorough"` (the original CP05 grids; slow).
#' @param upsample Upsample the minority class in training data.
#' @param seed Random seed.
#' @param progress Optional function called as `progress(i, n, model_name)`.
#' @param parallel Tune on several CPU cores (needs the future package).
#' @return An object of class `plethr_ml`: a list with `metrics` (per model, at
#'   session and animal level), `predictions` (held-out predictions with ids),
#'   `tuning` (best parameters), `importance`, `fits` (final models trained on all
#'   rows, for prediction), `errors`, `data` and `settings`.
#' @export
ml_fit <- function(data, models = names(ml_model_names()), validation = c("animal", "random"),
                   folds = 5, tuning = c("quick", "thorough"), upsample = TRUE, seed = 1, progress = NULL,
                   parallel = FALSE) {
  ml_check_packages()
  validation <- match.arg(validation)
  tuning <- match.arg(tuning)
  info <- attr(data, "ml")
  labels <- levels(data$outcome)
  pos_col <- paste0(".pred_", labels[1])
  id_cols <- c("subject", "group", "timepoint")
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
  if (upsample) rec <- themis::step_upsample(rec, "outcome", over_ratio = 1, seed = seed)
  n_pred <- ncol(recipes::bake(recipes::prep(rec, training = data), new_data = NULL,
                               recipes::all_predictors()))

  if (validation == "animal") {
    n_animals <- length(unique(data$subject))
    v <- max(2, min(folds, min(table(data$outcome[!duplicated(data$subject)]))))
    resamples <- rsample::group_vfold_cv(data, group = "subject", v = v, strata = "outcome")
    train <- data
    test <- NULL
  } else {
    split <- rsample::initial_split(data, prop = 0.8, strata = "outcome")
    train <- rsample::training(split)
    test <- rsample::testing(split)
    resamples <- rsample::vfold_cv(train, v = 10, strata = "outcome")
  }
  mset <- yardstick::metric_set(yardstick::roc_auc, yardstick::accuracy)
  specs <- ml_specs()[models]

  metrics <- list(); preds <- list(); tuned <- list(); imps <- list(); fits <- list(); errors <- list()
  for (i in seq_along(models)) {
    m <- models[i]
    if (is.function(progress)) progress(i, length(models), ml_model_names()[[m]])
    res <- tryCatch({
      wf <- workflows::add_recipe(workflows::add_model(workflows::workflow(), specs[[m]]), rec)
      grid <- ml_grid(m, n_pred, tuning)
      if (is.null(grid)) {
        rs <- tune::fit_resamples(wf, resamples, metrics = mset, control = tune::control_resamples(save_pred = TRUE))
        best <- NULL
        final_wf <- wf
      } else {
        rs <- tune::tune_grid(wf, resamples, grid = grid, metrics = mset, control = tune::control_grid(save_pred = TRUE))
        best <- tune::select_best(rs, metric = "roc_auc")
        final_wf <- tune::finalize_workflow(wf, best)
        # Permutation importance is only needed for the final model (slow during tuning).
        if (m == "rf") {
          final_wf <- workflows::update_model(final_wf, parsnip::set_engine(workflows::extract_spec_parsnip(final_wf), "ranger", importance = "permutation"))
        }
      }
      cv <- tune::collect_metrics(rs)
      if (!is.null(best)) cv <- merge(cv, best[, setdiff(names(best), ".config"), drop = FALSE])
      cv_auc <- cv$mean[cv$.metric == "roc_auc"][1]

      if (validation == "animal") {
        p <- if (is.null(best)) tune::collect_predictions(rs) else tune::collect_predictions(rs, parameters = best)
        p <- p[order(p$.row), , drop = FALSE]
        held <- cbind(data[p$.row, id_cols], outcome = p$outcome, prob = p[[pos_col]], pred = p$.pred_class)
      } else {
        test_fit <- parsnip::fit(final_wf, train)
        pr <- stats::predict(test_fit, test, type = "prob")
        held <- cbind(test[, id_cols], outcome = test$outcome, prob = pr[[pos_col]],
                      pred = factor(ifelse(pr[[pos_col]] >= 0.5, labels[1], labels[2]), levels = labels))
      }
      final_fit <- parsnip::fit(final_wf, data)
      list(held = held, best = best, cv_auc = cv_auc, fit = final_fit)
    }, error = function(e) e)

    if (inherits(res, "error")) {
      errors[[m]] <- conditionMessage(res)
      next
    }
    held <- res$held
    held$model <- m
    preds[[m]] <- held
    animal <- stats::aggregate(prob ~ subject + outcome, data = held, FUN = mean)
    animal$pred <- factor(ifelse(animal$prob >= 0.5, labels[1], labels[2]), levels = labels)
    metrics[[m]] <- rbind(
      cbind(model = m, level = "session", ml_metrics(held$outcome, held$prob, held$pred), cv_roc_auc = res$cv_auc),
      cbind(model = m, level = "animal", ml_metrics(animal$outcome, animal$prob, animal$pred), cv_roc_auc = NA_real_)
    )
    tuned[[m]] <- if (is.null(res$best)) data.frame(model = m, parameters = "none")
                  else data.frame(model = m, parameters = paste(sprintf("%s = %s", names(res$best)[names(res$best) != ".config"],
                                                                        signif(unlist(res$best[names(res$best) != ".config"]), 3)),
                                                                collapse = ", "))
    imps[[m]] <- ml_importance(m, res$fit, res$best)
    fits[[m]] <- butcher_fit(res$fit)
  }

  if (length(metrics) == 0) stop("No model could be fitted: ", paste(unlist(errors), collapse = "; "))
  metrics <- do.call(rbind, metrics)
  metrics$model_name <- unname(ml_model_names()[metrics$model])
  structure(list(
    metrics = metrics,
    predictions = do.call(rbind, preds),
    tuning = do.call(rbind, tuned),
    importance = do.call(rbind, imps),
    fits = fits,
    errors = errors,
    data = data,
    settings = c(info, list(validation = validation, folds = if (validation == "animal") v else 10,
                            tuning = tuning, upsample = upsample, seed = seed, models = models,
                            n_train = nrow(train), n_test = if (is.null(test)) NA else nrow(test)))
  ), class = "plethr_ml")
}

# Drop the training data stored inside fitted workflows to keep saved models small.
butcher_fit <- function(fit) {
  fit$pre$mold$predictors <- fit$pre$mold$predictors[0, , drop = FALSE]
  fit$pre$mold$outcomes <- fit$pre$mold$outcomes[0, , drop = FALSE]
  fit
}

#' @export
print.plethr_ml <- function(x, ...) {
  s <- x$settings
  cat("plethR classification models:", s$labels[1], "vs", s$labels[2], "\n")
  cat("Validation:", if (s$validation == "animal") paste0("animals held out (", s$folds, "-fold)") else "random 80/20 rows", "\n")
  m <- x$metrics
  m[, c("roc_auc", "accuracy", "sensitivity", "specificity")] <- round(m[, c("roc_auc", "accuracy", "sensitivity", "specificity")], 3)
  print(m[, c("model_name", "level", "roc_auc", "accuracy", "sensitivity", "specificity", "n")], row.names = FALSE)
  if (length(x$errors)) cat("Failed:", paste(names(x$errors), collapse = ", "), "\n")
  invisible(x)
}

#' Predict the Class of New Animals
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
#' @return A list with `sessions` (probability of the positive class for each animal
#'   and session) and `animals` (mean probability per animal and predicted class).
#' @export
ml_predict <- function(object, sessions, model, covariate = NULL,
                       exclude_timepoints = object$settings$exclude_timepoints) {
  ml_check_packages()
  s <- object$settings
  if (!model %in% names(object$fits)) stop("Model ", model, " was not trained.")
  sessions <- sessions[!as.character(sessions$timepoint) %in% exclude_timepoints, , drop = FALSE]
  missing <- setdiff(s$parameters, names(sessions))
  if (length(missing)) stop("The new data is missing these parameters: ", paste(missing, collapse = ", "))
  d <- data.frame(subject = sessions$subject, group = if ("group" %in% names(sessions)) as.character(sessions$group) else NA_character_,
                  timepoint = as.character(sessions$timepoint),
                  outcome = factor(NA, levels = s$labels),
                  sessions[, s$parameters, drop = FALSE], check.names = FALSE, stringsAsFactors = FALSE)
  if (isTRUE(s$include_day)) d$day <- sessions$day
  if (!is.null(s$covariate)) {
    if (is.null(covariate)) stop("These models use ", s$covariate_name, "; give its level for each new animal.")
    d[[s$covariate_name]] <- factor(unname(covariate[d$subject]), levels = sort(unique(unname(s$covariate))))
  }
  keep <- stats::complete.cases(d[, setdiff(names(d), c("group", "outcome")), drop = FALSE])
  d <- d[keep, , drop = FALSE]
  pr <- stats::predict(object$fits[[model]], d, type = "prob")
  out <- data.frame(subject = d$subject, timepoint = factor(d$timepoint, levels = unique(as.character(sessions$timepoint))),
                    prob = pr[[paste0(".pred_", s$labels[1])]])
  animals <- stats::aggregate(prob ~ subject, data = out, FUN = mean)
  animals$n_sessions <- as.vector(table(out$subject)[animals$subject])
  animals$predicted <- ifelse(animals$prob >= 0.5, s$labels[1], s$labels[2])
  names(animals)[names(animals) == "prob"] <- paste0("p_", s$labels[1])
  list(sessions = out, animals = animals)
}

# ---- Plots -------------------------------------------------------------------

model_colors <- function(models) {
  plethr_colors(models, "okabe-ito")
}

#' Plot Model Performance
#'
#' ROC AUC (with accuracy) for every model, at session and animal level.
#'
#' @param object Output of [ml_fit()].
#' @return A ggplot object.
#' @export
plot_ml_performance <- function(object) {
  m <- object$metrics
  ses <- m[m$level == "session", ]
  m$model_name <- factor(m$model_name, levels = ses$model_name[order(ses$roc_auc)])
  m$level <- factor(ifelse(m$level == "session", "Each session", "Each animal (mean of its sessions)"),
                    levels = c("Each session", "Each animal (mean of its sessions)"))
  val <- if (object$settings$validation == "animal") "held-out animals" else "random 20% test rows"
  ggplot2::ggplot(m, ggplot2::aes(x = .data$roc_auc, y = .data$model_name, color = .data$level)) +
    ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey55") +
    ggplot2::geom_point(size = 3.5, position = ggplot2::position_dodge(width = 0.5)) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$roc_auc)), position = ggplot2::position_dodge(width = 0.5),
                       hjust = -0.45, size = 3.3, show.legend = FALSE) +
    ggplot2::scale_x_continuous(limits = c(0, 1.08), breaks = seq(0, 1, 0.25)) +
    ggplot2::scale_color_manual(values = c("#1F5F8B", "#D55E00")) +
    ggplot2::labs(title = "Model performance", x = "ROC AUC (0.5 = chance, 1 = perfect)", y = NULL,
                  caption = paste0("Evaluated on ", val)) +
    theme_plethr()
}

#' Plot ROC Curves
#'
#' @param object Output of [ml_fit()].
#' @param level `"session"` or `"animal"`.
#' @param models Models to show. Defaults to all.
#' @return A ggplot object.
#' @export
plot_ml_roc <- function(object, level = c("session", "animal"), models = NULL) {
  level <- match.arg(level)
  p <- object$predictions
  if (!is.null(models)) p <- p[p$model %in% models, , drop = FALSE]
  if (level == "animal") p <- stats::aggregate(prob ~ subject + outcome + model, data = p, FUN = mean)
  curves <- do.call(rbind, lapply(split(p, p$model), function(d) {
    rc <- yardstick::roc_curve(d, truth = "outcome", "prob")
    data.frame(model = d$model[1], fpr = 1 - rc$specificity, tpr = rc$sensitivity)
  }))
  auc <- object$metrics[object$metrics$level == level, ]
  curves$label <- factor(sprintf("%s (AUC %.2f)", ml_model_names()[curves$model], auc$roc_auc[match(curves$model, auc$model)]))
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
#' @param object Output of [ml_fit()].
#' @param model Model code.
#' @param level `"session"` or `"animal"`.
#' @return A ggplot object.
#' @export
plot_ml_confusion <- function(object, model, level = c("session", "animal")) {
  level <- match.arg(level)
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
    ggplot2::labs(title = paste0(ml_model_names()[[model]], ": ", if (level == "session") "sessions" else "animals"),
                  x = "Predicted", y = "Actual", caption = "Percent of each actual class") +
    theme_plethr() +
    ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank())
}

#' Plot Predicted Probability over Time
#'
#' Mean held-out probability of the positive class for each group at each
#' timepoint, showing when the classes become distinguishable.
#'
#' @param object Output of [ml_fit()].
#' @param model Model code.
#' @param colors Named group colors.
#' @return A ggplot object.
#' @export
plot_ml_over_time <- function(object, model, colors = NULL) {
  p <- object$predictions[object$predictions$model == model, , drop = FALSE]
  lv <- object$settings$timepoints
  lv <- lv[lv %in% p$timepoint]
  s <- p %>%
    dplyr::group_by(.data$group, .data$timepoint) %>%
    dplyr::summarize(mean = mean(.data$prob), sem = if (dplyr::n() > 1) stats::sd(.data$prob) / sqrt(dplyr::n()) else 0,
                     n = dplyr::n(), .groups = "drop")
  s$x <- match(s$timepoint, lv)
  if (is.null(colors)) colors <- plethr_colors(unique(s$group))
  ggplot2::ggplot(s, ggplot2::aes(x = .data$x, y = .data$mean, color = .data$group, group = .data$group)) +
    ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey55") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = pmax(0, .data$mean - .data$sem), ymax = pmin(1, .data$mean + .data$sem), fill = .data$group),
                         alpha = 0.15, color = NA) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_fill_manual(values = colors, guide = "none") +
    ggplot2::scale_x_continuous(breaks = seq_along(lv), labels = lv, expand = ggplot2::expansion(add = 0.4)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(x * 100, "%")) +
    ggplot2::labs(title = paste0("Predicted probability of \"", object$settings$labels[1], "\" over time"),
                  subtitle = ml_model_names()[[model]], x = NULL, y = paste0("P(", object$settings$labels[1], ")"),
                  caption = "Mean \u00b1 SEM of held-out predictions across animals; dashed line = 50%") +
    theme_plethr() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = if (length(lv) > 6) 45 else 0, hjust = if (length(lv) > 6) 1 else 0.5))
}

#' Plot Variable Importance
#'
#' @param object Output of [ml_fit()].
#' @param model Model code (`"rf"`, `"bt"`, `"en"` or `"log"`).
#' @param n Number of features to show.
#' @return A ggplot object.
#' @export
plot_ml_importance <- function(object, model, n = 15) {
  imp <- object$importance
  imp <- imp[imp$model == model, , drop = FALSE]
  if (is.null(imp) || nrow(imp) == 0) stop("No variable importance for ", ml_model_names()[[model]], ".")
  imp <- utils::head(imp[order(-imp$importance), , drop = FALSE], n)
  imp$feature <- factor(imp$feature, levels = rev(imp$feature))
  how <- switch(model, rf = "permutation importance", bt = "gain", en = "|standardized coefficient|",
                log = "|standardized coefficient|")
  ggplot2::ggplot(imp, ggplot2::aes(x = .data$importance, y = .data$feature)) +
    ggplot2::geom_col(fill = "#1F5F8B", width = 0.7) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(title = paste0("Variable importance: ", ml_model_names()[[model]]),
                  x = paste0("Relative importance (", how, ", max = 100)"), y = NULL) +
    theme_plethr()
}
