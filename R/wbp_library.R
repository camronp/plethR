# Multi-study library: processed studies saved in a folder, combined for
# training models that are validated on studies they have never seen.
#
# Layout of a library folder:
#   index.csv            one row per study
#   studies/<id>.rds     session-level data, conditions and study information
#   models/index.csv     one row per saved model
#   models/<id>.rds      saved model (plethr_ml object)

lib_paths <- function(lib_dir) {
  list(index = file.path(lib_dir, "index.csv"), studies = file.path(lib_dir, "studies"),
       models = file.path(lib_dir, "models"), model_index = file.path(lib_dir, "models", "index.csv"))
}

safe_id <- function(x) gsub("[^A-Za-z0-9_.-]+", "_", trimws(x))

#' Create or Open a Study Library
#'
#' @param lib_dir Folder for the library (created if needed).
#' @return `lib_dir`, invisibly.
#' @export
library_init <- function(lib_dir) {
  p <- lib_paths(lib_dir)
  dir.create(p$studies, recursive = TRUE, showWarnings = FALSE)
  dir.create(p$models, recursive = TRUE, showWarnings = FALSE)
  invisible(lib_dir)
}

#' List the Studies in a Library
#'
#' @param lib_dir Library folder.
#' @return A data frame with one row per study (empty if none).
#' @export
library_list <- function(lib_dir) {
  p <- lib_paths(lib_dir)
  if (!file.exists(p$index)) {
    return(data.frame(study_id = character(0), added = character(0), animals = integer(0), sessions = integer(0),
                      conditions = character(0), parameters = character(0), infection_session = character(0),
                      summary = character(0), source_file = character(0), notes = character(0)))
  }
  utils::read.csv(p$index, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Add a Study to the Library
#'
#' Saves the session-level data of one study with each group's condition
#' (e.g. Infected / Uninfected / Treated) and days post-infection, so studies
#' with different group names and schedules can be combined.
#'
#' @param lib_dir Library folder.
#' @param sessions Output of [summarize_sessions()] with groups assigned (may
#'   include weight or variability columns).
#' @param study_id Short unique name for the study.
#' @param conditions Named character vector mapping each group to a condition:
#'   `"Infected"`, `"Uninfected"`, `"Treated"` or `"Exclude"`.
#' @param infection_timepoint First session after infection (for days post-infection).
#' @param offset Days between infection and that session.
#' @param design Optional output of [read_design()] (CFU and animal details are kept).
#' @param source_file Name of the original data file.
#' @param notes Free text.
#' @param overwrite Replace an existing study with the same id.
#' @return The updated study index, invisibly.
#' @export
library_add_study <- function(lib_dir, sessions, study_id, conditions, infection_timepoint = NULL, offset = 0,
                              design = NULL, source_file = NA_character_, notes = "", overwrite = FALSE) {
  library_init(lib_dir)
  p <- lib_paths(lib_dir)
  id <- safe_id(study_id)
  if (!nzchar(id)) stop("Give the study an id.")
  idx <- library_list(lib_dir)
  if (id %in% idx$study_id && !overwrite) stop("Study '", id, "' is already in the library. Choose another id or allow replacing it.")
  groups <- as.character(unique(sessions$group))
  missing <- setdiff(groups, names(conditions))
  if (length(missing)) stop("Give a condition for every group: ", paste(missing, collapse = ", "))
  s <- sessions
  s$condition <- unname(conditions[as.character(s$group)])
  s <- s[s$condition != "Exclude", , drop = FALSE]
  if (!is.null(infection_timepoint) && nzchar(infection_timepoint) && infection_timepoint %in% levels(s$timepoint)) {
    s <- add_dpi(s, infection_timepoint, offset)
  } else {
    s$dpi <- NA_real_
  }
  s$study <- id
  params <- attr(sessions, "parameters")
  entry <- list(sessions = s, parameters = params, conditions = conditions,
                info = list(study_id = id, source_file = source_file, infection_timepoint = infection_timepoint %||% NA,
                            offset = offset, settings = attr(sessions, "settings"), added = format(Sys.time(), "%Y-%m-%d %H:%M"),
                            plethr_version = as.character(tryCatch(utils::packageVersion("plethR"), error = function(e) NA))),
                design = design)
  saveRDS(entry, file.path(p$studies, paste0(id, ".rds")))
  cond_n <- tapply(s$subject, s$condition, function(x) length(unique(x)))
  row <- data.frame(study_id = id, added = entry$info$added, animals = length(unique(s$subject)), sessions = nrow(s),
                    conditions = paste(sprintf("%s: %d", names(cond_n), as.integer(cond_n)), collapse = "; "),
                    parameters = length(params), infection_session = infection_timepoint %||% "",
                    summary = paste0((entry$info$settings$stat %||% "median"), " per ", (entry$info$settings$timepoint %||% "phase")),
                    source_file = source_file %||% "", notes = notes, stringsAsFactors = FALSE)
  idx <- rbind(idx[idx$study_id != id, , drop = FALSE], row)
  utils::write.csv(idx, p$index, row.names = FALSE)
  invisible(idx)
}

#' Remove a Study from the Library
#'
#' @param lib_dir Library folder.
#' @param study_id Study to remove.
#' @return The updated study index, invisibly.
#' @export
library_remove <- function(lib_dir, study_id) {
  p <- lib_paths(lib_dir)
  idx <- library_list(lib_dir)
  unlink(file.path(p$studies, paste0(study_id, ".rds")))
  idx <- idx[!idx$study_id %in% study_id, , drop = FALSE]
  utils::write.csv(idx, p$index, row.names = FALSE)
  invisible(idx)
}

#' Load and Combine Library Studies
#'
#' @param lib_dir Library folder.
#' @param study_ids Studies to load. Defaults to all.
#' @return A data frame of all sessions with columns `study`, `subject` (prefixed
#'   with the study id so animal names cannot collide), `group`, `condition`,
#'   `timepoint`, `day`, `dpi` and the parameters shared by every study
#'   (attribute `"parameters"`).
#' @export
library_load <- function(lib_dir, study_ids = NULL) {
  p <- lib_paths(lib_dir)
  idx <- library_list(lib_dir)
  if (is.null(study_ids)) study_ids <- idx$study_id
  if (!length(study_ids)) stop("The library has no studies.")
  entries <- lapply(study_ids, function(id) readRDS(file.path(p$studies, paste0(id, ".rds"))))
  common <- Reduce(intersect, lapply(entries, function(e) e$parameters))
  out <- do.call(rbind, lapply(entries, function(e) {
    s <- e$sessions
    data.frame(study = e$info$study_id, subject = paste0(e$info$study_id, ":", s$subject), group = paste0(e$info$study_id, ":", as.character(s$group)),
               condition = s$condition, timepoint = paste0(e$info$study_id, ":", as.character(s$timepoint)),
               day = s$day, dpi = s$dpi, s[, common, drop = FALSE], check.names = FALSE, stringsAsFactors = FALSE)
  }))
  attr(out, "parameters") <- common
  attr(out, "studies") <- study_ids
  out
}

#' Prepare Library Data for Cross-Study Models
#'
#' @param lib_data Output of [library_load()].
#' @param task `"infection"` (infected after infection vs uninfected; pre-infection
#'   sessions left out) or `"phase"` (acute vs chronic within infected animals).
#' @param parameters Predictors. Defaults to all shared parameters.
#' @param acute_days Last day post-infection counted as acute (for `"phase"`).
#' @return Data ready for [ml_fit()] (including a `study` id column, so
#'   `validation = "study"` holds out whole studies).
#' @export
ml_prepare_library <- function(lib_data, task = c("infection", "phase"), parameters = NULL, acute_days = 14) {
  task <- match.arg(task)
  if (is.null(parameters)) parameters <- attr(lib_data, "parameters")
  d <- lib_data[!is.na(lib_data$dpi), , drop = FALSE]
  if (nrow(d) == 0) stop("No study in the selection has an infection session, so days post-infection are unknown.")
  if (task == "infection") {
    d <- d[d$condition %in% c("Infected", "Uninfected") & d$dpi >= 0, , drop = FALSE]
    outcome <- factor(ifelse(d$condition == "Infected", "Infected", "Uninfected"), levels = c("Infected", "Uninfected"))
  } else {
    d <- d[d$condition == "Infected" & d$dpi >= 0, , drop = FALSE]
    outcome <- factor(ifelse(d$dpi <= acute_days, "Acute", "Chronic"), levels = c("Acute", "Chronic"))
  }
  if (length(unique(outcome)) < 2) stop("Both classes are needed; check the conditions of the selected studies.")
  out <- data.frame(subject = d$subject, group = d$group, timepoint = d$timepoint, study = d$study, outcome = outcome,
                    d[, parameters, drop = FALSE], check.names = FALSE, stringsAsFactors = FALSE)
  out <- out[stats::complete.cases(out), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "ml") <- list(type = "classification", task = paste0("library_", task), parameters = parameters,
                          labels = levels(outcome), include_day = FALSE, covariate = NULL, covariate_name = "covariate",
                          timepoints = unique(out$timepoint), studies = unique(out$study), acute_days = acute_days)
  out
}

#' Performance of a Cross-Study Model in Each Study
#'
#' @param object Output of [ml_fit()] on library data.
#' @return A data frame: model, study, session-level and animal-level ROC AUC and accuracy.
#' @export
ml_study_metrics <- function(object) {
  p <- object$predictions
  if (!"study" %in% names(p)) stop("These predictions have no study column.")
  labels <- object$settings$labels
  do.call(rbind, lapply(split(p, list(p$model, p$study), drop = TRUE), function(d) {
    a <- stats::aggregate(prob ~ subject + outcome, data = d, FUN = mean)
    a$pred <- factor(ifelse(a$prob >= 0.5, labels[1], labels[2]), levels = labels)
    ms <- ml_metrics(d$outcome, d$prob, d$pred); ma <- ml_metrics(a$outcome, a$prob, a$pred)
    data.frame(model = d$model[1], study = d$study[1], sessions = nrow(d), animals = nrow(a),
               session_roc_auc = ms$roc_auc, animal_roc_auc = ma$roc_auc, animal_accuracy = ma$accuracy)
  }))
}

#' Save a Model in the Library
#'
#' @param lib_dir Library folder.
#' @param object Output of [ml_fit()].
#' @param name Short name for the model.
#' @param notes Free text.
#' @return The model id, invisibly.
#' @export
library_save_model <- function(lib_dir, object, name, notes = "") {
  library_init(lib_dir)
  p <- lib_paths(lib_dir)
  id <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", safe_id(name))
  saveRDS(object, file.path(p$models, paste0(id, ".rds")))
  m <- object$metrics
  type <- ml_type(object)
  score <- if (type == "regression") m$rsq else m$roc_auc
  best <- m$model[m$level == "session"][which.max(score[m$level == "session"])]
  row <- data.frame(model_id = id, name = name, created = format(Sys.time(), "%Y-%m-%d %H:%M"),
                    task = object$settings$task %||% "", outcome = ml_outcome_label(object),
                    validation = object$settings$validation, studies = paste(object$settings$studies %||% "", collapse = ", "),
                    n_studies = length(object$settings$studies %||% 1), animals = length(unique(object$data$subject)),
                    best_model = unname(ml_model_names("all")[best]),
                    session_score = score[m$level == "session" & m$model == best],
                    animal_score = score[m$level == "animal" & m$model == best],
                    metric = if (type == "regression") "R2" else "ROC AUC", notes = notes, stringsAsFactors = FALSE)
  idx <- library_models(lib_dir)
  utils::write.csv(rbind(idx, row), p$model_index, row.names = FALSE)
  invisible(id)
}

#' List Saved Models
#'
#' @param lib_dir Library folder.
#' @return A data frame with one row per saved model (empty if none).
#' @export
library_models <- function(lib_dir) {
  p <- lib_paths(lib_dir)
  if (!file.exists(p$model_index)) {
    return(data.frame(model_id = character(0), name = character(0), created = character(0), task = character(0), outcome = character(0),
                      validation = character(0), studies = character(0), n_studies = integer(0), animals = integer(0),
                      best_model = character(0), session_score = numeric(0), animal_score = numeric(0), metric = character(0),
                      notes = character(0)))
  }
  utils::read.csv(p$model_index, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Load a Saved Model
#'
#' @param lib_dir Library folder.
#' @param model_id Model id from [library_models()].
#' @return The saved `plethr_ml` object.
#' @export
library_load_model <- function(lib_dir, model_id) readRDS(file.path(lib_paths(lib_dir)$models, paste0(model_id, ".rds")))

#' Plot Model History
#'
#' Held-out performance of saved models over time, to see whether models improve
#' as studies are added.
#'
#' @param models Output of [library_models()].
#' @return A ggplot object.
#' @export
plot_model_history <- function(models) {
  if (nrow(models) == 0) stop("No saved models yet.")
  d <- models
  d$created <- as.POSIXct(d$created, format = "%Y-%m-%d %H:%M")
  d$label <- paste0(d$name, " (", d$n_studies, " stud", ifelse(d$n_studies == 1, "y", "ies"), ")")
  ggplot2::ggplot(d, ggplot2::aes(x = .data$created, y = .data$animal_score, color = .data$task)) +
    ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey60") +
    ggplot2::geom_line(data = function(x) x[duplicated(x$task) | duplicated(x$task, fromLast = TRUE), , drop = FALSE],
                       ggplot2::aes(group = .data$task), alpha = 0.5) +
    ggplot2::geom_point(ggplot2::aes(size = .data$n_studies)) +
    ggrepel::geom_text_repel(ggplot2::aes(label = .data$label), size = 3, show.legend = FALSE, seed = 1) +
    ggplot2::scale_size_continuous(range = c(2.5, 6), name = "Studies") +
    ggplot2::labs(title = "Saved models over time", x = NULL, y = "Held-out score (animal level)",
                  caption = "ROC AUC for classification (0.5 = chance), R\u00b2 for severity; label = model name and number of studies") +
    theme_plethr(11) +
    ggplot2::theme(legend.position = "right")
}
