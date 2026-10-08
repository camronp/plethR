# Factorial (e.g. genotype x infection) analysis for designs where every group is a
# combination of two factors.

#' Suggest a Two-Factor Design from Group Names
#'
#' Splits each group name into its first word and the rest, so
#' `"Infected WT"` becomes factor A `"Infected"` and factor B `"WT"`. The
#' suggestion is only returned when the groups form a complete (fully crossed)
#' design; otherwise the factor columns are `NA` and must be filled in by hand.
#'
#' @param groups Group names, in display order.
#' @return A data frame with columns `group`, `A` and `B`.
#' @examples
#' suggest_factorial_design(c("Uninfected WT", "Uninfected BENaC", "Infected WT", "Infected BENaC"))
#' @export
suggest_factorial_design <- function(groups) {
  first <- sub(" .*$", "", groups)
  rest <- trimws(sub("^\\S+\\s*", "", groups))
  design <- data.frame(group = groups, A = first, B = rest, stringsAsFactors = FALSE)
  if (!is_crossed_design(design)) design$A <- design$B <- NA_character_
  design
}

# TRUE when every group has two non-empty levels and every combination of levels occurs once.
is_crossed_design <- function(design) {
  a <- design$A
  b <- design$B
  if (any(is.na(a) | is.na(b) | !nzchar(a) | !nzchar(b))) return(FALSE)
  na <- length(unique(a))
  nb <- length(unique(b))
  na >= 2 && nb >= 2 && !anyDuplicated(paste(a, b, sep = "\r")) && nrow(design) == na * nb
}

check_design <- function(design) {
  if (!all(c("group", "A", "B") %in% names(design))) stop("design needs columns group, A and B")
  if (!is_crossed_design(design)) {
    stop("The groups must form a complete two-factor design: every group needs a level for both factors, ",
         "each factor needs at least 2 levels, and every combination of levels must be exactly one group.")
  }
}

name_terms <- function(term, factor_names) {
  term <- gsub("\\btimepoint\\b", "Time", term)
  term <- gsub("\\bA\\b", factor_names[1], term)
  term <- gsub("\\bB\\b", factor_names[2], term)
  gsub(":", " \u00d7 ", term)
}

add_design <- function(d, design) {
  i <- match(as.character(d$group), design$group)
  d$A <- factor(design$A[i], levels = unique(design$A))
  d$B <- factor(design$B[i], levels = unique(design$B))
  d[!is.na(d$A) & !is.na(d$B), , drop = FALSE]
}

prep_response <- function(y, log_transform) {
  if (!log_transform) return(y)
  y[!is.na(y) & y <= 0] <- NA
  log(y)
}

#' Two-Factor Mixed Model over Time
#'
#' Fits, for each parameter, a linear mixed model with the two factors, time and
#' all their interactions as fixed effects and a random intercept for each animal:
#' `value ~ A * B * timepoint + (1 | animal)`. Using every session (rather than one
#' summary value per animal) accounts for repeated measurements of the same animal.
#' Tests are marginal (type III) F-tests with sum-to-zero contrasts.
#'
#' @param sessions Output of [summarize_sessions()] with groups assigned.
#' @param design Data frame with columns `group`, `A`, `B` giving each group's
#'   level of the two factors (see [suggest_factorial_design()]).
#' @param parameters Parameters to model. Defaults to all.
#' @param factor_names Names of factors A and B, used to label terms.
#' @param log_transform Model log(value); useful for skewed ratio parameters such as Penh.
#' @return A long data frame: `parameter`, `term`, `num_df`, `den_df`, `F`, `p`.
#'   Parameters whose model fails to fit are returned with `NA`.
#' @export
factorial_mixed_model <- function(sessions, design, parameters = NULL, factor_names = c("A", "B"),
                                  log_transform = FALSE) {
  check_design(design)
  if (is.null(parameters)) parameters <- attr(sessions, "parameters")
  d0 <- add_design(sessions, design)
  d0$timepoint <- droplevels(d0$timepoint)
  with_time <- nlevels(d0$timepoint) >= 2
  form <- if (with_time) y ~ A * B * timepoint else y ~ A * B
  ctr <- list(A = "contr.sum", B = "contr.sum")
  if (with_time) ctr$timepoint <- "contr.sum"

  out <- lapply(parameters, function(p) {
    d <- d0
    d$y <- prep_response(d[[p]], log_transform)
    d <- d[!is.na(d$y), , drop = FALSE]
    res <- tryCatch({
      fit <- nlme::lme(form, random = ~ 1 | subject, data = d, contrasts = ctr, method = "REML")
      a <- stats::anova(fit, type = "marginal")
      a <- a[rownames(a) != "(Intercept)", , drop = FALSE]
      data.frame(term = rownames(a), num_df = a[["numDF"]], den_df = a[["denDF"]],
                 F = a[["F-value"]], p = a[["p-value"]], stringsAsFactors = FALSE)
    }, error = function(e) data.frame(term = NA_character_, num_df = NA, den_df = NA, F = NA, p = NA))
    cbind(parameter = p, res, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out)
  out$term <- name_terms(out$term, factor_names)
  rownames(out) <- NULL
  out
}

#' Two-Way ANOVA on One Value per Animal
#'
#' For each parameter, a two-way ANOVA (`value ~ A * B`) on per-animal summary
#' values such as AUC, with type III F-tests.
#'
#' @param values Output of [summarize_subjects()].
#' @inheritParams factorial_mixed_model
#' @return A long data frame: `parameter`, `term`, `num_df`, `den_df`, `F`, `p`.
#' @export
factorial_anova <- function(values, design, parameters = NULL, factor_names = c("A", "B"),
                            log_transform = FALSE) {
  check_design(design)
  if (is.null(parameters)) parameters <- unique(values$parameter)
  d0 <- add_design(values, design)
  out <- lapply(parameters, function(p) {
    d <- d0[d0$parameter == p, , drop = FALSE]
    d$y <- prep_response(d$value, log_transform)
    d <- d[!is.na(d$y), , drop = FALSE]
    res <- tryCatch({
      fit <- stats::lm(y ~ A * B, data = d, contrasts = list(A = "contr.sum", B = "contr.sum"))
      a <- stats::drop1(fit, scope = ~ A + B + A:B, test = "F")
      a <- a[-1, , drop = FALSE]
      data.frame(term = rownames(a), num_df = a[["Df"]], den_df = fit$df.residual,
                 F = a[["F value"]], p = a[["Pr(>F)"]], stringsAsFactors = FALSE)
    }, error = function(e) data.frame(term = NA_character_, num_df = NA, den_df = NA, F = NA, p = NA))
    cbind(parameter = p, res, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out)
  out$term <- name_terms(out$term, factor_names)
  rownames(out) <- NULL
  out
}

#' Interaction Plot for a Two-Factor Design
#'
#' Mean \eqn{\pm} SEM of one value per animal for each combination of the two
#' factors, with each animal shown. Non-parallel lines suggest an interaction.
#'
#' @param values Output of [summarize_subjects()].
#' @param design Data frame with columns `group`, `A`, `B`.
#' @param parameter Parameter to plot.
#' @param factor_names Names of factors A (x axis) and B (lines).
#' @param colors Optional named colors for the levels of factor B.
#' @param y_label Y-axis label.
#' @param title Plot title.
#' @return A ggplot object.
#' @export
plot_interaction <- function(values, design, parameter, factor_names = c("A", "B"),
                             colors = NULL, y_label = parameter, title = NULL) {
  check_design(design)
  d <- add_design(values[values$parameter == parameter & !is.na(values$value), , drop = FALSE], design)
  if (nrow(d) == 0) stop("No data for parameter ", parameter)
  if (is.null(colors)) colors <- plethr_colors(levels(d$B))
  sm <- d %>%
    dplyr::group_by(.data$A, .data$B) %>%
    dplyr::summarize(mean = mean(.data$value),
                     sem = if (dplyr::n() > 1) stats::sd(.data$value) / sqrt(dplyr::n()) else 0,
                     .groups = "drop")
  pos <- ggplot2::position_dodge(width = 0.25)
  pos_pts <- ggplot2::position_jitterdodge(jitter.width = 0.08, dodge.width = 0.25, seed = 1)
  info <- wbp_parameter_info()
  full <- wbp_feature_name(parameter)

  ggplot2::ggplot(sm, ggplot2::aes(x = .data$A, y = .data$mean, color = .data$B, group = .data$B)) +
    ggplot2::geom_point(data = d, ggplot2::aes(y = .data$value, fill = .data$B), position = pos_pts,
                        alpha = 0.45, size = 2, show.legend = FALSE) +
    ggplot2::geom_line(linewidth = 1, position = pos) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$mean - .data$sem, ymax = .data$mean + .data$sem),
                           width = 0.12, linewidth = 0.6, position = pos) +
    ggplot2::geom_point(size = 3.2, position = pos) +
    ggplot2::scale_color_manual(values = colors, name = factor_names[2]) +
    ggplot2::scale_fill_manual(values = colors, guide = "none") +
    ggplot2::labs(title = if (is.null(title)) (if (is.na(full)) parameter else full) else title,
                  x = factor_names[1], y = y_label,
                  caption = "Mean \u00b1 SEM; each faint point is one animal; non-parallel lines suggest an interaction") +
    theme_plethr() +
    ggplot2::theme(legend.title = ggplot2::element_text(face = "bold"))
}
