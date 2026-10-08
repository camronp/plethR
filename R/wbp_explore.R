# Exploratory views of WBP data beyond the standard time course.

# Polar coordinates that draw straight edges between spokes (coord_polar curves them).
coord_radar <- function() {
  ggplot2::ggproto("CoordRadar", ggplot2::CoordPolar, theta = "x", r = "y", start = 0, direction = 1,
                   is_linear = function(coord) TRUE)
}

tp_breaks <- function(lv, max_labels = 12) {
  step <- max(1, ceiling(length(lv) / max_labels))
  idx <- seq(1, length(lv), by = step)
  list(breaks = idx, labels = lv[idx])
}

#' Dashboard of All Parameters
#'
#' Small multiples: the group time course of every chosen parameter on one page.
#'
#' @param group_summary Output of [summarize_groups()].
#' @param parameters Parameters to show. Defaults to all in `group_summary`.
#' @param colors Named group colors.
#' @param error `"sem"`, `"sd"` or `"none"`.
#' @param ncol Number of panel columns.
#' @return A ggplot object.
#' @export
plot_dashboard <- function(group_summary, parameters = NULL, colors = NULL, error = c("sem", "sd", "none"), ncol = 4) {
  error <- match.arg(error)
  d <- group_summary
  if (!is.null(parameters)) d <- d[d$parameter %in% parameters, , drop = FALSE]
  if (nrow(d) == 0) stop("No data for these parameters.")
  lv <- levels(d$timepoint)
  d$x <- match(as.character(d$timepoint), lv)
  d$err <- switch(error, sem = d$sem, sd = d$sd, none = 0)
  d$err[is.na(d$err)] <- 0
  d$panel <- factor(d$parameter, levels = unique(d$parameter[order(match(d$parameter, parameters %||% unique(d$parameter)))]))
  if (is.null(colors)) colors <- plethr_colors(levels(droplevels(as.factor(d$group))))
  b <- tp_breaks(lv, 6)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$mean, color = .data$group, fill = .data$group)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$mean - .data$err, ymax = .data$mean + .data$err), alpha = 0.15, color = NA) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(size = 1) +
    ggplot2::facet_wrap(~ panel, scales = "free_y", ncol = ncol) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_fill_manual(values = colors, guide = "none") +
    ggplot2::scale_x_continuous(breaks = b$breaks, labels = b$labels) +
    ggplot2::labs(title = "All parameters over time", x = NULL, y = NULL,
                  caption = paste0("Group mean", switch(error, sem = " \u00b1 SEM", sd = " \u00b1 SD", none = ""), "; each panel has its own y axis")) +
    theme_plethr(11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7))
}

#' Heatmap of Every Animal over Time
#'
#' One row per animal and one column per session, colored by the parameter's
#' value. Shows at a glance which animals respond, when, and how consistently.
#'
#' @param sessions Output of [summarize_sessions()] with groups assigned.
#' @param parameter Parameter to show.
#' @param scale `"z"` (standardized across all sessions), `"percent"` (% of each
#'   animal's first session) or `"value"` (raw values).
#' @param title Plot title.
#' @return A ggplot object.
#' @export
plot_animal_heatmap <- function(sessions, parameter, scale = c("z", "percent", "value"), title = NULL) {
  scale <- match.arg(scale)
  d <- sessions[!is.na(sessions[[parameter]]), c("subject", "group", "timepoint", parameter)]
  names(d)[4] <- "v"
  lv <- levels(sessions$timepoint)
  if (scale == "z") {
    d$fill <- (d$v - mean(d$v)) / stats::sd(d$v)
    lim <- max(abs(stats::quantile(d$fill, c(0.02, 0.98), na.rm = TRUE)))
    d$fill <- pmax(pmin(d$fill, lim), -lim)
    fill_scale <- ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "#F7F7F7", high = "#B2182B", midpoint = 0,
                                                limits = c(-lim, lim), name = "z-score")
  } else if (scale == "percent") {
    first <- d[order(match(as.character(d$timepoint), lv)), ]
    first <- first[!duplicated(first$subject), c("subject", "v")]
    b <- first$v[match(d$subject, first$subject)]
    d$fill <- ifelse(b == 0, NA_real_, d$v / b * 100)
    lim <- max(abs(stats::quantile(d$fill - 100, c(0.02, 0.98), na.rm = TRUE)))
    d$fill <- pmax(pmin(d$fill, 100 + lim), 100 - lim)
    fill_scale <- ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "#F7F7F7", high = "#B2182B", midpoint = 100,
                                                limits = c(100 - lim, 100 + lim), name = "% of first\nsession")
  } else {
    d$fill <- d$v
    fill_scale <- ggplot2::scale_fill_viridis_c(name = wbp_axis_label(parameter), option = "D")
  }
  d$timepoint <- factor(as.character(d$timepoint), levels = lv)
  d$subject <- factor(d$subject, levels = rev(unique(d$subject[order(d$group, d$subject)])))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$timepoint, y = .data$subject, fill = .data$fill)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.3) +
    fill_scale +
    ggplot2::facet_grid(group ~ ., scales = "free_y", space = "free_y") +
    ggplot2::labs(title = title %||% paste0(wbp_feature_name(parameter), ": every animal over time"), x = NULL, y = NULL,
                  caption = switch(scale, z = "Standardized across all animals and sessions; extreme values capped",
                                   percent = "Relative to each animal's first session; extreme values capped", value = "Measured values")) +
    theme_plethr(11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), axis.line = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(), strip.text.y = ggplot2::element_text(angle = 0, hjust = 0),
                   legend.position = "right", legend.title = ggplot2::element_text(size = 9))
}

#' Correlation Map of Parameters
#'
#' Spearman (or Pearson) correlations between parameters, ordered by clustering,
#' either between animals (each animal's mean over sessions) or between sessions.
#'
#' @param sessions Output of [summarize_sessions()].
#' @param parameters Parameters to include.
#' @param level `"animal"` (one mean per animal; the honest choice for small n) or
#'   `"session"` (every animal-session; correlations partly reflect differences between animals).
#' @param method `"spearman"` or `"pearson"`.
#' @param groups Optional groups to restrict to.
#' @return A ggplot object.
#' @export
plot_correlation <- function(sessions, parameters = NULL, level = c("animal", "session"),
                             method = c("spearman", "pearson"), groups = NULL) {
  level <- match.arg(level)
  method <- match.arg(method)
  if (is.null(parameters)) parameters <- attr(sessions, "parameters")
  s <- sessions
  if (!is.null(groups)) s <- s[as.character(s$group) %in% groups, , drop = FALSE]
  m <- if (level == "animal") {
    stats::aggregate(s[, parameters, drop = FALSE], by = list(subject = s$subject), FUN = mean, na.rm = TRUE)[, parameters, drop = FALSE]
  } else s[, parameters, drop = FALSE]
  m <- m[, vapply(m, function(x) stats::sd(x, na.rm = TRUE) > 0, logical(1)), drop = FALSE]
  if (ncol(m) < 2) stop("Need at least 2 parameters that vary.")
  r <- stats::cor(m, method = method, use = "pairwise.complete.obs")
  ord <- stats::hclust(stats::as.dist(1 - abs(r)))$order
  lv <- colnames(r)[ord]
  d <- as.data.frame(as.table(r))
  names(d) <- c("a", "b", "r")
  d$a <- factor(d$a, levels = lv); d$b <- factor(d$b, levels = rev(lv))
  d <- d[as.integer(d$a) <= length(lv) + 1 - as.integer(d$b), , drop = FALSE]
  ggplot2::ggplot(d, ggplot2::aes(x = .data$a, y = .data$b, fill = .data$r)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$r), color = abs(.data$r) > 0.6), size = 2.8, show.legend = FALSE) +
    ggplot2::scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "black"), guide = "none") +
    ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "#F7F7F7", high = "#B2182B", limits = c(-1, 1), name = "r") +
    ggplot2::labs(title = "How parameters move together", x = NULL, y = NULL,
                  caption = sprintf("%s correlation between %s (n = %d); ordered by clustering",
                                    if (method == "spearman") "Spearman" else "Pearson",
                                    if (level == "animal") "animals (mean of each animal's sessions)" else "animal-sessions", nrow(m))) +
    theme_plethr(11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), axis.line = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(), legend.position = "right")
}

#' Group Profile (Radar Chart)
#'
#' Each group's fingerprint across parameters: the % difference of its mean from
#' the reference group, one spoke per parameter.
#'
#' @param values Output of [summarize_subjects()].
#' @param reference Reference group.
#' @param parameters Parameters (spokes). Defaults to all.
#' @param colors Named group colors.
#' @param limit Differences beyond +/- this percentage are capped.
#' @return A ggplot object.
#' @export
plot_group_profile <- function(values, reference, parameters = NULL, colors = NULL, limit = 50) {
  v <- values
  if (!is.null(parameters)) v <- v[v$parameter %in% parameters, , drop = FALSE]
  m <- stats::aggregate(value ~ group + parameter, data = v, FUN = mean)
  ref <- m[m$group == reference, c("parameter", "value")]
  m$ref <- ref$value[match(m$parameter, ref$parameter)]
  m$pct <- ifelse(m$ref == 0, NA_real_, (m$value - m$ref) / abs(m$ref) * 100)
  m <- m[m$group != reference & !is.na(m$pct), , drop = FALSE]
  if (nrow(m) == 0) stop("Nothing to compare with the reference group.")
  pl <- unique(if (is.null(parameters)) m$parameter else intersect(parameters, m$parameter))
  m$x <- match(m$parameter, pl)
  m$y <- pmax(pmin(m$pct, limit), -limit)
  m <- m[order(m$group, m$x), , drop = FALSE]
  closed <- m
  if (is.null(colors)) colors <- plethr_colors(unique(as.character(m$group)))
  colors <- colors[names(colors) %in% m$group]
  ring <- data.frame(x = seq_along(pl), y = 0)
  ggplot2::ggplot(closed, ggplot2::aes(x = .data$x, y = .data$y, color = .data$group, group = .data$group)) +
    ggplot2::geom_polygon(data = ring, ggplot2::aes(x = .data$x, y = .data$y), inherit.aes = FALSE, fill = NA, color = "grey35", linetype = "dashed") +
    ggplot2::geom_polygon(ggplot2::aes(fill = .data$group), alpha = 0.08, linewidth = 0.9) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_x_continuous(breaks = seq_along(pl), labels = pl, limits = c(0.5, length(pl) + 0.5)) +
    ggplot2::scale_y_continuous(limits = c(-limit, limit), breaks = c(-limit, -limit / 2, 0, limit / 2, limit),
                                labels = function(x) sprintf("%+g%%", x)) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_fill_manual(values = colors, guide = "none") +
    coord_radar() +
    ggplot2::labs(title = paste("Group profiles relative to", reference), x = NULL, y = NULL,
                  caption = sprintf("%% difference of group means from %s (dashed ring = no difference); capped at \u00b1%g%%", reference, limit)) +
    theme_plethr(11) +
    ggplot2::theme(axis.line = ggplot2::element_blank(), panel.grid.major = ggplot2::element_line(color = "grey90"),
                   axis.text.x = ggplot2::element_text(face = "bold", size = 10))
}

#' Effect Sizes with Confidence Intervals (Forest Plot)
#'
#' Percent difference from the reference group with 95% confidence intervals for
#' every parameter: shows the size and the uncertainty of each effect.
#'
#' @param comparisons Output of [compare_groups()] (with a reference group).
#' @param parameters Parameters to show. Defaults to all.
#' @param colors Named group colors.
#' @return A ggplot object.
#' @export
plot_effect_forest <- function(comparisons, parameters = NULL, colors = NULL) {
  d <- comparisons
  if (!is.null(parameters)) d <- d[d$parameter %in% parameters, , drop = FALSE]
  d <- d[!is.na(d$pct_difference), , drop = FALSE]
  if (nrow(d) == 0) stop("No comparisons to show.")
  ord <- stats::aggregate(pct_difference ~ parameter, data = d, FUN = function(x) max(abs(x)))
  d$parameter <- factor(d$parameter, levels = ord$parameter[order(ord$pct_difference)])
  d$label <- paste(d$group2, "vs", d$group1)
  if (is.null(colors)) colors <- plethr_colors(unique(d$group2))
  colors <- colors[names(colors) %in% d$group2]
  pos <- ggplot2::position_dodge(width = 0.6)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$pct_difference, y = .data$parameter, color = .data$group2)) +
    ggplot2::geom_vline(xintercept = 0, color = "grey40") +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = .data$pct_ci_low, xmax = .data$pct_ci_high), width = 0, linewidth = 0.7, position = pos, orientation = "y") +
    ggplot2::geom_point(ggplot2::aes(shape = !is.na(.data$p_adj) & .data$p_adj < 0.05), size = 2.6, position = pos) +
    ggplot2::scale_shape_manual(values = c(`FALSE` = 1, `TRUE` = 16), labels = c(`FALSE` = "adjusted p \u2265 0.05", `TRUE` = "adjusted p < 0.05"), name = NULL) +
    ggplot2::scale_color_manual(values = colors, name = NULL) +
    ggplot2::labs(title = paste("Effect sizes vs", d$group1[1]), x = "% difference (95% CI)", y = NULL,
                  caption = "Intervals crossing 0 are compatible with no difference; wide intervals mean few animals or high variability") +
    theme_plethr(11) +
    ggplot2::theme(legend.position = "top", legend.box = "vertical")
}

#' Distribution of Breath Records
#'
#' Density of the individual FinePointe records for each group at chosen
#' sessions (all animals of a group pooled). Descriptive: shows the shape of the
#' data behind each session summary, such as long tails in Penh.
#'
#' @param data Output of [read_wbp()] passed through [assign_groups()].
#' @param parameter Parameter to show.
#' @param timepoints Sessions (Phase labels or dates) to show; up to 6 are used.
#' @param timepoint `"phase"` or `"date"`, as in [summarize_sessions()].
#' @param colors Named group colors.
#' @param log_x Log-scale x axis. Default: chosen automatically for skewed parameters.
#' @return A ggplot object.
#' @export
plot_record_distribution <- function(data, parameter, timepoints, timepoint = c("phase", "date"), colors = NULL, log_x = NULL) {
  timepoint <- match.arg(timepoint)
  tp <- if (timepoint == "phase") data$Phase else format(as.Date(data$Time), "%Y-%m-%d")
  timepoints <- utils::head(timepoints, 6)
  d <- data.frame(group = data$group, timepoint = tp, v = data[[parameter]])
  d <- d[d$timepoint %in% timepoints & !is.na(d$v), , drop = FALSE]
  if (nrow(d) == 0) stop("No records for these sessions.")
  d$timepoint <- factor(d$timepoint, levels = timepoints)
  if (is.null(log_x)) log_x <- all(d$v > 0) && stats::quantile(d$v, 0.99) / stats::median(d$v) > 5
  if (log_x) d <- d[d$v > 0, , drop = FALSE]
  if (is.null(colors)) colors <- plethr_colors(levels(droplevels(as.factor(d$group))))
  med <- stats::aggregate(v ~ group + timepoint, data = d, FUN = stats::median)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$v, color = .data$group, fill = .data$group)) +
    ggplot2::geom_density(alpha = 0.12, linewidth = 0.7, adjust = 1.2) +
    ggplot2::geom_vline(data = med, ggplot2::aes(xintercept = .data$v, color = .data$group), linetype = "dashed", linewidth = 0.5) +
    ggplot2::facet_wrap(~ timepoint, ncol = min(3, length(timepoints))) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_fill_manual(values = colors, guide = "none") +
    ggplot2::labs(title = paste0(wbp_feature_name(parameter), ": distribution of breath records"),
                  x = wbp_axis_label(parameter), y = "Density",
                  caption = "All records of each group pooled; dashed lines = group medians. Descriptive only: statistics use one value per animal.") +
    theme_plethr(11)
  if (log_x) p <- p + ggplot2::scale_x_log10()
  p
}

#' Two-Parameter Trajectories
#'
#' Each group's mean path through the space of two parameters over time, with
#' arrows from the first to the last session. Animals' sessions are shown faintly.
#'
#' @param group_summary Output of [summarize_groups()] (must include both parameters).
#' @param x,y Parameters for the two axes.
#' @param sessions Optional output of [summarize_sessions()] to show each animal-session.
#' @param colors Named group colors.
#' @param smooth Rolling-average window (in sessions) applied to each group's path;
#'   1 = no smoothing.
#' @return A ggplot object.
#' @export
plot_trajectory <- function(group_summary, x, y, sessions = NULL, colors = NULL, smooth = 3) {
  gx <- group_summary[group_summary$parameter == x, c("group", "timepoint", "mean")]
  gy <- group_summary[group_summary$parameter == y, c("group", "timepoint", "mean")]
  d <- merge(gx, gy, by = c("group", "timepoint"), suffixes = c("_x", "_y"))
  if (nrow(d) == 0) stop("No data for these parameters.")
  lv <- levels(group_summary$timepoint)
  d <- d[order(d$group, match(as.character(d$timepoint), lv)), , drop = FALSE]
  if (smooth > 1) {
    roll <- function(v) {
      k <- min(smooth, length(v))
      out <- as.numeric(stats::filter(v, rep(1 / k, k), sides = 2))
      out[is.na(out)] <- v[is.na(out)]
      out
    }
    d <- do.call(rbind, lapply(split(d, as.character(d$group)), function(g) { g$mean_x <- roll(g$mean_x); g$mean_y <- roll(g$mean_y); g }))
  }
  if (is.null(colors)) colors <- plethr_colors(levels(droplevels(as.factor(d$group))))
  ends <- do.call(rbind, lapply(split(d, as.character(d$group)), function(g) g[c(1, nrow(g)), ]))
  ends$label <- as.character(ends$timepoint)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$mean_x, y = .data$mean_y, color = .data$group))
  if (!is.null(sessions)) {
    p <- p + ggplot2::geom_point(data = sessions, ggplot2::aes(x = .data[[x]], y = .data[[y]], color = .data$group),
                                 alpha = 0.15, size = 1, inherit.aes = FALSE)
  }
  p +
    ggplot2::geom_path(ggplot2::aes(group = .data$group), linewidth = 0.9,
                       arrow = ggplot2::arrow(length = ggplot2::unit(0.18, "cm"), type = "closed")) +
    ggplot2::geom_point(size = 1.6) +
    ggplot2::geom_point(data = ends, size = 3.2, shape = 21, fill = "white", stroke = 1.2) +
    ggrepel::geom_text_repel(data = ends, ggplot2::aes(label = .data$label), size = 3, show.legend = FALSE, seed = 1) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(title = paste(wbp_feature_name(y), "vs", wbp_feature_name(x)), x = wbp_axis_label(x), y = wbp_axis_label(y),
                  caption = paste0("Lines: group means", if (smooth > 1) sprintf(" (rolling average of %d sessions)", smooth) else "", " in time order, arrow = direction of time; faint points: individual animal-sessions")) +
    theme_plethr(11)
}

#' Waterfall of Change from Baseline
#'
#' Each animal's average % change from its own baseline over a window, sorted,
#' colored by group.
#'
#' @param sessions Output of [summarize_sessions()] with groups assigned.
#' @param parameter Parameter to show.
#' @param from,to Window (timepoints). Default: second to last timepoint.
#' @param baseline Baseline timepoint. Default: the first.
#' @param colors Named group colors.
#' @return A ggplot object.
#' @export
plot_waterfall <- function(sessions, parameter, from = NULL, to = NULL, baseline = NULL, colors = NULL) {
  lv <- levels(sessions$timepoint)
  if (is.null(baseline)) baseline <- lv[1]
  if (is.null(from)) from <- lv[min(2, length(lv))]
  if (is.null(to)) to <- lv[length(lv)]
  win <- lv[match(from, lv):match(to, lv)]
  b <- sessions[sessions$timepoint == baseline, c("subject", parameter)]
  d <- sessions[as.character(sessions$timepoint) %in% win & !is.na(sessions[[parameter]]), c("subject", "group", parameter)]
  d$base <- b[[parameter]][match(d$subject, b$subject)]
  d <- d[!is.na(d$base) & d$base != 0, , drop = FALSE]
  if (nrow(d) == 0) stop("No animals with a baseline value.")
  d$pct <- (d[[parameter]] / d$base - 1) * 100
  a <- stats::aggregate(pct ~ subject + group, data = d, FUN = mean)
  a <- a[order(a$pct), , drop = FALSE]
  a$subject <- factor(a$subject, levels = a$subject)
  if (is.null(colors)) colors <- plethr_colors(levels(droplevels(as.factor(a$group))))
  ggplot2::ggplot(a, ggplot2::aes(x = .data$subject, y = .data$pct, fill = .data$group)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey40") +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::scale_y_continuous(labels = function(x) sprintf("%+g%%", x)) +
    ggplot2::labs(title = paste0(wbp_feature_name(parameter), ": change from baseline by animal"),
                  x = NULL, y = paste0("Mean % change from ", baseline),
                  caption = sprintf("Average of %s to %s, each animal relative to its own %s value", win[1], win[length(win)], baseline)) +
    theme_plethr(11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1, size = 8))
}
