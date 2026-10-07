# Plots for the subject-level pipeline in wbp_pipeline.R.

#' Group Colors
#'
#' Returns a named vector of colors, one per group, so every figure uses the same
#' color for the same group.
#'
#' @param groups Group names, in display order.
#' @param palette `"okabe-ito"` (colorblind-safe, default), `"set1"`, `"dark2"`,
#'   `"viridis"` or `"grayscale"`.
#' @return A named character vector of hex colors.
#' @export
plethr_colors <- function(groups, palette = c("okabe-ito", "set1", "dark2", "viridis", "grayscale")) {
  palette <- match.arg(palette)
  n <- length(groups)
  cols <- switch(palette,
    "okabe-ito" = rep(c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00",
                        "#56B4E9", "#F0E442", "#000000"), length.out = n),
    "set1" = rep(RColorBrewer::brewer.pal(9, "Set1"), length.out = n),
    "dark2" = rep(RColorBrewer::brewer.pal(8, "Dark2"), length.out = n),
    "viridis" = viridisLite::viridis(n, end = 0.9),
    "grayscale" = grDevices::gray.colors(n, start = 0.15, end = 0.75)
  )
  stats::setNames(cols, groups)
}

#' Publication Theme for plethR Figures
#'
#' @param base_size Base font size in points.
#' @return A ggplot2 theme.
#' @export
theme_plethr <- function(base_size = 13) {
  ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(color = "black"),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.line = ggplot2::element_line(linewidth = 0.5),
      axis.ticks = ggplot2::element_line(linewidth = 0.5),
      plot.title = ggplot2::element_text(face = "bold", size = base_size * 1.15),
      plot.subtitle = ggplot2::element_text(color = "grey30"),
      plot.caption = ggplot2::element_text(color = "grey40", hjust = 0),
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      legend.key.width = ggplot2::unit(1.6, "lines"),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
}

#' Plot a Group Time Course
#'
#' Group mean with error bars (or a shaded band) at each timepoint, optionally
#' with each animal's own trace and significance markers versus a reference group.
#'
#' @param group_summary Output of [summarize_groups()].
#' @param parameter Parameter to plot.
#' @param sessions Output of [summarize_sessions()]; needed for `show_individuals`.
#' @param colors Named group colors, e.g. from [plethr_colors()].
#' @param error `"sem"`, `"sd"` or `"none"`.
#' @param error_style `"bars"` or `"band"`.
#' @param x_axis `"timepoint"` (evenly spaced session labels) or `"day"` (true
#'   spacing in days since the first session).
#' @param show_individuals Draw faint lines for each animal.
#' @param stats Optional output of [compare_timepoints()]; significant timepoints
#'   get stars in the color of the group being compared with the reference.
#' @param transform Baseline transform used, for the y-axis label.
#' @param title Plot title. Defaults to the parameter's full name.
#' @return A ggplot object.
#' @export
plot_timecourse <- function(group_summary, parameter, sessions = NULL, colors = NULL,
                            error = c("sem", "sd", "none"), error_style = c("bars", "band"),
                            x_axis = c("timepoint", "day"), show_individuals = FALSE,
                            stats = NULL, transform = "none", title = NULL) {
  error <- match.arg(error)
  error_style <- match.arg(error_style)
  x_axis <- match.arg(x_axis)

  d <- group_summary[group_summary$parameter == parameter, , drop = FALSE]
  if (nrow(d) == 0) stop("No data for parameter ", parameter)
  if (is.null(colors)) colors <- plethr_colors(levels(droplevels(as.factor(d$group))))
  lv <- levels(d$timepoint)
  xpos <- function(tp, day) if (x_axis == "day") day else match(as.character(tp), lv)

  d$x <- xpos(d$timepoint, d$day)
  d$err <- switch(error, sem = d$sem, sd = d$sd, none = 0)
  d$err[is.na(d$err)] <- 0
  dodge <- if (x_axis == "timepoint" && error_style == "bars") ggplot2::position_dodge(width = 0.25) else ggplot2::position_identity()

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$mean, color = .data$group, group = .data$group))

  if (show_individuals && !is.null(sessions)) {
    ind <- sessions[!is.na(sessions[[parameter]]), , drop = FALSE]
    ind$x <- xpos(ind$timepoint, ind$day)
    ind$y <- ind[[parameter]]
    p <- p + ggplot2::geom_line(data = ind, ggplot2::aes(x = .data$x, y = .data$y, group = .data$subject, color = .data$group),
                                alpha = 0.25, linewidth = 0.4, inherit.aes = FALSE)
  }

  if (error != "none") {
    p <- p + if (error_style == "band") {
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$mean - .data$err, ymax = .data$mean + .data$err, fill = .data$group),
                           alpha = 0.18, color = NA)
    } else {
      ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$mean - .data$err, ymax = .data$mean + .data$err),
                             width = if (x_axis == "day") diff(range(d$x)) / 80 else 0.25, linewidth = 0.5, position = dodge)
    }
  }

  p <- p +
    ggplot2::geom_line(linewidth = 0.9, position = dodge) +
    ggplot2::geom_point(size = 2.3, position = dodge) +
    ggplot2::scale_color_manual(values = colors)
  if (error != "none" && error_style == "band") p <- p + ggplot2::scale_fill_manual(values = colors, guide = "none")

  if (!is.null(stats) && nrow(stats) > 0) {
    s <- stats[stats$parameter == parameter & !is.na(stats$p_adj) & stats$p_adj < 0.05, , drop = FALSE]
    if (nrow(s) > 0) {
      top <- max(d$mean + d$err, na.rm = TRUE)
      if (show_individuals && !is.null(sessions)) top <- max(top, sessions[[parameter]], na.rm = TRUE)
      rng <- diff(range(c(d$mean - d$err, top), na.rm = TRUE))
      tp_day <- tapply(d$day, d$timepoint, mean)
      s$x <- xpos(s$timepoint, tp_day[as.character(s$timepoint)])
      s$rank <- match(s$group, names(colors))
      s$y <- top + rng * (0.06 + 0.07 * (s$rank - 1))
      p <- p + ggplot2::geom_text(data = s, ggplot2::aes(x = .data$x, y = .data$y, label = .data$stars, color = .data$group),
                                  inherit.aes = FALSE, size = 4.5, fontface = "bold", show.legend = FALSE)
    }
  }

  if (x_axis == "timepoint") {
    p <- p + ggplot2::scale_x_continuous(breaks = seq_along(lv), labels = lv, expand = ggplot2::expansion(add = 0.4))
  }
  if (transform == "percent") p <- p + ggplot2::geom_hline(yintercept = 100, linetype = "dashed", color = "grey50")
  if (transform == "difference") p <- p + ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50")

  info <- wbp_parameter_info()
  full <- info$name[match(parameter, info$parameter)]
  err_lab <- switch(error, sem = "Mean \u00b1 SEM", sd = "Mean \u00b1 SD", none = "Mean")
  rotate <- x_axis == "timepoint" && length(lv) > 6
  p + ggplot2::labs(
    title = if (is.null(title)) (if (is.na(full)) parameter else full) else title,
    x = if (x_axis == "day") "Study day" else NULL,
    y = wbp_axis_label(parameter, transform),
    caption = paste0(err_lab, "; n = animals per group",
                     if (!is.null(stats) && nrow(stats) > 0) "; * adjusted p < 0.05 vs reference group" else "")
  ) + theme_plethr() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = if (rotate) 45 else 0, hjust = if (rotate) 1 else 0.5))
}

#' Plot One Value per Animal by Group
#'
#' Shows each animal as a point over the group mean \eqn{\pm} SEM (bar or box
#' style), with brackets for pairwise comparisons.
#'
#' @param values Output of [summarize_subjects()].
#' @param parameter Parameter to plot.
#' @param comparisons Optional output of [compare_groups()].
#' @param colors Named group colors.
#' @param style `"bar"` (mean \eqn{\pm} SEM with points), `"box"` (median and
#'   quartiles with points) or `"dot"` (points with mean \eqn{\pm} SEM lines).
#' @param show_ns Also draw brackets for non-significant comparisons.
#' @param y_label Y-axis label.
#' @param title Plot title.
#' @return A ggplot object.
#' @export
plot_group_comparison <- function(values, parameter, comparisons = NULL, colors = NULL,
                                  style = c("bar", "box", "dot"), show_ns = FALSE,
                                  y_label = parameter, title = NULL) {
  style <- match.arg(style)
  d <- values[values$parameter == parameter & !is.na(values$value), , drop = FALSE]
  if (nrow(d) == 0) stop("No data for parameter ", parameter)
  d$group <- droplevels(as.factor(d$group))
  if (is.null(colors)) colors <- plethr_colors(levels(d$group))

  sm <- d %>%
    dplyr::group_by(.data$group) %>%
    dplyr::summarize(mean = mean(.data$value),
                     sem = if (dplyr::n() > 1) stats::sd(.data$value) / sqrt(dplyr::n()) else 0,
                     .groups = "drop")

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$group, y = .data$value, color = .data$group, fill = .data$group))
  if (style == "bar") {
    p <- p +
      ggplot2::geom_col(data = sm, ggplot2::aes(y = .data$mean), width = 0.65, alpha = 0.35, linewidth = 0.6) +
      ggplot2::geom_errorbar(data = sm, ggplot2::aes(y = .data$mean, ymin = .data$mean - .data$sem, ymax = .data$mean + .data$sem),
                             width = 0.22, linewidth = 0.6, color = "black")
  } else if (style == "box") {
    p <- p + ggplot2::geom_boxplot(width = 0.6, alpha = 0.25, outlier.shape = NA, linewidth = 0.6)
  } else {
    p <- p +
      ggplot2::geom_errorbar(data = sm, ggplot2::aes(y = .data$mean, ymin = .data$mean - .data$sem, ymax = .data$mean + .data$sem),
                             width = 0.2, linewidth = 0.6, color = "black") +
      ggplot2::geom_errorbar(data = sm, ggplot2::aes(y = .data$mean, ymin = .data$mean, ymax = .data$mean),
                             width = 0.45, linewidth = 1, color = "black")
  }
  p <- p + ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.12, height = 0, seed = 1),
                               size = 2.6, shape = 21, color = "black", stroke = 0.4, alpha = 0.9)

  top <- max(c(d$value, sm$mean + sm$sem), na.rm = TRUE)
  bottom <- min(c(0, d$value), na.rm = TRUE)
  rng <- top - bottom
  if (!is.null(comparisons)) {
    cm <- comparisons[comparisons$parameter == parameter & !is.na(comparisons$p_adj), , drop = FALSE]
    if (!show_ns) cm <- cm[cm$p_adj < 0.05, , drop = FALSE]
    if (nrow(cm) > 0) {
      lv <- levels(d$group)
      cm$x1 <- match(cm$group1, lv)
      cm$x2 <- match(cm$group2, lv)
      cm <- cm[!is.na(cm$x1) & !is.na(cm$x2), , drop = FALSE]
      cm <- cm[order(abs(cm$x2 - cm$x1)), , drop = FALSE]
      cm$y <- top + rng * 0.08 * seq_len(nrow(cm))
      cm$label <- ifelse(cm$stars == "ns", "ns",
                         paste0(cm$stars, "\n", ifelse(cm$p_adj < 0.001, "p < 0.001", sprintf("p = %.3f", cm$p_adj))))
      p <- p +
        ggplot2::geom_segment(data = cm, ggplot2::aes(x = .data$x1, xend = .data$x2, y = .data$y, yend = .data$y),
                              inherit.aes = FALSE, linewidth = 0.4) +
        ggplot2::geom_segment(data = cm, ggplot2::aes(x = .data$x1, xend = .data$x1, y = .data$y, yend = .data$y - rng * 0.015),
                              inherit.aes = FALSE, linewidth = 0.4) +
        ggplot2::geom_segment(data = cm, ggplot2::aes(x = .data$x2, xend = .data$x2, y = .data$y, yend = .data$y - rng * 0.015),
                              inherit.aes = FALSE, linewidth = 0.4) +
        ggplot2::geom_text(data = cm, ggplot2::aes(x = (.data$x1 + .data$x2) / 2, y = .data$y, label = .data$label),
                           inherit.aes = FALSE, vjust = -0.25, size = 3.1, lineheight = 0.85)
      top <- max(cm$y) + rng * 0.1
    }
  }

  info <- wbp_parameter_info()
  full <- info$name[match(parameter, info$parameter)]
  p +
    ggplot2::scale_color_manual(values = colors, guide = "none") +
    ggplot2::scale_fill_manual(values = colors, guide = "none") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(if (style == "bar") 0 else 0.05, 0.08))) +
    ggplot2::coord_cartesian(ylim = c(if (style == "bar") min(0, bottom) else NA, top + rng * 0.05)) +
    ggplot2::labs(title = if (is.null(title)) (if (is.na(full)) parameter else full) else title,
                  x = NULL, y = y_label,
                  caption = paste0(if (style == "box") "Box: median and quartiles" else "Mean \u00b1 SEM",
                                   "; each point is one animal")) +
    theme_plethr() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = if (nlevels(d$group) > 3) 30 else 0,
                                                       hjust = if (nlevels(d$group) > 3) 1 else 0.5))
}

#' Heatmap of Percent Difference from a Reference Group
#'
#' @param comparisons Output of [compare_groups()] with a reference group (each row
#'   compares `group2` against `group1`), or any data frame with columns `row`,
#'   `col`, `pct_difference` and `stars`.
#' @param rows,cols Column names to use for heatmap rows and columns.
#' @param limit Color scale limit in percent; larger differences are capped.
#' @param cluster_rows Order rows by hierarchical clustering of their profiles.
#' @param title Plot title.
#' @return A ggplot object.
#' @export
plot_difference_heatmap <- function(comparisons, rows = "parameter", cols = "group2",
                                    limit = NULL, cluster_rows = TRUE, title = NULL) {
  d <- comparisons
  d$row <- d[[rows]]
  d$col <- d[[cols]]
  if (is.null(limit)) limit <- max(10, min(100, stats::quantile(abs(d$pct_difference), 0.95, na.rm = TRUE)))
  d$fill <- pmax(pmin(d$pct_difference, limit), -limit)

  row_levels <- unique(as.character(d$row))
  if (cluster_rows && length(row_levels) > 2) {
    m <- tidyr::pivot_wider(d[, c("row", "col", "fill")], names_from = "col", values_from = "fill")
    mm <- as.matrix(m[, -1, drop = FALSE])
    mm[is.na(mm)] <- 0
    if (ncol(mm) >= 1) row_levels <- as.character(m$row)[stats::hclust(stats::dist(mm))$order]
  }
  d$row <- factor(d$row, levels = rev(row_levels))
  if (!is.factor(d$col)) d$col <- factor(d$col, levels = unique(d$col))
  d$label <- ifelse(is.na(d$pct_difference), "",
                    paste0(sprintf("%+.0f%%", d$pct_difference), ifelse(d$stars %in% c("", "ns"), "", paste0("\n", d$stars))))

  ggplot2::ggplot(d, ggplot2::aes(x = .data$col, y = .data$row, fill = .data$fill)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label,
                                    color = abs(.data$fill) > limit * 0.6),
                       size = 3, lineheight = 0.8, show.legend = FALSE) +
    ggplot2::scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "black")) +
    ggplot2::scale_fill_gradientn(colors = c("#2166AC", "#67A9CF", "#F7F7F7", "#EF8A62", "#B2182B"),
                                  limits = c(-limit, limit), name = "% difference",
                                  na.value = "grey90") +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(x = NULL, y = NULL, title = title,
                  caption = "Cell text: % difference in group mean; stars mark adjusted p < 0.05") +
    theme_plethr() +
    ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                   legend.position = "right", legend.title = ggplot2::element_text(face = "bold", size = 10),
                   axis.text.x.top = ggplot2::element_text(angle = 30, hjust = 0))
}

#' PCA of Animals
#'
#' Principal component analysis with one point per animal, using one summary value
#' per parameter (e.g. from [summarize_subjects()]). Parameters are centered and
#' scaled, so each contributes equally regardless of units.
#'
#' @param values Output of [summarize_subjects()].
#' @param colors Named group colors.
#' @param show_labels Label each point with the animal name.
#' @param group_shapes `"hull"` outlines each group's animals (honest for small
#'   groups), `"ellipse"` draws 95 percent normal ellipses (for groups with at least 3
#'   animals; very uncertain with few animals), `"none"` draws neither.
#' @param n_loadings Number of parameter loading arrows to draw (0 for none).
#' @param title Plot title.
#' @return A list with `plot`, `variance` (data frame), `scores`, `loadings`,
#'   `dropped` (parameters removed for missing values or zero variance).
#' @export
plot_subject_pca <- function(values, colors = NULL, show_labels = FALSE,
                             group_shapes = c("hull", "ellipse", "none"),
                             n_loadings = 5, title = "PCA of animals") {
  group_shapes <- match.arg(group_shapes)
  wide <- tidyr::pivot_wider(values[, c("subject", "group", "parameter", "value")],
                             names_from = "parameter", values_from = "value")
  params <- setdiff(names(wide), c("subject", "group"))
  mat <- as.matrix(wide[, params, drop = FALSE])
  ok <- colSums(is.na(mat)) == 0 & apply(mat, 2, function(x) stats::sd(x, na.rm = TRUE) > 0)
  dropped <- params[!ok]
  mat <- mat[, ok, drop = FALSE]
  if (ncol(mat) < 2) stop("PCA needs at least 2 parameters with complete, non-constant values.")
  if (nrow(mat) < 3) stop("PCA needs at least 3 animals.")

  pca <- stats::prcomp(mat, center = TRUE, scale. = TRUE)
  ve <- pca$sdev^2 / sum(pca$sdev^2) * 100
  variance <- data.frame(Component = paste0("PC", seq_along(ve)), `Variance (%)` = ve,
                         `Cumulative (%)` = cumsum(ve), check.names = FALSE)
  scores <- data.frame(subject = wide$subject, group = wide$group, PC1 = pca$x[, 1], PC2 = pca$x[, 2])
  if (is.null(colors)) colors <- plethr_colors(levels(droplevels(as.factor(scores$group))))

  p <- ggplot2::ggplot(scores, ggplot2::aes(x = .data$PC1, y = .data$PC2, color = .data$group))
  p <- p + ggplot2::geom_hline(yintercept = 0, color = "grey85") + ggplot2::geom_vline(xintercept = 0, color = "grey85")
  if (group_shapes == "ellipse") {
    big <- names(which(table(scores$group) >= 3))
    if (length(big)) {
      p <- p + ggplot2::stat_ellipse(data = scores[scores$group %in% big, ], ggplot2::aes(fill = .data$group),
                                     geom = "polygon", alpha = 0.12, level = 0.95, show.legend = FALSE)
    }
  } else if (group_shapes == "hull") {
    hulls <- do.call(rbind, lapply(split(scores, scores$group), function(g) {
      if (nrow(g) < 3) return(NULL)
      g[grDevices::chull(g$PC1, g$PC2), , drop = FALSE]
    }))
    if (!is.null(hulls)) {
      p <- p + ggplot2::geom_polygon(data = hulls, ggplot2::aes(fill = .data$group, group = .data$group),
                                     alpha = 0.12, linewidth = 0.4, show.legend = FALSE)
    }
  }
  if (n_loadings > 0) {
    ld <- data.frame(parameter = rownames(pca$rotation), PC1 = pca$rotation[, 1], PC2 = pca$rotation[, 2])
    ld <- ld[order(-(ld$PC1^2 + ld$PC2^2)), , drop = FALSE][seq_len(min(n_loadings, nrow(ld))), , drop = FALSE]
    k <- 0.8 * max(abs(scores[, c("PC1", "PC2")])) / max(abs(ld[, c("PC1", "PC2")]))
    p <- p +
      ggplot2::geom_segment(data = ld, ggplot2::aes(x = 0, y = 0, xend = .data$PC1 * k, yend = .data$PC2 * k),
                            inherit.aes = FALSE, color = "grey35",
                            arrow = ggplot2::arrow(length = ggplot2::unit(0.18, "cm"))) +
      ggplot2::geom_text(data = ld, ggplot2::aes(x = .data$PC1 * k * 1.1, y = .data$PC2 * k * 1.1, label = .data$parameter),
                         inherit.aes = FALSE, color = "grey25", size = 3.3, fontface = "italic")
  }
  p <- p + ggplot2::geom_point(size = 3.2)
  if (show_labels) {
    p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = .data$subject), size = 3, show.legend = FALSE,
                                      max.overlaps = 30, seed = 1)
  }
  has_fill <- any(vapply(p$layers, function(l) "fill" %in% names(l$mapping), logical(1)))
  if (has_fill) p <- p + ggplot2::scale_fill_manual(values = colors)
  p <- p +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(title = title, x = sprintf("PC1 (%.1f%%)", ve[1]), y = sprintf("PC2 (%.1f%%)", ve[2]),
                  caption = paste0("Parameters centered and scaled (", ncol(mat), " parameters)",
                                   if (n_loadings > 0) "; arrows: strongest parameter loadings" else "")) +
    theme_plethr()

  list(plot = p, variance = variance, scores = scores,
       loadings = as.data.frame(pca$rotation[, 1:min(3, ncol(pca$rotation)), drop = FALSE]),
       dropped = dropped)
}
