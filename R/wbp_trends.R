# Per-animal trends: the direction each animal's values move over time.

#' Trend of Each Animal over Time
#'
#' Fits a straight line to each animal's values against study day within a time
#' window and reports the slope (per week), its p-value and a direction label.
#'
#' @param sessions Output of [summarize_sessions()] (or [lung_health_score()]).
#' @param variable Column to analyze, e.g. `"Penh"` or `"lung_score"`.
#' @param from,to First and last timepoint of the window. Default: all timepoints.
#' @param higher_is What higher values mean: `"neutral"` (labels increasing /
#'   decreasing), `"worse"` (labels worsening / improving, e.g. a disease score) or
#'   `"better"`.
#' @param alpha Significance level for calling a trend.
#' @return A data frame with one row per animal: `subject`, `group`, `n`,
#'   `slope_per_week`, `change` (fitted change over the window), `p`, `direction`.
#' @export
animal_trends <- function(sessions, variable, from = NULL, to = NULL,
                          higher_is = c("neutral", "worse", "better"), alpha = 0.05) {
  higher_is <- match.arg(higher_is)
  lv <- levels(sessions$timepoint)
  if (is.null(from)) from <- lv[1]
  if (is.null(to)) to <- lv[length(lv)]
  window <- lv[match(from, lv):match(to, lv)]
  d <- sessions[as.character(sessions$timepoint) %in% window & !is.na(sessions[[variable]]), , drop = FALSE]
  up <- switch(higher_is, neutral = "increasing", worse = "worsening", better = "improving")
  down <- switch(higher_is, neutral = "decreasing", worse = "improving", better = "worsening")
  out <- lapply(split(d, as.character(d$subject)), function(a) {
    res <- data.frame(subject = a$subject[1], group = as.character(a$group[1]), n = nrow(a),
                      slope_per_week = NA_real_, change = NA_real_, p = NA_real_, direction = "too few sessions",
                      stringsAsFactors = FALSE)
    if (nrow(a) >= 3 && stats::sd(a$day) > 0) {
      fit <- stats::lm(a[[variable]] ~ a$day)
      co <- summary(fit)$coefficients
      slope <- co[2, 1]
      res$slope_per_week <- slope * 7
      res$change <- slope * diff(range(a$day))
      res$p <- if (nrow(co) >= 2 && ncol(co) >= 4) co[2, 4] else NA_real_
      res$direction <- if (!is.na(res$p) && res$p < alpha) (if (slope > 0) up else down) else "no clear trend"
    }
    res
  })
  out <- do.call(rbind, out)
  if (is.factor(sessions$group)) out$group <- factor(out$group, levels = levels(sessions$group))
  out <- out[order(out$group, out$subject), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "window") <- window
  out
}

#' Plot Each Animal's Trend
#'
#' One small panel per animal showing its values at each session, a trend line,
#' and the direction of the trend (from [animal_trends()]) in the panel title.
#'
#' @param sessions Output of [summarize_sessions()] (or [lung_health_score()]).
#' @param variable Column to plot.
#' @param trends Optional output of [animal_trends()]; computed if `NULL`.
#' @param colors Named group colors.
#' @param method Trend line: `"linear"` (matches the slope and p-value) or `"loess"` (smooth curve).
#' @param from,to Window used for the trend (shaded). Default: all timepoints.
#' @param higher_is Passed to [animal_trends()].
#' @param reference_line Optional y value to draw as a dashed line (e.g. 0 for a health score).
#' @param y_label Y-axis label.
#' @param title Plot title.
#' @param ncol Number of panel columns.
#' @return A ggplot object.
#' @export
plot_animal_trends <- function(sessions, variable, trends = NULL, colors = NULL, method = c("linear", "loess"),
                               from = NULL, to = NULL, higher_is = c("neutral", "worse", "better"),
                               reference_line = NULL, y_label = variable, title = NULL, ncol = 4) {
  method <- match.arg(method)
  higher_is <- match.arg(higher_is)
  if (is.null(trends)) trends <- animal_trends(sessions, variable, from, to, higher_is)
  window <- attr(trends, "window")
  d <- sessions[!is.na(sessions[[variable]]), , drop = FALSE]
  d$y <- d[[variable]]
  d$group <- as.character(d$group)
  if (is.null(colors)) colors <- plethr_colors(unique(as.character(trends$group)))

  arrow <- c(increasing = "\u2191", decreasing = "\u2193", worsening = "\u2191", improving = "\u2193")
  if (higher_is == "better") arrow[c("worsening", "improving")] <- c("\u2193", "\u2191")
  trends$label <- sprintf("%s (%s)\n%s %s%s", trends$subject, trends$group,
                          ifelse(trends$direction %in% names(arrow), arrow[trends$direction], "\u2192"), trends$direction,
                          ifelse(is.na(trends$slope_per_week), "", sprintf(", %+.2g/wk", trends$slope_per_week)))
  d$label <- factor(trends$label[match(d$subject, trends$subject)], levels = trends$label)
  d <- d[!is.na(d$label), , drop = FALSE]
  d$in_window <- as.character(d$timepoint) %in% window
  span <- range(d$day[d$in_window])

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$day, y = .data$y, color = .data$group)) +
    ggplot2::annotate("rect", xmin = span[1], xmax = span[2], ymin = -Inf, ymax = Inf, fill = "#1F5F8B", alpha = 0.05)
  if (!is.null(reference_line)) p <- p + ggplot2::geom_hline(yintercept = reference_line, linetype = "dashed", color = "grey55")
  p <- p +
    ggplot2::geom_line(alpha = 0.45, linewidth = 0.5) +
    ggplot2::geom_point(size = 1.6) +
    ggplot2::geom_smooth(data = d[d$in_window, , drop = FALSE], method = if (method == "linear") "lm" else "loess",
                         formula = y ~ x, se = FALSE, linewidth = 1.1, span = 0.9) +
    ggplot2::facet_wrap(~ label, ncol = ncol) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(title = title, x = "Study day", y = y_label,
                  caption = paste0("Thick line: ", if (method == "linear") "linear trend" else "LOESS smooth",
                                   " within the shaded window; direction from the linear slope (p < 0.05, descriptive: sessions treated as independent); slope per week")) +
    theme_plethr() +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 8.5, face = "bold", hjust = 0),
                   panel.spacing = ggplot2::unit(0.8, "lines"))
  p
}
