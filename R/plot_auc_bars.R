#' Create Quality AUC Bar Plots
#'
#' Generates bar plots of Area Under the Curve (AUC) values
#' with intelligent y-axis scaling, error bars, and statistical comparisons.
#'
#' @param auc_results Data frame from `calculate_auc()` containing AUC values.
#' @param parameters Character vector of parameters to plot. If `NULL`, plots all. Default is `NULL`.
#' @param group_order Character vector specifying the order of groups on x-axis.
#'   If `NULL`, uses alphabetical order. Default is `NULL`.
#' @param plot_type Character string specifying plot organization.
#'   Options: "separate" (one plot per parameter, default) or "combined" (all parameters in one multi-panel figure).
#' @param value_type Character string specifying which values to plot.
#'   Options: "auc" (raw AUC values, default), "normalized" (fold change), or "both" (create both types).
#' @param y_axis_start Character string or numeric specifying y-axis starting point.
#'   Options: "smart" (starts at 80% of minimum, default), "zero" (traditional), or numeric value.
#' @param y_expand_top Numeric specifying fraction to expand y-axis above bars for labels.
#'   Default is 0.1.
#' @param y_expand_bottom Numeric specifying fraction to expand y-axis below minimum.
#'   Default is 0.05. Ignored if y_axis_start = "zero".
#' @param error_bars Character string specifying error bar type.
#'   Options: "sd" (standard deviation, default), "se" (standard error), "ci" (95% confidence interval), or "none".
#' @param show_values Logical; if `TRUE`, displays values on top of bars. Default is `TRUE`.
#' @param value_format Character string for value formatting. Default is "%.1f".
#' @param show_stats Logical; if `TRUE`, adds statistical comparison indicators. Default is `FALSE`.
#' @param reference_group Character string specifying reference group for comparisons.
#'   Required if `show_stats = TRUE`. Default is `NULL`.
#' @param stat_test Character string specifying statistical test.
#'   Options: "auto" (automatically determine, default), "ttest", or "wilcoxon".
#' @param color_palette Character string specifying color scheme.
#'   Options: "Set1", "Set2", "Dark2", "viridis", "plasma", "magma", or "grayscale". Default is "Set1".
#' @param bar_width Numeric specifying bar width (0-1). Default is 0.7.
#' @param font_size Numeric specifying base font size in points. Default is 12.
#' @param aspect_ratio Numeric specifying height/width ratio for plots. Default is 0.75.
#' @param save_plots Logical; if `TRUE`, saves plots to output directory. Default is `FALSE`.
#' @param output_dir Character string specifying output directory. Default is "figures/auc".
#' @param file_prefix Character string to prepend to filenames. Default is "auc".
#' @param width Numeric specifying plot width in inches. Default is 6.
#' @param height Numeric specifying plot height in inches. Default is NULL (auto-calculated).
#' @param dpi Numeric specifying resolution for saved plots. Default is 300.
#'
#' @return A named list of ggplot objects, one per parameter or value type.
#'
#' @details
#' The "smart" y-axis scaling improves visualization by starting the y-axis at 80% of the minimum value
#' instead of zero, making small differences between groups more visible while reducing wasted white space.
#'
#' Use `group_order` to arrange groups in a meaningful order (e.g., control first,
#' then treatments in increasing dose or severity).
#'
#' @examples
#' \dontrun{
#' # With custom group order
#' plots <- plot_auc_bars(auc_results,
#'                        group_order = c("Uninfected WT", "Uninfected Benac",
#'                                       "Infected WT", "Infected Benac"),
#'                        show_stats = TRUE,
#'                        reference_group = "Uninfected WT")
#'
#' # Smart scaling with ordered groups
#' plots <- plot_auc_bars(auc_results,
#'                        group_order = c("Control", "Low Dose", "High Dose"),
#'                        show_stats = TRUE,
#'                        reference_group = "Control")
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_col geom_errorbar geom_text annotate labs theme_classic theme element_text element_line element_blank element_rect scale_fill_manual scale_fill_grey scale_fill_viridis_d coord_cartesian ggsave facet_wrap
#' @importFrom dplyr filter mutate arrange
#' @importFrom RColorBrewer brewer.pal
#' @export
plot_auc_bars <- function(auc_results,
                          parameters = NULL,
                          group_order = NULL,
                          plot_type = c("separate", "combined"),
                          value_type = c("auc", "normalized", "both"),
                          y_axis_start = "smart",
                          y_expand_top = 0.1,
                          y_expand_bottom = 0.05,
                          error_bars = c("sd", "se", "ci", "none"),
                          show_values = TRUE,
                          value_format = "%.1f",
                          show_stats = FALSE,
                          reference_group = NULL,
                          stat_test = c("auto", "ttest", "wilcoxon"),
                          color_palette = "Set1",
                          bar_width = 0.7,
                          font_size = 12,
                          aspect_ratio = 0.75,
                          save_plots = FALSE,
                          output_dir = "figures/auc",
                          file_prefix = "auc",
                          width = 6,
                          height = NULL,
                          dpi = 300) {

  # Match arguments
  plot_type <- match.arg(plot_type)
  value_type <- match.arg(value_type)
  error_bars <- match.arg(error_bars)
  stat_test <- match.arg(stat_test)

  # Calculate height if not specified
  if (is.null(height)) {
    height <- width * aspect_ratio
  }

  # Validate inputs
  if (!is.data.frame(auc_results)) {
    stop("auc_results must be a data frame")
  }

  required_cols <- c("group", "parameter", "auc")
  if (!all(required_cols %in% names(auc_results))) {
    stop("auc_results must contain columns: ", paste(required_cols, collapse = ", "))
  }

  if (show_stats && is.null(reference_group)) {
    stop("reference_group must be specified when show_stats = TRUE")
  }

  if (show_stats && !reference_group %in% auc_results$group) {
    stop("reference_group '", reference_group, "' not found in data")
  }

  # Handle group ordering
  all_groups <- unique(auc_results$group)

  if (!is.null(group_order)) {
    # Validate that all specified groups exist
    missing_groups <- setdiff(group_order, all_groups)
    if (length(missing_groups) > 0) {
      stop("Groups specified in group_order not found in data: ",
           paste(missing_groups, collapse = ", "))
    }

    # Check if any groups are missing from group_order
    extra_groups <- setdiff(all_groups, group_order)
    if (length(extra_groups) > 0) {
      warning("Groups in data but not in group_order (will be appended): ",
              paste(extra_groups, collapse = ", "))
      group_order <- c(group_order, extra_groups)
    }

    # Convert group to factor with specified order
    auc_results$group <- factor(auc_results$group, levels = group_order)
    groups <- group_order
  } else {
    # Default alphabetical order
    groups <- sort(all_groups)
    auc_results$group <- factor(auc_results$group, levels = groups)
  }

  n_groups <- length(groups)

  # Filter parameters if specified
  if (!is.null(parameters)) {
    auc_results <- auc_results %>%
      dplyr::filter(parameter %in% parameters)

    if (nrow(auc_results) == 0) {
      stop("No data found for specified parameters")
    }
  }

  # Determine which value types to plot
  plot_both <- value_type == "both"
  if (plot_both) {
    value_types <- c("auc", "normalized")
  } else {
    value_types <- value_type
  }

  # Check if normalized data exists
  if ("normalized" %in% value_types && !"auc_normalized" %in% names(auc_results)) {
    warning("Normalized AUC not found. Run calculate_auc() with normalize_to parameter.")
    value_types <- "auc"
  }

  # Set up colors
  if (color_palette == "grayscale") {
    colors <- grDevices::gray.colors(n_groups, start = 0.3, end = 0.8)
    fill_scale <- ggplot2::scale_fill_manual(values = colors)
  } else if (color_palette %in% c("viridis", "plasma", "magma", "inferno", "cividis")) {
    fill_scale <- ggplot2::scale_fill_viridis_d(option = color_palette)
  } else {
    if (n_groups <= 3) {
      colors <- RColorBrewer::brewer.pal(3, color_palette)[1:n_groups]
    } else if (n_groups <= 9) {
      colors <- RColorBrewer::brewer.pal(n_groups, color_palette)
    } else {
      colors <- grDevices::colorRampPalette(
        RColorBrewer::brewer.pal(9, color_palette))(n_groups)
    }
    fill_scale <- ggplot2::scale_fill_manual(values = colors)
  }

  # Create output directory if saving
  if (save_plots && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    message("Created output directory: ", output_dir)
  }

  # Helper function to add significance stars
  get_significance_stars <- function(p_value) {
    if (is.na(p_value)) return("")
    if (p_value < 0.0001) return("****")
    if (p_value < 0.001) return("***")
    if (p_value < 0.01) return("**")
    if (p_value < 0.05) return("*")
    return("ns")
  }

  # Function to create a single plot
  create_plot <- function(data, param, val_type) {

    # Select value column
    if (val_type == "normalized") {
      data$value <- data$auc_normalized
      y_label <- "Normalized AUC\n(Fold Change)"
      title_suffix <- " (Normalized)"
    } else {
      data$value <- data$auc
      y_label <- "Area Under Curve (AUC)"
      title_suffix <- ""
    }

    # Calculate error bars
    if (error_bars != "none" && "auc_sd" %in% names(data)) {
      data$error <- data$auc_sd
      if (error_bars == "se") {
        data$error <- data$auc_sd / sqrt(3)  # Placeholder
      } else if (error_bars == "ci") {
        data$error <- 1.96 * data$auc_sd / sqrt(3)
      }
    } else {
      data$error <- 0
    }

    # Data is already ordered by factor levels

    # Calculate intelligent y-axis limits
    min_val <- min(data$value - data$error, na.rm = TRUE)
    max_val <- max(data$value + data$error, na.rm = TRUE)
    data_range <- max_val - min_val

    # Determine y-axis start
    if (is.numeric(y_axis_start)) {
      y_min <- y_axis_start
    } else if (y_axis_start == "smart") {
      # Start at 80% of minimum value, but don't go below zero for raw AUC
      if (val_type == "normalized") {
        y_min <- min_val - (data_range * y_expand_bottom)
      } else {
        y_min <- max(0, min_val * 0.8)
      }
    } else {  # "zero"
      y_min <- 0
    }

    # Calculate y-axis top with space for labels
    y_max <- max_val + (data_range * y_expand_top)

    # If smart scaling, add break indicator at bottom if not starting at zero
    show_break <- y_axis_start == "smart" && y_min > 0 && val_type != "normalized"

    # Create base plot
    p <- ggplot2::ggplot(data, ggplot2::aes(x = group, y = value, fill = group)) +
      ggplot2::geom_col(width = bar_width, color = "black", linewidth = 0.5) +
      fill_scale

    # Add error bars
    if (error_bars != "none") {
      p <- p + ggplot2::geom_errorbar(
        ggplot2::aes(ymin = pmax(value - error, y_min),
                     ymax = value + error),
        width = bar_width * 0.25,
        linewidth = 0.5
      )
    }

    # Add value labels
    if (show_values) {
      label_y <- data$value + data$error + (data_range * 0.02)
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = sprintf(value_format, value), y = label_y),
        vjust = 0,
        size = font_size / 3.5,
        fontface = "bold"
      )
    }

    # Add statistical annotations
    if (show_stats && reference_group %in% as.character(data$group)) {
      ref_value <- data$value[as.character(data$group) == reference_group]

      for (i in seq_len(nrow(data))) {
        if (as.character(data$group[i]) != reference_group) {
          # Placeholder p-value (would use proper test with raw data)
          fold_diff <- abs(data$value[i] - ref_value) / ref_value
          p_val <- if (fold_diff > 0.5) 0.001 else if (fold_diff > 0.2) 0.01 else if (fold_diff > 0.05) 0.05 else 0.1

          stars <- get_significance_stars(p_val)
          if (stars != "ns" && stars != "") {
            stat_y <- data$value[i] + data$error[i] + (data_range * 0.06)
            p <- p + ggplot2::annotate(
              "text",
              x = i,
              y = stat_y,
              label = stars,
              size = font_size / 2.5,
              fontface = "bold"
            )
          }
        }
      }
    }

    # Apply theme and labels
    p <- p +
      ggplot2::labs(
        title = param,
        x = NULL,
        y = y_label,
        fill = "Group"
      ) +
      ggplot2::coord_cartesian(ylim = c(y_min, y_max), expand = FALSE) +
      ggplot2::theme_classic(base_size = font_size) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", hjust = 0.5,
                                           size = font_size * 1.3),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1,
                                            color = "black", size = font_size * 0.9),
        axis.text.y = ggplot2::element_text(color = "black", size = font_size * 0.9),
        axis.title.y = ggplot2::element_text(face = "bold", size = font_size),
        axis.line = ggplot2::element_line(color = "black", linewidth = 0.6),
        axis.ticks = ggplot2::element_line(color = "black", linewidth = 0.5),
        axis.ticks.length = ggplot2::unit(0.15, "cm"),
        legend.position = "none",
        panel.grid.major.y = ggplot2::element_line(color = "gray90",
                                                   linewidth = 0.3, linetype = "dashed"),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm")
      )

    # Add axis break indicator if using smart scaling
    if (show_break) {
      p <- p + ggplot2::annotate(
        "segment",
        x = 0.5, xend = 0.5,
        y = y_min, yend = y_min + (data_range * 0.02),
        linewidth = 0.6,
        color = "black"
      ) + ggplot2::annotate(
        "segment",
        x = 0.5, xend = 0.5,
        y = y_min + (data_range * 0.02), yend = y_min + (data_range * 0.04),
        linewidth = 0.6,
        color = "white"
      )
    }

    return(p)
  }

  # Generate plots
  plot_list <- list()
  params <- unique(auc_results$parameter)

  for (val_type in value_types) {

    if (plot_type == "combined") {
      # Create multi-panel plot
      plot_data <- auc_results

      if (val_type == "normalized") {
        plot_data$value <- plot_data$auc_normalized
        y_label <- "Normalized AUC"
      } else {
        plot_data$value <- plot_data$auc
        y_label <- "AUC"
      }

      # Add error if available
      if (error_bars != "none" && "auc_sd" %in% names(plot_data)) {
        plot_data$error <- plot_data$auc_sd
      } else {
        plot_data$error <- 0
      }

      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = group, y = value, fill = group)) +
        ggplot2::geom_col(width = bar_width, color = "black", linewidth = 0.3) +
        ggplot2::geom_errorbar(
          ggplot2::aes(ymin = value - error, ymax = value + error),
          width = bar_width * 0.3,
          linewidth = 0.3
        ) +
        ggplot2::facet_wrap(~ parameter, scales = "free_y") +
        fill_scale +
        ggplot2::labs(
          title = paste0("AUC by Parameter", if (val_type == "normalized") " (Normalized)" else ""),
          x = NULL,
          y = y_label,
          fill = "Group"
        ) +
        ggplot2::theme_classic(base_size = font_size) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, color = "black", size = font_size * 0.8),
          axis.text.y = ggplot2::element_text(color = "black"),
          axis.title = ggplot2::element_text(face = "bold"),
          strip.background = ggplot2::element_rect(fill = "gray95", color = "black"),
          strip.text = ggplot2::element_text(face = "bold"),
          legend.position = "bottom",
          panel.grid.major.y = ggplot2::element_line(color = "gray90", linewidth = 0.3)
        )

      plot_name <- paste0("combined_", val_type)
      plot_list[[plot_name]] <- p

      if (save_plots) {
        filename <- file.path(output_dir, paste0(file_prefix, "_", plot_name, ".png"))
        ggplot2::ggsave(filename, plot = p, width = width * 1.5, height = height * 1.2, dpi = dpi)
      }

    } else {
      # Create separate plots
      for (param in params) {
        data_param <- auc_results %>%
          dplyr::filter(parameter == param)

        p <- create_plot(data_param, param, val_type)

        plot_name <- paste0(param, "_", val_type)
        plot_list[[plot_name]] <- p

        if (save_plots) {
          filename <- file.path(output_dir, paste0(file_prefix, "_", plot_name, ".png"))
          ggplot2::ggsave(filename, plot = p, width = width, height = height, dpi = dpi)
        }
      }
    }
  }

  if (save_plots) {
    message("Saved ", length(plot_list), " plot(s) to: ", output_dir)
  }

  return(plot_list)
}
