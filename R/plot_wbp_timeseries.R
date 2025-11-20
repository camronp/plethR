#' Plot Plethysmography Time Series Data
#'
#' Creates time series plots of respiratory parameters from whole body plethysmography data.
#' Supports multiple grouping strategies, smoothing, and customization options.
#'
#' @param data Either a single data frame or a named list of data frames. Can be output from
#'   `calculate_group_averages()` or `organize_by_groups()`.
#' @param time_var Character string specifying the time column name. Default is "Time".
#' @param plot_type Character string specifying plot organization:
#'   - "by_parameter": One plot per parameter, all groups on same plot (default)
#'   - "by_group": One plot per group, all parameters as facets
#'   - "combined": Single plot with all groups and parameters
#' @param parameters Character vector of parameters to plot. If `NULL`, plots all numeric columns
#'   except the time variable. Default is `NULL`.
#' @param smooth_method Character string specifying smoothing method:
#'   - "none": No smoothing (default)
#'   - "rolling": Rolling/moving average
#'   - "loess": LOESS smoothing
#' @param smooth_window Integer specifying window size for rolling average. Default is 5.
#' @param smooth_span Numeric specifying span for LOESS smoothing (0-1). Default is 0.1.
#' @param color_palette Character string specifying color palette. Options include:
#'   - "Set1", "Set2", "Dark2" (ColorBrewer)
#'   - "viridis", "plasma", "magma" (viridis palettes)
#'   Default is "Set1".
#' @param facet_scales Character string for facet scales ("fixed", "free", "free_x", "free_y").
#'   Default is "free_y".
#' @param line_size Numeric specifying line thickness. Default is 0.8.
#' @param show_points Logical; if `TRUE`, adds points to lines. Default is `FALSE`.
#' @param show_se Logical; if `TRUE` and using LOESS smoothing, shows confidence interval.
#'   Default is `FALSE`.
#' @param save_plots Logical; if `TRUE`, saves plots to output directory. Default is `FALSE`.
#' @param output_dir Character string specifying output directory for saved plots.
#'   Default is "plots".
#' @param file_prefix Character string to prepend to saved plot filenames. Default is "plot".
#' @param width Numeric specifying plot width in inches for saved plots. Default is 10.
#' @param height Numeric specifying plot height in inches for saved plots. Default is 6.
#'
#' @return A list of ggplot objects. Can be displayed individually or using `patchwork` to combine.
#'
#' @details
#' This function provides flexible visualization of plethysmography data with options for:
#' - Multiple plot layouts (by parameter, by group, or combined)
#' - Data smoothing (rolling average or LOESS)
#' - Customizable aesthetics (colors, line sizes, points)
#' - Automatic plot saving
#'
#' The function automatically handles both single data frames (e.g., from `combine_all = TRUE`)
#' and lists of data frames (e.g., from `calculate_group_averages()`).
#'
#' @examples
#' \dontrun{
#' # Load and process data
#' df_list <- sheets_into_list("data.xlsx", clean_time = TRUE)
#' groups <- set_group_names("Control", "Treated")
#' mapping <- assign_groups_interactive(groups, df_list)
#' averages <- calculate_group_averages(df_list, mapping)
#'
#' # Basic plot by parameter
#' plots <- plot_wbp_timeseries(averages)
#'
#' # Plot specific parameters with smoothing
#' plots <- plot_wbp_timeseries(averages,
#'                              parameters = c("f", "TVb", "Penh"),
#'                              smooth_method = "rolling",
#'                              smooth_window = 3)
#'
#' # Plot by group with LOESS smoothing
#' plots <- plot_wbp_timeseries(averages,
#'                              plot_type = "by_group",
#'                              smooth_method = "loess",
#'                              smooth_span = 0.2)
#'
#' # Save plots with custom styling
#' plots <- plot_wbp_timeseries(averages,
#'                              color_palette = "viridis",
#'                              line_size = 1.2,
#'                              show_points = TRUE,
#'                              save_plots = TRUE,
#'                              output_dir = "figures",
#'                              file_prefix = "wbp")
#'
#' # View individual plots
#' plots[[1]]  # First plot
#'
#' # Combine with patchwork (if installed)
#' # library(patchwork)
#' # wrap_plots(plots[1:4], ncol = 2)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_smooth facet_wrap labs theme_bw theme element_text scale_color_manual scale_color_viridis_d ggsave
#' @importFrom dplyr filter group_by mutate ungroup arrange
#' @importFrom tidyr pivot_longer
#' @importFrom zoo rollmean
#' @importFrom RColorBrewer brewer.pal
#' @export
plot_wbp_timeseries <- function(data,
                                time_var = "Time",
                                plot_type = c("by_parameter", "by_group", "combined"),
                                parameters = NULL,
                                smooth_method = c("none", "rolling", "loess"),
                                smooth_window = 5,
                                smooth_span = 0.1,
                                color_palette = "Set1",
                                facet_scales = "free_y",
                                line_size = 0.8,
                                show_points = FALSE,
                                show_se = FALSE,
                                save_plots = FALSE,
                                output_dir = "plots",
                                file_prefix = "plot",
                                width = 10,
                                height = 6) {

  # Match arguments
  plot_type <- match.arg(plot_type)
  smooth_method <- match.arg(smooth_method)

  # Convert single data frame to list if needed
  if (is.data.frame(data)) {
    if (!"group" %in% names(data)) {
      stop("Single data frame must contain a 'group' column. ",
           "Use organize_by_groups() or calculate_group_averages() to add group identifiers.")
    }
    data_list <- split(data, data$group)
  } else if (is.list(data)) {
    data_list <- data
  } else {
    stop("data must be either a data frame or a list of data frames")
  }

  # Validate all elements are data frames
  if (!all(sapply(data_list, is.data.frame))) {
    stop("All elements in data must be data frames")
  }

  # Check for time variable
  has_time <- sapply(data_list, function(df) time_var %in% names(df))
  if (!all(has_time)) {
    stop("Time variable '", time_var, "' not found in all data frames")
  }

  # Combine all data
  combined_data <- dplyr::bind_rows(
    lapply(names(data_list), function(name) {
      df <- data_list[[name]]
      df$group <- name
      return(df)
    })
  )

  # Identify numeric columns (excluding time and identifiers)
  exclude_cols <- c(time_var, "group", "subject_id", "n")
  numeric_cols <- names(combined_data)[sapply(combined_data, is.numeric)]
  numeric_cols <- setdiff(numeric_cols, exclude_cols)

  if (length(numeric_cols) == 0) {
    stop("No numeric parameters found to plot")
  }

  # Filter parameters if specified
  if (!is.null(parameters)) {
    invalid_params <- setdiff(parameters, numeric_cols)
    if (length(invalid_params) > 0) {
      warning("Parameters not found: ", paste(invalid_params, collapse = ", "))
    }
    numeric_cols <- intersect(parameters, numeric_cols)
  }

  if (length(numeric_cols) == 0) {
    stop("No valid parameters to plot")
  }

  # Pivot to long format
  data_long <- combined_data %>%
    dplyr::select(dplyr::all_of(c(time_var, "group", numeric_cols))) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(numeric_cols),
      names_to = "parameter",
      values_to = "value"
    ) %>%
    dplyr::arrange(group, parameter, .data[[time_var]])

  # Apply smoothing if requested
  if (smooth_method == "rolling" && smooth_window > 1) {
    data_long <- data_long %>%
      dplyr::group_by(group, parameter) %>%
      dplyr::mutate(
        value = zoo::rollmean(value, k = smooth_window, fill = NA, align = "center")
      ) %>%
      dplyr::ungroup()
  }

  # Set up colors
  n_groups <- length(unique(data_long$group))

  if (color_palette %in% c("viridis", "plasma", "magma", "inferno", "cividis")) {
    color_scale <- ggplot2::scale_color_viridis_d(option = color_palette)
  } else {
    # Use RColorBrewer
    if (n_groups <= 3) {
      colors <- RColorBrewer::brewer.pal(3, color_palette)[1:n_groups]
    } else if (n_groups <= 9) {
      colors <- RColorBrewer::brewer.pal(n_groups, color_palette)
    } else {
      colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, color_palette))(n_groups)
    }
    color_scale <- ggplot2::scale_color_manual(values = colors)
  }

  # Create output directory if saving
  if (save_plots && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    message("Created output directory: ", output_dir)
  }

  # Generate plots based on type
  plot_list <- list()

  if (plot_type == "by_parameter") {
    # One plot per parameter, all groups on same plot
    for (param in unique(data_long$parameter)) {
      df_param <- dplyr::filter(data_long, parameter == param)

      p <- ggplot2::ggplot(df_param, ggplot2::aes(x = .data[[time_var]], y = value, color = group))

      if (smooth_method == "loess") {
        p <- p + ggplot2::geom_smooth(method = "loess", span = smooth_span,
                                      se = show_se, linewidth = line_size)
      } else {
        p <- p + ggplot2::geom_line(linewidth = line_size)
      }

      if (show_points) {
        p <- p + ggplot2::geom_point(size = 1.5, alpha = 0.6)
      }

      p <- p +
        color_scale +
        ggplot2::labs(
          title = paste("Parameter:", param),
          x = "Time",
          y = param,
          color = "Group"
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          legend.position = "right"
        )

      plot_list[[param]] <- p

      if (save_plots) {
        filename <- file.path(output_dir, paste0(file_prefix, "_", param, ".png"))
        ggplot2::ggsave(filename, plot = p, width = width, height = height)
      }
    }

  } else if (plot_type == "by_group") {
    # One plot per group, all parameters as facets
    for (grp in unique(data_long$group)) {
      df_group <- dplyr::filter(data_long, group == grp)

      p <- ggplot2::ggplot(df_group, ggplot2::aes(x = .data[[time_var]], y = value))

      if (smooth_method == "loess") {
        p <- p + ggplot2::geom_smooth(method = "loess", span = smooth_span,
                                      se = show_se, linewidth = line_size,
                                      color = colors[1])
      } else {
        p <- p + ggplot2::geom_line(linewidth = line_size, color = colors[1])
      }

      if (show_points) {
        p <- p + ggplot2::geom_point(size = 1.5, alpha = 0.6, color = colors[1])
      }

      p <- p +
        ggplot2::facet_wrap(~ parameter, scales = facet_scales) +
        ggplot2::labs(
          title = paste("Group:", grp),
          x = "Time",
          y = "Value"
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
        )

      plot_list[[grp]] <- p

      if (save_plots) {
        filename <- file.path(output_dir, paste0(file_prefix, "_group_", grp, ".png"))
        ggplot2::ggsave(filename, plot = p, width = width, height = height)
      }
    }

  } else if (plot_type == "combined") {
    # Single combined plot with facets
    p <- ggplot2::ggplot(data_long, ggplot2::aes(x = .data[[time_var]], y = value, color = group))

    if (smooth_method == "loess") {
      p <- p + ggplot2::geom_smooth(method = "loess", span = smooth_span,
                                    se = show_se, linewidth = line_size)
    } else {
      p <- p + ggplot2::geom_line(linewidth = line_size)
    }

    if (show_points) {
      p <- p + ggplot2::geom_point(size = 1.5, alpha = 0.6)
    }

    p <- p +
      ggplot2::facet_wrap(~ parameter, scales = facet_scales) +
      color_scale +
      ggplot2::labs(
        title = "All Parameters by Group",
        x = "Time",
        y = "Value",
        color = "Group"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )

    plot_list[["combined"]] <- p

    if (save_plots) {
      filename <- file.path(output_dir, paste0(file_prefix, "_combined.png"))
      ggplot2::ggsave(filename, plot = p, width = width, height = height * 1.2)
    }
  }

  if (save_plots) {
    message("Saved ", length(plot_list), " plot(s) to: ", output_dir)
  }

  return(plot_list)
}
