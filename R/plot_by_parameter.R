#' Plot Time Series Data by Parameter
#'
#' This function generates line plots of parameters over time for different groups. It supports the option to apply a rolling
#' average to smooth the data and can save the resulting plots to a specified folder.
#'
#' @param groups_merged_named A list of data frames, where each element represents a group and contains a `Time` column and
#' numeric parameters to be plotted.
#' @param rolling_average An optional integer specifying the window size for the rolling average. If `NULL`, no rolling average
#' is applied. Default is `NULL`.
#' @param save_plots A logical indicating whether to save the generated plots as PNG files. If `TRUE`, plots will be saved to
#' a folder named after the base name of the input file. Default is `FALSE`.
#'
#' @details
#' This function takes the provided list of data frames (`groups_merged_named`) from merge_group_avg() and generates line plots for each unique parameter
#' in the data, grouped by time. The plots can be customized to show a rolling average of the values for each parameter. If
#' `save_plots` is `TRUE`, the function will save each plot as a PNG file in a folder named after the base name of the provided
#' `excel_file` (if applicable).
#'
#' @return A list of ggplot objects, each corresponding to a plot for one parameter.
#' @importFrom RColorBrewer brewer.pal
#' @importFrom purrr map map2_dfr
#' @importFrom zoo rollapply
#'
#' @examples
#' # Assuming `groups_merged_named` is a list of data frames, each containing a Time column and parameters
#' plot_list <- plot_by_parameter(groups_merged_named, rolling_average = 2, save_plots = FALSE)
#'
#' # If you want to save the plots to files
#' plot_list <- plot_by_parameter(groups_merged_named, rolling_average = 5, save_plots = TRUE)
#'
#' @export




plot_by_parameter <- function(groups_merged_named, rolling_average = NULL, save_plots = FALSE) {

  # Create a folder to save plots, if necessary
  base_name <- tools::file_path_sans_ext(basename(excel_file))
  folder_name <- paste0(base_name, "_parameter_line_plots")

  # Create the folder if it doesn't exist
  if (save_plots && !dir.exists(folder_name)) {
    dir.create(folder_name)
  }

  # Combine data into a single data frame
  combined_long <- map2_dfr(groups_merged_named, names(groups_merged_named), function(df, group_name) {
    df_long <- df %>%
      pivot_longer(cols = -Time, names_to = "parameter", values_to = "value") %>%
      mutate(group = group_name)

    # Apply rolling average if specified
    if (!is.null(rolling_average)) {
      df_long <- df_long %>%
        group_by(parameter, group) %>%
        mutate(value = rollapply(value, width = rolling_average, FUN = mean, fill = NA, align = "center")) %>%
        ungroup()
    }

    return(df_long)
  })

  # Get unique parameters
  parameters <- unique(combined_long$parameter)

  # Use a colorblind-friendly palette from RColorBrewer (Set1)
  colors <- brewer.pal(n = length(unique(combined_long$group)), name = "Set1")

  # Generate plots for each parameter
  plot_list <- map(parameters, function(param) {
    df_param <- combined_long %>% filter(parameter == param)

    plot <- ggplot(df_param, aes(x = Time, y = value, color = group)) +
      geom_line() +
      labs(title = paste("Parameter:", param),
           x = "Time",
           y = "Value") +
      scale_color_manual(values = colors) +  # Apply colorblind-friendly color palette
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Optional: Angle x-axis labels for better readability

    # Save plot if save_plots is TRUE
    if (save_plots) {
      file_path <- file.path(folder_name, paste0(param, ".png"))
      ggsave(filename = file_path, plot = plot, width = 8, height = 6)
    }

    return(plot)
  })

  return(plot_list)
}


#plot_by_parameter(groups_merged_named, rolling_average = 5, save_plots = FALSE)
