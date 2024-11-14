#' Plot Time Series Data by Group
#'
#' This function generates line plots for each group over time, with the option to apply a rolling average to smooth the data.
#' The function also supports saving the resulting plots to a specified folder.
#'
#' @param groups_merged_named A list of data frames, where each element represents a group, containing a `Time` column and
#' numeric variables that will be plotted.
#' @param rolling_average An optional integer specifying the window size for the rolling average. If `NULL`, no rolling average
#' is applied. Default is `NULL`.
#' @param save_plots A logical indicating whether to save the generated plots as PNG files. If `TRUE`, plots will be saved to
#' a folder named after the base name of the input file. Default is `FALSE`.
#'
#' @details
#' This function takes the provided list of data frames (`groups_merged_named`) from merge_group_avg(), where each data frame corresponds to a group,
#' and generates line plots for each unique variable in the data, grouped by time. The plots are faceted by variable, and the
#' function supports the application of a rolling average for smoothing. If `save_plots` is `TRUE`, the plots are saved to a
#' folder named based on the input file, without the `.xlsx` extension.
#'
#' @return A list of ggplot objects, each corresponding to a plot for one group.
#' @import ggplot2
#' @import dplyr
#' @importFrom purrr map2
#' @importFrom zoo rollapply
#'
#' @examples
#' # Assuming `groups_merged_named` is a list of data frames, each containing a Time column and variables
#' plot_list <- plot_by_group(groups_merged_named, rolling_average = 5, save_plots = FALSE)
#'
#' # If you want to save the plots to files
#' plot_list <- plot_by_group(groups_merged_named, rolling_average = 5, save_plots = TRUE)
#'
#' @export

plot_by_group <- function(groups_merged_named, rolling_average = NULL, save_plots = FALSE) {

  # Create a folder name based on excel_file without the .xlsx extension
  base_name <- tools::file_path_sans_ext(basename(excel_file))
  folder_name <- paste0(base_name, "_group_line_plots")

  # If save_plots is TRUE, create the folder if it doesn't exist
  if (save_plots && !dir.exists(folder_name)) {
    dir.create(folder_name)
  }

  # Generate plots for each group
  plot_list <- map2(groups_merged_named, names(groups_merged_named), function(df, group_name) {

    df_long <- df %>%
      pivot_longer(cols = -Time, names_to = "variable", values_to = "value")

    # Apply rolling average if specified
    if (!is.null(rolling_average)) {
      df_long <- df_long %>%
        group_by(variable) %>%
        mutate(value = rollapply(value, width = rolling_average, FUN = mean, fill = NA, align = "center")) %>%
        ungroup()
    }

    df_long$group <- group_name

    plot <- ggplot(df_long, aes(x = Time, y = value, color = variable)) +
      geom_line() +
      facet_wrap(~ variable, scales = "free_y") +
      labs(title = paste("Group:", group_name),
           x = "Time",
           y = "Value") +
      theme_bw() +
      theme(legend.position = "none")

    # Save plot if save_plots is TRUE
    if (save_plots) {
      file_path <- file.path(folder_name, paste0("group_", group_name, ".png"))
      ggsave(filename = file_path, plot = plot, width = 8, height = 6)
    }

    return(plot)
  })

  return(plot_list)
}

#plots <- plot_by_group(groups_merged_named)
#plot_by_group(groups_merged_named, rolling_average = 2, save_plots = FALSE)

