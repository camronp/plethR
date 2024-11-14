#' Generate and Save Bar Plots of AUC by Parameter
#'
#' This function generates bar plots of Area Under the Curve (AUC) values for each parameter in the provided AUC results,
#' with optional error bars and normalization. The plots can be saved to disk if specified.
#'
#' @param auc_results A data frame containing the AUC results, including columns for `Group`, `Parameter`, `AUC`, and `AUC_sd`.
#' @param save_plots A logical value indicating whether to save the plots as PNG files. Default is `FALSE`.
#'        If `TRUE`, the plots will be saved in a folder named "AUC_Results_parameter_bar_plots".
#' @param y_scale A logical value indicating whether to automatically scale the y-axis based on AUC values and standard deviations.
#'        Default is `FALSE`. If `TRUE`, the y-axis will be scaled to accommodate the full range of AUC values and their standard deviations.
#'
#' @details
#' This function generates a separate bar plot for each unique parameter in the `auc_results` data frame. Each plot represents
#' the AUC values for different groups, with error bars representing the standard deviation of the AUC. The color scheme used
#' in the plots is colorblind-friendly, derived from the `RColorBrewer` package.
#' The plots are generated using `ggplot2`. If `save_plots` is set to `TRUE`, the plots are saved as PNG files in a directory
#' named "AUC_Results_parameter_bar_plots".
#'
#' @return A list of `ggplot` objects, each corresponding to a plot of AUC for a parameter.
#'
#' @import ggplot2
#' @import dplyr
#' @import RColorBrewer
#'
#' @examples
#' # Assuming `auc_results_normalized` is a data frame with AUC results
#' # Generate and display plots without saving
#' plot_list <- plot_auc_by_parameter(auc_results_normalized)
#'
#' # Generate, scale, and save plots
#' plot_list_saved <- plot_auc_by_parameter(auc_results_normalized, save_plots = TRUE, y_scale = TRUE)
#'
#' @export

plot_auc_by_parameter <- function(auc_results, save_plots = FALSE, y_scale = FALSE) {

  # Create a folder based on the AUC results file name for saving plots
  base_name <- "AUC_Results"
  folder_name <- paste0(base_name, "_parameter_bar_plots")

  # If save_plots is TRUE, create the folder if it doesn't exist
  if (save_plots && !dir.exists(folder_name)) {
    dir.create(folder_name)
  }

  # Extract unique parameters
  parameters <- unique(auc_results$Parameter)

  # Use a colorblind-friendly palette from RColorBrewer
  colors <- brewer.pal(n = length(unique(auc_results$Group)), name = "Set1")

  # Generate a plot for each parameter
  plot_list <- lapply(parameters, function(param) {
    # Filter data for the specific parameter
    df_param <- auc_results %>% filter(Parameter == param)

    # If scaling is requested, calculate the y-axis limits
    if (y_scale) {
      y_min <- min(df_param$AUC - df_param$AUC_sd, na.rm = TRUE)
      y_max <- max(df_param$AUC + df_param$AUC_sd, na.rm = TRUE)
    } else {
      y_min <- 0
      y_max <- max(df_param$AUC + df_param$AUC_sd, na.rm = TRUE) # Default scaling
    }

    # Create the bar plot with error bars
    plot <- ggplot(df_param, aes(x = Group, y = AUC, fill = Group)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(aes(ymin = AUC - AUC_sd, ymax = AUC + AUC_sd), width = 0.2) +
      labs(title = paste("AUC for Parameter:", param),
           x = "Group",
           y = "AUC") +
      scale_fill_manual(values = colors) +  # Apply colorblind-friendly colors
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      coord_cartesian(ylim = c(y_min, y_max)) # Use coord_cartesian to adjust y-axis range

    # Save plot if save_plots is TRUE
    if (save_plots) {
      file_path <- file.path(folder_name, paste0(param, "_AUC_plot.png"))
      ggsave(filename = file_path, plot = plot, width = 8, height = 6)
    }

    return(plot)
  })

  return(plot_list)
}



#plot_auc_by_parameter(auc_results_normalized, save_plots = FALSE, y_scale = TRUE)
