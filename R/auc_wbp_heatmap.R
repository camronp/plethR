#' Generate and Save a Heatmap of Normalized AUC Values by Group and Parameter
#'
#' This function generates a heatmap of normalized Area Under the Curve (AUC) values for each parameter, grouped by experimental conditions (e.g., different groups). The function allows for optional removal of specific groups and parameters, and it can save the generated heatmap as a PNG file.
#'
#' @param auc_results_normalized A data frame containing the normalized AUC results. The data frame should include the columns `Group`, `Parameter`, and `AUC_normalized`.
#' @param group_to_remove A character string specifying a group to be removed from the heatmap data. Default is `NULL`. If specified, this group will be excluded from the heatmap.
#' @param remove_parameters A vector of character strings specifying the parameters to be removed from the heatmap data. Default is `NULL`. If specified, these parameters will be excluded from the heatmap.
#' @param main_title A character string specifying the title of the heatmap. Default is "Normalized AUC by Group and Parameter".
#' @param save_heatmap A logical value indicating whether to save the heatmap as a PNG file. Default is `FALSE`. If `TRUE`, the heatmap will be saved in the current working directory.
#'
#' @details
#' This function creates a heatmap where the rows represent different parameters and the columns represent different groups. The values in the heatmap are the normalized AUC values. The data is clustered both by rows and columns using hierarchical clustering, with Euclidean distance and complete linkage. The heatmap is generated using the `pheatmap` package.
#'
#' If `save_heatmap` is `TRUE`, the heatmap is saved as a PNG file with the title specified by `main_title`. The file will be saved in the current working directory with the title formatted as "main_title_heatmap.png".
#'
#' @return A heatmap plot displayed in the RStudio plot window. If `save_heatmap` is `TRUE`, the plot is saved as a PNG file.
#'
#' @import pheatmap
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#'
#' @examples
#' # Generate and display a heatmap without saving
#' auc_wbp_heatmap(auc_results_normalized)
#'
#' # Generate and display a heatmap, removing specific group and parameters, and saving the heatmap as a PNG file
#' auc_wbp_heatmap(auc_results_normalized, group_to_remove = "Uninfected Wildtype",
#'                 remove_parameters = c("Comp", "EEP"), main_title = "Test", save_heatmap = TRUE)
#'
#' @export

auc_wbp_heatmap <- function(auc_results_normalized,
                            group_to_remove = NULL,
                            remove_parameters = NULL,
                            main_title = "Normalized AUC by Group and Parameter",
                            save_heatmap = FALSE) {

  # If a group is specified to remove, exclude it from the data
  if (!is.null(group_to_remove)) {
    auc_results_normalized <- auc_results_normalized %>%
      filter(Group != group_to_remove)
  }

  # If parameters are specified to remove, exclude them from the data
  if (!is.null(remove_parameters)) {
    auc_results_normalized <- auc_results_normalized %>%
      filter(!Parameter %in% remove_parameters)
  }

  # Reshape the data to a wide format where each row is a parameter, and each column is a group
  auc_matrix <- auc_results_normalized %>%
    select(Group, Parameter, AUC_normalized) %>%
    spread(key = Group, value = AUC_normalized)

  # Convert the data frame to a matrix (ignoring the Parameter column)
  auc_matrix <- auc_matrix %>%
    column_to_rownames("Parameter") %>%
    as.matrix()

  # Generate the heatmap with the user-defined main title
  heatmap_plot <- pheatmap(auc_matrix,
                           clustering_distance_rows = "euclidean",
                           clustering_distance_cols = "euclidean",
                           clustering_method = "complete",
                           main = main_title,  # Use the user-defined main title
                           subtitle = "AUC values normalized to specified group",
                           xlab = "Group",
                           ylab = "Parameter",
                           angle = 0,
                           cellwidth = 100,
                           fontsize = 12,
                           family = "Calibri",
                           silent = FALSE)  # Show the plot in RStudio (do not store it in an object)

  # If save_heatmap is TRUE, save the plot
  if (save_heatmap) {
    # Construct file name based on the main title
    file_name <- paste0(gsub(" ", "_", main_title), "_heatmap.png")
    # Save the plot as a PNG file
    ggsave(file_name, plot = heatmap_plot$gtable, width = 10, height = 8)
    message(paste("Plot saved as", file_name))
  }
}

#

