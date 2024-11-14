#' PCA Plot with User Defined K-means Clustering and Optional Saving
#'
#' This function performs Principal Component Analysis (PCA) on a list of data frames, applies k-means clustering,
#' and creates a 2D PCA plot of the first two components with clusters highlighted by ellipses.
#' It also allows for optional saving of the plot as a PNG file.
#'
#' @param df_list Input the object output from 'sheets_into_list()' step. Each data frame should contain numeric columns only.
#' @param num_clusters Integer specifying the number of clusters for k-means clustering. Default is 4.
#' @param text_size Numeric value for the text size of labels on the plot. Default is 5.
#' @param save_plots Logical. If `TRUE`, saves the plot as "pca_plot.png" in the current directory. Default is `FALSE`.
#'
#' @return A ggplot object showing the PCA plot with clusters and ellipses.
#'
#' @examples
#' # Assuming object is a list of numeric data frames
#' wbp_pca_plot(df_list_summarized, num_clusters = 3, text_size = 4, save_plots = TRUE)
#'
#' @export

wbp_pca_plot <- function(df_list, num_clusters = 4, text_size = 5, save_plots = FALSE) {

  combined_df <- bind_rows(lapply(names(df_list), function(name) {
    df <- df_list[[name]]
    df$group <- name
    df
  }))

  combined_df <- combined_df[, sapply(combined_df, is.numeric)]
  standardized_df <- scale(combined_df)

  pca_result <- prcomp(standardized_df, center = TRUE, scale. = TRUE)

  pca_4_data <- pca_result$x[, 1:4]

  plot_df <- data.frame(ID = 1:length(df_list))
  plot_df[, paste0("PC", 1:4)] <- pca_4_data

  kmeans_result <- kmeans(plot_df[, c("PC1", "PC2")], centers = num_clusters)
  plot_df$Cluster <- as.factor(kmeans_result$cluster)

  label_df <- data.frame(PC1 = plot_df$PC1, PC2 = plot_df$PC2, Label = plot_df$ID, Cluster = plot_df$Cluster)

  df_summary_names <- names(df_list)
  label_df$Label <- set_group_names(df_summary_names[label_df$Label])
  label_df$Label <- gsub(".WBPth", "", label_df$Label)

  cluster_colors <- brewer.pal(num_clusters, "Set2")

  pca_plot_ellipse <- ggplot(plot_df, aes(x = PC1, y = PC2, color = Cluster)) +
    geom_point(size = 3) +
    geom_text_repel(data = label_df, aes(x = PC1, y = PC2, label = Label),
                    size = text_size, hjust = 0.5, vjust = -0.5, color = "black") +
    geom_mark_ellipse(aes(fill = as.factor(Cluster))) +
    labs(title = "K-means Clustering Based on First 2 Principal Components",
         x = "Principal Component 1", y = "Principal Component 2") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 20)) +
    scale_color_manual(values = cluster_colors) +
    scale_fill_manual(values = cluster_colors) +
    guides(color = FALSE, fill = guide_legend(title = "Cluster"))

  # Step 2: Save plot if save_plots is TRUE
  if (save_plots) {
    # Construct the file path in the current working directory
    file_path <- "pca_plot.png"
    # Save the plot
    ggsave(filename = file_path, plot = pca_plot_ellipse, width = 8, height = 6)
    message(paste("Plot saved to", file_path))
  }

  return(pca_plot_ellipse)
}

# Example usage:
#wbp_pca_plot(cp_df_list_summarized, num_clusters = 4, text_size = 4, save_plots = TRUE)

