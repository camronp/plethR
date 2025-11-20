#' Create AUC Heatmap
#'
#' Generates a heatmap of AUC values (raw or normalized) across parameters and groups
#' with hierarchical clustering and customizable aesthetics.
#'
#' @param auc_results Data frame from `calculate_auc()` containing AUC values.
#' @param value_type Character string specifying which values to plot:
#'   - "normalized": Plot normalized AUC values (default, requires normalization)
#'   - "auc": Plot raw AUC values
#' @param exclude_groups Character vector of group names to exclude. Default is `NULL`.
#' @param exclude_parameters Character vector of parameter names to exclude. Default is `NULL`.
#' @param include_parameters Character vector of parameter names to include (overrides exclude).
#'   Default is `NULL` (includes all).
#' @param cluster_rows Logical; if `TRUE`, clusters parameters by similarity. Default is `TRUE`.
#' @param cluster_cols Logical; if `TRUE`, clusters groups by similarity. Default is `TRUE`.
#' @param clustering_method Character string specifying clustering method:
#'   "complete", "average", "single", "ward.D", "ward.D2". Default is "complete".
#' @param clustering_distance Character string specifying distance metric:
#'   "euclidean", "correlation", "manhattan". Default is "euclidean".
#' @param color_scheme Character string specifying color palette:
#'   - "RdYlBu": Red-Yellow-Blue diverging (default)
#'   - "RdBu": Red-Blue diverging
#'   - "viridis", "plasma", "magma": Sequential
#'   - "PRGn", "BrBG": Colorblind-friendly diverging
#' @param scale_data Character string specifying scaling:
#'   - "none": No scaling (default for normalized data)
#'   - "row": Scale each parameter (row) to mean=0, sd=1
#'   - "column": Scale each group (column) to mean=0, sd=1
#' @param show_values Logical; if `TRUE`, displays values in cells. Default is `FALSE`.
#' @param value_format Character string for value formatting. Default is "%.2f".
#' @param cell_width Numeric specifying cell width in points. Default is 40.
#' @param cell_height Numeric specifying cell height in points. Default is 40.
#' @param font_size Numeric specifying base font size. Default is 10.
#' @param main_title Character string for plot title. Default is auto-generated.
#' @param save_plot Logical; if `TRUE`, saves plot to file. Default is `FALSE`.
#' @param output_dir Character string specifying output directory. Default is "figures/heatmaps".
#' @param file_prefix Character string to prepend to filename. Default is "auc_heatmap".
#' @param width Numeric specifying plot width in inches. Default is 8.
#' @param height Numeric specifying plot height in inches. Default is 6.
#' @param dpi Numeric specifying resolution for saved plots. Default is 300.
#'
#' @return A pheatmap object. The plot is displayed and optionally saved.
#'
#' @details
#' Creates a publication-quality heatmap with:
#' - Hierarchical clustering of parameters and/or groups
#' - Multiple color schemes including colorblind-friendly options
#' - Optional value display in cells
#' - Flexible filtering of groups and parameters
#' - High-resolution output
#'
#' For normalized data, values represent fold-change relative to reference group.
#' Values > 1 indicate increase, < 1 indicate decrease.
#'
#' @examples
#' \dontrun{
#' # Basic normalized heatmap
#' plot_auc_heatmap(auc_results)
#'
#' # Heatmap with specific parameters
#' plot_auc_heatmap(auc_results,
#'                  include_parameters = c("f", "TVb", "MVb", "Penh"))
#'
#' # Exclude certain groups and parameters
#' plot_auc_heatmap(auc_results,
#'                  exclude_groups = "Control",
#'                  exclude_parameters = c("Comp", "EEP"))
#'
#' # Raw AUC values with row scaling
#' plot_auc_heatmap(auc_results,
#'                  value_type = "auc",
#'                  scale_data = "row",
#'                  show_values = TRUE)
#'
#' # Colorblind-friendly with custom clustering
#' plot_auc_heatmap(auc_results,
#'                  color_scheme = "PRGn",
#'                  clustering_method = "ward.D2",
#'                  save_plot = TRUE)
#' }
#'
#' @importFrom pheatmap pheatmap
#' @importFrom dplyr filter select
#' @importFrom tidyr pivot_wider
#' @importFrom grDevices colorRampPalette png dev.off
#' @importFrom RColorBrewer brewer.pal
#' @export
plot_auc_heatmap <- function(auc_results,
                             value_type = c("normalized", "auc"),
                             exclude_groups = NULL,
                             exclude_parameters = NULL,
                             include_parameters = NULL,
                             cluster_rows = TRUE,
                             cluster_cols = TRUE,
                             clustering_method = "complete",
                             clustering_distance = "euclidean",
                             color_scheme = "RdYlBu",
                             scale_data = "none",
                             show_values = FALSE,
                             value_format = "%.2f",
                             cell_width = 40,
                             cell_height = 40,
                             font_size = 10,
                             main_title = NULL,
                             save_plot = FALSE,
                             output_dir = "figures/heatmaps",
                             file_prefix = "auc_heatmap",
                             width = 8,
                             height = 6,
                             dpi = 300) {

  # Match arguments
  value_type <- match.arg(value_type)

  # Validate inputs
  if (!is.data.frame(auc_results)) {
    stop("auc_results must be a data frame")
  }

  required_cols <- c("group", "parameter", "auc")
  if (!all(required_cols %in% names(auc_results))) {
    stop("auc_results must contain columns: ", paste(required_cols, collapse = ", "))
  }

  # Check for normalized data if requested
  if (value_type == "normalized" && !"auc_normalized" %in% names(auc_results)) {
    stop("Normalized AUC not found. Run calculate_auc() with normalize_to parameter.")
  }

  # Filter data
  data_filtered <- auc_results

  # Exclude groups
  if (!is.null(exclude_groups)) {
    data_filtered <- data_filtered %>%
      dplyr::filter(!group %in% exclude_groups)
  }

  # Handle parameters
  if (!is.null(include_parameters)) {
    # Include only specified parameters
    data_filtered <- data_filtered %>%
      dplyr::filter(parameter %in% include_parameters)
  } else if (!is.null(exclude_parameters)) {
    # Exclude specified parameters
    data_filtered <- data_filtered %>%
      dplyr::filter(!parameter %in% exclude_parameters)
  }

  # Check if data remains
  if (nrow(data_filtered) == 0) {
    stop("No data remaining after filtering")
  }

  # Select value column
  if (value_type == "normalized") {
    data_filtered$value <- data_filtered$auc_normalized
    default_title <- "Normalized AUC by Group and Parameter"
    legend_title <- "Fold Change"
  } else {
    data_filtered$value <- data_filtered$auc
    default_title <- "AUC by Group and Parameter"
    legend_title <- "AUC"
  }

  # Use default title if not specified
  if (is.null(main_title)) {
    main_title <- default_title
  }

  # Reshape to matrix format
  data_wide <- data_filtered %>%
    dplyr::select(parameter, group, value) %>%
    tidyr::pivot_wider(names_from = group, values_from = value)

  # Convert to matrix
  heatmap_matrix <- as.matrix(data_wide[, -1])
  rownames(heatmap_matrix) <- data_wide$parameter

  # Check for missing values
  if (any(is.na(heatmap_matrix))) {
    warning("Matrix contains NA values which may affect clustering")
  }

  # Set up color palette
  if (color_scheme %in% c("viridis", "plasma", "magma", "inferno", "cividis")) {
    n_colors <- 100
    color_func <- switch(color_scheme,
                         viridis = viridisLite::viridis,
                         plasma = viridisLite::plasma,
                         magma = viridisLite::magma,
                         inferno = viridisLite::inferno,
                         cividis = viridisLite::cividis)
    colors <- color_func(n_colors)
  } else {
    # Use RColorBrewer palettes
    if (color_scheme %in% c("RdYlBu", "RdBu", "PRGn", "BrBG", "PiYG", "RdGy")) {
      colors <- grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(11, color_scheme)))(100)
    } else {
      # Default sequential
      colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrRd"))(100)
    }
  }

  # Prepare display numbers
  display_numbers <- if (show_values) {
    matrix(sprintf(value_format, heatmap_matrix), nrow = nrow(heatmap_matrix))
  } else {
    FALSE
  }

  # Create output directory if saving
  if (save_plot && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    message("Created output directory: ", output_dir)
  }

  # Generate filename
  if (save_plot) {
    filename <- file.path(output_dir,
                          paste0(file_prefix, "_",
                                 gsub(" ", "_", tolower(main_title)),
                                 ".png"))
  }

  # Create heatmap
  if (save_plot) {
    # Save to file with high resolution
    grDevices::png(filename,
                   width = width,
                   height = height,
                   units = "in",
                   res = dpi)
  }

  heatmap_plot <- pheatmap::pheatmap(
    heatmap_matrix,
    color = colors,
    cluster_rows = cluster_rows,
    cluster_cols = cluster_cols,
    clustering_method = clustering_method,
    clustering_distance_rows = clustering_distance,
    clustering_distance_cols = clustering_distance,
    scale = scale_data,
    main = main_title,
    fontsize = font_size,
    fontsize_row = font_size,
    fontsize_col = font_size,
    cellwidth = cell_width,
    cellheight = cell_height,
    display_numbers = display_numbers,
    number_format = value_format,
    fontsize_number = font_size * 0.8,
    legend = TRUE,
    legend_breaks = if (value_type == "normalized") c(0.5, 0.75, 1, 1.25, 1.5) else NA,
    legend_labels = if (value_type == "normalized") c("0.5", "0.75", "1.0", "1.25", "1.5") else NA,
    border_color = "grey60",
    na_col = "grey90"
  )

  if (save_plot) {
    grDevices::dev.off()
    message("Heatmap saved to: ", filename)
  }

  return(invisible(heatmap_plot))
}
