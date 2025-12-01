#' Create PCA Plot
#'
#' Performs PCA on individual subjects, with each point representing one subject's
#' average respiratory parameters across time.
#'
#' @param data A list of data frames from `sheets_into_list()`, where each element
#'   is one subject/mouse. Each data frame should contain time series respiratory data.
#' @param group_mapping Named list from `assign_groups_interactive()` mapping groups
#'   to sheet indices. If `NULL`, each subject is its own group. Default is `NULL`.
#' @param parameters Character vector of parameters to include in PCA. If `NULL`, uses all
#'   numeric columns. Default is `NULL`.
#' @param use_clustering Logical; if `TRUE`, applies k-means clustering. Default is `FALSE`.
#' @param num_clusters Integer specifying number of k-means clusters. Default is 4.
#' @param color_by Character string specifying coloring.
#'   Options: "group" (color by experimental group, default) or "cluster" (color by k-means cluster).
#' @param show_ellipses Logical; if `TRUE`, shows confidence ellipses around groups. Default is `TRUE`.
#' @param ellipse_type Character string specifying ellipse type.
#'   Options: "norm", "t", or "euclid". Default is "norm".
#' @param ellipse_level Numeric confidence level (0-1). Default is 0.95.
#' @param show_labels Logical; if `TRUE`, displays subject labels. Default is `TRUE`.
#' @param label_size Numeric specifying label text size. Default is 3.
#' @param point_size Numeric specifying point size. Default is 3.
#' @param components Character vector of PCs to plot. Default is c("PC1", "PC2").
#' @param show_variance Logical; if `TRUE`, shows variance percentage in axis labels. Default is `TRUE`.
#' @param show_loadings Logical; if `TRUE`, adds loading vectors. Default is `FALSE`.
#' @param n_loadings Integer number of loading vectors to show. Default is 5.
#' @param color_palette Character string specifying color scheme.
#'   Options: "Set1" (default), "Set2", "Dark2", "viridis", etc.
#' @param random_seed Integer for k-means reproducibility. Default is 123.
#' @param save_plot Logical; if `TRUE`, saves plot to file. Default is `FALSE`.
#' @param output_dir Character string specifying output directory. Default is "figures/pca".
#' @param file_prefix Character string to prepend to filename. Default is "pca".
#' @param width Numeric specifying plot width in inches. Default is 8.
#' @param height Numeric specifying plot height in inches. Default is 6.
#' @param dpi Numeric specifying resolution for saved plots. Default is 300.
#'
#' @return A list containing: plot (ggplot object), pca (prcomp object), variance (data frame),
#'   clusters (k-means results if used, NULL otherwise), and scores (PC scores with metadata).
#'
#' @examples
#' \dontrun{
#' # Load data
#' df_list <- sheets_into_list("data.xlsx", clean_time = TRUE)
#'
#' # Define groups
#' groups <- set_group_names("Control", "Low", "Medium", "High")
#' mapping <- assign_groups_interactive(groups, df_list)
#'
#' # PCA colored by group (16 mice, 4 groups)
#' result <- plot_pca(df_list, group_mapping = mapping)
#' result$plot
#'
#' # Check variance explained
#' print(result$variance)
#'
#' # With clustering
#' result <- plot_pca(df_list,
#'                    group_mapping = mapping,
#'                    use_clustering = TRUE,
#'                    num_clusters = 4)
#'
#' # Show which parameters drive separation
#' result <- plot_pca(df_list,
#'                    group_mapping = mapping,
#'                    show_loadings = TRUE,
#'                    parameters = c("f", "TVb", "MVb", "Penh"))
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_point stat_ellipse labs theme_bw theme element_text scale_color_manual scale_color_viridis_d ggsave geom_segment geom_text arrow unit
#' @importFrom ggrepel geom_text_repel
#' @importFrom dplyr bind_rows group_by summarize across
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats prcomp kmeans
#' @export
plot_pca <- function(data,
                     group_mapping = NULL,
                     parameters = NULL,
                     use_clustering = FALSE,
                     num_clusters = 4,
                     color_by = c("group", "cluster"),
                     show_ellipses = TRUE,
                     ellipse_type = "norm",
                     ellipse_level = 0.95,
                     show_labels = TRUE,
                     label_size = 3,
                     point_size = 3,
                     components = c("PC1", "PC2"),
                     show_variance = TRUE,
                     show_loadings = FALSE,
                     n_loadings = 5,
                     color_palette = "Set1",
                     random_seed = 123,
                     save_plot = FALSE,
                     output_dir = "figures/pca",
                     file_prefix = "pca",
                     width = 8,
                     height = 6,
                     dpi = 300) {

  # Match arguments
  color_by <- match.arg(color_by)

  # Validate components
  if (length(components) != 2) {
    stop("components must be a vector of length 2 (e.g., c('PC1', 'PC2'))")
  }

  # Validate input is a list
  if (!is.list(data) || is.data.frame(data)) {
    stop("data must be a list of data frames (output from sheets_into_list)")
  }

  if (length(data) == 0) {
    stop("data list is empty")
  }

  message("Processing ", length(data), " subjects...")

  # Process each subject: average across time to get one row per subject
  subject_data_list <- list()

  for (i in seq_along(data)) {
    df <- data[[i]]
    subject_name <- names(data)[i]
    if (is.null(subject_name)) subject_name <- paste0("Subject_", i)

    # Identify numeric columns (exclude metadata)
    metadata_cols <- c("Time", "time", "Tbody", "Subject", "Phase", "Rinx",
                       "RH", "Tc", "Recording", "Alarms", "BFCF", "group",
                       "subject_id", "sample_id", "n")
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    numeric_cols <- setdiff(numeric_cols, metadata_cols)

    if (length(numeric_cols) == 0) {
      warning("No numeric columns in subject ", subject_name, ". Skipping.")
      next
    }

    # Calculate mean across all timepoints for this subject
    subject_means <- colMeans(df[, numeric_cols], na.rm = TRUE)

    # Create one-row data frame
    subject_row <- as.data.frame(t(subject_means))
    subject_row$subject_id <- subject_name

    subject_data_list[[i]] <- subject_row
  }

  # Combine all subjects
  pca_input <- dplyr::bind_rows(subject_data_list)

  # Assign groups based on group_mapping
  if (!is.null(group_mapping)) {
    # Convert string indices to numeric if needed
    group_mapping_clean <- lapply(group_mapping, function(indices) {
      if (is.character(indices)) {
        as.numeric(gsub("\\.WBPth$", "", indices))
      } else {
        indices
      }
    })

    # Create group assignment
    pca_input$group <- NA_character_
    for (group_name in names(group_mapping_clean)) {
      indices <- group_mapping_clean[[group_name]]
      for (idx in indices) {
        if (idx <= nrow(pca_input)) {
          pca_input$group[idx] <- group_name
        }
      }
    }

    # Check for unassigned subjects
    if (any(is.na(pca_input$group))) {
      warning("Some subjects not assigned to groups")
      pca_input$group[is.na(pca_input$group)] <- "Unassigned"
    }
  } else {
    # No group mapping - each subject is its own group
    pca_input$group <- pca_input$subject_id
  }

  message("Prepared ", nrow(pca_input), " subjects for PCA")

  # Get numeric columns for PCA
  metadata_cols <- c("subject_id", "group")
  all_cols <- setdiff(names(pca_input), metadata_cols)

  # Filter to specified parameters if provided
  if (!is.null(parameters)) {
    all_cols <- intersect(parameters, all_cols)
    if (length(all_cols) == 0) {
      stop("None of the specified parameters found in data")
    }
  }

  message("Using ", length(all_cols), " parameters: ",
          paste(head(all_cols, 5), collapse = ", "),
          if (length(all_cols) > 5) "..." else "")

  # Prepare PCA data
  pca_data <- pca_input[, all_cols]

  # Check for complete cases
  complete_cases <- complete.cases(pca_data)
  if (sum(!complete_cases) > 0) {
    warning("Removed ", sum(!complete_cases), " subjects with missing values")
    pca_data <- pca_data[complete_cases, ]
    pca_input <- pca_input[complete_cases, ]
  }

  if (nrow(pca_data) < 3) {
    stop("Need at least 3 subjects for PCA")
  }

  # Perform PCA
  pca_result <- stats::prcomp(pca_data, center = TRUE, scale. = TRUE)

  # Extract PC scores
  pca_scores <- as.data.frame(pca_result$x)

  # Calculate variance explained
  variance_explained <- summary(pca_result)$importance[2, ] * 100
  variance_df <- data.frame(
    PC = paste0("PC", seq_along(variance_explained)),
    Variance = variance_explained,
    Cumulative = cumsum(variance_explained)
  )

  message("PC1 explains ", round(variance_explained[1], 1), "% of variance")
  message("PC2 explains ", round(variance_explained[2], 1), "% of variance")

  # Add metadata
  pca_scores$subject_id <- pca_input$subject_id
  pca_scores$group <- pca_input$group

  # Create clean labels
  pca_scores$label <- gsub("\\.WBPth$", "", pca_scores$subject_id)

  # Apply k-means clustering if requested
  cluster_result <- NULL
  if (use_clustering) {
    set.seed(random_seed)
    pc_indices <- which(colnames(pca_scores) %in% components)
    cluster_data <- pca_scores[, pc_indices]
    cluster_result <- stats::kmeans(cluster_data, centers = num_clusters, nstart = 25)
    pca_scores$cluster <- as.factor(cluster_result$cluster)
    message("K-means clustering with ", num_clusters, " clusters applied")
  }

  # Set up colors
  if (color_by == "group") {
    n_colors <- length(unique(pca_scores$group))
    color_var <- "group"
  } else {
    if (!use_clustering) {
      stop("color_by = 'cluster' requires use_clustering = TRUE")
    }
    n_colors <- num_clusters
    color_var <- "cluster"
  }

  # Create color scale
  if (color_palette %in% c("viridis", "plasma", "magma", "inferno", "cividis")) {
    color_scale <- ggplot2::scale_color_viridis_d(option = color_palette)
    fill_scale <- ggplot2::scale_fill_viridis_d(option = color_palette)
  } else {
    if (n_colors <= 3) {
      colors <- RColorBrewer::brewer.pal(3, color_palette)[1:n_colors]
    } else if (n_colors <= 9) {
      colors <- RColorBrewer::brewer.pal(n_colors, color_palette)
    } else {
      colors <- grDevices::colorRampPalette(
        RColorBrewer::brewer.pal(9, color_palette))(n_colors)
    }
    color_scale <- ggplot2::scale_color_manual(values = colors)
    fill_scale <- ggplot2::scale_fill_manual(values = colors)
  }

  # Extract component indices
  pc1_idx <- as.numeric(gsub("PC", "", components[1]))
  pc2_idx <- as.numeric(gsub("PC", "", components[2]))

  # Create axis labels
  if (show_variance) {
    x_label <- sprintf("%s (%.1f%%)", components[1], variance_explained[pc1_idx])
    y_label <- sprintf("%s (%.1f%%)", components[2], variance_explained[pc2_idx])
  } else {
    x_label <- components[1]
    y_label <- components[2]
  }

  # Create base plot
  p <- ggplot2::ggplot(pca_scores, ggplot2::aes(x = .data[[components[1]]],
                                                y = .data[[components[2]]],
                                                color = .data[[color_var]],
                                                fill = .data[[color_var]])) +
    ggplot2::geom_point(size = point_size, alpha = 0.8) +
    color_scale +
    fill_scale +
    ggplot2::labs(
      title = "Principal Component Analysis",
      x = x_label,
      y = y_label,
      color = if (color_by == "group") "Group" else "Cluster",
      fill = if (color_by == "group") "Group" else "Cluster"
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 14),
      legend.position = "right",
      panel.grid.minor = ggplot2::element_blank()
    )

  # Add confidence ellipses
  if (show_ellipses) {
    group_counts <- table(pca_scores[[color_var]])

    # Only warn but still try to draw ellipses
    if (any(group_counts < 3)) {
      warning("Some groups have < 3 subjects. Ellipses may be unreliable.")
    }

    # Try to draw ellipses for groups with at least 2 points
    tryCatch({
      p <- p + ggplot2::stat_ellipse(
        ggplot2::aes(color = .data[[color_var]], fill = .data[[color_var]]),
        type = ellipse_type,
        level = ellipse_level,
        geom = "polygon",
        alpha = 0.15,
        linewidth = 1
      )
    }, error = function(e) {
      warning("Could not draw ellipses: ", e$message)
    })
  }

  # Add labels
  if (show_labels) {
    p <- p + ggrepel::geom_text_repel(
      ggplot2::aes(label = label),
      size = label_size,
      color = "black",
      max.overlaps = 20,
      box.padding = 0.5,
      point.padding = 0.3,
      segment.color = "grey50",
      segment.size = 0.3
    )
  }

  # Add loading vectors if requested
  if (show_loadings) {
    loadings <- pca_result$rotation[, c(pc1_idx, pc2_idx)]
    contributions <- rowSums(loadings^2)
    top_vars <- names(sort(contributions, decreasing = TRUE)[1:min(n_loadings, length(contributions))])

    load_scale <- min(diff(range(pca_scores[, components[1]])),
                      diff(range(pca_scores[, components[2]]))) * 0.3

    loading_df <- data.frame(
      x_start = 0,
      y_start = 0,
      x_end = loadings[top_vars, 1] * load_scale,
      y_end = loadings[top_vars, 2] * load_scale,
      variable = top_vars
    )

    p <- p +
      ggplot2::geom_segment(
        data = loading_df,
        ggplot2::aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
        arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),
        color = "darkred",
        linewidth = 0.7,
        alpha = 0.7,
        inherit.aes = FALSE
      ) +
      ggplot2::geom_text(
        data = loading_df,
        ggplot2::aes(x = x_end * 1.15, y = y_end * 1.15, label = variable),
        color = "darkred",
        size = 3,
        fontface = "bold",
        inherit.aes = FALSE
      )
  }

  # Save plot if requested
  if (save_plot) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      message("Created output directory: ", output_dir)
    }

    filename <- file.path(output_dir,
                          paste0(file_prefix, "_",
                                 paste(components, collapse = "_"),
                                 ".png"))
    ggplot2::ggsave(filename, plot = p, width = width, height = height, dpi = dpi)
    message("PCA plot saved to: ", filename)
  }

  # Return results
  return(list(
    plot = p,
    pca = pca_result,
    variance = variance_df,
    clusters = if (use_clustering) cluster_result else NULL,
    scores = pca_scores
  ))
}
