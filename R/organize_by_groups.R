#' Organize Data Frames by Group Assignment
#'
#' Organizes a list of data frames into groups based on sheet-to-group mappings,
#' with options to add identifiers and combine data within or across groups.
#'
#' @param df_list A list of data frames (typically from `sheets_into_list()`).
#' @param group_mapping A named list where each element contains sheet indices
#'   for that group (from `assign_groups_interactive()`).
#' @param add_subject_id Logical; if `TRUE`, adds a 'subject_id' column identifying
#'   individual subjects within each group. Default is `FALSE`.
#' @param combine_groups Logical; if `TRUE`, combines all data frames within each
#'   group into a single data frame per group. Default is `FALSE`.
#' @param combine_all Logical; if `TRUE`, combines all groups into a single data frame.
#'   Requires `combine_groups = TRUE`. Default is `FALSE`.
#'
#' @return Depending on parameters:
#'   - If `combine_all = TRUE`: A single combined data frame
#'   - If `combine_groups = TRUE`: A list with one data frame per group
#'   - Otherwise: A nested list organized by group, then by subject
#'
#' @examples
#' \dontrun{
#' # Load data
#' df_list <- sheets_into_list("data.xlsx")
#'
#' # Define groups
#' groups <- set_group_names("Control", "Treated")
#' mapping <- assign_groups_interactive(groups, df_list = df_list)
#'
#' # Organize without combining (nested list)
#' organized <- organize_by_groups(df_list, mapping)
#'
#' # Organize and combine within groups
#' combined <- organize_by_groups(df_list, mapping,
#'                                combine_groups = TRUE,
#'                                add_subject_id = TRUE)
#'
#' # Access data: combined$Control, combined$Treated
#'
#' # Create single data frame for analysis
#' all_data <- organize_by_groups(df_list, mapping,
#'                                combine_groups = TRUE,
#'                                combine_all = TRUE,
#'                                add_subject_id = TRUE)
#' }
#'
#' @importFrom dplyr bind_rows
#' @export
organize_by_groups <- function(df_list,
                               group_mapping,
                               add_subject_id = FALSE,
                               combine_groups = FALSE,
                               combine_all = FALSE) {

  # Validate inputs
  if (!is.list(df_list)) {
    stop("df_list must be a list of data frames")
  }

  if (!is.list(group_mapping) || is.null(names(group_mapping))) {
    stop("group_mapping must be a named list (output from assign_groups_interactive)")
  }

  if (combine_all && !combine_groups) {
    stop("combine_all = TRUE requires combine_groups = TRUE")
  }

  # Extract all indices from group_mapping (handle both numeric and string formats)
  all_indices <- unlist(group_mapping)
  if (is.character(all_indices)) {
    # Handle ".WBPth" format
    all_indices <- as.numeric(gsub("\\.WBPth$", "", all_indices))
  }

  # Validate indices
  if (any(is.na(all_indices))) {
    stop("group_mapping contains invalid indices")
  }

  if (any(all_indices > length(df_list)) || any(all_indices < 1)) {
    stop("group_mapping contains indices outside the range of df_list (1 to ",
         length(df_list), ")")
  }

  # Initialize output
  organized_data <- list()

  # Process each group
  for (group_name in names(group_mapping)) {
    indices <- group_mapping[[group_name]]

    # Convert string indices to numeric if needed
    if (is.character(indices)) {
      indices <- as.numeric(gsub("\\.WBPth$", "", indices))
    }

    group_dfs <- list()

    # Process each subject in the group
    for (i in seq_along(indices)) {
      idx <- indices[i]
      df <- df_list[[idx]]

      # Validate it's actually a data frame
      if (!is.data.frame(df)) {
        warning("Element ", idx, " in df_list is not a data frame. Skipping.")
        next
      }

      # Add group identifier
      df$group <- group_name

      # Add subject identifier if requested
      if (add_subject_id) {
        df$subject_id <- paste0(group_name, "_", i)
      }

      # Store in list
      group_dfs[[i]] <- df
    }

    # Combine within group if requested
    if (combine_groups) {
      organized_data[[group_name]] <- dplyr::bind_rows(group_dfs)
    } else {
      # Keep as nested list with informative names
      names(group_dfs) <- paste0(group_name, "_", seq_along(indices))
      organized_data[[group_name]] <- group_dfs
    }
  }

  # Combine all groups if requested
  if (combine_all) {
    return(dplyr::bind_rows(organized_data))
  }

  return(organized_data)
}
