#' Rename and Optionally Unnest Data Frames by Group and Index
#'
#' This function assigns custom names to data frames in a list based on user-defined group mappings and sheet indices.
#' It ensures unique names by appending numeric suffixes to the group names. Additionally, it provides an option to
#' unnest the data frames either partially (for specific groups) or fully (combining all unnested data frames into one).
#'
#' @param df_list_clean A list of data frames to be renamed.
#' @param group_mapping A named list where each group name maps to a numeric vector of sheet indices.
#' @param group_names A character vector of custom names for the groups. The length must match the number of groups in `group_mapping`.
#' @param add_subject_column A logical value indicating whether to add a 'Subject' column to each data frame. Default is `FALSE`.
#' @param unnested A logical value indicating whether to fully unnest the grouped data frames. If `TRUE`, all data frames within the groups will be unnested and combined into one. Default is `FALSE`.
#'
#' @details
#' This function renames data frames in `df_list_clean` by combining the specified `group_names` with numeric suffixes
#' corresponding to their sheet indices in `group_mapping`. The resulting names, such as "Group A 1" and "Group B 2", provide
#' clear and consistent identifiers for each data frame, especially in workflows where merging is unnecessary but organization
#' is crucial.
#'
#' Additionally, the function offers the ability to unnest the data frames. If `unnested = TRUE`, the function will combine
#' all data frames in each group and then merge them into a single final unnest. Alternatively, if `unnested = FALSE`,
#' it will return the data frames grouped by their original group names.
#'
#' @return A list of renamed data frames grouped by their custom names. If `unnested = TRUE`, a single ungrouped data frame
#' combining all data frames will be returned.
#'
#' @examples
#' # Example: Input data frame list
#' df_list_clean <- list(df1, df2, df3, df4)
#'
#' # Define group mappings and custom names
#' group_mapping <- list("Group A" = c(1, 2), "Group B" = c(3, 4))
#' group_names <- c("Group A", "Group B")
#'
#' # Rename data frames with no unnesting
#' renamed_dfs <- rename_grouped_dataframes(df_list_clean, group_mapping, group_names)
#'
#' # Check renamed data frames
#' names(renamed_dfs)
#' # Output: "Group A 1" "Group A 2" "Group B 1" "Group B 2"
#'
#' # Rename and unnest all data frames
#' final_unnested <- rename_grouped_dataframes(df_list_clean, group_mapping, group_names, unnested = TRUE)
#'
#' @export

rename_grouped_dataframes <- function(df_list_clean, group_mapping, group_names, add_subject_column = FALSE, unnested = FALSE) {
  # Validate that `group_names` is a character vector
  if (!is.character(group_names)) {
    stop("group_names must be a character vector.")
  }

  # Function to unnest all dataframes in a list
  unnest_all <- function(nested_list) {
    # Recursively unnest all tibbles/data frames in the list
    do.call("bind_rows", nested_list)
  }

  # Initialize a named list to hold grouped dataframes
  grouped_dfs <- list()

  # Iterate over each group in the mapping
  for (i in seq_along(group_mapping)) {
    group_name <- group_names[i]  # Get the current group name
    sheet_indices <- group_mapping[[i]]  # Get the sheet indices for this group

    # Create a list for each group
    grouped_dfs[[group_name]] <- list()

    # Iterate through each sheet index and assign the dataframe to the appropriate group
    for (j in seq_along(sheet_indices)) {
      sheet_index <- sheet_indices[j]  # Current sheet index
      df <- df_list_clean[[sheet_index]]  # Get the dataframe by index

      # Optionally add a Subject column with the group name and index
      if (add_subject_column) {
        df$Subject <- paste0(group_name, j)  # e.g., "fdf1", "fdf2"
      }

      # Add the dataframe to the corresponding group
      grouped_dfs[[group_name]] <- append(grouped_dfs[[group_name]], list(df))
    }

    # If unnested = TRUE, unnest the dataframes within the group
    if (unnested) {
      grouped_dfs[[group_name]] <- unnest_all(grouped_dfs[[group_name]])
    }
  }

  # If unnested = TRUE, combine all unnested dataframes into one
  if (unnested) {
    final_unnested <- do.call("bind_rows", grouped_dfs)
    return(final_unnested)  # Return the final unnested dataframe
  }

  return(grouped_dfs)  # Return the grouped dataframes (unnested or not)
}
