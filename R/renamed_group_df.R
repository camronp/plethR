#' Rename Data Frames by Group and Parameter
#'
#' This function assigns custom names to data frames in a list based on user-defined group and sheet mappings.
#' It appends numeric suffixes to distinguish data frames within the same group and ensures no duplicate names are assigned.
#'
#' @param df_list_clean A list of data frames, each representing a dataset to be renamed.
#' @param group_mapping A named list where each group (name) is associated with a numeric vector of sheet indices.
#' @param group_names A character vector of custom names for the groups. The length must match the number of groups in
#' `group_mapping`.
#'
#' @details
#' This function renames individual data frames in the input list (`df_list_clean`) according to the user-defined group names
#' and their respective sheet indices from `group_mapping`. The new names are constructed by appending a numeric suffix to the
#' group name (e.g., "Group A 1", "Group A 2"). This is particularly useful for workflows where merging is not desired,
#' but clear and consistent naming of data frames is needed.
#'
#' @return A named list of data frames with updated names.
#'
#' @examples
#' # Example data frame list
#' df_list <- list(df1, df2, df3, df4)
#'
#' # Define group mappings and names
#' group_mapping <- list("Group A" = c(1, 2), "Group B" = c(3, 4))
#' group_names <- c("Group A", "Group B")
#'
#' # Rename the data frames
#' renamed_dfs <- rename_grouped_dataframes(df_list, group_mapping, group_names)
#'
#' # Access renamed data frames
#' names(renamed_dfs)
#' # [1] "Group A 1" "Group A 2" "Group B 1" "Group B 2"
#'
#' @export


rename_grouped_dataframes <- function(df_list_clean, group_mapping, group_names) {
  # Validate that `group_names` is a character vector
  if (!is.character(group_names)) {
    stop("group_names must be a character vector.")
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
    for (sheet_index in sheet_indices) {
      df <- df_list_clean[[sheet_index]]  # Get the dataframe by index

      # Add the dataframe to the corresponding group
      grouped_dfs[[group_name]] <- append(grouped_dfs[[group_name]], list(df))
    }
  }

  return(grouped_dfs)  # Return the grouped dataframes
}
