#' Merge and Average Data by Group
#'
#' This function merges multiple data frames into groups based on specified group identities, and then averages the data
#' within each group by the `Time` variable. It is useful for summarizing data across different groups in a time series context.
#'
#' @param df_list_clean A list of cleaned data frames. Each data frame should contain a `Time` column and other numeric columns.
#' @param group_identities A list of numeric vectors specifying the indices of the data frames to be merged for each group.
#' @param group_names A character vector of names for each group. If provided, this will be used to name the resulting list of averaged groups. Default is `NULL`.
#'
#' @details
#' The function will take the data frames specified by `sheets_into_list`, merge them by group, and then calculate the mean for each
#' numeric column, grouped by `Time`. group_identities must be predefined by the define_group() function to properly summarize by group.
#' If `group_names` is provided, the resulting list of averaged data frames will be named according to the given `group_names` as previously defined by set_group_names().
#'
#' @return A list of data frames, where each element represents a group. Each data frame contains the average values of
#' the columns for each `Time` value within the group.
#'
#' @examples
#' # Assuming df_list_summarized is a list of cleaned data frames and group_identities is a list of group indices
#' groups_merged <- merge_group_avg(cp_df_list_summarized, group_identities)
#'
#' # If you want to name the groups, provide the group names
#' groups_merged_named <- merge_group_avg(df_list_summarized, group_identities, group_names)
#'
#' @export



merge_group_avg <- function(df_list_clean, group_identities, group_names = NULL) {
  # Apply the merging and averaging process for each group
  groups_merged <- lapply(group_identities, function(indices) {
    # Merge the selected dataframes by binding rows
    df_merged <- bind_rows(df_list_clean[indices])

    # Group by Time and calculate the mean of each column
    df_averaged <- df_merged %>%
      group_by(Time) %>%
      summarize(across(everything(), mean, na.rm = TRUE))

    return(df_averaged)
  })

  # Set group names using set_group_names if provided
  if (!is.null(group_names)) {
    group_names <- set_group_names(group_names)  # Call the set_group_names function
    names(groups_merged) <- group_names  # Assign names to the list
  }

  return(groups_merged)
}

