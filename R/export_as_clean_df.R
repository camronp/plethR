#' Export Data Frames as Clean Excel File
#'
#' This function exports either cleaned renamed data frames or merged renamed data frames
#' to an Excel file, saved in the current working directory. Each data frame is saved
#' as a separate sheet in the Excel file.
#'
#' @param data_frames A named list of data frames to be exported. This can be the output of either
#' `rename_grouped_dataframes()` or `merge_group_avg()`.
#' @param file_name A character string specifying the base file name for the Excel file.
#' The file will be saved with the `.xlsx` extension in the current working directory.
#'
#' @details
#' Each data frame in the input list will be written to a separate sheet in the Excel file.
#' The sheet names will be derived from the names of the data frames in the list.
#'
#' @return NULL (invisible). The function is used for its side effect of exporting the Excel file.
#'
#' @examples
#' # Example usage with renamed cleaned data frames
#' renamed_dfs <- rename_grouped_dataframes(df_list, group_mapping, group_names)
#' export_as_clean_df(renamed_dfs, file_name = "cleaned_data")
#'
#' # Example usage with merged renamed data frames
#' merged_dfs <- merge_group_avg(df_list_clean, group_identities, group_names)
#' export_as_clean_df(merged_dfs, file_name = "merged_data")
#'
#' @export
export_as_clean_df <- function(data_frames, file_name) {
  # Validate inputs
  if (!is.list(data_frames) || !all(sapply(data_frames, is.data.frame))) {
    stop("data_frames must be a list of data frames.")
  }
  if (!is.character(file_name) || length(file_name) != 1) {
    stop("file_name must be a single character string.")
  }

  # Add .xlsx extension if not already present
  if (!grepl("\\.xlsx$", file_name)) {
    file_name <- paste0(file_name, ".xlsx")
  }

  # Create the Excel file
  file_path <- file.path(getwd(), file_name)
  writexl::write_xlsx(data_frames, path = file_path)

  message("Data frames have been successfully exported to ", file_path)

  invisible(NULL)
}
