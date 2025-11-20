#' Export Data Frames to Excel File
#'
#' Exports data frames to an Excel workbook. Can handle either a single data frame
#' or a list of data frames (each as a separate sheet).
#'
#' @param data_frames Either a single data frame or a named list of data frames.
#'   Can be output from `organize_by_groups()`, `calculate_group_averages()`,
#'   or any data frame/list of data frames.
#' @param file_name Character string specifying the output file name.
#'   The `.xlsx` extension is added automatically if not present.
#' @param sheet_name Character string specifying the sheet name when exporting
#'   a single data frame. Default is "Data". Ignored if `data_frames` is a list.
#' @param output_dir Character string specifying the output directory.
#'   Default is the current working directory (`getwd()`).
#' @param overwrite Logical; if `TRUE`, overwrites existing file without warning.
#'   Default is `FALSE`.
#' @param timestamp Logical; if `TRUE`, appends timestamp to filename to prevent
#'   overwriting. Default is `FALSE`.
#'
#' @return Invisibly returns the full file path of the created Excel file.
#'
#' @details
#' If a single data frame is provided, it's written to a single sheet. If a list
#' is provided, each data frame is written to a separate sheet with names derived
#' from list element names.
#'
#' Sheet names longer than 31 characters (Excel's limit) are truncated with a warning.
#' If the output directory doesn't exist, it will be created automatically.
#'
#' @examples
#' \dontrun{
#' # Export single data frame
#' all_data <- organize_by_groups(df_list, mapping,
#'                                combine_groups = TRUE,
#'                                combine_all = TRUE)
#' export_to_excel(all_data, "combined_data")
#'
#' # Export list of data frames (one sheet per group)
#' averages <- calculate_group_averages(df_list, mapping)
#' export_to_excel(averages, "group_averages")
#'
#' # Custom sheet name for single data frame
#' export_to_excel(all_data, "my_data", sheet_name = "AllSubjects")
#'
#' # Export to specific directory with timestamp
#' export_to_excel(averages, "results",
#'                 output_dir = "~/analysis/results",
#'                 timestamp = TRUE)
#' }
#'
#' @importFrom writexl write_xlsx
#' @export
export_to_excel <- function(data_frames,
                            file_name,
                            sheet_name = "Data",
                            output_dir = getwd(),
                            overwrite = FALSE,
                            timestamp = FALSE) {

  # Handle single data frame - convert to list
  if (is.data.frame(data_frames)) {
    data_frames <- setNames(list(data_frames), sheet_name)
    message("Exporting single data frame to sheet: ", sheet_name)
  }

  # Validate inputs
  if (!is.list(data_frames)) {
    stop("data_frames must be either a data frame or a list of data frames")
  }

  if (length(data_frames) == 0) {
    stop("data_frames is empty - nothing to export")
  }

  if (!all(sapply(data_frames, is.data.frame))) {
    stop("All elements in data_frames must be data frames")
  }

  if (!is.character(file_name) || length(file_name) != 1 || nchar(file_name) == 0) {
    stop("file_name must be a non-empty character string")
  }

  # Check for unnamed list elements
  if (is.null(names(data_frames))) {
    warning("data_frames has no names. Assigning default names: Sheet1, Sheet2, ...")
    names(data_frames) <- paste0("Sheet", seq_along(data_frames))
  } else if (any(names(data_frames) == "")) {
    warning("Some data frames are unnamed. Assigning default names.")
    unnamed_idx <- which(names(data_frames) == "")
    names(data_frames)[unnamed_idx] <- paste0("Sheet", unnamed_idx)
  }

  # Check sheet name length (Excel limit is 31 characters)
  long_names <- names(data_frames)[nchar(names(data_frames)) > 31]
  if (length(long_names) > 0) {
    warning("Sheet names longer than 31 characters will be truncated:\n  ",
            paste(long_names, collapse = "\n  "))
    names(data_frames) <- substr(names(data_frames), 1, 31)
  }

  # Add timestamp if requested
  if (timestamp) {
    timestamp_str <- format(Sys.time(), "%Y%m%d_%H%M%S")
    file_name <- paste0(tools::file_path_sans_ext(file_name),
                        "_", timestamp_str)
  }

  # Ensure .xlsx extension
  if (!grepl("\\.xlsx$", file_name, ignore.case = TRUE)) {
    file_name <- paste0(file_name, ".xlsx")
  }

  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    message("Creating output directory: ", output_dir)
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Construct full file path
  file_path <- file.path(output_dir, file_name)

  # Check if file exists
  if (file.exists(file_path) && !overwrite) {
    stop("File already exists: ", file_path,
         "\nUse overwrite = TRUE to replace, or timestamp = TRUE to create unique filename")
  }

  # Export to Excel
  tryCatch({
    writexl::write_xlsx(data_frames, path = file_path)

    if (length(data_frames) == 1) {
      message("Successfully exported ", nrow(data_frames[[1]]), " rows to:\n  ", file_path)
    } else {
      message("Successfully exported ", length(data_frames), " sheet(s) to:\n  ", file_path)
    }
  }, error = function(e) {
    stop("Failed to write Excel file: ", e$message)
  })

  # Return file path invisibly
  invisible(file_path)
}
