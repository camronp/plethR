# R/sheets_into_list.R

#' Load and Process Excel Sheets into a List of Data Frames
#'
#' This function loads all sheets from an Excel file into a list of data frames,
#' with options to apply several preprocessing steps, including removing specific sheets,
#' selecting relevant columns, cleaning date formats, removing rows with missing values,
#' and averaging data by date.
#'
#' @param excel_file A character string specifying the path to the Excel file.
#' @param remove_apnea Logical, if `TRUE`, removes sheets that contain "Apnea" in their names. Default is `FALSE`.
#' @param clean_time Logical, if `TRUE`, formats the "Time" column as a date in the format `YYYY-MM-DD`. Default is `FALSE`.
#' @param date_average Logical, if `TRUE`, averages all columns by date in the "Time" column. Requires `clean_time = TRUE` to ensure the "Time" column is formatted correctly. Default is `FALSE`.
#' @param remove_na Logical, if `TRUE`, removes rows with missing values in each sheet. Default is `FALSE`.
#'
#' @return A list of data frames, with each data frame representing one processed sheet from the Excel file.
#'
#' @details
#' This function loads all sheets in the specified Excel file into a list of data frames.
#' Several options allow for data cleaning and preprocessing:
#'
#' - `remove_apnea`: If `TRUE`, any sheet with "Apnea" in its name is excluded from the list.
#' - `clean_time`: If `TRUE`, the "Time" column (if present) is converted to a date format.
#' - `remove_na`: If `TRUE`, rows containing `NA` values are removed from each sheet.
#' - `date_average`: If `TRUE`, the data is averaged by date, with each unique date in the "Time" column representing a summary row.
#'
#' @examples
#' #recommend saving excel file as: excel_file <- "example.xlsx". and optionally use as object in sheets_into_list()
#'
#' # Load all sheets from an Excel file and remove any sheets containing "Apnea" in their name
#' df_list <- sheets_into_list(excel_file, remove_apnea = TRUE)
#'
#' # Load sheets and clean the "Time" column, remove rows with NA, and average data by date
#' df_list <- sheets_into_list("data.xlsx", clean_time = TRUE, date_average = TRUE, remove_na = TRUE)
#'
#' @export
#'

sheets_into_list <- function(excel_file, remove_apnea = FALSE,
                             clean_time = FALSE, date_average = FALSE,
                             remove_na = FALSE) {

  sheet_names <- readxl::excel_sheets(excel_file)

  df_list <- lapply(sheet_names, function(sheet_name) {
    readxl::read_excel(excel_file, sheet = sheet_name)
  })
  names(df_list) <- sheet_names

  # Optionally remove sheets with "Apnea" in the name
  if (remove_apnea) {
    df_list <- df_list[!grepl("Apnea", names(df_list))]
  }

  df_list <- lapply(df_list, function(df) {
    df %>%
      select(-c(Tbody, Subject, Phase, Rinx, RH, Tc, Recording, Alarms))
  })

  # Optionally clean the Time column
  if (clean_time) {
    df_list <- lapply(df_list, function(df) {
      if ("Time" %in% names(df)) {
        df$Time <- as.Date(substr(df$Time, 1, 10), format = "%Y-%m-%d")
      }
      return(df)
    })
  }

  # Optionally remove rows with NA values
  if (remove_na) {
    df_list <- lapply(df_list, function(df) na.omit(df))
  }

  # Optionally average data by date in the Time column
  if (date_average) {
    df_list <- lapply(df_list, function(df) {
      df %>%
        group_by(Time) %>%
        summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))
    })
  }

  return(df_list)
}

#cp_df_list_summarized <- sheets_into_list(excel_file, remove_apnea = TRUE, remove_na = TRUE, date_average = TRUE)

#cp_df_list <- sheets_into_list(excel_file, remove_apnea = TRUE, clean_time = TRUE, remove_na = TRUE)

