#' Calculate Group Averages Across Time
#'
#' Computes summary statistics (mean by default) for each experimental group at
#' each time point, using a group mapping produced by `assign_groups_interactive()`
#' or `custom_define_group()`.
#'
#' @param df_list A list of data frames (typically from `sheets_into_list()`).
#' @param group_mapping A named list where each element contains the indices (or
#'   `".WBPth"`-style string identifiers) of the data frames belonging to that group.
#' @param time_var Character string specifying the time column name. Default is "Time".
#' @param summary_fun A summary function to apply to each numeric column within a
#'   group at each time point (e.g., `mean`, `median`, `sd`). Default is `mean`.
#' @param keep_n Logical; if `TRUE`, adds an `n` column reporting the number of
#'   subjects contributing to each time point. Default is `FALSE`.
#'
#' @details
#' For each group, the data frames referenced by `group_mapping` are stacked and
#' then summarized by `time_var`, applying `summary_fun` to every other numeric
#' column. Non-numeric columns (other than `time_var`) are dropped, since they
#' cannot be meaningfully summarized across subjects.
#'
#' `NA` values are ignored when computing the summary (`na.rm = TRUE` is passed to
#' `summary_fun` where supported).
#'
#' @return A named list of data frames, one per group, each containing `time_var`,
#'   the summarized numeric columns, and (if `keep_n = TRUE`) an `n` column.
#'
#' @examples
#' \dontrun{
#' df_list <- sheets_into_list("data.xlsx", clean_time = TRUE)
#' groups <- set_group_names("Control", "Treated")
#' mapping <- assign_groups_interactive(groups, df_list = df_list)
#'
#' group_avgs <- calculate_group_averages(df_list, mapping)
#'
#' # Include sample size per time point
#' group_avgs <- calculate_group_averages(df_list, mapping, keep_n = TRUE)
#'
#' # Use a different summary statistic
#' group_sds <- calculate_group_averages(df_list, mapping, summary_fun = sd)
#' }
#'
#' @importFrom dplyr bind_rows group_by summarize across ungroup
#' @export
calculate_group_averages <- function(df_list,
                                     group_mapping,
                                     time_var = "Time",
                                     summary_fun = mean,
                                     keep_n = FALSE) {

  # Validate inputs
  if (!is.list(df_list)) {
    stop("df_list must be a list of data frames")
  }

  if (!is.list(group_mapping) || is.null(names(group_mapping))) {
    stop("group_mapping must be a named list (output from assign_groups_interactive)")
  }

  if (!is.function(summary_fun)) {
    stop("summary_fun must be a function")
  }

  group_averages <- list()

  for (group_name in names(group_mapping)) {
    indices <- group_mapping[[group_name]]

    # Convert string indices (e.g. "1.WBPth") to numeric if needed
    if (is.character(indices)) {
      indices <- as.numeric(gsub("\\.WBPth$", "", indices))
    }

    if (any(is.na(indices))) {
      stop("group_mapping contains invalid indices for group: ", group_name)
    }

    if (any(indices > length(df_list)) || any(indices < 1)) {
      stop("group_mapping contains indices outside the range of df_list (1 to ",
           length(df_list), ") for group: ", group_name)
    }

    group_dfs <- df_list[indices]

    if (!all(sapply(group_dfs, is.data.frame))) {
      stop("All elements referenced by group_mapping must be data frames for group: ",
           group_name)
    }

    if (!all(sapply(group_dfs, function(df) time_var %in% names(df)))) {
      stop("Time variable '", time_var, "' not found in all data frames for group: ",
           group_name)
    }

    df_merged <- dplyr::bind_rows(group_dfs)

    # Keep only the time variable and numeric columns
    numeric_cols <- names(df_merged)[sapply(df_merged, is.numeric)]
    numeric_cols <- setdiff(numeric_cols, time_var)
    df_merged <- df_merged[, c(time_var, numeric_cols), drop = FALSE]

    df_summary <- df_merged %>%
      dplyr::group_by(.data[[time_var]]) %>%
      dplyr::summarize(
        dplyr::across(dplyr::all_of(numeric_cols),
                      ~ summary_fun(.x, na.rm = TRUE)),
        n = dplyr::n(),
        .groups = "drop"
      )

    if (!keep_n) {
      df_summary$n <- NULL
    }

    group_averages[[group_name]] <- df_summary
  }

  message("Calculated group averages for ", length(group_averages), " group(s)")

  return(group_averages)
}
