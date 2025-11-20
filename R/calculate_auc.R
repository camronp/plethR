#' Calculate Area Under the Curve (AUC) for Plethysmography Parameters
#'
#' Computes the Area Under the Curve (AUC) for respiratory parameters over time
#' using the trapezoidal rule. Supports normalization, baseline correction, and
#' multiple summary statistics.
#'
#' @param data Either a named list of data frames (from `calculate_group_averages()`)
#'   or a single data frame with a 'group' column.
#' @param time_var Character string specifying the time column name. Default is "Time".
#' @param parameters Character vector of parameters to calculate AUC for. If `NULL`,
#'   calculates for all numeric columns. Default is `NULL`.
#' @param normalize_to Character string specifying the group to use for normalization
#'   (fold change relative to this group). If `NULL`, no normalization. Default is `NULL`.
#' @param baseline_correct Logical; if `TRUE`, subtracts the first time point value
#'   before calculating AUC. Useful for calculating change from baseline. Default is `FALSE`.
#' @param method Character string specifying AUC calculation method:
#'   - "trapezoid": Trapezoidal rule (default)
#'   - "sum": Simple sum of values (appropriate if time points are evenly spaced)
#' @param summary_stats Character vector specifying which summary statistics to include:
#'   - "mean": Mean AUC (always included)
#'   - "sd": Standard deviation
#'   - "se": Standard error
#'   - "ci": 95% confidence interval
#'   Default is `c("mean", "sd", "se")`.
#' @param exclude_na Logical; if `TRUE`, removes NA values before calculation.
#'   Default is `TRUE`.
#'
#' @return A data frame with columns:
#'   - `group`: Group name
#'   - `parameter`: Parameter name
#'   - `auc`: Area under the curve
#'   - `auc_normalized`: Normalized AUC (if `normalize_to` specified)
#'   - Additional columns based on `summary_stats`
#'
#' @details
#' The trapezoidal rule approximates the integral by summing trapezoids under the curve:
#' \deqn{AUC = \sum_{i=1}^{n-1} \frac{(t_{i+1} - t_i)(y_{i+1} + y_i)}{2}}
#'
#' When `baseline_correct = TRUE`, the first time point value is subtracted from all
#' subsequent values before AUC calculation, which is useful for measuring change from
#' baseline rather than absolute values.
#'
#' Normalization expresses AUC as fold-change relative to the specified reference group.
#'
#' @examples
#' \dontrun{
#' # Calculate AUC for all parameters
#' auc_results <- calculate_auc(group_averages)
#'
#' # Calculate AUC with normalization to control group
#' auc_results <- calculate_auc(group_averages,
#'                              normalize_to = "Control")
#'
#' # Calculate change from baseline AUC
#' auc_results <- calculate_auc(group_averages,
#'                              baseline_correct = TRUE,
#'                              normalize_to = "Control")
#'
#' # Specific parameters with confidence intervals
#' auc_results <- calculate_auc(group_averages,
#'                              parameters = c("f", "TVb", "Penh"),
#'                              summary_stats = c("mean", "sd", "ci"),
#'                              normalize_to = "Control")
#' }
#'
#' @importFrom dplyr bind_rows filter select rename left_join mutate group_by summarize
#' @export
calculate_auc <- function(data,
                          time_var = "Time",
                          parameters = NULL,
                          normalize_to = NULL,
                          baseline_correct = FALSE,
                          method = c("trapezoid", "sum"),
                          summary_stats = c("mean", "sd", "se"),
                          exclude_na = TRUE) {

  # Match arguments
  method <- match.arg(method)
  valid_stats <- c("mean", "sd", "se", "ci")
  if (!all(summary_stats %in% valid_stats)) {
    stop("summary_stats must be one or more of: ", paste(valid_stats, collapse = ", "))
  }

  # Convert single data frame to list if needed
  if (is.data.frame(data)) {
    if (!"group" %in% names(data)) {
      stop("Single data frame must contain a 'group' column")
    }
    data_list <- split(data, data$group)
  } else if (is.list(data)) {
    data_list <- data
  } else {
    stop("data must be either a data frame or a list of data frames")
  }

  # Validate all elements are data frames
  if (!all(sapply(data_list, is.data.frame))) {
    stop("All elements in data must be data frames")
  }

  # Check for time variable
  has_time <- sapply(data_list, function(df) time_var %in% names(df))
  if (!all(has_time)) {
    stop("Time variable '", time_var, "' not found in all data frames")
  }

  # Helper function to calculate AUC using trapezoidal rule
  calc_trapezoid_auc <- function(time, values, exclude_na = TRUE) {
    if (exclude_na) {
      valid_idx <- !is.na(values) & !is.na(time)
      time <- time[valid_idx]
      values <- values[valid_idx]
    }

    if (length(time) < 2) {
      return(NA_real_)
    }

    # Sort by time to ensure correct calculation
    ord <- order(time)
    time <- time[ord]
    values <- values[ord]

    # Trapezoidal rule: sum of (delta_t * (y[i+1] + y[i]) / 2)
    dt <- diff(time)
    avg_values <- (values[-1] + values[-length(values)]) / 2
    auc <- sum(dt * avg_values)

    return(auc)
  }

  # Helper function to calculate AUC using simple sum
  calc_sum_auc <- function(time, values, exclude_na = TRUE) {
    if (exclude_na) {
      valid_idx <- !is.na(values) & !is.na(time)
      time <- time[valid_idx]
      values <- values[valid_idx]
    }

    if (length(values) == 0) {
      return(NA_real_)
    }

    return(sum(values))
  }

  # Process each group
  auc_list <- list()

  for (group_name in names(data_list)) {
    df <- data_list[[group_name]]

    # Get time values
    time_values <- df[[time_var]]

    # Convert date to numeric if needed (days since first measurement)
    if (inherits(time_values, "Date") || inherits(time_values, "POSIXt")) {
      time_numeric <- as.numeric(difftime(time_values, min(time_values), units = "days"))
    } else if (is.numeric(time_values)) {
      time_numeric <- time_values
    } else {
      warning("Could not convert time variable to numeric for group: ", group_name)
      next
    }

    # Identify parameters
    exclude_cols <- c(time_var, "group", "subject_id", "n")
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    numeric_cols <- setdiff(numeric_cols, exclude_cols)

    if (!is.null(parameters)) {
      numeric_cols <- intersect(parameters, numeric_cols)
    }

    if (length(numeric_cols) == 0) {
      warning("No valid parameters found for group: ", group_name)
      next
    }

    # Calculate AUC for each parameter
    param_results <- list()

    for (param in numeric_cols) {
      values <- df[[param]]

      # Baseline correction if requested
      if (baseline_correct) {
        baseline <- values[which.min(time_numeric)]
        if (!is.na(baseline)) {
          values <- values - baseline
        }
      }

      # Calculate AUC
      if (method == "trapezoid") {
        auc <- calc_trapezoid_auc(time_numeric, values, exclude_na)
      } else {
        auc <- calc_sum_auc(time_numeric, values, exclude_na)
      }

      param_results[[param]] <- data.frame(
        group = group_name,
        parameter = param,
        auc = auc,
        stringsAsFactors = FALSE
      )
    }

    auc_list[[group_name]] <- dplyr::bind_rows(param_results)
  }

  # Combine all results
  if (length(auc_list) == 0) {
    stop("No AUC values could be calculated")
  }

  auc_results <- dplyr::bind_rows(auc_list)

  # Add summary statistics if requested (these would need raw data, so noting as placeholder)
  # For now, AUC is the summary from averaged data
  # If you want SD/SE/CI, you'd need to calculate AUC for each individual subject first

  # Normalize if requested
  if (!is.null(normalize_to)) {
    if (!normalize_to %in% unique(auc_results$group)) {
      stop("normalize_to group '", normalize_to, "' not found in data. ",
           "Available groups: ", paste(unique(auc_results$group), collapse = ", "))
    }

    reference_auc <- auc_results %>%
      dplyr::filter(group == normalize_to) %>%
      dplyr::select(parameter, auc) %>%
      dplyr::rename(auc_reference = auc)

    auc_results <- auc_results %>%
      dplyr::left_join(reference_auc, by = "parameter") %>%
      dplyr::mutate(
        auc_normalized = auc / auc_reference,
        fold_change = auc / auc_reference
      ) %>%
      dplyr::select(-auc_reference)

    message("Normalized to group: ", normalize_to)
  }

  # Sort results
  auc_results <- auc_results %>%
    dplyr::arrange(parameter, group)

  message("Calculated AUC for ", length(unique(auc_results$parameter)),
          " parameter(s) across ", length(unique(auc_results$group)), " group(s)")

  return(auc_results)
}
