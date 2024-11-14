#' Calculate Area Under the Curve (AUC) for Each Parameter by Group
#'
#' This function calculates the Area Under the Curve (AUC) for each parameter over time for different groups.
#' The AUC is computed using the trapezoidal rule, and the standard deviation of the AUC is also calculated.
#' Optionally, the AUC values can be normalized to a specified group.
#'
#' @param groups_merged_named A list of data frames, where each element represents a group, and each data frame
#' contains a `Time` column and numeric variables (parameters) to compute AUC for.
#' @param group_to_normalize A character string specifying the group whose AUC values will be used for normalization.
#' If `NULL`, no normalization is performed. Default is `NULL`.
#'
#' @details
#' This function computes the AUC for each parameter by applying the trapezoidal method, which approximates the area under the
#' curve defined by each parameter's time series data. The function also computes the standard deviation (SD) of the AUC values.
#' If `group_to_normalize` is provided, the AUC values of all groups are normalized to the AUC of the specified group for each
#' parameter. The function returns a data frame containing the AUC values, AUC standard deviations, and optionally the
#' normalized AUC values.
#'
#' @return A data frame containing the following columns:
#' \itemize{
#'   \item \strong{Group}: The name of the group.
#'   \item \strong{Parameter}: The name of the parameter.
#'   \item \strong{AUC}: The computed Area Under the Curve for the parameter in the group.
#'   \item \strong{AUC_sd}: The standard deviation of the AUC values.
#'   \item \strong{AUC_normalized} (optional): The normalized AUC value, if `group_to_normalize` is provided.
#' }
#'
#' @import dplyr
#' @import purrr
#'
#' @examples
#' # Assuming `groups_merged_named` is a list of data frames, each containing Time and parameters
#' auc_results <- auc_by_parameter(groups_merged_named)
#'
#' # Calculate AUC and normalize by the specified group
#' auc_results_normalized <- auc_by_parameter(groups_merged_named, group_to_normalize = "Uninfected Wildtype")
#'
#' @export

auc_by_parameter <- function(groups_merged_named, group_to_normalize = NULL) {

  calc_auc_sd <- function(auc_df) {
    auc_values <- sapply(auc_df[, -1], function(x) {
      sum(diff(auc_df$Time) * (x[-1] + x[-length(x)])/2)
    })

    auc_sd_values <- sapply(auc_df[, -1], function(x) {
      sd(diff(auc_df$Time) * (x[-1] + x[-length(x)])/2)
    })

    return(list(AUC = auc_values, AUC_SD = auc_sd_values))
  }
  auc_list <- map2(groups_merged_named, names(groups_merged_named), function(df, group_name) {

    auc_result <- calc_auc_sd(df)
    data.frame(
      Group = group_name,
      Parameter = names(auc_result$AUC),
      AUC = auc_result$AUC,
      AUC_sd = auc_result$AUC_SD
    )
  })
  auc_results_combined <- bind_rows(auc_list)

  if (!is.null(group_to_normalize)) {
    group_auc <- auc_results_combined %>%
      filter(Group == group_to_normalize) %>%
      select(Parameter, AUC) %>%
      rename(AUC_group = AUC)

    auc_results_combined <- auc_results_combined %>%
      left_join(group_auc, by = "Parameter") %>%
      mutate(AUC_normalized = AUC / AUC_group) %>%
      select(Group, Parameter, AUC, AUC_normalized, AUC_sd)
  }

  return(auc_results_combined)
}

#auc_results_normalized <- auc_by_parameter(groups_merged_named, group_to_normalize = "Uninfected Wildtype")
