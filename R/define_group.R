#' Define Groups for Data
#'
#' This function allows you to define groups of items based on either a fixed number of groups and items per group, or custom group sizes.
#' It returns a list where each element contains the indices for a group.
#'
#' @param num_groups Integer specifying the number of groups to create. Required if `custom_sizes` is not provided.
#' @param items_per_group Integer specifying the number of items in each group. Required if `custom_sizes` is not provided.
#' @param custom_sizes Numeric vector specifying the custom sizes for each group. If provided, `num_groups` and `items_per_group` should not be used.
#'
#' @details
#' - If `custom_sizes` is provided, the function generates groups with sizes specified by the vector.
#' - If `num_groups` and `items_per_group` are provided, the function generates groups with the specified number of groups and items per group.
#' - If neither `custom_sizes` nor both `num_groups` and `items_per_group` are provided, an error is raised.
#'
#' @return A list where each element represents a group and contains the indices of the items in that group.
#'
#' @examples
#' # Define 4 groups with 4 items each
#' group_identities <- define_group(num_groups = 4, items_per_group = 4)
#'
#' # Define 5 groups with custom sizes (3, 1, 4, 4, 4)
#' group_identities <- define_group(num_groups = 5, custom_sizes = c(3, 1, 4, 4, 4))
#'
#' @export


define_group <- function(num_groups = NULL, items_per_group = NULL, custom_sizes = NULL) {

  if (!is.null(custom_sizes)) {
    group_identities <- lapply(seq_along(custom_sizes), function(i) {
      start <- if (i == 1) 1 else sum(custom_sizes[1:(i - 1)]) + 1
      end <- start + custom_sizes[i] - 1
      start:end
    })
  } else if (!is.null(num_groups) && !is.null(items_per_group)) {
    group_identities <- lapply(1:num_groups, function(i) {
      start <- (i - 1) * items_per_group + 1
      end <- start + items_per_group - 1
      start:end
    })
  } else {
    stop("Either specify num_groups and items_per_group, or provide custom_sizes.")
  }

  return(group_identities)
}

#group_identities <- define_group(4, custom_sizes = c(3, 1, 4, 4, 4))
#group_identities <- define_group(4, 4)
