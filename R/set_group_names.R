#' Set Group Names
#'
#' Creates a character vector of group names from individual arguments or a vector.
#' This is a convenience function for defining experimental group labels.
#'
#' @param ... Individual group names as character strings, or a single character vector.
#'
#' @return A character vector containing the group names.
#'
#' @examples
#' # Pass names individually (no c() needed)
#' group_names <- set_group_names("Uninfected Wildtype", "Uninfected Benac",
#'                                "Infected Benac", "Infected Wildtype")
#'
#' # Or pass as a vector (still works)
#' group_names <- set_group_names(c("Control", "Treated"))
#'
#' # Single group
#' group_names <- set_group_names("Control")
#'
#' @export
set_group_names <- function(...) {
  # Capture all arguments
  args <- list(...)

  # If single argument that's already a vector, use it directly
  if (length(args) == 1 && is.character(args[[1]]) && length(args[[1]]) > 1) {
    names_vector <- args[[1]]
  } else {
    # Otherwise, concatenate all arguments
    names_vector <- unlist(args)
  }

  # Validate that result is character
  if (!is.character(names_vector)) {
    stop("All inputs must be character strings")
  }

  # Check for empty strings
  if (any(names_vector == "")) {
    stop("Group names cannot be empty strings")
  }

  # Check for duplicates (might be intentional, so just warn)
  if (any(duplicated(names_vector))) {
    warning("Duplicate group names detected: ",
            paste(names_vector[duplicated(names_vector)], collapse = ", "))
  }

  return(names_vector)
}





