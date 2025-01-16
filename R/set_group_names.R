#' Set Group Names
#'
#' This function takes a character vector and returns it as-is, with an error check to ensure the input is a character vector.
#'
#' @param names_vector A character vector containing group names.
#'
#' @return The same character vector passed as input, containing the group names.
#'
#' @examples
#' # Example usage:
#' group_names <- set_group_names(c("Uninfected Wildtype", "Uninfected Benac", "Infected Benac", "Infected Wildtype"))
#' print(group_names)
#'
#' @export


set_group_names <- function(names_vector) {

  if(!is.character(names_vector)) {
    stop("Input must be a character.")
  }

  #Return the group names
  return(names_vector)
}



