#' Assign Sheet Numbers to Groups Interactively with Optional String Mapping
#'
#' This function allows users to assign sheet numbers to predefined groups interactively.
#' It returns a named list where each element represents a group and contains the indices
#' of the sheets assigned to that group. An optional feature allows for mapping sheet numbers
#' to string identifiers (e.g., "1.WBPth") if `use_string_value` is set to `TRUE`.
#'
#' @param group_names Character vector specifying the names of the groups. Each name will be
#' prompted to collect the corresponding sheet numbers.
#' @param use_string_value Logical. If `TRUE`, sheet numbers will be mapped to string identifiers
#' (e.g., "1.WBPth"). Default is `FALSE`.
#'
#' @details
#' - The function prompts the user to input sheet numbers for each group interactively. Sheet numbers
#' should be entered as a comma-separated list (e.g., "1,3,5").
#' - Duplicate sheet assignments across groups are not allowed and will result in an error.
#' - The sheet numbers are assumed to correspond to the order in the input data (e.g., Sheet1 = 1, Sheet2 = 2, etc.).
#' - User should NOT insert spaces between comma-separated values.
#' - If `use_string_value` is `TRUE`, the sheet numbers will be appended with ".WBPth" to map them to string identifiers.
#'
#' @return A named list where each element corresponds to a group and contains the assigned sheet numbers
#' or string identifiers, depending on the value of `use_string_value`.
#'
#' @examples
#' # Requires user predefined group names using set_group_names function.
#' test_names <- set_group_names(c("Group A", "Group B", "Group C", "Group D"))
#'
#' # Interactively assign sheet numbers to these groups with string identifiers
#' group_mapping <- custom_define_group(group_names = test_names, use_string_value = TRUE)
#'
#' @export


custom_define_group <- function(group_names, use_string_value = FALSE) {
  # Ensure `group_names` is a character vector
  if (!is.character(group_names)) {
    stop("group_names must be a character vector.")
  }

  # Initialize an empty list to store group-to-sheet mappings
  group_mapping <- list()

  # Iterate through group names to collect sheet numbers
  for (group in group_names) {
    cat(sprintf("Enter sheet numbers for group '%s' as a comma-separated list (e.g., 1,3,5): ", group))
    user_input <- readline()  # Read user input
    # Convert input string to numeric vector
    sheet_numbers <- as.numeric(unlist(strsplit(user_input, ",")))

    # If use_string_value is TRUE, map sheet numbers to string identifiers
    if (use_string_value) {
      sheet_numbers <- paste0(sheet_numbers, ".WBPth")
    }

    group_mapping[[group]] <- sheet_numbers
  }

  # Validate that there are no duplicate sheet assignments
  all_sheets <- unlist(group_mapping)
  if (length(all_sheets) != length(unique(all_sheets))) {
    stop("Duplicate sheet numbers detected! Each sheet must belong to only one group.")
  }

  # Return the group mapping as a named list
  return(group_mapping)
}
