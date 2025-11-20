#' Assign Sheet Indices to Groups Interactively
#'
#' Interactively assigns sheet numbers to predefined experimental groups.
#' Returns a named list mapping each group to its assigned sheet indices.
#'
#' @param group_names Character vector of group names.
#' @param df_list Optional list of data frames from `sheets_into_list()`. If provided,
#'   displays available sheet names to help users select indices. Default is `NULL`.
#' @param use_string_value Logical; if `TRUE`, converts indices to string identifiers
#'   (e.g., "1.WBPth"). Used for compatibility with DSI naming conventions. Default is `FALSE`.
#' @param allow_ranges Logical; if `TRUE`, allows range notation (e.g., "1-4" for 1,2,3,4).
#'   Default is `TRUE`.
#'
#' @details
#' The function prompts users to input sheet numbers for each group. Input formats:
#' - Comma-separated: "1,3,5"
#' - Ranges (if `allow_ranges = TRUE`): "1-4,7,9-11"
#' - Spaces are automatically removed
#'
#' Duplicate assignments across groups will produce an error. If `df_list` is provided,
#' the function displays available sheets with their names before prompting.
#'
#' @return A named list where each element corresponds to a group and contains
#'   assigned sheet indices (numeric or character if `use_string_value = TRUE`).
#'
#' @examples
#' \dontrun{
#' # Define groups
#' groups <- set_group_names("Control", "Treated", "Vehicle")
#'
#' # Assign sheets interactively
#' mapping <- assign_groups_interactive(groups)
#'
#' # With data frame list for reference
#' df_list <- sheets_into_list("data.xlsx")
#' mapping <- assign_groups_interactive(groups, df_list = df_list)
#'
#' # With string identifiers for DSI compatibility
#' mapping <- assign_groups_interactive(groups, use_string_value = TRUE)
#' }
#'
#' @export

assign_groups_interactive <- function(group_names,
                                      df_list = NULL,
                                      use_string_value = FALSE,
                                      allow_ranges = TRUE) {

  # Validate inputs
  if (!is.character(group_names)) {
    stop("group_names must be a character vector")
  }

  if (length(group_names) == 0) {
    stop("group_names cannot be empty")
  }

  # Display available sheets if df_list provided
  if (!is.null(df_list)) {
    cat("\n=== Available Sheets ===\n")
    for (i in seq_along(df_list)) {
      cat(sprintf("%2d. %s\n", i, names(df_list)[i]))
    }
    cat("========================\n\n")
    max_sheet <- length(df_list)
  } else {
    max_sheet <- Inf
  }

  # Initialize group mapping
  group_mapping <- list()

  # Helper function to parse input
  parse_sheet_input <- function(input_str) {
    # Remove all whitespace
    input_str <- gsub("\\s+", "", input_str)

    if (input_str == "") {
      return(numeric(0))
    }

    # Split by comma
    parts <- unlist(strsplit(input_str, ","))

    # Process each part (could be single number or range)
    all_numbers <- c()
    for (part in parts) {
      if (allow_ranges && grepl("-", part)) {
        # Handle range (e.g., "1-4")
        range_parts <- as.numeric(unlist(strsplit(part, "-")))
        if (length(range_parts) != 2 || any(is.na(range_parts))) {
          stop("Invalid range format: ", part)
        }
        all_numbers <- c(all_numbers, seq(range_parts[1], range_parts[2]))
      } else {
        # Single number
        num <- as.numeric(part)
        if (is.na(num)) {
          stop("Invalid number: ", part)
        }
        all_numbers <- c(all_numbers, num)
      }
    }

    return(all_numbers)
  }

  # Collect sheet assignments for each group
  for (group in group_names) {
    valid_input <- FALSE

    while (!valid_input) {
      cat(sprintf("\nEnter sheet numbers for group '%s'\n", group))
      if (allow_ranges) {
        cat("  Format: comma-separated or ranges (e.g., '1,3,5' or '1-4,7,9-11'): ")
      } else {
        cat("  Format: comma-separated (e.g., '1,3,5'): ")
      }

      user_input <- readline()

      # Try to parse input
      sheet_numbers <- tryCatch({
        parse_sheet_input(user_input)
      }, error = function(e) {
        cat("  Error:", e$message, "\n")
        return(NULL)
      })

      if (is.null(sheet_numbers)) {
        next  # Try again
      }

      # Validate sheet numbers
      if (length(sheet_numbers) == 0) {
        cat("  Warning: No sheets assigned to this group. Continue anyway? (y/n): ")
        confirm <- tolower(readline())
        if (confirm != "y") {
          next
        }
      }

      if (any(sheet_numbers < 1)) {
        cat("  Error: Sheet numbers must be positive integers\n")
        next
      }

      if (!is.infinite(max_sheet) && any(sheet_numbers > max_sheet)) {
        cat(sprintf("  Error: Sheet numbers cannot exceed %d (total sheets available)\n", max_sheet))
        next
      }

      # Check for duplicates within this group
      if (length(sheet_numbers) != length(unique(sheet_numbers))) {
        cat("  Warning: Duplicate sheet numbers within group removed\n")
        sheet_numbers <- unique(sheet_numbers)
      }

      # Input is valid
      valid_input <- TRUE

      # Convert to string if requested
      if (use_string_value) {
        sheet_numbers <- paste0(sheet_numbers, ".WBPth")
      }

      group_mapping[[group]] <- sheet_numbers

      # Display what was assigned
      cat(sprintf("  Assigned %d sheet(s) to '%s'\n", length(sheet_numbers), group))
    }
  }

  # Validate no duplicates across groups
  if (use_string_value) {
    all_sheets <- unlist(group_mapping)
  } else {
    all_sheets <- unlist(lapply(group_mapping, function(x) {
      if (is.character(x)) as.numeric(gsub(".WBPth", "", x)) else x
    }))
  }

  if (length(all_sheets) != length(unique(all_sheets))) {
    duplicates <- all_sheets[duplicated(all_sheets)]
    stop("Duplicate sheet assignments detected: ",
         paste(unique(duplicates), collapse = ", "),
         "\nEach sheet must belong to only one group.")
  }

  # Summary
  cat("\n=== Group Assignment Summary ===\n")
  for (group in names(group_mapping)) {
    sheets <- group_mapping[[group]]
    if (length(sheets) == 0) {
      cat(sprintf("  %s: (no sheets)\n", group))
    } else {
      cat(sprintf("  %s: %s\n", group, paste(sheets, collapse = ", ")))
    }
  }
  cat("================================\n\n")

  return(group_mapping)
}
