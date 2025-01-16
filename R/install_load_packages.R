
#' Install and Load Required Packages
#'
#' This function installs and loads all the necessary packages for the package development and usage.
#'
#' @export

install_load_packages <- function() {

  # List of packages required
  packages <- c("devtools", "ggrepel", "ggforce", "tidyverse",
                "readxl", "dplyr", "ggplot2", "zoo", "pheatmap", "extrafont", "RColorBrewer")

  # Check and install missing packages
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    install.packages(new_packages)
  }

  # Load packages
  lapply(packages, library, character.only = TRUE)

  # Return a message or object (if needed)
  return("All required packages are installed and loaded.")
}
