#' Launch the plethR Shiny Application
#'
#' Starts an interactive Shiny application for the complete plethR workflow:
#' importing DSI whole body plethysmography data, defining experimental groups,
#' calculating group averages and area under the curve, and generating
#' publication-quality time series plots, bar plots, heatmaps, and PCA plots.
#'
#' @param launch_browser Logical; if `TRUE`, opens the app in the system's default
#'   web browser. Default is `TRUE`.
#'
#' @return Does not return a value; starts the Shiny application.
#'
#' @examples
#' \dontrun{
#' run_plethR_app()
#' }
#'
#' @export
run_plethR_app <- function(launch_browser = TRUE) {
  required_pkgs <- c("shiny", "bslib", "DT", "shinycssloaders")
  missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    stop("The following packages are required to run the app: ",
         paste(missing_pkgs, collapse = ", "),
         ". Install them with install.packages(c(",
         paste(sprintf('"%s"', missing_pkgs), collapse = ", "), ")).")
  }

  app_dir <- system.file("shiny-app", package = "plethR")

  if (app_dir == "") {
    stop("Could not find the plethR Shiny app directory. Reinstall the plethR package.")
  }

  shiny::runApp(app_dir, launch.browser = launch_browser)
}

