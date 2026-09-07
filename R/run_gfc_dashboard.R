#' Launch the Forest Change Analysis Dashboard
#'
#' Starts the interactive Shiny dashboard for uploading or drawing an Area of
#' Interest (AOI), downloading and thresholding Hansen et al. Global Forest
#' Change (GFC) tiles, computing forest cover and loss statistics, and
#' building change maps, comparison maps, and animations.
#'
#' All files created during a session (downloaded tiles, thresholded
#' rasters, annual layers, plots, animations) are written to a private,
#' per-session temporary folder that is removed automatically when the
#' session ends. Nothing is written outside that folder unless the user
#' explicitly downloads a result.
#'
#' @param launch.browser Logical, or a function. Passed straight through to
#'   \code{\link[shiny]{runApp}}. Defaults to \code{TRUE}, which opens the
#'   dashboard in the system's default web browser.
#' @param ... Additional arguments passed on to \code{\link[shiny]{runApp}}
#'   (e.g. \code{port}, \code{host}).
#'
#' @return Invisibly returns \code{NULL}. Called for its side effect of
#'   launching a Shiny application.
#'
#' @examples
#' if (interactive()) {
#'   run_gfc_dashboard()
#' }
#'
#' @export
run_gfc_dashboard <- function(launch.browser = TRUE, ...) {
  app <- shiny::shinyApp(ui = gfc_app_ui(), server = gfc_app_server)
  shiny::runApp(app, launch.browser = launch.browser, ...)
}
