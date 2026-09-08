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
#' The first time this is called after a fresh install of R (or if any
#' dependency somehow went missing), it automatically checks for and
#' installs anything needed before opening the dashboard - you don't need
#' to install or manage any packages yourself.
#'
#' @param ui Where the dashboard opens. \code{"browser"} (the default)
#'   always opens it in your system's default web browser - this is how the
#'   original script behaved. \code{"window"} uses RStudio's own pop-up
#'   window when run from inside RStudio (falling back to the browser
#'   outside RStudio). \code{"pane"} opens it in RStudio's Viewer pane.
#' @param ... Additional arguments passed on to \code{\link[shiny]{runApp}}
#'   (e.g. \code{port}, \code{host}).
#'
#' @return Invisibly returns \code{NULL}. Called for its side effect of
#'   launching a Shiny application.
#'
#' @examples
#' if (interactive()) {
#'   run_gfc_dashboard()                 # opens in your default browser
#'   run_gfc_dashboard(ui = "window")    # RStudio pop-up window
#'   run_gfc_dashboard(ui = "pane")      # RStudio Viewer pane
#' }
#'
#' @export
run_gfc_dashboard <- function(ui = c("browser", "window", "pane"), ...) {
  ui <- match.arg(ui)

  ensure_gfcdash_dependencies()

  launch_browser <- switch(ui,
    browser = TRUE,
    window  = getOption("shiny.launch.browser", interactive()),
    pane    = getOption("viewer", TRUE)
  )

  app <- shiny::shinyApp(ui = gfc_app_ui(), server = gfc_app_server)
  shiny::runApp(app, launch.browser = launch_browser, ...)
}
