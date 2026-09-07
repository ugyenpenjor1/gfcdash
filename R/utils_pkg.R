# Small internal utilities shared across the package.
#
# IMPORTANT PACKAGING NOTE:
# Top-level statements in R/ files are evaluated once, at build/install 
# time, on the developer's machine - the resulting TRUE/FALSE would get 
# baked into the package and shipped unchanged to every user, regardless 
# of what they actually have installed. These are now computed in 
# .onLoad() (see zzz.R) instead, so each user's own R session determines 
# its own values correctly.

#' `%||%`
#'
#' Return `b` if `a` is `NULL`, length zero, or `NA`; otherwise return `a`.
#' @param a,b Any R objects.
#' @return `a` or `b`.
#' @keywords internal
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

# Required runtime dependencies (kept in sync with DESCRIPTION's Imports).
# Used only by update_required_packages() for the "Check for package
# updates" button - ordinary loading/installation of these is already
# handled automatically by R when a user installs this package.
required_pkgs <- c("shiny", "shinydashboard", "shinyjs", "shinyWidgets", "sf", "terra",
                    "leaflet", "leafem", "mapgl", "RCurl", "stringr", "DT", "ggplot2",
                    "patchwork", "dplyr", "tidyr", "tidyterra", "scales", "animation",
                    "jsonlite")

#' Check for and install updates to gfcdash's dependency packages
#'
#' Checks CRAN for newer versions of the packages gfcdash depends on, and
#' installs any that are out of date. Intended to be called from the
#' dashboard's "Check for package updates" button, not run automatically -
#' silently auto-updating dependencies on every launch risks pulling in a
#' breaking change you haven't tested against.
#'
#' @param pkgs Character vector of package names to check. Defaults to
#'   gfcdash's own required packages.
#' @return Invisibly, `NULL`. Called for its side effect of installing
#'   updates and messaging progress.
#' @export
update_required_packages <- function(pkgs = required_pkgs) {
  old <- tryCatch(utils::old.packages(repos = "https://cloud.r-project.org"),
                   error = function(e) NULL)
  if (is.null(old)) {
    message("Could not check for updates (no internet connection?).")
    return(invisible(NULL))
  }
  to_update <- intersect(pkgs, rownames(old))
  if (length(to_update) > 0) {
    message("Updating: ", paste(to_update, collapse = ", "))
    utils::install.packages(to_update, repos = "https://cloud.r-project.org")
  } else {
    message("All required packages are already up to date.")
  }
  invisible(NULL)
}
