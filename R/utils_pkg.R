# Small internal utilities shared across the package.
#
# IMPORTANT PACKAGING NOTE:
# In the original single-file script, `.has_hrbrthemes <- requireNamespace(...)`
# and friends were top-level statements, safe to evaluate fresh every time a
# *script* is sourced. In a *package* they need care: a bare top-level
# assignment is only ever evaluated once, at build time. An earlier version
# of this file tried to fix that with makeActiveBinding() in .onLoad(), but
# active bindings do not survive R's package lazy-load serialization - they
# silently freeze into NULL instead of staying dynamic. The robust fix is a
# plain function, called fresh every time it's needed - ordinary functions
# always survive package (de)serialization correctly.

#' Check whether an optional (Suggests-only) package is installed
#'
#' Used to gate optional plot styling / hover-query features that degrade
#' gracefully if the relevant package isn't installed, rather than being a
#' hard dependency of gfcdash itself.
#'
#' @param pkg Character. Package name to check.
#' @return `TRUE`/`FALSE`.
#' @keywords internal
pkg_available <- function(pkg) requireNamespace(pkg, quietly = TRUE)

#' `%||%`
#'
#' Return `b` if `a` is `NULL`, length zero, or `NA`; otherwise return `a`.
#' @param a,b Any R objects.
#' @return `a` or `b`.
#' @keywords internal
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

# Required runtime dependencies (kept in sync with DESCRIPTION's Imports).
required_pkgs <- c("shiny", "shinydashboard", "shinyjs", "shinyWidgets", "sf", "terra",
                    "leaflet", "leafem", "mapgl", "RCurl", "stringr", "DT", "ggplot2",
                    "patchwork", "dplyr", "tidyr", "tidyterra", "scales", "animation",
                    "jsonlite", "ggfx", "viridisLite", "zip", "units", "hrbrthemes",
                    "randomcoloR")

#' Make sure every gfcdash dependency is actually installed, and install
#' anything missing
#'
#' Called automatically at the start of \code{\link{run_gfc_dashboard}}, so
#' this runs fresh every launch - not just once at package-install time.
#' This is what catches the case that silently bit us before: R gets
#' upgraded (which on Windows typically starts you with a brand new, empty
#' package library), or a single package fails to install for some
#' unrelated reason, and the dashboard would otherwise just quietly run
#' with reduced features (or a plain crash) with no explanation to a user
#' who has no way to diagnose that themselves.
#'
#' If anything needs installing, this prints a plain-language message and
#' installs it right then, before the dashboard UI is built. If a package
#' still can't be installed afterward (e.g. no internet connection), this
#' stops with a clear, actionable message rather than letting the app
#' launch into a silently broken state.
#'
#' @param pkgs Character vector of package names to check. Defaults to
#'   gfcdash's own required packages.
#' @return Invisibly, `NULL`. Called for its side effect of installing
#'   missing packages and messaging progress.
#' @keywords internal
ensure_gfcdash_dependencies <- function(pkgs = required_pkgs) {
  missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_pkgs) == 0) {
    return(invisible(NULL))
  }

  message(
    "gfcdash needs ", length(missing_pkgs), " package(s) that aren't installed yet: ",
    paste(missing_pkgs, collapse = ", "), ".\n",
    "Installing them now - this is a one-time step and may take a minute..."
  )

  utils::install.packages(missing_pkgs, repos = "https://cloud.r-project.org", dependencies = TRUE)

  still_missing <- missing_pkgs[!vapply(missing_pkgs, requireNamespace, logical(1), quietly = TRUE)]

  if (length(still_missing) > 0) {
    stop(
      "gfcdash could not install the following required package(s): ",
      paste(still_missing, collapse = ", "), ".\n",
      "This usually means there's no internet connection, or CRAN is temporarily ",
      "unreachable. Please check your connection and try running run_gfc_dashboard() ",
      "again. If the problem continues, try running this line yourself and read the ",
      "error message it gives:\n\n  install.packages(c(\"",
      paste(still_missing, collapse = "\", \""), "\"))",
      call. = FALSE
    )
  }

  message("All required packages are now installed. Launching the dashboard...")
  invisible(NULL)
}

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
  to_update <- base::intersect(pkgs, rownames(old))
  if (length(to_update) > 0) {
    message("Updating: ", paste(to_update, collapse = ", "))
    utils::install.packages(to_update, repos = "https://cloud.r-project.org")
  } else {
    message("All required packages are already up to date.")
  }
  invisible(NULL)
}
