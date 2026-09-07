# Internal package environment, used to cache the GFC tile grid so it is
# only built once per R session rather than on every call to
# get_gfc_tile_grid(). Also holds the optional-package availability flags.
#
# Both are computed in .onLoad(), which runs once per USER, when THEY load
# the package - never baked in once at build time on the developer's
# machine (see utils_pkg.R for why that distinction matters).
.gfc_env <- NULL

.onLoad <- function(libname, pkgname) {
  .gfc_env <<- new.env(parent = emptyenv())

  .gfc_env$has_hrbrthemes  <- requireNamespace("hrbrthemes", quietly = TRUE)
  .gfc_env$has_ggfx        <- requireNamespace("ggfx", quietly = TRUE)
  .gfc_env$has_randomcoloR <- requireNamespace("randomcoloR", quietly = TRUE)
  .gfc_env$has_leafem      <- requireNamespace("leafem", quietly = TRUE)
}

# Convenience accessors so the rest of the package can keep using the short
# `.has_hrbrthemes` etc. names it already had, without every call site
# needing to write `.gfc_env$has_hrbrthemes`.
makeActiveBinding(".has_hrbrthemes",  function() .gfc_env$has_hrbrthemes,  environment())
makeActiveBinding(".has_ggfx",        function() .gfc_env$has_ggfx,        environment())
makeActiveBinding(".has_randomcoloR", function() .gfc_env$has_randomcoloR, environment())
makeActiveBinding(".has_leafem",      function() .gfc_env$has_leafem,     environment())
