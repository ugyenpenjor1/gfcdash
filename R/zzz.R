# Internal package environment, used to cache the GFC tile grid so it is
# only built once per R session rather than on every call to
# get_gfc_tile_grid(). Recreated fresh in .onLoad() every time a user loads
# the package - never baked in once at build time on the developer's
# machine.
.gfc_env <- NULL

.onLoad <- function(libname, pkgname) {
  .gfc_env <<- new.env(parent = emptyenv())
}
