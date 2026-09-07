# gfcdash 0.1.0

* Initial release.
* `run_gfc_dashboard()` launches the interactive dashboard: upload or draw
  an AOI on a 3D globe, extract and threshold Hansen GFC tiles, compute
  annual cover/loss statistics, build classified and multi-panel maps, and
  animate the time series.
* `extract_gfc()`, `threshold_gfc()`, `gfc_stats()`, `calc_gfc_tiles()`,
  `check_aoi()`, and `utm_epsg()` are exported for use outside the
  dashboard (e.g. in scripted/batch workflows).
* `update_required_packages()` checks for and installs updates to
  gfcdash's own dependencies, on demand.
