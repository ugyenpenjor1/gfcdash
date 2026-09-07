# Core GFC (Global Forest Change) helper functions - download, mosaic,
# threshold, and summarise Hansen et al. tiles for a user-supplied AOI.
#
# The tile-grid cache (.gfc_env$gfc_tiles) is created fresh per R session in
# zzz.R's .onLoad() hook, so it is never shared or stale across users/sessions.

################################################################################
# SECTION 1: CORE GFC HELPER FUNCTIONS
# (adapted from gfanalysis package that is now retired)
# new functions that were not in gfcanalysis package are: get_gfc_tile_grid,
# check_aoi, make_tile_mosaic, utm_epsg, forest_cover_year, plot_forest_change,
# compute_forest_mask, compute_yearly_stats
################################################################################


get_gfc_tile_grid <- function() {
  if (is.null(.gfc_env$gfc_tiles)) {
    # Hansen GFC tiles are a regular 10x10 degree grid.
    # Longitude: -180 to 170 (west edges), Latitude: -60 to 70 (south edges)
    # covers the full published extent (roughly 80N to 60S).
    lon_mins <- seq(-180, 170, by = 10)
    lat_mins <- seq(-60, 70, by = 10)
    
    grid_cells <- expand.grid(xmin = lon_mins, ymin = lat_mins)
    grid_cells$xmax <- grid_cells$xmin + 10
    grid_cells$ymax <- grid_cells$ymin + 10
    
    polys <- lapply(seq_len(nrow(grid_cells)), function(i) {
      g <- grid_cells[i, ]
      sf::st_polygon(list(matrix(
        c(g$xmin, g$ymin,
          g$xmax, g$ymin,
          g$xmax, g$ymax,
          g$xmin, g$ymax,
          g$xmin, g$ymin),
        ncol = 2, byrow = TRUE
      )))
    })
    
    gfc_tiles <- sf::st_sf(
      geometry = sf::st_sfc(polys, crs = 4326)
    )
    
    .gfc_env$gfc_tiles <- gfc_tiles
  }
  .gfc_env$gfc_tiles
}

# ---- check_aoi ---------------------------------------------------------------
#' Validate and repair an AOI geometry
#'
#' Coerces `sp`/`terra` inputs to `sf`, drops Z/M dimensions, and repairs
#' invalid geometries with \code{sf::st_make_valid()}.
#'
#' @param aoi An `sf`, `SpatialPolygonsDataFrame`, or `SpatVector` object.
#' @return A valid `sf` object.
#' @export
check_aoi <- function(aoi) {
  if (inherits(aoi, "SpatialPolygonsDataFrame")) {
    aoi <- sf::st_as_sf(aoi)
    
  } else if (inherits(aoi, "SpatVector")) {
    aoi <- sf::st_as_sf(aoi)
    
  } else if (!inherits(aoi, "sf")) {
    stop("AOI must be an sf, SpatialPolygonsDataFrame, or SpatVector object.")
  }
  
  aoi <- sf::st_zm(aoi) # Remove Z/M dimensions
  valid <- sf::st_is_valid(aoi) # Check geometry validity
  
  # Repair invalid geometries
  if (any(!valid, na.rm = TRUE)) {
    aoi <- sf::st_make_valid(aoi)
  }
  
  # Make sure everything is valid after repair
  valid_after <- sf::st_is_valid(aoi)
  
  if (any(!valid_after, na.rm = TRUE)) {
    stop("AOI contains geometries that could not be repaired.")
  }
  aoi
}

# ---- calc_gfc_tiles -----------------------------------------------------------
#' Determine which Hansen GFC tiles intersect an AOI
#'
#' @param aoi An `sf`, `SpatialPolygonsDataFrame`, or `SpatVector` object.
#' @return An `sf` object of the intersecting 10x10 degree tile polygons.
#' @export
calc_gfc_tiles <- function(aoi) {
  aoi <- check_aoi(aoi)
  gfc_tiles <- get_gfc_tile_grid()

  if (!identical(sf::st_crs(aoi), sf::st_crs(gfc_tiles))) {
    message("aoi and GFC tile grid have different CRS - reprojecting aoi.")
    aoi <- sf::st_transform(aoi, sf::st_crs(gfc_tiles))
  }

  intersecting <- apply(
    sf::st_intersects(gfc_tiles, sf::st_convex_hull(aoi), sparse = FALSE),
    MARGIN = 1, FUN = any
  )

  if (sum(intersecting) == 0) stop("No intersecting GFC tiles found for this AOI.")

  gfc_tiles[intersecting, ]
}

# ---- download helpers ----------------------------------------------------------
verify_download <- function(tile_url, local_path) {
  ok <- tryCatch({
    header <- RCurl::getURL(tile_url, nobody = 1L, header = 1L)
    header <- strsplit(header, "\r\n")[[1]]
    content_length <- header[grepl("Content-Length: ", header)]
    remote_size <- as.numeric(stringr::str_extract(content_length, "[0-9]+"))
    local_size <- file.info(local_path)$size
    if (length(remote_size) == 0 || is.na(remote_size) || remote_size != local_size) {
      3
    } else {
      0
    }
  }, error = function(e) 3)
  ok
}

download_tile <- function(tile_url, local_path) {
  ret_code <- tryCatch(
    download.file(tile_url, local_path, mode = "wb", quiet = TRUE),
    error = function(e) 1
  )
  if (ret_code != 0) {
    message(paste("Warning: problem downloading", basename(local_path)))
    return(1)
  } else if (verify_download(tile_url, local_path) != 0) {
    message(paste("Warning: verification failed on", basename(local_path)))
    return(2)
  }
  0
}

download_tiles <- function(tiles, output_folder,
                            images = c("treecover2000", "lossyear", "gain", "datamask"),
                            dataset = "GFC-2025-v1.13",
                            progress_fun = NULL) {

  stopifnot(all(images %in% c("treecover2000", "lossyear", "gain",
                               "datamask", "first", "last")))
  if (!dir.exists(output_folder)) stop("output_folder does not exist")

  successes <- 0; failures <- 0; skips <- 0
  n_tiles <- nrow(tiles)

  for (n in seq_len(n_tiles)) {
    gfc_tile <- tiles[n, ]
    bb <- sf::st_bbox(gfc_tile)
    min_x <- bb["xmin"]; max_y <- bb["ymax"]
    min_x <- if (min_x < 0) paste0(sprintf("%03i", abs(min_x)), "W") else paste0(sprintf("%03i", min_x), "E")
    max_y <- if (max_y < 0) paste0(sprintf("%02i", abs(max_y)), "S") else paste0(sprintf("%02i", max_y), "N")

    file_root <- paste0("Hansen_", dataset, "_")
    file_suffix <- paste0("_", max_y, "_", min_x, ".tif")
    filenames <- paste0(file_root, images, file_suffix)

    tile_urls <- paste0(
      "http://commondatastorage.googleapis.com/earthenginepartners-hansen/",
      dataset, "/", filenames
    )
    local_paths <- file.path(output_folder, filenames)

    for (i in seq_along(filenames)) {
      if (file.exists(local_paths[i]) && verify_download(tile_urls[i], local_paths[i]) == 0) {
        skips <- skips + 1
        next
      }
      if (download_tile(tile_urls[i], local_paths[i]) == 0) {
        successes <- successes + 1
      } else {
        failures <- failures + 1
      }
    }

    if (!is.null(progress_fun)) {
      progress_fun(n / n_tiles, detail = sprintf("Tile %d of %d", n, n_tiles))
    }
  }

  list(successes = successes, skips = skips, failures = failures)
}

# ---- make_tile_mosaic (terra version) -------------------------------------------
make_tile_mosaic <- function(aoi, data_folder, dataset, stack = "change") {
  if (stack == "change") {
    image_names <- c("treecover2000", "lossyear", "gain", "datamask")
    band_names <- image_names
  } else if (stack %in% c("first", "last")) {
    image_names <- stack
    band_names <- c("Band3", "Band4", "Band5", "Band7")
  } else {
    stop('"stack" must be equal to "change", "first", or "last"')
  }

  aoi <- check_aoi(aoi)
  tiles <- calc_gfc_tiles(aoi)
  aoi_proj <- sf::st_transform(aoi, sf::st_crs(tiles))
  aoi_vect <- terra::vect(aoi_proj)

  file_root <- paste0("Hansen_", dataset, "_")
  tile_rasters <- vector("list", nrow(tiles))

  for (n in seq_len(nrow(tiles))) {
    tile <- tiles[n, ]
    bb <- sf::st_bbox(tile)
    min_x <- bb["xmin"]; max_y <- bb["ymax"]
    min_x <- if (min_x < 0) paste0(sprintf("%03i", abs(min_x)), "W") else paste0(sprintf("%03i", min_x), "E")
    max_y <- if (max_y < 0) paste0(sprintf("%02i", abs(max_y)), "S") else paste0(sprintf("%02i", max_y), "N")

    file_suffix <- paste0("_", max_y, "_", min_x, ".tif")
    filenames <- file.path(data_folder, paste0(file_root, image_names, file_suffix))

    if (!all(file.exists(filenames))) {
      stop("Missing tile file(s) in data_folder: ",
           paste(filenames[!file.exists(filenames)], collapse = ", "))
    }

    tile_stack <- terra::rast(filenames)
    names(tile_stack) <- band_names
    tile_rasters[[n]] <- terra::crop(tile_stack, aoi_vect)
  }

  tile_mosaic <- if (length(tile_rasters) > 1) {
    do.call(terra::mosaic, c(tile_rasters, fun = "mean"))
  } else {
    tile_rasters[[1]]
  }

  names(tile_mosaic) <- band_names
  tile_mosaic
}

# ---- utm_epsg (returns an EPSG code, not a proj4string) -------------------------
#' Look up the UTM zone EPSG code for a longitude/latitude point
#'
#' @param x Longitude, in decimal degrees.
#' @param y Latitude, in decimal degrees.
#' @return An integer EPSG code.
#' @export
utm_epsg <- function(x, y) {
  if (x < -180 || x > 180) stop("longitude must be between -180 and 180")
  if (y < -90 || y > 90) stop("latitude must be between -90 and 90")

  zone_num <- floor((x + 180) / 6) + 1
  if (y >= 56.0 && y < 64.0 && x >= 3.0 && x < 12.0) zone_num <- 32
  if (y >= 72.0 && y < 84.0) {
    if (x >= 0.0 && x < 9.0) zone_num <- 31
    else if (x >= 9.0 && x < 21.0) zone_num <- 33
    else if (x >= 21.0 && x < 33.0) zone_num <- 35
    else if (x >= 33.0 && x < 42.0) zone_num <- 37
  }
  prefix <- if (y >= 0) "326" else "327"
  as.integer(paste0(prefix, sprintf("%02i", zone_num)))
}


# ---- extract_gfc -----------------------------------------------------------------
#' Extract GFC data for an AOI, downloading tiles to a temp folder
#' @param aoi sf, sp SpatialPolygonsDataFrame, or terra SpatVector
#' @param to_UTM reproject output to the UTM zone of the AOI centroid
#' @param stack "change" (default), "first", or "last"
#' @param dataset which Hansen dataset version to use
#' @param keep_tiles if TRUE, don't delete the downloaded tiles afterward
#' @param temp_folder folder to download raw Hansen tiles into (always a
#'   temp directory in this app - never a hard-coded path)
#' @param progress_fun optional callback(fraction, detail) for a Shiny progress bar
#' @return a SpatRaster
#' @export
extract_gfc <- function(aoi,
                         to_UTM = TRUE,
                         stack = "change",
                         dataset = "GFC-2025-v1.13",
                         keep_tiles = FALSE,
                         temp_folder = NULL,
                         progress_fun = NULL) {

  if (stack == "change") {
    images <- c("treecover2000", "lossyear", "gain", "datamask")
  } else if (stack %in% c("first", "last")) {
    images <- stack
  } else {
    stop('"stack" must be equal to "change", "first", or "last"')
  }

  aoi <- check_aoi(aoi)
  tiles <- calc_gfc_tiles(aoi)

  created_temp <- is.null(temp_folder)
  if (created_temp) {
    temp_folder <- tempfile(pattern = "GFC_tiles_")
    dir.create(temp_folder)
  }

  on.exit({
    if (created_temp && !keep_tiles) unlink(temp_folder, recursive = TRUE)
  }, add = TRUE)

  download_tiles(tiles, output_folder = temp_folder, images = images,
                  dataset = dataset, progress_fun = progress_fun)

  tile_mosaic <- make_tile_mosaic(aoi, data_folder = temp_folder, dataset = dataset, stack = stack)

  if (to_UTM) {
    ext_poly <- terra::as.polygons(terra::ext(tile_mosaic), crs = terra::crs(tile_mosaic))
    centroid_ll <- terra::project(terra::centroids(ext_poly), "EPSG:4326")
    xy <- terra::crds(centroid_ll)
    epsg <- utm_epsg(xy[1, 1], xy[1, 2])
    # nearest neighbour, since these are categorical layers
    tile_mosaic <- terra::project(tile_mosaic, paste0("EPSG:", epsg), method = "near")
  }

  tile_mosaic
}

# ---- threshold_gfc ----------------------------------------------------------
#' Threshold the GFC product into forest/loss/gain classes
#' @param gfc a SpatRaster with 4 layers: treecover2000, lossyear, gain, datamask
#' @param forest_threshold percent canopy cover to use as forest/non-forest cutoff
#' @param filename optional - path to write the output raster to disk (a temp
#'   session path in this app)
#' @param overwrite overwrite filename if it exists
#' @return a SpatRaster with 5 layers: forest2000, lossyear, gain, lossgain, datamask
#' Threshold the GFC product into forest/loss/gain classes
#'
#' @param gfc A `SpatRaster` with 4 layers: treecover2000, lossyear, gain,
#'   datamask (i.e. the output of \code{\link{extract_gfc}}).
#' @param forest_threshold Percent canopy cover used as the forest/non-forest
#'   cutoff.
#' @param filename Optional path to write the output raster to disk.
#' @param overwrite Overwrite `filename` if it already exists.
#' @return A `SpatRaster` with 5 layers: forest2000, lossyear, gain,
#'   lossgain, datamask.
#' @export
threshold_gfc <- function(gfc, forest_threshold = 30, filename = NULL, overwrite = FALSE) {

  if (terra::nlyr(gfc) != 4) stop("gfc must have 4 layers: treecover2000, lossyear, gain, datamask")
  names(gfc) <- c("treecover2000", "lossyear", "gain", "datamask")

  recode_fun <- function(treecover2000, lossyear, gain, datamask) {
    forest2000       <- ifelse(treecover2000 > forest_threshold, 1, 0)
    lossyear_recode  <- lossyear * forest2000
    gain_recode      <- ifelse(gain == 1 & forest2000 == 0, 1, 0)
    lossgain         <- ifelse(gain == 1 & lossyear != 0, 1, 0)
    cbind(forest2000, lossyear_recode, gain_recode, lossgain, datamask)
  }

  thresholded <- terra::lapp(gfc[[c("treecover2000", "lossyear", "gain", "datamask")]],
                              fun = recode_fun)
  names(thresholded) <- c("forest2000", "lossyear", "gain", "lossgain", "datamask")

  if (!is.null(filename)) {
    thresholded <- terra::writeRaster(thresholded, filename,
                                       overwrite = overwrite, datatype = "INT1U")
  }

  thresholded
}

# ---- gfc_stats ----------------------------------------------------------------
#' Calculate annual forest loss/gain statistics for an AOI
#' Calculate annual forest loss/gain statistics for an AOI
#'
#' @param aoi An `sf` object (typically \code{check_aoi()}-validated).
#' @param gfc A `SpatRaster`, the thresholded output of
#'   \code{\link{threshold_gfc}}.
#' @param scale_factor Optional multiplier applied to area sums.
#' @param dataset Which Hansen dataset version was used (controls the
#'   final year in the annual series).
#' @return A list with `loss_table` and `gain_table` data frames.
#' @export
gfc_stats <- function(aoi, gfc, scale_factor = 1, dataset = "GFC-2025-v1.13") {

  if (terra::nlyr(gfc) != 5) stop("gfc must be thresholded output from threshold_gfc()")
  names(gfc) <- c("forest2000", "lossyear", "gain", "lossgain", "datamask")

  aoi <- check_aoi(aoi)
  aoi <- sf::st_transform(aoi, terra::crs(gfc))
  aoi_vect <- terra::vect(aoi)

  gfc_extent_poly <- terra::as.polygons(terra::ext(gfc), crs = terra::crs(gfc))
  if (!any(terra::relate(gfc_extent_poly, aoi_vect, "intersects"))) {
    stop("aoi does not intersect supplied GFC extract")
  }

  if (!("label" %in% names(aoi))) {
    aoi$label <- paste("AOI", seq_len(nrow(aoi)))
  }
  uniq_aoi_labels <- unique(aoi$label)

  data_year <- as.numeric(stringr::str_extract(dataset, "(?<=GFC-?)[0-9]{4}"))
  years <- seq(2000, data_year)
  n_years <- max(years) - 2000

  pixel_area <- terra::cellSize(gfc, unit = "ha")

  loss_table <- data.frame(
    year  = rep(years, length(uniq_aoi_labels)),
    aoi   = rep(uniq_aoi_labels, each = length(years)),
    cover = 0,
    loss  = 0
  )

  gain_table <- data.frame(
    period   = rep(paste0("2000-", max(years)), length(uniq_aoi_labels)),
    aoi      = uniq_aoi_labels,
    gain     = 0,
    lossgain = 0
  )

  area_sum <- function(mask_layer, area_r) {
    terra::global(mask_layer * area_r, "sum", na.rm = TRUE)[1, 1] * scale_factor
  }

  for (i in seq_len(nrow(aoi))) {
    this_label <- aoi$label[i]
    this_poly  <- aoi_vect[i, ]

    gfc_masked  <- terra::mask(terra::crop(gfc, this_poly), this_poly)
    area_masked <- terra::mask(terra::crop(pixel_area, this_poly), this_poly)

    loss_st_row <- match(this_label, loss_table$aoi)
    gain_row    <- match(this_label, gain_table$aoi)

    loss_table$cover[loss_st_row] <- area_sum(gfc_masked[["forest2000"]], area_masked)
    loss_table$loss[loss_st_row]  <- NA

    lossyear_r <- gfc_masked[["lossyear"]]
    for (yr_i in seq_len(n_years)) {
      this_row <- loss_st_row + yr_i
      loss_area <- area_sum(lossyear_r == yr_i, area_masked)
      loss_table$loss[this_row]  <- loss_area
      loss_table$cover[this_row] <- loss_table$cover[this_row - 1] - loss_area
    }

    gain_table$gain[gain_row]     <- area_sum(gfc_masked[["gain"]], area_masked)
    gain_table$lossgain[gain_row] <- area_sum(gfc_masked[["lossgain"]], area_masked)
  }

  list(loss_table = loss_table, gain_table = gain_table)
}

# ---- forest_cover_year -------------------------------------------------------
#' Reconstruct forest cover for any year from GFC loss/gain layers
forest_cover_year <- function(gfc, target_year, aoi = NULL) {
  names(gfc) <- c("treecover2000", "lossyear", "gain", "datamask")

  loss_mask <- gfc[["lossyear"]] > 0 & gfc[["lossyear"]] <= (target_year - 2000)

  forest_current <- gfc[["treecover2000"]]
  forest_current[loss_mask] <- 0

  gain_mask <- gfc[["gain"]] == 1 & !loss_mask
  forest_current[gain_mask] <- gfc[["treecover2000"]][gain_mask]

  if (!is.null(aoi)) {
    aoi <- check_aoi(aoi)
    aoi <- sf::st_transform(aoi, terra::crs(forest_current))
    forest_current <- terra::mask(forest_current, terra::vect(aoi))
  }

  names(forest_current) <- paste0("treecover_", target_year)
  forest_current
}

# ---- plot_forest_change (ggplot/patchwork version) ---------------------------
plot_forest_change <- function(gfc, aoi, target_year, combined = TRUE) {
  aoi_sf <- check_aoi(aoi)
  aoi_sf <- sf::st_transform(aoi_sf, terra::crs(gfc))

  tree2000 <- terra::mask(gfc[["treecover2000"]], terra::vect(aoi_sf))
  names(tree2000) <- "treecover"

  forest_target <- forest_cover_year(gfc, target_year, aoi = aoi_sf)
  names(forest_target) <- "treecover"

  p1 <- ggplot() +
    geom_spatraster(data = tree2000) +
    geom_sf(data = aoi_sf, fill = NA, color = "#cc3333", linewidth = 0.8) +
    scale_fill_viridis_c(name = "% cover", na.value = "transparent", limits = c(0, 100)) +
    labs(title = "Treecover 2000") +
    theme_minimal()

  p2 <- ggplot() +
    geom_spatraster(data = forest_target) +
    geom_sf(data = aoi_sf, fill = NA, color = "#cc3333", linewidth = 0.8) +
    scale_fill_viridis_c(name = "% cover", na.value = "transparent", limits = c(0, 100)) +
    labs(title = paste("Treecover", target_year)) +
    theme_minimal()

  if (combined) p1 + p2 else list(p2000 = p1, target = p2)
}

# ---- annual_stack -------------------------------------------------------------
#' Codes: 0 nodata, 1 forest, 2 non-forest, 3 loss, 4 gain, 5 loss+gain, 6 water
annual_stack <- function(gfc, dataset = "GFC-2025-v1.13", progress_fun = NULL) {

  if (terra::nlyr(gfc) != 5) stop("gfc must be thresholded output from threshold_gfc()")
  names(gfc) <- c("forest2000", "lossyear", "gain", "lossgain", "datamask")

  data_year <- as.numeric(stringr::str_extract(dataset, "(?<=GFC-?)[0-9]{4}"))
  years <- seq(2000, data_year)
  layer_names <- paste0("y", years)

  out_layers <- vector("list", length(layer_names))

  for (n in seq_along(layer_names)) {
    if (n == 1) {
      this_year <- gfc[["forest2000"]]
      this_year[this_year == 0] <- 2  # non-forest
    } else {
      this_year <- out_layers[[n - 1]]
      this_year[(gfc[["lossyear"]] == (n - 1)) & (gfc[["gain"]] == 0)] <- 3  # loss
    }
    this_year[(gfc[["gain"]] == 1) & (gfc[["lossyear"]] == 0)] <- 4  # gain
    this_year[gfc[["lossgain"]] == 1] <- 5                           # loss + gain
    this_year[gfc[["datamask"]] == 2] <- 6                           # water
    names(this_year) <- layer_names[n]
    out_layers[[n]] <- this_year

    if (!is.null(progress_fun)) {
      progress_fun(n / length(layer_names), detail = paste("Year", years[n]))
    }
  }

  out <- terra::rast(out_layers)
  out[gfc[["datamask"]] == 0] <- 0
  terra::time(out) <- as.Date(paste0(years, "-01-01"))
  names(out) <- layer_names
  out
}

# ---- theme_dark_custom --------------------------------------------------------
# Dark, glow-friendly theme used for on-screen dashboard display.
theme_dark_custom <- function() {
  base_family <- if (pkg_available("hrbrthemes")) "sans" else ""
  theme_minimal(base_size = 13) +
    theme(
      text = element_text(family = base_family),
      plot.background  = element_rect(fill = "black", colour = NA),
      panel.background = element_rect(fill = "black", colour = NA),
      legend.background = element_rect(fill = "black", colour = NA),
      legend.key = element_rect(fill = "black", colour = NA),
      panel.grid = element_line(colour = "grey15")
    )
}

# with_glow(): wraps a geom in ggfx::with_outer_glow when ggfx is available,
# otherwise returns the geom unchanged so the app still works without it.
with_glow <- function(geom, colour = "white", sigma = 15, expand = 15) {
  if (pkg_available("ggfx")) {
    ggfx::with_outer_glow(geom, colour = colour, sigma = sigma, expand = expand)
  } else {
    geom
  }
}


# ---- shared classified-map palette (used by both plot_gfc() and the Leaflet
#      interactive classified map, so the two views always agree) ---------------
GFC_CLASS_CODES  <- c(1, 2, 3, 4, 5, 6, 0)
GFC_CLASS_COLORS <- c("1" = "#009966", "2" = "#cc9933", "3" = "#cc3333",
                       "4" = "#993366", "5" = "#ff99cc", "6" = "#336699", "0" = "#999999")
GFC_CLASS_LABELS <- c("1" = "Forest", "2" = "Non-forest", "3" = "Forest loss",
                       "4" = "Forest gain", "5" = "Loss and gain", "6" = "Water", "0" = "No data")

# ---- plot_gfc: classified single-year change map -------------------------------
plot_gfc <- function(
    fchg,
    aoi = NULL,
    plot_aoi = TRUE,
    aoi_crop = FALSE,
    title_string = "",
    size_scale = 1,
    maxpixels = 300000
) {
  if (!inherits(fchg, "SpatRaster")) stop("fchg must be a terra::SpatRaster.")
  if (!is.logical(plot_aoi) || length(plot_aoi) != 1) stop("plot_aoi must be TRUE or FALSE.")
  if (!is.logical(aoi_crop) || length(aoi_crop) != 1) stop("aoi_crop must be TRUE or FALSE.")

  if (plot_aoi || aoi_crop) {
    if (is.null(aoi)) stop("An sf AOI must be supplied when plot_aoi or aoi_crop is TRUE.")
    if (!inherits(aoi, "sf")) stop("aoi must be an sf object.")
    aoi <- sf::st_zm(aoi, drop = TRUE, what = "ZM")
    if (any(sf::st_is_empty(aoi))) {
      aoi <- aoi[!sf::st_is_empty(aoi), ]
      if (nrow(aoi) == 0) stop("aoi contains no valid geometries.")
    }
    aoi <- sf::st_transform(aoi, sf::st_crs(terra::crs(fchg)))
    aoi <- sf::st_make_valid(aoi)
  }

  fchg_plot <- fchg
  if (aoi_crop) {
    aoi_vect <- terra::vect(aoi)
    fchg_plot <- terra::crop(fchg_plot, aoi_vect)
    fchg_plot <- terra::mask(fchg_plot, aoi_vect)
  }

  ncell_total <- terra::ncell(fchg_plot)
  if (ncell_total > maxpixels) {
    fact <- ceiling(sqrt(ncell_total / maxpixels))
    fchg_plot <- terra::aggregate(fchg_plot, fact = fact, fun = "modal", na.rm = TRUE)
  }

  raster_df <- terra::as.data.frame(fchg_plot, xy = TRUE, na.rm = FALSE)
  if (ncol(raster_df) < 3) stop("The raster contains no plottable data.")

  names(raster_df)[3] <- "value"
  raster_df$value <- factor(raster_df$value, levels = c(1, 2, 3, 4, 5, 6, 0))

  p <- ggplot(raster_df, aes(x = x, y = y, fill = value)) +
    geom_raster() +
    coord_equal() +
    scale_fill_manual(
      name = "Cover",
      breaks = c("1", "2", "3", "4", "5", "6", "0"),
      labels = c("Forest", "Non-forest", "Forest loss", "Forest gain",
                 "Loss and gain", "Water", "No data"),
      values = c("1" = "#009966", "2" = "#cc9933", "3" = "#cc3333",
                 "4" = "#993366", "5" = "#ff99cc", "6" = "#336699", "0" = "#999999"),
      drop = FALSE
    )

  if (plot_aoi) {
    p <- p + geom_sf(data = aoi, inherit.aes = FALSE, fill = NA, colour = "white", linewidth = 0.8)
  }

  p <- p +
    theme_bw(base_size = 12 * size_scale) +
    theme(
      axis.text.x = element_blank(), axis.text.y = element_blank(),
      axis.title.x = element_blank(), axis.title.y = element_blank(),
      panel.background = element_blank(), panel.border = element_blank(),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      plot.background = element_blank(), axis.ticks = element_blank(),
      plot.margin = grid::unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
      plot.title = element_text(hjust = 0.5, margin = margin(b = -0.5))
    ) +
    guides(fill = guide_legend(title = "", keywidth = 2.5, override.aes = list(alpha = 1))) +
    ggtitle(title_string)

  p
}

# ---- animate_annual -------------------------------------------------------------
animate_annual <- function(
    aoi,
    gfc_stack,
    out_dir,
    out_basename = "gfc_animation",
    site_name = "",
    type = "html",
    height = 3,
    width = 3,
    dpi = 150,
    dataset = "GFC-2025-v1.13",
    plot_aoi = TRUE,
    aoi_crop = FALSE,
    progress_fun = NULL
) {
  if (!inherits(gfc_stack, "SpatRaster")) stop("gfc_stack must be a terra::SpatRaster.")
  if (!inherits(aoi, "sf")) stop("aoi must be an sf object.")

  aoi <- sf::st_zm(aoi, drop = TRUE, what = "ZM")
  if (any(sf::st_is_empty(aoi))) {
    aoi <- aoi[!sf::st_is_empty(aoi), ]
    if (nrow(aoi) == 0) stop("aoi contains no valid geometries.")
  }
  aoi <- sf::st_make_valid(aoi)

  year_match <- stringr::str_extract(dataset, "(?<=GFC-?)[0-9]{4}")
  if (is.na(year_match)) stop("Could not extract the GFC year from dataset: ", dataset)
  data_year <- as.integer(year_match)
  n_layers <- terra::nlyr(gfc_stack)
  if (n_layers < 1) stop("gfc_stack contains no layers.")

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_dir <- normalizePath(out_dir, mustWork = TRUE)

  if (tools::file_ext(out_basename) != "") stop("out_basename should not have an extension.")

  type <- tolower(type)
  if (!type %in% c("gif", "html")) stop("type must be either 'gif' or 'html'.")

  dates <- seq(2001, by = 1, length.out = n_layers)
  maxpixels <- ceiling((width * height * dpi^2) / 1000) * 1000

  animation::ani.options(outdir = out_dir, ani.width = width * dpi,
                          ani.height = height * dpi, verbose = FALSE)

  if (type == "gif") {
    out_file <- paste0(out_basename, ".gif")
    animation::saveGIF({
      for (n in seq_len(n_layers)) {
        p <- plot_gfc(fchg = gfc_stack[[n]], aoi = aoi, plot_aoi = plot_aoi,
                       aoi_crop = aoi_crop, title_string = dates[n],
                       size_scale = 1.4, maxpixels = maxpixels)
        print(p)
        if (!is.null(progress_fun)) {
          progress_fun(n / n_layers, detail = paste("Rendering frame", dates[n]))
        }
      }
    }, interval = 0.5, movie.name = out_file)
  }

  if (type == "html") {
    animation::saveHTML({
      for (n in seq_len(n_layers)) {
        p <- plot_gfc(fchg = gfc_stack[[n]], aoi = aoi, plot_aoi = plot_aoi,
                       aoi_crop = aoi_crop, title_string = dates[n],
                       size_scale = 1.4, maxpixels = maxpixels)
        print(p)
        if (!is.null(progress_fun)) {
          progress_fun(n / n_layers, detail = paste("Rendering frame", dates[n]))
        }
      }
    }, img.name = out_basename, imgdir = paste0(out_basename, "_imgs"),
       outdir = out_dir, htmlfile = paste0(out_basename, ".html"),
       autobrowse = FALSE, title = paste(site_name, "forest change"))
  }

  invisible(file.path(out_dir, if (type == "gif") paste0(out_basename, ".gif") else paste0(out_basename, ".html")))
}

# ---- compute_forest_mask -------------------------------------------------------
compute_forest_mask <- function(tree_r, loss_r, gain_r, year, threshold = 30) {
  loss_code <- year - 2000
  was_forest_2000 <- terra::ifel(tree_r >= threshold, 1L, 0L)
  not_lost_by_yr <- terra::ifel((loss_r == 0L) | (loss_r > loss_code), 1L, 0L)
  cond_A <- was_forest_2000 * not_lost_by_yr

  is_gain <- terra::ifel(gain_r == 1L, 1L, 0L)
  cond_B <- is_gain * not_lost_by_yr

  terra::ifel((cond_A + cond_B) > 0L, 1L, 0L)
}

area_ha <- function(mask, px_area) {
  sum(terra::values(mask * px_area), na.rm = TRUE)
}

# ---- compute_yearly_stats -------------------------------------------------------
compute_yearly_stats <- function(tree_r, loss_r, gain_r,
                                  base_year, target_year,
                                  pixel_area_ha,
                                  threshold = 30,
                                  progress_fun = NULL) {

  base_loss_code <- base_year - 2000
  mask_base <- compute_forest_mask(tree_r, loss_r, gain_r, base_year, threshold)
  base_area <- area_ha(mask_base, pixel_area_ha)

  n_yrs_total <- target_year - base_year + 1
  rows <- vector("list", length = n_yrs_total)

  for (i in seq_along(base_year:target_year)) {
    yr <- base_year + i - 1
    loss_code <- yr - 2000

    mask_yr <- compute_forest_mask(tree_r, loss_r, gain_r, yr, threshold)
    area_yr <- area_ha(mask_yr, pixel_area_ha)

    if (yr == base_year) {
      annual_loss_ha <- 0
    } else {
      was_forest_or_gained <- terra::ifel((tree_r >= threshold) | (gain_r == 1L), 1L, 0L)
      annual_loss_mask <- terra::ifel(loss_r == loss_code, 1L, 0L) * was_forest_or_gained
      annual_loss_ha   <- area_ha(annual_loss_mask, pixel_area_ha)
    }

    was_forest_or_gained <- terra::ifel((tree_r >= threshold) | (gain_r == 1L), 1L, 0L)
    lost_in_window <- terra::ifel((loss_r > base_loss_code) & (loss_r <= loss_code), 1L, 0L)
    cum_loss_mask <- lost_in_window * was_forest_or_gained
    cum_loss_ha <- area_ha(cum_loss_mask, pixel_area_ha)

    net_change_ha <- area_yr - base_area
    pct_remaining <- (area_yr / base_area) * 100

    rows[[i]] <- data.frame(
      year = yr,
      forest_area_ha = round(area_yr, 2),
      annual_loss_ha = round(annual_loss_ha, 2),
      cumulative_loss_ha = round(cum_loss_ha, 2),
      net_change_ha = round(net_change_ha, 2),
      pct_remaining = round(pct_remaining, 2)
    )

    if (!is.null(progress_fun)) {
      progress_fun(i / n_yrs_total, detail = paste("Year", yr))
    }
  }

  do.call(rbind, rows)
}

