# Dashboard plotting helpers. Each plotting function takes a `dark` flag:
# dark = TRUE gives the glow-styled version used for on-screen display in
# the dashboard; dark = FALSE gives the plain version used for downloads.
# These are implementation details of the Shiny app and are not exported;
# call run_gfc_dashboard() to use the dashboard.

################################################################################
# SECTION 2: DASHBOARD PLOTTING HELPERS
# These wrap the various stand-alone plotting blocks from the original script
# into reusable functions, each taking a `dark` flag: dark = TRUE gives the
# glow-styled version used for on-screen display; dark = FALSE gives the
# plain version used when the user downloads a plot for reports.
################################################################################

# ---- Cover vs loss dual-axis trend line (from gfc_stats()$loss_table) ----------
make_cover_loss_plot <- function(dat, base_year, target_year, dark = FALSE) {
  dat <- dat
  dat[is.na(dat)] <- 0

  a.diff <- max(dat$cover) - min(dat$cover)
  b.diff <- max(dat$loss) - min(dat$loss)
  a.min  <- min(dat$cover)
  b.min  <- min(dat$loss)
  if (a.diff == 0) a.diff <- 1
  if (b.diff == 0) b.diff <- 1

  p <- ggplot(dat, aes(year, cover))

  if (dark) {
    p <- p +
      with_glow(geom_line(color = "#00cc66", linewidth = 0.6), colour = "green", sigma = 5, expand = 5) +
      with_glow(
        geom_line(aes(y = (loss - b.min) / b.diff * a.diff + a.min), color = "#990000", linewidth = 0.6),
        colour = "indianred", sigma = 5, expand = 5
      )
  } else {
    p <- p +
      geom_line(color = "darkgreen", linewidth = 0.9) +
      geom_line(aes(y = (loss - b.min) / b.diff * a.diff + a.min), color = "red", linewidth = 0.9)
  }

  p <- p +
    scale_y_continuous(
      name = "cover (ha)",
      sec.axis = sec_axis(transform = ~((. - a.min) * b.diff / a.diff) + b.min, name = "loss (ha)")
    ) +
    #scale_x_continuous(breaks = seq(base_year, target_year, by = max(1, round((target_year - base_year) / 15)))) +
    scale_x_continuous(breaks = seq(base_year, target_year, by = max(1, round((target_year - base_year) / 24)))) +
    labs(x = NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  if (dark) {
    p <- p + theme_dark_custom() +
      theme(
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color = "grey80"),
        axis.text.x = element_text(color = "grey80", angle = 45, hjust = 1),
        axis.title.y.left = element_text(color = "#00cc66"),
        axis.text.y.left = element_text(color = "#00cc66"),
        axis.title.y.right = element_text(color = "#cc0000"),
        axis.text.y.right = element_text(color = "#cc0000")
      )
  } else {
    p <- p + theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y.left = element_text(color = "darkgreen"),
        axis.text.y.left = element_text(color = "darkgreen"),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red")
      )
  }
  p
}

# ---- 4-panel yearly trend analysis (from compute_yearly_stats() output) -------
make_trend_panels <- function(yearly_stats, base_year, target_year, canopy_threshold, dark = FALSE) {

  x_by <- max(1, round((target_year - base_year) / 12))

  # Panel 1: absolute forest area over time
  p1 <- ggplot(yearly_stats, aes(x = year, y = forest_area_ha / 1000)) +
    { if (dark) geom_area(fill = "#2d6a4f", alpha = 0.35) else geom_area(fill = "#2d6a4f", alpha = 0.25) } +
    { if (dark) with_glow(geom_line(color = "#1b4332", linewidth = 0.5), colour = "#66cc99", sigma = 5, expand = 5)
      else geom_line(colour = "#1b4332", linewidth = 1) } +
    geom_point(colour = if (dark) "#1b4332" else "#1b4332", size = 2) +
    geom_vline(xintercept = base_year, linetype = "dashed", colour = "steelblue", linewidth = 0.8) +
    annotate("text", x = base_year + 0.2,
             y = max(yearly_stats$forest_area_ha / 1000, na.rm = TRUE) * 0.97,
             label = paste0("base year: ", base_year), hjust = 0,
             colour = "steelblue", size = 3.2) +
    #scale_x_continuous(breaks = seq(base_year, target_year, by = x_by)) +
    scale_x_continuous(breaks = seq(base_year, target_year, by = 1)) +
    labs(subtitle = paste0("baseline: ", base_year, " | canopy threshold: ", canopy_threshold, "%"),
         x = NULL, y = "forest area (000 ha)") +
    theme(panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

  # Panel 2: annual forest loss
  p2 <- ggplot(yearly_stats |> dplyr::filter(year > base_year), aes(x = year, y = annual_loss_ha / 1000)) +
    geom_col(fill = "#cc3333", alpha = 0.85) +
    { if (dark) with_glow(
        geom_smooth(method = "loess", span = 0.75, se = FALSE, colour = "#990000", linewidth = 1),
        colour = "indianred", sigma = 5, expand = 5)
      else geom_smooth(method = "loess", span = 0.75, se = FALSE, colour = "#6d0c0c", linewidth = 1) } +
    #scale_x_continuous(breaks = seq(base_year + 1, target_year, by = x_by)) +
    scale_x_continuous(breaks = seq(base_year + 1, target_year, by = 1)) +
    labs(subtitle = "loss per calendar year | trend line: LOESS smoother", x = NULL, y = "annual loss (000 ha)") +
    theme(panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

  # Panel 3: cumulative loss & net change
  loss_lab <- paste0("cumulative loss since ", base_year)
  net_lab  <- paste0("net change vs ", base_year)
  
  df3 <- yearly_stats |>
    dplyr::filter(year > base_year) |>
    dplyr::select(year, cumulative_loss_ha, net_change_ha) |>
    tidyr::pivot_longer(
      -year,
      names_to = "metric",
      values_to = "ha"
    ) |>
    dplyr::mutate(
      metric = dplyr::recode(
        metric,
        "cumulative_loss_ha" = loss_lab,
        "net_change_ha" = net_lab
      )
    )
  
  p3 <- ggplot(df3, aes(x = year, y = ha / 1000, colour = metric, linetype = metric)) +
    geom_hline(yintercept = 0, linetype = "dotted", colour = "grey50") +
    # Cumulative-loss ribbon
    geom_ribbon(
      data = dplyr::filter(df3, metric == loss_lab),
      aes(ymin = pmin(ha / 1000, 0),
        ymax = pmax(ha / 1000, 0)
      ),
      fill = "#cc3333",
      alpha = 0.30,
      colour = NA
    ) +
    # Net-change ribbon
    geom_ribbon(
      data = dplyr::filter(df3, metric == net_lab),
      aes(
        ymin = pmin(ha / 1000, 0),
        ymax = pmax(ha / 1000, 0)
      ),
      fill = "#457b9d",
      alpha = 0.30,
      colour = NA
    ) +
    # Normal lines
    geom_line(linewidth = 1) +
    # Glow around cumulative-loss line
    { if (dark) with_glow(
      geom_line(
        data = dplyr::filter(df3, metric == loss_lab),
        colour = "#cc3333",
        linewidth = 0.3
      ),
      colour = "indianred",
      sigma = 5,
      expand = 5
    ) else NULL } +
    # Glow around net-change line
    { if (dark) with_glow(
      geom_line(
        data = dplyr::filter(df3, metric == net_lab),
        colour = "#457b9d",
        linewidth = 0.3
      ),
      colour = "skyblue",
      sigma = 5,
      expand = 5
    ) else NULL } +
    # Points
    geom_point(size = 2) +
    scale_colour_manual(
      values = stats::setNames(c("#cc3333", "#3333cc"), c(loss_lab, net_lab))
    ) +
    #scale_x_continuous(breaks = seq(base_year + 1, target_year, by = x_by)) +
    scale_x_continuous(breaks = seq(base_year + 1, target_year, by = 1)) +
    labs(
      subtitle = paste0("cumulative loss & net change from ", base_year),
      x = NULL, y = "area (000 ha)", colour = NULL, linetype = NULL
    ) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "inside",
      legend.position.inside = c(0.02, 0.98),
      legend.justification = c("left", "top"),
      legend.background = element_rect(
        fill = if (dark) "black" else "white", colour = NA
      )
    )
  

  # Panel 4: % of base-year forest remaining
  p4 <- ggplot(yearly_stats, aes(x = year, y = pct_remaining)) +
    geom_ribbon(aes(ymin = pct_remaining, ymax = 100), fill = "#2d6a4f", alpha = 0.30) +
    { if (dark) with_glow(geom_line(color = "#1b4332", linewidth = 0.5), colour = "#66cc99", sigma = 5, expand = 5)
      else geom_line(colour = "#2d6a4f", linewidth = 1) } +
    geom_point(colour = "#1b4332", size = 2) +
    geom_hline(yintercept = 100, linetype = "dashed", colour = "steelblue") +
    annotate("text", x = base_year + 0.2, y = 100.5, label = paste0("100% = forest at ", base_year),
             hjust = 0, size = 3, colour = "steelblue") +
    #scale_x_continuous(breaks = seq(base_year, target_year, by = x_by)) +
    scale_x_continuous(breaks = seq(base_year, target_year, by = 1)) +
    coord_cartesian(ylim = c(NA, 102)) +
    labs(subtitle = paste0("% forest remaining vs ", base_year, " baseline"), x = "year", y = "% forest remaining") +
    theme(panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

  if (dark) {
    dark_theme_extra <- theme(
      axis.title = element_text(color = "grey80"), axis.text = element_text(color = "grey80"),
      text = element_text(colour = "grey80"), legend.text = element_text(colour = "grey80")
    )
    p1 <- p1 + theme_dark_custom() + dark_theme_extra
    p2 <- p2 + theme_dark_custom() + dark_theme_extra
    p3 <- p3 + theme_dark_custom() + dark_theme_extra
    p4 <- p4 + theme_dark_custom() + dark_theme_extra
  } else {
    p1 <- p1 + theme_minimal(base_size = 11) 
    p2 <- p2 + theme_minimal(base_size = 11)
    p3 <- p3 + theme_minimal(base_size = 11)
    p4 <- p4 + theme_minimal(base_size = 11)
  }
  
  # Force 45-degree x-axis labels on all panels
  p1 <- p1 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  p2 <- p2 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  p3 <- p3 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  p4 <- p4 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Apply this AFTER theme_dark_custom()/theme_minimal()
  # so the p3 legend remains p3's own legend.
  p3 <- p3 + theme(
    legend.position = "bottom",
    legend.justification = "left"
  )

  combined <- (p1 + p2) / (p3 + p4) +
    plot_annotation(
      #title = "Forest change trend analysis",
      # subtitle = paste0(
      #   "base year: ", base_year,
      #   "  |  target year: ", target_year,
      #   "  |  canopy threshold: \u2265", canopy_threshold, "%"
      # ),
      theme = theme(
        plot.title = element_text(
          face = "bold",
          size = 14,
          colour = if (dark) "grey90" else "black"
        ),
        plot.subtitle = element_text(
          size = 10,
          colour = if (dark) "grey60" else "grey40"
        ),
        plot.background = element_rect(
          fill = if (dark) "black" else "white",
          colour = NA
        )
      )
    )
  
  combined
  
}


# ---- Single-year % tree cover map (from plot_treecover()) ----------------------
#' @param r single-layer SpatRaster of % tree cover, ALREADY projected to EPSG:4326
#' @param aoi_wgs sf AOI in EPSG:4326
make_treecover_plot <- function(r, aoi_wgs, year_label) {
  df <- terra::as.data.frame(r, xy = TRUE, na.rm = FALSE)
  names(df) <- c("x", "y", "treecover")

  ggplot(df, aes(x = x, y = y, fill = treecover)) +
    geom_raster() +
    scale_fill_gradientn(
      name = "tree cover (%)",
      colours = c("#E1E1E1", "#FFE5AD", "#B4A022", "#61790A", "#245231", "#003B47"),
      limits = c(0, 100), breaks = seq(0, 100, 20), na.value = "transparent"
    ) +
    geom_sf(data = aoi_wgs, fill = NA, colour = scales::alpha("grey20", 0.6), linewidth = 0.6, inherit.aes = FALSE) +
    coord_sf() +
    scale_x_continuous(labels = function(x) sapply(x, function(z) sprintf("%g\u00b0%s", abs(z), ifelse(z < 0, "W", "E")))) +
    scale_y_continuous(labels = function(x) sapply(x, function(z) sprintf("%g\u00b0%s", abs(z), ifelse(z < 0, "S", "N")))) +
    labs(title = as.character(year_label), x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      legend.position = "right"
    )
}

# ---- Loss-year map (from the loss_df visualisation block) ---------------------
#' @param loss_raster single-layer SpatRaster (lossyear, 0-N code) in projected CRS
#' @param aoi_layer sf AOI in the SAME crs as loss_raster (usually UTM)
#' @param palette_type "viridis" (continuous colour ramp) or "distinct" (one
#'   visually distinct colour per year - needs randomcoloR)
#' @param legend_cols number of legend columns
#' @param max_year last year represented in the dataset (e.g. 2025)
make_loss_year_plot <- function(loss_raster, aoi_layer, palette_type = "viridis",
                                legend_cols = 5, max_year = 2025, palette_values = NULL) {
  
  loss_df <- terra::as.data.frame(loss_raster, xy = TRUE, na.rm = FALSE)
  names(loss_df) <- c("x", "y", "lossyear")
  loss_df$year <- ifelse(loss_df$lossyear == 0, NA, loss_df$lossyear + 2000)
  loss_df$year <- factor(loss_df$year, levels = 2001:max_year)
  
  p <- ggplot(loss_df, aes(x = x, y = y, fill = year)) + geom_raster()
  
  # Use the shared palette supplied by the server.
  # This ensures the static map uses exactly the same colours as the
  # interactive Leaflet map.
  
  if (!is.null(palette_values)) {
    p <- p +
      scale_fill_manual(
        name = "Loss year",
        values = palette_values,
        limits = as.character(2001:max_year),
        breaks = as.character(2001:max_year),
        drop = FALSE,
        na.value = "white",
        na.translate = FALSE
      )
  } else {
    # Fallback only. Normally the server will always supply palette_values.
    p <- p +
      scale_fill_viridis_d(
        name = "Loss year",
        limits = as.character(2001:max_year),
        breaks = as.character(2001:max_year),
        drop = FALSE,
        na.value = "grey90"
      )
  }
  
  p <- p +
    scale_x_continuous(
      labels = function(x) {
        sapply(
          x,
          function(z) { sprintf("%g\u00b0%s", abs(z), ifelse(z < 0, "W", "E")) }
        )
      }
    ) +
    scale_y_continuous(
      labels = function(x) {
        sapply(
          x,
          function(z) { sprintf("%g\u00b0%s", abs(z), ifelse(z < 0, "S", "N")) }
        )
      }
    ) +
    coord_sf() +
    labs(x = NULL, y = NULL) +
    geom_sf(
      data = aoi_layer,
      fill = NA,
      colour = scales::alpha("grey50", 0.95),
      linewidth = 0.6,
      inherit.aes = FALSE
    ) +
    guides(fill = guide_legend(ncol = legend_cols, byrow = FALSE)) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.key.size = unit(0.5, "cm"),
      legend.spacing.x = unit(0.15, "cm")
    )
  p
}


# ---- Multi-year facet builders (forest cover & annual loss) -------------------
# (Per-year data.frames are now built directly inside the facet_btn observer,
# in the same loop iteration each SpatRaster is created in - see the note
# there about why. make_forest_facet_plot()/make_loss_facet_plot() below just
# take the assembled data frames.)
make_forest_facet_plot <- function(raster_df, aoi_layer, canopy_threshold, n_cols = 8) {
  raster_df$forest <- ifelse(is.na(raster_df$value), NA,
                              ifelse(raster_df$value >= 0.5, "forest", "non-forest"))
  raster_df$forest <- factor(raster_df$forest, levels = c("non-forest", "forest"))

  n_years <- length(unique(raster_df$year))
  n_cols <- min(n_cols, n_years)

  ggplot(raster_df |> dplyr::filter(!is.na(value)), aes(x = x, y = y, fill = forest)) +
    geom_raster() +
    facet_wrap(~year, ncol = n_cols) +
    scale_fill_manual(values = c("non-forest" = "#c8a96e", "forest" = "#006633"), na.value = "white", name = NULL) +
    geom_sf(data = aoi_layer, fill = NA, colour = scales::alpha("black", 0.5), linewidth = 0.6, inherit.aes = FALSE) +
    coord_sf() +
    labs(subtitle = paste0("canopy threshold: \u2265", canopy_threshold, "%"), x = "", y = "") +
    theme_minimal(base_size = 11) +
    theme(
      plot.subtitle = element_text(colour = "grey40", size = 9),
      legend.position = "bottom", axis.text = element_text(size = 7),
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(face = "bold", size = 9),
      panel.spacing = unit(0.35, "lines")
    )
}

make_loss_facet_plot <- function(loss_df, aoi_layer, canopy_threshold, n_cols = 5) {
  loss_df$loss_class <- ifelse(is.na(loss_df$value), NA,
                                ifelse(loss_df$value >= 0.5, "loss", "no loss"))
  loss_df$loss_class <- factor(loss_df$loss_class, levels = c("no loss", "loss"))

  n_years <- length(unique(loss_df$year))
  n_cols <- min(n_cols, n_years)

  ggplot(loss_df |> dplyr::filter(!is.na(value)), aes(x = x, y = y, fill = loss_class)) +
    geom_raster() +
    facet_wrap(~year, ncol = n_cols) +
    scale_fill_manual(values = c("no loss" = "#d4e6c3", "loss" = "#b5152b"), na.value = "white", name = NULL) +
    geom_sf(data = aoi_layer, fill = NA, colour = "black", linewidth = 0.9, inherit.aes = FALSE) +
    coord_sf() +
    labs(
      subtitle = paste0("red = pixels lost in that specific year  |  canopy threshold: \u2265", canopy_threshold, "%"),
      x = "", y = ""
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.subtitle = element_text(colour = "grey40", size = 9),
      legend.position = "bottom", axis.text = element_text(size = 7),
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(face = "bold", size = 9),
      panel.spacing = unit(0.35, "lines")
    )
}

# ---- misc small helpers ---------------------------------------------------------
gfc_dataset_year <- function(dataset) as.integer(stringr::str_extract(dataset, "(?<=GFC-?)[0-9]{4}"))

# Zip a folder's contents into a single file - used for "download all annual layers".
# Prefers the pure-R 'zip' package (no external dependency); falls back to
# utils::zip(), which relies on a 'zip' command-line utility being on PATH.
zip_folder <- function(folder, zip_path) {
  files <- list.files(folder, full.names = FALSE)
  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zip(zipfile = zip_path, files = files, root = folder)
  } else {
    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(folder)
    utils::zip(zipfile = zip_path, files = files)
  }
  zip_path
}

# ---- circular progress modal helpers -------------------------------------------
# A small, reusable "circle progress bar" (an animated SVG ring that fills as
# work proceeds, with a live percentage in the middle) shown in a modal while
# a long-running download/computation runs. gfc_progress_open() shows it,
# gfc_progress_update() moves the ring (call this from the same progress_fun()
# callbacks already threaded through the helper functions above), and
# gfc_progress_close() removes it. Because Shiny flushes Progress/Modal
# websocket messages immediately (not only at the end of the reactive tick),
# the ring visibly animates while the (single-threaded, blocking) R code runs.
# gfc_progress_open <- function(title = "Working...", detail = "") {
#   showModal(modalDialog(
#     title = NULL, footer = NULL, easyClose = FALSE, size = "s",
#     div(
#       class = "gfc-progress-wrap",
#       tags$svg(
#         class = "gfc-progress-ring", viewBox = "0 0 120 120",
#         tags$circle(class = "gfc-progress-ring-bg", cx = 60, cy = 60, r = 54),
#         tags$circle(id = "gfc-progress-ring-fg", class = "gfc-progress-ring-fg", cx = 60, cy = 60, r = 54)
#       ),
#       div(id = "gfc-progress-pct", class = "gfc-progress-pct", "0%"),
#       h4(class = "gfc-progress-title", title),
#       p(id = "gfc-progress-detail", class = "gfc-progress-detail", detail)
#     )
#   ))
# }
# 
# gfc_progress_update <- function(session, frac, detail = NULL) {
#   session$sendCustomMessage("gfcProgress", list(
#     pct = round(100 * max(0, min(1, frac))),
#     detail = if (is.null(detail)) "" else as.character(detail)
#   ))
# }
# 
# gfc_progress_close <- function() {
#   removeModal()
# }

#' gfc_progress_open <- function(title = "Working...", detail = "") {
#'   
#'   showModal(
#'     modalDialog(
#'       title = NULL,
#'       footer = NULL,
#'       easyClose = FALSE,
#'       size = "s",
#'       
#'       tags$style(HTML("
#'         .gfc-progress-wrap {
#'           text-align: center;
#'           padding: 18px 20px 20px 20px;
#'         }
#' 
#'         .gfc-progress-ring-container {
#'           position: relative;
#'           width: 120px;
#'           height: 120px;
#'           margin: 0 auto 12px auto;
#'         }
#' 
#'         .gfc-progress-ring {
#'           width: 120px;
#'           height: 120px;
#'           display: block;
#'         }
#' 
#'         .gfc-progress-ring-fg {
#'           transform-origin: 60px 60px;
#'           animation: gfcSpin 1.2s linear infinite;
#'         }
#' 
#'         .gfc-progress-pct {
#'           position: absolute;
#'           top: 50%;
#'           left: 50%;
#'           transform: translate(-50%, -50%);
#'           font-family: 'IBM Plex Mono', monospace;
#'           font-size: 20px;
#'           font-weight: 600;
#'           color: #3fb950;
#'           line-height: 1;
#'         }
#' 
#'         .gfc-progress-title {
#'           margin: 8px 0 8px 0;
#'           font-size: 14px;
#'           line-height: 1.3;
#'         }
#' 
#'         .gfc-progress-detail {
#'           margin: 0;
#'           font-family: 'IBM Plex Mono', monospace;
#'           font-size: 11px;
#'           color: #8b949e;
#'           line-height: 1.4;
#'         }
#' 
#'         @keyframes gfcSpin {
#'           from {
#'             transform: rotate(0deg);
#'           }
#'           to {
#'             transform: rotate(360deg);
#'           }
#'         }
#'       ")),
#'       
#'       div(
#'         class = "gfc-progress-wrap",
#'         
#'         # ---- Spinning circle + percentage ----
#'         div(
#'           class = "gfc-progress-ring-container",
#'           
#'           tags$svg(
#'             class = "gfc-progress-ring",
#'             viewBox = "0 0 120 120",
#'             
#'             tags$circle(
#'               class = "gfc-progress-ring-bg",
#'               cx = 60,
#'               cy = 60,
#'               r = 54
#'             ),
#'             
#'             tags$circle(
#'               id = "gfc-progress-ring-fg",
#'               class = "gfc-progress-ring-fg",
#'               cx = 60,
#'               cy = 60,
#'               r = 54
#'             )
#'           ),
#'           
#'           # Percentage stays in centre of circle
#'           div(
#'             id = "gfc-progress-pct",
#'             class = "gfc-progress-pct",
#'             "0%"
#'           )
#'         ),
#'         
#'         # ---- Message BELOW the circle ----
#'         h4(
#'           class = "gfc-progress-title",
#'           title
#'         ),
#'         
#'         p(
#'           id = "gfc-progress-detail",
#'           class = "gfc-progress-detail",
#'           detail
#'         )
#'       )
#'     )
#'   )
#' }

gfc_progress_open <- function(title = "Working...", detail = "") {
  
  showModal(
    modalDialog(
      title = NULL,
      footer = NULL,
      easyClose = FALSE,
      size = "s",
      
      tags$style(HTML("
        .gfc-progress-wrap {
          text-align: center;
          padding: 18px 20px 20px 20px;
        }

        /* Dedicated area for the circle */
        .gfc-progress-ring-container {
          position: relative !important;
          width: 120px !important;
          height: 120px !important;
          margin: 0 auto 14px auto !important;
        }

        .gfc-progress-ring {
          position: absolute !important;
          top: 0 !important;
          left: 0 !important;
          width: 120px !important;
          height: 120px !important;
          display: block !important;
        }

        /* Spinning foreground ring */
        .gfc-progress-ring-fg {
          transform-origin: 60px 60px;
          animation: gfcSpin 1.2s linear infinite;
        }

        /* Percentage INSIDE the circle */
        .gfc-progress-pct {
          position: absolute !important;
          top: 60px !important;
          left: 60px !important;
          transform: translate(-50%, -50%) !important;
          z-index: 10 !important;

          margin: 0 !important;
          padding: 0 !important;

          font-family: 'IBM Plex Mono', monospace !important;
          font-size: 20px !important;
          font-weight: 600 !important;
          color: #3fb950 !important;
          line-height: 1 !important;
          width: auto !important;
          height: auto !important;
        }

        .gfc-progress-title {
          margin: 8px 0 8px 0 !important;
          font-size: 14px !important;
          line-height: 1.3 !important;
        }

        .gfc-progress-detail {
          margin: 0 !important;
          font-family: 'IBM Plex Mono', monospace !important;
          font-size: 11px !important;
          color: #8b949e !important;
          line-height: 1.4 !important;
        }

        @keyframes gfcSpin {
          from {
            transform: rotate(0deg);
          }
          to {
            transform: rotate(360deg);
          }
        }
      ")),
      
      div(
        class = "gfc-progress-wrap",
        
        # Circle + percentage
        div(
          class = "gfc-progress-ring-container",
          
          tags$svg(
            class = "gfc-progress-ring",
            viewBox = "0 0 120 120",
            
            tags$circle(
              class = "gfc-progress-ring-bg",
              cx = 60,
              cy = 60,
              r = 54
            ),
            
            tags$circle(
              id = "gfc-progress-ring-fg",
              class = "gfc-progress-ring-fg",
              cx = 60,
              cy = 60,
              r = 54
            )
          ),
          
          # THIS IS INSIDE THE CIRCLE
          div(
            id = "gfc-progress-pct",
            class = "gfc-progress-pct",
            "0%"
          )
        ),
        
        # Message below circle
        h4(
          class = "gfc-progress-title",
          title
        ),
        
        p(
          id = "gfc-progress-detail",
          class = "gfc-progress-detail",
          detail
        )
      )
    )
  )
}


gfc_progress_update <- function(session, frac, detail = NULL) {
  session$sendCustomMessage(
    "gfcProgress",
    list(
      pct = round(
        100 * max(0, min(1, frac))
      ),
      detail = if (is.null(detail)) {
        ""
      } else {
        as.character(detail)
      }
    )
  )
}

gfc_progress_close <- function() {
  removeModal()
}

