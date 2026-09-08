#' Dashboard server (internal)
#'
#' Defines the server logic for the app. Not exported; called only by
#' \code{\link{run_gfc_dashboard}}.
#'
#' @param input,output,session Standard Shiny server arguments.
#' @keywords internal
#'
#' @import shiny
#' @import shinyjs
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import patchwork
#' @import sf
#' @import terra
#' @import tidyterra
#' @import mapgl
#' @import leaflet
#' @import leafem
#' @import DT
#' @import scales
#' @import animation
#' @import jsonlite
#' @import zip
################################################################################
# SECTION 4: SERVER
################################################################################

gfc_app_server <- function(input, output, session) {

  # Set per-launch (not per-package-build) - safe to call every session.
  options(shiny.maxRequestSize = 200 * 1024^2)

  # ---- per-session temp workspace ------------------------------------------
  # Everything this app writes (merged rasters, thresholded rasters, annual
  # layers, plots, animations) lives here. Nothing is ever hard-coded to a
  # fixed path, and this folder is deleted automatically when the session ends.
  session_dir <- tempfile(pattern = "gfc_session_")
  dir.create(session_dir)
  session$onSessionEnded(function() {
    unlink(session_dir, recursive = TRUE)
  })

  rv <- reactiveValues(
    aoi = NULL, aoi_wgs = NULL, aoi_utm = NULL, aoi_native = NULL, utm_epsg_code = NULL,
    aoi_loaded = FALSE,

    gfc_raw = NULL,             # extract_gfc() output (4 layers)
    gfc_thresholded = NULL,     # threshold_gfc() output (5 layers)
    pixel_area_ha = NULL,
    stats = NULL,               # gfc_stats() output
    extract_done = FALSE,
    thresholded_path = NULL,

    yearly_stats = NULL,
    trend_done = FALSE,

    gfc_annual_stack = NULL,
    annual_stack_done = FALSE,
    annual_layers_dir = NULL,

    treecover_native = NULL,    # current single-year treecover raster (native CRS)
    treecover_ll_df = NULL,
    treecover_year_selected = NULL,
    treecover_done = FALSE,

    treecover_compare_plot = NULL,
    treecover_compare_done = FALSE,

    loss_year_plot_obj = NULL,
    loss_year_done = FALSE,

    yearly_rasters = NULL,
    forest_facet_plot_obj = NULL,
    loss_facet_plot_obj = NULL,
    facet_done = FALSE,

    animation_path = NULL,
    animation_type = NULL,
    animation_done = FALSE
  )
  
  safe_clear <- function(map, id) {
    tryCatch(clear_layer(map, id), error = function(e) map)
  }
  
  ####
  bbox_to_zoom <- function(bbox, padding_factor = 1.4) {
    width  <- as.numeric(bbox["xmax"] - bbox["xmin"])
    height <- as.numeric(bbox["ymax"] - bbox["ymin"])
    span   <- max(width, height, 0.0001) * padding_factor
    zoom   <- floor(log2(360 / span))
    min(max(zoom, 1), 18)
  }
  
  ####
  optimal_ncol <- function(n, max_col = 3) {
    if (n <= 1) return(1)
    
    min_col <- ceiling(sqrt(n))                    # never go narrower than square
    max_col <- max(min_col, min(max_col, n))        # respect the cap, but allow min_col to override it if needed
    
    candidates <- min_col:max_col
    rows       <- ceiling(n / candidates)
    wasted     <- candidates * rows - n             # empty grid cells for each candidate
    
    best <- candidates[wasted == min(wasted)]
    max(best)                                        # on a tie, prefer more columns (landscape)
  }
  
  ##############################################################################
  # TAB 1: UPLOAD AOI
  ##############################################################################
  ####
  observeEvent(input$load_aoi, {
    req(input$shapefile)
    
    withProgress(message = "Loading shapefile...", value = 0, {
      tryCatch({
        infiles <- input$shapefile$datapath
        names(infiles) <- input$shapefile$name
        shp_file <- infiles[grep("\\.shp$", names(infiles), ignore.case = TRUE)]
        if (length(shp_file) == 0) {
          showNotification("No .shp file found among the uploaded files.", type = "error")
          return(NULL)
        }
        
        aoi_upload_dir <- file.path(session_dir, "aoi_upload")
        dir.create(aoi_upload_dir, showWarnings = FALSE)
        base_name <- tools::file_path_sans_ext(basename(names(shp_file)))
        for (i in seq_along(infiles)) {
          ext <- tools::file_ext(names(infiles)[i])
          new_path <- file.path(aoi_upload_dir, paste0(base_name, ".", ext))
          file.copy(infiles[i], new_path, overwrite = TRUE)
        }
        
        incProgress(0.3, detail = "Reading shapefile...")
        shp_path <- file.path(aoi_upload_dir, paste0(base_name, ".shp"))
        aoi <- sf::st_read(shp_path, quiet = TRUE)
        
        incProgress(0.4, detail = "Checking AOI geometry...")
        aoi <- sf::st_zm(aoi)
        valid <- sf::st_is_valid(aoi)
        if (any(!valid, na.rm = TRUE)) {
          n_invalid <- sum(!valid, na.rm = TRUE)
          showNotification(sprintf("AOI contains %d invalid geometry feature(s). Attempting to repair them...", n_invalid),
                           type = "warning", duration = 8)
          aoi <- sf::st_make_valid(aoi)
        }
        valid_after <- sf::st_is_valid(aoi)
        if (any(!valid_after, na.rm = TRUE)) {
          stop("Some AOI geometries could not be repaired.")
        }
        
        if (is.na(sf::st_crs(aoi))) {
          showNotification("Shapefile has no CRS defined - assuming WGS84 (EPSG:4326).", type = "warning", duration = 8)
          sf::st_crs(aoi) <- 4326
        }
        aoi_wgs <- sf::st_transform(aoi, crs = 4326)
        
        incProgress(0.6, detail = "Determining local UTM zone...")
        ctr <- sf::st_coordinates(sf::st_centroid(sf::st_as_sfc(sf::st_bbox(aoi_wgs))))
        epsg <- utm_epsg(ctr[1, "X"], ctr[1, "Y"])
        aoi_utm <- sf::st_transform(aoi_wgs, paste0("EPSG:", epsg))
        
        rv$aoi <- aoi
        rv$aoi_wgs <- aoi_wgs
        rv$aoi_utm <- aoi_utm
        rv$utm_epsg_code <- epsg
        rv$aoi_loaded <- TRUE
        
        bbox <- sf::st_bbox(aoi_wgs)
        ctr_lng <- mean(c(bbox[["xmin"]], bbox[["xmax"]]))
        ctr_lat <- mean(c(bbox[["ymin"]], bbox[["ymax"]]))
        zoom_lvl <- bbox_to_zoom(bbox, padding_factor = 1.4)
        
        maplibre_proxy("aoi_map") |>
          safe_clear("aoi-fill") |>
          safe_clear("aoi-outline") |>
          add_fill_layer(id = "aoi-fill", source = aoi_wgs, fill_color = "#FF0000", fill_opacity = 0.1) |>
          add_line_layer(id = "aoi-outline", source = aoi_wgs, line_color = "#FF0000", line_width = 2) |>
          fly_to(center = c(ctr_lng, ctr_lat), zoom = zoom_lvl, duration = 6000, essential = TRUE, curve = 1.6)
        
        incProgress(1, detail = "Complete!")
        showNotification(sprintf("AOI loaded successfully! Local UTM zone: EPSG:%d", epsg), type = "message", duration = 4)
        
      }, error = function(e) {
        showNotification(paste("Error loading shapefile:", e$message), type = "error", duration = 10)
      })
    })
  })
  
  ####
  observeEvent(input$use_drawn_aoi, {
    withProgress(message = "Using drawn polygon...", value = 0, {
      tryCatch({
        incProgress(0.2, detail = "Reading drawn feature...")
        aoi <- get_drawn_features(maplibre_proxy("aoi_map"))
        
        if (is.null(aoi) || nrow(aoi) == 0) {
          showNotification("No polygon drawn yet. Use the polygon or rectangle tool on the globe first.",
                           type = "error", duration = 6)
          return(NULL)
        }
        
        incProgress(0.4, detail = "Checking AOI geometry...")
        aoi <- sf::st_zm(aoi)
        valid <- sf::st_is_valid(aoi)
        if (any(!valid, na.rm = TRUE)) {
          aoi <- sf::st_make_valid(aoi)
        }
        if (any(!sf::st_is_valid(aoi), na.rm = TRUE)) {
          stop("The drawn geometry could not be repaired.")
        }
        
        if (is.na(sf::st_crs(aoi))) {
          sf::st_crs(aoi) <- 4326   # drawn features are always WGS84 already, this is just a safety net
        }
        aoi_wgs <- sf::st_transform(aoi, crs = 4326)
        
        incProgress(0.6, detail = "Determining local UTM zone...")
        ctr <- sf::st_coordinates(sf::st_centroid(sf::st_as_sfc(sf::st_bbox(aoi_wgs))))
        epsg <- utm_epsg(ctr[1, "X"], ctr[1, "Y"])
        aoi_utm <- sf::st_transform(aoi_wgs, paste0("EPSG:", epsg))
        
        rv$aoi <- aoi
        rv$aoi_wgs <- aoi_wgs
        rv$aoi_utm <- aoi_utm
        rv$utm_epsg_code <- epsg
        rv$aoi_loaded <- TRUE
        
        bbox <- sf::st_bbox(aoi_wgs)
        ctr_lng <- mean(c(bbox[["xmin"]], bbox[["xmax"]]))
        ctr_lat <- mean(c(bbox[["ymin"]], bbox[["ymax"]]))
        zoom_lvl <- bbox_to_zoom(bbox, padding_factor = 1.4)
        
        maplibre_proxy("aoi_map") |>
          safe_clear("aoi-fill") |>
          safe_clear("aoi-outline") |>
          add_fill_layer(id = "aoi-fill", source = aoi_wgs, fill_color = "#FF0000", fill_opacity = 0.1) |>
          add_line_layer(id = "aoi-outline", source = aoi_wgs, line_color = "#FF0000", line_width = 2) |>
          fly_to(center = c(ctr_lng, ctr_lat), zoom = zoom_lvl, duration = 4000, essential = TRUE, curve = 1.4)
        
        incProgress(1, detail = "Complete!")
        showNotification(sprintf("Drawn polygon set as AOI! Local UTM zone: EPSG:%d", epsg), type = "message", duration = 4)
        
      }, error = function(e) {
        showNotification(paste("Error using drawn polygon:", e$message), type = "error", duration = 10)
      })
    })
  })
  
  ####
  observeEvent(input$clear_drawn_aoi, {
    maplibre_proxy("aoi_map") |>
      safe_clear("aoi-fill") |>
      safe_clear("aoi-outline") |>
      clear_drawn_features()
    
    rv$aoi <- NULL
    rv$aoi_wgs <- NULL
    rv$aoi_utm <- NULL
    rv$utm_epsg_code <- NULL
    rv$aoi_loaded <- FALSE
    
    showNotification("Drawn AOI cleared. Draw a new polygon or upload a shapefile.", type = "message", duration = 4)
  })

  observeEvent(input$clear_aoi, {
    showModal(modalDialog(
      title = "Confirm Clear All Data",
      "Are you sure you want to clear all data? This resets the entire analysis.",
      footer = tagList(modalButton("Cancel"), actionButton("confirm_clear", "Yes, Clear All", class = "btn-danger"))
    ))
  })

  observeEvent(input$confirm_clear, {
    for (nm in names(rv)) rv[[nm]] <- NULL
    rv$aoi_loaded <- FALSE
    rv$extract_done <- FALSE
    rv$trend_done <- FALSE
    rv$annual_stack_done <- FALSE
    rv$treecover_done <- FALSE
    rv$treecover_compare_done <- FALSE
    rv$loss_year_done <- FALSE
    rv$facet_done <- FALSE
    rv$animation_done <- FALSE
     
    ####
    maplibre_proxy("aoi_map") |>
      safe_clear("aoi-fill") |>
      safe_clear("aoi-outline") |>
      clear_drawn_features() |>
      fly_to(center = c(0, 20), zoom = 1, duration = 4000, essential = TRUE)

    # Also free the physical temp files written so far this session (downloaded
    # tiles, thresholded rasters, annual layers, animation frames, etc.)
    tryCatch({
      old_files <- list.files(session_dir, full.names = TRUE)
      if (length(old_files) > 0) unlink(old_files, recursive = TRUE, force = TRUE)
    }, error = function(e) NULL)

    # NOTE: the bare call `reset("shapefile")` used to crash the app with
    # "unable to find an inherited method for function 'reset' for signature
    # x = 'character'" - some of the spatial packages loaded above (e.g. terra)
    # define their own S4 generic called `reset()`, which was shadowing
    # shinyjs::reset() and being dispatched on the plain character input id
    # instead. Namespacing the call fixes it.
    tryCatch(shinyjs::reset("shapefile"), error = function(e) {
      showNotification(paste("Could not reset the file input:", e$message), type = "warning", duration = 6)
    })

    removeModal()
    showNotification("All data cleared.", type = "message", duration = 3)
  })

  output$aoi_info <- renderPrint({
    req(rv$aoi_wgs, rv$aoi_utm)
    
    area_m2 <- sum(sf::st_area(rv$aoi_utm))
    area_ha  <- units::set_units(area_m2, "ha")
    area_km2 <- units::set_units(area_m2, "km^2")
    
    cat("Number of features:", nrow(rv$aoi_wgs), "\n")
    cat("Original CRS (reprojected to WGS84 for display):", "EPSG:4326", "\n")
    cat("Local UTM zone for area-based analysis: EPSG:", rv$utm_epsg_code, "\n")
    cat("Total AOI area:", format(area_ha, digits = 4), "  (", format(area_km2, digits = 4), ")\n")
    cat("Bounding box (WGS84):\n")
    print(sf::st_bbox(rv$aoi_wgs))
  })

  output$aoi_map <- renderMaplibre({
    maplibre(style = "https://demotiles.maplibre.org/style.json", projection = "globe", zoom = 1.75, center = c(25, 10)) |>
      add_raster_source(
        id = "esri-satellite-src",
        tiles = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
        tileSize = 256,
        maxzoom = 19
      ) |>
      add_raster_layer(id = "satellite", source = "esri-satellite-src", visibility = "visible") |>
      add_raster_source(
        id = "osm-src",
        tiles = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
        tileSize = 256,
        maxzoom = 19
      ) |>
      add_raster_layer(id = "osm", source = "osm-src", visibility = "none") |>
      add_globe_minimap() |>
      ####
      add_draw_control(
        position = "top-left",
        rectangle = TRUE,
        freehand = TRUE,
        fill_color = "#FF0000",
        fill_opacity = 0.1,
        line_color = "#FF0000",
        controls = list(
          point = FALSE,
          line_string = FALSE,
          combine_features = FALSE,
          uncombine_features = FALSE,
          trash = TRUE,
          polygon = TRUE
        )
      )
  })
  
  observeEvent(input$basemap_choice, {
    maplibre_proxy("aoi_map") |>
      set_layout_property("satellite", "visibility",
                          if (input$basemap_choice == "satellite") "visible" else "none") |>
      set_layout_property("osm", "visibility",
                          if (input$basemap_choice == "osm") "visible" else "none")
  })

  output$aoi_loaded <- reactive({ rv$aoi_loaded })
  outputOptions(output, "aoi_loaded", suspendWhenHidden = FALSE)

  ##############################################################################
  # TAB 2: EXTRACT & THRESHOLD
  ##############################################################################
  observeEvent(input$extract_btn, {
    req(rv$aoi_wgs)

    gfc_progress_open("Downloading & processing GFC tiles...", "Starting...")
    on.exit(gfc_progress_close(), add = TRUE)

    tryCatch({
      gfc_progress_update(session, 0.05, "Calculating required tiles...")
      gfc_raw <- extract_gfc(
        aoi = rv$aoi_wgs,
        to_UTM = isTRUE(input$to_utm),
        dataset = input$dataset,
        keep_tiles = FALSE,
        progress_fun = function(frac, detail = NULL) gfc_progress_update(session, 0.05 + frac * 0.45, detail)
      )
      rv$gfc_raw <- gfc_raw
      rv$aoi_native <- if (isTRUE(input$to_utm)) rv$aoi_utm else rv$aoi_wgs

      gfc_progress_update(session, 0.6, "Thresholding into forest/loss/gain classes...")
      thresh_path <- file.path(session_dir, sprintf("gfc_thresholded_%dpct.tif", input$forest_threshold))
      gfc_thresholded <- threshold_gfc(gfc_raw, forest_threshold = input$forest_threshold,
                                        filename = thresh_path, overwrite = TRUE)
      rv$gfc_thresholded <- gfc_thresholded
      rv$thresholded_path <- thresh_path

      gfc_progress_update(session, 0.75, "Calculating pixel areas...")
      rv$pixel_area_ha <- terra::cellSize(gfc_raw[["treecover2000"]], unit = "ha")

      gfc_progress_update(session, 0.85, "Computing annual loss/cover statistics...")
      stats <- gfc_stats(aoi = sf::st_zm(rv$aoi_native), gfc = gfc_thresholded, dataset = input$dataset)
      rv$stats <- stats

      rv$extract_done <- TRUE
      gfc_progress_update(session, 1, "Complete!")
      showNotification("GFC data extracted and thresholded successfully!", type = "message", duration = 5)
    }, error = function(e) {
      showNotification(paste("Error during extraction:", e$message), type = "error", duration = 12)
    })
  })

  output$extract_done <- reactive({ rv$extract_done })
  outputOptions(output, "extract_done", suspendWhenHidden = FALSE)

  output$loss_table <- renderDT({
    req(rv$stats)
    dat <- rv$stats$loss_table
    dat$cover <- round(dat$cover, 1)
    dat$loss <- round(dat$loss, 1)
    datatable(dat, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$download_loss_csv <- downloadHandler(
    filename = function() paste0("gfc_loss_cover_table_", Sys.Date(), ".csv"),
    content = function(file) write.csv(rv$stats$loss_table, file, row.names = FALSE)
  )

  output$download_thresholded_raster <- downloadHandler(
    filename = function() basename(rv$thresholded_path),
    content = function(file) file.copy(rv$thresholded_path, file, overwrite = TRUE)
  )

  output$cover_loss_plot_dark <- renderPlot({
    req(rv$stats)
    dat <- rv$stats$loss_table
    base_yr <- min(dat$year)
    target_yr <- max(dat$year)
    make_cover_loss_plot(dat, base_yr, target_yr, dark = TRUE)
  }, bg = "black")

  output$download_cover_loss_plot <- downloadHandler(
    filename = function() paste0("gfc_cover_vs_loss_", Sys.Date(), ".png"),
    content = function(file) {
      dat <- rv$stats$loss_table
      base_yr <- min(dat$year)
      target_yr <- max(dat$year)
      p <- make_cover_loss_plot(dat, base_yr, target_yr, dark = FALSE)
      ggsave(file, p, width = 9, height = 6, dpi = 200, bg = "white")
    }
  )
  
  ##############################################################################
  # TAB 3: YEARLY TREND ANALYSIS
  ##############################################################################
  observeEvent(input$trend_btn, {
    req(rv$gfc_raw, rv$pixel_area_ha)
    # Explicitly use Shiny's validate() and need()
    shiny::validate(
      shiny::need(
        input$target_year_trend > input$base_year,
        "Target year must be after the base year."
      )
    )
    
    # Open custom progress modal
    gfc_progress_open(
      title = "Computing Yearly Forest Trends",
      detail = "Starting yearly forest change analysis..."
    )
    tryCatch({
      # Compute yearly statistics
      yearly_stats <- compute_yearly_stats(
        tree_r = rv$gfc_raw[["treecover2000"]],
        loss_r = rv$gfc_raw[["lossyear"]],
        gain_r = rv$gfc_raw[["gain"]],
        base_year = input$base_year,
        target_year = input$target_year_trend,
        pixel_area_ha = rv$pixel_area_ha,
        threshold = input$forest_threshold,
        # Send progress to our custom modal
        progress_fun = function(frac, detail = NULL) {
          gfc_progress_update(
            session = session,
            frac = frac,
            detail = detail
          )
        }
      )
      
      # Store results
      rv$yearly_stats <- yearly_stats
      rv$trend_done <- TRUE
      
      # Set progress to 100% before closing
      gfc_progress_update(
        session = session,
        frac = 1,
        detail = "Yearly trend analysis complete."
      )
      
      # Small notification after successful completion
      showNotification(
        "Yearly trend analysis complete!",
        type = "message",
        duration = 4
      )
      
    }, error = function(e) {
      # Report error
      showNotification(
        paste(
          "Error computing yearly trends:",
          e$message
        ),
        type = "error",
        duration = 12
      )
    }, finally = {
      # ALWAYS close the progress modal
      # This runs whether the calculation succeeds or fails.
      gfc_progress_close()
      
    })
  })
  
  # YEARLY TREND ANALYSIS - COMPLETION STATUS
  output$trend_done <- reactive({
    rv$trend_done
  })

  outputOptions(output, "trend_done", suspendWhenHidden = FALSE)
  
  # YEARLY TREND ANALYSIS - VALUE BOXES
  output$vb_base_area <- renderValueBox({
    req(rv$yearly_stats)
    base_row <- rv$yearly_stats[rv$yearly_stats$year == input$base_year, ]
    valueBox(
      paste0(
        format(
          round(base_row$forest_area_ha),
          big.mark = ","
        ),
        " ha"
      ),
      paste(
        "Forest area,",
        input$base_year
      ),
      icon = icon("tree"),
      color = "green"
    )
  })
  
  output$vb_target_area <- renderValueBox({
    req(rv$yearly_stats)
    target_row <- rv$yearly_stats[rv$yearly_stats$year == input$target_year_trend, ]
    
    valueBox(
      paste0(
        format(
          round(target_row$forest_area_ha),
          big.mark = ","
        ),
        " ha"
      ),
      paste(
        "Forest area,",
        input$target_year_trend
      ),
      icon = icon("tree"),
      color = "blue"
    )
  })
  
  output$vb_net_change <- renderValueBox({
    req(rv$yearly_stats)
    target_row <- rv$yearly_stats[rv$yearly_stats$year == input$target_year_trend, ]
    
    net <- target_row$net_change_ha
    valueBox(
      paste0(
        ifelse(net >= 0, "+", ""),
        format(
          round(net),
          big.mark = ","
        ),
        " ha"
      ),
      "Net change since base year",
      icon = icon(
        if (net >= 0) {
          "arrow-up"
        } else {
          "arrow-down"
        }
      ),
      color = if (net >= 0) "green" else "red"
    )
  })
  
  # YEARLY STATISTICS TABLE
  output$yearly_stats_table <- renderDT({
    req(rv$yearly_stats)
    datatable(
      rv$yearly_stats,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  # DOWNLOAD YEARLY STATISTICS CSV
  output$download_yearly_csv <- downloadHandler(
    filename = function() {
      paste0(
        "gfc_yearly_stats_",
        input$base_year,
        "_",
        input$target_year_trend,
        ".csv"
      )
    },
    content = function(file) {
      write.csv(rv$yearly_stats, file, row.names = FALSE)
    }
  )
  
  # YEARLY TREND PLOT - DARK DISPLAY VERSION
  output$trend_panels_dark <- renderPlot({
    req(rv$yearly_stats)
    make_trend_panels(
      rv$yearly_stats,
      input$base_year,
      input$target_year_trend,
      input$forest_threshold,
      dark = TRUE
    )
  }, bg = "black")
  
  # DOWNLOAD YEARLY TREND PLOT
  output$download_trend_plot <- downloadHandler(
    filename = function() {
      paste0(
        "gfc_trend_panels_",
        input$base_year,
        "_",
        input$target_year_trend,
        ".png"
      )
    },
    content = function(file) {
      p <- make_trend_panels(
        rv$yearly_stats,
        input$base_year,
        input$target_year_trend,
        input$forest_threshold,
        dark = FALSE
      )
      ggsave(
        file,
        p,
        width = 14,
        height = 10,
        dpi = 150,
        bg = "white"
      )
    }
  )
  
  ##############################################################################
  # TAB 4a: CLASSIFIED CHANGE MAP
  ##############################################################################
  observeEvent(input$build_annual_stack_btn, {
    
    req(rv$gfc_thresholded)
    
    # Validate inputs
    shiny::validate(
      shiny::need(
        !is.null(input$dataset) && nzchar(as.character(input$dataset)),
        "Please select a dataset."
      )
    )
  
    # Open custom progress modal
    gfc_progress_open(
      title = "Building Annual Classified Stack",
      detail = "Starting annual stack calculation..."
    )
    
    tryCatch({
      # Build annual classified stack
      gfc_annual_stack <- annual_stack(
        rv$gfc_thresholded,
        
        dataset = input$dataset,
        progress_fun = function(frac, detail = NULL) {
          gfc_progress_update(session = session, frac = frac, detail = detail)
        }
      )
      
      # Store result
      rv$gfc_annual_stack <- gfc_annual_stack
      rv$annual_stack_done <- TRUE
      
      # Final progress update
      gfc_progress_update(
        session = session, frac = 1, detail = "Annual classified stack built successfully."
      )
      showNotification("Annual classified stack built!", type = "message", duration = 4)
    }, error = function(e) {
      # Error notification
      showNotification(
        paste("Error building annual stack:", e$message),
        type = "error",
        duration = 12
      )
    }, finally = {
      # close progress modal
      gfc_progress_close()
    })
  })
  
  # ANNUAL STACK - COMPLETION STATUS
  output$annual_stack_done <- reactive({
    rv$annual_stack_done
  })
  
  outputOptions(output, "annual_stack_done", suspendWhenHidden = FALSE)
  
  # CLASSIFIED YEAR SELECTOR
  output$classified_year_ui <- renderUI({
    req(rv$gfc_annual_stack)
    
    yrs <- as.integer(gsub("^y", "", names(rv$gfc_annual_stack)))
    shiny::validate(
      shiny::need(
        length(yrs) > 0 && all(!is.na(yrs)),
        "No valid years are available in the annual stack."
      )
    )
    sliderInput(
      "classified_year",
      "Year:",
      min = min(yrs),
      max = max(yrs),
      value = max(yrs),
      step = 1,
      sep = "",
      animate = TRUE
    )
  })
  
  # SELECTED CLASSIFIED LAYER
  classified_layer <- reactive({
    req(rv$gfc_annual_stack, input$classified_year)
    
    lyr_name <- paste0("y", input$classified_year)
    shiny::validate(
      shiny::need(
        lyr_name %in% names(rv$gfc_annual_stack),
        "Selected year not available in the annual stack."
      )
    )
    rv$gfc_annual_stack[[lyr_name]]
  })
  
  # CLASSIFIED MAP - STATIC VERSION
  output$classified_map_plot <- renderPlot({
    req(classified_layer())
    
    plot_gfc(
      fchg = classified_layer(),
      aoi = sf::st_zm(rv$aoi_native),
      plot_aoi = TRUE,
      title_string = as.character(input$classified_year),
      size_scale = 1.1
    )
  })
  
  # CLASSIFIED MAP - WGS84 / LEAFLET VERSION
  # Reproject to WGS84 using nearest-neighbour resampling.
  # This is important because the raster contains categorical class codes.
  # Nearest-neighbour prevents interpolation from creating artificial
  # fractional class values.
  classified_layer_ll <- reactive({
    req(classified_layer())
    terra::project(classified_layer(), "EPSG:4326", method = "near")
  })
  
  # CLASSIFIED MAP COLOUR PALETTE
  # Explicitly provide `levels` so that the class-code ordering is preserved.
  # GFC_CLASS_CODES is deliberately ordered:
  # 1, 2, 3, 4, 5, 6, 0
  classified_pal <- leaflet::colorFactor(
    palette = unname(GFC_CLASS_COLORS[as.character(GFC_CLASS_CODES)]),
    domain = GFC_CLASS_CODES,
    levels = as.character(GFC_CLASS_CODES)
  )
  
  # INTERACTIVE CLASSIFIED MAP
  output$classified_map_leaflet <- renderLeaflet({
    req(classified_layer_ll())
    leaflet() %>%
      addMapPane("classified_raster_pane", zIndex = 350) %>%
    # Base map 1
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      # Base map 2
      addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
  
    # Classified raster
    # Because this is an overlay group, Leaflet automatically gives the user
    # an ON/OFF checkbox for the raster layer.
    addRasterImage(
      classified_layer_ll(),
      colors = classified_pal,
      opacity = input$classified_opacity %||% 0.8,
      project = FALSE,
      group = "Classified raster",
      options = leaflet::gridOptions(pane = "classified_raster_pane")
    ) %>%
    # AOI boundary
    addPolygons(
      data = sf::st_zm(rv$aoi_wgs),
      fill = FALSE,
      color = "white",
      weight = 2,
      group = "AOI boundary"
    ) %>%
    # Legend
    addLegend(
      position = "bottomright",
      colors = unname(GFC_CLASS_COLORS[as.character(GFC_CLASS_CODES)]),
      labels = unname(GFC_CLASS_LABELS[as.character(GFC_CLASS_CODES)]),
      title = "Cover class",
      opacity = 1
    ) %>%
      
    # Layer controls
    # `collapsed = FALSE` keeps the controls visible.
    # The user can now toggle:
    #   Classified raster  ON/OFF
    #   AOI boundary       ON/OFF
    addLayersControl(
      baseGroups = c("Satellite", "OpenStreetMap"),
      overlayGroups = c("Classified raster", "AOI boundary"),
      options = layersControlOptions(collapsed = FALSE)
    )
  })
  
  # CLASSIFIED MAP OPACITY
  observeEvent(
    input$classified_opacity,
    {
      req(classified_layer_ll())
      leafletProxy("classified_map_leaflet") %>%
        clearGroup("Classified raster") %>%
        addRasterImage(
          classified_layer_ll(),
          colors = classified_pal,
          opacity = input$classified_opacity,
          project = FALSE,
          group = "Classified raster",
          options = leaflet::gridOptions(pane = "classified_raster_pane")
        )
    },
    ignoreInit = TRUE
  )
  
  # DOWNLOAD CLASSIFIED MAP - PNG
  output$download_classified_plot <- downloadHandler(
    filename = function() {
      paste0("gfc_classified_map_",input$classified_year, ".png")
    },
    content = function(file) {
      p <- plot_gfc(
        fchg = classified_layer(),
        aoi = sf::st_zm(rv$aoi_native),
        plot_aoi = TRUE,
        title_string = as.character(
          input$classified_year
        ),
        size_scale = 1.1,
        maxpixels = 1e6
      )
      ggsave(file, p, width = 8, height = 7, dpi = 200, bg = "white")
    }
  )
  
  # DOWNLOAD CURRENT CLASSIFIED RASTER
  output$download_classified_raster <- downloadHandler(
    filename = function() {
      paste0("gfc_classified_", input$classified_year, ".tif")
    },
    content = function(file) {
      terra::writeRaster(classified_layer(), file, overwrite = TRUE)
    }
  )
  
  # DOWNLOAD ALL ANNUAL LAYERS
  output$download_all_annual_layers <- downloadHandler(
    filename = function() {
      "gfc_annual_layers.zip"
    },
    
    content = function(file) {
      layers_dir <- file.path(session_dir, "gfc_annual_layers")
      dir.create(layers_dir, showWarnings = FALSE, recursive = TRUE)
      
      # Open custom progress modal
      gfc_progress_open(
        title = "Writing Annual Layers",
        detail = "Preparing annual raster files..."
      )
      
      tryCatch({
        # Number of annual layers
        n <- terra::nlyr(rv$gfc_annual_stack)
        
        if (is.null(n) || n == 0) {
          stop("No annual layers are available to write.")
        }
        
        # Write annual raster layers
        for (i in seq_len(n)) {
          lyr <- rv$gfc_annual_stack[[i]]
          yr_name <- names(rv$gfc_annual_stack)[i]
          terra::writeRaster(lyr, file.path(layers_dir, paste0(yr_name, ".tif")), overwrite = TRUE)
          
          # Update percentage
          gfc_progress_update(
            session = session, frac = i / n, detail = paste("Writing", yr_name, "...")
          )
        }
        
        # Create ZIP archive
        gfc_progress_update(
          session = session, frac = 1, detail = "Creating ZIP archive..."
        )
        
        zip_folder(layers_dir, file)

        # Complete
        gfc_progress_update(
          session = session, frac = 1, detail = "Annual layers ZIP created successfully."
        )
      }, error = function(e) {
        showNotification(
          paste("Error writing annual layers:", e$message),
          type = "error", duration = 12
        )
      }, finally = {
        # close progress modal
        gfc_progress_close()
      })
    }
  )
  
  ##############################################################################
  # TAB 4b: TREE COVER % MAP
  ##############################################################################
  output$treecover_year_ui <- renderUI({
    req(rv$gfc_raw)
    max_yr <- gfc_dataset_year(input$dataset)
    sliderInput("treecover_year", "Year:", min = 2001, max = max_yr, value = max_yr, step = 1, sep = "")
  })

  output$treecover_compare_ui <- renderUI({
    req(rv$gfc_raw)
    max_yr <- gfc_dataset_year(input$dataset)
    choices <- 2001:max_yr
    default_sel <- unique(round(seq(2001, max_yr, length.out = 4)))
    checkboxGroupInput("treecover_compare_years", "Years to compare (choose up to 6):",
                        choices = choices, selected = default_sel, inline = TRUE)
  })
  

  observeEvent(input$treecover_plot_btn, {
    req(rv$gfc_raw, input$treecover_year)
    
    withProgress(message = "Building tree cover map...", value = 0.2, {
      tryCatch({
        
        r_native <- forest_cover_year(
          rv$gfc_raw,
          target_year = input$treecover_year,
          aoi = sf::st_zm(rv$aoi_native)
        )
        
        # Keep the full-resolution raster untouched
        rv$treecover_native <- r_native
        rv$treecover_year_selected <- input$treecover_year
        
        incProgress(0.4, detail = "Reprojecting for display...")
        
        r_ll <- terra::project(r_native, "EPSG:4326")
        
        # ---------------------------------------------------------------
        # Leaflet display raster
        # ---------------------------------------------------------------
        
        leaflet_max_bytes <- 4 * 1024^2   # Original Leaflet limit
        leaflet_max_cells <- 1000000      # Display safeguard
        
        n_cells <- terra::ncell(r_ll)
        
        # Start with full-resolution raster
        r_display <- r_ll
        resampled <- FALSE
        
        # If the raster is large, reduce it for visualisation
        if (n_cells > leaflet_max_cells) {
          
          fact <- ceiling(sqrt(n_cells / leaflet_max_cells))
          
          r_display <- terra::aggregate(
            r_ll,
            fact = fact,
            fun = "mean",
            na.rm = TRUE
          )
          
          resampled <- TRUE
        }
        
        rv$treecover_ll_df <- r_display
        rv$treecover_resampled <- resampled
        
        if (resampled) {
          showNotification(
            "The tree-cover raster was resampled for Leaflet visualisation only. The downloaded raster remains at full resolution.",
            type = "message",
            duration = 8
          )
        }
        
        rv$treecover_done <- TRUE
        
        incProgress(1)
        
      }, error = function(e) {
        
        showNotification(
          paste("Error building tree cover map:", e$message),
          type = "error",
          duration = 12
        )
        
      })
    })
  })
  

  output$treecover_done <- reactive({ rv$treecover_done })
  outputOptions(output, "treecover_done", suspendWhenHidden = FALSE)

  output$treecover_plot <- renderPlot({
    req(rv$treecover_ll_df)
    make_treecover_plot(rv$treecover_ll_df, rv$aoi_wgs, rv$treecover_year_selected)
  })

  # ---- Interactive (Leaflet) version of the tree cover % map --------------------
  treecover_pal <- leaflet::colorNumeric(
    palette = c("#E1E1E1", "#FFE5AD", "#B4A022", "#61790A", "#245231", "#003B47"),
    domain = c(0, 100), na.color = "transparent"
  )
  
 
  output$treecover_map_leaflet <- renderLeaflet({
    req(rv$treecover_ll_df, rv$aoi_wgs)
    
    bbox <- sf::st_bbox(rv$aoi_wgs)
    
    leaflet() %>%
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Satellite"
      ) %>%
      addProviderTiles(
        providers$OpenStreetMap,
        group = "OpenStreetMap"
      ) %>%
      addRasterImage(
        rv$treecover_ll_df,
        colors = treecover_pal,
        opacity = input$treecover_opacity %||% 0.85,
        project = FALSE,
        group = "Tree cover %",
        maxBytes = 20 * 1024^2
      ) %>%
      addPolygons(
        data = sf::st_zm(rv$aoi_wgs),
        fill = FALSE,
        color = "white",
        weight = 2,
        group = "AOI boundary"
      ) %>%
      addLegend(
        position = "bottomright",
        pal = treecover_pal,
        values = c(0, 100),
        title = "Tree cover (%)",
        opacity = 1
      ) %>%
      addLayersControl(
        baseGroups = c("Satellite", "OpenStreetMap"),
        overlayGroups = c("Tree cover %", "AOI boundary"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      fitBounds(
        bbox[["xmin"]],
        bbox[["ymin"]],
        bbox[["xmax"]],
        bbox[["ymax"]]
      )
  })
  
  observeEvent(input$treecover_opacity, {
    
    req(rv$treecover_ll_df)
    
    leafletProxy("treecover_map_leaflet") %>%
      clearGroup("Tree cover %") %>%
      addRasterImage(
        rv$treecover_ll_df,
        colors = treecover_pal,
        opacity = input$treecover_opacity,
        project = FALSE,
        group = "Tree cover %",
        maxBytes = 20 * 1024^2
      )
    
  }, ignoreInit = TRUE)
  
  
  # }, ignoreInit = TRUE)

  output$download_treecover_plot <- downloadHandler(
    filename = function() paste0("gfc_treecover_", rv$treecover_year_selected, ".png"),
    content = function(file) {
      p <- make_treecover_plot(rv$treecover_ll_df, rv$aoi_wgs, rv$treecover_year_selected)
      ggsave(file, p, width = 8, height = 7, dpi = 200, bg = "white")
    }
  )

  output$download_treecover_raster <- downloadHandler(
    filename = function() paste0("gfc_treecover_", rv$treecover_year_selected, ".tif"),
    content = function(file) terra::writeRaster(rv$treecover_native, file, overwrite = TRUE)
  )

  observeEvent(input$treecover_compare_btn, {
    req(rv$gfc_raw, input$treecover_compare_years)
    yrs <- as.integer(input$treecover_compare_years)
    shiny::validate(shiny::need(length(yrs) >= 2 && length(yrs) <= 6, "Please select between 2 and 6 years to compare."))

    withProgress(message = "Building comparison maps...", value = 0, {
      tryCatch({
        plots <- vector("list", length(yrs))
        for (i in seq_along(yrs)) {
          r_native <- forest_cover_year(rv$gfc_raw, target_year = yrs[i], aoi = sf::st_zm(rv$aoi_native))
          r_ll <- terra::project(r_native, "EPSG:4326")
          plots[[i]] <- make_treecover_plot(r_ll, rv$aoi_wgs, yrs[i])
          incProgress(1 / length(yrs), detail = paste("Year", yrs[i]))
        }
        # combined <- patchwork::wrap_plots(plots, ncol = 2) +
        #   patchwork::plot_layout(guides = "collect") &
        #   theme(legend.position = "bottom")
        ####
        ncol_dynamic <- optimal_ncol(length(yrs))
        
        combined <- patchwork::wrap_plots(plots, ncol = ncol_dynamic) +
          patchwork::plot_layout(guides = "collect") &
          theme(legend.position = "bottom")
        
        rv$treecover_compare_plot <- combined
        rv$treecover_compare_done <- TRUE
      }, error = function(e) {
        showNotification(paste("Error building comparison:", e$message), type = "error", duration = 12)
      })
    })
  })

  output$treecover_compare_done <- reactive({ rv$treecover_compare_done })
  outputOptions(output, "treecover_compare_done", suspendWhenHidden = FALSE)

  output$treecover_compare_plot <- renderPlot({
    req(rv$treecover_compare_plot)
    rv$treecover_compare_plot
  })

  output$download_treecover_compare_plot <- downloadHandler(
    filename = function() paste0("gfc_treecover_comparison_", Sys.Date(), ".png"),
    content = function(file) ggsave(file, rv$treecover_compare_plot, width = 12, height = 9, dpi = 200, bg = "white")
  )

  ##############################################################################
  # TAB 4c: LOSS YEAR MAP
  ##############################################################################
  # SHARED LOSS-YEAR PALETTE
  # Both the static ggplot map and the interactive Leaflet map use this
  # exact same named colour vector.
  
  loss_year_palette <- reactive({
    req(input$loss_palette)
    max_yr <- gfc_dataset_year(input$dataset)
    years <- 2001:max_yr
    
    if (
      identical(input$loss_palette, "distinct") &&
      isTRUE(pkg_available("randomcoloR"))
    ) {
      cols <- randomcoloR::distinctColorPalette(length(years))
    } else {
      cols <- viridisLite::viridis(length(years), option = "D")
    }
    names(cols) <- as.character(years)
    cols
  })
  
  # BUILD LOSS YEAR MAP
  observeEvent(input$loss_year_plot_btn, {
    req(rv$gfc_raw)
    gfc_progress_open(
      title = "Building Loss Year Map",
      detail = "Preparing loss-year map..."
    )
    tryCatch({
      max_yr <- gfc_dataset_year(input$dataset)
      # Get the ONE shared palette used by both maps.
      shared_palette <- loss_year_palette()
      # STATIC MAP
      gfc_progress_update(
        session = session, frac = 0.30, detail = "Building static loss-year map..."
      )
      rv$loss_year_plot_obj <- make_loss_year_plot(
        loss_raster = rv$gfc_raw[["lossyear"]],
        aoi_layer = sf::st_zm(rv$aoi_native),
        palette_type = input$loss_palette,
        legend_cols = input$loss_legend_cols,
        max_year = max_yr,
        palette_values = shared_palette
      )
      rv$loss_year_max_yr <- max_yr
      # INTERACTIVE LEAFLET RASTER
      gfc_progress_update(
        session = session, frac = 0.60, detail = "Preparing interactive loss-year map..."
      )
      lossyear_r <- rv$gfc_raw[["lossyear"]]
      # 0 = no loss -> NA
      # 1 = 2001
      # 2 = 2002
      # etc.
      year_r <- terra::classify(lossyear_r, rbind(c(0, NA))) + 2000
      rv$loss_year_map_r <- terra::project(year_r, "EPSG:4326", method = "near")
      rv$loss_year_done <- TRUE
      gfc_progress_update(
        session = session, frac = 1, detail = "Loss-year map built successfully."
      )
      showNotification("Loss-year map built!", type = "message", duration = 4)
    }, error = function(e) {
      showNotification(
        paste("Error building loss-year map:", conditionMessage(e)),
        type = "error", duration = 12
      )
    }, finally = {
      gfc_progress_close()
    })
  })

  # UPDATE STATIC MAP WHEN PALETTE CHANGES
  observeEvent(input$loss_palette, {
    req(rv$gfc_raw, rv$loss_year_max_yr, loss_year_palette())
    tryCatch({
      rv$loss_year_plot_obj <- make_loss_year_plot(
        loss_raster = rv$gfc_raw[["lossyear"]],
        aoi_layer = sf::st_zm(rv$aoi_native),
        palette_type = input$loss_palette,
        legend_cols = input$loss_legend_cols,
        max_year = rv$loss_year_max_yr,
        palette_values = loss_year_palette()
      )
    }, error = function(e) {
      showNotification(
        paste("Error updating loss-year palette:", conditionMessage(e)),
        type = "error", duration = 8
      )
    })
  }, ignoreInit = TRUE)
    
    # LOSS YEAR DONE
    output$loss_year_done <- reactive({
      rv$loss_year_done
    })
    outputOptions(output, "loss_year_done", suspendWhenHidden = FALSE)
    
    # STATIC LOSS YEAR PLOT
    output$loss_year_plot <- renderPlot({
      req(rv$loss_year_plot_obj)
      rv$loss_year_plot_obj
    })
    
    # DOWNLOAD STATIC LOSS YEAR PLOT
    output$download_loss_year_plot <- downloadHandler(
      filename = function() {
        paste0("gfc_loss_year_map_", Sys.Date(), ".png")
      },
      content = function(file) {
        ggsave(
          file, rv$loss_year_plot_obj, width = 10, height = 8, dpi = 200, bg = "white"
        )
      }
    )
    
    # LEAFLET PALETTE
    loss_year_pal <- reactive({
      req(rv$loss_year_max_yr, loss_year_palette())
      years <- 2001:rv$loss_year_max_yr
      leaflet::colorFactor(
        palette = loss_year_palette(),
        domain = years,
        na.color = "transparent"
      )
    })
    
    # INTERACTIVE LEAFLET LOSS YEAR MAP
    output$loss_year_map_leaflet <- renderLeaflet({
      req(rv$loss_year_map_r, rv$loss_year_max_yr, loss_year_pal(), rv$aoi_wgs)
      
      years <- 2001:rv$loss_year_max_yr
      pal <- loss_year_pal()
      bbox <- sf::st_bbox(rv$aoi_wgs)
      
      m <- leaflet() %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
        addRasterImage(
          rv$loss_year_map_r, colors = pal,
          opacity = input$loss_year_opacity %||% 0.85,
          project = FALSE, group = "Loss year"
        ) %>%
        addPolygons(
          data = sf::st_zm(rv$aoi_wgs), fill = FALSE,
          color = "white", weight = 2, group = "AOI boundary"
        ) %>%
        addLegend(position = "bottomright", pal = pal, values = years,
                  title = "Loss year", opacity = 1) %>%
        addLayersControl(
          baseGroups = c("Satellite", "OpenStreetMap"),
          overlayGroups = c("Loss year", "AOI boundary"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>%
        fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
      
      if (pkg_available("leafem")) {
        m <- m %>%
          leafem::addImageQuery(
            rv$loss_year_map_r, project = FALSE, layerId = "Loss year",
            type = "mousemove", digits = 0, prefix = "Loss year: "
          )
      }
      m
    })
    
    # UPDATE LEAFLET OPACITY
    observeEvent(
      input$loss_year_opacity,
      {
        req(rv$loss_year_map_r, rv$loss_year_max_yr, loss_year_pal())
        leafletProxy("loss_year_map_leaflet") %>%
          clearGroup("Loss year") %>%
          addRasterImage(
            rv$loss_year_map_r,
            colors = loss_year_pal(),
            opacity = input$loss_year_opacity,
            project = FALSE,
            group = "Loss year"
          )
      },
      ignoreInit = TRUE
    )
    

  ##############################################################################
  # TAB 5: MULTI-YEAR FACET MAPS
  ##############################################################################
  observeEvent(input$facet_btn, {
    req(rv$gfc_raw)
    base_yr <- as.integer(input$facet_base_year)
    target_yr <- as.integer(input$facet_target_year)
    
    if (is.na(base_yr) || is.na(target_yr)) {
      showNotification(
        "Please select valid base and target years.", type = "error", duration = 6
      )
      return()
    }
    
    if (target_yr <= base_yr) {
      showNotification(
        "Target year must be after the base year.", type = "error", duration = 6
      )
      return()
    }
    
    # Open custom progress modal
    gfc_progress_open(
      title = "Building Multi-Year Facet Maps",
      detail = "Starting annual forest-cover and loss calculations..."
    )
    
    tryCatch({
      yrs <- base_yr:target_yr
      n_yrs <- length(yrs)
      
      agg_factor <- as.integer(input$facet_agg_factor)
      threshold <- as.numeric(input$forest_threshold)
      
      if (is.na(agg_factor) || agg_factor < 1L) {
        agg_factor <- 1L
      }
      
      if (is.na(threshold)) {
        threshold <- 30
      }
      
      # Get source rasters
      tree_r <- rv$gfc_raw[["treecover2000"]]
      loss_r <- rv$gfc_raw[["lossyear"]]
      gain_r <- rv$gfc_raw[["gain"]]
      
      req(tree_r, loss_r, gain_r)
      
      # Calculate masks that do NOT depend on year only once.
      # This avoids repeatedly evaluating the same terra
      # expressions inside the yearly loop.
      was_forest_2000 <- terra::ifel(tree_r >= threshold, 1L, 0L)
      is_gain <- terra::ifel(gain_r == 1L, 1L, 0L)
      
      # Forest/gain source that can potentially contribute to
      # the annual loss calculation.
      was_forest_or_gained <- terra::ifel((was_forest_2000 == 1L) | (is_gain == 1L), 1L, 0L)
      
      # We store ONLY ordinary R data.frames in these lists.
      # We do NOT retain SpatRaster objects from each iteration.
      # Each raster is converted to a data.frame immediately.
      cover_dfs <- vector("list", n_yrs)
      n_loss <- max(0L, n_yrs - 1L)
      
      if (n_loss > 0L) {
        loss_dfs <- vector("list", n_loss)
      } else {
        loss_dfs <- list()
      }
      loss_index <- 0L
      
      # Process each year independently
      for (i in seq_along(yrs)) {
        yr <- yrs[i]
        loss_code <- yr - 2000L
        
        # Progress message
        gfc_progress_update(
          session = session, frac = (i - 1) / n_yrs,
          detail = paste("Processing forest cover and loss for year", yr, "...")
        )
        
        # 1. FOREST COVER MASK
        # A pixel is still present if:
        #   - it has no recorded loss, OR
        #   - its loss occurred after the selected year.
        not_lost_by_yr <- terra::ifel((loss_r == 0L) | (loss_r > loss_code), 1L, 0L)
        # Original 2000 forest surviving through this year.
        cond_A <- was_forest_2000 * not_lost_by_yr
        # Gain pixels surviving through this year.
        # The GFC gain raster is binary and does not contain the
        # exact year of gain. Therefore this reproduces the logic
        # of the original application: gain pixels are treated as
        # forest provided they have not subsequently been lost.
        cond_B <- is_gain * not_lost_by_yr
        cover_mask <- terra::ifel((cond_A + cond_B) > 0L, 1L, 0L)
        
        # Optional aggregation
        if (agg_factor > 1L) {
          cover_mask <- terra::aggregate(cover_mask, fact = agg_factor, fun = "mean")
        }
        
        # Convert IMMEDIATELY to an ordinary data.frame.
        # na.rm = TRUE avoids retaining all NA cells.
        cdf <- terra::as.data.frame(cover_mask, xy = TRUE, na.rm = TRUE)
        
        # Make sure the raster value column has a predictable name.
        names(cdf)[1:3] <- c("x", "y", "value")
        cdf$year <- yr
        cover_dfs[[i]] <- cdf
        
        # Explicitly release this year's temporary raster.
        rm(cover_mask, not_lost_by_yr, cond_A, cond_B, cdf)
        
        # 2. ANNUAL FOREST LOSS MASK
        # The base year has no "loss during that year" panel
        # because the GFC loss-year raster starts at 2001.
        # Therefore loss panels are generated only for:
        #   base_yr + 1 ... target_yr
        
        if (yr != base_yr) {
          # Pixels whose GFC loss year is exactly this year.
          loss_this_year <- terra::ifel(loss_r == loss_code, 1L, 0L)
          
          # Restrict loss to pixels that were either:
          #   - forest in 2000, OR
          #   - marked as gain.
          loss_mask <- loss_this_year * was_forest_or_gained
          
          if (agg_factor > 1L) {
            loss_mask <- terra::aggregate(loss_mask, fact = agg_factor, fun = "max")
          }
          
          # Immediately convert raster to ordinary data.frame.
          ldf <- terra::as.data.frame(loss_mask, xy = TRUE, na.rm = TRUE)
          names(ldf)[1:3] <- c("x", "y", "value")
          ldf$year <- yr
          loss_index <- loss_index + 1L
          loss_dfs[[loss_index]] <- ldf
          
          rm(loss_this_year,loss_mask, ldf) # release temporary objects
        }
        
        # Update progress after this year is complete
        gfc_progress_update(
          session = session, frac = i / n_yrs, detail = paste("Completed year", yr)
        )
        
        # Give R a chance to clean up temporary objects.
        if (i %% 2L == 0L) {
          gc(verbose = FALSE)
        }
        
        # # Release temporary objects
        # rm(not_lost_by_yr, cond_A, cond_B, cover_mask, cdf)
      }
      
      gfc_progress_update(
        session = session, frac = 0.90,
        detail = "Combining annual forest-cover and loss data..."
      )
      
      raster_df <- dplyr::bind_rows(cover_dfs) # combine forest data.frames
      
      # Combine loss data.frames
      if (loss_index > 0L) {
        loss_df <- dplyr::bind_rows(loss_dfs[seq_len(loss_index)])
      } else {
        # Empty but correctly structured data.frame.
        loss_df <- data.frame(x = numeric(0), y = numeric(0), value = numeric(0), year = integer(0))
      }
      
      # Release source helper objects that are no longer needed.
      #
      # Do NOT remove tree_r/loss_r/gain_r because they are only
      # local references anyway, but releasing them here can help
      # memory before ggplot construction.
      rm(cover_dfs, loss_dfs, was_forest_2000, is_gain, was_forest_or_gained)
      gc(verbose = FALSE)
      
      aoi_layer <- sf::st_zm(rv$aoi_native) # prepare AOI layer
      
      gfc_progress_update(
        session = session, frac = 0.94,
        detail = "Building forest-cover facet maps..."
      )
      
      # Build forest facet plot
      rv$forest_facet_plot_obj <- make_forest_facet_plot(
        raster_df = raster_df,
        aoi_layer = aoi_layer,
        canopy_threshold = threshold,
        n_cols = min(5L, max(1L, n_yrs - 1L)) #min(8L, n_yrs)
      )
      
      gfc_progress_update(
        session = session, frac = 0.97,
        detail = "Building annual forest-loss facet maps..."
      )
      
      # Build loss facet plot
      rv$loss_facet_plot_obj <- make_loss_facet_plot(
        loss_df = loss_df,
        aoi_layer = aoi_layer,
        canopy_threshold = threshold,
        n_cols = min(5L, max(1L, n_yrs - 1L))
      )
      
      rv$facet_done <- TRUE # mark Tab 5 as complete
      
      # Final progress update
      gfc_progress_update(
        session = session, frac = 1,
        detail = "Multi-year facet maps built successfully."
      )
    
      showNotification("Facet maps built!", type = "message", duration = 4)
      
    }, error = function(e) {
      # Report errors without crashing the Shiny application.
      showNotification(
        paste("Error building facet maps:", conditionMessage(e)),
        type = "error", duration = 12
      )
    }, finally = {
      gfc_progress_close() # close progress bar
    })
  })
  
  ######################################################################
  # TAB 5 STATUS
  ######################################################################
  output$facet_done <- reactive({
    isTRUE(rv$facet_done)
  })

  outputOptions(output, "facet_done", suspendWhenHidden = FALSE)
  
  ######################################################################
  # FOREST FACET PLOT
  ######################################################################
  output$forest_facet_plot <- renderPlot({
    req(rv$forest_facet_plot_obj)
    rv$forest_facet_plot_obj
  })
  
  ######################################################################
  # LOSS FACET PLOT
  ######################################################################
  output$loss_facet_plot <- renderPlot({
    req(rv$loss_facet_plot_obj)
    rv$loss_facet_plot_obj
  })
  
  ######################################################################
  # DOWNLOAD FOREST FACET PLOT
  ######################################################################
  output$download_forest_facet_plot <- downloadHandler(
    filename = function() {
      paste0(
        "gfc_forest_facet_",
        input$facet_base_year,
        "_",
        input$facet_target_year,
        ".png"
      )
    },
    
    content = function(file) {
      req(rv$forest_facet_plot_obj)
      
      n_yrs <- as.integer(input$facet_target_year - input$facet_base_year + 1L)
      n_cols <- min(8L, n_yrs)
      n_rows <- ceiling(n_yrs / n_cols)
      
      ggsave(
        filename = file,
        plot = rv$forest_facet_plot_obj,
        width = n_cols * 3.5,
        height = n_rows * 3.0 + 1.5,
        dpi = 180,
        limitsize = FALSE,
        bg = "white"
      )
    }
  )
  
  ######################################################################
  # DOWNLOAD LOSS FACET PLOT
  ######################################################################
  output$download_loss_facet_plot <- downloadHandler(
    filename = function() {
      paste0(
        "gfc_loss_facet_",
        input$facet_base_year,
        "_",
        input$facet_target_year,
        ".png"
      )
    },
    
    content = function(file) {
      req(rv$loss_facet_plot_obj)
      
      n_yrs <- as.integer(input$facet_target_year - input$facet_base_year)
      n_cols <- min(5L, max(1L, n_yrs))
      n_rows <- ceiling(n_yrs / n_cols)
      
      ggsave(
        filename = file,
        plot = rv$loss_facet_plot_obj,
        width = n_cols * 3.5,
        height = n_rows * 3.0 + 1.5,
        dpi = 180,
        limitsize = FALSE,
        bg = "white"
      )
    }
  )
  

##############################################################################
# TAB 6: ANIMATION
##############################################################################
observeEvent(input$build_animation_btn, {
  req(rv$gfc_annual_stack, rv$aoi_wgs)
  
  # Open the same custom spinning progress modal used in Tab 5
  gfc_progress_open(
    title = "Creating Animation",
    detail = "Starting animation rendering..."
  )
  
  tryCatch({
    
    anim_dir <- file.path(session_dir, "animation")
    dir.create(anim_dir, showWarnings = FALSE)
    
    if (is.null(rv$anim_resource_registered)) {
      addResourcePath(paste0("anim_", session$token), anim_dir)
      rv$anim_resource_registered <- TRUE
    }
    
    gfc_progress_update(
      session = session,
      frac = 0.05,
      detail = "Preparing animation frames..."
    )
    
    out_path <- animate_annual(
      aoi = sf::st_zm(rv$aoi_wgs),
      gfc_stack = rv$gfc_annual_stack,
      out_dir = anim_dir,
      out_basename = "gfc_animation",
      type = input$anim_type,
      height = 4,
      width = 4.5,
      dpi = input$anim_dpi,
      dataset = input$dataset,
      plot_aoi = isTRUE(input$anim_plot_aoi),
      aoi_crop = isTRUE(input$anim_crop_aoi),
      
      # Update the same custom progress spinner
      progress_fun = function(frac, detail = NULL) {
        
        # Keep progress safely between 0 and 1
        frac <- max(0, min(1, frac))
        
        # Reserve a little room for the finalisation steps
        overall_frac <- 0.05 + (frac * 0.85)
        
        gfc_progress_update(
          session = session,
          frac = overall_frac,
          detail = if (!is.null(detail)) {
            detail
          } else {
            "Rendering animation frames..."
          }
        )
      }
    )
    
    gfc_progress_update(
      session = session,
      frac = 0.93,
      detail = "Finalising animation..."
    )
    
    rv$animation_path <- out_path
    rv$animation_type <- input$anim_type
    rv$animation_done <- TRUE

    gfc_progress_update(
      session = session,
      frac = 1,
      detail = "Animation created successfully."
    )

    # showNotification(
    #   "Animation created!",
    #   type = "message",
    #   duration = 4
    # )
    
    showNotification(
      paste0("Animation created and saved to: ", out_path),
      type = "message",
      duration = 8
    )
    
  }, error = function(e) {
    
    showNotification(
      paste("Error creating animation:", conditionMessage(e)),
      type = "error",
      duration = 12
    )
    
  }, finally = {
    
    # Always close the custom progress spinner
    gfc_progress_close()
    
  })
})


output$animation_done <- reactive({
  rv$animation_done
})

outputOptions(
  output,
  "animation_done",
  suspendWhenHidden = FALSE
)


# output$animation_display <- renderUI({
#   req(rv$animation_done)
#   if (rv$animation_type == "gif") {
#     imageOutput(
#       "animation_gif_img",
#       height = "500px"
#     )
#   } else {
#     helpText(
#       "HTML animation created - use the download button below, then open the file in a web browser to view it."
#     )
#   }
# })
# 
# 
# output$animation_gif_img <- renderImage({
#   req(rv$animation_path)
#   list(
#     src = rv$animation_path,
#     contentType = "image/gif",
#     height = 480
#   )
# }, deleteFile = FALSE)
# 
# 
# output$download_animation <- downloadHandler(
#   filename = function() {
#     basename(rv$animation_path)
#   },
#   content = function(file) {
#     file.copy(
#       rv$animation_path,
#       file,
#       overwrite = TRUE
#     )
#   }
# )

output$animation_saved_msg <- renderUI({
  req(rv$animation_path)
  tags$p(
    tags$b("Saved to: "), tags$code(rv$animation_path)
  )
})

output$animation_display <- renderUI({
  req(rv$animation_done)
  # if (rv$animation_type == "gif") {
  #   imageOutput(
  #     "animation_gif_img",
  #     height = "500px"
  #   )
  if (rv$animation_type == "gif") {
    div(
      style = "max-width: 500px;",
      imageOutput("animation_gif_img", height = "auto")
    )
  } else {
    tags$iframe(
      src = file.path(paste0("anim_", session$token), basename(rv$animation_path)),
      width = "100%",
      height = "600px",
      style = "border:none;"
    )
  }
})

# output$animation_gif_img <- renderImage({
#   req(rv$animation_path, file.exists(rv$animation_path))
#   list(
#     src = normalizePath(rv$animation_path),
#     contentType = "image/gif",
#     width = "100%",
#     height = "auto"
#   )
# }, deleteFile = FALSE)

output$animation_gif_img <- renderImage({
  req(rv$animation_path, file.exists(rv$animation_path))
  list(
    src = normalizePath(rv$animation_path),
    contentType = "image/gif",
    width = 450,
    height = "auto"
  )
}, deleteFile = FALSE)

output$download_animation <- downloadHandler(
  filename = function() {
    req(rv$animation_path)
    if (rv$animation_type == "gif") {
      basename(rv$animation_path)
    } else {
      paste0(tools::file_path_sans_ext(basename(rv$animation_path)), ".zip")
    }
  },
  content = function(file) {
    req(rv$animation_path)
    
    if (rv$animation_type == "gif") {
      
      # GIF is a single self-contained file - just copy it
      file.copy(rv$animation_path, file, overwrite = TRUE)
      
    } else {
      
      # HTML animation needs its file PLUS its supporting "lib" folder
      # anim_dir  <- dirname(rv$animation_path)
      # html_file <- basename(rv$animation_path)
      # lib_dir   <- file.path(anim_dir, paste0(tools::file_path_sans_ext(html_file), "_files"))
      
      anim_dir  <- dirname(rv$animation_path)
      html_file <- basename(rv$animation_path)
      lib_dir   <- file.path(anim_dir, paste0(tools::file_path_sans_ext(html_file), "_imgs"))
      
      files_to_zip <- html_file
      if (dir.exists(lib_dir)) {
        files_to_zip <- c(files_to_zip, basename(lib_dir))
      }
      
      old_wd <- setwd(anim_dir)
      on.exit(setwd(old_wd), add = TRUE)
      
      zip::zip(zipfile = file, files = files_to_zip)
    }
  }
)


  # # ---- Animation outputs ------------------------------------------------------
  # 
  # output$animation_done <- reactive({
  #   rv$animation_done
  # })
  # 
  # outputOptions(
  #   output,
  #   "animation_done",
  #   suspendWhenHidden = FALSE
  # )
  

# Package update
observeEvent(input$check_updates, {
  withProgress(message = "Checking for package updates...", {
    update_required_packages(required_pkgs)
  })
  showNotification("Update check complete. Restart the app to use updated packages.", type = "message", duration = 6)
})

}
