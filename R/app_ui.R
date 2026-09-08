#' Dashboard UI (internal)
#'
#' Builds the dashboardPage() UI. Not exported; called only by
#' \code{\link{run_gfc_dashboard}}.
#'
#' @return A shinydashboard UI definition.
#' @keywords internal
#'
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
#' @import shinyjs
#' @import mapgl
#' @import leaflet
#' @import DT
gfc_app_ui <- function() {

# ---- dark theme CSS (look adapted from the Spatial Data Explorer dashboard) ----
# Same design language: IBM Plex Mono/Sans fonts, dark slate background,
# forest-green accent (#3fb950), monospace box headers/buttons. Kept in its own object
# so it is easy to tweak in one place.
gfc_dark_css <- "
@import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500;600&family=IBM+Plex+Sans:wght@300;400;500;600&display=swap');

:root {
  --bg-deep:     #0d1117;  --bg-panel:    #161b22;  --bg-card:     #1c2333;
  --bg-hover:    #243044;  --border:      #30363d;  --border-lite: #3d444d;
  --accent:      #33cc99;  --accent-dim:  #1a4731;  --accent-glow: rgba(63,185,80,0.14);
  --teal:        #39c5bb;  --teal-dim:    #0e3535;
  --orange:      #f0883e;  --orange-dim:  #3d1f00;
  --red:         #f85149;  --red-dim:     #3d0000;
  --info:        #58a6ff;  --info-dim:    #0e2b52;
  --text-primary:#e6edf3;  --text-muted:  #8b949e;  --text-dim: #999999;
  --mono: 'IBM Plex Mono', monospace;
  --sans: 'IBM Plex Sans', sans-serif;
}
body, .wrapper { background-color: var(--bg-deep) !important; color: var(--text-primary) !important; font-family: var(--sans) !important; font-size: 14px !important; }
::-webkit-scrollbar { width: 5px; } ::-webkit-scrollbar-track { background: var(--bg-deep); } ::-webkit-scrollbar-thumb { background: var(--border); border-radius: 3px; }

.main-header .navbar, .main-header .logo { background-color: var(--bg-panel) !important; border-bottom: 1px solid var(--border) !important; box-shadow: none !important; }
.main-header .logo { font-family: var(--mono) !important; font-weight: 600 !important; font-size: 20px !important; color: var(--accent) !important; }
.main-header .navbar .sidebar-toggle { color: var(--text-muted) !important; }
.main-header .navbar .sidebar-toggle:hover { color: var(--text-primary) !important; background: var(--bg-hover) !important; }

.main-sidebar, .left-side { background-color: var(--bg-panel) !important; border-right: 1px solid var(--border) !important; }
.sidebar-menu > li > a { color: var(--text-muted) !important; font-family: var(--mono) !important; font-size: 12px !important; font-weight: 500 !important; border-left: 3px solid transparent !important; padding: 10px 14px !important; transition: all 0.15s !important; }
.sidebar-menu > li > a:hover, .sidebar-menu > li.active > a { background-color: var(--bg-hover) !important; color: var(--text-primary) !important; border-left-color: var(--accent) !important; }
.sidebar-menu > li.active > a { color: var(--accent) !important; }
.sidebar-menu > li > a .fa { color: var(--text-dim) !important; width: 18px !important; }
.sidebar-menu > li.active > a .fa, .sidebar-menu > li > a:hover .fa { color: var(--accent) !important; }
.sidebar-footer-custom { padding: 12px 16px; background: var(--bg-deep); border-top: 1px solid var(--border); font-family: var(--mono); font-size: 11px; color: var(--text-dim); }
.sidebar-footer-custom a { color: #c1cdc1 !important; text-decoration: none !important; }

.content-wrapper, .main-footer { background-color: var(--bg-deep) !important; }
.content { padding: 20px !important; }

.box { background: var(--bg-card) !important; border: 1px solid var(--border) !important; border-top: none !important; border-radius: 8px !important; box-shadow: none !important; }
.box-header { background: var(--bg-panel) !important; border-bottom: 1px solid var(--border) !important; border-radius: 8px 8px 0 0 !important; padding: 10px 16px !important; }
.box-title { font-family: var(--mono) !important; font-size: 13px !important; font-weight: 600 !important; letter-spacing: 0.4px !important; color: var(--text-primary) !important; text-transform: uppercase !important; }
.box-header .fa { color: var(--text-muted) !important; margin-right: 7px !important; }
.box-body { padding: 16px !important; }
.box.box-solid > .box-header { background: var(--bg-panel) !important; }
.box.box-solid.box-primary, .box-primary:not(.box-solid) { border-top: 2px solid var(--teal)   !important; }
.box.box-solid.box-success, .box-success:not(.box-solid) { border-top: 2px solid var(--accent) !important; }
.box.box-solid.box-info,    .box-info:not(.box-solid)    { border-top: 2px solid var(--info)   !important; }
.box.box-solid.box-danger,  .box-danger:not(.box-solid)  { border-top: 2px solid var(--red)    !important; }

.btn { font-family: var(--mono) !important; font-size: 11px !important; font-weight: 600 !important; letter-spacing: 0.4px !important; border-radius: 5px !important; white-space: normal !important; padding: 7px 12px !important; min-height: 32px !important; height: auto !important; line-height: 1.4 !important; transition: all 0.15s !important; }
.btn-default, .btn-primary, .btn-success, .btn-info, .btn.action-button { background-color: var(--teal-dim) !important; border: 1px solid var(--teal) !important; color: var(--teal) !important; }
.btn-default:hover, .btn-primary:hover, .btn-success:hover, .btn-info:hover, .btn.action-button:hover { background-color: rgba(57,197,187,0.2) !important; color: #fff !important; }
.btn.shiny-download-link { background-color: var(--info-dim) !important; border: 1px solid var(--info) !important; color: var(--info) !important; }
.btn.shiny-download-link:hover { background-color: rgba(88,166,255,0.2) !important; color: #fff !important; }
.btn-danger { background-color: var(--red-dim) !important; border: 1px solid var(--red) !important; color: var(--red) !important; }
.btn-danger:hover { background-color: rgba(248,81,73,0.2) !important; color: #fff !important; }
.btn-lg { font-size: 12px !important; }

label, .control-label { color: var(--text-muted) !important; font-size: 12px !important; }
.form-control { background-color: var(--bg-deep) !important; border: 1px solid var(--border) !important; color: var(--text-primary) !important; border-radius: 5px !important; font-family: var(--mono) !important; font-size: 12px !important; }
.form-control:focus { border-color: var(--teal) !important; box-shadow: 0 0 0 2px rgba(57,197,187,0.15) !important; }
.shiny-input-container input[type='text'] { background: var(--bg-deep) !important; border: 1px solid var(--border) !important; color: var(--text-muted) !important; font-family: var(--mono) !important; font-size: 11px !important; border-radius: 5px !important; }
.checkbox label, .radio label { color: var(--text-primary) !important; font-size: 13px !important; }
.checkbox { margin: 3px 0 !important; } input[type='checkbox']:checked { accent-color: var(--accent); }
.shiny-input-container { margin-bottom: 8px !important; }
.irs--shiny .irs-bar, .irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to { background: var(--accent) !important; border-color: var(--accent) !important; }

pre, .shiny-text-output { background: var(--bg-deep) !important; border: 1px solid var(--border) !important; color: var(--accent) !important; font-family: var(--mono) !important; font-size: 12px !important; border-radius: 6px !important; padding: 12px !important; }

h1, h2, h3, h4, h5, h6 { font-family: var(--mono) !important; font-weight: 600 !important; color: var(--text-primary) !important; }
h2 { font-size: 18px !important; } h5 { font-size: 12px !important; }
a { color: var(--info) !important; } a:hover { color: var(--teal) !important; }
hr { border-color: var(--border) !important; margin: 12px 0 !important; }

.modal-content { background: var(--bg-card) !important; border: 1px solid var(--border) !important; border-radius: 8px !important; }
.modal-header { background: var(--bg-panel) !important; border-bottom: 1px solid var(--border) !important; }
.modal-title { color: var(--text-primary) !important; font-family: var(--mono) !important; }
.modal-body { color: var(--text-muted) !important; } .modal-footer { border-top: 1px solid var(--border) !important; }

.dataTables_wrapper { color: var(--text-muted) !important; font-family: var(--mono) !important; font-size: 12px !important; }
table.dataTable { background: var(--bg-deep) !important; color: var(--text-primary) !important; }
table.dataTable thead th { color: var(--text-primary) !important; border-bottom: 1px solid var(--border) !important; }
table.dataTable tbody td { border-color: var(--border) !important; }
table.dataTable.stripe tbody tr.odd { background-color: var(--bg-panel) !important; }

.dark-panel { background-color:#000000; padding:10px; border-radius:6px; border: 1px solid var(--border); }
.dark-panel .shiny-plot-output { background-color:#000000; }

.info-box, .small-box { border: 1px solid var(--border) !important; box-shadow: none !important; border-radius: 8px !important; }

/* ---- circular progress ring (used in the download/computation modals) ---- */
.gfc-progress-wrap { text-align: center; padding: 10px 6px 4px 6px; }
.gfc-progress-ring { width: 120px; height: 120px; transform: rotate(-90deg); }
.gfc-progress-ring-bg { fill: none; stroke: var(--border); stroke-width: 10; }
.gfc-progress-ring-fg { fill: none; stroke: var(--accent); stroke-width: 10; stroke-linecap: round; stroke-dasharray: 339.292; stroke-dashoffset: 339.292; transition: stroke-dashoffset 0.25s ease; }
.gfc-progress-pct { margin-top: -78px; font-family: var(--mono); font-size: 22px; font-weight: 600; color: var(--accent); }
.gfc-progress-title { margin-top: 14px; font-size: 13px; }
.gfc-progress-detail { font-family: var(--mono); font-size: 11px; color: var(--text-muted); min-height: 16px; }
"

gfc_progress_js <- "
Shiny.addCustomMessageHandler('gfcProgress', function(msg) {
  var circumference = 339.292;
  var fg = document.getElementById('gfc-progress-ring-fg');
  var pct = document.getElementById('gfc-progress-pct');
  var detail = document.getElementById('gfc-progress-detail');
  if (fg) fg.style.strokeDashoffset = circumference - (circumference * msg.pct / 100);
  if (pct) pct.innerText = msg.pct + '%';
  if (detail && typeof msg.detail !== 'undefined') detail.innerText = msg.detail;
});
"

  dashboardPage(
  skin = "black",
  dashboardHeader(
    title = tags$span("Forest Change Analysis",
                       style = "font-family:'IBM Plex Mono',monospace; font-weight:600;"),
    titleWidth = 300
  ),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("1. Upload AOI", tabName = "upload", icon = icon("upload")),
      menuItem("2. Extract & threshold", tabName = "extract", icon = icon("cloud-download-alt")),
      menuItem("3. Yearly trend analysis", tabName = "trends", icon = icon("chart-line")),
      menuItem("4. Map visualisations", tabName = "maps", icon = icon("map")),
      menuItem("5. Multi-year maps", tabName = "facets", icon = icon("th")),
      menuItem("6. Animate forest loss", tabName = "animation", icon = icon("film")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    tags$div(
      class = "sidebar-footer-custom",
      "Developed by Ugyen Penjor"
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet",
                href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
      tags$style(HTML(gfc_dark_css)),
      ####
      tags$style(HTML("
      .mapbox-gl-draw_ctrl-draw-btn.active {
        background-color: #fbb03b !important;
        box-shadow: inset 0 0 0 2px #d97706 !important;
      }
    ")),
      tags$script(HTML(gfc_progress_js))
    ),
    tabItems(

      ########################################################################
      # TAB 1: UPLOAD AOI
      ########################################################################
      tabItem(
        tabName = "upload",
        fluidRow(
          box(
            title = "Upload Area of Interest (AOI)", status = "primary", solidHeader = TRUE, width = 12,
            fileInput("shapefile", "Upload shapefile (select all components: .shp, .shx, .dbf, .prj)",
                      multiple = TRUE, accept = c(".shp", ".shx", ".dbf", ".prj", ".cpg")),
            helpText("Please upload all shapefile components together (.shp, .shx, .dbf, .prj). ",
                     "Nothing you upload here is ever saved to disk permanently - it is kept only ",
                     "in a temporary session folder for this analysis and removed automatically."),
            ####
            fluidRow(
              column(6, actionButton("load_aoi", "Load AOI", icon = icon("check"), class = "btn-primary btn-block")),
              column(6, actionButton("clear_aoi", "Clear all data", icon = icon("trash"), class = "btn-danger btn-block"))
            ),
            hr(),
            helpText("Or draw your AOI directly on the globe below using the polygon or rectangle tool ",
                     "(top-left of the map), then click 'Use Drawn Polygon'."),
            fluidRow(
              column(6, actionButton("use_drawn_aoi", "Use drawn polygon", icon = icon("draw-polygon"), class = "btn-success btn-block")),
              column(6, actionButton("clear_drawn_aoi", "Clear drawn AOI", icon = icon("eraser"), class = "btn-warning btn-block"))
            ),
            ####
            hr(),
            maplibreOutput("aoi_map", height = 440),
            radioButtons(
              "basemap_choice", "Basemap:",
              choices = c("Satellite" = "satellite", "OpenStreetMap" = "osm"),
              selected = "satellite",
              inline = TRUE
            ),
            conditionalPanel(
              condition = "output.aoi_loaded",
              h4("AOI Information:"),
              verbatimTextOutput("aoi_info")
            )
          )
        )
      ),

      ########################################################################
      # TAB 2: EXTRACT & THRESHOLD
      ########################################################################
      tabItem(
        tabName = "extract",
        fluidRow(
          box(
            title = "Download & Threshold GFC Data", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(4, selectInput("dataset", "GFC dataset version:",
                                     choices = c("GFC-2025-v1.13", "GFC-2024-v1.12", "GFC-2023-v1.11",
                                                 "GFC-2022-v1.10", "GFC-2021-v1.9", "GFC-2020-v1.8"),
                                     selected = "GFC-2025-v1.13")),
              chooseSliderSkin("Flat", color = "#006666"),
              column(4, sliderInput("forest_threshold", "Canopy cover threshold for 'forest' (%):",
                                     min = 10, max = 75, value = 30, step = 5,
                                     post = "%")),
              column(4, checkboxInput("to_utm", "Reproject to local UTM zone (recommended, needed for accurate hectare areas)",
                                       value = TRUE))
            ),
            actionButton("extract_btn", "Extract & process GFC data",
                         icon = icon("cloud-download-alt"), class = "btn-success btn-lg"),
            hr(),
            conditionalPanel(
              condition = "output.extract_done",
              h4("Annual loss / cover summary"),
              DTOutput("loss_table"),
              downloadButton("download_loss_csv", "Download loss/cover table (CSV)", class = "btn-success"),
              downloadButton("download_thresholded_raster", "Download thresholded raster (GeoTIFF)", class = "btn-success"),
              hr(),
              h4("Tree cover vs. forest loss over time"),
              helpText("Shown here in the dark display style; use the download button for a plain version suitable for reports."),
              div(class = "dark-panel", plotOutput("cover_loss_plot_dark", height = 420)),
              downloadButton("download_cover_loss_plot", "Download plot (report style, PNG)", class = "btn-success")
            )
          )
        )
      ),

      ########################################################################
      # TAB 3: YEARLY TREND ANALYSIS
      ########################################################################
      tabItem(
        tabName = "trends",
        fluidRow(
          box(
            title = "Yearly Forest Change Trend Analysis", status = "primary", solidHeader = TRUE, width = 12,
            helpText("Requires step 2 (Extract & threshold) to be completed first."),
            fluidRow(
              column(4, numericInput("base_year", "Base year:", value = 2000, min = 2000, max = 2024, step = 1)),
              column(4, numericInput("target_year_trend", "Target year:", value = 2025, min = 2001, max = 2025, step = 1)),
              column(4, br(), actionButton("trend_btn", "Compute yearly trends",
                                            icon = icon("calculator"), class = "btn-success btn-lg"))
            ),
            hr(),
            conditionalPanel(
              condition = "output.trend_done",
              fluidRow(
                valueBoxOutput("vb_base_area", width = 4),
                valueBoxOutput("vb_target_area", width = 4),
                valueBoxOutput("vb_net_change", width = 4)
              ),
              h4("Yearly statistics"),
              DTOutput("yearly_stats_table"),
              downloadButton("download_yearly_csv", "Download table (CSV)", class = "btn-success"),
              hr(),
              h4("Trend panels (forest area, annual loss, cumulative loss/net change, % remaining)"),
              helpText("Dark display version shown below; download for a plain report-ready version."),
              div(class = "dark-panel", plotOutput("trend_panels_dark", height = 750)),
              downloadButton("download_trend_plot", "Download plot (report style, PNG)", class = "btn-success")
            )
          )
        )
      ),

      ########################################################################
      # TAB 4: MAP VISUALISATIONS
      ########################################################################
      tabItem(
        tabName = "maps",
        tabsetPanel(
          id = "map_tabs",

          # 4a. Classified change map
          tabPanel(
            "Classified change map",
            br(),
            fluidRow(
              box(
                title = "Annual classified forest-change map", status = "primary", solidHeader = TRUE, width = 12,
                helpText("Builds a classified map (forest / non-forest / loss / gain / loss+gain / water) ",
                         "for every year from 2000 to the dataset's end year. This can take a little while ",
                         "for large AOIs - it only needs to be built once per session."),
                actionButton("build_annual_stack_btn", "Build annual classified stack",
                             icon = icon("layer-group"), class = "btn-success"),
                conditionalPanel(
                  condition = "output.annual_stack_done",
                  hr(),
                  uiOutput("classified_year_ui"),
                  tabsetPanel(
                    tabPanel(
                      "Interactive map (Leaflet)",
                      br(),
                      chooseSliderSkin("Flat", color = "#006666"),
                      sliderInput("classified_opacity", "Layer transparency:", min = 0, max = 1, value = 0.8, step = 0.05, width = "260px"),
                      leafletOutput("classified_map_leaflet", height = 520)
                    ),
                    tabPanel(
                      "Static map (for download)",
                      br(),
                      plotOutput("classified_map_plot", height = 500)
                    )
                  ),
                  fluidRow(
                    column(6, downloadButton("download_classified_plot", "Download this map (PNG)", class = "btn-success")),
                    column(6, downloadButton("download_classified_raster", "Download this year's raster (GeoTIFF)", class = "btn-success"))
                  ),
                  br(),
                  downloadButton("download_all_annual_layers", "Download ALL annual layers (zipped GeoTIFFs)", class = "btn-success")
                )
              )
            )
          ),

          # 4b. Tree cover % map
          tabPanel(
            "Tree cover",
            br(),
            fluidRow(
              box(
                title = "Percent tree cover for a chosen year", status = "primary", solidHeader = TRUE, width = 12,
                helpText("Requires step 2 (Extract & threshold) to be completed first."),
                uiOutput("treecover_year_ui"),
                actionButton("treecover_plot_btn", "Build tree cover map", icon = icon("tree"), class = "btn-success"),
                conditionalPanel(
                  condition = "output.treecover_done",
                  hr(),
                  tabsetPanel(
                    tabPanel(
                      "Interactive map (Leaflet)",
                      br(),
                      chooseSliderSkin("Flat", color = "#006666"),
                      sliderInput("treecover_opacity", "Layer transparency:", min = 0, max = 1, value = 0.85, step = 0.05, width = "260px"),
                      leafletOutput("treecover_map_leaflet", height = 520)
                    ),
                    tabPanel(
                      "Static map (for download)",
                      br(),
                      plotOutput("treecover_plot", height = 500)
                    )
                  ),
                  fluidRow(
                    column(6, downloadButton("download_treecover_plot", "Download this map (PNG)", class = "btn-success")),
                    column(6, downloadButton("download_treecover_raster", "Download this year's raster (GeoTIFF)", class = "btn-success"))
                  )
                ),
                hr(),
                h4("Compare several years side-by-side"),
                uiOutput("treecover_compare_ui"),
                actionButton("treecover_compare_btn", "Build Comparison", icon = icon("columns"), class = "btn-success"),
                conditionalPanel(
                  condition = "output.treecover_compare_done",
                  plotOutput("treecover_compare_plot", height = 500),
                  downloadButton("download_treecover_compare_plot", "Download comparison (PNG)", class = "btn-success")
                )
              )
            )
          ),

          # 4c. Loss year map
          tabPanel(
            "Loss year map",
            br(),
            fluidRow(
              box(
                title = "Map of the year each forest-loss pixel occurred", status = "primary", solidHeader = TRUE, width = 12,
                helpText("Requires step 2 (Extract & threshold) to be completed first."),
                fluidRow(
                  column(4, radioButtons("loss_palette", "Colour palette:",
                                          choices = c("Continuous (viridis)" = "viridis",
                                                      "One distinct colour per year" = "distinct"),
                                          selected = "viridis")),
                  chooseSliderSkin("Flat", color = "#006666"),
                  column(4, sliderInput("loss_legend_cols", "Legend columns:", min = 1, max = 8, value = 5)),
                  column(4, br(), actionButton("loss_year_plot_btn", "Build loss year map",
                                                icon = icon("calendar-alt"), class = "btn-success"))
                ),
                conditionalPanel(
                  condition = "output.loss_year_done",
                  hr(),
                  tabsetPanel(
                    tabPanel(
                      "Interactive map (Leaflet)",
                      br(),
                      helpText(if (pkg_available("leafem")) "Hover over the map to see the loss year under the cursor."
                               else "Install the 'leafem' package to enable hover-to-see-year on this map."),
                      chooseSliderSkin("Flat", color = "#006666"),
                      sliderInput("loss_year_opacity", "Layer transparency:", min = 0, max = 1, value = 0.85, step = 0.05, width = "260px"),
                      leafletOutput("loss_year_map_leaflet", height = 650)
                    ),
                    tabPanel(
                      "Static map (for download)",
                      br(),
                      plotOutput("loss_year_plot", height = 650)
                    )
                  ),
                  downloadButton("download_loss_year_plot", "Download this map (PNG)", class = "btn-success")
                )
              )
            )
          )
        )
      ),

      ########################################################################
      # TAB 5: MULTI-YEAR FACET MAPS
      ########################################################################
      tabItem(
        tabName = "facets",
        fluidRow(
          box(
            title = "Multi-panel maps across all years", status = "primary", solidHeader = TRUE, width = 12,
            helpText("Requires step 2 (Extract & threshold) to be completed first. This computes a ",
                     "forest/non-forest mask AND an annual-loss mask for every year in the chosen range, ",
                     "then displays them as small-multiple ('faceted') maps. For large AOIs this can be slow - ",
                     "use the aggregation factor to trade off speed for detail (display only; downloaded ",
                     "rasters elsewhere in the app are always full resolution)."),
            fluidRow(
              column(3, numericInput("facet_base_year", "Base year:", value = 2000, min = 2000, max = 2024, step = 1)),
              column(3, numericInput("facet_target_year", "Target year:", value = 2025, min = 2001, max = 2025, step = 1)),
              chooseSliderSkin("Flat", color = "#006666"),
              column(3, sliderInput("facet_agg_factor", "Aggregation factor (display only):", min = 1, max = 20, value = 4)),
              column(3, br(), actionButton("facet_btn", "Build maps", icon = icon("th"), class = "btn-success btn-lg"))
            ),
            hr(),
            conditionalPanel(
              condition = "output.facet_done",
              h4("Forest cover by year"),
              plotOutput("forest_facet_plot", height = 750),
              downloadButton("download_forest_facet_plot", "Download (PNG)", class = "btn-success"),
              hr(),
              h4("Annual forest loss by year"),
              plotOutput("loss_facet_plot", height = 750),
              downloadButton("download_loss_facet_plot", "Download (PNG)", class = "btn-success")
            )
          )
        )
      ),

      ########################################################################
      # TAB 6: ANIMATION
      ########################################################################
      # tabItem(
      #   tabName = "animation",
      #   fluidRow(
      #     box(
      #       title = "Animate forest change through time", status = "primary", solidHeader = TRUE, width = 12,
      #       helpText("Requires the annual classified stack to be built first (Map visualisations > Classified change map). ",
      #                "Rendering an animation can take a while, especially at higher resolution or for many years."),
      #       fluidRow(
      #         column(3, radioButtons("anim_type", "Format:", choices = c("GIF" = "gif", "HTML" = "html"), selected = "gif")),
      #         column(3, numericInput("anim_dpi", "Resolution (dpi):", value = 120, min = 60, max = 300, step = 10)),
      #         column(3, checkboxInput("anim_plot_aoi", "Show AOI outline", value = TRUE)),
      #         column(3, checkboxInput("anim_crop_aoi", "Crop to AOI", value = FALSE))
      #       ),
      #       actionButton("build_animation_btn", "Create animation", icon = icon("film"), class = "btn-success btn-lg"),
      #       hr(),
      #       conditionalPanel(
      #         condition = "output.animation_done",
      #         uiOutput("animation_display"),
      #         downloadButton("download_animation", "Download animation file", class = "btn-success")
      #       )
      #     )
      #   )
      # ),
      
      tabItem(
        tabName = "animation",
        fluidRow(
          box(
            title = "Animate forest change through time", status = "primary", solidHeader = TRUE, width = 12,
            helpText("Requires the annual classified stack to be built first (Map visualisations > Classified change map). ",
                     "Rendering an animation can take a while, especially at higher resolution or for many years."),
            fluidRow(
              column(3, radioButtons("anim_type", "Format:", choices = c("GIF" = "gif", "HTML" = "html"), selected = "gif")),
              column(3, numericInput("anim_dpi", "Resolution (dpi):", value = 120, min = 60, max = 300, step = 10)),
              column(3, checkboxInput("anim_plot_aoi", "Show AOI outline", value = TRUE)),
              column(3, checkboxInput("anim_crop_aoi", "Crop to AOI", value = FALSE))
            ),
            actionButton("build_animation_btn", "Create animation", icon = icon("film"), class = "btn-success btn-lg"),
            hr(),
            conditionalPanel(
              condition = "output.animation_done",
              uiOutput("animation_saved_msg"),
              uiOutput("animation_display")
            )
          )
        )
      ),

      ########################################################################
      # ABOUT TAB
      ########################################################################
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            title = "About This Dashboard", status = "info", solidHeader = TRUE, width = 12,
            h3("Forest Change Analysis Dashboard"),
            p("This dashboard analyses forest cover change using data from the Global Forest Change ",
              "(GFC) project by Hansen et al."),
            h4("Workflow:"),
            tags$ol(
              tags$li("Upload an AOI shapefile and preview it on a satellite map."),
              tags$li("Download and threshold the relevant Hansen GFC tiles for your AOI."),
              tags$li("Compute year-by-year forest cover, loss and net-change statistics."),
              tags$li("Build classified change maps, tree cover maps, and loss-year maps."),
              tags$li("Build multi-panel maps comparing many years at once."),
              tags$li("Animate the full time series as a GIF or HTML slideshow.")
            ),
            h4("Data handling:"),
            p("Every file this dashboard creates - downloaded Hansen tiles, merged/thresholded rasters, ",
              "annual layers, plots, and animations - is written to a private, temporary folder created ",
              "for your session only. It is deleted automatically when the session ends. You never need to ",
              "set or worry about a file path; anything you want to keep, download using the buttons provided."),
            h4("Data Source:"),
            p("Hansen, M. C., P. V. Potapov, R. Moore, M. Hancher, S. A. Turubanova, A. Tyukavina, D. Thau, ",
              "S. V. Stehman, S. J. Goetz, T. R. Loveland, A. Kommareddy, A. Egorov, L. Chini, C. O. Justice, ",
              "and J. R. G. Townshend. 2013. High-Resolution Global Maps of 21st-Century Forest Cover Change. ",
              "Science 342: 850-53."),
            a("Visit GFC website",
              href = "https://glad.earthengine.app/view/global-forest-change#bl=off;old=off;dl=1;lon=-2.8279982953588334;lat=39.4265557409519;zoom=2;",
              target = "_blank", class = "btn btn-info"),
            h4("Maintenance:"),
            p("This dashboard depends on several R packages. If something looks broken after an update to R itself, ",
              "you can check whether newer versions of those packages are available below."),
            actionButton("check_updates", "Check for package updates", icon = icon("rotate")),
            hr(),
            h4("Author:"), p("Ugyen Penjor, Fauna & Flora")
          )
        )
      )
    )
  )
)
}
