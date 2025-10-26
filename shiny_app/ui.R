# ==============================================================
# ui.R — FHIR Packages Dashboard UI
# ==============================================================

navbarPage(
  title = "FHIR Packages Dashboard",
  theme = bslib::bs_theme(bootswatch = "cosmo"),
  header = tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom-dark.css"),
    
    # --- GLOBAL FILTER BAR (always visible) ---
    div(
      id = "global-filterbar",
      style = "padding:8px 16px; border-bottom:1px solid #e5e5e5; background:#f9fafb;",
      fluidRow(
        column(3, selectizeInput("flt_version", "FHIR version(s)", choices = NULL, multiple = TRUE)),
        column(3, selectizeInput("flt_status",  "Status",          choices = NULL, multiple = TRUE)),
        column(3, selectizeInput("flt_author",  "Author",          choices = NULL, multiple = TRUE)),
        column(2, selectizeInput("flt_realm",   "Realm",           choices = NULL, multiple = TRUE)),
        column(1, div(style="margin-top:26px;", actionButton("flt_reset", "Reset", class = "btn btn-sm btn-primary")))
      )
    )
  ),
  
  tabPanel("Overview",
           fluidPage(
             br(),
             fluidRow(
               column(3, wellPanel(
                 h4("Data Snapshot"),
                 p(strong("Built at: "), if (!is.null(meta$built_at)) format(meta$built_at, "%Y-%m-%d %H:%M") else "n/a"),
                 p(strong("Source dirs:")),
                 tags$ul(
                   tags$li("processed: ", meta$source_dirs$processed %||% "n/a"),
                   tags$li("raw: ",       meta$source_dirs$raw %||% "n/a")
                 )
               )),
               column(9, wellPanel(
                 h4("Key Metrics (filtered)"),
                 fluidRow(
                   column(3, div(h5("Rows"),        textOutput("kpi_rows"))),
                   column(3, div(h5("Packages"),    textOutput("kpi_packages"))),
                   column(3, div(h5("Resources"),   textOutput("kpi_resources"))),
                   column(3, div(h5("Authors (≠ empty)"), textOutput("kpi_authors")))
                 )
               ))
             ),
             hr(),
             fluidRow(column(12, h4("FHIR Version Distribution"), plotOutput("plot_versions", height = "360px")))
           )
  ),
  
  tabPanel("Authors",
           fluidPage(
             br(),
             fluidRow(
               column(6, plotOutput("plot_authors", height = "420px")),
               column(6, DTOutput("tbl_authors"))
             )
           )
  ),
  
  tabPanel("Evolution",
           fluidPage(
             br(),
             fluidRow(
               column(6, plotOutput("plot_added", height = "320px")),
               column(6, plotOutput("plot_removed", height = "320px"))
             ),
             br(),
             fluidRow(column(12, DTOutput("tbl_transitions")))
           )
  ),
  
  tabPanel("Tables",
           fluidPage(
             br(),
             tabsetPanel(
               tabPanel("Resources",       DTOutput("tbl_resources")),
               tabPanel("Presence Matrix", DTOutput("tbl_matrix")),
               tabPanel("Stable (≥4)",     DTOutput("tbl_stable")),
               tabPanel("Raw Preview",     DTOutput("tbl_raw"))
             )
           )
  )
)
