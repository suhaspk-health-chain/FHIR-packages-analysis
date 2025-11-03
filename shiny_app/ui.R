# ==============================================================
# ui.R — FHIR Packages Dashboard UI
# ==============================================================

navbarPage(
  title = "FHIR Packages Dashboard",
  theme = bslib::bs_theme(bootswatch = "cosmo"),
  
  # ---- TAB 1: Overview (with filter) ----
  tabPanel("Overview",
           fluidPage(
             # --- FILTER BAR (only visible in Overview tab) ---
             div(
               id = "overview-filterbar",
               style = "padding:12px 16px; margin-bottom:20px; border:1px solid #ddd; border-radius:5px; background:#f9fafb;",
               h4("Filters", style="margin-top:0; color:#f26d21;"),
               fluidRow(
                 column(3, selectizeInput("flt_version", "FHIR version(s)", choices = NULL, multiple = TRUE)),
                 column(3, selectizeInput("flt_status", "Status", choices = NULL, multiple = TRUE)),
                 column(3, selectizeInput("flt_author", "Author", choices = NULL, multiple = TRUE)),
                 column(2, selectizeInput("flt_realm", "Realm", choices = NULL, multiple = TRUE)),
                 column(1, div(style="margin-top:26px;", actionButton("flt_reset", "Reset", class = "btn btn-sm btn-primary")))
               )
             ),
             
             # KPI Cards
             fluidRow(
               column(3, wellPanel(
                 h4("Data Snapshot"),
                 p(strong("Built at: "), textOutput("meta_built_at", inline = TRUE)),
                 p(strong("Source dirs:")),
                 tags$ul(
                   tags$li("processed: ", textOutput("meta_processed", inline = TRUE)),
                   tags$li("raw: ", textOutput("meta_raw", inline = TRUE))
                 )
               )),
               column(9, wellPanel(
                 h4("Key Metrics (filtered)"),
                 fluidRow(
                   column(3, div(h5("Rows"), h3(textOutput("kpi_rows"), style="color:#f26d21;"))),
                   column(3, div(h5("Packages"), h3(textOutput("kpi_packages"), style="color:#f26d21;"))),
                   column(3, div(h5("Resources"), h3(textOutput("kpi_resources"), style="color:#f26d21;"))),
                   column(3, div(h5("Authors"), h3(textOutput("kpi_authors"), style="color:#f26d21;")))
                 )
               ))
             ),
             
             hr(),
             
             # Version Distribution Plot
             fluidRow(
               column(12, 
                      h4("FHIR Version Distribution"),
                      downloadButton("download_plot_versions", "Download Plot", class = "btn-sm btn-info", style="margin-bottom:10px;"),
                      plotOutput("plot_versions", height = "500px")
               )
             ),
             
             br(),
             hr(),
             
             # NEW: Facet Plot by Realm
             fluidRow(
               column(12, 
                      h4("FHIR Version Distribution by Realm"),
                      downloadButton("download_plot_realm_facet", "Download Plot", class = "btn-sm btn-info", style="margin-bottom:10px;"),
                      plotOutput("plot_realm_facet", height = "600px")
               )
             )
           )
  ),
  
  # ---- TAB 2: Authors (no filter) ----
  tabPanel("Authors",
           fluidPage(
             br(),
             fluidRow(
               column(6, 
                      h4("Top Authors by Package Count"),
                      downloadButton("download_plot_authors", "Download Plot", class = "btn-sm btn-info", style="margin-bottom:10px;"),
                      plotOutput("plot_authors", height = "500px")
               ),
               column(6, 
                      h4("Authors Table"),
                      DTOutput("tbl_authors")
               )
             )
           )
  ),
  
  # ---- TAB 3: Evolution (no filter) ----
  tabPanel("Evolution",
           fluidPage(
             br(),
             fluidRow(
               column(6, 
                      h4("Resources Added per Transition"),
                      downloadButton("download_plot_added", "Download Plot", class = "btn-sm btn-info", style="margin-bottom:10px;"),
                      plotOutput("plot_added", height = "450px")
               ),
               column(6, 
                      h4("Resources Removed per Transition"),
                      downloadButton("download_plot_removed", "Download Plot", class = "btn-sm btn-info", style="margin-bottom:10px;"),
                      plotOutput("plot_removed", height = "450px")
               )
             ),
             br(),
             fluidRow(
               column(12, 
                      h4("Transition Details"),
                      DTOutput("tbl_transitions")
               )
             )
           )
  ),
  
  # ---- TAB 4: Resource Changes (NEW - no filter) ----
  tabPanel("Resource Changes",
           fluidPage(
             br(),
             h3("FHIR Resources Additions and Deletions by Name", style="color:#f26d21;"),
             hr(),
             fluidRow(
               column(6,
                      h4("Added Resources", style="color:#28a745;"),
                      DTOutput("tbl_added_resources")
               ),
               column(6,
                      h4("Removed Resources", style="color:#dc3545;"),
                      DTOutput("tbl_removed_resources")
               )
             )
           )
  ),
  
  # ---- TAB 5: Tables (no filter) ----
  tabPanel("Tables",
           fluidPage(
             br(),
             tabsetPanel(
               tabPanel("Resources", 
                        br(),
                        h4("All Resources (Filtered)"),
                        DTOutput("tbl_resources")
               ),
               tabPanel("Presence Matrix", 
                        br(),
                        h4("Resource Presence Matrix"),
                        DTOutput("tbl_matrix")
               ),
               tabPanel("Stable (≥4)", 
                        br(),
                        h4("Stable Resources (Present in ≥4 versions)"),
                        DTOutput("tbl_stable")
               ),
               tabPanel("Raw Preview", 
                        br(),
                        h4("Raw Data Preview (First 100 rows)"),
                        DTOutput("tbl_raw")
               )
             )
           )
  )
)
