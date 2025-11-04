# ==============================================================
# ui.R — FHIR Packages Dashboard UI
# ==============================================================

navbarPage(
  title = "FHIR Packages Dashboard",
  theme = bslib::bs_theme(bootswatch = "cosmo"),
  
  # ---- TAB 1: Overview (with interactive plot controls) ----
  tabPanel("Overview",
           fluidPage(
             br(),
             
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
                 h4("Key Metrics"),
                 fluidRow(
                   column(3, div(h5("Rows"), h3(textOutput("kpi_rows"), style="color:#f26d21;"))),
                   column(3, div(h5("Packages"), h3(textOutput("kpi_packages"), style="color:#f26d21;"))),
                   column(3, div(h5("Resources"), h3(textOutput("kpi_resources"), style="color:#f26d21;"))),
                   column(3, div(h5("Authors"), h3(textOutput("kpi_authors"), style="color:#f26d21;")))
                 )
               ))
             ),
             
             hr(),
             
             # Interactive Plot Controls
             fluidRow(
               column(12,
                      wellPanel(
                        style = "background:#f9fafb; border:1px solid #ddd;",
                        h4("Generate Custom Plot", style="color:#f26d21; margin-top:0;"),
                        fluidRow(
                          column(3,
                                 selectInput(
                                   "plot_x_var",
                                   "X-Axis Variable:",
                                   choices = c(
                                     "FHIR Version" = "version",
                                     "Realm" = "realm",
                                     "Status" = "status",
                                     "Author" = "auth"
                                   ),
                                   selected = "version"
                                 )
                          ),
                          column(3,
                                 selectInput(
                                   "plot_type",
                                   "Plot Type:",
                                   choices = c(
                                     "Bar Chart" = "bar",
                                     "Grouped by Version" = "grouped",
                                     "Stacked Bar" = "stacked"
                                   ),
                                   selected = "bar"
                                 )
                          ),
                          column(3,
                                 conditionalPanel(
                                   condition = "input.plot_type == 'grouped' || input.plot_type == 'stacked'",
                                   selectInput(
                                     "plot_fill_var",
                                     "Group/Fill Variable:",
                                     choices = c(
                                       "FHIR Version" = "version",
                                       "Realm" = "realm",
                                       "Status" = "status"
                                     ),
                                     selected = "version"
                                   )
                                 )
                          ),
                          column(3,
                                 div(style="margin-top:25px;",
                                     actionButton(
                                       "generate_plot",
                                       "Generate Plot",
                                       class = "btn-primary btn-lg",
                                       icon = icon("chart-bar"),
                                       width = "100%"
                                     )
                                 )
                          )
                        )
                      )
               )
             ),
             
             # Plot Output Area
             fluidRow(
               column(12,
                      conditionalPanel(
                        condition = "output.plot_generated",
                        downloadButton("download_custom_plot", "Download Plot", class = "btn-sm btn-info", style="margin-bottom:10px;")
                      ),
                      plotOutput("custom_plot", height = "550px")
               )
             )
           )
  ),
  
  # ---- TAB 2: Authors ----
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
  
  # ---- TAB 3: Evolution ----
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
  
  # ---- TAB 4: Resource Changes ----
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
  
  # ---- TAB 5: Tables ----
  tabPanel("Tables",
           fluidPage(
             br(),
             tabsetPanel(
               tabPanel("Resources", 
                        br(),
                        h4("All Resources"),
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
