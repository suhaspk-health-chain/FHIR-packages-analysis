# ==============================================================
# ui.R — FHIR Packages Dashboard UI
# ==============================================================

navbarPage(
  title = "FHIR Packages Dashboard",
  theme = bslib::bs_theme(bootswatch = "cosmo"),
  
  # ---- TAB 1: Overview ----
  tabPanel("Overview",
           fluidPage(
             br(),
             
             # Introduction Section
             fluidRow(
               column(12,
                      wellPanel(
                        style = "background:#ffffff; border:2px solid #f26d21; padding:20px;",
                        h2("Welcome to the FHIR Packages Dashboard", style="color:#f26d21; text-align:center;"),
                        hr(style="border-color:#f26d21;"),
                        h4("Introduction"),
                        p(style="font-size:16px; line-height:1.6;",
                          "This dashboard provides comprehensive insights into FHIR (Fast Healthcare Interoperability Resources) packages 
              across different versions, realms, and implementation statuses. FHIR is a standard for healthcare data exchange, 
              and this tool helps analyze the distribution and evolution of FHIR packages globally."
                        ),
                        br(),
                        h4("Objectives"),
                        tags$ul(
                          style="font-size:16px; line-height:1.8;",
                          tags$li(strong("Visualize Package Distribution:"), " Explore how FHIR packages are distributed across versions, realms (countries/regions), statuses, and authors."),
                          tags$li(strong("Track Resource Evolution:"), " Monitor additions and deletions of FHIR resources across version transitions."),
                          tags$li(strong("Identify Patterns:"), " Discover relationships between different categorical variables through various chart types including heatmaps and correlation matrices."),
                          tags$li(strong("Support Decision Making:"), " Provide data-driven insights for healthcare IT professionals, standards developers, and implementation teams.")
                        )
                      )
               )
             ),
             
             br(),
             
             # Key Metrics Section
             fluidRow(
               column(12,
                      h3("Key Metrics", style="color:#f26d21; text-align:center; margin-bottom:20px;")
               )
             ),
             
             fluidRow(
               column(3, wellPanel(
                 style="background:#f9fafb; text-align:center;",
                 h4("Total Rows"),
                 h2(textOutput("kpi_rows"), style="color:#f26d21; font-weight:bold;"),
                 p("Data entries analyzed", style="color:#666; font-size:14px;")
               )),
               column(3, wellPanel(
                 style="background:#f9fafb; text-align:center;",
                 h4("Packages"),
                 h2(textOutput("kpi_packages"), style="color:#f26d21; font-weight:bold;"),
                 p("Unique FHIR packages", style="color:#666; font-size:14px;")
               )),
               column(3, wellPanel(
                 style="background:#f9fafb; text-align:center;",
                 h4("Resources"),
                 h2(textOutput("kpi_resources"), style="color:#f26d21; font-weight:bold;"),
                 p("Distinct resource types", style="color:#666; font-size:14px;")
               )),
               column(3, wellPanel(
                 style="background:#f9fafb; text-align:center;",
                 h4("Authors"),
                 h2(textOutput("kpi_authors"), style="color:#f26d21; font-weight:bold;"),
                 p("Contributing organizations", style="color:#666; font-size:14px;")
               ))
             ),
             
             fluidRow(
               column(12, wellPanel(
                 style="background:#f0f8ff; border:1px solid #d0e8f7; padding:10px;",
                 p(strong("Data Snapshot:"), 
                   " Built at ", textOutput("meta_built_at", inline = TRUE),
                   " | Processed: ", textOutput("meta_processed", inline = TRUE),
                   " | Raw: ", textOutput("meta_raw", inline = TRUE),
                   style="margin:0; color:#0c223f;")
               ))
             ),
             
             hr(style="border-top: 2px solid #f26d21; margin:30px 0;"),
             
             # Visualization Section Title
             fluidRow(
               column(12,
                      h3("Interactive Visualization", style="color:#f26d21; text-align:center; margin-bottom:20px;"),
                      p("Configure your visualization parameters on the left and generate custom plots to explore the data.",
                        style="text-align:center; color:#666; font-size:16px; margin-bottom:30px;")
               )
             ),
             
             # Plot Controls (40%) and Plot Output (60%) Side by Side
             fluidRow(
               # LEFT COLUMN: Plot Controls (40%)
               column(5,
                      wellPanel(
                        style = "background:#f9fafb; border:2px solid #ddd; padding:20px; height:850px; overflow-y:auto;",
                        h4("Plot Configuration", style="color:#f26d21; margin-top:0; text-align:center;"),
                        hr(),
                        
                        selectInput(
                          "plot_type",
                          "Select Plot Type:",
                          choices = c(
                            "Bar Chart" = "bar",
                            "Pie Chart" = "pie",
                            "Grouped Bars" = "grouped",
                            "Stacked Bars" = "stacked",
                            "Scatter Plot" = "scatter",
                            "Heatmap" = "heatmap",
                            "Correlation Matrix" = "correlation"
                          ),
                          selected = "bar"
                        ),
                        
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
                        ),
                        
                        conditionalPanel(
                          condition = "input.plot_type != 'bar' && input.plot_type != 'pie' && input.plot_type != 'correlation'",
                          selectInput(
                            "plot_y_var",
                            "Y-Axis Variable:",
                            choices = c(
                              "FHIR Version" = "version",
                              "Realm" = "realm",
                              "Status" = "status",
                              "Author" = "auth"
                            ),
                            selected = "realm"
                          )
                        ),
                        
                        conditionalPanel(
                          condition = "input.plot_type == 'grouped' || input.plot_type == 'stacked'",
                          selectInput(
                            "facet_var",
                            "Facet By:",
                            choices = c(
                              "None" = "none",
                              "FHIR Version" = "version",
                              "Realm" = "realm",
                              "Status" = "status",
                              "Author" = "auth"
                            ),
                            selected = "none"
                          )
                        ),
                        
                        conditionalPanel(
                          condition = "input.plot_type == 'scatter'",
                          checkboxInput(
                            "show_trend",
                            "Show Trend Line",
                            value = TRUE
                          )
                        ),
                        
                        numericInput(
                          "top_n",
                          "Top N Values:",
                          value = 15,
                          min = 5,
                          max = 30,
                          step = 5
                        ),
                        
                        hr(),
                        
                        selectizeInput(
                          "realm_filter",
                          "Filter by Realm (optional):",
                          choices = NULL,
                          multiple = TRUE,
                          options = list(
                            placeholder = 'Select realms to filter',
                            plugins = list('remove_button')
                          )
                        ),
                        
                        hr(),
                        
                        actionButton(
                          "generate_plot",
                          "Generate Plot",
                          class = "btn-primary btn-lg btn-block",
                          icon = icon("chart-bar"),
                          style = "margin-top:20px; font-size:18px; padding:12px;"
                        )
                      )
               ),
               
               # RIGHT COLUMN: Plot Output (60%)
               column(7,
                      wellPanel(
                        style = "background:#ffffff; border:2px solid #ddd; padding:15px; min-height:850px;",
                        conditionalPanel(
                          condition = "output.plot_generated",
                          downloadButton("download_custom_plot", "Download Plot", 
                                         class = "btn-sm btn-success", 
                                         style="margin-bottom:15px; float:right;")
                        ),
                        plotOutput("custom_plot", height = "800px")
                      )
               )
             )
           )
  ),
  
  # ---- TAB 2: Evolution ----
  tabPanel("Evolution",
           fluidPage(
             br(),
             h3("FHIR Resource Evolution", style="color:#f26d21; text-align:center;"),
             p("Track how FHIR resources have been added and removed across version transitions.",
               style="text-align:center; color:#666; margin-bottom:30px;"),
             hr(),
             
             # Charts Section
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
             hr(style="border-top: 2px solid #f26d21; margin:30px 0;"),
             
             # Transition Summary Table
             fluidRow(
               column(12, 
                      h4("Transition Summary", style="color:#f26d21;"),
                      p("Overview of resource changes across version transitions.", style="color:#666;"),
                      DTOutput("tbl_transitions")
               )
             ),
             
             br(),
             hr(style="border-top: 2px solid #f26d21; margin:30px 0;"),
             
             # Detailed Resource Changes Table
             fluidRow(
               column(12,
                      h3("Detailed Resource Changes by Name", style="color:#f26d21; text-align:center;"),
                      p("Complete list of all resources added and removed for each version transition.",
                        style="text-align:center; color:#666; margin-bottom:30px;")
               )
             ),
             
             fluidRow(
               column(12,
                      wellPanel(
                        style="background:#f9fafb; padding:20px;",
                        DTOutput("tbl_detailed_resource_changes")
                      )
               )
             )
           )
  ),
  
  # ---- TAB 3: Data Tables ----
  tabPanel("Data Tables",
           fluidPage(
             br(),
             h3("Raw Data Tables", style="color:#f26d21; text-align:center;"),
             p("Explore the underlying data in tabular format.",
               style="text-align:center; color:#666; margin-bottom:30px;"),
             hr(),
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
