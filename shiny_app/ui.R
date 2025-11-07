# ==============================================================
# ui.R — FHIR Packages Dashboard UI (With Version Filter)
# ==============================================================

# Create footer component (will be added to each page)
footer_component <- div(
  style = "margin-top: 50px; padding: 20px; background-color: #f9fafb; border-top: 2px solid #f26d21; text-align: center;",
  fluidRow(
    column(12,
           p(
             "Built with ", icon("heart", style="color:#f26d21;"), " using R Shiny | ",
             "November 2025 | ",
             "Follow: ",
             tags$a(href="https://www.linkedin.com/in/YOUR_LINKEDIN_USERNAME", target="_blank",
                    icon("linkedin", style="color:#0A66C2; margin:0 8px;"), "LinkedIn"),
             tags$a(href="https://YOUR_QUARTO_SITE.com", target="_blank",
                    icon("globe", style="color:#2a5eb4; margin:0 8px;"), "Quarto"),
             tags$a(href="https://github.com/suhaspk-health-chain/FHIR-packages-analysis", target="_blank",
                    icon("github", style="color:#333; margin:0 8px;"), "GitHub"),
             style="font-size:14px; color:#666; margin:0;"
           )
    )
  )
)

navbarPage(
  title = "FHIR Packages Dashboard",
  theme = bslib::bs_theme(bootswatch = "cosmo"),
  
  # Add responsive CSS for navbar
  tags$head(
    tags$style(HTML("
      /* Navbar improvements for responsiveness */
      .navbar {
        flex-wrap: wrap;
      }
      
      .navbar-brand {
        padding-right: 15px !important;
        font-weight: bold;
        color: #0c223f !important;
      }
      
      .navbar-nav {
        flex-direction: row;
        align-items: center;
      }
      
      .navbar-nav .nav-item {
        padding: 0 5px;
        white-space: nowrap;
      }
      
      .navbar-nav .nav-link {
        padding: 0.5rem 0.75rem !important;
        font-size: 14px;
      }
      
      /* Responsive adjustments */
      @media (max-width: 992px) {
        .navbar-nav .nav-link {
          font-size: 13px;
          padding: 0.5rem 0.5rem !important;
        }
        
        .navbar-brand {
          font-size: 18px;
          padding-right: 10px !important;
        }
      }
      
      @media (max-width: 768px) {
        .navbar-brand {
          font-size: 16px;
        }
        
        .navbar-nav .nav-link {
          font-size: 12px;
          padding: 0.4rem 0.4rem !important;
        }
      }
      
      /* Collapse button improvements */
      .navbar-toggler {
        padding: 0.25rem 0.5rem;
        border: 1px solid rgba(0,0,0,.1);
      }
      
      .navbar-toggler:focus {
        outline: none;
        box-shadow: none;
      }
      
      /* Tab text improvements */
      .nav-link {
        color: #0c223f !important;
        font-weight: 500;
        transition: color 0.3s ease;
      }
      
      .nav-link:hover {
        color: #f26d21 !important;
        background-color: rgba(242, 109, 33, 0.1);
        border-radius: 4px;
      }
      
      .nav-link.active {
        color: #f26d21 !important;
        font-weight: bold;
        border-bottom: 3px solid #f26d21;
      }
      
      /* Fix navbar collapse to the right */
      .navbar-collapse {
        order: 3;
        flex-basis: 100%;
      }
      
      @media (min-width: 768px) {
        .navbar-collapse {
          order: 2;
          flex-basis: auto;
        }
      }
    "))
  ),
  
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
                          tags$li(strong("Identify Patterns:"), " Discover relationships between different categorical variables through bar charts (with optional faceting) and heatmaps."),
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
                        
                        # Plot Type Selection
                        selectInput(
                          "plot_type",
                          "Select Plot Type:",
                          choices = c(
                            "Bar Chart" = "bar",
                            "Heatmap" = "heatmap"
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
                        
                        # Show Y variable for heatmap
                        conditionalPanel(
                          condition = "input.plot_type == 'heatmap'",
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
                        
                        # Facet option for bar charts
                        conditionalPanel(
                          condition = "input.plot_type == 'bar'",
                          selectInput(
                            "facet_var",
                            "Facet By (Optional):",
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
                        
                        numericInput(
                          "top_n",
                          "Top N Values:",
                          value = 15,
                          min = 5,
                          max = 30,
                          step = 5
                        ),
                        
                        hr(),
                        
                        h5("Data Filters", style="color:#0c223f; font-weight:bold; margin-bottom:15px;"),
                        
                        # NEW: FHIR Version Filter
                        selectizeInput(
                          "version_filter_plot",
                          "Filter by FHIR Version (optional):",
                          choices = NULL,
                          multiple = TRUE,
                          options = list(
                            placeholder = 'Select FHIR versions to filter',
                            plugins = list('remove_button')
                          )
                        ),
                        
                        # Existing Realm Filter
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
             ),
             
             # Footer
             footer_component
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
             
             # Version Selection Controls
             fluidRow(
               column(12,
                      wellPanel(
                        style = "background:#f9fafb; border:2px solid #ddd; padding:15px;",
                        h4("Filter Transitions", style="color:#f26d21; margin-top:0;"),
                        fluidRow(
                          column(6,
                                 selectizeInput(
                                   "version_filter",
                                   "Select Version (optional):",
                                   choices = NULL,
                                   multiple = TRUE,
                                   options = list(
                                     placeholder = 'Leave empty to show all transitions',
                                     plugins = list('remove_button')
                                   )
                                 )
                          ),
                          column(6,
                                 p(strong("Info:"), " Select one or more versions to filter transitions.",
                                   style="margin-top:25px; color:#666; font-size:14px;")
                          )
                        )
                      )
               )
             ),
             
             br(),
             
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
             ),
             
             # Footer
             footer_component
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
             ),
             
             # Footer
             footer_component
           )
  ),
  
  # ---- TAB 4: About ----
  tabPanel("About",
           fluidPage(
             br(),
             
             # About the Project Section
             fluidRow(
               column(12,
                      wellPanel(
                        style = "background:#ffffff; border:2px solid #f26d21; padding:25px;",
                        h2(icon("info-circle"), " About This Project", style="color:#f26d21; margin-top:0;"),
                        hr(style="border-color:#f26d21;"),
                        
                        h3("Project Overview", style="color:#0c223f;"),
                        p(style="font-size:16px; line-height:1.8;",
                          "The FHIR Packages Dashboard is an interactive data exploration tool designed to analyze and visualize
              the distribution and evolution of Fast Healthcare Interoperability Resources (FHIR) packages across
              different versions, implementation guides, and global healthcare realms."
                        ),
                        
                        br(),
                        
                        h3("Key Features", style="color:#0c223f;"),
                        tags$ul(
                          style="font-size:16px; line-height:1.8;",
                          tags$li(strong("Interactive Visualization:"), " Generate custom plots with bar charts (optionally faceted) and heatmaps for clear data exploration."),
                          tags$li(strong("Version Analysis:"), " Track resource evolution across FHIR versions (DSTU2, STU3, R4, R4B, R5, R6)."),
                          tags$li(strong("Global Coverage:"), " Analyze packages by realm (country/region) and implementing organization."),
                          tags$li(strong("Dynamic Filtering:"), " Filter data by FHIR version and realm for focused analysis."),
                          tags$li(strong("Export Capabilities:"), " Download high-quality plots and export data tables in multiple formats.")
                        ),
                        
                        br(),
                        
                        h3("Technology Stack", style="color:#0c223f;"),
                        fluidRow(
                          column(4,
                                 tags$ul(
                                   style="font-size:15px;",
                                   tags$li(icon("r-project"), strong(" R & Shiny")),
                                   tags$li(icon("chart-line"), strong(" ggplot2")),
                                   tags$li(icon("table"), strong(" DT (DataTables)"))
                                 )
                          ),
                          column(4,
                                 tags$ul(
                                   style="font-size:15px;",
                                   tags$li(icon("code"), strong(" tidyverse")),
                                   tags$li(icon("database"), strong(" JSON/CSV")),
                                   tags$li(icon("palette"), strong(" RColorBrewer"))
                                 )
                          ),
                          column(4,
                                 tags$ul(
                                   style="font-size:15px;",
                                   tags$li(icon("bootstrap"), strong(" Bootstrap")),
                                   tags$li(icon("fire"), strong(" FHIR Standard")),
                                   tags$li(icon("hospital"), strong(" HL7 International"))
                                 )
                          )
                        ),
                        
                        br(),
                        
                        h3("Author Information", style="color:#0c223f;"),
                        p(style="font-size:16px; line-height:1.8;",
                          strong("Developed by:"), " [Your Name]", br(),
                          strong("Organization:"), " [Your Organization/Institution]", br(),
                          strong("Contact:"), " [Your Email]", br(),
                          strong("Date:"), " November 2025", br(),
                          strong("Version:"), " 1.0.0"
                        ),
                        
                        br(),
                        
                        h3("Connect With Me", style="color:#0c223f;"),
                        p(style="font-size:16px; line-height:2;",
                          tags$a(
                            href="https://www.linkedin.com/in/YOUR_LINKEDIN_USERNAME",
                            target="_blank",
                            icon("linkedin", style="font-size:24px; color:#0A66C2; margin-right:10px;"),
                            strong("LinkedIn"),
                            style="text-decoration:none; color:#0c223f;"
                          ),
                          br(),
                          tags$a(
                            href="https://YOUR_QUARTO_SITE.com",
                            target="_blank",
                            icon("globe", style="font-size:24px; color:#2a5eb4; margin-right:10px;"),
                            strong("Quarto Blog"),
                            style="text-decoration:none; color:#0c223f;"
                          ),
                          br(),
                          tags$a(
                            href="https://github.com/suhaspk-health-chain/FHIR-packages-analysis",
                            target="_blank",
                            icon("github", style="font-size:24px; color:#333; margin-right:10px;"),
                            strong("GitHub Repository"),
                            style="text-decoration:none; color:#0c223f;"
                          )
                        ),
                        
                        br(),
                        
                        h3("GitHub Contributions", style="color:#0c223f;"),
                        p(style="font-size:16px; line-height:1.6; color:#666;",
                          "This is an open-source project! Your contributions are welcome and appreciated.
              Whether you've found a bug, have a feature request, or want to contribute code,
              we'd love to hear from you."
                        ),
                        
                        br(),
                        
                        # GitHub Contribution Cards
                        fluidRow(
                          # Issues Card
                          column(6,
                                 div(
                                   style="background: linear-gradient(135deg, #f6f8fa 0%, #ffffff 100%);
                         border: 2px solid #0969da; border-radius: 10px; padding: 25px;
                         min-height: 220px; transition: all 0.3s ease;
                         box-shadow: 0 2px 8px rgba(9,105,218,0.1);",
                                   onmouseover="this.style.boxShadow='0 4px 16px rgba(9,105,218,0.2)'; this.style.transform='translateY(-2px)';",
                                   onmouseout="this.style.boxShadow='0 2px 8px rgba(9,105,218,0.1)'; this.style.transform='translateY(0)';",
                                   
                                   div(
                                     style="text-align:center; margin-bottom:15px;",
                                     icon("exclamation-circle", style="font-size:48px; color:#d1242f;")
                                   ),
                                   
                                   h4("Report an Issue", style="text-align:center; color:#0c223f; margin-bottom:15px;"),
                                   
                                   p(style="font-size:14px; text-align:center; color:#666; margin-bottom:20px;",
                                     "Found a bug or have a suggestion? Open an issue on GitHub and help us improve!"
                                   ),
                                   
                                   div(
                                     style="text-align:center;",
                                     tags$a(
                                       href="https://github.com/suhaspk-health-chain/FHIR-packages-analysis/issues",
                                       target="_blank",
                                       class="btn btn-danger btn-lg",
                                       style="width:80%; font-size:16px; font-weight:bold;",
                                       icon("github"), " Open Issue"
                                     )
                                   )
                                 )
                          ),
                          
                          # Pull Request Card
                          column(6,
                                 div(
                                   style="background: linear-gradient(135deg, #f6f8fa 0%, #ffffff 100%);
                         border: 2px solid #0969da; border-radius: 10px; padding: 25px;
                         min-height: 220px; transition: all 0.3s ease;
                         box-shadow: 0 2px 8px rgba(9,105,218,0.1);",
                                   onmouseover="this.style.boxShadow='0 4px 16px rgba(9,105,218,0.2)'; this.style.transform='translateY(-2px)';",
                                   onmouseout="this.style.boxShadow='0 2px 8px rgba(9,105,218,0.1)'; this.style.transform='translateY(0)';",
                                   
                                   div(
                                     style="text-align:center; margin-bottom:15px;",
                                     icon("code-branch", style="font-size:48px; color:#1f883d;")
                                   ),
                                   
                                   h4("Submit Pull Request", style="text-align:center; color:#0c223f; margin-bottom:15px;"),
                                   
                                   p(style="font-size:14px; text-align:center; color:#666; margin-bottom:20px;",
                                     "Have improvements to contribute? Fork the repo and submit a pull request!"
                                   ),
                                   
                                   div(
                                     style="text-align:center;",
                                     tags$a(
                                       href="https://github.com/suhaspk-health-chain/FHIR-packages-analysis/pulls",
                                       target="_blank",
                                       class="btn btn-success btn-lg",
                                       style="width:80%; font-size:16px; font-weight:bold;",
                                       icon("code-branch"), " Create PR"
                                     )
                                   )
                                 )
                          )
                        ),
                        
                        br(),
                        
                        # GitHub Repository Info
                        div(
                          style="background:#f6f8fa; padding:20px; border-radius:8px; border-left:4px solid #0969da; margin-top:20px;",
                          fluidRow(
                            column(2,
                                   div(style="text-align:center; padding-top:10px;",
                                       icon("github", style="font-size:48px; color:#333;")
                                   )
                            ),
                            column(10,
                                   h5(
                                     tags$a(
                                       href="https://github.com/suhaspk-health-chain/FHIR-packages-analysis",
                                       target="_blank",
                                       "suhaspk-health-chain/FHIR-packages-analysis",
                                       style="color:#0969da; text-decoration:none; font-weight:bold;"
                                     ),
                                     style="margin-top:0; margin-bottom:10px;"
                                   ),
                                   p(style="font-size:14px; color:#666; margin-bottom:10px;",
                                     "View the complete source code, documentation, and project roadmap on GitHub."
                                   ),
                                   tags$a(
                                     href="https://github.com/suhaspk-health-chain/FHIR-packages-analysis",
                                     target="_blank",
                                     icon("external-link-alt"), " View Repository",
                                     style="color:#0969da; font-size:14px; font-weight:500;"
                                   )
                            )
                          )
                        ),
                        
                        br(),
                        
                        # Contribution Guidelines
                        div(
                          style="background:#fff3cd; padding:15px; border-radius:6px; border:1px solid #ffc107;",
                          p(
                            icon("info-circle", style="color:#856404; margin-right:8px;"),
                            strong("New to contributing?"),
                            " Check out our ",
                            tags$a(
                              href="https://github.com/suhaspk-health-chain/FHIR-packages-analysis/blob/main/CONTRIBUTING.md",
                              target="_blank",
                              "Contributing Guidelines",
                              style="color:#0969da;"
                            ),
                            " to get started!",
                            style="font-size:14px; color:#856404; margin:0;"
                          )
                        ),
                        
                        br()
                      )
               )
             ),
             
             # Data Sources Section
             fluidRow(
               column(12,
                      wellPanel(
                        style = "background:#f9fafb; border:2px solid #0c223f; padding:25px;",
                        h2(icon("database"), " Data Sources & Resources", style="color:#0c223f; margin-top:0;"),
                        hr(style="border-color:#0c223f;"),
                        
                        h3("Primary Data Source", style="color:#f26d21;"),
                        div(
                          style="background:#ffffff; padding:15px; border-left:4px solid #f26d21; margin-bottom:20px;",
                          h4(tags$a(
                            href="https://packages2.fhir.org/xig",
                            target="_blank",
                            icon("external-link-alt"), " FHIR Implementation Guide Statistics",
                            style="color:#f26d21;"
                          )),
                          p(style="font-size:15px; margin-bottom:0;",
                            "Official FHIR package registry containing comprehensive statistics on 75,000+ resources
                across all published implementation guides."
                          )
                        ),
                        
                        br(),
                        
                        h3("FHIR Resource Specifications", style="color:#f26d21;"),
                        p(style="font-size:15px; margin-bottom:15px;",
                          "Official FHIR resource list documentation for each version standard:"
                        ),
                        
                        fluidRow(
                          column(6,
                                 div(
                                   style="background:#ffffff; padding:12px; margin-bottom:10px; border-radius:5px;",
                                   h5(
                                     tags$a(href="https://hl7.org/fhir/DSTU2/resourcelist.html", target="_blank",
                                            icon("file-alt"), " FHIR DSTU2 (R2)", style="color:#0c223f;"),
                                     style="margin:0;"
                                   )
                                 ),
                                 div(
                                   style="background:#ffffff; padding:12px; margin-bottom:10px; border-radius:5px;",
                                   h5(
                                     tags$a(href="https://hl7.org/fhir/STU3/resourcelist.html", target="_blank",
                                            icon("file-alt"), " FHIR STU3 (R3)", style="color:#0c223f;"),
                                     style="margin:0;"
                                   )
                                 ),
                                 div(
                                   style="background:#ffffff; padding:12px; margin-bottom:10px; border-radius:5px;",
                                   h5(
                                     tags$a(href="https://hl7.org/fhir/R4/resourcelist.html", target="_blank",
                                            icon("file-alt"), " FHIR R4", style="color:#0c223f;"),
                                     style="margin:0;"
                                   )
                                 )
                          ),
                          column(6,
                                 div(
                                   style="background:#ffffff; padding:12px; margin-bottom:10px; border-radius:5px;",
                                   h5(
                                     tags$a(href="https://hl7.org/fhir/R4B/resourcelist.html", target="_blank",
                                            icon("file-alt"), " FHIR R4B", style="color:#0c223f;"),
                                     style="margin:0;"
                                   )
                                 ),
                                 div(
                                   style="background:#ffffff; padding:12px; margin-bottom:10px; border-radius:5px;",
                                   h5(
                                     tags$a(href="https://hl7.org/fhir/R5/resourcelist.html", target="_blank",
                                            icon("file-alt"), " FHIR R5", style="color:#0c223f;"),
                                     style="margin:0;"
                                   )
                                 ),
                                 div(
                                   style="background:#ffffff; padding:12px; margin-bottom:10px; border-radius:5px;",
                                   h5(
                                     tags$a(href="https://build.fhir.org/resourcelist.html", target="_blank",
                                            icon("file-alt"), " FHIR R6 (Build)", style="color:#0c223f;"),
                                     style="margin:0;"
                                   )
                                 )
                          )
                        ),
                        
                        br()
                      )
               )
             ),
             
             # Footer
             footer_component
           )
  )
)
