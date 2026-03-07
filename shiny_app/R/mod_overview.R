# ==============================================================
# mod_overview.R - FIXED
# ==============================================================

mod_overview_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    br(),
    
    # Introduction
    fluidRow(
      column(12,
             wellPanel(
               style = "background:#ffffff; border:2px solid #f26d21; padding:24px;",
               h2("FHIR Packages Ecosystem Explorer", style="color:#f26d21; text-align:center; margin-bottom:4px;"),
               p("A beginner-friendly look at how the global healthcare interoperability community uses FHIR",
                 style="text-align:center; color:#666; font-size:15px; margin-bottom:8px;"),
               p(
                 tags$a(
                   href   = "https://rpubs.com/suhasPK/xig-FHIR-resources-eda",
                   target = "_blank",
                   style  = "color:#f26d21; font-weight:600; font-size:14px; text-decoration:none;",
                   icon("file-alt"), " Read the full EDA report on RPubs"
                 ),
                 style = "text-align:center; margin-bottom:14px;"
               ),
               hr(style="border-color:#f26d21;"),

               fluidRow(
                 column(6,
                   h4(icon("question-circle"), " What is FHIR?", style="color:#0c223f;"),
                   p(style="font-size:15px; line-height:1.7;",
                     strong("FHIR"), " (Fast Healthcare Interoperability Resources) is an international standard
                     for exchanging healthcare data electronically. Think of it as a ", strong("universal language"),
                     " that lets hospital systems, insurance apps, and government health agencies all talk to each other."
                   ),
                   p(style="font-size:15px; line-height:1.7;",
                     "A ", strong("FHIR Package"), " is like a published rulebook that bundles together the exact
                     definitions, value lists, and data templates that a specific healthcare system agrees to follow.
                     This dataset captures ", strong("every public FHIR package"), " from the global registry."
                   )
                 ),
                 column(6,
                   h4(icon("map"), " How to Read This Dashboard", style="color:#0c223f;"),
                   tags$ul(style="font-size:15px; line-height:2.0;",
                     tags$li(icon("layer-group"), " ",
                       strong("Overview (this page)"), " - Big-picture numbers and what they mean"),
                     tags$li(icon("code-branch"), " ",
                       strong("Evolution"), " - How FHIR resource types have changed across versions"),
                     tags$li(icon("globe"), " ",
                       strong("Global Landscape"), " - Which countries contribute the most IGs"),
                     tags$li(icon("sitemap"), " ",
                       strong("Data Hierarchy"), " - How packages nest into resource types"),
                     tags$li(icon("flag"), " ",
                       strong("US Deep Dive"), " - The United States is the biggest contributor"),
                     tags$li(icon("table"), " ",
                       strong("Data Tables"), " - Browse the raw data yourself")
                   )
                 )
               )
             )
      )
    ),
    
    br(),
    
    # Key Metrics
    fluidRow(
      column(12, h3(icon("chart-bar"), " At a Glance", style="color:#f26d21; text-align:center; margin-bottom:20px;"))
    ),

    fluidRow(
      column(3, wellPanel(
        style="background:#fff8f3; border:2px solid #f26d21; text-align:center; padding:18px;",
        tags$i(class="fa fa-database fa-2x", style="color:#f26d21;"),
        h4("Total Resource Entries", style="margin-top:8px; color:#0c223f;"),
        h2(textOutput(ns("kpi_rows")), style="color:#f26d21; font-weight:bold; font-size:2em;"),
        p("Individual resource definitions scraped from the global FHIR package registry",
          style="color:#666; font-size:13px; line-height:1.4;")
      )),
      column(3, wellPanel(
        style="background:#f3f8ff; border:2px solid #0c223f; text-align:center; padding:18px;",
        tags$i(class="fa fa-boxes fa-2x", style="color:#0c223f;"),
        h4("Published Packages", style="margin-top:8px; color:#0c223f;"),
        h2(textOutput(ns("kpi_packages")), style="color:#0c223f; font-weight:bold; font-size:2em;"),
        p("Unique FHIR Implementation Guides, each a versioned rulebook for a specific use case",
          style="color:#666; font-size:13px; line-height:1.4;")
      )),
      column(3, wellPanel(
        style="background:#f3fff8; border:2px solid #33d17a; text-align:center; padding:18px;",
        tags$i(class="fa fa-shapes fa-2x", style="color:#33d17a;"),
        h4("Distinct Resource Types", style="margin-top:8px; color:#0c223f;"),
        h2(textOutput(ns("kpi_resources")), style="color:#33d17a; font-weight:bold; font-size:2em;"),
        p("Types of healthcare objects defined (e.g. Patient, Observation, Medication, ValueSet)",
          style="color:#666; font-size:13px; line-height:1.4;")
      )),
      column(3, wellPanel(
        style="background:#fdfaf3; border:2px solid #e8a838; text-align:center; padding:18px;",
        tags$i(class="fa fa-users fa-2x", style="color:#e8a838;"),
        h4("Contributing Authors", style="margin-top:8px; color:#0c223f;"),
        h2(textOutput(ns("kpi_authors")), style="color:#e8a838; font-weight:bold; font-size:2em;"),
        p("Organizations and individuals publishing FHIR packages (HL7, WHO, national agencies, etc.)",
          style="color:#666; font-size:13px; line-height:1.4;"),
        uiOutput(ns("author_buttons"))
      ))
    ),

    # Key Insights callout
    fluidRow(
      column(12,
        wellPanel(
          style="background:#0c223f; border:none; padding:20px; margin-top:5px;",
          h4(icon("lightbulb"), " Key Insights from the Data", style="color:#f26d21; margin-top:0;"),
          fluidRow(
            column(3,
              tags$div(style="color:white;",
                tags$b(style="color:#f26d21; font-size:1.1em;", "R4 is the dominant standard"),
                tags$p("FHIR R4 accounts for the vast majority of all published packages.
                        Most new healthcare apps target R4 for maximum compatibility.",
                       style="font-size:13px; line-height:1.5; margin-top:4px;")
              )
            ),
            column(3,
              tags$div(style="color:white;",
                tags$b(style="color:#f26d21; font-size:1.1em;", "2 types dominate 76% of resources"),
                tags$p("ValueSets (allowed-value lists) and StructureDefinitions (data templates) together
                        account for three-quarters of everything in the registry.",
                       style="font-size:13px; line-height:1.5; margin-top:4px;")
              )
            ),
            column(3,
              tags$div(style="color:white;",
                tags$b(style="color:#f26d21; font-size:1.1em;", "The US leads globally"),
                tags$p("The United States has the most published Implementation Guides of any country,
                        driven by CMS and ONC mandates requiring FHIR-based data exchange.",
                       style="font-size:13px; line-height:1.5; margin-top:4px;")
              )
            ),
            column(3,
              tags$div(style="color:white;",
                tags$b(style="color:#f26d21; font-size:1.1em;", "FHIR keeps growing"),
                tags$p("From 103 resource types in FHIR R2 (DSTU2) to 167 in R5, a 62% growth
                        in the vocabulary of healthcare data exchange over a decade.",
                       style="font-size:13px; line-height:1.5; margin-top:4px;")
              )
            )
          )
        )
      )
    ),

    # Data Snapshot
    fluidRow(
      column(12, wellPanel(
        style="background:#f0f8ff; border:1px solid #d0e8f7; padding:10px;",
        p(strong("Data Snapshot:"),
          " Built: ", textOutput(ns("meta_built_at"), inline = TRUE),
          " | Source: ",
          tags$a("HL7 FHIR XIG Registry", href = "https://packages2.fhir.org/xig",
                 target = "_blank", style = "color:#f26d21; font-weight:600;"),
          " | Filter by realm below to update all metrics above.",
          style="margin:0; color:#0c223f;")
      ))
    ),
    
    hr(style="border-top: 2px solid #f26d21; margin:30px 0;"),

    fluidRow(
      column(12, h3("Interactive Visualization",
                    style="color:#f26d21; text-align:center; margin-bottom:4px;")),
      column(12, p("Build any chart you want: pick axes, measure, grouping, facets, and filters independently.",
                   style="text-align:center; color:#666; font-size:14px; margin-bottom:18px;"))
    ),

    fluidRow(
      # ---- Config panel ----
      column(4,
        wellPanel(
          style = "background:#f9fafb; border:2px solid #ddd; padding:16px;",

          # -- Axes & Type --
          tags$p(tags$b("Axes & Chart Type"), style="color:#f26d21; margin:0 0 8px; font-size:14px; text-transform:uppercase; letter-spacing:.05em;"),

          fluidRow(
            column(6, selectInput(ns("plot_type"), "Chart Type:",
                       choices = c("Bar Chart" = "bar", "Grouped Bars" = "grouped", "Stacked Bars" = "stacked"),
                       selected = "bar")),
            column(6, selectInput(ns("plot_x_var"), "X-Axis (Category):",
                       choices = c("FHIR Version" = "version", "Resource Type" = "resource_type",
                                   "Realm" = "realm", "Status" = "status",
                                   "Author" = "auth", "Working Group" = "wg"),
                       selected = "version"))
          ),
          fluidRow(
            column(6, selectInput(ns("plot_y_measure"), "Y-Axis (Measure):",
                       choices = c("Count (rows)"         = "count",
                                   "% of filtered total"  = "pct",
                                   "Distinct packages"     = "n_packages",
                                   "Distinct authors"      = "n_authors",
                                   "Avg FMM score"         = "avg_fmm"),
                       selected = "count")),
            column(6, selectInput(ns("plot_sort"), "Sort bars:",
                       choices = c("Highest first" = "desc", "Lowest first" = "asc",
                                   "Alphabetical"  = "alpha"),
                       selected = "desc"))
          ),

          selectInput(ns("plot_fill_var"), "Group / Fill by:",
                      choices = c("None"          = "none",
                                  "FHIR Version"  = "version",
                                  "Resource Type" = "resource_type",
                                  "Realm"         = "realm",
                                  "Status"        = "status",
                                  "Author"        = "auth",
                                  "Working Group" = "wg"),
                      selected = "none"),
          tags$small("Fill applies to Grouped / Stacked chart types.",
                     style="color:#999; font-style:italic; display:block; margin-bottom:8px;"),

          selectInput(ns("plot_facet_var"), "Facet by (optional):",
                      choices = c("None"          = "none",
                                  "FHIR Version"  = "version",
                                  "Resource Type" = "resource_type",
                                  "Realm"         = "realm",
                                  "Status"        = "status",
                                  "Working Group" = "wg"),
                      selected = "none"),

          numericInput(ns("top_n"), "Top N categories:", value = 12, min = 3, max = 50, step = 1),

          hr(style="border-color:#ddd; margin:10px 0;"),

          # -- Data Filters --
          tags$p(tags$b("Data Filters"), style="color:#f26d21; margin:0 0 8px; font-size:14px; text-transform:uppercase; letter-spacing:.05em;"),
          tags$small("Filters apply to the plot only. KPIs above are controlled by the realm filter in the snapshot bar.",
                     style="color:#999; font-style:italic; display:block; margin-bottom:8px;"),

          selectizeInput(ns("filter_version"), "FHIR Version:",
                         choices = NULL, multiple = TRUE,
                         options = list(placeholder = "All versions")),
          selectizeInput(ns("realm_filter"), "Realm:",
                         choices = NULL, multiple = TRUE,
                         options = list(placeholder = "All realms")),
          selectizeInput(ns("filter_resource_type"), "Resource Type:",
                         choices = NULL, multiple = TRUE,
                         options = list(placeholder = "All types")),
          selectizeInput(ns("filter_status"), "Status:",
                         choices = NULL, multiple = TRUE,
                         options = list(placeholder = "All statuses")),
          selectizeInput(ns("filter_wg"), "Working Group:",
                         choices = NULL, multiple = TRUE,
                         options = list(placeholder = "All WGs")),

          tags$small("Note: when Status is used as an axis or fill, each resource is counted once at its most recent status.",
                     style="color:#888; font-style:italic; display:block; margin:6px 0 10px;"),

          actionButton(ns("generate_plot"), "Generate Plot",
                       class = "btn-primary btn-lg btn-block", icon = icon("chart-bar"))
        )
      ),

      # ---- Plot panel ----
      column(8,
        wellPanel(
          style = "background:#ffffff; border:2px solid #ddd; padding:15px; min-height:700px;",
          div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:12px;",
              uiOutput(ns("plot_subtitle_ui")),
              downloadButton(ns("download_custom_plot"), "Download",
                             class = "btn-sm btn-success")
          ),
          plotOutput(ns("custom_plot"), height = "630px")
        )
      )
    )
  )
}

mod_overview_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    VARS <- c("FHIR Version" = "version", "Resource Type" = "resource_type",
              "Realm" = "realm", "Status" = "status", "Author" = "auth",
              "Working Group" = "wg")
    VAR_LABELS <- setNames(names(VARS), VARS)

    clean_vals <- function(x) x[!is.na(x) & x != "" & x != "none" & x != "unknown" & x != "NA"]

    # Populate all filter dropdowns once
    observe({
      df <- resources_tbl
      updateSelectizeInput(session, "filter_version",
        choices = sort(unique(clean_vals(df$version))), server = TRUE)
      updateSelectizeInput(session, "realm_filter",
        choices = df %>% filter(!is.na(realm), realm != "", realm != "NA", realm != "none") %>%
          count(realm, sort = TRUE) %>% pull(realm), server = TRUE)
      updateSelectizeInput(session, "filter_resource_type",
        choices = df %>% filter(!is.na(resource_type)) %>%
          count(resource_type, sort = TRUE) %>% pull(resource_type), server = TRUE)
      updateSelectizeInput(session, "filter_status",
        choices = sort(unique(clean_vals(df$status))), server = TRUE)
      updateSelectizeInput(session, "filter_wg",
        choices = df %>% filter(!is.na(wg), wg != "", wg != "na") %>%
          count(wg, sort = TRUE) %>% pull(wg), server = TRUE)
    })

    # KPI-level data — only realm_filter (keeps KPIs stable)
    filtered_data <- reactive({
      df <- resources_tbl
      if (length(input$realm_filter) > 0) df <- df %>% filter(realm %in% input$realm_filter)
      df
    })

    # Plot-level data — all filters
    plot_filtered <- reactive({
      df <- resources_tbl
      if (length(input$filter_version)       > 0) df <- df %>% filter(version       %in% input$filter_version)
      if (length(input$realm_filter)         > 0) df <- df %>% filter(realm         %in% input$realm_filter)
      if (length(input$filter_resource_type) > 0) df <- df %>% filter(resource_type %in% input$filter_resource_type)
      if (length(input$filter_status)        > 0) df <- df %>% filter(status        %in% input$filter_status)
      if (length(input$filter_wg)            > 0) df <- df %>% filter(wg            %in% input$filter_wg)
      df
    })
    
    # FIXED KPIs - exclude "none" and "unknown" properly
    output$kpi_rows <- renderText({ scales::comma(nrow(filtered_data())) })
    output$kpi_packages <- renderText({ scales::comma(n_distinct(filtered_data()$package_text)) })
    output$kpi_resources <- renderText({ scales::comma(n_distinct(filtered_data()$identity_text)) })
    
    # FIXED: Author count excluding "none"
    output$kpi_authors <- renderText({
      n_authors <- filtered_data() %>%
        filter(!is.na(auth), auth != "", auth != "none") %>%
        pull(auth) %>%
        n_distinct()
      scales::comma(n_authors)
    })
    
    # Author buttons - top 4 excluding "none"
    output$author_buttons <- renderUI({
      top_authors <- filtered_data() %>%
        filter(!is.na(auth), auth != "", auth != "none") %>%
        count(auth, sort = TRUE) %>%
        head(4) %>%
        pull(auth)
      
      if (length(top_authors) == 0) {
        return(p("No authors", style="font-size:12px; color:#999;"))
      }
      
      tags$div(
        style="margin-top:10px; display:flex; flex-wrap:wrap; gap:5px; justify-content:center;",
        lapply(top_authors, function(auth) {
          tags$span(
            style="background:#f26d21; color:white; padding:4px 8px; border-radius:12px; font-size:11px;",
            auth
          )
        })
      )
    })
    
    # Meta info
    output$meta_built_at <- renderText({
      format(as.POSIXct(meta$built_at), "%Y-%m-%d %H:%M")
    })

    # ---- Measure helper: compute the Y value for a grouped df ----
    compute_measure <- function(df, group_cols, measure, total_n) {
      if (measure == "count") {
        df %>% count(across(all_of(group_cols))) %>%
          rename(y_val = last_col())
      } else if (measure == "pct") {
        df %>% count(across(all_of(group_cols))) %>%
          rename(y_val = last_col()) %>%
          mutate(y_val = round(100 * y_val / total_n, 2))
      } else if (measure == "n_packages") {
        df %>% group_by(across(all_of(group_cols))) %>%
          summarise(y_val = n_distinct(package_text, na.rm = TRUE), .groups = "drop")
      } else if (measure == "n_authors") {
        df %>% group_by(across(all_of(group_cols))) %>%
          summarise(y_val = n_distinct(auth[auth != "" & auth != "none" & !is.na(auth)]), .groups = "drop")
      } else if (measure == "avg_fmm") {
        df %>% group_by(across(all_of(group_cols))) %>%
          summarise(y_val = round(mean(as.numeric(fmm), na.rm = TRUE), 2), .groups = "drop")
      }
    }

    y_label_for <- function(measure) {
      c(count = "Count", pct = "% of total", n_packages = "Distinct packages",
        n_authors = "Distinct authors", avg_fmm = "Avg FMM score")[measure]
    }

    # ---- Plot data (fires on button) ----
    plot_data <- eventReactive(input$generate_plot, {
      df <- plot_filtered()
      if (nrow(df) == 0) return(NULL)

      x_var     <- input$plot_x_var
      fill_var  <- if (input$plot_fill_var  == "none") NULL else input$plot_fill_var
      facet_var <- if (input$plot_facet_var == "none") NULL else input$plot_facet_var
      plt_type  <- input$plot_type
      measure   <- input$plot_y_measure
      sort_ord  <- input$plot_sort
      top_n     <- input$top_n

      # Status deduplication when status is on any axis
      uses_status <- x_var == "status" || identical(fill_var, "status")
      if (uses_status) {
        date_col <- intersect(c("date", "published"), names(df))
        if (length(date_col) > 0)
          df <- df %>% arrange(desc(.data[[date_col[1]]])) %>%
            distinct(identity_text, version, .keep_all = TRUE)
        else
          df <- df %>% distinct(identity_text, version, .keep_all = TRUE)
      }

      total_n <- nrow(df)

      # Clean x_var values
      df <- df %>% filter(!is.na(.data[[x_var]]),
                          .data[[x_var]] != "", .data[[x_var]] != "none",
                          .data[[x_var]] != "unknown")

      # Determine top-N categories on X based on raw count (stable across measures)
      top_x <- df %>% count(.data[[x_var]], sort = TRUE) %>%
        slice_head(n = top_n) %>% pull(1)
      df <- df %>% filter(.data[[x_var]] %in% top_x)

      # Build group columns
      group_cols <- x_var
      if (!is.null(fill_var) && fill_var != x_var) {
        df <- df %>% filter(!is.na(.data[[fill_var]]),
                            .data[[fill_var]] != "", .data[[fill_var]] != "none")
        group_cols <- c(x_var, fill_var)
      }
      facet_added <- FALSE
      if (!is.null(facet_var) && facet_var != x_var && (is.null(fill_var) || facet_var != fill_var)) {
        df <- df %>% filter(!is.na(.data[[facet_var]]),
                            .data[[facet_var]] != "", .data[[facet_var]] != "none")
        group_cols <- unique(c(group_cols, facet_var))
        facet_added <- TRUE
      }

      counts <- compute_measure(df, group_cols, measure, total_n)
      names(counts)[1] <- "label"
      if (!is.null(fill_var) && fill_var != x_var) names(counts)[2] <- "fill_label"
      if (facet_added) {
        facet_col_idx <- if (!is.null(fill_var) && fill_var != x_var) 3 else 2
        if (ncol(counts) >= facet_col_idx) names(counts)[facet_col_idx] <- "facet_label"
      }

      # Apply sort to label factor
      counts <- counts %>%
        mutate(label = switch(sort_ord,
          desc  = factor(label, levels = counts %>% group_by(label) %>%
                           summarise(s = sum(y_val, na.rm = TRUE)) %>%
                           arrange(s) %>% pull(label)),
          asc   = factor(label, levels = counts %>% group_by(label) %>%
                           summarise(s = sum(y_val, na.rm = TRUE)) %>%
                           arrange(desc(s)) %>% pull(label)),
          alpha = factor(label, levels = sort(unique(as.character(label))))
        ))

      list(counts = counts, x_var = x_var, fill_var = fill_var,
           facet_var = facet_var, plot_type = plt_type,
           measure = measure, sort_ord = sort_ord)
    }, ignoreNULL = TRUE)

    # ---- Build plot ----
    build_plot <- function(pdata) {
      placeholder <- ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Configure options and\nclick 'Generate Plot'",
                 size = 6, color = "#bbb", hjust = 0.5, vjust = 0.5) +
        theme_void() +
        theme(plot.background = element_rect(fill = "white", color = NA))

      if (is.null(pdata) || nrow(pdata$counts) == 0) return(placeholder)

      counts    <- pdata$counts
      x_var     <- pdata$x_var
      fill_var  <- pdata$fill_var
      facet_var <- pdata$facet_var
      plt_type  <- pdata$plot_type
      y_lab     <- y_label_for(pdata$measure)
      x_lab     <- VAR_LABELS[x_var]

      has_fill  <- !is.null(fill_var) && "fill_label" %in% names(counts)
      has_facet <- !is.null(facet_var) && "facet_label" %in% names(counts)

      # Format value labels
      fmt <- if (pdata$measure == "pct") function(x) paste0(x, "%")
             else if (pdata$measure == "avg_fmm") function(x) sprintf("%.1f", x)
             else scales::comma

      if (has_fill && plt_type %in% c("grouped", "stacked")) {
        pos <- if (plt_type == "grouped") position_dodge(0.85) else position_stack()
        fill_lab <- VAR_LABELS[fill_var]
        p <- ggplot(counts, aes(x = label, y = y_val, fill = fill_label)) +
          geom_col(position = pos, color = "white", size = 0.2) +
          coord_flip() +
          scale_fill_healthchain() +
          labs(title = paste(y_lab, "by", x_lab, "and", fill_lab),
               x = x_lab, y = y_lab, fill = fill_lab) +
          hc_theme() +
          theme(plot.background = element_rect(fill = "white", color = NA))
      } else {
        p <- ggplot(counts, aes(x = label, y = y_val, fill = label)) +
          geom_col(show.legend = FALSE, color = "white", size = 0.2) +
          geom_text(aes(label = fmt(y_val)), hjust = -0.1, size = 3.5,
                    color = "#0c223f", fontface = "bold") +
          coord_flip() +
          expand_limits(y = max(counts$y_val, na.rm = TRUE) * 1.2) +
          scale_fill_healthchain() +
          labs(title = paste("Top", x_lab, "by", y_lab),
               x = x_lab, y = y_lab) +
          hc_theme() +
          theme(plot.background = element_rect(fill = "white", color = NA))
      }

      if (has_facet) {
        p <- p + facet_wrap(~ facet_label, scales = "free_x") +
          theme(strip.text = element_text(face = "bold", size = 10))
      }
      p
    }

    # ---- Subtitle showing active config ----
    output$plot_subtitle_ui <- renderUI({
      if (!isTruthy(input$generate_plot) || input$generate_plot == 0) return(NULL)
      pd <- plot_data()
      if (is.null(pd)) return(NULL)
      parts <- c(paste0("X: ", VAR_LABELS[pd$x_var]),
                 paste0("Y: ", y_label_for(pd$measure)))
      if (!is.null(pd$fill_var))  parts <- c(parts, paste0("Fill: ", VAR_LABELS[pd$fill_var]))
      if (!is.null(pd$facet_var)) parts <- c(parts, paste0("Facet: ", VAR_LABELS[pd$facet_var]))
      tags$small(paste(parts, collapse = "  \u00b7  "),
                 style = "color:#666; font-style:italic;")
    })

    output$custom_plot <- renderPlot({
      if (!isTruthy(input$generate_plot) || input$generate_plot == 0) {
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "Configure options and\nclick 'Generate Plot'",
                   size = 6, color = "#bbb", hjust = 0.5, vjust = 0.5) +
          theme_void() +
          theme(plot.background = element_rect(fill = "white", color = NA))
      } else {
        build_plot(plot_data())
      }
    })

    build_caption <- function() {
      pd <- plot_data()
      if (is.null(pd)) return("")
      parts <- c(
        paste0("X: ", VAR_LABELS[pd$x_var]),
        paste0("Y: ", y_label_for(pd$measure)),
        paste0("Type: ", pd$plot_type),
        paste0("Sort: ", pd$sort_ord),
        paste0("Top N: ", input$top_n)
      )
      if (!is.null(pd$fill_var))  parts <- c(parts, paste0("Fill: ",  VAR_LABELS[pd$fill_var]))
      if (!is.null(pd$facet_var)) parts <- c(parts, paste0("Facet: ", VAR_LABELS[pd$facet_var]))
      active_filters <- c(
        if (length(input$filter_version) > 0)       paste0("Version=",  paste(input$filter_version,       collapse=",")),
        if (length(input$realm_filter) > 0)          paste0("Realm=",    paste(input$realm_filter,          collapse=",")),
        if (length(input$filter_resource_type) > 0)  paste0("Type=",     paste(input$filter_resource_type,  collapse=",")),
        if (length(input$filter_status) > 0)         paste0("Status=",   paste(input$filter_status,         collapse=",")),
        if (length(input$filter_wg) > 0)             paste0("WG=",       paste(input$filter_wg,             collapse=","))
      )
      if (length(active_filters) > 0) parts <- c(parts, paste0("Filters: ", paste(active_filters, collapse=" | ")))
      parts <- c(parts, paste0("FHIR XIG Registry  |  EDA by Suhas P K  |  ", format(Sys.Date(), "%Y-%m-%d")))
      paste(parts, collapse = "  \u00b7  ")
    }

    output$download_custom_plot <- downloadHandler(
      filename = function() paste0("overview_plot_", Sys.Date(), ".png"),
      content  = function(file) {
        p <- add_caption_style(build_plot(plot_data()), build_caption())
        ggplot2::ggsave(file, plot = p, width = 11, height = 7, dpi = 200, bg = "white")
      }
    )

  }) # end moduleServer
} # end mod_overview_server