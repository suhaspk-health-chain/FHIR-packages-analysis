# ==============================================================
# mod_nlp.R - Text Analysis & Pattern Discovery
# ==============================================================

# Stop words removed before word-frequency analysis
NLP_STOP_WORDS <- c(
  "the","a","an","of","for","in","on","at","to","by","and","or","is","are",
  "was","were","be","been","being","with","from","as","this","that","these",
  "those","it","its","use","used","using","all","any","can","has","have",
  "had","not","but","also","which","who","how","when","where","what","one",
  "new","based","per","via","about","into","will","may","each","their",
  # FHIR structural terms — neutral scaffolding, not domain signal
  "fhir","hl7","profile","resource","implementation","guide","ig","definition",
  "extension","set","code","system","value","structure","base","data","type",
  "types","version","spec","specification","release","standard","model"
)

mod_nlp_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h2(icon("brain"), " Text Analysis & Pattern Discovery", style = "color:#0c223f;"),
    wellPanel(
      style = "background:#f3f8ff; border-left:4px solid #4a90d9; padding:14px 18px; margin-bottom:16px;",
      p(style = "margin:0; font-size:15px; line-height:1.6;",
        strong("What you're looking at: "),
        "Every FHIR resource has a human-readable title (e.g. \"US Core Patient Profile\"). ",
        "Across 75,000+ resources, these titles encode a vocabulary of clinical domains, ",
        "administrative workflows, and organizational priorities. This section tokenizes titles, ",
        "extracts publisher patterns from package names, tracks resource lifecycle stages, ",
        "and maps which HL7 working groups are most active."
      )
    ),

    fluidRow(
      column(4, selectizeInput(ns("filter_version"), "Filter by FHIR Version:",
                               choices = NULL, multiple = TRUE,
                               options = list(placeholder = "All versions"))),
      column(4, selectizeInput(ns("filter_realm"), "Filter by Realm:",
                               choices = NULL, multiple = TRUE,
                               options = list(placeholder = "All realms"))),
      column(4, numericInput(ns("top_n_words"), "Top N Terms:", value = 25, min = 10, max = 50, step = 5))
    ),

    hr(style = "border-color:#ddd; margin:16px 0;"),

    # --- Section 1: Word frequency ---
    h3(icon("spell-check"), " Most Common Words in Resource Titles",
       style = "color:#0c223f; margin-top:4px;"),
    p(style = "color:#555; font-size:14px; margin-bottom:14px;",
      "Titles tokenised into words; stop words and FHIR structural terms removed. ",
      "The terms that remain reveal the clinical and administrative domains the ecosystem actually covers."
    ),
    fluidRow(
      column(8, wellPanel(style = "padding:10px;",
        plotOutput(ns("plot_word_freq"), height = "520px")
      )),
      column(4,
        wellPanel(
          style = "background:#f9fafb; border:1px solid #ddd; margin-bottom:12px;",
          h5("How to read this:", style = "color:#0c223f; margin-top:0;"),
          tags$ul(style = "font-size:13px; color:#444; line-height:1.9; padding-left:18px;",
            tags$li(strong("Bar length"), " = titles containing this term"),
            tags$li("Medical terms dominate: patient, observation, claim"),
            tags$li("Administrative terms: prior, authorization, coverage"),
            tags$li("Use version/realm filters to see domain shifts")
          )
        ),
        wellPanel(
          style = "background:#f9fafb; border:1px solid #ddd;",
          h5("Top term by FHIR version:", style = "color:#0c223f; margin-top:0;"),
          DTOutput(ns("tbl_top_term_by_version"))
        )
      )
    ),
    downloadButton(ns("dl_word_freq"), "Download Chart", style = "margin:8px 0 20px;"),

    hr(style = "border-color:#ddd; margin:20px 0;"),

    # --- Section 2: Publisher patterns + realm vocabulary ---
    h3(icon("sitemap"), " Package Naming Patterns", style = "color:#0c223f;"),
    p(style = "color:#555; font-size:14px; margin-bottom:14px;",
      "FHIR package identifiers follow a dot-separated convention: ",
      tags$code("publisher.fhir.realm.name"),
      " (e.g. ", tags$code("hl7.fhir.us.core"), "). ",
      "The left chart extracts the publisher prefix to show who produces the most resources. ",
      "The right chart shows the top title words per realm, revealing vocabulary differences between countries."
    ),
    fluidRow(
      column(6, wellPanel(style = "padding:10px;",
        plotOutput(ns("plot_publisher"), height = "430px")
      )),
      column(6, wellPanel(style = "padding:10px;",
        plotOutput(ns("plot_realm_vocab"), height = "430px")
      ))
    ),
    fluidRow(
      column(6, downloadButton(ns("dl_publisher"), "Download", style = "margin:6px 0 16px;")),
      column(6, downloadButton(ns("dl_realm_vocab"), "Download", style = "margin:6px 0 16px;"))
    ),

    hr(style = "border-color:#ddd; margin:20px 0;"),

    # --- Section 3: Resource lifecycle ---
    h3(icon("recycle"), " Resource Lifecycle: Status by FHIR Version", style = "color:#0c223f;"),
    p(style = "color:#555; font-size:14px; margin-bottom:14px;",
      "Resources move through a lifecycle: ",
      tags$span(style = "color:#4a90d9; font-weight:bold;", "draft"),
      " (work in progress) -> ",
      tags$span(style = "color:#33d17a; font-weight:bold;", "active"),
      " (published standard) -> ",
      tags$span(style = "color:#d63031; font-weight:bold;", "retired"),
      " (superseded). ",
      "Each resource is counted once using its most recent recorded status. ",
      "A maturing ecosystem shows a rising active share over time."
    ),
    wellPanel(style = "padding:10px;",
      plotOutput(ns("plot_status_version"), height = "400px")
    ),
    downloadButton(ns("dl_status_version"), "Download", style = "margin:8px 0 20px;"),

    hr(style = "border-color:#ddd; margin:20px 0;"),

    # --- Section 4: Working groups ---
    h3(icon("users-cog"), " Working Group Coverage", style = "color:#0c223f;"),
    p(style = "color:#555; font-size:14px; margin-bottom:14px;",
      "HL7 International is divided into Working Groups (WGs), each responsible for a clinical or ",
      "administrative domain. The WG behind the most resources shows where implementation guide ",
      "activity is concentrated, a proxy for where standardisation energy is being spent."
    ),
    fluidRow(
      column(8, wellPanel(style = "padding:10px;",
        plotOutput(ns("plot_wg"), height = "460px")
      )),
      column(4,
        wellPanel(
          style = "background:#fff8f3; border:1px solid #f26d21;",
          h5("Common Working Groups:", style = "color:#f26d21; margin-top:0;"),
          tags$ul(style = "font-size:13px; color:#444; line-height:2.0; padding-left:18px;",
            tags$li(strong("fhir-i"), " - FHIR Infrastructure"),
            tags$li(strong("pc"), " - Patient Care"),
            tags$li(strong("oo"), " - Orders & Observations"),
            tags$li(strong("fm"), " - Financial Management"),
            tags$li(strong("sd"), " - Structured Documents"),
            tags$li(strong("vocab"), " - Vocabulary / Terminology"),
            tags$li(strong("phx"), " - Pharmacy"),
            tags$li(strong("cds"), " - Clinical Decision Support")
          )
        )
      )
    ),
    downloadButton(ns("dl_wg"), "Download", style = "margin:8px 0 20px;")
  )
}

mod_nlp_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    observe({
      versions <- sort(unique(resources_tbl$version[!is.na(resources_tbl$version)]))
      updateSelectizeInput(session, "filter_version", choices = versions, server = TRUE)
      realms <- resources_tbl %>%
        filter(!is.na(realm), realm != "", realm != "none", realm != "unknown") %>%
        count(realm, sort = TRUE) %>% pull(realm)
      updateSelectizeInput(session, "filter_realm", choices = realms, server = TRUE)
    })

    filtered <- reactive({
      df <- resources_tbl
      if (length(input$filter_version) > 0) df <- df %>% filter(version %in% input$filter_version)
      if (length(input$filter_realm)  > 0) df <- df %>% filter(realm  %in% input$filter_realm)
      df
    })

    # helper: build caption string from current inputs
    make_caption <- function(extra = "") {
      parts <- c("FHIR XIG Registry", "EDA by Suhas P K", format(Sys.Date(), "%Y-%m-%d"))
      if (length(input$filter_version) > 0)
        parts <- c(parts, paste0("Version: ", paste(input$filter_version, collapse = ", ")))
      if (length(input$filter_realm) > 0)
        parts <- c(parts, paste0("Realm: ", paste(input$filter_realm, collapse = ", ")))
      if (nzchar(extra)) parts <- c(parts, extra)
      paste(parts, collapse = "  |  ")
    }

    tokenize <- function(titles) {
      words <- unlist(strsplit(tolower(titles[!is.na(titles) & titles != ""]), "[^a-z]+"))
      words[nchar(words) > 2 & !words %in% NLP_STOP_WORDS]
    }

    # ---- Word frequency ----
    word_counts <- reactive({
      titles <- filtered()$title
      if (is.null(titles) || sum(!is.na(titles) & titles != "") == 0)
        return(tibble(word = character(), n = integer()))
      words <- tokenize(titles)
      tbl <- sort(table(words), decreasing = TRUE)
      tibble(word = names(tbl), n = as.integer(tbl)) %>% head(input$top_n_words)
    })

    plot_word_freq <- reactive({
      wc <- word_counts()
      if (nrow(wc) == 0)
        return(ggplot() + annotate("text", x=.5, y=.5, label="No title data available",
                                   size=5, color="#999") + theme_void())
      wc %>%
        mutate(word = factor(word, levels = rev(word))) %>%
        ggplot(aes(x = word, y = n, fill = word)) +
        geom_col(show.legend = FALSE, color = "white", size = 0.2) +
        geom_text(aes(label = scales::comma(n)), hjust = -0.1, size = 3.5,
                  color = "#0c223f", fontface = "bold") +
        coord_flip() +
        expand_limits(y = max(wc$n, na.rm = TRUE) * 1.2) +
        scale_fill_healthchain() +
        labs(title = "Most Frequent Terms in Resource Titles",
             subtitle = paste0("Top ", input$top_n_words, " terms (stop words & FHIR structural terms removed)"),
             x = NULL, y = "Occurrences") +
        hc_theme()
    })

    output$plot_word_freq <- renderPlot({ plot_word_freq() })

    output$tbl_top_term_by_version <- renderDT({
      df <- filtered()
      if (!"title" %in% names(df) || !"version" %in% names(df))
        return(datatable(data.frame(Message = "No data"), options = list(dom = "t")))
      result <- df %>%
        filter(!is.na(version), !is.na(title), title != "") %>%
        group_by(version) %>%
        summarise(top_term = {
          w <- tokenize(title)
          if (length(w) == 0) NA_character_ else names(sort(table(w), decreasing = TRUE))[1]
        }, .groups = "drop") %>%
        arrange(version)
      datatable(result, options = list(dom = "t", pageLength = 10), rownames = FALSE,
                colnames = c("Version", "Top Term"))
    }, server = FALSE)

    # ---- Publisher prefix ----
    plot_publisher <- reactive({
      pkgs <- filtered()$package_text
      if (is.null(pkgs)) return(ggplot() + theme_void())
      df <- tibble(pkg = pkgs[!is.na(pkgs) & pkgs != "" & pkgs != "unknown"]) %>%
        mutate(publisher = str_extract(pkg, "^[^.]+")) %>%
        filter(!is.na(publisher), publisher != "") %>%
        count(publisher, sort = TRUE) %>%
        head(15) %>%
        mutate(publisher = factor(publisher, levels = rev(publisher)))
      if (nrow(df) == 0) return(ggplot() + theme_void())
      ggplot(df, aes(x = publisher, y = n, fill = publisher)) +
        geom_col(show.legend = FALSE, color = "white", size = 0.2) +
        geom_text(aes(label = scales::comma(n)), hjust = -0.1, size = 3.5,
                  color = "#0c223f", fontface = "bold") +
        coord_flip() +
        expand_limits(y = max(df$n) * 1.25) +
        scale_fill_healthchain() +
        labs(title = "Top Package Publishers",
             subtitle = "Extracted from package identifier prefix (e.g. hl7, ihe, who)",
             x = NULL, y = "Number of Resources") +
        hc_theme()
    })

    output$plot_publisher <- renderPlot({ plot_publisher() })

    # ---- Top words per realm ----
    plot_realm_vocab <- reactive({
      df <- filtered()
      if (!"title" %in% names(df)) return(ggplot() + theme_void())
      top_realms <- df %>%
        filter(!is.na(realm), realm != "", realm != "none", realm != "unknown") %>%
        count(realm, sort = TRUE) %>% head(6) %>% pull(realm)
      if (length(top_realms) == 0) return(ggplot() + theme_void())

      realm_words <- do.call(bind_rows, lapply(top_realms, function(r) {
        titles <- df %>% filter(realm == r, !is.na(title), title != "") %>% pull(title)
        if (length(titles) == 0) return(tibble(realm = character(), word = character(), n = integer()))
        w <- tokenize(titles)
        tbl <- sort(table(w), decreasing = TRUE)
        k <- min(5L, length(tbl))
        tibble(realm = r, word = names(tbl)[seq_len(k)], n = as.integer(tbl)[seq_len(k)])
      }))

      if (nrow(realm_words) == 0) return(ggplot() + theme_void())
      ggplot(realm_words, aes(x = reorder(word, n), y = n, fill = realm)) +
        geom_col(show.legend = FALSE, color = "white", size = 0.2) +
        coord_flip() +
        facet_wrap(~ realm, scales = "free", ncol = 3) +
        scale_fill_healthchain() +
        labs(title = "Top Title Terms by Realm",
             subtitle = "Domain vocabulary differs between national ecosystems",
             x = NULL, y = "Count") +
        hc_theme() +
        theme(axis.text.y  = element_text(size = 9),
              strip.text   = element_text(face = "bold", size = 10))
    })

    output$plot_realm_vocab <- renderPlot({ plot_realm_vocab() })

    # ---- Lifecycle: status by version ----
    plot_status_version <- reactive({
      df <- filtered()
      if (!all(c("status","version") %in% names(df))) return(ggplot() + theme_void())

      # Deduplicate to latest status per resource per version
      date_col <- intersect(c("date","published"), names(df))
      if (length(date_col) > 0)
        df <- df %>% arrange(desc(.data[[date_col[1]]])) %>%
          distinct(identity_text, version, .keep_all = TRUE)

      sv <- df %>%
        filter(!is.na(status), !is.na(version),
               status %in% c("active","draft","retired","experimental")) %>%
        count(version, status) %>%
        group_by(version) %>% mutate(pct = 100 * n / sum(n)) %>% ungroup()
      if (nrow(sv) == 0) return(ggplot() + theme_void())

      v_order <- c("R2","R3","R4","R4B","R5","R6")
      sv <- sv %>% mutate(version = factor(version, levels = intersect(v_order, unique(version))))

      ggplot(sv, aes(x = version, y = pct, fill = status)) +
        geom_col(position = "stack", color = "white", size = 0.3) +
        geom_text(aes(label = ifelse(pct > 4, paste0(round(pct), "%"), "")),
                  position = position_stack(vjust = 0.5), size = 3.5,
                  color = "white", fontface = "bold") +
        scale_fill_manual(values = c(active = "#33d17a", draft = "#4a90d9",
                                     retired = "#d63031", experimental = "#e8a838")) +
        labs(title = "Resource Lifecycle Distribution by FHIR Version",
             subtitle = "Each resource counted once at its most recent recorded status",
             x = "FHIR Version", y = "Percentage (%)", fill = "Status") +
        hc_theme()
    })

    output$plot_status_version <- renderPlot({ plot_status_version() })

    # ---- Working groups ----
    plot_wg <- reactive({
      wg_col <- filtered()$wg
      if (is.null(wg_col))
        return(ggplot() + annotate("text", x=.5, y=.5,
                                   label="Working group data not available",
                                   size=5, color="#999") + theme_void())
      df <- tibble(wg = wg_col) %>%
        filter(!is.na(wg), wg != "", wg != "na", wg != "none") %>%
        count(wg, sort = TRUE) %>% head(15) %>%
        mutate(wg = factor(wg, levels = rev(wg)))
      if (nrow(df) == 0)
        return(ggplot() + annotate("text", x=.5, y=.5, label="No WG data",
                                   size=5, color="#999") + theme_void())
      ggplot(df, aes(x = wg, y = n, fill = wg)) +
        geom_col(show.legend = FALSE, color = "white", size = 0.2) +
        geom_text(aes(label = scales::comma(n)), hjust = -0.1, size = 3.5,
                  color = "#0c223f", fontface = "bold") +
        coord_flip() +
        expand_limits(y = max(df$n) * 1.25) +
        scale_fill_healthchain() +
        labs(title = "Top 15 Working Groups by Resource Count",
             subtitle = "Which HL7 committees produce the most FHIR definitions",
             x = NULL, y = "Number of Resources") +
        hc_theme()
    })

    output$plot_wg <- renderPlot({ plot_wg() })

    # ---- Downloads with dynamic captions ----
    output$dl_word_freq <- downloadHandler(
      filename = function() paste0("nlp_word_freq_", Sys.Date(), ".png"),
      content  = function(f) ggplot2::ggsave(f,
        plot  = add_caption_style(plot_word_freq(), make_caption(paste0("Top ", input$top_n_words, " terms"))),
        width = 11, height = 8, dpi = 200, bg = "white")
    )
    output$dl_publisher <- downloadHandler(
      filename = function() paste0("nlp_publishers_", Sys.Date(), ".png"),
      content  = function(f) ggplot2::ggsave(f,
        plot  = add_caption_style(plot_publisher(), make_caption("Package publisher prefix analysis")),
        width = 10, height = 7, dpi = 200, bg = "white")
    )
    output$dl_realm_vocab <- downloadHandler(
      filename = function() paste0("nlp_realm_vocab_", Sys.Date(), ".png"),
      content  = function(f) ggplot2::ggsave(f,
        plot  = add_caption_style(plot_realm_vocab(), make_caption("Top 5 terms per realm; faceted")),
        width = 12, height = 8, dpi = 200, bg = "white")
    )
    output$dl_status_version <- downloadHandler(
      filename = function() paste0("nlp_lifecycle_", Sys.Date(), ".png"),
      content  = function(f) ggplot2::ggsave(f,
        plot  = add_caption_style(plot_status_version(),
                                  make_caption("Status deduplicated by latest date per resource per version")),
        width = 10, height = 7, dpi = 200, bg = "white")
    )
    output$dl_wg <- downloadHandler(
      filename = function() paste0("nlp_workgroups_", Sys.Date(), ".png"),
      content  = function(f) ggplot2::ggsave(f,
        plot  = add_caption_style(plot_wg(), make_caption("HL7 Working Group coverage")),
        width = 10, height = 7, dpi = 200, bg = "white")
    )
  })
}
