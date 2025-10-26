# ==============================================================
# server.R — FHIR Packages Dashboard Server Logic
# ==============================================================

function(input, output, session) {
  
  # ---- Initialize filter choices from live data (JSON or fallback) ----
  observe({
    updateSelectizeInput(session, "flt_version",
                         choices = choices$version, selected = choices$version, server = TRUE)
    updateSelectizeInput(session, "flt_status",
                         choices = choices$status,  selected = choices$status,  server = TRUE)
    updateSelectizeInput(session, "flt_author",
                         choices = choices$author,  selected = choices$author,  server = TRUE)
    updateSelectizeInput(session, "flt_realm",
                         choices = choices$realm,   selected = choices$realm,   server = TRUE)
  })
  
  observeEvent(input$flt_reset, {
    updateSelectizeInput(session, "flt_version", selected = choices$version)
    updateSelectizeInput(session, "flt_status",  selected = choices$status)
    updateSelectizeInput(session, "flt_author",  selected = choices$author)
    updateSelectizeInput(session, "flt_realm",   selected = choices$realm)
  })
  
  # ---- Global filtered dataset ---------------------------------------------
  filtered <- reactive({
    df <- resources_tbl
    req(nrow(df) > 0)
    
    if (!is.null(input$flt_version) && length(input$flt_version))
      df <- df %>% filter(version %in% input$flt_version)
    
    if (!is.null(input$flt_status) && length(input$flt_status))
      df <- df %>% filter(status %in% input$flt_status)
    
    if (!is.null(input$flt_author) && length(input$flt_author))
      df <- df %>% filter(auth %in% input$flt_author)
    
    if (!is.null(input$flt_realm) && length(input$flt_realm)) {
      want_miss <- "(missing)" %in% input$flt_realm
      sel <- setdiff(input$flt_realm, "(missing)")
      df <- df %>% filter( (want_miss & is.na(realm)) | (!is.na(realm) & realm %in% sel) )
    }
    
    df
  })
  
  # ---- KPIs (filtered) ------------------------------------------------------
  output$kpi_rows      <- renderText(scales::comma(kpi_rows(filtered())))
  output$kpi_packages  <- renderText(scales::comma(kpi_n_distinct(filtered()$package_text)))
  output$kpi_resources <- renderText(scales::comma(kpi_n_distinct(filtered()$identity_text)))
  output$kpi_authors   <- renderText(scales::comma(kpi_authors_nonempty(filtered())))
  
  # ---- Overview: version distribution (filtered) ----------------------------
  output$plot_versions <- renderPlot({
    df <- filtered()
    req(nrow(df) > 0)
    df %>%
      count(version, sort = TRUE) %>%
      ggplot(aes(x = reorder(version, n), y = n, fill = version)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = scales::comma(n)), vjust = -0.3, color = "#0f1f2e") +
      labs(title = "FHIR Package Distribution by Version", x = "FHIR Version", y = "Count") +
      theme_minimal(base_size = 14)
  })
  
  # ---- Authors (filtered) ---------------------------------------------------
  output$plot_authors <- renderPlot({
    df <- filtered(); req(nrow(df) > 0)
    df %>%
      filter(!is.na(auth), auth != "") %>%
      count(auth, sort = TRUE) %>%
      slice_head(n = 15) %>%
      ggplot(aes(x = reorder(auth, n), y = n, fill = auth)) +
      geom_col(show.legend = FALSE) + coord_flip() +
      geom_text(aes(label = scales::comma(n)), hjust = -0.1, color = "#0f1f2e") +
      labs(title = "Top Authors (filtered)", x = "Author", y = "Count") +
      theme_minimal(base_size = 14)
  })
  
  output$tbl_authors <- renderDT({
    df <- filtered(); req(nrow(df) > 0)
    df %>%
      filter(!is.na(auth), auth != "") %>%
      count(auth, sort = TRUE) %>%
      datatable(class = "stripe hover compact", options = list(pageLength = 10))
  })
  
  # ---- Evolution (now respects Version filter) ------------------------------
  parse_dc <- reactive({
    dc <- delta_counts
    if (!nrow(dc)) return(tibble())
    if (!("from" %in% names(dc) && "to" %in% names(dc))) {
      dc <- dc %>% tidyr::separate(transition, into = c("from","to"), sep = " *→ *", remove = FALSE)
    }
    dc
  })
  
  filtered_dc <- reactive({
    dc <- parse_dc()
    if (!nrow(dc)) return(dc)
    if (!is.null(input$flt_version) && length(input$flt_version)) {
      dc <- dc %>% filter(from %in% input$flt_version & to %in% input$flt_version)
    }
    dc
  })
  
  output$plot_added <- renderPlot({
    dc <- filtered_dc(); req(nrow(dc) > 0)
    ggplot(dc, aes(x = transition, y = added, fill = transition)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = added), vjust = -0.3, color = "#0f1f2e") +
      labs(title = "Resources Added per Transition", x = "Transition", y = "Count") +
      theme_minimal(base_size = 14)
  })
  
  output$plot_removed <- renderPlot({
    dc <- filtered_dc(); req(nrow(dc) > 0)
    ggplot(dc, aes(x = transition, y = removed, fill = transition)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = removed), vjust = -0.3, color = "#0f1f2e") +
      labs(title = "Resources Removed per Transition", x = "Transition", y = "Count") +
      theme_minimal(base_size = 14)
  })
  
  output$tbl_transitions <- renderDT({
    dc <- filtered_dc()
    if (!nrow(dc)) datatable(data.frame(message = "No transition data available"), options = list(dom='t'))
    else datatable(dc, class = "stripe hover compact", options = list(pageLength = 10))
  })
  
  # ---- Tables (filtered resources; others static) ---------------------------
  output$tbl_resources <- renderDT({
    datatable(filtered(), class = "stripe hover compact", options = list(pageLength = 10))
  })
  
  output$tbl_matrix <- renderDT({
    if (!nrow(matrix_tbl)) datatable(data.frame(message="No matrix available"), options = list(dom='t'))
    else datatable(matrix_tbl, class = "stripe hover compact", options = list(pageLength = 10))
  })
  
  output$tbl_stable <- renderDT({
    if (!nrow(stable_tbl)) datatable(data.frame(message="No stable list available"), options = list(dom='t'))
    else datatable(stable_tbl, class = "stripe hover compact", options = list(pageLength = 10))
  })
  
  output$tbl_raw <- renderDT({
    if (!nrow(raw_preview)) datatable(data.frame(message="Raw preview not available"), options = list(dom='t'))
    else datatable(raw_preview, class = "stripe hover compact", options = list(pageLength = 5))
  })
}
