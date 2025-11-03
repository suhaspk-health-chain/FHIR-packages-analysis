# ==============================================================
# server.R — FHIR Packages Dashboard Server Logic
# ==============================================================

function(input, output, session) {
  
  # ---- Initialize filter choices from live data (JSON or fallback) ----
  observe({
    updateSelectizeInput(session, "flt_version",
                         choices = choices$version, selected = choices$version, server = TRUE)
    updateSelectizeInput(session, "flt_status",
                         choices = choices$status, selected = choices$status, server = TRUE)
    updateSelectizeInput(session, "flt_author",
                         choices = choices$author, selected = choices$author, server = TRUE)
    updateSelectizeInput(session, "flt_realm",
                         choices = choices$realm, selected = choices$realm, server = TRUE)
  })
  
  observeEvent(input$flt_reset, {
    updateSelectizeInput(session, "flt_version", selected = choices$version)
    updateSelectizeInput(session, "flt_status", selected = choices$status)
    updateSelectizeInput(session, "flt_author", selected = choices$author)
    updateSelectizeInput(session, "flt_realm", selected = choices$realm)
  })
  
  # ---- Global filtered dataset (for Overview tab only) -------------------------
  filtered <- reactive({
    df <- resources_tbl
    req(nrow(df) > 0)
    
    # Apply filters
    if (!is.null(input$flt_version) && length(input$flt_version) > 0)
      df <- df %>% filter(version %in% input$flt_version)
    
    if (!is.null(input$flt_status) && length(input$flt_status) > 0)
      df <- df %>% filter(status %in% input$flt_status)
    
    if (!is.null(input$flt_author) && length(input$flt_author) > 0)
      df <- df %>% filter(auth %in% input$flt_author)
    
    if (!is.null(input$flt_realm) && length(input$flt_realm) > 0) {
      want_miss <- "(missing)" %in% input$flt_realm
      sel <- setdiff(input$flt_realm, "(missing)")
      df <- df %>% filter( (want_miss & is.na(realm)) | (!is.na(realm) & realm %in% sel) )
    }
    
    df
  })
  
  # ---- Meta info outputs --------------------------------------------------------
  output$meta_built_at <- renderText({
    if (!is.null(meta$built_at)) format(meta$built_at, "%Y-%m-%d %H:%M") else "n/a"
  })
  
  output$meta_processed <- renderText({
    meta$source_dirs$processed %||% "n/a"
  })
  
  output$meta_raw <- renderText({
    meta$source_dirs$raw %||% "n/a"
  })
  
  # ---- KPIs (filtered - Overview tab only) --------------------------------------
  output$kpi_rows <- renderText({
    scales::comma(kpi_rows(filtered()))
  })
  
  output$kpi_packages <- renderText({
    scales::comma(kpi_n_distinct(filtered()$package_text))
  })
  
  output$kpi_resources <- renderText({
    scales::comma(kpi_n_distinct(filtered()$identity_text))
  })
  
  output$kpi_authors <- renderText({
    scales::comma(kpi_authors_nonempty(filtered()))
  })
  
  # ---- Overview: version distribution (filtered) --------------------------------
  plot_versions_reactive <- reactive({
    df <- filtered()
    req(nrow(df) > 0)
    
    p <- df %>%
      count(version, sort = TRUE) %>%
      ggplot(aes(x = reorder(version, n), y = n, fill = version)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = scales::comma(n)), hjust = -0.2, size = 4, color = "#0f1f2e") +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(title = "FHIR Package Distribution by Version", 
           x = "FHIR Version", 
           y = "Package Count") +
      theme_healthchain(base_size = 13)
    
    return(p)
  })
  
  output$plot_versions <- renderPlot({
    plot_versions_reactive()
  }, height = 500, width = "auto")
  
  # FIXED: Download handler with suspendWhenHidden
  output$download_plot_versions <- downloadHandler(
    filename = function() { 
      paste0("fhir_versions_", Sys.Date(), ".png") 
    },
    content = function(file) {
      # Get the plot
      p <- plot_versions_reactive()
      
      # Try to add logo, if fails use plot without logo
      p_final <- tryCatch({
        if (file.exists(HC_LOGO_PATH)) {
          add_logo_cowplot(p)
        } else {
          message("Logo file not found at: ", HC_LOGO_PATH)
          p
        }
      }, error = function(e) {
        message("Logo not added: ", e$message)
        p
      })
      
      # Save using png device directly
      png(file, width = 12, height = 8, units = "in", res = 300, bg = "white")
      print(p_final)
      dev.off()
    }
  )
  
  # Set suspendWhenHidden to FALSE
  outputOptions(output, "download_plot_versions", suspendWhenHidden = FALSE)
  
  # ---- NEW: Overview - Grouped Bar Chart by Realm (TOP 8 REALMS) ---------------
  plot_realm_facet_reactive <- reactive({
    df <- filtered()
    req(nrow(df) > 0)
    
    # Get top 8 realms by package count
    top_realms <- df %>%
      filter(!is.na(realm), realm != "", realm != "NA") %>%
      count(realm, sort = TRUE) %>%
      slice_head(n = 8) %>%
      pull(realm)
    
    req(length(top_realms) > 0)
    
    # Prepare data - filter to top realms and get version distribution
    df_realm <- df %>%
      filter(realm %in% top_realms) %>%
      count(version, realm, sort = TRUE) %>%
      # Order realms by total count
      mutate(realm = factor(realm, levels = top_realms))
    
    req(nrow(df_realm) > 0)
    
    # Create grouped bar chart
    p <- df_realm %>%
      ggplot(aes(x = realm, y = n, fill = version)) +
      geom_col(position = "dodge", width = 0.8) +
      geom_text(aes(label = scales::comma(n)), 
                position = position_dodge(width = 0.8),
                vjust = -0.3, size = 3, color = "#0f1f2e") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(title = "FHIR Package Distribution by Realm and Version", 
           subtitle = "Top 8 realms by total package count",
           x = "Realm", 
           y = "Package Count",
           fill = "FHIR Version") +
      theme_healthchain(base_size = 12) +
      theme(
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
        panel.grid.major.x = element_blank()
      )
    
    return(p)
  })
  
  output$plot_realm_facet <- renderPlot({
    plot_realm_facet_reactive()
  }, height = 550, width = "auto")
  
  output$download_plot_realm_facet <- downloadHandler(
    filename = function() { 
      paste0("fhir_versions_by_realm_", Sys.Date(), ".png") 
    },
    content = function(file) {
      # Get the plot
      p <- plot_realm_facet_reactive()
      
      # Try to add logo, if fails use plot without logo
      p_final <- tryCatch({
        if (file.exists(HC_LOGO_PATH)) {
          add_logo_cowplot(p)
        } else {
          message("Logo file not found at: ", HC_LOGO_PATH)
          p
        }
      }, error = function(e) {
        message("Logo not added: ", e$message)
        p
      })
      
      # Save using png device directly
      png(file, width = 14, height = 9, units = "in", res = 300, bg = "white")
      print(p_final)
      dev.off()
    }
  )
  outputOptions(output, "download_plot_realm_facet", suspendWhenHidden = FALSE)
  
  # ---- Authors (NOT filtered - uses full dataset) -------------------------------
  plot_authors_reactive <- reactive({
    df <- resources_tbl  # Use FULL dataset, not filtered()
    req(nrow(df) > 0)
    
    p <- df %>%
      filter(!is.na(auth), auth != "") %>%
      count(auth, sort = TRUE) %>%
      slice_head(n = 15) %>%
      ggplot(aes(x = reorder(auth, n), y = n, fill = auth)) +
      geom_col(show.legend = FALSE) + 
      coord_flip() +
      geom_text(aes(label = scales::comma(n)), hjust = -0.2, size = 4, color = "#0f1f2e") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(title = "Top 15 Authors by Package Count", 
           x = "Author", 
           y = "Package Count") +
      theme_healthchain(base_size = 13)
    
    return(p)
  })
  
  output$plot_authors <- renderPlot({
    plot_authors_reactive()
  }, height = 500, width = "auto")
  
  output$download_plot_authors <- downloadHandler(
    filename = function() { 
      paste0("top_authors_", Sys.Date(), ".png") 
    },
    content = function(file) {
      # Get the plot
      p <- plot_authors_reactive()
      
      # Try to add logo
      p_final <- tryCatch({
        if (file.exists(HC_LOGO_PATH)) {
          add_logo_cowplot(p)
        } else {
          p
        }
      }, error = function(e) {
        message("Logo not added: ", e$message)
        p
      })
      
      # Save using png device directly
      png(file, width = 12, height = 8, units = "in", res = 300, bg = "white")
      print(p_final)
      dev.off()
    }
  )
  outputOptions(output, "download_plot_authors", suspendWhenHidden = FALSE)
  
  output$tbl_authors <- renderDT({
    df <- resources_tbl  # Use FULL dataset
    req(nrow(df) > 0)
    
    df %>%
      filter(!is.na(auth), auth != "") %>%
      count(auth, sort = TRUE) %>%
      rename(Author = auth, `Package Count` = n) %>%
      datatable(
        class = "stripe hover compact", 
        options = list(pageLength = 15, scrollY = "400px", scrollCollapse = TRUE),
        rownames = FALSE
      )
  })
  
  # ---- Evolution (NOT filtered - uses .rds data) --------------------------------
  plot_added_reactive <- reactive({
    dc <- delta_counts
    req(nrow(dc) > 0)
    
    p <- ggplot(dc, aes(x = reorder(transition, added), y = added, fill = transition)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      geom_text(aes(label = scales::comma(added)), hjust = -0.2, size = 4, color = "#0f1f2e") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(title = "Resources Added per Transition", 
           x = "Version Transition", 
           y = "Resources Added") +
      theme_healthchain(base_size = 12)
    
    return(p)
  })
  
  output$plot_added <- renderPlot({
    plot_added_reactive()
  }, height = 450, width = "auto")
  
  output$download_plot_added <- downloadHandler(
    filename = function() { 
      paste0("resources_added_", Sys.Date(), ".png") 
    },
    content = function(file) {
      p <- plot_added_reactive()
      
      p_final <- tryCatch({
        if (file.exists(HC_LOGO_PATH)) {
          add_logo_cowplot(p)
        } else {
          p
        }
      }, error = function(e) {
        p
      })
      
      png(file, width = 10, height = 7, units = "in", res = 300, bg = "white")
      print(p_final)
      dev.off()
    }
  )
  outputOptions(output, "download_plot_added", suspendWhenHidden = FALSE)
  
  plot_removed_reactive <- reactive({
    dc <- delta_counts
    req(nrow(dc) > 0)
    
    p <- ggplot(dc, aes(x = reorder(transition, removed), y = removed, fill = transition)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      geom_text(aes(label = scales::comma(removed)), hjust = -0.2, size = 4, color = "#0f1f2e") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(title = "Resources Removed per Transition", 
           x = "Version Transition", 
           y = "Resources Removed") +
      theme_healthchain(base_size = 12)
    
    return(p)
  })
  
  output$plot_removed <- renderPlot({
    plot_removed_reactive()
  }, height = 450, width = "auto")
  
  output$download_plot_removed <- downloadHandler(
    filename = function() { 
      paste0("resources_removed_", Sys.Date(), ".png") 
    },
    content = function(file) {
      p <- plot_removed_reactive()
      
      p_final <- tryCatch({
        if (file.exists(HC_LOGO_PATH)) {
          add_logo_cowplot(p)
        } else {
          p
        }
      }, error = function(e) {
        p
      })
      
      png(file, width = 10, height = 7, units = "in", res = 300, bg = "white")
      print(p_final)
      dev.off()
    }
  )
  outputOptions(output, "download_plot_removed", suspendWhenHidden = FALSE)
  
  output$tbl_transitions <- renderDT({
    dc <- delta_counts
    if (!nrow(dc)) {
      datatable(data.frame(message = "No transition data available"), options = list(dom='t'))
    } else {
      datatable(
        dc, 
        class = "stripe hover compact", 
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    }
  })
  
  # ---- NEW: Resource Changes Tab ------------------------------------------------
  output$tbl_added_resources <- renderDT({
    dc <- delta_counts
    if (!nrow(dc)) {
      datatable(data.frame(message = "No data available"), options = list(dom='t'))
    } else if ("added_resources" %in% names(dc)) {
      added_list <- dc %>% 
        select(transition, added_resources) %>%
        filter(!is.na(added_resources) & added_resources != "")
      
      datatable(
        added_list, 
        class = "stripe hover compact", 
        options = list(pageLength = 15, scrollY = "500px", scrollCollapse = TRUE),
        rownames = FALSE
      )
    } else {
      datatable(
        data.frame(message = "Added resources column not found in data"), 
        options = list(dom='t')
      )
    }
  })
  
  output$tbl_removed_resources <- renderDT({
    dc <- delta_counts
    if (!nrow(dc)) {
      datatable(data.frame(message = "No data available"), options = list(dom='t'))
    } else if ("removed_resources" %in% names(dc)) {
      removed_list <- dc %>% 
        select(transition, removed_resources) %>%
        filter(!is.na(removed_resources) & removed_resources != "")
      
      datatable(
        removed_list, 
        class = "stripe hover compact", 
        options = list(pageLength = 15, scrollY = "500px", scrollCollapse = TRUE),
        rownames = FALSE
      )
    } else {
      datatable(
        data.frame(message = "Removed resources column not found in data"), 
        options = list(dom='t')
      )
    }
  })
  
  # ---- Tables (Resources table uses filtered data, others use .rds) -------------
  output$tbl_resources <- renderDT({
    datatable(
      filtered(), 
      class = "stripe hover compact", 
      options = list(pageLength = 20, scrollX = TRUE, scrollY = "600px", scrollCollapse = TRUE),
      rownames = FALSE,
      filter = 'top'
    )
  })
  
  output$tbl_matrix <- renderDT({
    if (!nrow(matrix_tbl)) {
      datatable(data.frame(message="No matrix available"), options = list(dom='t'))
    } else {
      datatable(
        matrix_tbl, 
        class = "stripe hover compact", 
        options = list(pageLength = 20, scrollX = TRUE, scrollY = "600px", scrollCollapse = TRUE),
        rownames = FALSE
      )
    }
  })
  
  output$tbl_stable <- renderDT({
    if (!nrow(stable_tbl)) {
      datatable(data.frame(message="No stable list available"), options = list(dom='t'))
    } else {
      datatable(
        stable_tbl, 
        class = "stripe hover compact", 
        options = list(pageLength = 20, scrollX = TRUE, scrollY = "600px", scrollCollapse = TRUE),
        rownames = FALSE
      )
    }
  })
  
  output$tbl_raw <- renderDT({
    if (!nrow(raw_preview)) {
      datatable(data.frame(message="Raw preview not available"), options = list(dom='t'))
    } else {
      datatable(
        raw_preview, 
        class = "stripe hover compact", 
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    }
  })
}
