# ==============================================================
# server.R — FHIR Packages Dashboard Server Logic
# ==============================================================

function(input, output, session) {
  
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
  
  # ---- KPIs (full dataset) ------------------------------------------------------
  output$kpi_rows <- renderText({
    scales::comma(kpi_rows(resources_tbl))
  })
  
  output$kpi_packages <- renderText({
    scales::comma(kpi_n_distinct(resources_tbl$package_text))
  })
  
  output$kpi_resources <- renderText({
    scales::comma(kpi_n_distinct(resources_tbl$identity_text))
  })
  
  output$kpi_authors <- renderText({
    scales::comma(kpi_authors_nonempty(resources_tbl))
  })
  
  # ---- NEW: Interactive Custom Plot Generation ----------------------------------
  custom_plot_reactive <- eventReactive(input$generate_plot, {
    df <- resources_tbl
    req(nrow(df) > 0)
    req(input$plot_x_var)
    req(input$plot_type)
    
    # Get variable labels
    var_labels <- c(
      "version" = "FHIR Version",
      "realm" = "Realm",
      "status" = "Status",
      "auth" = "Author"
    )
    
    x_label <- var_labels[input$plot_x_var]
    x_var <- input$plot_x_var
    
    # Clean data - remove NA and empty values
    df_clean <- df %>%
      filter(!is.na(.data[[x_var]]), .data[[x_var]] != "", .data[[x_var]] != "NA")
    
    req(nrow(df_clean) > 0)
    
    # Limit to top 15 values for clarity
    top_values <- df_clean %>%
      count(.data[[x_var]], sort = TRUE) %>%
      slice_head(n = 15) %>%
      pull(.data[[x_var]])
    
    df_plot <- df_clean %>%
      filter(.data[[x_var]] %in% top_values)
    
    # Generate plot based on type
    if (input$plot_type == "bar") {
      # Simple bar chart
      p <- df_plot %>%
        count(.data[[x_var]], sort = TRUE) %>%
        ggplot(aes(x = reorder(.data[[x_var]], n), y = n, fill = .data[[x_var]])) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = scales::comma(n)), hjust = -0.2, size = 4, color = "#0f1f2e") +
        coord_flip() +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(
          title = paste("FHIR Package Distribution by", x_label),
          subtitle = paste("Top 15", tolower(x_label), "values"),
          x = x_label,
          y = "Package Count"
        ) +
        theme_healthchain(base_size = 13)
      
    } else if (input$plot_type == "grouped") {
      # Grouped bar chart
      fill_var <- input$plot_fill_var
      fill_label <- var_labels[fill_var]
      
      df_grouped <- df_plot %>%
        filter(!is.na(.data[[fill_var]]), .data[[fill_var]] != "") %>%
        count(.data[[x_var]], .data[[fill_var]], sort = TRUE)
      
      p <- df_grouped %>%
        ggplot(aes(x = .data[[x_var]], y = n, fill = .data[[fill_var]])) +
        geom_col(position = "dodge", width = 0.8) +
        geom_text(
          aes(label = scales::comma(n)),
          position = position_dodge(width = 0.8),
          vjust = -0.3, size = 3, color = "#0f1f2e"
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(
          title = paste("FHIR Package Distribution by", x_label, "and", fill_label),
          x = x_label,
          y = "Package Count",
          fill = fill_label
        ) +
        theme_healthchain(base_size = 12) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right"
        )
      
    } else {
      # Stacked bar chart
      fill_var <- input$plot_fill_var
      fill_label <- var_labels[fill_var]
      
      df_stacked <- df_plot %>%
        filter(!is.na(.data[[fill_var]]), .data[[fill_var]] != "") %>%
        count(.data[[x_var]], .data[[fill_var]], sort = TRUE)
      
      p <- df_stacked %>%
        ggplot(aes(x = reorder(.data[[x_var]], n, sum), y = n, fill = .data[[fill_var]])) +
        geom_col(position = "stack") +
        coord_flip() +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
        labs(
          title = paste("FHIR Package Distribution by", x_label, "and", fill_label),
          x = x_label,
          y = "Package Count",
          fill = fill_label
        ) +
        theme_healthchain(base_size = 12) +
        theme(legend.position = "right")
    }
    
    return(p)
  })
  
  output$custom_plot <- renderPlot({
    custom_plot_reactive()
  }, height = 550, width = "auto")
  
  # Flag to show download button after plot is generated
  output$plot_generated <- reactive({
    !is.null(input$generate_plot) && input$generate_plot > 0
  })
  outputOptions(output, "plot_generated", suspendWhenHidden = FALSE)
  
  output$download_custom_plot <- downloadHandler(
    filename = function() {
      paste0("fhir_custom_", input$plot_x_var, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      p <- custom_plot_reactive()
      
      p_final <- tryCatch({
        if (file.exists(HC_LOGO_PATH)) {
          add_logo_cowplot(p)
        } else {
          p
        }
      }, error = function(e) {
        p
      })
      
      png(file, width = 12, height = 9, units = "in", res = 300, bg = "white")
      print(p_final)
      dev.off()
    }
  )
  outputOptions(output, "download_custom_plot", suspendWhenHidden = FALSE)
  
  # ---- Authors (full dataset) ---------------------------------------------------
  plot_authors_reactive <- reactive({
    df <- resources_tbl
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
      p <- plot_authors_reactive()
      
      p_final <- tryCatch({
        if (file.exists(HC_LOGO_PATH)) {
          add_logo_cowplot(p)
        } else {
          p
        }
      }, error = function(e) {
        p
      })
      
      png(file, width = 12, height = 8, units = "in", res = 300, bg = "white")
      print(p_final)
      dev.off()
    }
  )
  outputOptions(output, "download_plot_authors", suspendWhenHidden = FALSE)
  
  output$tbl_authors <- renderDT({
    df <- resources_tbl
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
  
  # ---- Evolution (uses .rds data) -----------------------------------------------
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
  
  # ---- Resource Changes Tab -----------------------------------------------------
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
  
  # ---- Tables -------------------------------------------------------------------
  output$tbl_resources <- renderDT({
    datatable(
      resources_tbl, 
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
