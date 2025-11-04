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
  
  # ---- NEW: Interactive Custom Plot Generation (ALL PLOT TYPES) ----------------
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
    top_n <- input$top_n %||% 15
    
    # Clean data - remove NA and empty values
    df_clean <- df %>%
      filter(!is.na(.data[[x_var]]), .data[[x_var]] != "", .data[[x_var]] != "NA")
    
    req(nrow(df_clean) > 0)
    
    # Limit to top N values
    top_values <- df_clean %>%
      count(.data[[x_var]], sort = TRUE) %>%
      slice_head(n = top_n) %>%
      pull(.data[[x_var]])
    
    df_plot <- df_clean %>%
      filter(.data[[x_var]] %in% top_values)
    
    # ============ PLOT TYPE 1: SIMPLE BAR CHART ============
    if (input$plot_type == "bar") {
      p <- df_plot %>%
        count(.data[[x_var]], sort = TRUE) %>%
        ggplot(aes(x = reorder(.data[[x_var]], n), y = n, fill = .data[[x_var]])) +
        geom_col(show.legend = FALSE, color = "white", linewidth = 0.5) +
        geom_text(aes(label = scales::comma(n)), hjust = -0.2, size = 4, color = "#0f1f2e") +
        coord_flip() +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(
          title = paste("FHIR Package Distribution by", x_label),
          subtitle = paste("Top", top_n, tolower(x_label), "values"),
          x = x_label,
          y = "Package Count"
        ) +
        theme_healthchain(base_size = 13)
      
      # ============ PLOT TYPE 2: PIE CHART ============
    } else if (input$plot_type == "pie") {
      # Limit to top 10 for pie chart clarity
      pie_data <- df_plot %>%
        count(.data[[x_var]], sort = TRUE) %>%
        slice_head(n = min(10, top_n)) %>%
        mutate(
          percentage = n / sum(n) * 100,
          label_text = paste0(.data[[x_var]], "\n", scales::comma(n), " (", round(percentage, 1), "%)")
        )
      
      p <- pie_data %>%
        ggplot(aes(x = "", y = n, fill = .data[[x_var]])) +
        geom_col(color = "white", linewidth = 2) +
        geom_text(
          aes(label = label_text),
          position = position_stack(vjust = 0.5),
          size = 3.5,
          color = "#0f1f2e",
          fontface = "bold"
        ) +
        coord_polar(theta = "y", start = 0) +
        scale_fill_manual(values = RColorBrewer::brewer.pal(min(10, nrow(pie_data)), "Set3")) +
        labs(
          title = paste("FHIR Package Distribution by", x_label),
          subtitle = paste("Top", nrow(pie_data), tolower(x_label), "values"),
          fill = x_label
        ) +
        theme_healthchain(base_size = 13) +
        theme(
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          legend.position = "right"
        )
      
      # ============ PLOT TYPE 3: GROUPED BAR CHART ============
    } else if (input$plot_type == "grouped") {
      y_var <- input$plot_y_var
      y_label <- var_labels[y_var]
      
      # Get top 8 for fill variable
      top_y_values <- df_plot %>%
        filter(!is.na(.data[[y_var]]), .data[[y_var]] != "") %>%
        count(.data[[y_var]], sort = TRUE) %>%
        slice_head(n = 8) %>%
        pull(.data[[y_var]])
      
      df_grouped <- df_plot %>%
        filter(!is.na(.data[[y_var]]), .data[[y_var]] %in% top_y_values) %>%
        count(.data[[x_var]], .data[[y_var]], sort = TRUE)
      
      p <- df_grouped %>%
        ggplot(aes(x = .data[[x_var]], y = n, fill = .data[[y_var]])) +
        geom_col(position = "dodge", width = 0.8, color = "white", linewidth = 0.8) +
        geom_text(
          aes(label = scales::comma(n)),
          position = position_dodge(width = 0.8),
          angle = 0,
          hjust = -0.1,
          size = 3,
          color = "#0f1f2e"
        ) +
        coord_flip() +
        scale_fill_manual(values = RColorBrewer::brewer.pal(8, "Set2")) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
        labs(
          title = paste("FHIR Package Distribution by", x_label, "and", y_label),
          x = x_label,
          y = "Package Count",
          fill = y_label
        ) +
        theme_healthchain(base_size = 12) +
        theme(
          legend.position = "right",
          legend.title = element_text(face = "bold")
        )
      
      # ============ PLOT TYPE 4: STACKED BAR CHART ============
    } else if (input$plot_type == "stacked") {
      y_var <- input$plot_y_var
      y_label <- var_labels[y_var]
      
      # Get top 8 categories for stacking
      top_y_values <- df_plot %>%
        filter(!is.na(.data[[y_var]]), .data[[y_var]] != "") %>%
        count(.data[[y_var]], sort = TRUE) %>%
        slice_head(n = 8) %>%
        pull(.data[[y_var]])
      
      df_stacked <- df_plot %>%
        filter(!is.na(.data[[y_var]]), .data[[y_var]] != "") %>%
        mutate(
          y_category = ifelse(.data[[y_var]] %in% top_y_values, 
                              as.character(.data[[y_var]]), 
                              "Other")
        ) %>%
        count(.data[[x_var]], y_category, sort = TRUE) %>%
        group_by(.data[[x_var]]) %>%
        mutate(
          total = sum(n),
          percentage = n / total * 100,
          cum_sum = cumsum(n),
          label_pos = cum_sum - n/2,
          show_label = percentage >= 5
        ) %>%
        ungroup()
      
      p <- df_stacked %>%
        ggplot(aes(x = reorder(.data[[x_var]], total), y = n, fill = y_category)) +
        geom_col(position = "stack", color = "white", linewidth = 2) +
        geom_text(
          data = . %>% filter(show_label),
          aes(y = label_pos, label = scales::comma(n)),
          angle = 0,
          hjust = 0.5,
          size = 4,
          color = "#0f1f2e",
          fontface = "bold"
        ) +
        coord_flip() +
        scale_fill_manual(
          values = RColorBrewer::brewer.pal(9, "Set1"),
          name = paste(y_label, "(Top 8)")
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
        labs(
          title = paste("FHIR Package Distribution by", x_label),
          subtitle = paste0("Grouped by ", tolower(y_label), ". Labels shown for segments ≥5%"),
          x = x_label,
          y = "Package Count"
        ) +
        theme_healthchain(base_size = 13) +
        theme(
          legend.position = "right",
          legend.title = element_text(face = "bold", size = 12),
          panel.grid.major.y = element_blank()
        )
      
      # ============ PLOT TYPE 5: SCATTER PLOT WITH TREND LINE ============
      # ============ PLOT TYPE 5: IMPROVED SCATTER PLOT ============
    } else if (input$plot_type == "scatter") {
      y_var <- input$plot_y_var
      y_label <- var_labels[y_var]
      
      # Create aggregated data for scatter plot
      df_scatter <- df %>%
        filter(
          !is.na(.data[[x_var]]), .data[[x_var]] != "",
          !is.na(.data[[y_var]]), .data[[y_var]] != ""
        ) %>%
        count(.data[[x_var]], .data[[y_var]]) %>%
        group_by(.data[[x_var]]) %>%
        mutate(x_count = sum(n)) %>%
        group_by(.data[[y_var]]) %>%
        mutate(y_count = sum(n)) %>%
        ungroup()
      
      # Get number of colors needed
      n_colors <- length(unique(df_scatter[[x_var]]))
      
      # Use Dark2 palette - much better visibility on white background
      if (n_colors <= 8) {
        color_palette <- RColorBrewer::brewer.pal(max(3, n_colors), "Dark2")[1:n_colors]
      } else if (n_colors <= 12) {
        # Combine Dark2 and Set1 for more colors
        color_palette <- c(
          RColorBrewer::brewer.pal(8, "Dark2"),
          RColorBrewer::brewer.pal(min(n_colors - 8, 9), "Set1")
        )[1:n_colors]
      } else {
        # Use multiple palettes for many colors
        color_palette <- rep(
          c(RColorBrewer::brewer.pal(8, "Dark2"), 
            RColorBrewer::brewer.pal(9, "Set1")), 
          length.out = n_colors
        )
      }
      
      # Create base scatter plot with improved styling
      p <- df_scatter %>%
        ggplot(aes(x = x_count, y = y_count, color = .data[[x_var]], size = n)) +
        geom_point(alpha = 0.8, stroke = 1) +  # Increased alpha and added stroke
        scale_size_continuous(
          range = c(4, 20), 
          name = "Intersection\nCount",
          breaks = c(100, 1000, 5000, 10000)
        ) +
        scale_color_manual(
          values = color_palette,
          name = x_label
        ) +
        labs(
          title = paste("Relationship between", x_label, "and", y_label),
          subtitle = paste(
            "Each point represents a", tolower(x_label), "-", tolower(y_label),
            "combination.\nBubble size shows package count at that intersection."
          ),
          x = paste("Total Packages by", x_label),
          y = paste("Total Packages by", y_label)
        ) +
        theme_healthchain(base_size = 12) +
        theme(
          legend.position = "right",
          legend.title = element_text(face = "bold", size = 11),
          legend.text = element_text(size = 9),
          panel.grid.major = element_line(color = "#e0e0e0", linewidth = 0.3),
          panel.grid.minor = element_line(color = "#f0f0f0", linewidth = 0.2)
        )
      
      # Add trend line if requested with better styling
      if (input$show_trend) {
        p <- p + 
          geom_smooth(
            method = "lm", 
            se = TRUE, 
            color = "#f26d21",      # Health Chain orange
            fill = "#f26d21",
            alpha = 0.15,
            linewidth = 2,
            linetype = "solid"
          ) +
          # Add correlation annotation
          annotate(
            "text",
            x = Inf,
            y = Inf,
            label = "Negative Correlation:\nHigher Status counts →\nLower Realm counts",
            hjust = 1.1,
            vjust = 1.5,
            size = 3.5,
            color = "#0c223f",
            fontface = "italic"
          )
      }
    }
    
    
    return(p)
  })
  
  output$custom_plot <- renderPlot({
    custom_plot_reactive()
  }, height = 600, width = "auto")
  
  output$plot_generated <- reactive({
    !is.null(input$generate_plot) && input$generate_plot > 0
  })
  outputOptions(output, "plot_generated", suspendWhenHidden = FALSE)
  
  output$download_custom_plot <- downloadHandler(
    filename = function() {
      paste0("fhir_custom_", input$plot_type, "_", input$plot_x_var, "_", Sys.Date(), ".png")
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
      
      # Adjust size based on plot type
      if (input$plot_type == "scatter") {
        png(file, width = 14, height = 10, units = "in", res = 300, bg = "white")
      } else {
        png(file, width = 12, height = 9, units = "in", res = 300, bg = "white")
      }
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
