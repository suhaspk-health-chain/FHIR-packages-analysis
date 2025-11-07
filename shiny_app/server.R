# ==============================================================
# server.R — FHIR Packages Dashboard Server Logic (With Version & Realm Filters)
# ==============================================================

function(input, output, session) {
  
  # ---- Initialize realm filter choices dynamically ----
  observe({
    realm_choices <- resources_tbl %>%
      filter(!is.na(realm), realm != "", realm != "NA") %>%
      count(realm, sort = TRUE) %>%
      pull(realm)
    
    updateSelectizeInput(
      session,
      "realm_filter",
      choices = realm_choices,
      server = TRUE
    )
  })
  
  # ---- NEW: Initialize FHIR version filter choices dynamically ----
  observe({
    version_choices <- resources_tbl %>%
      filter(!is.na(version), version != "", version != "NA") %>%
      count(version, sort = TRUE) %>%
      pull(version)
    
    updateSelectizeInput(
      session,
      "version_filter_plot",
      choices = version_choices,
      server = TRUE
    )
  })
  
  # ---- Create filtered dataset based on version AND realm selection ----
  filtered_data <- reactive({
    df <- resources_tbl
    
    # Apply FHIR version filter
    if (!is.null(input$version_filter_plot) && length(input$version_filter_plot) > 0) {
      df <- df %>%
        filter(version %in% input$version_filter_plot)
    }
    
    # Apply realm filter
    if (!is.null(input$realm_filter) && length(input$realm_filter) > 0) {
      df <- df %>%
        filter(realm %in% input$realm_filter)
    }
    
    return(df)
  })
  
  # ---- Meta info outputs ----
  output$meta_built_at <- renderText({
    if (!is.null(meta$built_at)) format(meta$built_at, "%Y-%m-%d %H:%M") else "n/a"
  })
  
  output$meta_processed <- renderText({
    meta$source_dirs$processed %||% "n/a"
  })
  
  output$meta_raw <- renderText({
    meta$source_dirs$raw %||% "n/a"
  })
  
  # ---- KPIs (use filtered data) ----
  output$kpi_rows <- renderText({
    scales::comma(nrow(filtered_data()))
  })
  
  output$kpi_packages <- renderText({
    scales::comma(n_distinct(filtered_data()$package_text))
  })
  
  output$kpi_resources <- renderText({
    scales::comma(n_distinct(filtered_data()$identity_text))
  })
  
  output$kpi_authors <- renderText({
    df <- filtered_data()
    auth_count <- df %>%
      filter(!is.na(auth), auth != "") %>%
      pull(auth) %>%
      n_distinct()
    scales::comma(auth_count)
  })
  
  # ---- Custom Plot Generation (BAR CHART WITH FACETING + HEATMAP) ----
  custom_plot_reactive <- eventReactive(input$generate_plot, {
    df <- filtered_data()
    req(nrow(df) > 0)
    req(input$plot_x_var)
    req(input$plot_type)
    
    var_labels <- c(
      "version" = "FHIR Version",
      "realm" = "Realm",
      "status" = "Status",
      "auth" = "Author"
    )
    
    x_label <- var_labels[input$plot_x_var]
    x_var <- input$plot_x_var
    top_n <- input$top_n %||% 15
    
    df_clean <- df %>%
      filter(!is.na(!!sym(x_var)), !!sym(x_var) != "", !!sym(x_var) != "NA")
    
    req(nrow(df_clean) > 0)
    
    top_values <- df_clean %>%
      count(!!sym(x_var), sort = TRUE) %>%
      slice_head(n = top_n) %>%
      pull(!!sym(x_var))
    
    df_plot <- df_clean %>%
      filter(!!sym(x_var) %in% top_values)
    
    # ============ BAR CHART (WITH OPTIONAL FACETING) ============
    if (input$plot_type == "bar") {
      use_facet <- !is.null(input$facet_var) && input$facet_var != "none"
      
      if (use_facet) {
        facet_var <- input$facet_var
        facet_label <- var_labels[facet_var]
        
        # Get top facet values
        top_facet_values <- df_plot %>%
          filter(!is.na(!!sym(facet_var)), !!sym(facet_var) != "") %>%
          count(!!sym(facet_var), sort = TRUE) %>%
          slice_head(n = 6) %>%
          pull(!!sym(facet_var))
        
        df_bar <- df_plot %>%
          filter(
            !is.na(!!sym(facet_var)),
            !!sym(facet_var) %in% top_facet_values
          ) %>%
          count(!!sym(x_var), !!sym(facet_var), sort = TRUE)
        
        # Build subtitle with active filters
        subtitle_parts <- paste("Faceted by", facet_label, "- Top 6 facet values")
        if (!is.null(input$version_filter_plot) && length(input$version_filter_plot) > 0) {
          subtitle_parts <- paste0(subtitle_parts, " | Filtered by FHIR Version")
        }
        if (!is.null(input$realm_filter) && length(input$realm_filter) > 0) {
          subtitle_parts <- paste0(subtitle_parts, " | Filtered by Realm")
        }
        
        p <- df_bar %>%
          ggplot(aes(x = reorder(!!sym(x_var), n), y = n, fill = !!sym(x_var))) +
          geom_col(show.legend = FALSE, color = "white", linewidth = 0.5) +
          geom_text(aes(label = scales::comma(n)), hjust = -0.2, size = 3.5, color = "#0f1f2e") +
          coord_flip() +
          scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
          facet_wrap(as.formula(paste("~", facet_var)), scales = "free_y", ncol = 2) +
          labs(
            title = paste("FHIR Package Distribution by", x_label),
            subtitle = subtitle_parts,
            x = x_label,
            y = "Package Count"
          ) +
          theme_healthchain(base_size = 12) +
          theme(
            strip.text = element_text(face = "bold", size = 11),
            strip.background = element_rect(fill = "#f26d21", color = "#0c223f", linewidth = 1)
          )
        
      } else {
        # Simple bar chart without faceting
        
        # Build subtitle with active filters
        subtitle_parts <- paste("Top", top_n, tolower(x_label), "values")
        if (!is.null(input$version_filter_plot) && length(input$version_filter_plot) > 0) {
          subtitle_parts <- paste0(subtitle_parts, " | Filtered by FHIR Version")
        }
        if (!is.null(input$realm_filter) && length(input$realm_filter) > 0) {
          subtitle_parts <- paste0(subtitle_parts, " | Filtered by Realm")
        }
        
        p <- df_plot %>%
          count(!!sym(x_var), sort = TRUE) %>%
          ggplot(aes(x = reorder(!!sym(x_var), n), y = n, fill = !!sym(x_var))) +
          geom_col(show.legend = FALSE, color = "white", linewidth = 0.5) +
          geom_text(aes(label = scales::comma(n)), hjust = -0.2, size = 4, color = "#0f1f2e") +
          coord_flip() +
          scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
          labs(
            title = paste("FHIR Package Distribution by", x_label),
            subtitle = subtitle_parts,
            x = x_label,
            y = "Package Count"
          ) +
          theme_healthchain(base_size = 13)
      }
      
      # ============ HEATMAP ============
    } else if (input$plot_type == "heatmap") {
      y_var <- input$plot_y_var
      y_label <- var_labels[y_var]
      
      top_x <- df %>%
        filter(!is.na(!!sym(x_var)), !!sym(x_var) != "") %>%
        count(!!sym(x_var), sort = TRUE) %>%
        slice_head(n = min(top_n, 15)) %>%
        pull(!!sym(x_var))
      
      top_y <- df %>%
        filter(!is.na(!!sym(y_var)), !!sym(y_var) != "") %>%
        count(!!sym(y_var), sort = TRUE) %>%
        slice_head(n = min(top_n, 15)) %>%
        pull(!!sym(y_var))
      
      heatmap_data <- df %>%
        filter(
          !!sym(x_var) %in% top_x,
          !!sym(y_var) %in% top_y
        ) %>%
        count(!!sym(x_var), !!sym(y_var)) %>%
        complete(!!sym(x_var), !!sym(y_var), fill = list(n = 0))
      
      # Build subtitle with active filters
      subtitle_parts <- paste("Cell color intensity shows package count. Top", length(top_x), "×", length(top_y), "combinations")
      if (!is.null(input$version_filter_plot) && length(input$version_filter_plot) > 0) {
        subtitle_parts <- paste0(subtitle_parts, " | Filtered by FHIR Version")
      }
      if (!is.null(input$realm_filter) && length(input$realm_filter) > 0) {
        subtitle_parts <- paste0(subtitle_parts, " | Filtered by Realm")
      }
      
      p <- heatmap_data %>%
        ggplot(aes(x = !!sym(x_var), y = !!sym(y_var), fill = n)) +
        geom_tile(color = "white", linewidth = 1) +
        geom_text(aes(label = ifelse(n > 0, scales::comma(n), "")),
                  color = "white", fontface = "bold", size = 3.5) +
        scale_fill_gradientn(
          colors = c("#0c223f", "#123358", "#f26d21", "#ffa500"),
          values = scales::rescale(c(0, 0.25, 0.75, 1)),
          name = "Package\nCount",
          labels = scales::comma
        ) +
        labs(
          title = paste("Package Distribution Heatmap:", x_label, "vs", y_label),
          subtitle = subtitle_parts,
          x = x_label,
          y = y_label
        ) +
        theme_healthchain(base_size = 12) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.text.y = element_text(size = 10),
          panel.grid = element_blank(),
          legend.position = "right"
        ) +
        coord_fixed(ratio = 1)
    }
    
    return(p)
  })
  
  output$custom_plot <- renderPlot({
    custom_plot_reactive()
  }, height = 800, width = "auto")
  
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
        if (file.exists(HC_LOGO_PATH) && HC_LOGO_PATH != "") {
          add_logo_cowplot(p)
        } else {
          p
        }
      }, error = function(e) {
        p
      })
      
      if (input$plot_type == "heatmap") {
        png(file, width = 12, height = 12, units = "in", res = 300, bg = "white")
      } else if (input$plot_type == "bar" && !is.null(input$facet_var) && input$facet_var != "none") {
        png(file, width = 14, height = 10, units = "in", res = 300, bg = "white")
      } else {
        png(file, width = 12, height = 9, units = "in", res = 300, bg = "white")
      }
      
      print(p_final)
      dev.off()
    }
  )
  
  outputOptions(output, "download_custom_plot", suspendWhenHidden = FALSE)
  
  # ---- Evolution Tab: Initialize version filter choices ----
  observe({
    dc <- delta_counts
    req(nrow(dc) > 0)
    
    # Extract all unique versions from transitions
    all_versions <- unique(unlist(strsplit(
      gsub(" → ", ",", dc$transition),
      ","
    )))
    all_versions <- trimws(all_versions)
    all_versions <- sort(unique(all_versions))
    
    updateSelectizeInput(
      session,
      "version_filter",
      choices = all_versions,
      server = TRUE
    )
  })
  
  # ---- Evolution Tab: Filtered data with smart transition merging ----
  filtered_delta_counts <- reactive({
    dc <- delta_counts
    req(nrow(dc) > 0)
    
    # If no version selected, return all transitions
    if (is.null(input$version_filter) || length(input$version_filter) == 0) {
      return(dc)
    }
    
    selected_versions <- input$version_filter
    
    # Step 1: Extract all versions from original transitions
    all_transitions <- dc %>%
      mutate(
        from_version = str_trim(str_extract(transition, "^[^ ]+")),
        to_version = str_trim(str_extract(transition, "[^ ]+$"))
      )
    
    # Step 2: Get all unique versions in order
    all_versions_ordered <- unique(c(
      all_transitions$from_version,
      all_transitions$to_version
    ))
    
    # Step 3: Keep only selected versions, preserving order
    kept_versions <- intersect(all_versions_ordered, selected_versions)
    
    # Step 4: Create new sequential transitions
    if (length(kept_versions) < 2) {
      return(tibble())
    }
    
    new_transitions <- tibble()
    
    for (i in 1:(length(kept_versions) - 1)) {
      from_v <- kept_versions[i]
      to_v <- kept_versions[i + 1]
      new_transition <- paste(from_v, "→", to_v)
      
      # Step 5: Find all original transitions that bridge these versions
      from_idx <- which(all_versions_ordered == from_v)
      to_idx <- which(all_versions_ordered == to_v)
      intermediate_versions <- all_versions_ordered[from_idx:to_idx]
      
      # Find all rows that transition within this range
      bridging_rows <- all_transitions %>%
        filter(
          from_version %in% intermediate_versions,
          to_version %in% intermediate_versions
        )
      
      if (nrow(bridging_rows) > 0) {
        # Aggregate the data
        added_total <- sum(bridging_rows$added, na.rm = TRUE)
        removed_total <- sum(bridging_rows$removed, na.rm = TRUE)
        
        # Combine resource names
        added_resources_combined <- bridging_rows %>%
          filter(!is.na(added_resources), added_resources != "") %>%
          pull(added_resources) %>%
          paste(collapse = ", ")
        
        removed_resources_combined <- bridging_rows %>%
          filter(!is.na(removed_resources), removed_resources != "") %>%
          pull(removed_resources) %>%
          paste(collapse = ", ")
        
        new_transitions <- new_transitions %>%
          bind_rows(tibble(
            transition = new_transition,
            added_resources = if (added_resources_combined == "") NA_character_ else added_resources_combined,
            removed_resources = if (removed_resources_combined == "") NA_character_ else removed_resources_combined,
            added = added_total,
            removed = removed_total
          ))
      }
    }
    
    return(new_transitions)
  })
  
  # ---- Evolution Plot: Resources Added (with version filter) ----
  plot_added_reactive <- reactive({
    dc <- filtered_delta_counts()
    
    if (nrow(dc) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No transitions match selected versions",
                   size = 5, color = "#666") +
          theme_void()
      )
    }
    
    # Maintain original transition order
    dc <- dc %>%
      mutate(transition = factor(transition, levels = transition))
    
    ggplot(dc, aes(x = transition, y = added, fill = transition)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      geom_text(aes(label = scales::comma(added)), hjust = -0.2, size = 4, color = "#0f1f2e") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(
        title = "Resources Added per Transition",
        x = "Version Transition",
        y = "Resources Added"
      ) +
      theme_healthchain(base_size = 12)
  })
  
  output$plot_added <- renderPlot({ plot_added_reactive() }, height = 450, width = "auto")
  
  output$download_plot_added <- downloadHandler(
    filename = function() { paste0("resources_added_", Sys.Date(), ".png") },
    content = function(file) {
      p <- plot_added_reactive()
      p_final <- tryCatch({
        if (file.exists(HC_LOGO_PATH) && HC_LOGO_PATH != "") add_logo_cowplot(p) else p
      }, error = function(e) p)
      png(file, width = 10, height = 7, units = "in", res = 300, bg = "white")
      print(p_final)
      dev.off()
    }
  )
  
  outputOptions(output, "download_plot_added", suspendWhenHidden = FALSE)
  
  # ---- Evolution Plot: Resources Removed (with version filter) ----
  plot_removed_reactive <- reactive({
    dc <- filtered_delta_counts()
    
    if (nrow(dc) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No transitions match selected versions",
                   size = 5, color = "#666") +
          theme_void()
      )
    }
    
    # Maintain original transition order
    dc <- dc %>%
      mutate(transition = factor(transition, levels = transition))
    
    ggplot(dc, aes(x = transition, y = removed, fill = transition)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      geom_text(aes(label = scales::comma(removed)), hjust = -0.2, size = 4, color = "#0f1f2e") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(
        title = "Resources Removed per Transition",
        x = "Version Transition",
        y = "Resources Removed"
      ) +
      theme_healthchain(base_size = 12)
  })
  
  output$plot_removed <- renderPlot({ plot_removed_reactive() }, height = 450, width = "auto")
  
  output$download_plot_removed <- downloadHandler(
    filename = function() { paste0("resources_removed_", Sys.Date(), ".png") },
    content = function(file) {
      p <- plot_removed_reactive()
      p_final <- tryCatch({
        if (file.exists(HC_LOGO_PATH) && HC_LOGO_PATH != "") add_logo_cowplot(p) else p
      }, error = function(e) p)
      png(file, width = 10, height = 7, units = "in", res = 300, bg = "white")
      print(p_final)
      dev.off()
    }
  )
  
  outputOptions(output, "download_plot_removed", suspendWhenHidden = FALSE)
  
  # ---- Evolution Table: Transition Summary (with version filter) ----
  output$tbl_transitions <- renderDT({
    dc <- filtered_delta_counts()
    
    if (nrow(dc) == 0) {
      datatable(
        data.frame(Message = "No transitions match selected versions"),
        options = list(dom='t'),
        rownames = FALSE
      )
    } else {
      datatable(
        dc,
        class = "stripe hover compact",
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    }
  })
  
  # ---- Evolution Table: Detailed Resource Changes (with version filter) ----
  output$tbl_detailed_resource_changes <- renderDT({
    dc <- filtered_delta_counts()
    
    if (nrow(dc) == 0) {
      datatable(
        data.frame(Message = "No transitions match selected versions"),
        options = list(dom='t'),
        rownames = FALSE
      )
    } else {
      # Process data using tidyr
      added_data <- NULL
      removed_data <- NULL
      
      if ("added_resources" %in% names(dc)) {
        added_data <- dc %>%
          select(transition, added_resources) %>%
          filter(!is.na(added_resources), added_resources != "") %>%
          separate_rows(added_resources, sep = ",\\s*") %>%
          mutate(
            `Change Type` = "Added",
            `Resource Name` = trimws(added_resources)
          ) %>%
          select(Transition = transition, `Change Type`, `Resource Name`)
      }
      
      if ("removed_resources" %in% names(dc)) {
        removed_data <- dc %>%
          select(transition, removed_resources) %>%
          filter(!is.na(removed_resources), removed_resources != "") %>%
          separate_rows(removed_resources, sep = ",\\s*") %>%
          mutate(
            `Change Type` = "Removed",
            `Resource Name` = trimws(removed_resources)
          ) %>%
          select(Transition = transition, `Change Type`, `Resource Name`)
      }
      
      detailed_df <- bind_rows(added_data, removed_data)
      
      if (is.null(detailed_df) || nrow(detailed_df) == 0) {
        datatable(
          data.frame(Message = "No resource changes found"),
          options = list(dom='t'),
          rownames = FALSE
        )
      } else {
        datatable(
          detailed_df,
          class = "stripe hover compact",
          filter = 'top',
          options = list(
            pageLength = 25,
            scrollY = "600px",
            scrollCollapse = TRUE,
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel'),
            order = list(list(0, 'asc'), list(1, 'asc'))
          ),
          extensions = 'Buttons',
          rownames = FALSE
        ) %>%
          formatStyle(
            'Change Type',
            target = 'row',
            backgroundColor = styleEqual(
              c('Added', 'Removed'),
              c('#d4edda', '#f8d7da')
            )
          ) %>%
          formatStyle(
            'Change Type',
            color = styleEqual(
              c('Added', 'Removed'),
              c('#155724', '#721c24')
            ),
            fontWeight = 'bold'
          )
      }
    }
  })
  
  # ---- Data Tables ----
  output$tbl_resources <- renderDT({
    datatable(resources_tbl, class = "stripe hover compact",
              options = list(pageLength = 20, scrollX = TRUE, scrollY = "600px", scrollCollapse = TRUE),
              rownames = FALSE, filter = 'top')
  })
  
  output$tbl_matrix <- renderDT({
    if (!nrow(matrix_tbl)) {
      datatable(data.frame(message="No matrix available"), options = list(dom='t'))
    } else {
      datatable(matrix_tbl, class = "stripe hover compact",
                options = list(pageLength = 20, scrollX = TRUE, scrollY = "600px", scrollCollapse = TRUE), rownames = FALSE)
    }
  })
  
  output$tbl_stable <- renderDT({
    if (!nrow(stable_tbl)) {
      datatable(data.frame(message="No stable list available"), options = list(dom='t'))
    } else {
      datatable(stable_tbl, class = "stripe hover compact",
                options = list(pageLength = 20, scrollX = TRUE, scrollY = "600px", scrollCollapse = TRUE), rownames = FALSE)
    }
  })
  
  output$tbl_raw <- renderDT({
    if (!nrow(raw_preview)) {
      datatable(data.frame(message="Raw preview not available"), options = list(dom='t'))
    } else {
      datatable(raw_preview, class = "stripe hover compact",
                options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    }
  })
}
