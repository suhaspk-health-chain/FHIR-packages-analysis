# ==============================================================
# mod_evolution.R - FIXED
# ==============================================================

mod_evolution_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    h2(icon("code-branch"), " FHIR Version Evolution", style = "color: #0c223f; margin-bottom: 10px;"),
    wellPanel(
      style="background:#fff8f3; border-left:4px solid #f26d21; padding:14px 18px; margin-bottom:16px;",
      p(style="margin:0; font-size:15px; line-height:1.6;",
        strong("What you're looking at: "),
        "FHIR has gone through six versions (R2 through R6). Each version adds new resource types
        (healthcare concepts that didn't exist before) and removes deprecated ones. ",
        strong("'Added'"), " means a brand-new type of healthcare record was introduced. ",
        strong("'Removed'"), " means an old one was retired (usually merged into something better).
        R4B was a minor branch of R4 focused on pharmaceutical data. R5 is the current stable release.
        R6 is experimental. Use the filters below to focus on specific versions or country realms."
      )
    ),
    fluidRow(
      column(6, selectizeInput(ns("version_filter"), "Filter by FHIR Version:",
                               choices = NULL, multiple = TRUE,
                               options = list(placeholder = "All versions (default)"))),
      column(6, selectizeInput(ns("realm_filter"), "Filter by Realm (for realm-specific charts):",
                               choices = NULL, multiple = TRUE,
                               options = list(placeholder = "Top 6 realms (default)")))
    ),

    h3("Overall Version Transitions", style="color:#0c223f; margin-top:20px;"),
    fluidRow(
      column(6, 
             plotOutput(ns("plot_added"), height = "450px"),
             downloadButton(ns("download_added"), "Download", style = "margin-top: 10px;")
      ),
      column(6, 
             plotOutput(ns("plot_removed"), height = "450px"),
             downloadButton(ns("download_removed"), "Download", style = "margin-top: 10px;")
      )
    ),
    
    h3("Realm-Specific Evolution", style = "margin-top: 30px;"),
    fluidRow(
      column(6, 
             plotOutput(ns("plot_realm_added"), height = "550px"),
             downloadButton(ns("download_realm_added"), "Download", style = "margin-top: 10px;")
      ),
      column(6, 
             plotOutput(ns("plot_realm_removed"), height = "550px"),
             downloadButton(ns("download_realm_removed"), "Download", style = "margin-top: 10px;")
      )
    ),
    
    h3("Transition Details", style = "margin-top: 30px;"),
    DTOutput(ns("tbl_transitions"))
  )
}

mod_evolution_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize filters
    observe({
      dc <- delta_counts
      version_order <- c("R2", "R3", "R4", "R4B", "R5", "R6")
      
      if (nrow(dc) > 0) {
        all_versions <- unique(c(
          str_trim(str_extract(dc$transition, "^[^→]+")),
          str_trim(str_extract(dc$transition, "[^→]+$"))
        ))
        all_versions <- version_order[version_order %in% all_versions]
      } else {
        all_versions <- version_order
      }
      
      updateSelectizeInput(session, "version_filter", choices = all_versions, server = TRUE)
    })
    
    observe({
      realm_choices <- resources_tbl %>%
        filter(!is.na(realm), realm != "", realm != "none") %>%
        count(realm, sort = TRUE) %>%
        pull(realm)
      updateSelectizeInput(session, "realm_filter", choices = realm_choices, server = TRUE)
    })
    
    # Filtered delta
    filtered_delta <- reactive({
      dc <- delta_counts
      req(nrow(dc) > 0)
      
      if (is.null(input$version_filter) || length(input$version_filter) == 0) {
        return(dc)
      }
      
      selected_versions <- input$version_filter
      all_transitions <- dc %>%
        mutate(
          from_version = str_trim(str_extract(transition, "^[^→]+")),
          to_version = str_trim(str_extract(transition, "[^→]+$"))
        )
      
      all_versions_ordered <- unique(c(all_transitions$from_version, all_transitions$to_version))
      kept_versions <- intersect(all_versions_ordered, selected_versions)
      
      if (length(kept_versions) < 2) return(tibble())
      
      new_transitions <- tibble()
      for (i in 1:(length(kept_versions) - 1)) {
        from_v <- kept_versions[i]
        to_v <- kept_versions[i + 1]
        new_transition <- paste(from_v, "→", to_v)
        
        from_idx <- which(all_versions_ordered == from_v)
        to_idx <- which(all_versions_ordered == to_v)
        intermediate_versions <- all_versions_ordered[from_idx:to_idx]
        
        bridging_rows <- all_transitions %>%
          filter(from_version %in% intermediate_versions, to_version %in% intermediate_versions)
        
        if (nrow(bridging_rows) > 0) {
          new_transitions <- bind_rows(new_transitions, tibble(
            transition = new_transition,
            added_resources = paste(bridging_rows$added_resources[bridging_rows$added_resources != ""], collapse = ", "),
            removed_resources = paste(bridging_rows$removed_resources[bridging_rows$removed_resources != ""], collapse = ", "),
            added = sum(bridging_rows$added, na.rm = TRUE),
            removed = sum(bridging_rows$removed, na.rm = TRUE)
          ))
        }
      }
      return(new_transitions)
    })
    
    # Plot: Added Resources
    plot_added <- reactive({
      dc <- filtered_delta()
      if (nrow(dc) == 0) {
        return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No transitions", size = 5) + theme_void())
      }
      
      dc <- dc %>% mutate(transition = factor(transition, levels = transition))
      
      ggplot(dc, aes(x = transition, y = added, fill = transition)) +
        geom_col(show.legend = FALSE, color = "white", size = 0.3) +
        coord_flip() +
        geom_text(aes(label = scales::comma(added)), hjust = -0.2, size = 4, 
                  color = "#0c223f", fontface = "bold") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        scale_fill_healthchain() +
        labs(title = "Resources Added per Transition", x = "Version Transition", y = "Resources Added") +
        hc_theme()
    })
    
    output$plot_added <- renderPlot({ plot_added() }, height = 450)
    
    # Plot: Removed Resources
    plot_removed <- reactive({
      dc <- filtered_delta()
      if (nrow(dc) == 0) {
        return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No transitions", size = 5) + theme_void())
      }
      
      dc <- dc %>% mutate(transition = factor(transition, levels = transition))
      
      ggplot(dc, aes(x = transition, y = removed, fill = transition)) +
        geom_col(show.legend = FALSE, color = "white", size = 0.3) +
        coord_flip() +
        geom_text(aes(label = scales::comma(removed)), hjust = -0.2, size = 4, 
                  color = "#0c223f", fontface = "bold") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        scale_fill_healthchain() +
        labs(title = "Resources Removed per Transition", x = "Version Transition", y = "Resources Removed") +
        hc_theme()
    })
    
    output$plot_removed <- renderPlot({ plot_removed() }, height = 450)
    
    # Realm-specific plots
    plot_realm_added <- reactive({
      dc <- filtered_delta()
      if (nrow(dc) == 0) return(ggplot() + theme_void())
      
      selected_realms <- input$realm_filter
      if (is.null(selected_realms) || length(selected_realms) == 0) {
        selected_realms <- resources_tbl %>% 
          filter(!is.na(realm), realm != "", realm != "none") %>% 
          pull(realm) %>% unique() %>% head(6)
      }
      
      dc_realm <- tibble()
      for (realm_val in selected_realms) {
        for (i in 1:nrow(dc)) {
          versions <- str_split(dc$transition[i], " → ")[[1]]
          from_v <- str_trim(versions[1])
          to_v <- str_trim(versions[2])
          
          from_res <- resources_tbl %>% filter(realm == realm_val, version == from_v) %>% pull(identity_text) %>% unique()
          to_res <- resources_tbl %>% filter(realm == realm_val, version == to_v) %>% pull(identity_text) %>% unique()
          
          dc_realm <- bind_rows(dc_realm, tibble(
            transition = dc$transition[i],
            realm = realm_val,
            added = length(setdiff(to_res, from_res))
          ))
        }
      }
      
      ggplot(dc_realm, aes(x = transition, y = added, fill = realm)) +
        geom_col(show.legend = FALSE, color = "white", size = 0.3) +
        facet_wrap(~realm, scales = "free_y", ncol = 2) +
        coord_flip() +
        geom_text(aes(label = scales::comma(added)), hjust = -0.2, size = 3.5, 
                  color = "#0c223f", fontface = "bold") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        scale_fill_healthchain() +
        labs(title = "Resources Added - By Realm", x = "Transition", y = "Added") +
        hc_theme()
    })
    
    output$plot_realm_added <- renderPlot({ plot_realm_added() }, height = 550)
    
    plot_realm_removed <- reactive({
      dc <- filtered_delta()
      if (nrow(dc) == 0) return(ggplot() + theme_void())
      
      selected_realms <- input$realm_filter
      if (is.null(selected_realms) || length(selected_realms) == 0) {
        selected_realms <- resources_tbl %>% 
          filter(!is.na(realm), realm != "", realm != "none") %>% 
          pull(realm) %>% unique() %>% head(6)
      }
      
      dc_realm <- tibble()
      for (realm_val in selected_realms) {
        for (i in 1:nrow(dc)) {
          versions <- str_split(dc$transition[i], " → ")[[1]]
          from_v <- str_trim(versions[1])
          to_v <- str_trim(versions[2])
          
          from_res <- resources_tbl %>% filter(realm == realm_val, version == from_v) %>% pull(identity_text) %>% unique()
          to_res <- resources_tbl %>% filter(realm == realm_val, version == to_v) %>% pull(identity_text) %>% unique()
          
          dc_realm <- bind_rows(dc_realm, tibble(
            transition = dc$transition[i],
            realm = realm_val,
            removed = length(setdiff(from_res, to_res))
          ))
        }
      }
      
      ggplot(dc_realm, aes(x = transition, y = removed, fill = realm)) +
        geom_col(show.legend = FALSE, color = "white", size = 0.3) +
        facet_wrap(~realm, scales = "free_y", ncol = 2) +
        coord_flip() +
        geom_text(aes(label = scales::comma(removed)), hjust = -0.2, size = 3.5, 
                  color = "#0c223f", fontface = "bold") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        scale_fill_healthchain() +
        labs(title = "Resources Removed - By Realm", x = "Transition", y = "Removed") +
        hc_theme()
    })
    
    output$plot_realm_removed <- renderPlot({ plot_realm_removed() }, height = 550)
    
    # Data table
    output$tbl_transitions <- renderDT({
      dc <- filtered_delta()
      if (nrow(dc) == 0) {
        datatable(data.frame(Message = "No transitions"), options = list(dom = 't'), rownames = FALSE)
      } else {
        datatable(dc, options = list(pageLength = 10, scrollX = TRUE, dom = 'Bfrtip', buttons = c('copy', 'csv')), 
                  extensions = 'Buttons', rownames = FALSE)
      }
    })
    
    evo_caption <- function(chart_name) {
      parts <- c(chart_name, "FHIR XIG Registry", "EDA by Suhas P K",
                 format(Sys.Date(), "%Y-%m-%d"))
      if (length(input$version_filter) > 0)
        parts <- c(parts, paste0("Versions: ", paste(input$version_filter, collapse = ", ")))
      if (length(input$realm_filter) > 0)
        parts <- c(parts, paste0("Realms: ", paste(input$realm_filter, collapse = ", ")))
      paste(parts, collapse = "  |  ")
    }

    output$download_added <- downloadHandler(
      filename = function() paste0("resources_added_", Sys.Date(), ".png"),
      content  = function(file) ggplot2::ggsave(file,
        plot  = add_caption_style(plot_added(), evo_caption("Resources Added per Version Transition")),
        width = 10, height = 7, dpi = 200, bg = "white")
    )

    output$download_removed <- downloadHandler(
      filename = function() paste0("resources_removed_", Sys.Date(), ".png"),
      content  = function(file) ggplot2::ggsave(file,
        plot  = add_caption_style(plot_removed(), evo_caption("Resources Removed per Version Transition")),
        width = 10, height = 7, dpi = 200, bg = "white")
    )

    output$download_realm_added <- downloadHandler(
      filename = function() paste0("realm_added_", Sys.Date(), ".png"),
      content  = function(file) ggplot2::ggsave(file,
        plot  = add_caption_style(plot_realm_added(), evo_caption("Resources Added by Realm; faceted")),
        width = 12, height = 9, dpi = 200, bg = "white")
    )

    output$download_realm_removed <- downloadHandler(
      filename = function() paste0("realm_removed_", Sys.Date(), ".png"),
      content  = function(file) ggplot2::ggsave(file,
        plot  = add_caption_style(plot_realm_removed(), evo_caption("Resources Removed by Realm; faceted")),
        width = 12, height = 9, dpi = 200, bg = "white")
    )
  })
}