# ==============================================================
# mod_hypothesis.R - Interactive Hypothesis Testing Lab
# ==============================================================

# Pre-built hypotheses: each has a UI description + a runner function
# runner(df) returns list(test, viz_plot, plain_english)
HYPOTHESES <- list(

  h1 = list(
    label = "H1 — R4 packages contain more resources than R3 packages",
    h0    = "The median number of resources per package is the same for FHIR R3 and R4.",
    h1    = "FHIR R4 packages contain significantly more resources per package than R3 packages.",
    method = "Wilcoxon Rank-Sum Test (one-sided, R4 > R3)",
    context = paste0(
      "FHIR R4 (2019) introduced 40+ new resource types and is the first normative release. ",
      "If this drove larger, richer packages, we expect the distribution of resources-per-package ",
      "to be right-shifted in R4 relative to R3.")
  ),

  h2 = list(
    label = "H2 — US realm accounts for more than 40% of all packages",
    h0    = "The US realm contributes 40% or fewer of all unique FHIR packages.",
    h1    = "The US realm contributes more than 40% of all unique FHIR packages.",
    method = "One-Sample Proportion Test (one-sided)",
    context = paste0(
      "US federal regulations (CMS-0057F, ONC 21st Century Cures) have driven mass adoption. ",
      "We test whether the US share is statistically above 40%, which would confirm regulatory ",
      "mandates as a dominant driver of ecosystem growth.")
  ),

  h3 = list(
    label = "H3 — Status distribution differs across FHIR versions",
    h0    = "The distribution of resource status (active/draft/retired) is the same across all FHIR versions.",
    h1    = "The distribution of resource status is significantly different across FHIR versions.",
    method = "Chi-Square Test of Independence",
    context = paste0(
      "Older versions should have more 'retired' resources as the standard matures. ",
      "Newer versions should skew toward 'active' as normative content stabilises. ",
      "A significant chi-square result confirms the ecosystem is not static.")
  ),

  h4 = list(
    label = "H4 — ValueSets & StructureDefinitions exceed 70% of all resources",
    h0    = "ValueSets and StructureDefinitions together make up 70% or fewer of all resources.",
    h1    = "ValueSets and StructureDefinitions together account for more than 70% of all resources.",
    method = "One-Sample Proportion Test (one-sided)",
    context = paste0(
      "The hypothesis that FHIR packages are dominated by conformance artefacts ",
      "(rules and terminology) rather than clinical instance data is central to understanding ",
      "what FHIR developers actually work with. A > 70% share would confirm this decisively.")
  ),

  h5 = list(
    label = "H5 — US packages contain more resources per package than non-US packages",
    h0    = "The median resources-per-package is the same for US and non-US realms.",
    h1    = "US packages contain a significantly different number of resources than non-US packages.",
    method = "Wilcoxon Rank-Sum Test (two-sided)",
    context = paste0(
      "US IGs are often comprehensive (Da Vinci, US Core) spanning large clinical domains. ",
      "Non-US IGs may be narrower in scope. A significant difference in package size would ",
      "reflect the US's deeper regulatory prescription about what must be implemented.")
  ),

  h6 = list(
    label = "H6 — Resource type mix differs between US and UV (Universal) realms",
    h0    = "The distribution of resource types is the same in the US and UV realms.",
    h1    = "The US and UV realms have significantly different mixes of resource types.",
    method = "Chi-Square Test of Independence (top 6 resource types)",
    context = paste0(
      "UV (Universal) guides target global interoperability and tend to be more abstract. ",
      "US guides are driven by payer/billing workflows (lots of Coverage, Claim, ExplanationOfBenefit). ",
      "A significant result confirms the ecosystems serve fundamentally different purposes.")
  )
)

# ---- UI ----
mod_hypothesis_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h2(icon("flask"), " Hypothesis Testing Lab", style = "color:#0c223f;"),
    wellPanel(
      style = "background:#fff8f3; border-left:4px solid #f26d21; padding:14px 18px; margin-bottom:16px;",
      p(style = "margin:0; font-size:15px; line-height:1.6;",
        strong("What is this? "),
        "Each claim about the FHIR ecosystem — 'R4 is bigger', 'US dominates', 'conformance resources rule' — ",
        "can be tested statistically. Select a hypothesis, choose a significance level, and run the test. ",
        "Results include the test statistic, p-value, and a plain-English verdict so you can draw ",
        "evidence-based conclusions rather than rely on intuition."
      )
    ),

    # Controls
    fluidRow(
      column(6,
        selectInput(ns("hypothesis"), "Select Hypothesis:",
                    choices = setNames(names(HYPOTHESES), sapply(HYPOTHESES, `[[`, "label")),
                    selected = "h1", width = "100%")
      ),
      column(3,
        selectInput(ns("alpha"), "Significance Level (\u03b1):",
                    choices = c("0.01" = 0.01, "0.05" = 0.05, "0.10" = 0.10),
                    selected = 0.05)
      ),
      column(3,
        br(),
        actionButton(ns("run_test"), "Run Test",
                     class = "btn-primary btn-lg btn-block", icon = icon("play"))
      )
    ),

    # Hypothesis framing card
    uiOutput(ns("hypothesis_card")),

    hr(style = "border-color:#ddd; margin:16px 0;"),

    # Results
    uiOutput(ns("results_panel")),

    # Visualization
    conditionalPanel(
      condition = sprintf("input['%s'] > 0", ns("run_test")),
      h4("Data Behind the Test", style = "color:#0c223f; margin-top:20px;"),
      wellPanel(style = "padding:12px;",
        plotOutput(ns("test_plot"), height = "400px")
      ),
      downloadButton(ns("dl_test_plot"), "Download Chart", style = "margin:8px 0 20px;")
    )
  )
}

# ---- Server ----
mod_hypothesis_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Show hypothesis framing whenever selection changes
    output$hypothesis_card <- renderUI({
      h <- HYPOTHESES[[input$hypothesis]]
      wellPanel(
        style = "background:#f0f8ff; border:1px solid #cde; padding:16px; margin-top:12px;",
        fluidRow(
          column(6,
            tags$p(tags$b("H\u2080 (Null):"), style = "margin-bottom:4px;"),
            tags$p(h$h0, style = "color:#555; font-size:14px; line-height:1.5;"),
            tags$p(tags$b("H\u2081 (Alternative):"), style = "margin:8px 0 4px;"),
            tags$p(h$h1, style = "color:#0c223f; font-size:14px; line-height:1.5;")
          ),
          column(6,
            tags$p(tags$b("Statistical Method:"),
                   tags$span(h$method, style = "color:#f26d21; font-weight:600;"),
                   style = "margin-bottom:8px;"),
            tags$p(tags$b("Why this matters:"), style = "margin-bottom:4px;"),
            tags$p(h$context, style = "color:#555; font-size:13px; line-height:1.6;")
          )
        )
      )
    })

    # Run test result
    test_result <- eventReactive(input$run_test, {
      alpha <- as.numeric(input$alpha)
      df    <- resources_tbl

      switch(input$hypothesis,

        h1 = {
          r3 <- df %>% filter(version == "R3") %>% count(package_text) %>% pull(n)
          r4 <- df %>% filter(version == "R4") %>% count(package_text) %>% pull(n)
          if (length(r3) < 2 || length(r4) < 2)
            return(list(error = "Insufficient data for R3 or R4 packages."))
          tst <- wilcox.test(r4, r3, alternative = "greater")
          viz <- bind_rows(
            tibble(version = "R3", resources = r3),
            tibble(version = "R4", resources = r4)
          )
          list(tst = tst, viz = viz, viz_type = "boxplot",
               x = "version", y = "resources",
               title = "Resources per Package: R3 vs R4",
               subtitle = "Each point is one package",
               stat_label = "W", stat_val = tst$statistic[["W"]], p = tst$p.value,
               alpha = alpha)
        },

        h2 = {
          pkgs_total <- n_distinct(df$package_text[!is.na(df$package_text) & df$package_text != "unknown"])
          pkgs_us    <- df %>%
            filter(tolower(realm) == "us", !is.na(package_text), package_text != "unknown") %>%
            pull(package_text) %>% n_distinct()
          if (pkgs_total < 5) return(list(error = "Insufficient package data."))
          tst <- prop.test(pkgs_us, pkgs_total, p = 0.40, alternative = "greater")
          viz <- df %>%
            filter(!is.na(realm), realm != "", realm != "unknown", realm != "none") %>%
            mutate(realm_grp = ifelse(tolower(realm) == "us", "US", "Non-US")) %>%
            count(realm_grp)
          list(tst = tst, viz = viz, viz_type = "bar",
               x = "realm_grp", y = "n",
               title = "Package Count: US vs Non-US Realms",
               subtitle = paste0("US packages: ", pkgs_us, " of ", pkgs_total, " total"),
               stat_label = "X\u00b2", stat_val = tst$statistic, p = tst$p.value,
               alpha = alpha)
        },

        h3 = {
          date_col <- intersect(c("date","published"), names(df))
          if (length(date_col) > 0)
            df <- df %>% arrange(desc(.data[[date_col[1]]])) %>%
              distinct(identity_text, version, .keep_all = TRUE)
          ct <- df %>%
            filter(!is.na(status), !is.na(version),
                   status %in% c("active","draft","retired")) %>%
            count(version, status) %>%
            tidyr::pivot_wider(names_from = status, values_from = n, values_fill = 0L)
          if (nrow(ct) < 2) return(list(error = "Need at least 2 FHIR versions in data."))
          mat <- as.matrix(ct[, -1])
          rownames(mat) <- ct$version
          tst <- suppressWarnings(chisq.test(mat))
          viz <- df %>%
            filter(!is.na(status), !is.na(version),
                   status %in% c("active","draft","retired")) %>%
            count(version, status)
          list(tst = tst, viz = viz, viz_type = "stacked",
               x = "version", y = "n", fill = "status",
               title = "Status Distribution Across FHIR Versions",
               subtitle = "Each resource counted once at its most recent status",
               stat_label = "X\u00b2", stat_val = tst$statistic, p = tst$p.value,
               alpha = alpha)
        },

        h4 = {
          total  <- nrow(df)
          vs_sd  <- sum(df$resource_type %in% c("ValueSet","StructureDefinition"), na.rm = TRUE)
          if (total < 10) return(list(error = "Insufficient resource data."))
          tst <- prop.test(vs_sd, total, p = 0.70, alternative = "greater")
          viz <- df %>%
            filter(!is.na(resource_type)) %>%
            mutate(grp = ifelse(resource_type %in% c("ValueSet","StructureDefinition"),
                                resource_type, "All Others")) %>%
            count(grp, sort = TRUE)
          list(tst = tst, viz = viz, viz_type = "bar",
               x = "grp", y = "n",
               title = "ValueSet + StructureDefinition vs All Other Types",
               subtitle = paste0("VS+SD: ", scales::comma(vs_sd), " of ", scales::comma(total), " resources"),
               stat_label = "X\u00b2", stat_val = tst$statistic, p = tst$p.value,
               alpha = alpha)
        },

        h5 = {
          us_sizes    <- df %>% filter(tolower(realm) == "us") %>%
            count(package_text) %>% pull(n)
          other_sizes <- df %>% filter(tolower(realm) != "us",
                                       !is.na(realm), realm != "unknown", realm != "") %>%
            count(package_text) %>% pull(n)
          if (length(us_sizes) < 2 || length(other_sizes) < 2)
            return(list(error = "Insufficient data for US or non-US realms."))
          tst <- wilcox.test(us_sizes, other_sizes, alternative = "two.sided")
          viz <- bind_rows(
            tibble(realm_grp = "US",     resources = us_sizes),
            tibble(realm_grp = "Non-US", resources = other_sizes)
          )
          list(tst = tst, viz = viz, viz_type = "boxplot",
               x = "realm_grp", y = "resources",
               title = "Resources per Package: US vs Non-US",
               subtitle = "Each point represents one unique package",
               stat_label = "W", stat_val = tst$statistic[["W"]], p = tst$p.value,
               alpha = alpha)
        },

        h6 = {
          top_types <- df %>% filter(!is.na(resource_type)) %>%
            count(resource_type, sort = TRUE) %>% head(6) %>% pull(resource_type)
          ct <- df %>%
            filter(tolower(realm) %in% c("us","uv"), resource_type %in% top_types) %>%
            mutate(realm_grp = toupper(realm)) %>%
            count(realm_grp, resource_type) %>%
            tidyr::pivot_wider(names_from = resource_type, values_from = n, values_fill = 0L)
          if (nrow(ct) < 2) return(list(error = "Need both US and UV data."))
          mat <- as.matrix(ct[, -1])
          rownames(mat) <- ct$realm_grp
          tst <- suppressWarnings(chisq.test(mat))
          viz <- df %>%
            filter(tolower(realm) %in% c("us","uv"), resource_type %in% top_types) %>%
            mutate(realm_grp = toupper(realm)) %>%
            count(realm_grp, resource_type)
          list(tst = tst, viz = viz, viz_type = "grouped",
               x = "resource_type", y = "n", fill = "realm_grp",
               title = "Resource Type Mix: US vs UV Realm",
               subtitle = "Top 6 resource types compared",
               stat_label = "X\u00b2", stat_val = tst$statistic, p = tst$p.value,
               alpha = alpha)
        }
      )
    }, ignoreNULL = TRUE)

    # Results panel
    output$results_panel <- renderUI({
      req(input$run_test > 0)
      res <- test_result()
      if (!is.null(res$error)) {
        return(wellPanel(
          style = "background:#fff3f3; border:1px solid #d63031;",
          p(icon("exclamation-triangle"), " ", res$error,
            style = "color:#d63031; margin:0; font-weight:bold;")
        ))
      }
      alpha   <- res$alpha
      p_val   <- res$p
      reject  <- p_val < alpha
      h       <- HYPOTHESES[[input$hypothesis]]

      verdict_style <- if (reject)
        "background:#f0fff4; border:2px solid #33d17a;"
      else
        "background:#fff8f3; border:2px solid #e8a838;"

      verdict_icon  <- if (reject) icon("check-circle", style="color:#33d17a;") else icon("times-circle", style="color:#e8a838;")
      verdict_head  <- if (reject) "Reject H\u2080 — Evidence supports H\u2081" else "Fail to reject H\u2080 — Insufficient evidence for H\u2081"

      plain_english <- if (reject) {
        switch(input$hypothesis,
          h1 = "R4 packages are statistically significantly larger than R3 packages. The move to R4 brought not just new resource types but bigger, more comprehensive Implementation Guides.",
          h2 = "The US realm's share of all packages is statistically significantly above 40%. Regulatory mandates have made the US the undisputed dominant force in FHIR package production.",
          h3 = "Status distributions are significantly different across FHIR versions. The FHIR ecosystem is dynamic — resources move through active/draft/retired stages as the standard evolves.",
          h4 = "ValueSets and StructureDefinitions together account for significantly more than 70% of all resources. The ecosystem is overwhelmingly about defining rules, not storing clinical instances.",
          h5 = "There is a statistically significant difference in package size between US and non-US realms. US packages are built differently — likely broader in scope due to regulatory requirements.",
          h6 = "The resource type mix is significantly different between US and UV realms. US and universal guides serve fundamentally different purposes with different technical content."
        )
      } else {
        switch(input$hypothesis,
          h1 = "We cannot confirm R4 packages are larger than R3 at this significance level. The difference may exist but our data does not provide sufficient evidence.",
          h2 = "We cannot confirm the US share exceeds 40% at this significance level. The US may still be the largest contributor, but not by a statistically decisive margin.",
          h3 = "We cannot confirm status distributions differ across versions at this significance level. The data is consistent with the null hypothesis of uniform lifecycle stages.",
          h4 = "We cannot confirm VS+SD exceed 70% at this significance level. They are still dominant, just not at the threshold tested.",
          h5 = "We cannot confirm US packages differ in size from non-US at this significance level. Package sizes may be more uniform than expected.",
          h6 = "We cannot confirm US and UV have different resource type mixes at this significance level. Their content may be more similar than expected."
        )
      }

      wellPanel(
        style = "padding:0; border:none; background:transparent;",
        fluidRow(
          column(4,
            wellPanel(
              style = "background:#f9fafb; border:1px solid #ddd; height:100%;",
              h5("Test Result", style = "color:#0c223f; margin-top:0;"),
              tags$table(style = "width:100%; font-size:14px;",
                tags$tr(tags$td(tags$b("Method:")), tags$td(h$method)),
                tags$tr(tags$td(tags$b(paste0(res$stat_label, ":"))),
                        tags$td(round(res$stat_val, 4))),
                tags$tr(tags$td(tags$b("p-value:")),
                        tags$td(tags$span(
                          style = paste0("font-weight:bold; color:", if (reject) "#33d17a" else "#e8a838"),
                          ifelse(p_val < 0.0001, "< 0.0001", round(p_val, 6))
                        ))),
                tags$tr(tags$td(tags$b("\u03b1 used:")), tags$td(alpha))
              )
            )
          ),
          column(8,
            wellPanel(
              style = paste0(verdict_style, " height:100%;"),
              h5(verdict_icon, " ", verdict_head, style = "margin-top:0; color:#0c223f;"),
              p(plain_english, style = "font-size:14px; line-height:1.6; color:#333; margin-bottom:0;")
            )
          )
        )
      )
    })

    # Visualization
    test_viz <- reactive({
      req(input$run_test > 0)
      res <- test_result()
      if (!is.null(res$error) || is.null(res$viz)) return(ggplot() + theme_void())
      viz  <- res$viz
      type <- res$viz_type
      ttl  <- res$title
      sub  <- res$subtitle

      if (type == "boxplot") {
        ggplot(viz, aes(x = .data[[res$x]], y = .data[[res$y]],
                        fill = .data[[res$x]])) +
          geom_boxplot(outlier.alpha = 0.3, color = "#0c223f", size = 0.5) +
          scale_fill_healthchain() +
          labs(title = ttl, subtitle = sub, x = res$x, y = res$y) +
          hc_theme() + theme(legend.position = "none")

      } else if (type == "bar") {
        ggplot(viz, aes(x = reorder(.data[[res$x]], .data[[res$y]]),
                        y = .data[[res$y]], fill = .data[[res$x]])) +
          geom_col(show.legend = FALSE, color = "white", size = 0.2) +
          geom_text(aes(label = scales::comma(.data[[res$y]])), hjust = -0.1,
                    size = 4, color = "#0c223f", fontface = "bold") +
          coord_flip() +
          expand_limits(y = max(viz[[res$y]], na.rm = TRUE) * 1.2) +
          scale_fill_healthchain() +
          labs(title = ttl, subtitle = sub, x = NULL, y = "Count") +
          hc_theme()

      } else if (type == "stacked") {
        v_order <- c("R2","R3","R4","R4B","R5","R6")
        viz <- viz %>% mutate(version = factor(version, levels = intersect(v_order, unique(version))))
        ggplot(viz, aes(x = .data[[res$x]], y = .data[[res$y]], fill = .data[[res$fill]])) +
          geom_col(position = "fill", color = "white", size = 0.3) +
          scale_y_continuous(labels = scales::percent) +
          scale_fill_healthchain() +
          labs(title = ttl, subtitle = sub, x = res$x, y = "Proportion", fill = res$fill) +
          hc_theme()

      } else if (type == "grouped") {
        ggplot(viz, aes(x = reorder(.data[[res$x]], .data[[res$y]]),
                        y = .data[[res$y]], fill = .data[[res$fill]])) +
          geom_col(position = position_dodge(0.8), color = "white", size = 0.2) +
          coord_flip() +
          scale_fill_healthchain() +
          labs(title = ttl, subtitle = sub, x = NULL, y = "Count", fill = res$fill) +
          hc_theme()
      }
    })

    output$test_plot <- renderPlot({ test_viz() })

    # Download with dynamic caption
    output$dl_test_plot <- downloadHandler(
      filename = function() paste0("hypothesis_", input$hypothesis, "_", Sys.Date(), ".png"),
      content  = function(f) {
        res    <- test_result()
        h      <- HYPOTHESES[[input$hypothesis]]
        reject <- !is.null(res$p) && res$p < as.numeric(input$alpha)
        caption <- paste0(
          h$method, "  |  ",
          res$stat_label, " = ", round(res$stat_val, 4), "  |  ",
          "p = ", ifelse(!is.null(res$p) && res$p < 0.0001, "< 0.0001", round(res$p, 6)), "  |  ",
          "\u03b1 = ", input$alpha, "  |  ",
          ifelse(reject, "REJECT H\u2080", "FAIL TO REJECT H\u2080"), "  |  ",
          "FHIR XIG Registry  |  EDA by Suhas P K  |  ", format(Sys.Date(), "%Y-%m-%d")
        )
        ggplot2::ggsave(f,
          plot  = add_caption_style(test_viz(), caption),
          width = 11, height = 7, dpi = 200, bg = "white")
      }
    )
  })
}
