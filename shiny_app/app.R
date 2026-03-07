# ==============================================================================
# app.R - 3-tab navbar + sidebar layout
# ==============================================================================

source("global.R", local = TRUE)

tryCatch({ source("R/utils.R",           local = TRUE); message("utils loaded") },           error = function(e) message("utils error: ",           e$message))
tryCatch({ source("R/mod_overview.R",    local = TRUE); message("overview loaded") },    error = function(e) message("overview error: ",    e$message))
tryCatch({ source("R/mod_evolution.R",   local = TRUE); message("evolution loaded") },   error = function(e) message("evolution error: ",   e$message))
tryCatch({ source("R/mod_data_tables.R", local = TRUE); message("tables loaded") },      error = function(e) message("tables error: ",      e$message))
tryCatch({ source("R/mod_verification.R",local = TRUE); message("verification loaded") },error = function(e) message("verification error: ",e$message))
tryCatch({ source("R/mod_nlp.R",         local = TRUE); message("nlp loaded") },         error = function(e) message("nlp error: ",         e$message))
tryCatch({ source("R/mod_hypothesis.R",  local = TRUE); message("hypothesis loaded") },  error = function(e) message("hypothesis error: ", e$message))

# ------------------------------------------------------------------------------
# Sidebar nav items
# ------------------------------------------------------------------------------
SIDEBAR_ITEMS <- list(
  list(id = "evolution",   label = "Evolution",        icon = "code-branch"),
  list(id = "global",      label = "Global Landscape", icon = "globe"),
  list(id = "hierarchy",   label = "Data Hierarchy",   icon = "sitemap"),
  list(id = "catalog",     label = "Resource Catalog", icon = "list-ul"),
  list(id = "us",          label = "US Deep Dive",     icon = "flag"),
  list(id = "nlp",         label = "Text Analysis",    icon = "brain"),
  list(id = "hypothesis",  label = "Hypothesis Lab",   icon = "flask"),
  list(id = "tables",      label = "Data Tables",      icon = "table"),
  list(id = "verify",      label = "Verification",     icon = "check-circle")
)

# ------------------------------------------------------------------------------
# Footer
# ------------------------------------------------------------------------------
footer_component <- div(
  style = "margin-top:50px; padding:18px 24px; background-color:#0c223f;
           border-top:3px solid #f26d21; text-align:center;",
  p(
    "An EDA project by ", tags$strong(style = "color:#f26d21;", "Suhas P K"),
    " | Built with R Shiny | ",
    tags$span(style = "color:#aaa;", format(Sys.Date(), "%B %d, %Y")),
    style = "font-size:13px; color:#ccc; margin:0;"
  )
)

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------
ui <- navbarPage(
  title    = "FHIR Packages Dashboard",
  theme    = bslib::bs_theme(bootswatch = "cosmo"),
  id       = "main_nav",
  collapsible = TRUE,

  tags$head(
    tags$link(
      rel  = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css"
    ),
    tags$style(HTML("
    /* ---- Navbar ---- */
    .navbar {
      background-color: #ffffff !important;
      box-shadow: 0 2px 6px rgba(0,0,0,0.08);
      padding: 6px 20px;
    }
    .navbar-brand { font-weight: 700; color: #0c223f !important; font-size: 17px; }
    .nav-link     { color: #0c223f !important; font-weight: 500; font-size: 14px; padding: 8px 16px !important; }
    .nav-link:hover  { color: #f26d21 !important; }
    .nav-link.active { color: #f26d21 !important; border-bottom: 3px solid #f26d21; }

    /* ---- Sidebar shell ---- */
    .explore-shell {
      display: flex;
      min-height: calc(100vh - 56px);
      align-items: flex-start;
    }

    /* ---- Sidebar panel ---- */
    .sidebar-panel {
      width: 210px;
      flex-shrink: 0;
      background: #f8f9fa;
      border-right: 2px solid #ebebeb;
      padding: 20px 0;
      min-height: calc(100vh - 56px);
      position: sticky;
      top: 56px;
    }
    .sidebar-heading {
      font-size: 10px;
      font-weight: 700;
      letter-spacing: .12em;
      text-transform: uppercase;
      color: #aaa;
      padding: 0 18px 10px;
    }
    .sidebar-link {
      display: flex;
      align-items: center;
      gap: 10px;
      padding: 10px 18px;
      font-size: 13.5px;
      font-weight: 500;
      color: #0c223f;
      text-decoration: none;
      cursor: pointer;
      border-left: 3px solid transparent;
      transition: background .15s, color .15s, border-color .15s;
      background: none;
      border-top: none;
      border-right: none;
      border-bottom: none;
      width: 100%;
      text-align: left;
    }
    .sidebar-link:hover {
      background: #fff3ec;
      color: #f26d21;
      border-left-color: #f26d21;
    }
    .sidebar-link.active {
      background: #fff3ec;
      color: #f26d21;
      border-left-color: #f26d21;
      font-weight: 600;
    }
    .sidebar-link .fa { width: 16px; text-align: center; }

    /* ---- Main content area ---- */
    .explore-content {
      flex: 1;
      min-width: 0;
      padding: 24px 28px;
      overflow-x: auto;
    }

    /* ---- Mobile: sidebar stacks on top ---- */
    @media (max-width: 768px) {
      .explore-shell   { flex-direction: column; }
      .sidebar-panel   { width: 100%; min-height: unset; position: static;
                         border-right: none; border-bottom: 2px solid #ebebeb;
                         padding: 12px 0; }
      .sidebar-link    { padding: 8px 16px; }
      .explore-content { padding: 16px; }
    }
  "))),

  # ---- Tab 1: Overview ----
  tabPanel("Overview",
    mod_overview_ui("overview"),
    footer_component
  ),

  # ---- Tab 2: Explore ----
  tabPanel("Explore",
    div(class = "explore-shell",

      # Sidebar
      div(class = "sidebar-panel",
        div(class = "sidebar-heading", "Analysis"),
        uiOutput("sidebar_nav")
      ),

      # Content — all UIs stay mounted so updateSelectizeInput works on init
      div(class = "explore-content",
        tabsetPanel(
          id   = "explore_tabs",
          type = "hidden",
          selected = "evolution",
          tabPanel("evolution",  mod_evolution_ui("evolution")),
          tabPanel("global",     mod_global_ui("global")),
          tabPanel("hierarchy",  mod_hierarchy_ui("hierarchy")),
          tabPanel("catalog",    mod_catalog_ui("catalog")),
          tabPanel("us",         mod_us_ui("us")),
          tabPanel("nlp",        mod_nlp_ui("nlp")),
          tabPanel("hypothesis", mod_hypothesis_ui("hyp")),
          tabPanel("tables",     mod_data_tables_ui("tables")),
          tabPanel("verify",     mod_verification_ui("verify"))
        ),
        footer_component
      )
    )
  ),

  # ---- Tab 3: About ----
  tabPanel("About",
    mod_about_ui("about"),
    footer_component
  )
)

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------
server <- function(input, output, session) {

  # Always initialise all module servers
  mod_overview_server("overview")
  mod_evolution_server("evolution")
  mod_data_tables_server("tables")
  mod_global_server("global")
  mod_hierarchy_server("hierarchy")
  mod_catalog_server("catalog")
  mod_us_server("us")
  mod_nlp_server("nlp")
  mod_hypothesis_server("hyp")
  mod_verification_server("verify")
  mod_about_server("about")

  # Active explore section
  active <- reactiveVal("evolution")

  # One observer per sidebar item
  lapply(SIDEBAR_ITEMS, function(item) {
    observeEvent(input[[paste0("sb_", item$id)]], {
      active(item$id)
    }, ignoreInit = TRUE)
  })

  # Render sidebar with active highlight
  output$sidebar_nav <- renderUI({
    cur <- active()
    tagList(lapply(SIDEBAR_ITEMS, function(item) {
      is_active <- cur == item$id
      actionButton(
        inputId = paste0("sb_", item$id),
        label   = tagList(icon(item$icon), " ", item$label),
        class   = paste("sidebar-link", if (is_active) "active" else ""),
        style   = "border-radius:0;"
      )
    }))
  })

  # Switch visible panel when sidebar selection changes
  observeEvent(active(), {
    updateTabsetPanel(session, "explore_tabs", selected = active())
  })
}

shinyApp(ui = ui, server = server)
