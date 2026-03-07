# ==============================================================================
# app.R - COMPLETE FIXED
# ==============================================================================

# Source global first
source("global.R", local = TRUE)

# Load utils
tryCatch({
  source("R/utils.R", local = TRUE)
  message("✅ Utils loaded")
}, error = function(e) {
  message("⚠️ Error loading utils: ", e$message)
})

# Load modules
tryCatch({
  source("R/mod_overview.R", local = TRUE)
  message("✅ Overview module loaded")
}, error = function(e) {
  message("⚠️ Error loading overview module: ", e$message)
})

tryCatch({
  source("R/mod_evolution.R", local = TRUE)
  message("✅ Evolution module loaded")
}, error = function(e) {
  message("⚠️ Error loading evolution module: ", e$message)
})

tryCatch({
  source("R/mod_data_tables.R", local = TRUE)
  message("✅ Data tables module loaded")
}, error = function(e) {
  message("⚠️ Error loading data tables module: ", e$message)
})

tryCatch({
  source("R/mod_verification.R", local = TRUE)
  message("✅ Verification module loaded")
}, error = function(e) {
  message("⚠️ Error loading verification module: ", e$message)
})

tryCatch({
  source("R/mod_nlp.R", local = TRUE)
  message("✅ NLP module loaded")
}, error = function(e) {
  message("⚠️ Error loading NLP module: ", e$message)
})

tryCatch({
  source("R/mod_hypothesis.R", local = TRUE)
  message("✅ Hypothesis module loaded")
}, error = function(e) {
  message("⚠️ Error loading hypothesis module: ", e$message)
})

# Footer component
footer_component <- div(
  style = "margin-top: 50px; padding: 18px 24px; background-color: #0c223f; border-top: 3px solid #f26d21; text-align: center;",
  p(
    "An EDA project by ", tags$strong(style="color:#f26d21;", "Suhas P K"),
    " | Built with R Shiny | ",
    tags$span(style="color:#aaa;", format(Sys.Date(), "%B %d, %Y")),
    style="font-size:13px; color:#ccc; margin:0;"
  )
)

# UI
ui <- navbarPage(
  title = "FHIR Packages Dashboard",
  theme = bslib::bs_theme(bootswatch = "cosmo"),
  tags$head(tags$style(HTML("
    .navbar { flex-wrap: wrap; }
    .navbar-brand { font-weight: bold; color: #0c223f !important; }
    .nav-link { color: #0c223f !important; font-weight: 500; }
    .nav-link:hover { color: #f26d21 !important; }
    .nav-link.active { color: #f26d21 !important; border-bottom: 3px solid #f26d21; }
  "))),
  
  tabPanel("Overview", mod_overview_ui("overview"), footer_component),
  tabPanel("Evolution", mod_evolution_ui("evolution"), footer_component),
  tabPanel("Data Tables", mod_data_tables_ui("tables"), footer_component),
  tabPanel("Global Landscape", mod_global_ui("global"), footer_component),
  tabPanel("Data Hierarchy", mod_hierarchy_ui("hierarchy"), footer_component),
  tabPanel("Resource Catalog", mod_catalog_ui("catalog"), footer_component),
  tabPanel("US Deep Dive", mod_us_ui("us"), footer_component),
  tabPanel("Text Analysis", mod_nlp_ui("nlp"), footer_component),
  tabPanel("Hypothesis Lab", mod_hypothesis_ui("hyp"), footer_component),
  tabPanel("Verification", mod_verification_ui("verify"), footer_component),
  tabPanel("About", mod_about_ui("about"), footer_component)
)

# Server
server <- function(input, output, session) {
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
}

shinyApp(ui = ui, server = server)
