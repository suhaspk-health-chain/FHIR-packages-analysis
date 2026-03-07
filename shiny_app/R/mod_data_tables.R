# ==============================================================
# mod_data_tables.R - Data Tables Module
# ==============================================================

mod_data_tables_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h2(icon("table"), " Raw Data Explorer", style = "color: #0c223f; margin-bottom: 10px;"),
    wellPanel(
      style="background:#f9fafb; border-left:4px solid #666; padding:12px 18px; margin-bottom:14px;",
      p(style="margin:0; font-size:14px; line-height:1.6; color:#444;",
        strong("Browse the raw data: "),
        tags$b("Resources"), " — Every resource definition scraped from the FHIR package registry (75K rows). ",
        tags$b("Presence Matrix"), " — Which official resource types exist in which FHIR version (R2–R6). ",
        tags$b("Stable Resources"), " — Types present in 4 or more versions, indicating long-term stability. ",
        tags$b("Raw Preview"), " — First 100 rows of the original unprocessed data."
      )
    ),
    tabsetPanel(
      tabPanel("Resources", DTOutput(ns("tbl_resources"))),
      tabPanel("Presence Matrix", DTOutput(ns("tbl_matrix"))),
      tabPanel("Stable Resources", DTOutput(ns("tbl_stable"))),
      tabPanel("Raw Preview", DTOutput(ns("tbl_raw")))
    )
  )
}

mod_data_tables_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$tbl_resources <- renderDT({
      datatable(resources_tbl, class = "stripe hover compact",
                options = list(pageLength = 20, scrollX = TRUE, scrollY = "600px",
                               dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')),
                extensions = 'Buttons', rownames = FALSE, filter = 'top')
    })
    
    output$tbl_matrix <- renderDT({
      if (nrow(matrix_tbl) == 0) {
        datatable(data.frame(Message = "No matrix available"), options = list(dom = 't'))
      } else {
        datatable(matrix_tbl, class = "stripe hover compact",
                  options = list(pageLength = 20, scrollX = TRUE, scrollY = "600px",
                                 dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')),
                  extensions = 'Buttons', rownames = FALSE)
      }
    })
    
    output$tbl_stable <- renderDT({
      if (nrow(stable_tbl) == 0) {
        datatable(data.frame(Message = "No stable list"), options = list(dom = 't'))
      } else {
        datatable(stable_tbl, class = "stripe hover compact",
                  options = list(pageLength = 20, scrollX = TRUE, scrollY = "600px",
                                 dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')),
                  extensions = 'Buttons', rownames = FALSE)
      }
    })
    
    output$tbl_raw <- renderDT({
      datatable(raw_preview, class = "stripe hover compact",
                options = list(pageLength = 10, scrollX = TRUE,
                               dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')),
                extensions = 'Buttons', rownames = FALSE)
    })
  })
}
