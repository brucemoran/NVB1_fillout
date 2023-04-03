shiny::fluidPage(
  shinyjs::useShinyjs(),
  shiny::headerPanel("SI Garda Vetting NVB1 Filling Application"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(width=12,
                        shiny::splitLayout(
                          shiny::fileInput(
                            inputId = "FILENAMES",
                            label = NULL,
                            multiple = FALSE,
                            accept = c(".csv"),
                            buttonLabel = "Import CSV...",
                            placeholder = NULL),
                          shiny::actionButton("manual", "Enter Data Manually")
                        ),
                        shiny::conditionalPanel(
                          condition = "input.go_get_pdf == 1",
                          shiny::downloadButton("downloadData", "Download PDF")
                        )
    ),
    shiny::mainPanel()
  )
)
