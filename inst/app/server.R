function(input, output, session) {
  vals <- shiny::reactiveValues(CSV = NULL, OUT = NULL)
  obsev_FILENAMES(input, vals)
  obsev_manual(input, vals)
  obsev_go_fill_pdf(input, vals)
}
