#! R

##fill in the supplied NVB1 form programmatically
##allowed input is by user input (modal)
##and by upload of CSV

function(input, output, session) {

  ##create reactive to store values from CSV and parse CSV input
  vals <- shiny::reactiveValues(CSV = NULL, OUT = NULL)

  obsev_FILENAMES(input, vals)

  obsev_manual(input, vals)

  obsev_go_fill_pdf(input, vals)

}
