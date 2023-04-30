# app/view/set_picker.R

box::use(
  shiny[moduleServer, NS, observe, reactive, div, verbatimTextOutput, observeEvent],
  shinyWidgets[pickerInput, updatePickerInput],
)
box::use(
  app/logic/mtg
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  pickerInput(inputId=ns("set"),
            label="Card Set(s)",
            choices=c(mtg$fetch_set_data()$name),
            options=list('actions-box'=TRUE),
            multiple=TRUE)
}

#' @export
server <- function(id) {

  moduleServer(id, function(input, output, session) {
    reactive(input$set)
  })
}
