box::use(
  shiny[moduleServer, NS],
  reactable[reactableOutput, renderReactable],
)
box::use(
  app/logic/rhinos
)

#' @export
ui <- function (id) {
  ns <- NS(id)
  reactableOutput(ns("table"))
}

#' @export
server <- function (id, data) {
  moduleServer(id, function(input, output, session) {
    # refactored to receive data reactive as argument
    # data <- reactive(rhinos$fetch_data())
    output$table <- renderReactable(
      rhinos$table(data())
    )
  })
}
