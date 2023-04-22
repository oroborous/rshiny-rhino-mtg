box::use(
  shiny[moduleServer, NS, reactive],
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
server <- function (id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive(rhinos$fetch_data())
    output$table <- renderReactable(
      rhinos$table(data())
    )
  })
}
