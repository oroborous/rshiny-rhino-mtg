box::use(
  shiny[bootstrapPage, moduleServer, NS, reactive],
)
box::use(
  app/view/table,
  app/view/chart,
  app/logic/rhinos
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    chart$ui(ns("chart")),
    table$ui(ns("table"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive(rhinos$fetch_data())
    chart$server("chart", data)
    table$server("table", data)
  })
}
