box::use(
  shiny[bootstrapPage, moduleServer, NS, reactive, titlePanel, div],
)
box::use(
  app/view[table, chart],
  app/logic/rhinos
)

grid <- function(...) div(class="grid", ...)
card <- function(...) div(class="card", ...)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    titlePanel("Rhino population over time"),
    grid(
      card(chart$ui(ns("chart"))),
      card(table$ui(ns("table")))
    )
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
