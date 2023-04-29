# app/view/list.R

box::use(
  shiny[actionButton, column, div, fluidRow,
        h2, moduleServer, NS, observeEvent],
  shiny.router[change_page],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      width=6,
      div(
        class="jumbotron",
        h2("The End")
      )
    )
  )
}

#' @export
server <- function (id, data) {
  moduleServer(id, function(input, output, session) {

  })
}
