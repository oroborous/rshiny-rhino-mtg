# app/view/trades.R

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
        actionButton(
          inputId=ns("go_to_list"),
          label="Print Trade List",
          class="btn-primary btn-lg"
        )
      )
    )
  )
}

#' @export
server <- function (id, data) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$go_to_list, {
      change_page("list")
    })
  })
}
