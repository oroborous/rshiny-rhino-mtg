# app/view/cards_by_set.R

box::use(
  shiny[actionButton, column, div, fluidRow,
        h2, moduleServer, NS, observeEvent, reactive],
  shiny.router[change_page],
  reactable[reactableOutput, renderReactable, getReactableState],
)
box::use(
  app/logic/mtg
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
          inputId=ns("go_to_types"),
          label="Your Cards by Type",
          class="btn-primary btn-lg"
        )
      ),
      mtg$set_picker_input()
    ),
    column(
      width=6,
      reactableOutput(ns("table"))
    )
  )
}

#' @export
server <- function (id, data) {
  moduleServer(id, function(input, output, session) {
    output$table <- renderReactable(
      mtg$table(data())
    )

    observeEvent(input$go_to_types, {
      change_page("types")
    })
  })
}
