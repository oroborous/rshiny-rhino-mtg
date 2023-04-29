# app/view/cards_by_set.R

box::use(
  shiny[actionButton, column, div, bootstrapPage,
        h2, moduleServer, NS, observeEvent, reactive, selectInput],
  shiny.router[change_page],
  reactable[reactableOutput, renderReactable, getReactableState],
)
box::use(
  app/logic/mtg
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  bootstrapPage(
    div(class="container",
        div(class="row",
            div(class="col-3",
                selectInput("ordering", "Order By", c("Release Date", "Percent Complete"))
            ),
            div(class="col-3",
                selectInput("showing", "Show", c("Card Count", "Dollars"))
            ),
            div(class="col",
                mtg$set_picker_input()
            )
        ),
        div(class="row",
            div(class="col-10",
                reactableOutput(ns("table"))
            ),
            div(class="col-2",
                actionButton(
                  inputId=ns("go_to_types"),
                  label="Your Cards by Type",
                  class="btn-primary btn-lg"
                )
            )
        ),
        div(class="row",

        )
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
