# app/view/trades.R

box::use(
  shiny[actionButton, column, div, bootstrapPage,
        h2, moduleServer, NS, observeEvent, reactive, selectInput],
  shiny.router[change_page],
  reactable[reactableOutput, renderReactable, getReactableState],
  echarts4r[echarts4rOutput, renderEcharts4r],
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
                selectInput("hiding", "Hide cards with less than",
                            c("Show All", "4 copies owned", "8 copies owned"))
            ),
            div(class="col",
                mtg$set_picker_input()
            )
        ),
        div(class="row",
            div(class="col-6",
                reactableOutput(ns("table"))
            ),
            div(class="col-6",
                echarts4rOutput(ns("chart"))
            )
        ),
        div(class="row",
            div(class="col-4 offset-8 text-right",
                actionButton(
                  inputId=ns("go_to_list"),
                  label="Print this Trade List!",
                  class="btn-primary btn-lg"
                )
            )
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

    output$chart <- renderEcharts4r(
      mtg$chart(data())
    )

    observeEvent(input$go_to_list, {
      change_page("list")
    })
  })
}
