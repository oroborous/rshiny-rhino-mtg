# app/view/price_history.R

box::use(
  shiny[actionButton, column, div, bootstrapPage,
        h2, moduleServer, NS, observeEvent, reactive, selectInput],
  shiny.router[change_page],
  reactable[reactableOutput, renderReactable, getReactableState],
  echarts4r[echarts4rOutput, renderEcharts4r],
  shinyBS[bsCollapse, bsCollapsePanel],
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
                selectInput(ns("breakout"), "Breakout By", c("Card Type", "Card Color"))
            ),
            div(class="col-3",
                selectInput(ns("showing_set"), "Price type for all set cards", c("Purchase Price", "Selling Price"))
            ),
            div(class="col-3",
                selectInput(ns("showing_owned"), "Price type for your set cards", c("Purchase Price", "Selling Price"))
            ),
            div(class="col",
                mtg$set_picker_input(ns("set"))
            )
        ),
        div(class="row",
            div(class="col-12",
                echarts4rOutput(ns("chart"))

            )
        ),
        div(class="row",
            div(class="col-12",
                bsCollapse(id="details",
                           bsCollapsePanel("More Details",
                                           reactableOutput(ns("table")),
                                           style="info")
                )
            )
        ),
        div(class="row",
            div(class="col-4 offset-8 text-right",
                actionButton(
                  inputId=ns("go_to_trades"),
                  label="Continue to Trade Builder",
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

    observeEvent(input$go_to_trades, {
      change_page("trades")
    })
  })
}
