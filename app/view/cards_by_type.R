# app/view/cards_by_type.R

box::use(
  dplyr[filter],
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
                selectInput(ns("ordering"), "Order By", c("Cards in Set", "Percent Complete"))
            ),
            div(class="col-3",
                selectInput(ns("showing"), "Show", c("Card Count", "Dollars"))
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
                  inputId=ns("go_to_prices"),
                  label="Continue to Price History",
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
    df <- reactive(data() |> filter(name %in% input$set))

    output$chart <- renderEcharts4r(
      mtg$chart(df())
    )

    output$table <- renderReactable(
      mtg$table(data())
    )

    observeEvent(input$go_to_prices, {
      change_page("prices")
    })
  })
}
