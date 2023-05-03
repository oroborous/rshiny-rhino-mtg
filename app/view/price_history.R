# app/view/price_history.R

box::use(
  dplyr[filter, group_by, arrange, summarise],
  shiny[actionButton, column, div, bootstrapPage,
        verbatimTextOutput, renderPrint,
        moduleServer, NS, observeEvent, reactive,
        selectInput, reactiveVal],
  shiny.router[change_page],
  shinyWidgets[updatePickerInput],
  reactable[reactable, reactableOutput, renderReactable, getReactableState],
  echarts4r,
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
                selectInput(ns("priceset"), "Price list for all set cards",
                            c("Purchase Price (Retail)" = "avgretailprice",
                              "Selling Price (Buylist)" = "avgbuylistprice"))
            ),
            div(class="col-3",
                selectInput(ns("priceowned"), "Price list for your cards",
                            c("Purchase Price (Retail)" = "avgretailprice",
                              "Selling Price (Buylist)" = "avgbuylistprice"))
            ),
            div(class="col",
                mtg$set_picker_input(ns("set"))
            )
        ),
        div(class="row", div(class="col", verbatimTextOutput(ns("temp")))),
        div(class="row",
            div(class="col-12",
                echarts4r$echarts4rOutput(ns("chart"))
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
server <- function (id, userSetsR, selectedSetsR, useremailR) {

  moduleServer(id, function(input, output, session) {

    df <- reactiveVal()

    # update everything when a new user's data is loaded
    observeEvent(useremailR(), {
      df(mtg$fetch_price_history(useremailR()))

      # debug output
      # output$temp <- renderPrint(paste0(nrow(df()), "/",
      #                                   length(userSetsR()), "/",
      #                                   length(selectedSetsR()), "/",
      #                                   breakout()))

      # update the options in the set picker to only include
      # sets this user owns
      updatePickerInput(session=session,
                        inputId="set",
                        choices=userSetsR(),
                        selected=selectedSetsR())
    })

    # only update set selections when the picker input window closes
    observeEvent(
      input$set_open,
      {
        if (!isTRUE(input$set_open)) {
          selectedSetsR(input$set)
        }
      }
    )

    # reactives for the dropdown box values
    display <- reactive(input$showing)
    breakout <- reactive(input$breakout)

    output$chart <- echarts4r$renderEcharts4r(
      df() |>
        filter(setname %in% selectedSetsR()) |>
        group_by(grouptype) |>
        echarts4r$e_chart(pricedate) |>
        echarts4r$e_line(avgretailprice) |>
        # echarts4r$e_x_axis(Year, formatter = JS("App.formatYear")) |>
        echarts4r$e_tooltip() |>
        echarts4r$e_mark_point(title="Max!", serie="all", data = list(name="Max", type = "max"))
    )

    output$table <- renderReactable(
      df() |>
        filter(setname %in% selectedSetsR()) |>
        reactable()
    )

    observeEvent(input$go_to_trades, {
      change_page("trades")
    })
  })
}
