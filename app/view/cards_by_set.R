# app/view/cards_by_set.R

box::use(
  dplyr[filter],
  shiny[actionButton, column, div, bootstrapPage,
        verbatimTextOutput, renderPrint, observe,
        h2, moduleServer, NS, observeEvent, reactive, selectInput],
  shiny.router[change_page],
  shinyWidgets[updatePickerInput],
  reactable[reactableOutput, renderReactable, getReactableState],
  echarts4r[echarts4rOutput, renderEcharts4r],
  shinyBS[bsCollapse, bsCollapsePanel],
)
box::use(
  app/logic/mtg,
)

#' @export
ui <- function(id, setPicker) {
  ns <- NS(id)

  bootstrapPage(
    div(class="container",
        div(class="row",
            div(class="col-3",
                selectInput(ns("ordering"), "Order By", c("Release Date" = "releasedate",
                                                          "Percent Complete" = "percentowned"))
            ),
            div(class="col-3",
                selectInput(ns("showing"), "Show", c("Card Count", "Dollars"))
            ),
            div(class="col",
              mtg$set_picker_input(ns("set"))
            )
            ,div(class="col", verbatimTextOutput(ns("temp")))
        ),
        div(class="row",
            div(class="col-12",
                echarts4rOutput(ns("chart"))
            ),
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
                  inputId=ns("go_to_types"),
                  label="Continue to Cards by Type",
                  class="btn-primary btn-lg"
                )
            )
        )
    )
  )
}

#' @export
server <- function (id, userSetsR, selectedSetsR) {

  moduleServer(id, function(input, output, session) {

    #df <- reactive(data() |> filter(setname %in% dfSelectedSetsR()))

    observe({
      updatePickerInput(session=session,
                        inputId="set",
                        choices=userSetsR(),
                        selected=selectedSetsR())
    })

    observeEvent(
      input$set_open,
      {
        if (!isTRUE(input$set_open)) {
          output$temp <- renderPrint(selectedSetsR())
          selectedSetsR(input$set)
        }
      }
    )

    # output$chart <- renderEcharts4r(
    #   data |>
    #     echarts4r$e_chart(code) |>
    #     echarts4r$e_bar(totalsetsize) |>
    #     # echarts4r$e_x_axis(Year, formatter = JS("App.formatYear")) |>
    #     echarts4r$e_tooltip()
    # )
    #
    # output$table <- renderReactable(
    #   data |>
    #     reactable()
    # )

    observeEvent(input$go_to_types, {
      change_page("types")
    })
  })

}
