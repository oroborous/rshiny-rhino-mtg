# app/view/cards_by_set.R

box::use(
  dplyr[filter],
  shiny[actionButton, column, div, bootstrapPage,
        verbatimTextOutput, renderPrint,
        h2, moduleServer, NS, observeEvent, reactive, selectInput],
  shiny.router[change_page],
  shinyWidgets[pickerInput, updatePickerInput],
  reactable[reactableOutput, renderReactable, getReactableState],
  echarts4r[echarts4rOutput, renderEcharts4r],
  shinyBS[bsCollapse, bsCollapsePanel],
)
box::use(
  app/view/set_picker,
  app/logic/mtg
)

#' @export
ui <- function(id, setPicker) {
  ns <- NS(id)

  bootstrapPage(
    div(class="container",
        div(class="row",
            div(class="col-3",
                selectInput(ns("ordering"), "Order By", c("Release Date", "Percent Complete"))
            ),
            div(class="col-3",
                selectInput(ns("showing"), "Show", c("Card Count", "Dollars"))
            ),
            div(class="col",
              setPicker
            ),
            div(class="col",
                verbatimTextOutput(ns("temp"))
            )
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
server <- function (id, data, selectedSets) {

  moduleServer(id, function(input, output, session) {


    df <- reactive(data() |> filter(name %in% selectedSets()))

    observeEvent(selectedSets, ignoreInit = FALSE, {
      output$temp <- renderPrint(selectedSets())
    })

    output$chart <- renderEcharts4r(
      mtg$chart(df())
    )

    output$table <- renderReactable(
      mtg$table(df())
    )

    observeEvent(input$go_to_types, {
      change_page("types")
    })
  })

}
