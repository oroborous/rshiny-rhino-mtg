# app/view/trades.R

box::use(
  dplyr[filter],
  shiny[actionButton, column, div, bootstrapPage, observe,
        h2, moduleServer, NS, observeEvent, reactive, selectInput],
  shiny.router[change_page],
  shinyWidgets[updatePickerInput],
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
                selectInput(ns("hiding"), "Hide cards with less than",
                            c("Show All", "4 copies owned", "8 copies owned"))
            ),
            div(class="col",
                mtg$set_picker_input(ns("set"))
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
server <- function (id, data, selectedSets) {

  moduleServer(id, function(input, output, session) {
    df <- reactive(data() |> filter(name %in% selectedSets()))

    observe({
      updatePickerInput(session=session,
                        inputId="set",
                        selected=selectedSets())
    })

    observeEvent(input$set, ignoreInit = FALSE, {
      selectedSets(input$set)
    })

    output$table <- renderReactable(
      mtg$table(df())
    )

    output$chart <- renderEcharts4r(
      mtg$chart(df())
    )

    observeEvent(input$go_to_list, {
      change_page("list")
    })
  })
}
