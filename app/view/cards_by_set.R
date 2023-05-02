# app/view/cards_by_set.R

box::use(
  dplyr[filter, group_by, arrange, select],
  shiny[actionButton, column, div, bootstrapPage,
        verbatimTextOutput, renderPrint, observe,
        h2, moduleServer, NS, observeEvent, reactive, selectInput],
  shiny.router[change_page],
  shinyWidgets[updatePickerInput],
  reactable[reactable, reactableOutput, renderReactable, getReactableState],
  echarts4r,
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
                selectInput(ns("ordering"), "Order By", c("Set Release Date" = "releasedate",
                                                          "Percent of Set Complete" = "percentowned"))
            ),
            div(class="col-3",
                selectInput(ns("showing"), "Show", c("Number of Cards" = "numcards",
                                                     "Dollar Value of Cards" = "avgretailprice"))
            ),
            div(class="col",
              mtg$set_picker_input(ns("set"))
            )

        ),
        div(class="row", div(class="col", verbatimTextOutput(ns("temp")))),
        div(class="row",
            div(class="col-12",
                echarts4r$echarts4rOutput(ns("chart"))
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
server <- function (id, userSetsR, selectedSetsR, useremailR) {

  moduleServer(id, function(input, output, session) {

    df <- reactive(mtg$fetch_cards_by_set(useremailR()) |>
                     filter(setname %in% selectedSetsR()) |>
                     group_by(grouptype) |>
                     arrange(input$ordering))

    # update set choices when a new user's data is loaded
    observeEvent(useremailR(), {
      output$temp <- renderPrint(length(userSetsR()))
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

    display <- reactive(input$showing)

    output$chart <- echarts4r$renderEcharts4r(
      if (display() == "numcards")
         df() |>
           echarts4r$e_chart(setcode) |>
           echarts4r$e_bar(numcards) |>
          # echarts4r$e_x_axis(Year, formatter = JS("App.formatYear")) |>
           echarts4r$e_tooltip()
       else
         df() |>
          echarts4r$e_chart(setcode) |>
          echarts4r$e_bar(avgretailprice) |>
          # echarts4r$e_x_axis(Year, formatter = JS("App.formatYear")) |>
          echarts4r$e_tooltip()
    )

    output$table <- renderReactable(
      df() |>
        reactable()
    )

    observeEvent(input$go_to_types, {
      change_page("types")
    })
  })

}
