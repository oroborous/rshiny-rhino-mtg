# app/view/cards_by_set.R

box::use(
  dplyr[filter, group_by, arrange],
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
  app/logic/mtg,
)

#' @export
ui <- function(id, setPicker) {
  ns <- NS(id)

  bootstrapPage(
    div(class="container",
        div(class="row",
            div(class="col-3",
                selectInput(ns("ordering"), "Order columns by",
                            c("the release date of the set" = "releasedate",
                              "the percent the set I own" = "percentowned"))
            ),
            div(class="col-3",
                selectInput(ns("showing"), "Columns height indicates the",
                            c("number of cards" = "numcards",
                              "dollar value of the cards" = "avgretailprice"))
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

    df <- reactiveVal()

    # update everything when a new user's data is loaded
    observeEvent(useremailR(), {
      df(mtg$fetch_cards_by_set(useremailR()))

      # debug output
      output$temp <- renderPrint(paste0(nrow(df()), "/",
                                        length(userSetsR()), "/",
                                        length(selectedSetsR()), "/",
                                        ordering())
                                 )

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
    ordering <- reactive(input$ordering)

    # output$chart <- echarts4r$renderEcharts4r(
    #   # !!rlang::sym() doesn't work for e_bar, alas
    #   if (display() == "numcards")
    #      df() |>
    #         filter(setname %in% selectedSetsR()) |>
    #         group_by(grouptype) |>
    #         arrange(!!rlang::sym(ordering())) |>
    #         echarts4r$e_chart(setcode, reorder=FALSE) |>
    #         echarts4r$e_bar(numcards) |>
    #       # echarts4r$e_x_axis(Year, formatter = JS("App.formatYear")) |>
    #         echarts4r$e_tooltip()
    #    else
    #      df() |>
    #         filter(setname %in% selectedSetsR()) |>
    #         group_by(grouptype) |>
    #         arrange(!!rlang::sym(ordering())) |>
    #         echarts4r$e_chart(setcode, reorder=FALSE) |>
    #         echarts4r$e_bar(avgretailprice) |>
    #         # echarts4r$e_x_axis(Year, formatter = JS("App.formatYear")) |>
    #         echarts4r$e_tooltip()
    # )

    output$chart <- echarts4r$renderEcharts4r(
        df() |>
          filter(setname %in% selectedSetsR()) |>
          group_by(grouptype) |>
          arrange(!!rlang::sym(ordering()), .by_group=TRUE) |>
          echarts4r$e_chart(setcode, reorder=FALSE) |>
          echarts4r$e_bar_(display()) |>
          # echarts4r$e_x_axis(Year, formatter = JS("App.formatYear")) |>
          echarts4r$e_tooltip() |>
          echarts4r$e_mark_point(title="Max!", data = list(name="Max", type = "max"))
    )

    output$table <- renderReactable(
      df() |>
        filter(setname %in% selectedSetsR()) |>
        arrange(releasedate, grouptype) |>
        reactable()
    )

    # listen for button click
    observeEvent(input$go_to_types, {
      change_page("types")
    })
  })

}
