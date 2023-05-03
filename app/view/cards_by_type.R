# app/view/cards_by_type.R

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
  app/logic/mtg,
)

#' @export
ui <- function(id, setPicker) {
  ns <- NS(id)

  bootstrapPage(
    div(class="container",
        div(class="row",
            div(class="col-3",
                selectInput(ns("breakout"), "Breakout columns by",
                            c("the type of the cards" = "cardtype",
                              "the color of the cards" = "color"))
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
server <- function (id, userSetsR, selectedSetsR, useremailR) {

  moduleServer(id, function(input, output, session) {

    df <- reactiveVal()

    # update everything when a new user's data is loaded
    observeEvent(useremailR(), {
      df(mtg$fetch_cards_by_type(useremailR()))

      # debug output
      output$temp <- renderPrint(paste0(nrow(df()), "/",
                                        length(userSetsR()), "/",
                                        length(selectedSetsR()), "/",
                                        breakout())
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
    breakout <- reactive(input$breakout)

    # output$chart <- echarts4r$renderEcharts4r(
    #
    #   # !!rlang::sym() doesn't work for e_bar, alas
    #   if (display() == "numcards")
    #     df() |>
    #       #select(-c(!!rlang::sym(breakout()))) |>
    #       filter(setname %in% selectedSetsR()) |>
    #       group_by(grouptype, !!rlang::sym(breakout())) |>
    #       summarise(sumcols=sum(numcards), .groups="drop") |>
    #       echarts4r$e_chart(breakout()), reorder=FALSE) |>
    #       echarts4r$e_bar(sumcol) |>
    #       # echarts4r$e_x_axis(Year, formatter = JS("App.formatYear")) |>
    #       echarts4r$e_flip_coords() |>
    #       echarts4r$e_tooltip()
    #   else
    #     df() |>
    #       #select(-c(!!rlang::sym(breakout()))) |>
    #       filter(setname %in% selectedSetsR()) |>
    #       group_by(grouptype, !!rlang::sym(breakout())) |>
    #       summarise(sumavgretailprice=sum(sumavgretailprice), .groups="drop") |>
    #       echarts4r$e_chart_(!!rlang::sym(breakout()), reorder=FALSE) |>
    #       echarts4r$e_bar(sumavgretailprice) |>
    #       # echarts4r$e_x_axis(Year, formatter = JS("App.formatYear")) |>
    #       echarts4r$e_flip_coords() |>
    #       echarts4r$e_tooltip()
    # )

    output$table <- renderReactable(
      df() |>
        filter(setname %in% selectedSetsR()) |>
        arrange(releasedate, grouptype) |>
        reactable()
    )

    observeEvent(input$go_to_prices, {
      change_page("prices")
    })
  })
}
