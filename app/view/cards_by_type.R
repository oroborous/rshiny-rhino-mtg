# app/view/cards_by_type.R

box::use(
  dplyr[filter, group_by, arrange, summarise],
  shiny[actionButton, column, div, bootstrapPage,
        verbatimTextOutput, renderPrint, h4, p,
        moduleServer, NS, observeEvent, reactive,
        selectInput, reactiveVal, observe],
  shiny.router[change_page],
  shinyWidgets[updatePickerInput],
  reactable[reactable, reactableOutput, renderReactable,
            colDef, colFormat],
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
        div(class="col callout callout-default",
            h4("Got a Deck In Mind?"),
            p(paste0("Looking for some new cards for your blue/green deck? Or maybe you'd prefer artifacts? ",
                     "Take a look to see which sets will give you more of the cards you want to play with!"))
        ),
        div(class="row",
            div(class="col-3",
                selectInput(ns("breakout"), "Breakout columns by",
                            c("the type of the cards" = "cardtype",
                              "the color of the cards" = "color"))
            ),
            div(class="col-3",
                selectInput(ns("showing"), "Columns height indicates the",
                            c("number of cards" = "sumnumcards",
                              "dollar value of the cards" = "sumavgretailprice"))
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

      # update the options in the set picker to only include
      # sets this user owns
      updatePickerInput(session=session,
                        inputId="set",
                        choices=userSetsR(),
                        selected=selectedSetsR())
    })

    observe({
      # debug output
      # output$temp <- renderPrint(selectedSetsR())

      # update the selected options in this picker when they change on any page
      updatePickerInput(session=session,
                        inputId="set",
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
          group_by(grouptype, !!rlang::sym(breakout())) |>
          summarise(sumnumcards=sum(numcards),
                    sumavgretailprice=sum(avgretailprice),
                    .groups="drop") |>
          as.data.frame() |>
          group_by(grouptype) |>
          arrange(!!rlang::sym(display()), .by_group=TRUE) |>
          echarts4r$e_charts_(breakout(), reorder=FALSE) |>
          echarts4r$e_bar_(display()) |>
          # echarts4r$e_x_axis(Year, formatter = JS("App.formatYear")) |>
          echarts4r$e_flip_coords() |>
          echarts4r$e_tooltip() |>
          echarts4r$e_mark_point(title="Max!", serie="all", data = list(name="Max", type = "max")) |>
          echarts4r$e_mark_point(title="Max!", serie=useremailR(), data = list(name="Max", type = "max"))
    )

    output$table <- renderReactable(
      df() |>
        filter(setname %in% selectedSetsR()) |>
        arrange(releasedate, grouptype) |>
        reactable(filterable=TRUE,
                  searchable=TRUE,
                  columns = list(
                    grouptype = colDef(name="Owner"),
                    setcode = colDef(name="Set Code"),
                    setname = colDef(name="Set Name"),
                    releasedate = colDef(name="Release Date",
                                         format = colFormat(date=TRUE,
                                                            locales="en-US")),
                    cardtype = colDef(name="Card Type"),
                    color = colDef(name="Card Color"),
                    numcards = colDef(name="Number of Cards"),
                    avgretailprice = colDef(name="Avg Retail Price",
                                            format=colFormat(prefix="$",
                                                             separators=TRUE,
                                                             digits=2))
        ))
    )

    # listen for button click
    observeEvent(input$go_to_prices, {
      change_page("prices")
    })
  })
}
