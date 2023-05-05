# app/view/price_history.R

box::use(
  dplyr[filter, group_by, arrange, summarise, across, mutate],
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
  stats[smooth],
)
box::use(
  app/logic/mtg
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  bootstrapPage(
    div(class="container",
        div(class="col callout callout-default",
            h4("Timing Is Everything"),
            p(paste0("Don't buy when the market is high! Find the sets with retail prices trending downwards, ",
                     "and sell your cards from sets with buylist prices on the rise."))
        ),
        div(class="row",
            div(class="col-3",
                selectInput(ns("pricelist"), "Price list to display",
                            c("Purchase Price (Retail)" = "smoothretail",
                              "Selling Price (Buylist)" = "smoothbuylist"))
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
            div(class="col-4 text-left",
                actionButton(
                  inputId=ns("go_to_types"),
                  label="Back to Cards by Type",
                  class="btn-secondary btn-lg"
                )
            ),
            div(class="col-4 offset-4 text-right",
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
    pricelist <- reactive(input$pricelist)

    observe({

      data <- df() |>
        filter(setname %in% selectedSetsR()) |>
        group_by(grouptype, pricedate) |>
        summarise(across(c(avgretailprice, avgbuylistprice), sum)) |>
       # smooth over some gaps in the price data (some cards missing on some days)
        mutate(smoothretail = as.numeric(smooth(avgretailprice, kind = "3RS3R"))) |>
        mutate(smoothbuylist = as.numeric(smooth(avgbuylistprice, kind = "3RS3R")))

      output$chart <- echarts4r$renderEcharts4r(
        chart <- data |>
          echarts4r$e_chart(pricedate) |>
          echarts4r$e_line_(pricelist(), smooth = TRUE) |>
          echarts4r$e_datazoom(type = "slider", showDetail = FALSE) |>
          # echarts4r$e_x_axis(Year, formatter = JS("App.formatYear")) |>
          echarts4r$e_tooltip() |>
          echarts4r$e_mark_line(data=list(type = "average", name = "Average")) |>
          echarts4r$e_mark_point(title="Max!", serie="all", data = list(name="Max", type = "max")) |>
          echarts4r$e_mark_point(title="Max!", serie=useremailR(), data = list(name="Max", type = "max"))
      )

      output$table <- renderReactable(
        df() |>
          filter(setname %in% selectedSetsR()) |>
          reactable(filterable=TRUE,
                    searchable=TRUE,
                    columns = list(
                      grouptype = colDef(name="Owner"),
                      setname = colDef(name="Set Name"),
                      pricedate = colDef(name="Release Date"),
                      avgretailprice = colDef(name="Avg Retail Price",
                                              format=colFormat(prefix="$",
                                                               separators=TRUE,
                                                               digits=2)),
                      avgbuylistprice = colDef(name="Avg Buylist Price",
                                              format=colFormat(prefix="$",
                                                               separators=TRUE,
                                                               digits=2))
                    ))
      )
    })

    # listen for buttons clicks
    observeEvent(input$go_to_types, {
      change_page("types")
    })

    observeEvent(input$go_to_trades, {
      change_page("trades")
    })
  })
}
