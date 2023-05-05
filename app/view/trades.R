# app/view/trades.R

box::use(
  dplyr[filter, group_by, arrange, summarise],
  shiny[actionButton, column, div, bootstrapPage,
        verbatimTextOutput, renderPrint,
        moduleServer, NS, observeEvent, reactive,
        selectInput, reactiveVal, observe],
  shiny.router[change_page],
  shinyWidgets[updatePickerInput],
  reactable[reactable, reactableOutput, renderReactable,
            colDef, colFormat],
  echarts4r,
  shinyBS[bsCollapse, bsCollapsePanel],
  tibble[add_column, add_row]
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
                            c("Show All" = "1000000",
                              "4 copies owned by me" = "4",
                              "8 copies owned by me" = "8"))
            ),
            div(class="col",
                mtg$set_picker_input(ns("set"))
            )
        ),
        div(class="row", div(class="col", verbatimTextOutput(ns("temp")))),
        div(class="row",
            div(class="col-3",
                echarts4r$echarts4rOutput(ns("chart"))
            ),
            div(class="col-9",
                reactableOutput(ns("table"))
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
server <- function (id, userSetsR, selectedSetsR, useremailR) {

  moduleServer(id, function(input, output, session) {

      dfCards <- reactiveVal()
      dfPrices <- reactiveVal()

      # update everything when a new user's data is loaded
      observeEvent(useremailR(), {
        dfCards(mtg$fetch_user_cards(useremailR()))
        dfPrices(mtg$fetch_completion_prices(useremailR()))

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
      hiding <- reactive(input$hiding)

      output$chart <- echarts4r$renderEcharts4r(
        dfPrices() |>
          filter(setname %in% selectedSetsR()) |>
          summarise(dollars=sum(avgretailprice)) |>
          as.data.frame() |>
          add_column(grouptype = "Funds Needed") |>
          add_row(grouptype="Your Trade Value", dollars=9000) |>
          group_by(grouptype) |>
          echarts4r$e_charts(grouptype, reorder=FALSE) |>
          echarts4r$e_bar(dollars) |>
          # echarts4r$e_x_axis(Year, formatter = JS("App.formatYear")) |>
          echarts4r$e_tooltip()
      )

      output$table <- renderReactable(
        dfCards() |>
          filter(setname %in% selectedSetsR()) |>
          filter(numowned < hiding()) |>
          add_column(numtotrade=0) |>
          add_column(tradevalue=0) |>
          arrange(desc(avgbuylistprice), desc(numowned)) |>
          reactable(filterable=TRUE,
                    searchable=TRUE,
                    columns = list(
                      setname = colDef(name="Set Name"),
                      releasedate = colDef(name="Release Date",
                                           format = colFormat(date=TRUE,
                                                              locales="en-US")),
                      cardname = colDef(name="Card Name"),
                      type = colDef(name="Card Type"),
                      color = colDef(name="Card Color"),
                      avgbuylistprice = colDef(name="Avg Buylist Price",
                                              format=colFormat(prefix="$",
                                                               separators=TRUE,
                                                               digits=2)),
                      numowned = colDef(name="# You Own"),
                      numtotrade = colDef(name="# To Trade"),
                      tradevalue = colDef(name="Trade Value"))
          )
      )

    observeEvent(input$go_to_list, {
      change_page("list")
    })
  })
}
