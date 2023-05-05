# app/view/trades.R

box::use(
  dplyr[filter, group_by, arrange, summarise],
  shiny[actionButton, column, div, bootstrapPage,
        verbatimTextOutput, renderPrint, h4, p,
        moduleServer, NS, observeEvent, reactive,
        selectInput, reactiveVal, observe,
        downloadButton, downloadHandler],
  shiny.router[change_page],
  shinyWidgets[updatePickerInput],
  reactable[reactable, reactableOutput, renderReactable,
            colDef, colFormat],
  echarts4r,
  shinyBS[bsCollapse, bsCollapsePanel],
  tibble[add_column, add_row],
  stringr[str_replace_all],
  utils[write.csv]
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
            h4("Get a Fair Deal"),
            p(paste0("Plan your trade in advance using the latest buylist prices. Don't settle for ",
                     "a below-market offer -- know the value of your cards!")),
            p(paste0("First, select the set(s) you want to complete. The cost of the cards you don't own will ",
                     "be displayed as Funds Needed. Adjust the quantities in the table until Your Trade Value matches it.")),
            p("Note: The '# to Trade' column is not currently interactive. In the meantime, you have been granted a $15,000 trade value. Congrats!")
        ),
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
            div(class="col",
                echarts4r$echarts4rOutput(ns("chart"))
            )
        ),
        div(class="row",
            div(class="col",
                reactableOutput(ns("table"))
            )
        ),
        div(class="row",
            div(class="col-4 text-left",
                actionButton(
                  inputId=ns("go_to_prices"),
                  label="Back to Price History",
                  class="btn-secondary btn-lg"
                )
            ),
            div(class="col-4 offset-4 text-right",
                downloadButton(
                  outputId=ns("download"),
                  label="Download this Trade List!",
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
        # output$temp <- renderPrint(str_replace_all(c(useremailR()), "[^a-zA-Z0-9]", "-"))

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
          add_column(grouptype="Funds Needed") |>
          add_row(grouptype="Your Trade Value", dollars=15000) |>
          group_by(grouptype) |>
          echarts4r$e_charts(grouptype, reorder=FALSE, height="100px") |>
          echarts4r$e_bar(dollars) |>
          #echarts4r$e_y_axis(dollars, formatter = JS("App.formatDollars")) |>
          echarts4r$e_tooltip()
      )

      output$table <- renderReactable(
        dfCards() |>
          filter(numowned < hiding()) |>
          add_column(numtotrade=0) |>
          add_column(tradevalue=0) |>
          arrange(desc(avgbuylistprice), desc(numowned)) |>
          reactable(filterable=TRUE,
                    searchable=TRUE,
                    showPageSizeOptions = TRUE,
                    pageSizeOptions = c(5, 10, 20),
                    defaultPageSize = 5,
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
                      numtotrade = colDef(name="# To Trade",
                                          cell=function(value) {paste0("\u2796  ", value, "  \u2795")}),
                      tradevalue = colDef(name="Trade Value"))
          )
      )

      # listen for button clicks
      observeEvent(input$go_to_prices, {
        change_page("prices")
      })

      output$download <- downloadHandler(
        filename = function() {
          emailsafe <- str_replace_all(c(useremailR()), "[^a-zA-Z0-9]", "-")
          paste(emailsafe, ".csv", sep = "")
        },
        content = function(file) {
          write.csv(dfCards(), file, row.names = FALSE)
        }
      )
  })
}
