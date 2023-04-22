box::use(
  shiny[moduleServer, NS, reactive],
  echarts4r[echarts4rOutput, renderEcharts4r],
)
box::use(
  app/logic/rhinos
)

#' @export
ui <- function (id) {
  ns <- NS(id)
  echarts4rOutput(ns("chart"))
}

#' @export
server <- function (id, data) {
  moduleServer(id, function(input, output, session) {
    # don't refetch data, reuse
    output$chart <- renderEcharts4r(
      rhinos$chart(data())
    )
  })
}
