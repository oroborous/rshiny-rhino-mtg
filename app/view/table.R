box::use(
  shiny[h3, moduleServer, NS, observeEvent, reactive, req, tagList],
  shiny.router[get_query_param, change_page],
  reactable[reactableOutput, renderReactable, getReactableState],
)
box::use(
  app/logic/rhinos
)

#' @export
ui <- function (id) {
  ns <- NS(id)
  tagList(
    h3("Table"),
    reactableOutput(ns("table"))
  )
}

#' @export
server <- function (id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    page_size <- reactive({
      page_size <- get_query_param("pageSize")

      if (is.null(page_size)) {
        page_size <- 10
      }

      as.numeric(page_size)
    })
    # refactored to receive data reactive as argument
    # data <- reactive(rhinos$fetch_data())
    output$table <- renderReactable(
      rhinos$table(data(), page_size())
    )

    observeEvent(
      getReactableState("table", "pageSize"), {
        table_page_size <- getReactableState("table", "pageSize")
        if (table_page_size != page_size()) {
          change_page(paste0("table?pageSize=", table_page_size))
        }
      })
  })
}
