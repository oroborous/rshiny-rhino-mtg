box::use(
  shiny[bootstrapPage, moduleServer, NS, reactive,
        titlePanel, div, tags, a],
  shiny.router[router_ui, router_server,
               route, route_link]
)
box::use(
  app/view[table, chart, intro, page_404],
  app/logic/rhinos
)

grid <- function(...) div(class="grid", ...)
card <- function(...) div(class="card", ...)

#' @export
ui <- function(id) {
  ns <- NS(id)

  bootstrapPage(
    tags$nav(
      class="navbar",
      tags$ul(
        class="nav navbar-nav",
        tags$li(
          a("Home", href=route_link("/"))
        ),
        tags$li(
          a("Table", href=route_link("table"))
        ),
        tags$li(
          a("Chart", href=route_link("chart"))
        )
      )
    ),
    router_ui(
      route("/", intro$ui(ns("intro"))),
      route("table", table$ui(ns("table"))),
      route("chart", chart$ui(ns("chart"))),
      page_404 = page_404$ui(ns("page_404"))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    router_server("/")

    data <- reactive(rhinos$fetch_data())

    intro$server("intro")
    chart$server("chart", data)
    table$server("table", data)
  })
}
