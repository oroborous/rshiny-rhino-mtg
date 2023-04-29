box::use(
  shiny[bootstrapPage, moduleServer, NS, reactive,
        titlePanel, div, tags, a],
  shiny.router[router_ui, router_server,
               route, route_link]
)
box::use(
  app/view[collection, cards_by_set, cards_by_type, price_history, trades, list, page_404],
  app/logic/rhinos
)

grid <- function(...) div(class="grid", ...)
card <- function(...) div(class="card", ...)

#' @export
ui <- function(id) {
  ns <- NS(id)

  bootstrapPage(
    tags$h1("Magic: The Gathering Set Collector"),
    tags$nav(
      class="navbar",
      tags$ul(
        class="nav navbar-nav",
        tags$li(
          a("Home", href=route_link("/"))
        ),
        tags$li(
          a("Your Sets", href=route_link("sets"))
        ),
        tags$li(
          a("Card Types", href=route_link("types"))
        ),
        tags$li(
          a("Price History", href=route_link("prices"))
        ),
        tags$li(
          a("Your Trades", href=route_link("trades"))
        ),
        tags$li(
          a("Trade List", href=route_link("list"))
        )
      )
    ),
    router_ui(
      route("/", collection$ui(ns("collection"))),
      route("sets", cards_by_set$ui(ns("sets"))),
      route("types", cards_by_type$ui(ns("types"))),
      route("prices", price_history$ui(ns("prices"))),
      route("trades", trades$ui(ns("trades"))),
      route("list", list$ui(ns("list"))),
      page_404 = page_404$ui(ns("page_404"))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    router_server("/")

    data <- reactive(rhinos$fetch_data())

    collection$server("collection")
    cards_by_set$server("sets", data)
    cards_by_type$server("types", data)
    price_history$server("prices", data)
    list$server("list", data)
    trades$server("trades", data)
  })
}
