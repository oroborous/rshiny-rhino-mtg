box::use(shiny[bootstrapPage, moduleServer, NS, observe,
               titlePanel, div, tags, a, reactiveVal],
         shiny.router[router_ui, router_server,
                      route, route_link],
         bslib[bs_theme])
box::use(app/view[collection, cards_by_set, cards_by_type, price_history, trades, list, page_404],
         app/logic/mtg)

#' @export
ui <- function(id) {
  ns <- NS(id)

  bootstrapPage(
    theme = bs_theme(version = 4),
    div(class = "container mt-5",
    div(class = "row",
      div(class = "col",
        tags$h1(class="logo", "Magic: The Gathering Set Collector"),
        tags$nav(class = "navbar",
          tags$ul(class = "nav navbar-nav",
            tags$li(a("Home", href = route_link("/"))),
            tags$li(a("Your Sets", href = route_link("sets"))),
            tags$li(a("Card Types", href = route_link("types"))),
            tags$li(a("Price History", href = route_link("prices"))),
            tags$li(a("Your Trades", href = route_link("trades"))),
            tags$li(a("Trade List", href = route_link("list")))
          )
        )
      )
    ),
    div(class = "row",
        div(class = "col",
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
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    router_server("/")

    useremailR <- reactiveVal("stacy@email.com")
    userSetsR <- reactiveVal()
    selectedSetsR <- reactiveVal()

    # load initial set data for sample user 'stacy@email.com'
    observe({
      userSetsR(c(mtg$fetch_user_sets(useremailR())$setname))
      selectedSetsR(userSetsR())
    })

    collection$server("collection", useremailR)
    cards_by_set$server("sets", userSetsR, selectedSetsR, useremailR)
    cards_by_type$server("types", userSetsR, selectedSetsR, useremailR)
    price_history$server("prices", userSetsR, selectedSetsR, useremailR)
    trades$server("trades", userSetsR, selectedSetsR, useremailR)
    list$server("list")
  })
}
