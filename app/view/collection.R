# app/view/collection.R

box::use(
  shiny[actionButton, column, div, bootstrapPage, h4,
        fileInput, textInput, p, textOutput, renderText,
        h2, moduleServer, NS, observeEvent],
  shiny.router[change_page],
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
          div(class="col callout callout-default",
              h4("Complete Your Collection!"),
              p(paste0("Do you want to own a complete set of M:TG cards, but don't want to spend a lot of money? ",
              "With this interactive tool you can create a plan to exchange cards you don't want for the cards you need at the best prices.")),
              p(paste0("Build your dream collection today!"))
          )
      ),
      div(class="row",
          div(class="col card mt-3 p-3 mr-1",
              h2("Upload your collection file"),
              fileInput(ns("csvfile"), "Select the *.csv file that contains your collection data", buttonLabel="Upload"),
              actionButton(
                inputId=ns("user_upload"),
                label="Upload My Collection",
                class="btn-primary btn-lg"
              )
          ),
          div(class="col card mt-3 p-3 ml-1",
            h2("View your saved collection"),
            textInput(ns("useremail"), "Enter your email to view your saved collection", value="stacy@email.com"),
            p("Hint: Just want to explore? Enter 'stacy@email.com' above."),
            actionButton(
              inputId=ns("go_to_sets"),
              label="View My Collection",
              class="btn-primary btn-lg"
            )
        )
      ),
      div(class="row mt-5",
          div(class="col text-center",
            p(textOutput(ns("temp")))
          )
      )
    )
  )
}

#' @export
server <- function(id, useremailR) {

  moduleServer(id, function(input, output, session) {
    observeEvent(input$go_to_sets, {
      change_page("sets")
      useremailR(input$useremail)
    })

    output$temp <- renderText("App is ready!");

  })
}
