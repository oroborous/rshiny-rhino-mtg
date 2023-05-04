# app/view/collection.R

box::use(
  shiny[actionButton, column, div, bootstrapPage,
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
    div(class="row",
        div(class="col card mt-3 p-3",
            h2("Upload Your Collection"),
            fileInput(ns("csvfile"), "Select the *.csv file that contains your collection data", buttonLabel="Upload")
        ),
      div(class="col card mt-3 p-3",
          h2("View your saved collection"),
          textInput(ns("useremail"), "Enter your email to view your saved collection", value="stacy@email.com"),
          actionButton(
            inputId=ns("go_to_sets"),
            label="View My Collection",
            class="btn-primary btn-lg"
          ),
          p("Just want to explore? Enter 'stacy@email.com' above.")
      )
    ),
    div(class="row mt-5",
        div(class="col text-center",
          p(textOutput(ns("temp")))
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
