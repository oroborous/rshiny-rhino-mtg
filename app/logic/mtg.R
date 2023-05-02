# /app/logic/mtg.R

box::use(
  DBI[dbConnect, dbSendQuery, dbBind, dbFetch, dbClearResult],
  htmlwidgets[JS],
  shinyWidgets[pickerInput],
  shiny[reactiveVal, reactive, observeEvent],
)

con <- dbConnect(RPostgres::Postgres(),
                      host="localhost",
                      dbname="postgres",
                      user="postgres",
                      password="T1t@nPps",
                      port=5432)

useremailR <- reactiveVal("stacy@email.com")
userSetsR<- reactiveVal()
selectedSetsR <- reactiveVal()
cardsBySetR <- reactiveVal()

#' @export
fetch_useremail <- function() {
  useremailR
}

#' @export
fetch_selected_sets <- function() {
  selectedSetsR
}

#' @export
fetch_user_sets <- function() {
  userSetsR
}

#' @export
fetch_cards_by_set <- function() {
  cardsBySetR
}

observeEvent(useremailR, {
  user_sets_query <- "select setname from v_user_sets where useremail = $1 order by setname"
  user_sets_result <- dbSendQuery(con, user_sets_query)
  dbBind(user_sets_result, list(useremailR()))
  df <- dbFetch(user_sets_result)

  userSetsR(c(df$setname))
  selectedSetsR(c(df$setname))

  dbClearResult(user_sets_result)

  cards_by_set_query <- paste0("select b.* from v_cards_by_set b ",
                              "join v_user_sets c on (b.setcode = c.setcode) ",
                              "where b.grouptype in ($1, 'all') ",
                              "and c.useremail = $2")
  cards_by_set_result <- dbSendQuery(con, cards_by_set_query)
  dbBind(cards_by_set_result, list(useremailR(), useremailR()))
  df <- dbFetch(cards_by_set_result)

  cardsBySetR(df)

  dbClearResult(cards_by_set_result)
})


#' @export
set_picker_input <- function(id) {
    pickerInput(inputId=id,
                label="Card Set(s)",
                choices=NULL,
                options=list('actions-box'=TRUE),
                multiple=TRUE)
}
