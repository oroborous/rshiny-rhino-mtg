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


#' @export
fetch_user_sets <- function(useremail) {
  user_sets_query <- "select setname from v_user_sets where useremail = $1 order by setname"
  user_sets_result <- dbSendQuery(con, user_sets_query)
  dbBind(user_sets_result, list(useremail))

  df <- dbFetch(user_sets_result)

  dbClearResult(user_sets_result)

  df
}

#' @export
fetch_cards_by_set <- function(useremail) {
  cards_by_set_query <- paste0("select b.* from v_cards_by_set b ",
                               "join v_user_sets c on (b.setcode = c.setcode) ",
                               "where b.grouptype in ($1, 'all') ",
                               "and c.useremail = $2")
  cards_by_set_result <- dbSendQuery(con, cards_by_set_query)
  dbBind(cards_by_set_result, list(useremail, useremail))

  df <- dbFetch(cards_by_set_result)

  dbClearResult(cards_by_set_result)

  df
}

#' @export
fetch_cards_by_type <- function(useremail) {
  cards_by_type_query <- paste0("select b.* from v_cards_by_type b ",
                              	"join v_user_sets c on (b.setcode = c.setcode) ",
                              	"where b.grouptype in ($1, 'all') ",
                              	"and c.useremail = $2 ",
                              	"order by setcode, grouptype, cardtype")
  cards_by_type_result <- dbSendQuery(con, cards_by_type_query)
  dbBind(cards_by_type_result, list(useremail, useremail))

  df <- dbFetch(cards_by_type_result)

  dbClearResult(cards_by_type_result)

  df
}


#' @export
set_picker_input <- function(id) {
    pickerInput(inputId=id,
                label="Card Set(s)",
                choices=NULL,
                options=list('actions-box'=TRUE),
                multiple=TRUE)
}
