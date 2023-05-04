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
fetch_price_history <- function(useremail) {
  price_history_query <- paste0("select grouptype, b.setname, pricedate, ",
                                "sum(avgretailprice) as avgretailprice, ",
                              	"sum(avgbuylistprice) as avgbuylistprice from mv_price_history b ",
                              	"join v_user_sets c on (b.setcode = c.setcode) ",
                              	"where b.grouptype in ($1, 'all') ",
                              	"and c.useremail = $2 ",
                              	"group by grouptype, b.setname, pricedate ",
                              	"order by grouptype, pricedate")

  price_history_result <- dbSendQuery(con, price_history_query)
  dbBind(price_history_result, list(useremail, useremail))

  df <- dbFetch(price_history_result)

  dbClearResult(price_history_result)

  df
}

#' @export
fetch_user_cards <- function(useremail) {
  user_cards_query <- paste0("select setname, releasedate, cardname, type, color, ",
                             "avgbuylistprice, numowned ",
                             "from v_user_cards ",
                             "where useremail = $1")

  user_cards_result <- dbSendQuery(con, user_cards_query)
  dbBind(user_cards_result, list(useremail))

  df <- dbFetch(user_cards_result)

  dbClearResult(user_cards_result)

  df
}

#' @export
fetch_completion_prices <- function(useremail) {
  completion_prices_query <- paste0("select setname, sum(avgretailprice) as avgretailprice from ( ",
                            	"select setname, uuid, avgretailprice from v_set_cards ",
                            	"except ",
                            	"select setname, uuid, avgretailprice from v_user_cards ",
                            	"where useremail = $1 ",
                              ") as missing_cards ",
                              "group by setname")
  completion_prices_result <- dbSendQuery(con, completion_prices_query)
  dbBind(completion_prices_result, list(useremail))

  df <- dbFetch(completion_prices_result)

  dbClearResult(completion_prices_result)

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
