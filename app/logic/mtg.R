# /app/logic/mtg.R

box::use(
  DBI,
  dplyr,
  reactable[reactable],
  tidyr,
  echarts4r,
  htmlwidgets[JS],
  shinyWidgets[pickerInput],
  shiny[reactiveVal, reactive, observeEvent],
)

con <- DBI::dbConnect(RPostgres::Postgres(),
                      host="localhost",
                      dbname="postgres",
                      user="postgres",
                      password="T1t@nPps",
                      port=5432)

df_sets <- DBI::dbGetQuery(con, "select code, name, releasedate, totalsetsize from mtg_sets order by releasedate, name")

#' @export
fetch_set_data <- function() {
  df_sets
}


useremail <- reactiveVal("stacy@email.com")

#' @export
fetch_useremail <- function() {
  useremail
}

df_user_sets <- reactiveVal()

observeEvent(useremail, {
  user_sets_query <- "select d.name, count(b.uuid) as owned from mtg_collections b join mtg_cards c on (b.uuid = c.uuid) join mtg_sets d on (c.setcode = d.code) where b.useremail = $1 group by d.name"
  user_sets_result <- DBI::dbSendQuery(con, user_sets_query)

  DBI::dbBind(user_sets_result, list(useremail()))
  df_user_sets(DBI::dbFetch(user_sets_result))
})

#' @export
fetch_user_set_data <- function() {
  df_user_sets
}


#' @export
set_picker_input <- function(id) {
  pickerInput(id, "Card Set(s)", c(df_sets$name),
              options=list('actions-box'=TRUE),
              multiple=TRUE)
}

#' @export
table <- function(data) {
  data |>
    reactable()
}

#' @export
chart <- function(data) {
  data |>
    echarts4r$e_chart(code) |>
    echarts4r$e_bar(totalsetsize) |>
   # echarts4r$e_x_axis(Year, formatter = JS("App.formatYear")) |>
    echarts4r$e_tooltip()
}

#' @export
cards_by_set_chart <- function(data) {
  data |>
    echarts4r$e_chart(name) |>
    echarts4r$e_bar(owned) |>
    # echarts4r$e_x_axis(Year, formatter = JS("App.formatYear")) |>
    echarts4r$e_tooltip()
}
