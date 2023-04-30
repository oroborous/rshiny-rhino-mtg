# /app/logic/mtg.R

box::use(
  DBI,
  dplyr,
  reactable[reactable],
  tidyr,
  echarts4r,
  htmlwidgets[JS],
  shinyWidgets[pickerInput],
  shiny[reactiveVal, reactive],
)

con <- DBI::dbConnect(RPostgres::Postgres(),
                      host="localhost",
                      dbname="postgres",
                      user="postgres",
                      password="T1t@nPps",
                      port=5432)

df_sets <- DBI::dbGetQuery(con, "select code, name, releasedate, totalsetsize from mtg_sets order by releasedate, name")

useremail <- reactiveVal("stacy@email.com")

#' @export
fetch_useremail <- function() {
  useremail
}

#' @export
fetch_set_data <- function() {
  df_sets
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
