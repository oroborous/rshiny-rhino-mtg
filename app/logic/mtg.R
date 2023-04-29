# /app/logic/mtg.R

box::use(
  DBI,
  dplyr,
  reactable[reactable],
  tidyr,
  echarts4r,
  htmlwidgets[JS],
)

con <- DBI::dbConnect(RPostgres::Postgres(),
                      host="localhost",
                      dbname="postgres",
                      user="postgres",
                      password="T1t@nPps",
                      port=5432)

df_sets <- DBI::dbGetQuery(con, "select code, name, releasedate from mtg_sets order by releasedate, name")

#' @export
fetch_set_data <- function() {
  df_sets
}

#' @export
table <- function(data) {
  data |>
    reactable()
}
