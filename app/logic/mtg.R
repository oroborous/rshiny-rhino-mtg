# /app/logic/mtg.R

box::use(
  DBI,
  dplyr,
  reactable[reactable],
  tidyr,
  echarts4r,
  htmlwidgets[JS],
  shinyWidgets[pickerInput],
  shiny[reactiveVal, reactive, observeEvent, observe],
)

con <- DBI::dbConnect(RPostgres::Postgres(),
                      host="localhost",
                      dbname="postgres",
                      user="postgres",
                      password="T1t@nPps",
                      port=5432)

useremailR <- reactiveVal("stacy@email.com")
userSetsR<- reactiveVal()
selectedSetsR <- reactiveVal()

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

observeEvent(useremailR, {
  user_sets_query <- "select setname from v_user_sets where useremail = $1 order by setname"
  user_sets_result <- DBI::dbSendQuery(con, user_sets_query)
  DBI::dbBind(user_sets_result, list(useremailR()))
  df <- DBI::dbFetch(user_sets_result)

  userSetsR(c(df$setname))
  selectedSetsR(c(df$setname))

  DBI::dbClearResult((user_sets_result))
})


#' @export
set_picker_input <- function(id) {
    pickerInput(inputId=id,
                label="Card Set(s)",
                choices=NULL,
                options=list('actions-box'=TRUE),
                multiple=TRUE)
}
