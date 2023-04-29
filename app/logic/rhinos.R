box::use(
  dplyr,
  reactable[reactable],
  rhino,
  tidyr,
  echarts4r,
  htmlwidgets[JS],
)

# connect to DB?

#' @export
fetch_data <- function() {
  rhino::rhinos
}

#' @export
table <- function(data, page_size) {
  data |>
    tidyr$pivot_wider(names_from = Species,
                      values_from = Population) |>
    dplyr$arrange(Year) |>
    reactable(
      defaultPageSize = page_size,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(5, 10, 15, 20, page_size) |>
        unique() |>
        sort()
    )
}

#' @export
chart <- function(data) {
  data |>
    dplyr$group_by(Species) |>
    echarts4r$e_chart(Year) |>
    echarts4r$e_line(Population) |>
    echarts4r$e_x_axis(Year, formatter = JS("App.formatYear")) |>
    echarts4r$e_tooltip()
}
