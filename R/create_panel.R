#' Create a panel of dates
#'
#' @description
#' `create_panel` creates a panel of dates.
#'
#' @param start_date A character in YYYY-MM-DD format. The start date of the panel
#' @param end_date A character in YYYY-MM-DD format. The end date of the panel
#' @param by
#'
#' @returns A tibble of dates in panel-format
#'
#' @examples
#'
#' ##
#' create_panel()
#'
#'
#'
#' @export
#'
#'


create_panel <- function(start_date, end_date, by) {
  panel <- seq(lubridate::as_date(start_date), lubridate::as_date(end_date) , by= by) |>
    tibble::as_tibble() |>
    dplyr::rename(date = value) |>
    dplyr::mutate(month = lubridate::month(date),
           year = lubridate::year(date),
           day = lubridate::day(date),
           week = lubridate::week(date)) |>
    dplyr::mutate(year_month = lubridate::ymd(paste0(year, "-",month, "-1")))
  return(panel)
}

