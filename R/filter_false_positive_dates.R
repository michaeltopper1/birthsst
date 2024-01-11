
#' Pass a panelsummary::panelsummary_raw dataframe into kableExtra::kbl() with typical defaults
#'
#' @description
#' `filter_false_positive_dates` omits any dates that are considered to be high false positives.
#'
#' @param x The data.frame (or tibble) from that needs the dates removed.
#' @returns A raw data frame that is ready for further manipulation.
#'
#' @examples
#'
#' ##
#' filter_false_positive_dates(x)
#'
#'
#'
#' @export
#'
#'



filter_false_positive_dates <- function(x){
  x <- x %>%
    dplyr::filter(!(day == 4 & month == 7)) |>
    dplyr::filter(!(day == 5 & month == 7)) |>
    dplyr::filter(!(day == 1 & month == 1)) |>
    dplyr::filter(!(day == 31 & month == 12))
  return(x)
}
