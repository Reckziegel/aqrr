#' Long Run Commodities Performance
#'
#' Downloads an updated version of the data used in “Commodities for the Long Run".
#'
#' @param .tidy A flag. Should the output be tidy? The default is \code{TRUE}.
#'
#' @return A \code{tibble}.
#' @export
#'
#' @references “Commodities for the Long Run" (Levine, Ooi, Richardson,
#' and Sasseville, FAJ, 2018)
#'
#' @examples
#' if (FALSE) {
#'   aqr_commodities_long_run()
#' }
aqr_commodities_long_run <- function(.tidy = TRUE) {

  assertthat::assert_that(assertthat::is.flag(.tidy))

  url <- "https://images.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Commodities-for-the-Long-Run-Index-Level-Data-Monthly.xlsx"
  destfile <- "Commodities_for_the_Long_Run_Index_Level_Data_Monthly.xlsx"
  curl::curl_download(url, destfile)

  commodities_raw <- readxl::read_excel(
    path      = destfile,
    sheet     = "Commodities for the Long Run",
    range     = "A11:L1747"
  )

  commodities <- commodities_raw |>
    dplyr::mutate(Date = lubridate::as_date(.data$Date)) |>
    dplyr::slice(-c(1:275)) |>
    dplyr::rename(date = "Date") |>
    stats::na.omit()

  if (.tidy) {
    commodities <- commodities |>
      tidyr::pivot_longer(cols = -c(.data$date, dplyr::starts_with("State")))
  }

  commodities

}
