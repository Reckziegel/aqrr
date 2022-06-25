#' Get the AQR Momentum Indexes
#'
#' Downloads data with the returns of AQR Momentum Index, AQR Small Cap Momentum Index,
#' and AQR International Momentum Index.
#'
#' @param .tidy A flag. Should the output be tidy? The default is \code{TRUE}.
#'
#' @return A \code{tibble}.
#' @export
#'
#'
#' @examples
#' if (FALSE) {
#'   aqr_momentum_monthly()
#' }
aqr_momentum_monthly <- function(.tidy = TRUE) {

  assertthat::assert_that(assertthat::is.flag(.tidy))

  url <- "https://www.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/AQR-Index-Returns.xls"
  destfile <- "AQR_Index_Returns.xls"
  curl::curl_download(url, destfile)

  momentum_raw <- readxl::read_excel(
    path      = destfile,
    sheet     = "Returns",
    range     = "A2:D510",
    col_types = c("date", rep("numeric", 3))
  )

  momentum <- momentum_raw |>
    dplyr::mutate(Month = lubridate::as_date(.data$Month)) |>
    dplyr::rename(date = "Month")

  if (.tidy) {
    momentum <- momentum |>
      tidyr::pivot_longer(cols = -.data$date)
  }

  momentum

}


