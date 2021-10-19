#' Premia From a Wide Array of Factors
#'
#' Downloads data with the  self-financing returns of equity, bonds, currencies and
#' commodities.
#'
#' @param .tidy A flag. Should the output be tidy? The default is \code{TRUE}.
#'
#' @return A \code{tibble}.
#' @export
#'
#' @references  Ilmanen, Israel, Lee, Moskowitz & Thapar (2021),
#' “How Do Factor Premia Vary Over Time? A Century of Evidence".
#'
#' @examples
#' if (FALSE) {
#'   aqr_factor_premia_monthly()
#' }
aqr_factor_premia_monthly <- function(.tidy = TRUE) {

  assertthat::assert_that(assertthat::is.flag(.tidy))

  url <- "https://images.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Century-of-Factor-Premia-Monthly.xlsx"
  destfile <- "Century_of_Factor_Premia_Monthly.xlsx"
  curl::curl_download(url, destfile)

  factor_premia_raw <- readxl::read_excel(
    path      = destfile,
    sheet     = "Century of Factor Premia",
    range     = "A19:AS1161",
    col_types = c("date", rep("numeric", 44))
  )

  factor_premia <- factor_premia_raw |>
    dplyr::mutate(Date = lubridate::as_date(.data$Date)) |>
    dplyr::select(-dplyr::starts_with("Intl")) |>
    dplyr::rename(date = "Date")

  if (.tidy) {
    factor_premia <- factor_premia |>
      tidyr::pivot_longer(cols = -.data$date)
  }

  factor_premia

}
