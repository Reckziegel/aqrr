
#' Credit Risk Premium
#'
#' Downloads the credit excess return data used in Asvanunt and Richardson, 2015,
#' “The Credit Risk Premium," working paper.
#'
#' @param .tidy A flag. Should the output be tidy? The default is \code{TRUE}.
#'
#' @return A \code{tibble}.
#' @export
#'
#' @examples
#' if (FALSE) {
#'   aqr_credit_risk_premium()
#' }
aqr_credit_risk_premium <- function(.tidy = TRUE) {

  assertthat::assert_that(assertthat::is.flag(.tidy))

  url <- "https://www.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Credit-Risk-Premium-Preliminary-Paper-Data.xlsx"
  destfile <- "Credit_Risk_Premium_Preliminary_Paper_Data.xlsx"
  curl::curl_download(url, destfile)

  credit_risk_raw <- readxl::read_excel(
    path      = destfile,
    sheet     = "Credit Risk Premium",
    range     = "A11:D1079",
    col_types = c("guess", rep("numeric", 3))
  )

  credit_risk <- credit_risk_raw |>
    dplyr::mutate(Date = lubridate::as_date(Date)) |>
    dplyr::rename(date = "Date")

  if (.tidy) {
    credit_risk <- credit_risk |>
      tidyr::pivot_longer(cols = -date, names_ptypes = factor())
  }

  credit_risk

}
