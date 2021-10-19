
# Betting Agains Beta -----------------------------------------------------

#' Get the Bet Against Beta Factor
#'
#' Downloads data with the excess returns of long/short equity Betting Against Beta
#' (BAB) factors.
#'
#' @param .tidy A flag. Should the output be tidy? The default is \code{TRUE}.
#'
#' @return A \code{tibble}.
#' @export
#'
#' @references Frazzini, Andrea and Lasse H. Pedersen (2014),
#' 'Betting Against Beta', Journal of Financial Economics, 111.
#'
#' @examples
#' if (FALSE) {
#'   aqr_bab_monthly()
#' }
aqr_bab_monthly <- function(.tidy = TRUE) {

  assertthat::assert_that(assertthat::is.flag(.tidy))

  url <- "https://images.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Betting-Against-Beta-Equity-Factors-Monthly.xlsx"
  destfile <- "Betting_Against_Beta_Equity_Factors_Monthly.xlsx"
  curl::curl_download(url, destfile)

  bab_raw <- readxl::read_excel(
    path      = destfile,
    sheet     = "BAB Factors",
    range     = "A19:AD1108",
    col_types = c("date", rep("numeric", 29))
  )

  bab <- bab_raw |>
    dplyr::mutate(DATE = lubridate::as_date(.data$DATE)) |>
    dplyr::rename(date = "DATE")

  if (.tidy) {
    bab <- bab |>
      tidyr::pivot_longer(cols = -.data$date)
  }

  bab

}


# Market Factor -----------------------------------------------------------

#' Get the Market Factor
#'
#' Downloads data with the market returns in excess of t-bills.
#'
#' @param .tidy A flag. Should the output be tidy? The default is \code{TRUE}.
#'
#' @return A \code{tibble}.
#' @export
#'
#' @examples
#' if (FALSE) {
#'   aqr_mkt_monthly()
#' }
aqr_mkt_monthly <- function(.tidy = TRUE) {

  assertthat::assert_that(assertthat::is.flag(.tidy))

  url <- "https://images.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Betting-Against-Beta-Equity-Factors-Monthly.xlsx"
  destfile <- "Betting_Against_Beta_Equity_Factors_Monthly.xlsx"
  curl::curl_download(url, destfile)

  mkt_raw <- readxl::read_excel(
    path      = destfile,
    sheet     = "MKT",
    range     = "A19:AD1161",
    col_types = c("date", rep("numeric", 29))
  )

  mkt <- mkt_raw |>
    dplyr::mutate(DATE = lubridate::as_date(.data$DATE)) |>
    dplyr::rename(date = "DATE")

  if (.tidy) {
    mkt <- mkt |>
      tidyr::pivot_longer(cols = -.data$date)
  }

  mkt

}


# Small Minus Big ---------------------------------------------------------

#' Get the Small Minus Big Factor
#'
#' Downloads data with the self-financing returns of equity Small Minus Big (SMB)
#' factors.
#'
#' @param .tidy A flag. Should the output be tidy? The default is \code{TRUE}.
#'
#' @return A \code{tibble}.
#' @export
#'
#' @examples
#' if (FALSE) {
#'   aqr_smb_monthly()
#' }
aqr_smb_monthly <- function(.tidy = TRUE) {

  assertthat::assert_that(assertthat::is.flag(.tidy))

  url <- "https://images.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Betting-Against-Beta-Equity-Factors-Monthly.xlsx"
  destfile <- "Betting_Against_Beta_Equity_Factors_Monthly.xlsx"
  curl::curl_download(url, destfile)

  smb_raw <- readxl::read_excel(
    path      = destfile,
    sheet     = "SMB",
    range     = "A19:AD1161",
    col_types = c("date", rep("numeric", 29))
  )

  smb <- smb_raw |>
    dplyr::mutate(DATE = lubridate::as_date(.data$DATE)) |>
    dplyr::rename(date = "DATE")

  if (.tidy) {
    smb <- smb |>
      tidyr::pivot_longer(cols = -.data$date)
  }

  smb

}


# High Minus Low (Fama French) --------------------------------------------

#' Get the High Minus Low Factor
#'
#' Downloads data with the self-financing returns of equity High Minus Low (HML)
#' factors using the Book to Market definition of Fama and French (1992).
#'
#' @param .tidy A flag. Should the output be tidy? The default is \code{TRUE}.
#'
#' @return A \code{tibble}.
#' @export
#'
#' @examples
#' if (FALSE) {
#'   aqr_hml_ff_monthly()
#' }
aqr_hml_ff_monthly <- function(.tidy = TRUE) {

  assertthat::assert_that(assertthat::is.flag(.tidy))

  url <- "https://images.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Betting-Against-Beta-Equity-Factors-Monthly.xlsx"
  destfile <- "Betting_Against_Beta_Equity_Factors_Monthly.xlsx"
  curl::curl_download(url, destfile)

  hml_ff_raw <- readxl::read_excel(
    path      = destfile,
    sheet     = "HML FF",
    range     = "A19:AD1161",
    col_types = c("date", rep("numeric", 29))
  )

  hml_ff <- hml_ff_raw |>
    dplyr::mutate(DATE = lubridate::as_date(.data$DATE)) |>
    dplyr::rename(date = "DATE")

  if (.tidy) {
    hml_ff <- hml_ff |>
      tidyr::pivot_longer(cols = -.data$date)
  }

  hml_ff

}


# High Minus Low (Devil) --------------------------------------------

#' Get the High Minus Low Factor
#'
#' Downloads data with the  self-financing returns of equity High Minus Low Devil
#' (HML Devil) factors using the Book to Market definition of Asness and Frazzini (2013).
#'aa
#' @param .tidy A flag. Should the output be tidy? The default is \code{TRUE}.
#'
#' @return A \code{tibble}.
#' @export
#'
#' @examples
#' if (FALSE) {
#'   aqr_hml_devil_monthly()
#' }
aqr_hml_devil_monthly <- function(.tidy = TRUE) {

  assertthat::assert_that(assertthat::is.flag(.tidy))

  url <- "https://images.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Betting-Against-Beta-Equity-Factors-Monthly.xlsx"
  destfile <- "Betting_Against_Beta_Equity_Factors_Monthly.xlsx"
  curl::curl_download(url, destfile)

  hml_devil_raw <- readxl::read_excel(
    path      = destfile,
    sheet     = "HML FF",
    range     = "A19:AD1155",
    col_types = c("date", rep("numeric", 29))
  )

  hml_devil <- hml_devil_raw |>
    dplyr::mutate(DATE = lubridate::as_date(.data$DATE)) |>
    dplyr::rename(date = "DATE")

  if (.tidy) {
    hml_devil <- hml_devil |>
      tidyr::pivot_longer(cols = -.data$date)
  }

  hml_devil

}


# Up Minus Down --------------------------------------------

#' Get the Up Minus Down Factor
#'
#' Downloads data with the self-financing returns of equity Up Minus Down (UMD) factors.
#'
#' @param .tidy A flag. Should the output be tidy? The default is \code{TRUE}.
#'
#' @return A \code{tibble}.
#' @export
#'
#' @examples
#' if (FALSE) {
#'   aqr_umd_monthly()
#' }
aqr_umd_monthly <- function(.tidy = TRUE) {

  assertthat::assert_that(assertthat::is.flag(.tidy))

  url <- "https://images.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Betting-Against-Beta-Equity-Factors-Monthly.xlsx"
  destfile <- "Betting_Against_Beta_Equity_Factors_Monthly.xlsx"
  curl::curl_download(url, destfile)

  hml_umd_raw <- readxl::read_excel(
    path      = destfile,
    sheet     = "HML FF",
    range     = "A19:AD25414",
    col_types = c("date", rep("numeric", 29))
  )

  hml_umd <- hml_umd_raw |>
    dplyr::mutate(DATE = lubridate::as_date(.data$DATE)) |>
    dplyr::rename(date = "DATE")

  if (.tidy) {
    umd <- hml_umd |>
      tidyr::pivot_longer(cols = -.data$date)
  }

  umd

}
