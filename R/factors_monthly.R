
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

  url <- "https://www.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Betting-Against-Beta-Equity-Factors-Monthly.xlsx"
  destfile <- "Betting_Against_Beta_Equity_Factors_Monthly.xlsx"
  curl::curl_download(url, destfile)

  bab_raw <- readxl::read_excel(
    path      = destfile,
    sheet     = "BAB Factors",
    range     = "A19:AD1115",
    col_types = c("guess", rep("numeric", 29))
  )

  bab <- bab_raw |>
    tidyr::separate(
      col     = "DATE",
      into    = c("month", "day", "year"),
      sep     = "/",
      remove  = TRUE,
      convert = TRUE) |>
    dplyr::mutate(date = lubridate::dmy(as.double(paste0(.data$day, .data$month, .data$year)))) |>
    dplyr::select(-c(.data$day, .data$month, .data$year)) |>
    dplyr::select(.data$date, dplyr::everything())

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

  url <- "https://www.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Betting-Against-Beta-Equity-Factors-Monthly.xlsx"
  destfile <- "Betting_Against_Beta_Equity_Factors_Monthly.xlsx"
  curl::curl_download(url, destfile)

  mkt_raw <- readxl::read_excel(
    path      = destfile,
    sheet     = "MKT",
    range     = "A19:AD1168",
    col_types = c("guess", rep("numeric", 29))
  )

  mkt <- mkt_raw |>
    tidyr::separate(
      col     = "DATE",
      into    = c("month", "day", "year"),
      sep     = "/",
      remove  = TRUE,
      convert = TRUE) |>
    dplyr::mutate(date = lubridate::dmy(as.double(paste0(.data$day, .data$month, .data$year)))) |>
    dplyr::select(-c(.data$day, .data$month, .data$year)) |>
    dplyr::select(.data$date, dplyr::everything())

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

  url <- "https://www.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Betting-Against-Beta-Equity-Factors-Monthly.xlsx"
  destfile <- "Betting_Against_Beta_Equity_Factors_Monthly.xlsx"
  curl::curl_download(url, destfile)

  smb_raw <- readxl::read_excel(
    path      = destfile,
    sheet     = "SMB",
    range     = "A19:AD1168",
    col_types = c("guess", rep("numeric", 29))
  )

  smb <- smb_raw |>
    tidyr::separate(
      col     = "DATE",
      into    = c("month", "day", "year"),
      sep     = "/",
      remove  = TRUE,
      convert = TRUE) |>
    dplyr::mutate(date = lubridate::dmy(as.double(paste0(.data$day, .data$month, .data$year)))) |>
    dplyr::select(-c(.data$day, .data$month, .data$year)) |>
    dplyr::select(.data$date, dplyr::everything())

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

  url <- "https://www.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Betting-Against-Beta-Equity-Factors-Monthly.xlsx"
  destfile <- "Betting_Against_Beta_Equity_Factors_Monthly.xlsx"
  curl::curl_download(url, destfile)

  hml_ff_raw <- readxl::read_excel(
    path      = destfile,
    sheet     = "HML FF",
    range     = "A19:AD1168",
    col_types = c("guess", rep("numeric", 29))
  )

  hml_ff <- hml_ff_raw |>
    tidyr::separate(
      col     = "DATE",
      into    = c("month", "day", "year"),
      sep     = "/",
      remove  = TRUE,
      convert = TRUE) |>
    dplyr::mutate(date = lubridate::dmy(as.double(paste0(.data$day, .data$month, .data$year)))) |>
    dplyr::select(-c(.data$day, .data$month, .data$year)) |>
    dplyr::select(.data$date, dplyr::everything())

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

  url <- "https://www.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Betting-Against-Beta-Equity-Factors-Monthly.xlsx"
  destfile <- "Betting_Against_Beta_Equity_Factors_Monthly.xlsx"
  curl::curl_download(url, destfile)

  hml_devil_raw <- readxl::read_excel(
    path      = destfile,
    sheet     = "HML Devil",
    range     = "A19:AD1168",
    col_types = c("guess", rep("numeric", 29))
  )

  hml_devil <- hml_devil_raw |>
    tidyr::separate(
      col     = "DATE",
      into    = c("month", "day", "year"),
      sep     = "/",
      remove  = TRUE,
      convert = TRUE) |>
    dplyr::mutate(date = lubridate::dmy(as.double(paste0(.data$day, .data$month, .data$year)))) |>
    dplyr::select(-c(.data$day, .data$month, .data$year)) |>
    dplyr::select(.data$date, dplyr::everything())

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

  url <- "https://www.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Betting-Against-Beta-Equity-Factors-Monthly.xlsx"
  destfile <- "Betting_Against_Beta_Equity_Factors_Monthly.xlsx"
  curl::curl_download(url, destfile)

  hml_umd_raw <- readxl::read_excel(
    path      = destfile,
    sheet     = "UMD",
    range     = "A19:AD1162",
    col_types = c("guess", rep("numeric", 29))
  )

  hml_umd <- hml_umd_raw |>
    tidyr::separate(
      col     = "DATE",
      into    = c("month", "day", "year"),
      sep     = "/",
      remove  = TRUE,
      convert = TRUE) |>
    dplyr::mutate(date = lubridate::dmy(as.double(paste0(.data$day, .data$month, .data$year)))) |>
    dplyr::select(-c(.data$day, .data$month, .data$year)) |>
    dplyr::select(.data$date, dplyr::everything())

  if (.tidy) {
    umd <- hml_umd |>
      tidyr::pivot_longer(cols = -.data$date)
  }

  umd

}


# Quality Minus Junk --------------------------------------------

#' Get the Quality Minus Junk Factor
#'
#' Downloads data with the self-financing returns of a of a long/short Quality
#' Minus Junk (QMJ) factors.
#'
#' @param .tidy A flag. Should the output be tidy? The default is \code{TRUE}.
#'
#' @return A \code{tibble}.
#' @export
#'
#' @examples
#' if (FALSE) {
#'   aqr_qmj_monthly()
#' }
aqr_qmj_monthly <- function(.tidy = TRUE) {

  assertthat::assert_that(assertthat::is.flag(.tidy))

  url <- "https://www.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Quality-Minus-Junk-Factors-Monthly.xlsx"
  destfile <- "Quality_Minus_Junk_Factors_Monthly.xlsx"
  curl::curl_download(url, destfile)

  qmj_raw <- readxl::read_excel(
    path      = destfile,
    sheet     = "QMJ Factors",
    range     = "A19:AD796",
    col_types = c("guess", rep("numeric", 29))
  )

  qmj <- qmj_raw |>
    tidyr::separate(
      col     = "DATE",
      into    = c("month", "day", "year"),
      sep     = "/",
      remove  = TRUE,
      convert = TRUE) |>
    dplyr::mutate(date = lubridate::dmy(as.double(paste0(.data$day, .data$month, .data$year)))) |>
    dplyr::select(-c(.data$day, .data$month, .data$year)) |>
    dplyr::select(.data$date, dplyr::everything())

  if (.tidy) {
    qmj <- qmj |>
      tidyr::pivot_longer(cols = -.data$date)
  }

  qmj

}
