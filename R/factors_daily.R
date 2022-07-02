
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
#'   aqr_bab_daily()
#' }
aqr_bab_daily <- function(.tidy = TRUE) {

  assertthat::assert_that(assertthat::is.flag(.tidy))

  url <- "https://www.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Betting-Against-Beta-Equity-Factors-Daily.xlsx"
  destfile <- "Betting_Against_Beta_Equity_Factors_Daily.xlsx"
  curl::curl_download(url, destfile)

  bab_raw <- readxl::read_excel(
    path      = destfile,
    sheet     = "BAB Factors",
    range     = "A19:AD24257",
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
#'   aqr_mkt_daily()
#' }
aqr_mkt_daily <- function(.tidy = TRUE) {

  assertthat::assert_that(assertthat::is.flag(.tidy))

  url <- "https://www.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Betting-Against-Beta-Equity-Factors-Daily.xlsx"
  destfile <- "Betting_Against_Beta_Equity_Factors_Daily.xlsx"
  curl::curl_download(url, destfile)

  mkt_raw <- readxl::read_excel(
    path      = destfile,
    sheet     = "MKT",
    range     = "A19:AD25556",
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
#'   aqr_smb_daily()
#' }
aqr_smb_daily <- function(.tidy = TRUE) {

  assertthat::assert_that(assertthat::is.flag(.tidy))

  url <- "https://www.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Betting-Against-Beta-Equity-Factors-Daily.xlsx"
  destfile <- "Betting_Against_Beta_Equity_Factors_Daily.xlsx"
  curl::curl_download(url, destfile)

  smb_raw <- readxl::read_excel(
    path      = destfile,
    sheet     = "SMB",
    range     = "A19:AD21451",
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
#'   aqr_hml_ff_daily()
#' }
aqr_hml_ff_daily <- function(.tidy = TRUE) {

  assertthat::assert_that(assertthat::is.flag(.tidy))

  url <- "https://images.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Betting-Against-Beta-Equity-Factors-Daily.xlsx"
  destfile <- "Betting_Against_Beta_Equity_Factors_Daily.xlsx"
  curl::curl_download(url, destfile)

  hml_ff_raw <- readxl::read_excel(
    path      = destfile,
    sheet     = "HML FF",
    range     = "A19:AD25566",
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
#'
#' @param .tidy A flag. Should the output be tidy? The default is \code{TRUE}.
#'
#' @return A \code{tibble}.
#' @export
#'
#' @examples
#' if (FALSE) {
#'   aqr_hml_devil_daily()
#' }
aqr_hml_devil_daily <- function(.tidy = TRUE) {

  assertthat::assert_that(assertthat::is.flag(.tidy))

  url <- "https://www.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/The-Devil-in-HMLs-Details-Factors-Daily.xlsx"
  destfile <- "Betting_Against_Beta_Equity_Factors_Daily.xlsx"
  curl::curl_download(url, destfile)

  hml_devil_raw <- readxl::read_excel(
    path      = destfile,
    sheet     = "HML FF",
    range     = "A19:AD25566",
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
#'   aqr_umd_daily()
#' }
aqr_umd_daily <- function(.tidy = TRUE) {

  assertthat::assert_that(assertthat::is.flag(.tidy))

  url <- "https://images.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Betting-Against-Beta-Equity-Factors-Daily.xlsx"
  destfile <- "Betting_Against_Beta_Equity_Factors_Daily.xlsx"
  curl::curl_download(url, destfile)

  hml_umd_raw <- readxl::read_excel(
    path      = destfile,
    sheet     = "UMD",
    range     = "A19:AD25416",
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

#' Get the Up Quality Minus Junk Factor
#'
#' Downloads data with the self-financing returns of of a long/short Quality
#' Minus Junk (QMJ) factors.
#'
#' @param .tidy A flag. Should the output be tidy? The default is \code{TRUE}.
#'
#' @return A \code{tibble}.
#' @export
#'
#' @examples
#' if (FALSE) {
#'   aqr_qmj_daily()
#' }
aqr_qmj_daily <- function(.tidy = TRUE) {

  assertthat::assert_that(assertthat::is.flag(.tidy))

  url <- "https://www.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Quality-Minus-Junk-Factors-Daily.xlsx"
  destfile <- "Quality_Minus_Junk_Factors_Daily.xlsx"
  curl::curl_download(url, destfile)

  qmj_raw <- readxl::read_excel(
    path      = destfile,
    sheet     = "QMJ Factors",
    range     = "A19:AD16657",
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
