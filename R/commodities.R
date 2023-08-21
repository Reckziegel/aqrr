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

  url <- "https://www.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Commodities-for-the-Long-Run-Index-Level-Data-Monthly.xlsx?sc_lang=en"
  destfile <- "Commodities_for_the_Long_Run_Index_Level_Data_Monthly.xlsx"
  curl::curl_download(url, destfile)

  suppressMessages(
    commodities_raw <- readxl::read_excel(
      path      = destfile,
      sheet     = "Commodities for the Long Run",
      range     = "A11:L1762",
    )
  )

  names(commodities_raw)[[1]] <- "date"

  suppressWarnings(
    commodities <- commodities_raw |>
      tidyr::separate(
        col     = "date",
        into    = c("year", "month", "day"),
        sep     = "-",
        remove  = TRUE,
        convert = TRUE) |>
      dplyr::mutate(date = lubridate::make_date(year, month, day)) |>
      dplyr::select(-c(day, month, year)) |>
      dplyr::select(date, dplyr::everything())
  )


  if (.tidy) {
    commodities <- commodities |>
      tidyr::pivot_longer(cols = -c(date, dplyr::starts_with("State"))) |>
      dplyr::mutate_if(is.character, as.factor)
  }

  commodities

}
