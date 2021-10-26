
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aqrr

<!-- badges: start -->

[![R-CMD-check](https://github.com/Reckziegel/aqqr/workflows/R-CMD-check/badge.svg)](https://github.com/Reckziegel/aqqr/actions)

<!-- badges: end -->

Get the [datasets](https://www.aqr.com/Insights/Datasets) computed by
the AQR research team in R.

## Installation

You can install the development version of aqrr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Reckziegel/aqqr")
```

``` r
library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(aqrr)

# Quality Minus Junk Factor
qmj <- aqr_qmj_monthly()
na.omit(qmj)
#> # A tibble: 9,669 x 3
#>    date       name     value
#>    <date>     <chr>    <dbl>
#>  1 1957-07-31 USA    0.0112 
#>  2 1957-08-31 USA    0.00488
#>  3 1957-09-30 USA    0.00701
#>  4 1957-10-31 USA    0.00271
#>  5 1957-11-30 USA   -0.00897
#>  6 1957-12-31 USA   -0.00327
#>  7 1958-01-31 USA   -0.0252 
#>  8 1958-02-28 USA    0.00480
#>  9 1958-03-31 USA    0.0159 
#> 10 1958-04-30 USA    0.00256
#> # ... with 9,659 more rows
```

``` r
qmj |> 
  filter(date >= "2000-01-02") |> 
  group_by(name) |> 
  na.omit() |> 
  mutate(performance = cumprod(exp(value))) |> 
  ungroup() |> 
  ggplot(aes(x = date, y = performance, color = name)) + 
  geom_line(show.legend = FALSE) + 
  scale_y_log10() + 
  facet_wrap(~name, scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title    = "Performance of the Quality Minus Junk Factor", 
       subtitle = "Monthly Data from 2020-01-02 up to 2021-08-31", 
       caption  = "More info on www.aqr.com", 
       x        = NULL, 
       y        = NULL)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

## Functions

See the all available functions in the
[reference](https://reckziegel.github.io/aqrr/reference/index.html)
page.
