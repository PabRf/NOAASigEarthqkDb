
# NOAASigEarthqkDb

<!-- badges: start -->
<!-- badges: end -->

The goal of NOAASigEarthqkDb is to provide tools for processing and visualizing the NOAA data so users may extract useful information embedded within.

## Installation

You can install the development version of NOAASigEarthqkDb like so:

``` r
install.packages("devtools")
install.packages("PabRf/NOAASigEarthqkDb")
```

## Example

These are basic examples which shows you how to use the functions:

``` r
library(NOAASigEarthqkDb)

eq_location_clean("earthquakes_-2150to2024.csv")

eq_clean_data("./inst/extdata/earthquakes_-2150to2024_clean1.csv")

eq_filter_data("./inst/extdata/earthquakes_-2150to2024_clean2.csv", xmin = as.Date("1950-01-01", "%Y-%m-%d"),
               xmax = as.Date("1955-01-01", "%Y-%m-%d"), c("Date", "xend", "y", "yend", "Mag", "Deaths", "Country"))

ggplot() + geom_timeline(data=sigEarthqks_final, aes(Date, y=0.2, size=Mag, colour=Deaths), alpha=0.3)
ggplot() + geom_timeline(data=sigEarthqks_final, aes(Date, Country, size=Mag, colour=Deaths), alpha=0.3)

ggplot() + geom_timeline_label(data=sigEarthqks_final, aes(Date, y=0.2, xend=xend, yend=yend, size=Mag, colour=Deaths,
  label=Country), alpha=0.3) 
ggplot() + geom_timeline_label(data=sigEarthqks_final, aes(Date, Country, xend=xend, yend=yend, size=Mag, colour=Deaths,
  label=Country), alpha=0.3)

library(magrittr)
dplyr::filter(sigEarthqks_clean2, Country == "MEXICO" & lubridate::year(Date) >= 2000) %>%
  eq_map(annot_col="Date", -120, -60, 0, 40)

dplyr::filter(sigEarthqks_clean2, Country == "MEXICO" & lubridate::year(Date) >= 2000) %>%
  dplyr::mutate(popup_text = eq_create_label(.)) %>%
  eq_map(annot_col="popup_text", -120, -60, 0, 40)

  
```

