## ----setup, include=FALSE---------------------------------------------------------------------------------------
knitr::opts_chunk$set(tidy = FALSE, echo = TRUE)


## ---------------------------------------------------------------------------------------------------------------
library(readr)
library(lubridate)
library(photobiology)
library(photobiologyInOut)
library(ggspectra)
library(lubridate)
library(dplyr)
library(ggplot2)


## ---------------------------------------------------------------------------------------------------------------
clear_sky_diff_fr <- readRDS("TUV-diffuse-direct-SZA/spline-fun.RDS")


## ---------------------------------------------------------------------------------------------------------------
viikki_bio3.geo <- data.frame(lon = 25.019212, lat = 60.226805, address = "BIO3, Viikki")
locale_UTC <- locale(tz = "UTC")


## ---------------------------------------------------------------------------------------------------------------
millisecond_raw.tb <- read_csi_dat(file = "data-latest/Viikki Tower_TableMilliSecond.dat", locale = locale_UTC)

millisecond_raw.tb
nrow(millisecond_raw.tb)
ncol(millisecond_raw.tb)


## ---------------------------------------------------------------------------------------------------------------
colnames(millisecond_raw.tb)


## ---------------------------------------------------------------------------------------------------------------
cat(comment(millisecond_raw.tb))


## ---------------------------------------------------------------------------------------------------------------
rle.check <- rle(sort(as.numeric(millisecond_raw.tb$TIMESTAMP)))
duplicates <- sum(rle.check$lengths > 1)
if (duplicates > 0L) {
  message("Found ", duplicates, " duplicated rows in data.")
  millisecond_raw.tb <- distinct(millisecond_raw.tb, TIMESTAMP, .keep_all = TRUE)
}


## ---------------------------------------------------------------------------------------------------------------
which(diff(millisecond_raw.tb$TIMESTAMP) > minutes(30) &
        diff(millisecond_raw.tb$TIMESTAMP) < minutes(90)) -> clock.forward.selector
clock.forward.selector
diff(millisecond_raw.tb$TIMESTAMP)[clock.forward.selector]
millisecond_raw.tb$TIMESTAMP[clock.forward.selector]


## ---------------------------------------------------------------------------------------------------------------
millisecond_raw.tb[["TIMESTAMP"]][1]
millisecond_raw.tb[["TIMESTAMP"]][nrow(millisecond_raw.tb)]
tz(millisecond_raw.tb[["TIMESTAMP"]][1])
millisecond_raw.tb[["TIMESTAMP"]] <-  millisecond_raw.tb[["TIMESTAMP"]] - hours(2) # UTC + 2h -> UTC
millisecond_raw.tb[["TIMESTAMP"]][1]
millisecond_raw.tb[["TIMESTAMP"]][nrow(millisecond_raw.tb)]
tz(millisecond_raw.tb[["TIMESTAMP"]][1])


## ---------------------------------------------------------------------------------------------------------------
  # NEW CR6 program
millisecond_raw.tb %>%
  filter(!is.na(TIMESTAMP)) %>%
  mutate(year = year(TIMESTAMP),
         month_of_year = month(TIMESTAMP),
         month_name = factor(month_of_year, levels = 12:1, labels = rev(month.name)),
         week_of_year = week(TIMESTAMP),
         day_of_year = yday(TIMESTAMP),
         time_of_day_utc = as_tod(TIMESTAMP),
         solar_time_h = solar_time(TIMESTAMP, 
                                   geocode = viikki_bio3.geo),
         solar_time_s = solar_time(TIMESTAMP, 
                                   geocode = viikki_bio3.geo, 
                                   unit.out = "seconds"),
         sun_elevation = sun_elevation(TIMESTAMP, 
                                       geocode = viikki_bio3.geo, 
                                       use.refraction = TRUE,
                                       tz = "UTC"),
         sun_azimuth = sun_azimuth(TIMESTAMP, 
                                   geocode = viikki_bio3.geo, 
                                   use.refraction = TRUE,
                                   tz = "UTC")) -> millisecond.tb


## ---------------------------------------------------------------------------------------------------------------
colnames(millisecond.tb)

## ---------------------------------------------------------------------------------------------------------------
obj.name <- paste("millisecond_", year(millisecond_raw.tb$TIMESTAMP[1]),
                  "_", month(millisecond_raw.tb$TIMESTAMP[1]), 
                  "_", day(millisecond_raw.tb$TIMESTAMP[1]), ".tb", sep = "")
assign(obj.name, millisecond.tb)

save(list = obj.name, file = paste("data-rda/", obj.name, ".rda", sep = ""))


## ---------------------------------------------------------------------------------------------------------------
millisecond.tb %>%
  head()
millisecond.tb %>%
  tail()


## ---------------------------------------------------------------------------------------------------------------
ggplot(millisecond.tb[-1, ], aes(PAR_Den_CS)) +
  stat_density() +
  scale_x_continuous(limits = function(x) {c(x[1] - 100, x[2] + 20)})


## ---------------------------------------------------------------------------------------------------------------
ggplot(millisecond.tb[-1, ], aes(TIMESTAMP, PAR_Den_CS)) +
  geom_line() +
  expand_limits(y = 0)


## ---------------------------------------------------------------------------------------------------------------
ggplot(subset(millisecond.tb[-1, ], TIMESTAMP > ymd_hm("2024-05-10 15:03") & TIMESTAMP < ymd_hm("2024-05-10 15:05")), aes(TIMESTAMP, PAR_Den_CS)) +
  geom_line() +
  expand_limits(y = 0)


## ---------------------------------------------------------------------------------------------------------------
ggplot(subset(millisecond.tb[-1, ], TIMESTAMP > ymd_hms("2024-05-10 15:03:30") & TIMESTAMP < ymd_hms("2024-05-10 15:03:45")), aes(TIMESTAMP, PAR_Den_CS)) +
  geom_line() +
  stat_panel_counts(label.y = "bottom") +
  expand_limits(y = 0)

