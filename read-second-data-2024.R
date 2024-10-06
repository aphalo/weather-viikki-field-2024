## ----setup, include=FALSE------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(tidy = FALSE, echo = TRUE)


## ------------------------------------------------------------------------------------------------------------------------
library(readr)
library(lubridate)
library(photobiology)
library(photobiologyInOut)
library(lubridate)
library(dplyr)


## ------------------------------------------------------------------------------------------------------------------------
clear_sky_diff_fr <- readRDS("TUV-diffuse-direct-SZA/spline-fun.RDS")


## ------------------------------------------------------------------------------------------------------------------------
viikki_bio3.geo <- data.frame(lon = 25.019212, lat = 60.226805, address = "BIO3, Viikki")
locale_UTC <- locale(tz = "UTC")


## ------------------------------------------------------------------------------------------------------------------------
second_raw.tb <- read_csi_dat(file = "data-latest/Viikki Tower_TableSecond.dat", locale = locale_UTC)

head(second_raw.tb, 50)
tail(second_raw.tb, 50)
nrow(second_raw.tb)
ncol(second_raw.tb)


## ------------------------------------------------------------------------------------------------------------------------
colnames(second_raw.tb) <- gsub("_Avg$", "", colnames(second_raw.tb))
colnames(second_raw.tb)


## ------------------------------------------------------------------------------------------------------------------------
comment(second_raw.tb) <- gsub("_Avg", "", comment(second_raw.tb))
cat(comment(second_raw.tb))


## ------------------------------------------------------------------------------------------------------------------------
rle.check <- rle(sort(as.numeric(second_raw.tb$TIMESTAMP)))
duplicates <- sum(rle.check$lengths > 1)
if (duplicates > 0L) {
  message("Found ", duplicates, " duplicated rows in data.")
  second_raw.tb <- distinct(second_raw.tb, TIMESTAMP, .keep_all = TRUE)
}

head(second_raw.tb, 50)
tail(second_raw.tb, 50)


## ------------------------------------------------------------------------------------------------------------------------
which(diff(second_raw.tb$TIMESTAMP) > minutes(30) &
        diff(second_raw.tb$TIMESTAMP) < minutes(90)) -> clock.forward.selector
clock.forward.selector
diff(second_raw.tb$TIMESTAMP)[clock.forward.selector]
second_raw.tb$TIMESTAMP[clock.forward.selector]


## ------------------------------------------------------------------------------------------------------------------------
second_raw.tb[["TIMESTAMP"]][1]
second_raw.tb[["TIMESTAMP"]][nrow(second_raw.tb)]
tz(second_raw.tb[["TIMESTAMP"]][1])
second_raw.tb[["TIMESTAMP"]] <-  second_raw.tb[["TIMESTAMP"]] - hours(2) # UTC + 2h -> UTC
second_raw.tb[["TIMESTAMP"]][1]
second_raw.tb[["TIMESTAMP"]][nrow(second_raw.tb)]
tz(second_raw.tb[["TIMESTAMP"]][1])

head(second_raw.tb, 50)
tail(second_raw.tb, 50)



## ------------------------------------------------------------------------------------------------------------------------
  # NEW CR6 program
second_raw.tb |>
  filter(!is.na(TIMESTAMP)) |>
  mutate(red_umol = ifelse(Red_Den_cal > 0, Red_Den_cal, 0),
         far_red_umol = ifelse(Far_red_Den_cal > 0, Far_red_Den_cal, 0),
         # Ratio is discarded if it is "too dark"
         # A reasonable cut off seems to be at 0.05 umol m-2 s-1.
         PAR_diff_fr_raw = PAR_BF_diff/PAR_BF_tot,
         # PAR readings from BF5 are noisy and start being unreliable at
         # a relatively high irradiance of 10 umol m-2 s-1.
         PAR_diff_fr = ifelse(PAR_BF_diff < 10,
                              NA_real_,
                              PAR_diff_fr_raw),
         year = year(TIMESTAMP),
         month_of_year = month(TIMESTAMP),
         month_name = factor(month_of_year, levels = 12:1, labels = rev(month.name)),
         week_of_year = week(TIMESTAMP),
         day_of_year = yday(TIMESTAMP),
         time_of_day_utc = as_tod(TIMESTAMP),
         solar_time_h = solar_time(TIMESTAMP, 
                                   geocode = viikki_bio3.geo),
         sun_elevation = sun_elevation(TIMESTAMP, 
                                       geocode = viikki_bio3.geo, 
                                       use.refraction = TRUE,
                                       tz = "UTC"),
         sun_azimuth = sun_azimuth(TIMESTAMP, 
                                   geocode = viikki_bio3.geo, 
                                   use.refraction = TRUE,
                                   tz = "UTC"),
         PAR_diff_fr_rel = 1 - ((1 - PAR_diff_fr) / (1 - clear_sky_diff_fr(sun_elevation))),
         sunny = PAR_diff_fr_rel < 0.6) -> second.tb

head(second.tb, 50)
tail(second.tb, 50)



## ------------------------------------------------------------------------------------------------------------------------
colnames(second.tb)
second.tb[1, ]


## ------------------------------------------------------------------------------------------------------------------------
series.name <- paste(year(second_raw.tb$TIMESTAMP[1]),
                  "_", month(second_raw.tb$TIMESTAMP[1]), 
                  "_", day(second_raw.tb$TIMESTAMP[1]), ".tb", sep = "")
obj.name <- paste("second_", series.name, sep = "")
assign(obj.name, second.tb)

head(second_2024_5_14.tb, 50)
tail(second_2024_5_14.tb, 50)

save(list = obj.name, file = paste("data-rda/", obj.name, ".rda", sep = ""))


## ----eval=FALSE----------------------------------------------------------------------------------------------------------
## second.tb$time.minute <- floor_date(second.tb$TIMESTAMP, unit = "minute")
## 
## second.tb |>
##   select(time.minute, PAR_Den_CS:PAR_diff_fr, PAR_diff_fr_rel) |>
##   group_by(time.minute) |>
##   summarise(across(PAR_Den_CS:PAR_diff_fr_rel, mean)) |>
##   ungroup() -> minute.tb
## 
## minute.tb |>
##     mutate(year = year(time.minute),
##            month_of_year = month(time.minute),
##            month_name = factor(month_of_year, levels = 12:1, labels = rev(month.name)),
##            week_of_year = week(time.minute),
##            day_of_year = yday(time.minute),
##            time_of_day_utc = as_tod(time.minute),
##            solar_time_h = solar_time(time.minute,
##                                      geocode = viikki_bio3.geo),
##            sun_elevation = sun_elevation(time.minute,
##                                          geocode = viikki_bio3.geo,
##                                          use.refraction = TRUE,
##                                          tz = "UTC"),
##            sun_azimuth = sun_azimuth(time.minute,
##                                      geocode = viikki_bio3.geo,
##                                      use.refraction = TRUE,
##                                      tz = "UTC"),
##            sunny = PAR_diff_fr_rel < 0.6) -> minute.tb


## ----eval=FALSE----------------------------------------------------------------------------------------------------------
## minute.tb %>%
##   head()
## minute.tb %>%
##   tail()


## ----eval=FALSE----------------------------------------------------------------------------------------------------------
## obj.name <- paste("minute_calc_", series.name, sep = "")
## assign(obj.name, minute.tb)
## 
## save(list = obj.name, file = paste("data-rda/", obj.name, ".rda", sep = ""))

