## ---------------------------------------------------------------------------------------------------------------------
rm(list = ls(pattern = "*"))


## ----setup, include=FALSE---------------------------------------------------------------------------------------------
knitr::opts_chunk$set(tidy = FALSE, echo = TRUE)


## ---------------------------------------------------------------------------------------------------------------------
library(readr)
library(lubridate)
library(photobiology)
library(photobiologyInOut)
library(lubridate)
library(dplyr)


## ---------------------------------------------------------------------------------------------------------------------
clear_sky_diff_fr <- readRDS("TUV-diffuse-direct-SZA/spline-fun.RDS")


## ---------------------------------------------------------------------------------------------------------------------
viikki_bio3.geo <- data.frame(lon = 25.019212, lat = 60.226805, address = "BIO3, Viikki")
locale_UTC <- locale(tz = "UTC")


## ---------------------------------------------------------------------------------------------------------------------
second_raw.tb <- read_csi_dat(file = "data-latest/Viikki Tower_TableSecond.dat", locale = locale_UTC)
# second_raw.tb <- read_csi_dat(file = "data-logged/data-2025-01-14/Viikki Tower_TableSecond.dat", locale = locale_UTC)

head(second_raw.tb, 50)
tail(second_raw.tb, 50)
nrow(second_raw.tb)
ncol(second_raw.tb)

range(second_raw.tb$TIMESTAMP)


## ---------------------------------------------------------------------------------------------------------------------
colnames(second_raw.tb) <- gsub("_Avg$", "", colnames(second_raw.tb))
colnames(second_raw.tb)


## ---------------------------------------------------------------------------------------------------------------------
comment(second_raw.tb) <- gsub("_Avg", "", comment(second_raw.tb))
cat(comment(second_raw.tb))


## ---------------------------------------------------------------------------------------------------------------------
rle.check <- rle(sort(as.numeric(second_raw.tb$TIMESTAMP)))
duplicates <- sum(rle.check$lengths > 1)
if (duplicates > 0L) {
  message("Found ", duplicates, " duplicated rows in data.")
  second_raw.tb <- distinct(second_raw.tb, TIMESTAMP, .keep_all = TRUE)
}

head(second_raw.tb, 50)
tail(second_raw.tb, 50)


## ---------------------------------------------------------------------------------------------------------------------
which(diff(second_raw.tb$TIMESTAMP) > minutes(30) &
        diff(second_raw.tb$TIMESTAMP) < minutes(90)) -> clock.forward.selector
clock.forward.selector
diff(second_raw.tb$TIMESTAMP)[clock.forward.selector]
second_raw.tb$TIMESTAMP[clock.forward.selector]


## ---------------------------------------------------------------------------------------------------------------------
second_raw.tb[["TIMESTAMP"]][1]
second_raw.tb[["TIMESTAMP"]][nrow(second_raw.tb)]
tz(second_raw.tb[["TIMESTAMP"]][1])
second_raw.tb[["TIMESTAMP"]] <-  second_raw.tb[["TIMESTAMP"]] - hours(2) # UTC + 2h -> UTC
second_raw.tb[["TIMESTAMP"]][1]
second_raw.tb[["TIMESTAMP"]][nrow(second_raw.tb)]
tz(second_raw.tb[["TIMESTAMP"]][1])

head(second_raw.tb, 50)
tail(second_raw.tb, 50)



## ---------------------------------------------------------------------------------------------------------------------
  # NEW CR6 program
second_raw.tb |>
  filter(!is.na(TIMESTAMP)) |>
  rename(time = TIMESTAMP) |>
  mutate(red_umol = Red_Den_cal,
         far_red_umol = Far_red_Den_cal,
         PAR_diff_fr_raw = PAR_BF_diff/PAR_BF_tot,
         # PAR readings from BF5 are noisy and start being unreliable at
         # a relatively high diffuse irradiance of 10 umol m-2 s-1.
         PAR_diff_fr = ifelse(PAR_BF_diff < 10,
                              NA_real_,
                              PAR_diff_fr_raw),
         year = year(time),
         month_of_year = month(time),
         month_name = factor(month_of_year, levels = 12:1, labels = rev(month.name)),
         week_of_year = week(time),
         day_of_year = yday(time),
         time_of_day_utc = as_tod(time),
         solar_time_h = solar_time(time, 
                                   geocode = viikki_bio3.geo),
         sun_elevation = sun_elevation(time, 
                                       geocode = viikki_bio3.geo, 
                                       use.refraction = TRUE,
                                       tz = "UTC"),
         sun_azimuth = sun_azimuth(time, 
                                   geocode = viikki_bio3.geo, 
                                   use.refraction = TRUE,
                                   tz = "UTC"),
         PAR_diff_fr_rel = 1 - ((1 - PAR_diff_fr) / (1 - clear_sky_diff_fr(sun_elevation))),
         sunny = PAR_diff_fr_rel < 0.6,
         solar_disk = factor(sunny,
                             levels = c(FALSE, TRUE),
                             labels = c("occluded", "visible"))) -> second.tb

head(second.tb, 50)
tail(second.tb, 50)



## ---------------------------------------------------------------------------------------------------------------------
colnames(second.tb)
second.tb[1, ]


## ---------------------------------------------------------------------------------------------------------------------
series.name <- paste(year(second_raw.tb$TIMESTAMP[1]),
                  "_", month(second_raw.tb$TIMESTAMP[1]), 
                  "_", day(second_raw.tb$TIMESTAMP[1]), ".tb", sep = "")
obj.name <- paste("second_", series.name, sep = "")
assign(obj.name, second.tb)

head(second.tb, 50)
tail(second.tb, 50)

save(list = obj.name, file = paste("data-rda-partial/", obj.name, ".rda", sep = ""))


## ----eval=TRUE--------------------------------------------------------------------------------------------------------
# second.tb$time.minute <- floor_date(second.tb$time, unit = "minute")

second.tb |>
  mutate(time.minute = trunc(time, units = "mins") + minutes(1)) |> # match FMI timing
  group_by(time.minute) |>
  summarize(across(time, first, .names = "{.col}_start"),
            across(time, last, .names = "{.col}_end"),
            across(year:day_of_year, first, .names = "{.col}"),
            across(time_of_day_utc:sun_azimuth, median, .names = "{.col}"),
            across(PAR_Den:PAR_diff_fr, mean, .names = "{.col}"),
            across(PAR_diff_fr_rel:sunny, mean, .names = "{.col}"),
            sun_visible_fr = sum(solar_disk == "visible") / n(),
            solar_disk = ifelse(sun_visible_fr < 0.5, "occluded", "visible"),
            n = n(),
            incomplete.data = n < 120,
            bad.data = n < 100) |>
  mutate(solar_disk = factor(solar_disk)) |>
  ungroup() |>
  mutate(time.minute = as.POSIXct(time.minute)) |>
  rename(time = time.minute) -> minute.tb

print(minute.tb)
colnames(minute.tb)


## ----eval=TRUE--------------------------------------------------------------------------------------------------------
minute.tb %>%
  head()
minute.tb %>%
  tail()


## ----eval=TRUE--------------------------------------------------------------------------------------------------------
obj.name <- paste("minute_calc_", series.name, sep = "")
assign(obj.name, minute.tb)

save(list = obj.name, file = paste("data-rda-partial/", obj.name, ".rda", sep = ""))

