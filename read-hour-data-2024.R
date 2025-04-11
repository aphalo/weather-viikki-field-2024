## ---------------------------------------------------------------------------------------------------------------------
rm(list = ls(pattern = "*"))


## ----setup, include=FALSE---------------------------------------------------------------------------------------------
knitr::opts_chunk$set(tidy = FALSE, echo = TRUE)


## ---------------------------------------------------------------------------------------------------------------------
library(readr)
library(lubridate)
library(photobiology)
library(photobiologyInOut)
library(dplyr)


## ---------------------------------------------------------------------------------------------------------------------
viikki_bio3.geo <- data.frame(lon = 25.019212, lat = 60.226805, address = "BIO3, Viikki")
locale_UTC <- locale(tz = "UTC")
# locale_FI <- locale(tz = "Europe/Helsinki")


## ---------------------------------------------------------------------------------------------------------------------
# hour_raw.tb <- read_csi_dat(file = "data-latest/Viikki Tower_TableHour.dat", locale = locale_UTC)
hour_raw.tb <- read_csi_dat(file = "data-logged/data-2025-01-14/Viikki Tower_TableHour.dat", locale = locale_UTC)

nrow(hour_raw.tb)
ncol(hour_raw.tb)
hour_raw.tb$TIMESTAMP[1]
hour_raw.tb$TIMESTAMP[nrow(hour_raw.tb)]


## ---------------------------------------------------------------------------------------------------------------------
colnames(hour_raw.tb)


## ---------------------------------------------------------------------------------------------------------------------
tail(hour_raw.tb$TIMESTAMP)
sum(hour_raw.tb$BattV_Min < 12)
hour_raw.tb$TIMESTAMP[which(hour_raw.tb$BattV_Min < 12)]


## ---------------------------------------------------------------------------------------------------------------------
tail(hour_raw.tb$TIMESTAMP)
sum(hour_raw.tb$BattV_Max < 12)


## ---------------------------------------------------------------------------------------------------------------------
cat(comment(hour_raw.tb))


## ---------------------------------------------------------------------------------------------------------------------
rle.check <- rle(sort(as.numeric(hour_raw.tb$TIMESTAMP)))
duplicates <- sum(rle.check$lengths > 1)
if (duplicates > 0L) {
  message("Found ", duplicates, " duplicated rows in data.")
  hour_raw.tb <- distinct(hour_raw.tb, TIMESTAMP, .keep_all = TRUE)
}


## ---------------------------------------------------------------------------------------------------------------------
hour_raw.tb[["TIMESTAMP"]][1]
hour_raw.tb[["TIMESTAMP"]][nrow(hour_raw.tb)]
tz(hour_raw.tb[["TIMESTAMP"]][1])
hour_raw.tb[["TIMESTAMP"]] <-  hour_raw.tb[["TIMESTAMP"]] - hours(2) # UTC + 2 -> UTC 
hour_raw.tb[["TIMESTAMP"]][1]
hour_raw.tb[["TIMESTAMP"]][nrow(hour_raw.tb)]
tz(hour_raw.tb[["TIMESTAMP"]][1])


## ---------------------------------------------------------------------------------------------------------------------
hour_raw.tb %>%
  mutate(year = year(TIMESTAMP),
         month_of_year = month(TIMESTAMP),
         month_name = factor(month_of_year, levels = 12:1, labels = rev(month.name)),
         week_of_year = week(TIMESTAMP),
         day_of_year = yday(TIMESTAMP),
         time_of_day = as_tod(TIMESTAMP)) -> hour.tb
nrow(hour.tb)


## ---------------------------------------------------------------------------------------------------------------------
obj.name <- paste("hour_", year(hour_raw.tb$TIMESTAMP[1]),
                  "_", month(hour_raw.tb$TIMESTAMP[1]), 
                  "_", day(hour_raw.tb$TIMESTAMP[1]), ".tb", sep = "")
assign(obj.name, hour.tb)

save(list = obj.name, file = paste("data-rda-partial/", obj.name, ".rda", sep = ""))


## ---------------------------------------------------------------------------------------------------------------------
head(hour.tb)
tail(hour.tb)

