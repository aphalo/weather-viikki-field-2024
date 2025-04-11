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
minute_raw.tb <- read_csi_dat(file = "data-latest/Viikki Tower_TableMinute.dat", locale = locale_UTC)
# minute_raw.tb <- read_csi_dat(file = "data-logged/data-2025-01-14/Viikki Tower_TableMinute.dat", locale = locale_UTC)

minute_raw.tb
nrow(minute_raw.tb)
ncol(minute_raw.tb)

range(minute_raw.tb$TIMESTAMP)


## ---------------------------------------------------------------------------------------------------------------------
colnames(minute_raw.tb)


## ---------------------------------------------------------------------------------------------------------------------
cat(comment(minute_raw.tb))


## ---------------------------------------------------------------------------------------------------------------------
rle.check <- rle(sort(as.numeric(minute_raw.tb$TIMESTAMP)))
duplicates <- sum(rle.check$lengths > 1)
if (duplicates > 0L) {
  message("Found ", duplicates, " duplicated rows in data.")
  minute_raw.tb <- distinct(minute_raw.tb, TIMESTAMP, .keep_all = TRUE)
}


## ---------------------------------------------------------------------------------------------------------------------
minute_raw.tb[["TIMESTAMP"]][1]
minute_raw.tb[["TIMESTAMP"]][nrow(minute_raw.tb)]
tz(minute_raw.tb[["TIMESTAMP"]][1])
minute_raw.tb[["TIMESTAMP"]] <-  minute_raw.tb[["TIMESTAMP"]] - hours(2) # EET -> UTC

minute_raw.tb[["TIMESTAMP"]][1]
minute_raw.tb[["TIMESTAMP"]][nrow(minute_raw.tb)]
tz(minute_raw.tb[["TIMESTAMP"]][1])


## ---------------------------------------------------------------------------------------------------------------------
series.name <- paste(year(minute_raw.tb$TIMESTAMP[1]),
                  "_", month(minute_raw.tb$TIMESTAMP[1]), 
                  "_", day(minute_raw.tb$TIMESTAMP[1]), sep = "")
obj.name <- paste("minute_", series.name, ".tb", sep = "")
calc.obj.name <- paste("minute_calc_", series.name, ".tb", sep = "") 


## ---------------------------------------------------------------------------------------------------------------------
load(paste("data-rda-partial/", calc.obj.name, ".rda", sep = ""))
minute_calc.tb <- get(calc.obj.name)
names(minute_raw.tb) <- gsub("_Avg$", "", names(minute_raw.tb))

minute.tb <- full_join(minute_calc.tb, minute_raw.tb, by = c("time" = "TIMESTAMP"))
minute.tb <- minute.tb[order(minute.tb$time), ] # ensure data are ordered
colnames(minute.tb)


## ---------------------------------------------------------------------------------------------------------------------
assign(obj.name, minute.tb)

save(list = obj.name, file = paste("data-rda-partial/", obj.name, ".rda", sep = ""))


## ---------------------------------------------------------------------------------------------------------------------
minute.tb %>%
  head()
minute.tb %>%
  tail()

