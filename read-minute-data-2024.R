## ----setup, include=FALSE---------------------------------------------------------------------------------------
knitr::opts_chunk$set(tidy = FALSE, echo = TRUE)


## ---------------------------------------------------------------------------------------------------------------
library(readr)
library(lubridate)
library(photobiology)
library(photobiologyInOut)
library(dplyr)


## ---------------------------------------------------------------------------------------------------------------
minute_raw.tb <- read_csi_dat(file = "data-latest/Viikki Tower_TableMinute.dat", locale = locale_UTC)

minute_raw.tb
nrow(minute_raw.tb)
ncol(minute_raw.tb)


## ---------------------------------------------------------------------------------------------------------------
colnames(minute_raw.tb)


## ---------------------------------------------------------------------------------------------------------------
cat(comment(minute_raw.tb))


## ---------------------------------------------------------------------------------------------------------------
rle.check <- rle(sort(as.numeric(minute_raw.tb$TIMESTAMP)))
duplicates <- sum(rle.check$lengths > 1)
if (duplicates > 0L) {
  message("Found ", duplicates, " duplicated rows in data.")
  minute_raw.tb <- distinct(minute_raw.tb, TIMESTAMP, .keep_all = TRUE)
}


## ---------------------------------------------------------------------------------------------------------------
minute_raw.tb[["TIMESTAMP"]][1]
minute_raw.tb[["TIMESTAMP"]][nrow(minute_raw.tb)]
tz(minute_raw.tb[["TIMESTAMP"]][1])
minute_raw.tb[["TIMESTAMP"]] <-  minute_raw.tb[["TIMESTAMP"]] - hours(2) # EET -> UTC

minute_raw.tb[["TIMESTAMP"]][1]
minute_raw.tb[["TIMESTAMP"]][nrow(minute_raw.tb)]
tz(minute_raw.tb[["TIMESTAMP"]][1])


## ---------------------------------------------------------------------------------------------------------------
load("data-rda/minute_calc_2024_5_14.tb.rda")
names(minute_raw.tb) <- gsub("_Avg$", "", names(minute_raw.tb))
minute.tb <- full_join(minute_calc_2024_5_14.tb, minute_raw.tb, by = c("time.minute" = "TIMESTAMP"))
colnames(minute.tb)


## ---------------------------------------------------------------------------------------------------------------
obj.name <- paste("minute_", year(minute_raw.tb$TIMESTAMP[1]),
                  "_", month(minute_raw.tb$TIMESTAMP[1]), ".tb", sep = "")
assign(obj.name, minute.tb)

save(list = obj.name, file = paste("data-rda/", obj.name, ".rda", sep = ""))


## ---------------------------------------------------------------------------------------------------------------
minute.tb %>%
  head()
minute.tb %>%
  tail()

