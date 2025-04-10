# Updated 2025-02-20 to use Version 0.0 and PRELIMINARY
# The main change is in the calculation used to guess if the sun was occluded or not

# Updated 2025-04-07 to use Version 0.1 and PRELIMINARY
# The main change is in the inclusion of the corrected rainfall data, previously set to NA
# Soil water content data is from August 1 onwards apparently wrong but not
# included in minute-interval data

library(dplyr)
library(photobiology)
library(lubridate)
library(readr) # Much faster and applies compression but triggers warnings.

rm(list = ls(pattern = "*")) # clear workspace

load("data-rda/minute_2024_latest.tb.rda")

minute_2024_latest.tb$series_start <- NULL
# minute_2015_latest.tb$was_sunny <- NULL
# minute_2024_latest.tb$rain_mm_h <- NA_real_ # some corrupted data in file

# attributes(minute_2015_2023.tb)
attr(minute_2024_latest.tb, "file.header") <- NULL
comment(minute_2024_latest.tb) <- NULL

data_start <- min(minute_2024_latest.tb$time)

data_end <- max(minute_2024_latest.tb$time)

for (year in year(data_start):year(data_end)) {
  name <- paste("Viikki-1min-", year, "-Version-0.1", sep = "")
  obj_name <- paste(gsub("-", "_", name), ".tb", sep = "")
  rda_name <- paste("data-osf/", name, ".rda", sep = "")
  csv_name <- paste("data-osf/", name, ".csv.gz", sep = "")
  met_name <- paste("data-osf/", name, ".met", sep = "")
  temp.tb <- filter(minute_2024_latest.tb, year(time) == year)
  na.cols <- sapply(temp.tb, function(x) {all(is.na(x))})
  comment(temp.tb) <-
    paste("This is PRELIMINARY release 0.1 of the 2024 data set and subject to future corrections.",
          "Known problems: Rain data not yet fully validated!",
          "Data acquired or computed by Pedro J. Aphalo.",
          "Data are provided as is, with no guarantee of suitability for any purpose.",
          "If used in publications cite based on DOI: 10.17605/OSF.IO/E4VAU",
          "University of Helsinki, Finland.",
          "License: CC BY-NC-SA Attribution-NonCommercial-ShareAlike 4.0 International",
          "See: https://creativecommons.org/licenses/by-nc-sa/4.0/",
          "Use in commercial AI training not allowed.",
          "Station information available at https://viikki-stn.r4photobiology.info/",
          paste("Period:", min(temp.tb$time), "to", max(temp.tb$time)),
          paste("Filename: ", name, ".rda", sep = ""),
          sep = "\n")
  assign(obj_name, temp.tb)
  save(list = obj_name, file = rda_name)
  write_csv(get(obj_name), file = csv_name, )
  # metadata
  metadata <- paste(comment(temp.tb),
                    "\nNumber of rows: ", nrow(temp.tb),
                    "\nNumber of variables (= columns): ", ncol(temp.tb),
                    "\nAll variables (consistent over whole time series):",
                    paste(colnames(temp.tb), collapse = "\n"),
                    paste("\nVariables with data (not all NA) for:", sum(!na.cols), "variables:"),
                    paste(colnames(temp.tb)[!na.cols], collapse = "\n"),
                    paste("\nVariables with no data (all NA) for:", sum(na.cols), "variables:"),
                    paste(colnames(temp.tb)[na.cols], collapse = "\n"),
                    sep = "\n")
  writeChar(metadata, met_name)
}

## Hourly data in two files
load("data-rda/hour_2024_latest.tb.rda")
# hour_2024_latest.tb$rain_mm_h <- NA_real_ # some corrupted data in file
hour_2024_latest.tb$VWC_5cm <-
  ifelse(hour_2024_latest.tb$time > ymd_hms("2024-07-31 23:59:59"),
         NA_real_,
         hour_2024_latest.tb$VWC_5cm)
hour_2024_latest.tb$VWC_10cm <-
  ifelse(hour_2024_latest.tb$time > ymd_hms("2024-07-31 23:59:59"),
         NA_real_,
         hour_2024_latest.tb$VWC_10cm)
hour_2024_latest.tb$VWC_20cm <-
  ifelse(hour_2024_latest.tb$time > ymd_hms("2024-07-31 23:59:59"),
         NA_real_,
         hour_2024_latest.tb$VWC_20cm)
hour_2024_latest.tb$VWC_20cm <-
  ifelse(hour_2024_latest.tb$time > ymd_hms("2024-07-31 23:59:59"),
         NA_real_,
         hour_2024_latest.tb$VWC_20cm)
hour_2024_latest.tb$VWC_30cm <-
  ifelse(hour_2024_latest.tb$time > ymd_hms("2024-07-31 23:59:59"),
         NA_real_,
         hour_2024_latest.tb$VWC_30cm)
hour_2024_latest.tb$VWC_40cm <-
  ifelse(hour_2024_latest.tb$time > ymd_hms("2024-07-31 23:59:59"),
         NA_real_,
         hour_2024_latest.tb$VWC_50cm)
hour_2024_latest.tb$VWC_20cm <-
  ifelse(hour_2024_latest.tb$time > ymd_hms("2024-07-31 23:59:59"),
         NA_real_,
         hour_2024_latest.tb$VWC_50cm)

hour_2024_latest.tb[ , which(grepl("^T107", colnames(hour_2024_latest.tb)))] <-
  NA_real_ # sensors no longer in use

Viikki_1h_2024_latest_Version_0.1.tb <- hour_2024_latest.tb
comment(Viikki_1h_2024_latest_Version_0.1.tb) <-
  paste("This is PRELIMINARY release 0.1 of the 2024 data set and subject to future corrections.",
        "Known problems: Soil water data are incomplete!",
        "Data acquired or computed by Pedro J. Aphalo.",
        "Data are provided as is, with no guarantee of suitability for any purpose.",
        "If used in publications cite based on DOI: 10.17605/OSF.IO/E4VAU.",
        "University of Helsinki, Finland.",
        "License: CC BY-NC-SA Attribution-NonCommercial-ShareAlike 4.0 International.",
        "See: https://creativecommons.org/licenses/by-nc-sa/4.0/",
        "Use in commercial AI training not allowed.",
        "Station information available at https://viikki-stn.r4photobiology.info/",
        paste("Period:", min(Viikki_1h_2024_latest_Version_0.1.tb$time),
              "to", max(Viikki_1h_2024_latest_Version_0.1.tb$time)),
        paste("Filename: ", "Viikki-1h-2024-latest-Version-0.1", ".rda", sep = ""),
        sep = "\n")

save(Viikki_1h_2024_latest_Version_0.1.tb,
     file = "data-osf/Viikki-1h-2024-latest-Version-0.1.rda")

write_csv(Viikki_1h_2024_latest_Version_0.1.tb,
          file =  "data-osf/Viikki-1h-2024-latest-Version-0.1.csv.gz")

metadata <- paste(comment(Viikki_1h_2024_latest_Version_0.1.tb),
                  "\nData rows: ", nrow(Viikki_1h_2024_latest_Version_0.1.tb),
                  "\nData columns: ", ncol(Viikki_1h_2024_latest_Version_0.1.tb),
                  "\nVariables:",
                  paste(colnames(Viikki_1h_2024_latest_Version_0.1.tb),
                        collapse = "\n"),
                  sep = "\n")
writeChar(metadata, "data-osf/Viikki-1h-2024-latest-Version-0.1.met")
