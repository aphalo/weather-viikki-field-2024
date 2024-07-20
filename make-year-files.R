# Updated 2024-07-16 to use Version 1 instead of PRELIMINARY
# The main change is in the calculation used to guess if the sun was occluded or not

library(dplyr)
library(photobiology)
library(lubridate)
library(readr) # Much faster and applies compression but triggers warnings.

rm(list = ls(pattern = "*")) # clear workspace

load("data-rda/minute_2015_latest.tb.rda")

minute_2015_latest.tb$series_start <- NULL
# minute_2015_latest.tb$was_sunny <- NULL

# attributes(minute_2015_2023.tb)
attr(minute_2015_latest.tb, "file.header") <- NULL
comment(minute_2015_latest.tb) <- NULL

data_start <- min(minute_2015_latest.tb$time)

data_end <- max(minute_2015_latest.tb$time)

for (year in year(data_start):year(data_end)) {
  name <- paste("Viikki-1min-", year, "-Version-1.0", sep = "")
  obj_name <- paste(gsub("-", "_", name), ".tb", sep = "")
  rda_name <- paste("data-osf/", name, ".rda", sep = "")
  csv_name <- paste("data-osf/", name, ".csv.gz", sep = "")
  met_name <- paste("data-osf/", name, ".met", sep = "")
  temp.tb <- filter(minute_2015_latest.tb, year(time) == year)
  na.cols <- sapply(temp.tb, function(x) {all(is.na(x))})
  comment(temp.tb) <-
    paste("Data acquired or computed by Pedro J. Aphalo",
          "Data are provided as is, with no guaranee of suitability for any purpose",
          "If used in publications cite based on DOI",
          "Faculty of Biological and Environmental Sciences, University of Helsinki, Finland",
          "License: CC BY-NC-SA Attribution-NonCommercial-ShareAlike 4.0 International",
          "See: https://creativecommons.org/licenses/by-nc-sa/4.0/",
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
load("data-rda/hour_soil_calc_2015_latest.tb.rda")

Viikki_1h_2017_latest_Version_1.0.tb <- hour_soil_calc_2015_latest.tb
comment(Viikki_1h_2017_latest_Version_1.0.tb) <-
  paste("Data acquired or computed by Pedro J. Aphalo",
        "Data are provided as is, with no guaranee of suitability for any purpose",
        "If used in publications cite based on DOI",
        "Faculty of Biological and Environmental Sciences, University of Helsinki, Finland",
        "License: CC BY-NC-SA Attribution-NonCommercial-ShareAlike 4.0 International",
        "See: https://creativecommons.org/licenses/by-nc-sa/4.0/",
        "Station information available at https://viikki-stn.r4photobiology.info/",
        paste("Period:", min(Viikki_1h_2017_latest_Version_1.0.tb$time),
              "to", max(Viikki_1h_2017_latest_Version_1.0.tb$time)),
        paste("Filename: ", "Viikki-1h-2017-latest-Version-1.0", ".rda", sep = ""),
        sep = "\n")

save(Viikki_1h_2017_latest_Version_1.0.tb,
     file = "data-osf/Viikki-1h-2017-latest-Version-1.0.rda")

write_csv(Viikki_1h_2017_latest_Version_1.0.tb,
          file =  "data-osf/Viikki-1h-2017-latest-Version-1.0.csv.gz")

metadata <- paste(comment(Viikki_1h_2017_latest_Version_1.0.tb),
                  "\nData rows: ", nrow(Viikki_1h_2017_latest_Version_1.0.tb),
                  "\nData columns: ", ncol(Viikki_1h_2017_latest_Version_1.0.tb),
                  "\nVariables:",
                  paste(colnames(Viikki_1h_2017_latest_Version_1.0.tb),
                        collapse = "\n"),
                  sep = "\n")
writeChar(metadata, "data-osf/Viikki-1h-2017-latest-Version-1.0.met")
