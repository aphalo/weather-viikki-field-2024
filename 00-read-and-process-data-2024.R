# This is a master script calling 7 separate scripts
#
# Data are logged only at one time-step, and for those data logged frequently,
# slower time series are created in the scripts.
#
# Values related to the solar position are computed also in these scripts based
# on time and geographic coordinates. Reference evapotranspiration is
# computed in the script, from weather data, for each minute and averaged or
# accumulated.
#
# Each script saves its output to a file on disk. These files are overwritten
# each time the scripts are run.
#
# To plot daily summaries, the R markdown file 'day-avg-water-et-plots-2024.Rmd'
# can be rendered to create a report.

# ---
# R code is extracted from the R markdown files used to read the raw data,
# only when needed (R file missing or out-of-date compared to Rmd file).

# The scripts expect the files with raw data from the logger to be in folder
# "data-latest" within the current folder!!
# This is checked and an error is raised if any of the input files are missing.

# The folder used to save internediate results is created if it does not exist.

# Loading and processing the data logged at 0.5 s intervals takes several
# minutes in a reasonably fast PC. Data are about 1 GB per month. R may use
# something like 4 GB RAM for a file this large. (I will need to think a strategy
# for handling this as the data set will keep growing).

library(knitr)

merge_only <- TRUE  # merge files already in ./data-rda-partial
# merge_only <- FALSE # read latest from ./data-logged before merging

# knit scripts if needed

if (!file.exists("read-millisecond-data-2024.R") ||
    file.mtime("read-millisecond-data-2024.Rmd") >
    file.mtime("read-millisecond-data-2024.R")) {
  purl("read-millisecond-data-2024.Rmd")
}

if (!file.exists("read-second-data-2024.R") ||
    file.mtime("read-second-data-2024.Rmd") >
    file.mtime("read-second-data-2024.R")) {
  purl("read-second-data-2024.Rmd")
}

if (!file.exists("read-minute-data-2024.R") ||
    file.mtime("read-minute-data-2024.Rmd") >
    file.mtime("read-minute-data-2024.R")) {
  purl("read-minute-data-2024.Rmd")
}

if (!file.exists("read-hour-data-2024.R") ||
    file.mtime("read-hour-data-2024.Rmd") >
    file.mtime("read-hour-data-2024.R")) {
  purl("read-hour-data-2024.Rmd")
}

# check that scripts to be sourced can be found

input_scripts <- list.files(".", pattern = "\\.R$")
required_scripts <-
  c("read-millisecond-data-2024.R", "merge-millisecond-data-2024.R",
    "read-second-data-2024.R", "merge-second-data-2024.R",
    "read-minute-data-2024.R", "merge-minute-data-2024.R",
    "read-hour-data-2024.R", "merge-hour-data-2024.R")

if (!all(required_scripts %in% input_scripts)) {
  missing_scrips <- setdiff(required_scripts, input_scripts)
  stop(length(missing_scrips), "/", length(required_scripts),
       " scripts(s) missing in '.': ",
       paste(missing_scrips, collapse = ", "))
}

# check that folders exist and create them if missing

if (!dir.exists("./data-rda-partial")) {
  message("Creating folder './data-rda-partial'")
  dir.create("./data-rda-partial")
}

if (!dir.exists("./data-rda")) {
  message("Creating folder './data-rda'")
  dir.create("./data-rda")
}

if (!dir.exists("./data-latest")) {
  message("Creating folder './data-latest'")
  dir.create("./data-latest")
}

if (!dir.exists("./data-osf")) {
  message("Creating folder './data-osf'")
  dir.create("./data-osf")
}

# check for missing data files

input_files <- list.files("./data-latest", pattern = "\\.dat$")
required_files <- paste("Viikki Tower",
                        c("TableSecond.dat",
                          "TableMinute.dat",
                          "TableHour.dat"), sep = "_")
if (!all(required_files %in% input_files)) {
  missing_files <- setdiff(required_files, input_files)
  stop(length(missing_files), " file(s) missing in './data-latest': ",
       paste(missing_files, collapse = ", "))
}

  # if (!file.exists("read-millisecond-data-2024.R") ||
  #     file.mtime("read-millisecond-data-2024.Rmd") >
  #     file.mtime("read-millisecond-data-2024.R")) {
  #   purl("read-millisecond-data-2024.Rmd")
  # }
  # message("Reading 50 milliseconds frequency data")
  # source("read-millisecond-data-2024.R")
  # message("Processing 1/2 second frequency data")
  # source("merge-second-data-2024.R")

if (!merge_only) {
  message("Reading 50 ms frequency data")
  source("read-millisecond-data-2024.R")
}
message("Processing 50 ms frequency data")
source("merge-millisecond-data-2024.R")

if (!merge_only) {
  message("Reading 500 ms frequency data")
  source("read-second-data-2024.R")
}
message("Processing 500 ms frequency data")
source("merge-second-data-2024.R")

if (!merge_only) {
  message("Reading 1 minute frequency data")
  source("read-minute-data-2024.R")
}
message("Processing 1 minute frequency data")
source("merge-minute-data-2024.R")

if (!merge_only) {
  message("Reading 1 hour frequency data")
  source("read-hour-data-2024.R")
}
message("Processing 1 hour frequency data")
source("merge-hour-data-2024.R")

# the input values are computed from 1 hour frequency ones!
message("Processing 1 day frequency data")
source("merge-day-data-2024.R")

message("Done!")

files <- list.files("./data-rda", ".*2024_latest\\.tb\\.rda", full.names = TRUE)
message("Data files in './data-rda': ", paste(files, collapse = ", "))
file.info(files)

