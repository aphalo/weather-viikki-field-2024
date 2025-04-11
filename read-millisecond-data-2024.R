## ----------------------------------------------------------------------------------------------------------------------
rm(list = ls(pattern = "*"))


## ----setup, include=FALSE----------------------------------------------------------------------------------------------
knitr::opts_chunk$set(tidy = FALSE, echo = TRUE)
make.plots <- FALSE # make plots or skip them


## ----------------------------------------------------------------------------------------------------------------------
library(readr)
library(lubridate)
library(photobiology)
library(photobiologyInOut)
library(ggspectra)
library(ggdensity)
library(lubridate)
library(dplyr)
library(ggpp)


## ----------------------------------------------------------------------------------------------------------------------
viikki_bio3.geo <- data.frame(lon = 25.019212, lat = 60.226805, address = "BIO3, Viikki")
locale_UTC <- locale(tz = "UTC")


## ----------------------------------------------------------------------------------------------------------------------
# read previous
# millisecond_raw.tb <- read_csi_dat(file = "data-latest/Viikki Tower_TableMilliSecond.dat.backup", locale = locale_UTC)

# read current
millisecond_raw.tb <- read_csi_dat(file = "data-latest/Viikki Tower_TableMilliSecond.dat", locale = locale_UTC)

millisecond_raw.tb
nrow(millisecond_raw.tb)
ncol(millisecond_raw.tb)

range(millisecond_raw.tb$TIMESTAMP)


## ----------------------------------------------------------------------------------------------------------------------
colnames(millisecond_raw.tb)


## ----------------------------------------------------------------------------------------------------------------------
cat(comment(millisecond_raw.tb))


## ----------------------------------------------------------------------------------------------------------------------
rle.check <- rle(sort(as.numeric(millisecond_raw.tb$TIMESTAMP)))
duplicates <- sum(rle.check$lengths > 1)
if (duplicates > 0L) {
  message("Found ", duplicates, " duplicated rows in data.")
  millisecond_raw.tb <- distinct(millisecond_raw.tb, TIMESTAMP, .keep_all = TRUE)
}


## ----------------------------------------------------------------------------------------------------------------------
millisecond_raw.tb[["TIMESTAMP"]][1]
millisecond_raw.tb[["TIMESTAMP"]][nrow(millisecond_raw.tb)]
tz(millisecond_raw.tb[["TIMESTAMP"]][1])
millisecond_raw.tb[["TIMESTAMP_UTC"]] <-  millisecond_raw.tb[["TIMESTAMP"]] - hours(2) # UTC + 2h -> UTC
millisecond_raw.tb[["TIMESTAMP_UTC"]][1]
millisecond_raw.tb[["TIMESTAMP_UTC"]][nrow(millisecond_raw.tb)]
tz(millisecond_raw.tb[["TIMESTAMP_UTC"]][1])


## ----------------------------------------------------------------------------------------------------------------------
  # NEW CR6 program
millisecond_raw.tb |>
  select(-RECORD) |>
  filter(!is.na(TIMESTAMP)) |>
  rename(time = TIMESTAMP_UTC,
         time_utcp2h = TIMESTAMP
         ) |>
  mutate(year = year(time),
         month_of_year = month(time),
         month_name = factor(month_of_year, levels = 12:1, labels = rev(month.name)),
         month_of_year_utcp2h = month(time_utcp2h),
         month_name_utcp2h = factor(month_of_year_utcp2h, levels = 12:1, labels = rev(month.name)),
         week_of_year = week(time),
         week_of_year_utcp2h = week(time_utcp2h),
         day_of_year = yday(time),
         day_of_year_utcp2h = yday(time_utcp2h),
         time_of_day_utc = as_tod(time),
         time_of_day_utcp2h = as_tod(time_utcp2h),
         solar_time_h = solar_time(time, 
                                   geocode = viikki_bio3.geo),
         solar_time_s = solar_time(time, 
                                   geocode = viikki_bio3.geo, 
                                   unit.out = "seconds"),
         sun_elevation = sun_elevation(time, 
                                       geocode = viikki_bio3.geo, 
                                       use.refraction = TRUE,
                                       tz = "UTC"),
         sun_azimuth = sun_azimuth(time, 
                                   geocode = viikki_bio3.geo, 
                                   use.refraction = TRUE,
                                   tz = "UTC")) -> millisecond.tb


## ----------------------------------------------------------------------------------------------------------------------
# first look

millisecond.tb |>
  head()
millisecond.tb |>
  tail()


## ----------------------------------------------------------------------------------------------------------------------
# summaries for manual checks
colnames(millisecond.tb)
millisecond.tb |>
  summarise(across(PAR_Den_CS:sun_azimuth, function(x) {class(x)[[1]]},  .names = "{.col}_cls"))

millisecond.tb |>
  summarise(across(PAR_Den_CS:sun_azimuth, function(x) {sum(is.na(x))},  .names = "{.col}_cls")) |>
  t() |> max()
  
millisecond.tb |> na.omit() -> millisecond.tb

millisecond.tb |>
  reframe(across(PAR_Den_CS:month_of_year, range, .names = "{.col}"),
          across(week_of_year_utcp2h, range, .names = "{.col}"),
          across(week_of_year:sun_azimuth, range, .names = "{.col}"))

millisecond.tb |>
  reframe(across(PAR_Den_CS:month_of_year, range, .names = "{.col}"),
          across(week_of_year_utcp2h, range, .names = "{.col}"),
          across(week_of_year:sun_azimuth, range, .names = "{.col}"),
          n = n(),
          .by = month_name)



## ----------------------------------------------------------------------------------------------------------------------
millisecond.tb |>
    mutate(f_in_the_open = (time > ymd_hm("2024-08-20 13:32") & time < ymd_hm("2024-08-20 14:15")) |
           (time > ymd_hm("2024-08-25 11:10") & time < ymd_hm("2024-08-20 11:24")) |
           (time > ymd_hm("2024-09-02 12:05") & time < ymd_hm("2024-09-02 12:22")) |
           (time > ymd_hm("2024-10-01 0:0")),
           f_in_canopy = !f_in_the_open & 
             (time > ymd_hm("2024-08-10 0:0") & time < ymd_hm("2024-09-30 0:0")),
           f_unknown = !xor(f_in_the_open, f_in_canopy) # after end of experiment possibly dirty or under snow.
    ) -> millisecond.tb

# we use rle for performance
find_time_slices <- function(x) {
  x <- as.numeric(x) # convert time to running seconds
  boundary <- c(TRUE, diff(x) > 1) # time gaps > 1 second
  # length of gaps should be always == 1 with TRUE indicating first row of each time slice, but we do not assume this
  # length of slices could vary because of failures to measure, although unlikely
  boundary_rle <- rle(boundary)
  cat("length in: ",  length(x), "\n",
      "RLE sum lengths: ", sum(boundary_rle$lengths), "\n")
  # RLE computes these lengths, remove the gaps and add length of gap to the length of each slice
  boundary_rle$lengths <- boundary_rle$lengths[!boundary_rle$value] + 
    boundary_rle$lengths[boundary_rle$value]
  # we replace the logical values with running numbers
  boundary_rle$values <- 1:length(boundary_rle$lengths)
  cat("RLE edited sum lengths: ", sum(boundary_rle$lengths), "\n")
  cat("Number of slices:", length(boundary_rle$lengths), "\n")
  cat("Values per slice in range:", range(boundary_rle$lengths), "\n")
  cat("Number of short slices:", sum(boundary_rle$lengths < 0.9 * max(boundary_rle$lengths)), "\n")
  # expand the RLE obtaining one distinct index for each slice
  inverse.rle(boundary_rle)
}

millisecond.tb |>
    mutate(time_slice = find_time_slices(time) # time gaps > 1 second start new slice
           ) -> millisecond.tb

# irradiance values acquired per sensor in millions
format(nrow(millisecond.tb), big.mark = ",")
format(sum(millisecond.tb$time_slice & millisecond.tb$f_in_the_open), big.mark = ",")
format(sum(millisecond.tb$time_slice & millisecond.tb$f_in_canopy), big.mark = ",")
format(sum(millisecond.tb$time_slice & millisecond.tb$f_unknown), big.mark = ",")

# number of time slices with measurements
length(unique(millisecond.tb[millisecond.tb$f_in_the_open, ]$time_slice))
length(unique(millisecond.tb[millisecond.tb$f_in_canopy, ]$time_slice))
length(unique(millisecond.tb[millisecond.tb$f_unknown, ]$time_slice))

range(rle(millisecond.tb[millisecond.tb$f_in_canopy , ]$time_slice)$lengths)
range(rle(millisecond.tb[millisecond.tb$f_in_the_open , ]$time_slice)$lengths)

length(unique(millisecond.tb$time_slice)) == max(millisecond.tb$time_slice)


## ----------------------------------------------------------------------------------------------------------------------
colnames(millisecond.tb)

series.name <- paste(year(millisecond_raw.tb$TIMESTAMP[1]),
                  "_", month(millisecond_raw.tb$TIMESTAMP[1]), 
                  "_", day(millisecond_raw.tb$TIMESTAMP[1]), ".tb", sep = "")
obj.name <- paste("millisecond_", series.name, sep = "")

assign(obj.name, millisecond.tb)

save(list = obj.name, file = paste("data-rda-partial/", obj.name, ".rda", sep = ""))


## ----eval=TRUE---------------------------------------------------------------------------------------------------------
millisecond.tb |>
  group_by(time_slice) |>
  summarize(across(f_in_the_open:f_unknown, first, .names = "{.col}"),
            across(time, first, .names = "{.col}_start_utc"),
            across(time, last, .names = "{.col}_end_utc"),
            time_step = max(median(as.numeric(diff(time))) * 1e3), # in ms
            across(year:day_of_year_utcp2h, first, .names = "{.col}"),
            across(time_of_day_utc:sun_azimuth, median, .names = "{.col}"),
            across(PAR_Den_CS:Greenf_Den, mean, .names = "{.col}"),
            n = n()) |>
  ungroup() |>
# logging depends on two conditions: time and PAR > 1 umol
# late and early in the day PAR varies around 1 umol and part of the slice is skipped
  mutate(slice_duration = time_end_utc - time_start_utc,
         good_slice = slice_duration > 290 & # seconds
           slice_duration < 310 & # seconds
           round(time_step) <= 51 & # milliseconds, no skips
           n > 5000) -> slice_avg.tb

nrow(slice_avg.tb)
colnames(slice_avg.tb)

sum(!slice_avg.tb$good_slice)
sum(slice_avg.tb$good_slice)
sum(slice_avg.tb$good_slice & slice_avg.tb$f_in_canopy)
sum(slice_avg.tb$good_slice & slice_avg.tb$f_in_the_open)
sum(slice_avg.tb$good_slice & slice_avg.tb$f_unknown)

range(slice_avg.tb$time_step)
range(slice_avg.tb$n)
range(slice_avg.tb$slice_duration)
sum(slice_avg.tb$n > 5000) # expected n = 6000 during 5 min at 50 ms
sum(slice_avg.tb$n <= 5000)
sum(slice_avg.tb$slice_duration < seconds(240)) # slices < 4 min
sum(slice_avg.tb$slice_duration < seconds(299)) # slices < 4 min 59 s
sum(slice_avg.tb$slice_duration > seconds(301)) # slices > 5 min 1 s

## ----eval=TRUE---------------------------------------------------------------------------------------------------------
obj.name <- paste("slice_calc_", series.name, sep = "")
assign(obj.name, slice_avg.tb)

save(list = obj.name, file = paste("data-rda-partial/", obj.name, ".rda", sep = ""))


## ----eval=TRUE---------------------------------------------------------------------------------------------------------
time_step <- mean(as.numeric(diff(millisecond.tb$time[1:10]))) # in seconds
values_second <- round(1/time_step, 0)
values_second_threshold <- values_second * 3 / 4
  
millisecond.tb |>
  mutate(time.second = trunc(time, units = "secs") + seconds(1)) |> # match FMI timing
  group_by(time.second) |>
  summarize(across(time, first, .names = "{.col}_start_utc"),
            # across(time, last, .names = "{.col}_end_utc"),
            across(year:day_of_year_utcp2h, first, .names = "{.col}"),
            across(time_of_day_utc:sun_azimuth, median, .names = "{.col}"),
            across(PAR_Den_CS:Greenf_Den, mean, .names = "{.col}"),
            n = n(),
            incomplete.data = n < values_second,
            bad.data = n < values_second_threshold) |>
  ungroup() |>
  mutate(time = as.POSIXct(time.second)) |>
  select(-time.second) -> second_avg.tb

print(second_avg.tb)
colnames(second_avg.tb)


## ----eval=TRUE---------------------------------------------------------------------------------------------------------
second_avg.tb |>
  head()
second_avg.tb |>
  tail()


## ----eval=TRUE---------------------------------------------------------------------------------------------------------
obj.name <- paste("second_calc_", series.name, sep = "")
assign(obj.name, second_avg.tb)

save(list = obj.name, file = paste("data-rda-partial/", obj.name, ".rda", sep = ""))


## ----eval=TRUE---------------------------------------------------------------------------------------------------------
values_minute <- values_second * 60
values_minute_threshold <- values_minute * 3 / 4

millisecond.tb |>
  mutate(time.minute = trunc(time, units = "mins") + minutes(1)) |> # match FMI timing
  group_by(time.minute) |>
  summarize(across(time, first, .names = "{.col}_start_utc"),
            # across(time, last, .names = "{.col}_end_utc"),
            across(year:day_of_year, first, .names = "{.col}"),
            across(time_of_day_utc:sun_azimuth, median, .names = "{.col}"),
            across(PAR_Den_CS:Greenf_Den, mean, .names = "{.col}"),
            n = n(),
            incomplete.data = n < values_minute,
            bad.data = n < values_minute_threshold) |>
  ungroup() |>
  mutate(time = as.POSIXct(time.minute)) |>
  select(-time.minute) -> minute_avg.tb

print(minute_avg.tb)
colnames(minute_avg.tb)


## ----eval=TRUE---------------------------------------------------------------------------------------------------------
nrow(minute_avg.tb)

sum(minute_avg.tb$bad.data)
sum(minute_avg.tb$incomplete.data)

minute_avg.tb |>
  head()
minute_avg.tb |>
  tail()


## ----eval=TRUE---------------------------------------------------------------------------------------------------------
obj.name <- paste("minute_fast_calc_", series.name, sep = "")
assign(obj.name, minute_avg.tb)

save(list = obj.name, file = paste("data-rda-partial/", obj.name, ".rda", sep = ""))

