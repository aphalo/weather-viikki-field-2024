library(dplyr)
library(lubridate)
library(photobiology)
library(photobiologyWavebands)

# clean up
rm(list = ls(pattern = "*"))
gc()

# First batch with logger program TowerViikki-024-05-fast

load("data-rda-partial/millisecond_2024_5_14.tb.rda")
colnames(millisecond_2024_5_14.tb)
nrow(millisecond_2024_5_14.tb)
millisecond_2024_5_14.tb |>
  rename(time = TIMESTAMP) |>
  mutate(series_start = time[1],
         PAR_umol_CS = PAR_Den_CS / 0.946 - 1.057, # -0 - 50
         bluef_umol = NA_real_,
         bluef_sellaro_umol = NA_real_,
         greenf_umol = NA_real_,
         greenf_sellaro_umol = NA_real_,
         UVAf_umol = NA_real_,
         UVBf_umol = NA_real_,
         UVA1f_umol = NA_real_,
         UVA2f_umol = NA_real_,
         UVA1fc_umol = NA_real_,
         UVA2fc_umol = NA_real_
  ) |>
  select(series_start,
         time,
         day_of_year,
         month_of_year,
         month_name,
         calendar_year = year,
         time_of_day_utc,
         solar_time_h,
         sun_elevation,
         sun_azimuth,
         PAR_umol_CS,
         greenf_umol,
         greenf_sellaro_umol,
         bluef_umol,
         bluef_sellaro_umol,
         UVA1f_umol,
         UVAf_umol,
         UVA2f_umol,
         UVBf_umol,
         UVA1fc_umol,
         UVA2fc_umol) -> millisecond_2024_5_14x.tb

# second batch with logger program TowerViikki-024-08-fast
# This added five sglux fast sensors, and their data to this table
# frequency remained unchanged.

load("data-rda-partial/millisecond_2024_8_9.tb.rda")
colnames(millisecond_2024_8_9.tb)
nrow(millisecond_2024_8_9.tb)
millisecond_2024_8_9.tb |>
  rename(time = TIMESTAMP) |>
  mutate(series_start = time[1],
         PAR_umol_CS = PAR_Den_CS / 0.946 - 1.057, # -0 - 50
         greenf_umol = Greenf_Den - 7.159275e-02,
         greenf_sellaro_umol = Greenf_Den - 7.159275e-02,
         bluef_umol = Bluef_Den / 1.4010 - 8.346781e-01,
         bluef_sellaro_umol = Bluef_Den / 1.9365 - 6.038647e-01,
         UVA1f_umol = UVA1f_Den / 6.4374 - 8.985690e-03,
         UVAf_umol = UVAf_Den / 4.3838 - 1.740200e-01,
         UVA2f_umol = UVAf_Den / 24.312 - 3.137829e-02,
         UVBf_umol = UVBf_Den / 273.14 - 1.870687e-04,
         UVA1fc_umol = 0.73747 * UVAf_umol + 0.03433 * bluef_sellaro_umol,
         UVA2fc_umol = 0.157966 * UVAf_umol + 1.364312 * UVBf_umol
  ) |>
  select(series_start,
         time,
         day_of_year,
         month_of_year,
         month_name,
         calendar_year = year,
         time_of_day_utc,
         solar_time_h,
         sun_elevation,
         sun_azimuth,
         PAR_umol_CS,
         greenf_umol,
         greenf_sellaro_umol,
         bluef_umol,
         bluef_sellaro_umol,
         UVA1f_umol,
         UVAf_umol,
         UVA2f_umol,
         UVBf_umol,
         UVA1fc_umol,
         UVA2fc_umol) -> millisecond_2024_8_9x.tb

# third batch with logger program TowerViikki-024-08-fast
# This added five sglux fast sensors, and their data to this table
# frequency remained unchanged.

load("data-rda-partial/millisecond_2024_8_21.tb.rda")
colnames(millisecond_2024_8_21.tb)
nrow(millisecond_2024_8_21.tb)
millisecond_2024_8_21.tb |>
#  rename(time = TIMESTAMP) |>
  mutate(series_start = time[1],
         PAR_umol_CS = PAR_Den_CS / 0.946 - 1.057, # -0 - 50
         greenf_umol = Greenf_Den - 7.159275e-02,
         greenf_sellaro_umol = Greenf_Den - 7.159275e-02,
         bluef_umol = Bluef_Den / 1.4010 - 8.346781e-01,
         bluef_sellaro_umol = Bluef_Den / 1.9365 - 6.038647e-01,
         UVA1f_umol = UVA1f_Den / 6.4374 - 8.985690e-03,
         UVAf_umol = UVAf_Den / 4.3838 - 1.740200e-01,
         UVA2f_umol = UVAf_Den / 24.312 - 3.137829e-02,
         UVBf_umol = UVBf_Den / 273.14 - 1.870687e-04,
         UVA1fc_umol = 0.73747 * UVAf_umol + 0.03433 * bluef_sellaro_umol,
         UVA2fc_umol = 0.157966 * UVAf_umol + 1.364312 * UVBf_umol
  ) |>
  select(series_start,
         time,
         day_of_year,
         month_of_year,
         month_name,
         calendar_year = year,
         time_of_day_utc,
         solar_time_h,
         sun_elevation,
         sun_azimuth,
         PAR_umol_CS,
         greenf_umol,
         greenf_sellaro_umol,
         bluef_umol,
         bluef_sellaro_umol,
         UVA1f_umol,
         UVAf_umol,
         UVA2f_umol,
         UVBf_umol,
         UVA1fc_umol,
         UVA2fc_umol,
         f_in_the_open,
         f_in_canopy,
         f_unknown) -> millisecond_2024_8_21x.tb

# bind data, here the columns differ so binding adds NAs

bind_rows(millisecond_2024_5_14x.tb,
          millisecond_2024_8_9x.tb,
          millisecond_2024_8_21x.tb) -> millisecond_2024_latest.tb
range(millisecond_2024_latest.tb$time)

# no data logged at night!
max(diff(millisecond_2024_latest.tb$time))
round(min(diff(millisecond_2024_latest.tb$time)), 5)

rm(millisecond_2024_5_14x.tb, millisecond_2024_8_9x.tb, millisecond_2024_8_21x.tb)
gc()

# We check for duplicate rows
# Sometimes after changes in the logger program data already downloaded has been
# appended to files, leading to duplicate rows.
# We now check this when reading the file from the logger but older data
# may have duplicate records.
rle.check <- rle(sort(as.numeric(millisecond_2024_latest.tb$time)))
duplicates <- sum(rle.check$lengths > 1)
if (duplicates > 0L) {
  message("Found ", duplicates, " duplicated rows in data. Deleting them!!")
  millisecond_2024_latest.tb <- distinct(millisecond_2024_latest.tb, .keep_all = TRUE)
  nrow(millisecond_2024_latest.tb)
}

# clean_spikes <- function(x,
#                          z.threshold = 50,
#                          d.threshold = NULL,
#                          na.rm = FALSE) {
#   max.spike.width <- 3
#   n <- length(x)
#   if (na.rm) {
#     na.idx <- which(is.na(x))
#     x <- na.omit(x)
#   }
#   d.var <- diff(x)
#   if (is.null(d.threshold)) {
#     d.threshold <- mean(abs(d.var))
#   }
#   z <- (d.var - median(d.var)) / mad(d.var) * 0.6745
#   bad.obs.idx <- ifelse(abs(d.var) < d.threshold,
#                         FALSE,
#                         abs(z) > z.threshold)
#   # ensure same length as input
#   bad.obs.idx <- c(FALSE, bad.obs.idx)
#   if (!is.null(max.spike.width) && max.spike.width > 0) {
#     # ignore broad peaks using run length encoding
#     runs <- rle(bad.obs.idx)
#     runs[["values"]] <- ifelse(runs[["lengths"]] > max.spike.width, FALSE, runs[["values"]])
#     bad.obs.idx <- inverse.rle(runs)
#   }
#   if (na.rm) {
#     # restore length of logical vector
#     for (i in na.idx) {
#       bad.obs.idx <- append(bad.obs.idx, FALSE, after = i - 1L)
#     }
#   }
#   # check assertion
#   stopifnot(length(bad.obs.idx) == length(x))
#   bad.obs.idx <- which(bad.obs.idx)
#   x[bad.obs.idx] <- NA_integer_
#   if (1L %in% bad.obs.idx) {
#     x[1L] <- x[2L]
#     bad.obs.idx <- setdiff(bad.obs.idx, 1L)
#   }
#   if (n %in% bad.obs.idx) {
#     x[n] <- x[n - 1L]
#     bad.obs.idx <- setdiff(bad.obs.idx, n)
#   }
#
#   x[bad.obs.idx] <- (x[bad.obs.idx - 1] + x[bad.obs.idx + 1]) / 2
#   message("Replaced ", length(bad.obs.idx), " observations.")
#   x
# }
#
# #  Clean spikes in data
# minute_2015_latest.tb |>
#   mutate(air_temp_C = clean_spikes(air_temp_C),
#          surf_temp_C = clean_spikes(surf_temp_C),
#          global_watt = clean_spikes(global_watt),
#          air_vp = clean_spikes(air_vp)
#   ) -> minute_2015_latest.tb

millisecond_2024_latest.tb |>
  rename(PAR_umol = PAR_umol_CS) |>
  mutate(bluef_greenf = ifelse(PAR_umol > 0.1 & f_in_the_open,
                               bluef_sellaro_umol / greenf_sellaro_umol, NA_real_),
         bluef_greenf_sq = bluef_greenf * wl_expanse(Green("Sellaro")) / wl_expanse(Blue("Sellaro")),
         UVAf_PAR = ifelse(PAR_umol > 0.1 & f_in_the_open, UVAf_umol / PAR_umol, NA_real_),
         UVAf_PAR_sq = UVAf_PAR * wl_expanse(PAR()) / wl_expanse(UVA()),
         UVA1f_PAR = ifelse(PAR_umol > 0.1 & f_in_the_open, UVA1f_umol / PAR_umol, NA_real_),
         UVA1f_PAR_sq = UVA1f_PAR * wl_expanse(PAR()) / wl_expanse(UVA1()),
         UVA2f_PAR = ifelse(PAR_umol > 0.1 & f_in_the_open, UVAf_umol / PAR_umol, NA_real_),
         UVA2f_PAR_sq = UVA2f_PAR * wl_expanse(PAR()) / wl_expanse(UVA2()),
         UVBf_PAR = ifelse(PAR_umol > 0.1 & f_in_the_open, UVBf_umol / PAR_umol, NA_real_),
         UVBf_PAR_sq = UVBf_PAR * wl_expanse(PAR()) / wl_expanse(UVB()),
         .after = "UVA2fc_umol")|>
  mutate(delta_time = c(NA, diff(time)),
         start_of_slice = delta_time > seconds(60)) -> millisecond_2024_latest.tb

gc()

# time range, rows, cols
range(millisecond_2024_latest.tb$time)
nrow(millisecond_2024_latest.tb)
ncol(millisecond_2024_latest.tb)
colnames(millisecond_2024_latest.tb)

# checks
# second_2024_latest.tb |>
#   group_by(calendar_year, month_of_year, day_of_year) |>
#   filter(PAR_umol == max(PAR_umol)) |>
#   select(time, calendar_year, month_of_year, day_of_year,
#          PAR_umol, solar_time, time_of_day, sun_elevation) -> solar.time.max.PAR
#
# solar.time.max.PAR |>
#   group_by(calendar_year, month_of_year) |>
#   summarise(solar.time.max.PAR = mean(solar_time) ,
#             local.time.max.PAR = mean(time_of_day),
#             elevation.max.PAR = mean(sun_elevation)) |>
#   ungroup() |>
#   arrange(month_of_year, calendar_year) -> zz

# View(zz)

tail(millisecond_2024_latest.tb)
save(millisecond_2024_latest.tb, file = "data-rda/millisecond_2024_latest.tb.rda")

# load(file = "data-rda/millisecond_2024_latest.tb.rda")
# compute hourly summaries

mean_min_max <- list(
  mean = ~mean(.x, na.rm = FALSE),
#  mean.trimmed = ~mean(.x, trim = 1/3, na.rm = FALSE),
#  median = ~median(.x, na.rm = FALSE),
  min = ~min(.x, na.rm = FALSE),
  max = ~max(.x, na.rm = FALSE)
)

colnames(millisecond_2024_latest.tb)

# time is the hour at the
millisecond_2024_latest.tb |>
  mutate(time_seconds = trunc(time, units = "secs") + seconds(1)) |> # match FMI timing
  select(-series_start) |>
  group_by(time_seconds) |>
  summarize(across(time, first, .names = "{.col}_start"),
            across(time, last, .names = "{.col}_end"),
            across(day_of_year:calendar_year, first, .names = "{.col}"),
            across(time_of_day_utc:sun_azimuth, median, .names = "{.col}"),
            across(PAR_umol:UVBf_PAR_sq, mean_min_max),
            across(f_in_the_open:f_unknown, all, .names = "{.col}"),
            n = n(),
            incomplete.data = n < 20,
            bad.data = n < 15) |>
  ungroup() |>
  #  select(-time) |>
  mutate(time_seconds = as.POSIXct(time_seconds)) |>
  rename(time = time_seconds) -> second_calc_2024_latest.tb

dim(second_calc_2024_latest.tb)
colnames(second_calc_2024_latest.tb)

save(second_calc_2024_latest.tb, file = "data-rda/second_calc_2024_latest.tb.rda")

## Check range of values
mean_min_max <- list(
  mean = ~mean(.x, na.rm = TRUE),
  #  mean.trimmed = ~mean(.x, trim = 1/3, na.rm = TRUE),
  #  median = ~median(.x, na.rm = TRUE),
  min = ~min(.x, na.rm = TRUE),
  max = ~max(.x, na.rm = TRUE)
)

colnames(millisecond_2024_latest.tb)
millisecond_2024_latest.tb |>
  summarize(
    across(PAR_umol:UVBf_PAR_sq, mean_min_max)
    ) |> t()

colnames(second_calc_2024_latest.tb)
second_calc_2024_latest.tb |>
  summarize(
    across(PAR_umol_mean:UVA2fc_umol_max, mean_min_max)
  ) |> t()


