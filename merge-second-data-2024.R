library(dplyr)
library(lubridate)
library(photobiology)
library(photobiologyWavebands)

# clean up
rm(list = ls(pattern = "*"))
gc()

load("data-rda/second_2024_5_14.tb.rda")
colnames(second_2024_5_14.tb)
nrow(second_2024_5_14.tb)
second_2024_5_14.tb %>%
  mutate(series_start = TIMESTAMP[1],
         PAR_umol = NA_real_,
         PAR_umol_LI = NA_real_,
         PAR_umol_CS = PAR_Den_CS / 0.946, # -0 - 50
         PAR_umol_BF = PAR_BF_tot * 1.124 + 3, # 2024 adjusted based on PAR_umol_CS
         blue_umol = Blue_Den * 0.881 / 0.902 - 0.944,
         blue_sellaro_umol = Blue_Den * 0.637 / 0.902 - 0.561,
         UVA_umol = UVA_Den * 0.2356 / 0.936 - 0.251,
         UVB_umol = UVB_Den * 0.0046644 / 0.854 - 0.00437,
         UVA1_umol = 0.73747 * UVA_umol + 0.03433 * blue_sellaro_umol,
         UVA2_umol = 0.157966 * UVA_umol + 1.364312 * UVB_umol,
         red_umol = red_umol * 5.18 / 1.050,
         far_red_umol = far_red_umol * 5.12 / 1.02, # / 1.048,
         solar_disk = factor(sunny,
                             levels = c(FALSE, TRUE),
                             labels = c("occluded", "visible"))
  ) %>%
  select(series_start,
         time = TIMESTAMP,
         day_of_year,
         month_of_year,
         month_name,
         calendar_year = year,
         time_of_day = time_of_day_utc,
         solar_time = solar_time_h,
         sun_elevation,
         sun_azimuth,
         PAR_umol_CS,
         PAR_umol_BF,
         PAR_umol,
         PAR_diff_fr,
         red_umol,
         far_red_umol,
         blue_umol,
         blue_sellaro_umol,
         UVA_umol,
         UVA1_umol,
         UVA2_umol,
         UVB_umol,
         global_irrad = Solar_irrad,
         solar_disk) -> second_2024_5x.tb

# bind_rows(minute_2015_8x.tb, minute_2016_8x.tb, minute_2017_6x.tb,
#           minute_2019_4x.tb, minute_2020_5x.tb, minute_2020_11x.tb,
#           minute_2021_6x.tb) -> minute_2015_latest.tb

bind_rows(second_2024_5x.tb) %>%
          arrange(time) -> second_2024_latest.tb
second_2024_latest.tb[nrow(second_2024_latest.tb), "time"]

rm(second_2024_5x.tb)
gc()

# We check for duplicate rows
# Sometimes after changes in the logger program data already downloaded has been
# appended to files, leading to duplicate rows.
# We now check this when reading the file from the logger but older data
# may have duplicate records.
rle.check <- rle(sort(as.numeric(second_2024_latest.tb$time)))
duplicates <- sum(rle.check$lengths > 1)
if (duplicates > 0L) {
  message("Found ", duplicates, " duplicated rows in data. Deleting them!!")
  second_2024_latest.tb <- distinct(second_2024_latest.tb, .keep_all = TRUE)
  nrow(second_2024_latest.tb)
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
# minute_2015_latest.tb %>%
#   mutate(air_temp_C = clean_spikes(air_temp_C),
#          surf_temp_C = clean_spikes(surf_temp_C),
#          global_watt = clean_spikes(global_watt),
#          air_vp = clean_spikes(air_vp)
#   ) -> minute_2015_latest.tb

# Add computed values for radiation and PET
albedo <- 0.23

second_2024_latest.tb %>%
  mutate(PAR_umol = PAR_umol_CS) %>%
  mutate(# correct for temperature coefficient assuming sensors are at air temperature
         UVA_umol = UVA_umol, # * (1 + 0.0033 * (air_temp_C - 20)),
         UVB_umol = UVB_umol, # * (1 + 0.0015 * (air_temp_C - 20)),
         blue_red = blue_umol / red_umol,
         blue_red_sq = blue_red * wl_expanse(Red("Smith10")) / wl_expanse(Blue("Sellaro")),
         UVA_PAR = UVA_umol / PAR_umol,
         UVA_PAR_sq = UVA_PAR * wl_expanse(PAR()) / wl_expanse(UVA()),
         UVA1_PAR = UVA1_umol / PAR_umol,
         UVA1_PAR_sq = UVA1_PAR * wl_expanse(PAR()) / wl_expanse(UVA1()),
         UVA2_PAR = UVA2_umol / PAR_umol,
         UVA2_PAR_sq = UVA2_PAR * wl_expanse(PAR()) / wl_expanse(UVA2()),
         UVB_PAR = UVB_umol / PAR_umol,
         UVB_PAR_sq = UVB_PAR * wl_expanse(PAR()) / wl_expanse(UVB()),
         red_far_red = red_umol / far_red_umol,
         .after = "UVB_umol") -> second_2024_latest.tb
second_2024_latest.tb[nrow(second_2024_latest.tb), "time"]

gc()

range(second_2024_latest.tb$time)
nrow(second_2024_latest.tb)
ncol(second_2024_latest.tb)
colnames(second_2024_latest.tb)

# checks
# second_2024_latest.tb %>%
#   group_by(calendar_year, month_of_year, day_of_year) %>%
#   filter(PAR_umol == max(PAR_umol)) %>%
#   select(time, calendar_year, month_of_year, day_of_year,
#          PAR_umol, solar_time, time_of_day, sun_elevation) -> solar.time.max.PAR
#
# solar.time.max.PAR %>%
#   group_by(calendar_year, month_of_year) %>%
#   summarise(solar.time.max.PAR = mean(solar_time) ,
#             local.time.max.PAR = mean(time_of_day),
#             elevation.max.PAR = mean(sun_elevation)) %>%
#   ungroup() %>%
#   arrange(month_of_year, calendar_year) -> zz

# View(zz)

range(second_2024_latest.tb$time)
tail(second_2024_latest.tb)

save(second_2024_latest.tb, file = "data-rda/second_2024_latest.tb.rda")

# compute hourly summaries

mean_min_max <- list(
  mean = ~mean(.x, na.rm = FALSE),
#  mean.trimmed = ~mean(.x, trim = 1/3, na.rm = FALSE),
#  median = ~median(.x, na.rm = FALSE),
  min = ~min(.x, na.rm = FALSE),
  max = ~max(.x, na.rm = FALSE)
)

colnames(second_2024_latest.tb)

# time is the hour at the
second_2024_latest.tb |>
  mutate(time_minutes = trunc(time, units = "mins") + minutes(1)) |> # match FMI timing
  select(-series_start) |>
  group_by(time_minutes) |>
  summarize(across(time, first, .names = "{.col}_start"),
            across(time, last, .names = "{.col}_end"),
            across(day_of_year:calendar_year, first, .names = "{.col}"),
            across(time_of_day:sun_azimuth, median, .names = "{.col}"),
            across(PAR_umol_CS:global_irrad, mean, .names = "{.col}"),
            sun_visible_fr = sum(solar_disk == "visible") / n(),
            solar_disk = ifelse(sun_visible_fr < 0.5, "occluded", "visible"),
              n = n()) |>
  mutate(solar_disk = factor(solar_disk)) |>
  ungroup() |>
  filter(n > 100) |> # delete summaries for minutes with less than 50 seconds of data
  #  select(-time) |>
  rename(time = time_minutes) -> minute_calc_2024_latest.tb

dim(minute_calc_2024_latest.tb)
colnames(minute_calc_2024_latest.tb)

save(minute_calc_2024_latest.tb, file = "data-rda/minute_calc_2024_latest.tb.rda")



