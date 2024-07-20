## A bit after 11:30 (UTC + 2) I disconnected the power source from mains.
##

# 2022-11-24 ensure use of UTC everywhere
# In Viikki data time is in UTC

library(fmi2)
library(dplyr)
library(sf)
library(lubridate)
library(ggplot2)
library(ggpmisc)

## Download and cache locally data from FMI

# # functions for stored queries not supported by 'fmi2' package
# # currently not working
# # modified from fmi2
# obs_soil_hourly <- function(starttime, endtime, fmisid = NULL) {
#   fmi_obj <- fmi_api(request = "getFeature",
#                      storedquery_id = "fmi::observations::soil::hourly::simple",
#                      starttime = starttime, endtime = endtime, fmisid = fmisid)
#   sf_obj <- fmi2:::to_sf(fmi_obj)
#   sf_obj <- sf_obj %>%
#     dplyr::select(time = .data$Time, variable = .data$ParameterName,
#                   value = .data$ParameterValue) %>%
#     dplyr::mutate(time = lubridate::parse_date_time(.data$time, "Ymd HMS"),
#                   variable = as.character(.data$variable),
#                   # Factor needs to be coerced into character first
#                   value = as.numeric(as.character(.data$value))) %>%
#     dplyr::mutate(value = ifelse(is.nan(.data$value), NA, .data$value))
#   return(sf_obj)
# }
#
# # modified from fmi2
# obs_weather_inst <- function(starttime, endtime, fmisid = NULL) {
#   fmi_obj <- fmi_api(request = "getFeature",
#                      storedquery_id = "fmi::observations::weather::simple",
#                      starttime = starttime, endtime = endtime, fmisid = fmisid)
#   sf_obj <- fmi2:::to_sf(fmi_obj)
#   sf_obj <- sf_obj %>%
#     dplyr::select(time = .data$Time, variable = .data$ParameterName,
#                   value = .data$ParameterValue) %>%
#     dplyr::mutate(time = lubridate::parse_date_time(.data$time, "Ymd HMS"),
#                   variable = as.character(.data$variable),
#                   # Factor needs to be coerced into character first
#                   value = as.numeric(as.character(.data$value))) %>%
#     dplyr::mutate(value = ifelse(is.nan(.data$value), NA, .data$value))
#   return(sf_obj)
# }

# If the merged data have already been saved, and are newer than the
# latest Vikki data, we skip the merging and read the file

if (!file.exists("data-rda/FMI/viikki_kumpula_hourly.tb.rda") ||
    file.info("data-rda/FMI/viikki_kumpula_hourly.tb.rda")$mtime <
    file.info("data-rda/hour_calc_2015_2023.tb.rda")$mtime) {
  # This section of the script reads weather data for Kumpula FMI station (1 h frequency)
  # It stores the read data in a file, and when run again downloads only newer data
  # To reset saved data, simply delete file "fmi-weather-wide.Rda"

  # we store locally data and read only new data since previous download
  if (!file.exists("data-rda/FMI/fmi-weather-data-wide.Rda")) {
    # Used only once or when replacing all data
    starttime.char <- "2019-12-31 22:00"  # UTC midnight in Finland
    starttime <- ymd_hm(starttime.char, tz = "UTC")
    wide_weather_data <- data.frame()
  } else {
    load("data-rda/FMI/fmi-weather-data-wide.Rda")
    # we start 1 h after end of previously downloaded data
    starttime <-force_tz(max(wide_weather_data$time), tzone = "UTC") + minutes(59)  # ensure tz UTC
  }

  endtime <- trunc(now(), units = "mins")

  # we read the new data to a new dataframe
  # (to avoid appending repeatedly to a long one)
  new_wide_data <- data.frame()
  while (starttime < endtime) {
    sliceendtime <- starttime + days(28) # keep query size at max of 4 weeks
    if (sliceendtime > endtime) {
      sliceendtime <- endtime
    }
    kumpula_data <- obs_weather_hourly(starttime = as.character(starttime),
                                       endtime = as.character(sliceendtime),
                                       fmisid = 101004)

    slice_data <- kumpula_data %>%
      tidyr::spread(variable, value) %>%
      # convert the sf object into a regular tibble
      sf::st_set_geometry(NULL)

    new_wide_data <- rbind(new_wide_data, slice_data)
    starttime <- sliceendtime + minutes(1)
    cat(".")
  }

  # new_wide_data$time <- new_wide_data$time - hours(2)
  # new_wide_data$time <- force_tz(new_wide_data$time, tzone = "UTC")

  range(new_wide_data$time)

  wide_weather_data <- rbind(wide_weather_data, new_wide_data)
  range(wide_weather_data$time)
  colnames(wide_weather_data)

  save(wide_weather_data, file = "data-rda/FMI/fmi-weather-data-wide.Rda")

  ## load data from own Viikki weather station
  load("data-rda/hour_calc_2015_2023.tb.rda")
  hour_calc_2015_2023.tb <- subset(hour_calc_2015_2023.tb, time >= min(wide_weather_data$time))
  range(hour_calc_2015_2023.tb$time)
  colnames(hour_calc_2015_2023.tb)
  nrow(hour_calc_2015_2023.tb)

  wide_weather_data <- subset(wide_weather_data, time <= max(hour_calc_2015_2023.tb$time))
  range(wide_weather_data$time)

  # we trim the extremes but use full-join to retain the FMI data for gaps
  viikki_kumpula_hourly <-
    full_join(wide_weather_data, hour_calc_2015_2023.tb, by = "time")

  range(viikki_kumpula_hourly$time)
  colnames(viikki_kumpula_hourly)
  ncol(viikki_kumpula_hourly)
  nrow(viikki_kumpula_hourly)

  save(viikki_kumpula_hourly, file = "data-rda/FMI/viikki_kumpula_hourly.tb.rda")

} else {
  load("data-rda/FMI/viikki_kumpula_hourly.tb.rda")
  range(viikki_kumpula_hourly$time)
}

viikki_kumpula_hourly <-
  within(viikki_kumpula_hourly,
         day_or_night <- factor(sun_elevation_median > 0,
                                levels = c(TRUE, FALSE),
                                labels = c("day", "night")))

# a very quick look using a variable tree
vtree::vtree(viikki_kumpula_hourly, "month_name_first day_or_night", horiz = FALSE)

## plots
theme_set(theme_bw())

## Simple plots

ggplot(viikki_kumpula_hourly,
       aes(x = time, y = TA_PT1H_AVG, ymax = TA_PT1H_MAX, ymin = TA_PT1H_MIN)) +
#  geom_pointrange(fatten = 0.5) +
  stat_quadrant_counts(quadrants = 0L) +
  geom_line() +
  expand_limits(y = c(-30, 32)) +
  ggtitle("FMI Kumpula")

ggplot(viikki_kumpula_hourly,
       aes(x = time,
           y = air_temp_C_mean)) +
  stat_quadrant_counts(quadrants = 0L) +
  geom_line() +
  expand_limits(y = c(-30, 32)) +
  ggtitle("OEB Viikki, based on minute means")

## air temperature difference

ggplot(viikki_kumpula_hourly,
       aes(x = time,
           y = air_temp_C_mean - TA_PT1H_AVG,
           colour = (air_temp_C_mean - TA_PT1H_AVG) < 0)) +
  stat_quadrant_counts(quadrants = 0L) +
  geom_point(alpha = 0.1) +
  stat_ma_line() +
  stat_ma_eq() +
  ggtitle("Air temperature difference\nOEB Viikki - FMI Kumpula (C)")

# compare air vs. surface
surf_air_tC.fig <-
  ggplot(subset(viikki_kumpula_hourly,
                month_of_year_first %in% 5:11 &
                  calendar_year_first > 2020),
       aes(x = solar_time_median, y = temp_surf2air_C_mean)) +
  geom_hline(colour = "red", yintercept = 0) +
  geom_point(alpha = 0.1) +
  stat_smooth() +
  ggtitle("OEB Viikki, surface to air T difference",
          "Filtered data, whole period") +
  facet_grid(cols = vars(month_of_year_first),
             rows = vars(calendar_year_first),
             drop = TRUE)

surf_air_tC.fig

ggplot(subset(viikki_kumpula_hourly,
              month_of_year_first %in% 5:11 &
                calendar_year_first > 2020),
       aes(x = time_of_day_median, y = temp_surf2air_C_mean)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_point(alpha=0.05) +
  stat_smooth() +
  ggtitle("OEB Viikki, surface to air T difference",
          "Filtered data, whole period") +
  facet_wrap(facets = vars(month_of_year_first), drop = TRUE)

## Viikki vs. Kumpula

plot_start_time <- min(viikki_kumpula_hourly$time)
# ymd_hm("2021-01-01 00:00")

ggplot(subset(viikki_kumpula_hourly,
              time > plot_start_time),
       aes(x = TA_PT1H_AVG, y = air_temp_C_mean)) +
  geom_point(alpha=0.025) +
  geom_abline(color = "red") +
  stat_ma_line() +
  stat_ma_eq(use_label(c("eq", "R2", "n"))) +
  ggtitle("OEB Viikki, based on minute max, vs. FMI Kumpula")

ggplot(subset(viikki_kumpula_hourly,
              time > plot_start_time),
       aes(x = logged_air_temp_C_mean, y = air_temp_C_mean)) +
  geom_point(alpha=0.025) +
  geom_abline(color = "red") +
  stat_ma_line() +
  stat_ma_eq(use_label(c("eq", "R2", "n"))) +
  ggtitle("OEB Viikki, logged minute means, vs. cleaned minute means")

# after fixing WXT356 firmware update
ggplot(subset(viikki_kumpula_hourly,
              time > ymd_hm("2022-09-05 12:00")),
       aes(x = TA_PT1H_AVG, y = air_temp_C_mean)) +
  geom_point(alpha=0.05) +
  geom_abline(color = "red") +
  stat_ma_line() +
  stat_ma_eq(use_label(c("eq", "R2", "n"))) +
  ggtitle("OEB Viikki, based on minute mean, vs. FMI Kumpula",
          "Filtered data, period after firmware update")

# with bad readings
ggplot(subset(viikki_kumpula_hourly,
              time > ymd_hm("2021-06-01 00:00") &
              time < ymd_hm("2022-09-05 12:00")),
       aes(x = TA_PT1H_AVG, y = air_temp_C_mean)) +
  geom_point(alpha=0.05) +
  geom_abline(color = "red") +
  stat_ma_line() +
  stat_ma_eq(use_label(c("eq", "R2", "n"))) +
  ggtitle("OEB Viikki, based on minute mean, vs. FMI Kumpula",
          "Filtered data, period with bad firmware")

# before start of bad readings
ggplot(subset(viikki_kumpula_hourly,
              time < ymd_hm("2021-06-01 00:00")),
       aes(x = TA_PT1H_AVG, y = air_temp_C_mean)) +
  geom_point(alpha=0.05) +
  geom_abline(color = "red") +
  stat_ma_line() +
  stat_ma_eq(use_label(c("eq", "R2", "n"))) +
  ggtitle("OEB Viikki, based on minute mean, vs. FMI Kumpula",
          "Filtered data, period with old WTX")

# compare Kumpula vs. Viikki
# by week

plot_end_time <- max(viikki_kumpula_hourly$time) - weeks(10)

ggplot(data = subset(viikki_kumpula_hourly,
                     time < plot_end_time & time > plot_end_time - weeks(1))) +
  geom_rect(aes(xmin = time - minutes(30),
                xmax = time + minutes(30),
                ymin = -Inf,
                ymax = Inf,
                fill = day_or_night),
            alpha = 0.1) +
  scale_fill_manual(name = "",
                    values = c(day = "yellow", night = "grey50"),
                    aesthetics = c("colour", "fill")) +
  geom_pointrange(mapping = aes(x = time,
                                y = TA_PT1H_AVG,
                                ymax = TA_PT1H_MAX,
                                ymin = TA_PT1H_MIN),
                  fatten = 0.5, color = "red", alpha = 0.5) +
  geom_line(mapping = aes(x = time,
                          y = TA_PT1H_AVG),
            color = "red") +
  geom_pointrange(mapping = aes(x = time,
                                y = surf_temp_C_mean,
                                ymax = surf_temp_C_max,
                                ymin = surf_temp_C_min),
                  fatten = 0.5, color = "green", alpha = 0.5) +
  geom_line(mapping = aes(x = time,
                          y = surf_temp_C_mean),
            color = "green") +
  geom_pointrange(mapping = aes(x = time,
                                y = air_temp_C_mean,
                                ymax = air_temp_C_max,
                                ymin = air_temp_C_min),
                  fatten = 0.5, alpha = 0.3) +
  geom_line(mapping = aes(x = time,
                          y = air_temp_C_mean)) +
  expand_limits(y = c(0, 5))

plot_end_time <- max(viikki_kumpula_hourly$time) - months(5) - years(0)

ggplot(data = subset(viikki_kumpula_hourly,
                     time < plot_end_time & time > plot_end_time - months(1))) +
  geom_rect(aes(xmin = time - minutes(30),
                xmax = time + minutes(30),
                ymin = -Inf,
                ymax = Inf,
                fill = day_or_night),
            alpha = 0.1) +
  scale_fill_manual(name = "",
                    values = c(day = "yellow", night = "grey50"),
                    aesthetics = c("colour", "fill")) +
  geom_pointrange(mapping = aes(x = time,
                                y = TA_PT1H_AVG,
                                ymax = TA_PT1H_MAX,
                                ymin = TA_PT1H_MIN),
                  fatten = 0.5, color = "red", alpha = 0.3) +
  geom_line(mapping = aes(x = time,
                          y = TA_PT1H_AVG),
            color = "red") +
  geom_pointrange(mapping = aes(x = time,
                                y = air_temp_C_mean,
                                ymax = air_temp_C_max,
                                ymin = air_temp_C_min),
                  fatten = 0.5, alpha = 0.3) +
  geom_line(mapping = aes(x = time,
                          y = air_temp_C_mean)) +
  expand_limits(y = c(0, 5))

ggplot(data = viikki_kumpula_hourly) +
  geom_pointrange(mapping = aes(x = time,
                                y = air_temp_C_mean,
                                ymax = air_temp_C_max,
                                ymin = air_temp_C_min),
                  fatten = 0.5, alpha = 0.5) +
  geom_line(mapping = aes(x = time,
                          y = air_temp_max_C_mean)) +
  geom_pointrange(mapping = aes(x = time,
                                y = TA_PT1H_AVG,
                                ymax = TA_PT1H_MAX,
                                ymin = TA_PT1H_MIN),
                  fatten = 0.5, color = "red", alpha = 0.5) +
  geom_line(mapping = aes(x = time,
                          y = TA_PT1H_AVG),
            color = "red") +
  xlim(starttime, endtime)

ggplot(data = viikki_kumpula_hourly) +
  geom_pointrange(mapping = aes(x = time,
                                y = air_temp_max_C_mean,
                                ymax = air_temp_max_C_max,
                                ymin = air_temp_max_C_min),
                  fatten = 0.5, alpha = 0.5) +
  geom_line(mapping = aes(x = time,
                          y = air_temp_max_C_mean)) +
  geom_pointrange(mapping = aes(x = time,
                                y = air_temp_min_C_mean,
                                ymax = air_temp_min_C_max,
                                ymin = air_temp_min_C_min),
                  fatten = 0.5, colour = "blue", alpha = 0.5) +
  geom_line(mapping = aes(x = time,
                          y = air_temp_max_C_mean),
            colour = "blue") +
  geom_pointrange(mapping = aes(x = time,
                                y = TA_PT1H_AVG,
                                ymax = TA_PT1H_MAX,
                                ymin = TA_PT1H_MIN),
                  fatten = 0.5, color = "red", alpha = 0.5) +
  geom_line(mapping = aes(x = time,
                          y = TA_PT1H_AVG),
            color = "red") +
  xlim(starttime, endtime)

ggplot(data = viikki_kumpula_hourly) +
  geom_point(mapping = aes(x = time,
                                y = air_RH_mean),
                  fatten = 0.5, alpha = 0.5) +
  geom_line(mapping = aes(x = time,
                          y = air_RH_mean)) +
  geom_point(mapping = aes(x = time,
                                y = RH_PT1H_AVG),
                  fatten = 0.5, color = "red", alpha = 0.5) +
  geom_line(mapping = aes(x = time,
                          y = RH_PT1H_AVG),
            color = "red") +
  xlim(starttime, endtime)

load("data-rda/minute_2022_9.tb.rda")

nrow(minute_2022_9.tb) -> last

minute_2022_9.tb[(last - 24 * 60):last,] -> last_day.tb
ggplot(last_day.tb, aes(TIMESTAMP, AirTemp_Avg)) + geom_point(alpha=0.2)
ggplot(last_day.tb, aes(TIMESTAMP, RelHumidity_Avg)) + geom_point(alpha=0.2)
#ggplot(last_day.tb, aes(time, PTemp_C)) + geom_point(alpha=0.2)
ggplot(last_day.tb, aes(TIMESTAMP, AirPressure_Avg)) + geom_point(alpha=0.2)

minute_2022_9.tb[(last - 3 * 60):last,] -> last_3h.tb
ggplot(last_3h.tb, aes(TIMESTAMP, AirTemp_Avg)) + geom_point(alpha=0.5)
ggplot(last_3h.tb, aes(TIMESTAMP, RelHumidity_Avg)) + geom_point(alpha=0.5)
#ggplot(last_day.tb, aes(time, PTemp_C)) + geom_point(alpha=0.2)
ggplot(last_3h.tb, aes(TIMESTAMP, AirPressure_Avg)) + geom_point(alpha=0.5)

load("data-rda/minute_2015_2022.tb.rda")
nrow(minute_2015_2022.tb) -> last

minute_2015_2022.tb[(last - 24 * 60):last,] -> last_day.tb
ggplot(last_day.tb, aes(time, air_temp_C)) + geom_point(alpha=0.2)
ggplot(last_day.tb, aes(time, air_RH)) + geom_point(alpha=0.2)
#ggplot(last_day.tb, aes(TIMESTAMP, PTemp_C)) + geom_point(alpha=0.2)
ggplot(last_day.tb, aes(time, air_pressure)) + geom_point(alpha=0.2)

minute_2015_2022.tb[(last - 3 * 60):last,] -> last_3h.tb
ggplot(last_3h.tb, aes(time, air_temp_C)) + geom_point(alpha=0.5)
ggplot(last_3h.tb, aes(time, air_RH)) + geom_point(alpha=0.5)
#ggplot(last_3h.tb, aes(TIMESTAMP, PTemp_C)) + geom_point(alpha=0.5)
ggplot(last_3h.tb, aes(time, air_pressure)) + geom_point(alpha=0.5)

ggplot(data = hour_calc_2015_2022.tb) +
  geom_pointrange(mapping = aes(x = as.POSIXct(time),
                                y = air_temp_C_mean,
                                ymax = air_temp_C_max,
                                ymin = air_temp_C_min),
                  fatten = 0.5, alpha = 0.3) +
  geom_line(mapping = aes(x = as.POSIXct(time), y = air_temp_C_max),
            color = "red") +
  xlim(ymd_hm("2022-05-01 12:00"), ymd_hm("2022-09-08 12:00"))

