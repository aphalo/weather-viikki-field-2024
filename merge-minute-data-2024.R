library(dplyr)
library(lubridate)
library(photobiology)
library(photobiologyWavebands)

## 2024-04-20
#
# Added code to recompute sun angles and solar time using more accurate
# coordinates for the location of the station
#
# Recompute was_sunny using a better algorithm based on the solar elevation
# and expected diffuse fraction function from TUV simulations.
#
# Instead of reading again the raw data, we overwrite these values in the
# data already imported. See line xxx

# clean up
rm(list = ls(pattern = "*"))
gc()

# value updated 2024-04-20 (difference about 200 m)
viikki_wstation.geo <- data.frame(lon = 25.019212, lat = 60.226805,
                                  address = "Viikki weather station")

load("data-rda/minute_2024_5.tb.rda")
nrow(minute_2024_5.tb)
colnames(minute_2024_5.tb)
minute_2024_5.tb |>
  mutate(series_start = time.minute[1],
         air_vp = water_RH2vp(RelHumidity, AirTemp),
         PAR_umol_LI = NA_real_, # PAR_Den_Avg * 1.26 / 1.06, #  / 0.948
         PAR_umol_CS = PAR_Den_CS / 0.946, # -0 - 50
         PAR_umol = PAR_umol_CS,
         PAR_umol_BF = PAR_BF_tot * 1.09 - 0.27,
         blue_umol = Blue_Den * 0.881 / 0.902 - 0.944,
         blue_sellaro_umol = Blue_Den * 0.637 / 0.902 - 0.561,
         UVA_umol = UVA_Den * 0.2356 / 0.936 - 0.251,
         UVB_umol = UVB_Den * 0.0046644 / 0.854 - 0.00437,
         UVA1_umol = 0.73747 * UVA_umol + 0.03433 * blue_sellaro_umol,
         UVA2_umol = 0.157966 * UVA_umol + 1.364312 * UVB_umol,
         red_umol = red_umol * 5.18 / 1.050,
         far_red_umol = far_red_umol * 5.12 / 1.02, # / 1.048
         SurfTemp_veg = ifelse(SurfTemp_veg < -27 | SurfTemp_veg > 100,
                                   NA_real_,
                                   SurfTemp_veg),
         SurfTemp_grnd = ifelse(SurfTemp_grnd < -27 | SurfTemp_grnd > 100,
                                    NA_real_,
                                    SurfTemp_grnd),
         surf_temp_sensor_delta_C = SurfTemp_veg - SurfTemp_grnd,
         surf_temp_C = (SurfTemp_veg + SurfTemp_grnd) / 2,
         surf_temp_C = ifelse(!is.na(surf_temp_C), surf_temp_C,
                              ifelse(is.na(SurfTemp_veg), SurfTemp_grnd, SurfTemp_veg))) |>
  select(series_start,
         time = time.minute,
         day_of_year,
         month_of_year,
         month_name,
         calendar_year = year,
         time_of_day = time_of_day_utc,
         solar_time = solar_time_h,
         sun_elevation,
         sun_azimuth,
         PAR_umol_LI,
         PAR_umol_CS,
         PAR_umol_BF,
         PAR_umol,
         PAR_diff_fr,
         global_watt = Solar_irrad,
         red_umol,
         far_red_umol,
         blue_umol,
         blue_sellaro_umol,
         UVA_umol,
         UVA1_umol,
         UVA2_umol,
         UVB_umol,
         wind_speed = WindSpd_S_WVT,
         wind_direction = WindDir_D1_WVT,
         air_temp_C = AirTemp,
         air_vp,
         air_RH = RelHumidity,
         air_DP = AirDewPoint,
         air_pressure = AirPressure,
         rain_mm_min = Ramount_Tot,
         surf_temp_C = surf_temp_C,
         surf_temp_sensor_delta_C = surf_temp_sensor_delta_C,
         was_sunny = sunny) -> minute_2024_5x.tb


# merge data files from different dates -----------------------------------

# merging of multiple data files should take place here in the future

minute_2024_latest.tb <- minute_2024_5x.tb

# save date range for later validation
original_time_range <- range(minute_2024_latest.tb$time)

# We check for duplicate rows
# Sometimes after changes in the logger program data already downloaded has been
# appended to files, leading to duplicate rows.
# We now check this when reading the file from the logger but older data
# may have duplicate records.
rle.check <- rle(sort(as.numeric(minute_2024_latest.tb$time)))
duplicates <- sum(rle.check$lengths > 1)
if (duplicates > 0L) {
  message("Found ", duplicates, " duplicated rows in data. Deleting them!!")
  minute_2024_latest.tb <- distinct(minute_2024_latest.tb, .keep_all = TRUE)
  nrow(minute_2024_latest.tb)
}

# validate
stopifnot(all(original_time_range == range(minute_2024_latest.tb$time)))


# add computed values -----------------------------------------------------

# Add computed values for radiation and PET

clear_sky_diff_fr <- readRDS("TUV-diffuse-direct-SZA/spline-fun.RDS")

albedo <- 0.23

minute_2024_latest.tb |>
  mutate(
    # corrected values overwrite original ones
    solar_time_h = solar_time(time,
                              geocode = viikki_wstation.geo),
    sun_elevation = sun_elevation(time,
                                  geocode = viikki_wstation.geo,
                                  use.refraction = TRUE,
                                  tz = "UTC"),
    sun_azimuth = sun_azimuth(time,
                              geocode = viikki_wstation.geo,
                              use.refraction = TRUE,
                              tz = "UTC"),
    was_day = sun_elevation > 0,
    PAR_diff_fr_rel = ifelse(was_day,
                             1 - ((1 - PAR_diff_fr) / (1 - clear_sky_diff_fr(sun_elevation))),
                             NA_real_),
    was_sunny = PAR_diff_fr_rel < 0.6,
    ## end corrections
    ## temperature and energy balance
    logged_air_temp_C = air_temp_C,
    # filter bad air temperature data
    # out of range
    air_temp_C = ifelse(logged_air_temp_C < -40 | logged_air_temp_C > 40,
                        NA_real_,
                        logged_air_temp_C),
    # use running median for 11 min as reference
    air_temp_run_median = runmed(air_temp_C, k = 21),
    # if minute average is much less than 21 min median, replace with median
    air_temp_C = ifelse((air_temp_C - air_temp_run_median) < 1 | is.na(air_temp_C),
                        air_temp_run_median,
                        logged_air_temp_C),

    temp_surf2air_C = surf_temp_C - air_temp_C,
    R_0 = irrad_extraterrestrial(time = time,
                                 geocode = viikki_wstation.geo),
    R_rel = ifelse(R_0 > 2,
                   global_watt / (R_0 * 0.75),
                   0.25), # for now we assume cloudy sky at night
    Rn_sw_ref = global_watt,
    Rn_ref = net_irradiance(temperature = air_temp_C,
                            sw.down.irradiance = Rn_sw_ref,
                            sw.albedo = albedo,
                            water.vp = air_vp,
                            R_rel = R_rel),
    ET_ref_FAO56 = ET_ref(temperature = air_temp_C,
                          water.vp = air_vp,
                          wind.speed = wind_speed,
                          net.irradiance = Rn_ref,
                          nighttime = sun_elevation < 0,
                          atmospheric.pressure = air_pressure,
                          method = "FAO.PM",
                          check.range = FALSE),
    ET_ref_short = ET_ref(temperature = air_temp_C,
                          water.vp = air_vp,
                          wind.speed = wind_speed,
                          net.irradiance = Rn_ref,
                          nighttime = sun_elevation < 0,
                          atmospheric.pressure = air_pressure,
                          method = "ASCE.PM.short",
                          check.range = FALSE),
    ET_ref_tall = ET_ref(temperature = air_temp_C,
                         water.vp = air_vp,
                         wind.speed = wind_speed,
                         net.irradiance = Rn_ref,
                         nighttime = sun_elevation < 0,
                         atmospheric.pressure = air_pressure,
                         method = "ASCE.PM.tall",
                         check.range = FALSE),
    # ET_ref_surf = ET_ref(temperature = surf_temp_C,
    #                 water.vp = air_vp,
    #                 wind.speed = wind_speed,
    #                 net.irradiance = Rn_ref,
    #                 nighttime = sun_elevation < 0,
    #                 atmospheric.pressure = air_pressure,
    #                 method = "ASCE.PM.short",
    #                 check.range = FALSE),
    air_vpd = ifelse(is.na(air_temp_C),
                     NA_real_,
                     water_vp_sat(air_temp_C) - air_vp),
    rain_mm_min = ifelse(time < ymd_hms("2021-06-09 14:00:00"),
                         NA_real_,
                         rain_mm_min),
    .after = "was_sunny") |>
  mutate(# correct for temperature coefficient assuming sensors are at air temperature
    UVA_umol = UVA_umol * (1 + 0.0033 * (air_temp_C - 20)),
    UVB_umol = UVB_umol * (1 + 0.0015 * (air_temp_C - 20)),
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
    .after = "UVB_umol") -> minute_2024_latest.tb


# data validation checks --------------------------------------------------

# TIMESTAMP/time is always defined, an NA indicates data corruption
# if the range has changed head or tail rows have been lost, or time zone messed
# If not strictly sorted, either repeated observations or truncated times are present
stopifnot(!anyNA(minute_2024_latest.tb$time),
          all(original_time_range == range(minute_2024_latest.tb$time)),
          !is.unsorted(minute_2024_latest.tb$time, strictly = TRUE)
          )

gc()

# reporting
range(minute_2024_latest.tb$time)
nrow(minute_2024_latest.tb)
ncol(minute_2024_latest.tb)
colnames(minute_2024_latest.tb)

# checks data for missing values
sum(is.na(minute_2024_latest.tb$air_temp_C))
sum(is.na(minute_2024_latest.tb$air_RH))

# validate that maximum irradiance coincides, roughly, with solar noon
# this is a check for possible time shifts
minute_2024_latest.tb |>
  group_by(calendar_year, month_of_year, day_of_year) |>
  filter(PAR_umol == max(PAR_umol)) |>
  select(time, calendar_year, month_of_year, day_of_year,
         PAR_umol, solar_time, time_of_day, sun_elevation) -> solar.time.max.PAR

solar.time.max.PAR |>
  group_by(calendar_year, month_of_year) |>
  summarise(solar.time.max.PAR = mean(solar_time) ,
            UTC.time.max.PAR = mean(time_of_day),
            elevation.max.PAR = mean(sun_elevation)) |>
  ungroup() |>
  arrange(month_of_year, calendar_year) -> zz

zz


# save to file 1 minute data set ------------------------------------------

save(minute_2024_latest.tb, file = "data-rda/minute_2024_latest.tb.rda")

# compute hourly summaries ------------------------------------------------
# data from sensors logged at 1 min, are not logged at 1 h
# we compute the means here and later read them in when merging 1 h data

mean_min_max <- list(
  mean = ~mean(.x, na.rm = FALSE),
#  mean.trimmed = ~mean(.x, trim = 1/3, na.rm = FALSE),
#  median = ~median(.x, na.rm = FALSE),
  min = ~min(.x, na.rm = FALSE),
  max = ~max(.x, na.rm = FALSE)
)

colnames(minute_2024_latest.tb)

# time is the hour at the
minute_2024_latest.tb |>
  # we truncate current time to the hour and add 1 h to match FMI timing
  # match FMI timing -> the time of a mean is the time of the LAST value / when mean is computed
  mutate(time_hours = trunc(time, units = "hours") + hours(1)) |>
  select(-series_start) |>
#  relocate(contains(c("time", "year", "month", "sun"))) |>
  group_by(time_hours) |>
    summarize(across(time, first, .names = "{.col}_start"),
              across(time, last, .names = "{.col}_end"),
              across(day_of_year:calendar_year, first, .names = "{.col}"),
              across(time_of_day:sun_azimuth, median, .names = "{.col}"),
              across(PAR_umol_LI:air_pressure, mean_min_max),
              across(surf_temp_C:surf_temp_sensor_delta_C, mean_min_max),
              across(PAR_diff_fr_rel:air_vpd, mean, .names = "{.col}_mean"),
              across(PAR_diff_fr_rel:air_vpd, max, .names = "{.col}_max"),
              across(PAR_diff_fr_rel:air_vpd, min, .names = "{.col}_min"),
              rain_mm_h = mean(rain_mm_min), # something wrong with units!!
              was_sunny = (sum(was_sunny) / n()) >= 0.5,
              was_day = (sum(was_day) / n()) >= 0.5,
              n = n(),
              incomplete.data = n < 120,
              bad.data = n < 100) |>
  ungroup() |>
  filter(n > 31) |> # delete summaries for hours with less than 31 min of data
#  select(-time) |>
  rename(time = time_hours) -> hour_calc_2024_latest.tb

sum(hour_calc_2024_latest.tb$incomplete.data)
sum(hour_calc_2024_latest.tb$bad.data)

sum(grepl("max$", colnames(hour_calc_2024_latest.tb)))
sum(grepl("min$", colnames(hour_calc_2024_latest.tb)))
sum(grepl("mean$", colnames(hour_calc_2024_latest.tb)))

ncol(hour_calc_2024_latest.tb)

dim(hour_calc_2024_latest.tb)
colnames(hour_calc_2024_latest.tb)

save(hour_calc_2024_latest.tb, file = "data-rda/hour_calc_2024_latest.tb.rda")

