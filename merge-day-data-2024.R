library(dplyr)
library(photobiology)
library(lubridate)

# clean up
rm(list = ls(pattern = "*"))
gc()

mean_min_max <- list(
  mean = ~mean(.x, na.rm = FALSE),
  #  mean.trimmed = ~mean(.x, trim = 1/3, na.rm = FALSE),
  #  median = ~median(.x, na.rm = FALSE),
  min = ~min(.x, na.rm = FALSE),
  max = ~max(.x, na.rm = FALSE)
)

# All values are calculated off-line here from hour-interval data!
# Only some variables are of interest at this frequency.

load("data-rda/hour_2024_latest.tb.rda")
colnames(hour_2024_latest.tb)

hour_2024_latest.tb |>
  # time is in UTC, as used by FMI
  mutate(time.day = floor_date(time, "day"), .before = "time") |>
  group_by(time.day) |>
  summarise(# above ground
            PAR_umol_mean = mean(PAR_umol_mean, na.rm = TRUE),
            PAR_mol_day = PAR_umol_mean * 0.0864, # 3600 * 24 * 1e-6
            PAR_umol_min = min(PAR_umol_min, na.rm = TRUE),
            PAR_umol_max = max(PAR_umol_max, na.rm = TRUE),

            UVB_umol_mean = mean(UVB_umol_mean, na.rm = TRUE),
            UVB_mol_day = UVB_umol_mean * 0.0864, # 3600 * 24 * 1e-6
            UVB_umol_min = min(UVB_umol_min, na.rm = TRUE),
            UVB_umol_max = max(UVB_umol_max, na.rm = TRUE),

            UVA2_umol_mean = mean(UVA2_umol_mean, na.rm = TRUE),
            UVA2_mol_day = UVA2_umol_mean * 0.0864, # 3600 * 24 * 1e-6
            UVA2_umol_min = min(UVA2_umol_min, na.rm = TRUE),
            UVA2_umol_max = max(UVA2_umol_max, na.rm = TRUE),

            UVA1_umol_mean = mean(UVA1_umol_mean, na.rm = TRUE),
            UVA1_mol_day = UVA1_umol_mean * 0.0864, # 3600 * 24 * 1e-6
            UVA1_umol_min = min(UVA1_umol_min, na.rm = TRUE),
            UVA1_umol_max = max(UVA1_umol_max, na.rm = TRUE),

            UVA_umol_mean = mean(UVA_umol_mean, na.rm = TRUE),
            UVA_mol_day = UVA_umol_mean * 0.0864, # 3600 * 24 * 1e-6
            UVA_umol_min = min(UVA_umol_min, na.rm = TRUE),
            UVA_umol_max = max(UVA_umol_max, na.rm = TRUE),

            global_watt_mean = mean(global_watt_mean, na.rm = TRUE),
            global_MJ_day = global_watt_mean * 0.0864, # 3600 * 24 * 1e-6
            global_watt_min = min(global_watt_min, na.rm = TRUE),
            global_watt_max = max(global_watt_max, na.rm = TRUE),

            air_temp_C_mean = mean(air_temp_C_mean, na.rm = TRUE),
            air_temp_C_min = min(air_temp_C_min, na.rm = TRUE),
            air_temp_C_max = max(air_temp_C_max, na.rm = TRUE),

            air_vp_mean = mean(air_vp_mean, na.rm = TRUE),
            air_vp_min = min(air_vp_min, na.rm = TRUE),
            air_vp_max = max(air_vp_max, na.rm = TRUE),

            surf_temp_C_mean = mean(surf_temp_C_mean, na.rm = TRUE),
            surf_temp_C_min = min(surf_temp_C_min, na.rm = TRUE),
            surf_temp_C_max = max(surf_temp_C_max, na.rm = TRUE),

            surf_temp_sensor_delta_C_mean = mean(surf_temp_sensor_delta_C_mean, na.rm = TRUE),
            surf_temp_sensor_delta_C_min = min(surf_temp_sensor_delta_C_min, na.rm = TRUE),
            surf_temp_sensor_delta_C_max = max(surf_temp_sensor_delta_C_max, na.rm = TRUE),

            surf_temp_sensor_delta_C_mean = mean(surf_temp_sensor_delta_C_mean, na.rm = TRUE),
            surf_temp_sensor_delta_C_min = min(surf_temp_sensor_delta_C_min, na.rm = TRUE),
            surf_temp_sensor_delta_C_max = max(surf_temp_sensor_delta_C_max, na.rm = TRUE),

            sunny_hours = sum(was_sunny, na.rm = TRUE),
            rain_mm_day = sum(rain_mm_h, na.rm = TRUE),
            n_minutes = sum(n),

            across(logged_air_temp_C_mean:air_vpd_mean, mean),
            across(logged_air_temp_C_max:air_vpd_max, max),
            across(logged_air_temp_C_min:air_vpd_min, min),

            ET_ref_FAO56_mm_day = ET_ref_FAO56_mean * 24,
            ET_ref_short_mm_day = ET_ref_short_mean * 24,
            ET_ref_tall_mm_day = ET_ref_tall_mean * 24,

            # belowground

            across(VWC_1:EC_3, mean_min_max),
            across(T_5cm:BulkEC_50cm, mean_min_max),

            # volumetric water content change
            VWC_5cm_day_start = VWC_5cm[1],
            VWC_5cm_day_end = VWC_5cm[length(VWC_5cm)],
            VWC_5cm_day_delta = VWC_5cm_day_end - VWC_5cm_day_start,

            VWC_10cm_day_start = VWC_10cm[1],
            VWC_10cm_day_end = VWC_10cm[length(VWC_10cm)],
            VWC_10cm_day_delta = VWC_10cm_day_end - VWC_10cm_day_start,

            VWC_20cm_day_start = VWC_20cm[1],
            VWC_20cm_day_end = VWC_20cm[length(VWC_20cm)],
            VWC_20cm_day_delta = VWC_20cm_day_end - VWC_20cm_day_start,

            VWC_30cm_day_start = VWC_30cm[1],
            VWC_30cm_day_end = VWC_30cm[length(VWC_30cm)],
            VWC_30cm_day_delta = VWC_30cm_day_end - VWC_30cm_day_start,

            VWC_40cm_day_start = VWC_40cm[1],
            VWC_40cm_day_end = VWC_40cm[length(VWC_40cm)],
            VWC_40cm_day_delta = VWC_40cm_day_end - VWC_40cm_day_start,

            VWC_50cm_day_start = VWC_50cm[1],
            VWC_50cm_day_end = VWC_50cm[length(VWC_50cm)],
            VWC_50cm_day_delta = VWC_50cm_day_end - VWC_50cm_day_start,

            water_mm_0to55cm_start = sum(VWC_5cm_day_start * 0.075 +
                                           VWC_10cm_day_start * 0.075 +
                                           VWC_20cm_day_start * 0.1 +
                                           VWC_30cm_day_start * 0.1 +
                                           VWC_40cm_day_start * 0.1 +
                                           VWC_50cm_day_start * 0.1) * 1e3,


            water_mm_0to55cm_end = sum(VWC_5cm_day_end * 0.075 +
                                         VWC_10cm_day_end * 0.075 +
                                         VWC_20cm_day_end * 0.1 +
                                         VWC_30cm_day_end * 0.1 +
                                         VWC_40cm_day_end * 0.1 +
                                         VWC_50cm_day_end * 0.1) * 1e3,

            water_mm_0to55cm_delta = water_mm_0to55cm_end - water_mm_0to55cm_start
  ) |>
  rename(time = time.day) |>
  mutate(date = as.Date(time), .before = "PAR_umol_mean") -> day.tb

colnames(day.tb)
str(day.tb)
day_2024_latest.tb <- day.tb

save(day_2024_latest.tb, file = "data-rda/day_2024_latest.tb.rda")

