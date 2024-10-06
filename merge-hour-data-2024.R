library(dplyr)
library(lubridate)
library(photobiology)

# clean up
rm(list = ls(pattern = "*"))
gc()

# First batch with logger program TowerViikki-024-05-fast

load("data-rda-partial/hour_2024_5_14.tb.rda")
colnames(hour_2024_5_14.tb) <- gsub("_Avg$", "", colnames(hour_2024_5_14.tb))
colnames(hour_2024_5_14.tb)
hour_2024_5_14.tb |>
  select(-year, -month_of_year, -month_name, -week_of_year, -day_of_year, -time_of_day) -> hour_2024_5_14.tb

# compute medians of the data from the three soil profile sensors
hour_2024_5_14.tb |>
  group_by(TIMESTAMP) %>%
  mutate(T_5cm = median(T_5cm_1, T_5cm_2, T_5cm_3, na.rm = TRUE),
         T_10cm = median(T_10cm_1, T_10cm_2, T_10cm_3, na.rm = TRUE),
         T_20cm = median(T_20cm_1, T_20cm_2, T_20cm_3, na.rm = TRUE),
         T_30cm = median(T_30cm_1, T_30cm_2, T_30cm_3, na.rm = TRUE),
         T_40cm = median(T_40cm_1, T_40cm_2, T_40cm_3, na.rm = TRUE),
         T_50cm = median(T_50cm_1, T_50cm_2, T_50cm_3, na.rm = TRUE),
         VWC_5cm = median(VWC_5cm_1, VWC_5cm_2, VWC_5cm_3, na.rm = TRUE),
         VWC_10cm = median(VWC_10cm_1, VWC_10cm_2, VWC_10cm_3, na.rm = TRUE),
         VWC_20cm = median(VWC_20cm_1, VWC_20cm_2, VWC_20cm_3, na.rm = TRUE),
         VWC_30cm = median(VWC_30cm_1, VWC_30cm_2, VWC_30cm_3, na.rm = TRUE),
         VWC_40cm = median(VWC_40cm_1, VWC_40cm_2, VWC_40cm_3, na.rm = TRUE),
         VWC_50cm = median(VWC_50cm_1, VWC_50cm_2, VWC_50cm_3, na.rm = TRUE),
         Ka_5cm = median(Ka_5cm_1, Ka_5cm_2, Ka_5cm_3, na.rm = TRUE),
         Ka_10cm = median(Ka_10cm_1, Ka_10cm_2, Ka_10cm_3, na.rm = TRUE),
         Ka_20cm = median(Ka_20cm_1, Ka_20cm_2, Ka_20cm_3, na.rm = TRUE),
         Ka_30cm = median(Ka_30cm_1, Ka_30cm_2, Ka_30cm_3, na.rm = TRUE),
         Ka_40cm = median(Ka_40cm_1, Ka_40cm_2, Ka_40cm_3, na.rm = TRUE),
         Ka_50cm = median(Ka_50cm_1, Ka_50cm_2, Ka_50cm_3, na.rm = TRUE),
         BulkEC_5cm = median(BulkEC_5cm_1, BulkEC_5cm_2, BulkEC_5cm_3, na.rm = TRUE),
         BulkEC_10cm = median(BulkEC_10cm_1, BulkEC_10cm_2, BulkEC_10cm_3, na.rm = TRUE),
         BulkEC_20cm = median(BulkEC_20cm_1, BulkEC_20cm_2, BulkEC_20cm_3, na.rm = TRUE),
         BulkEC_30cm = median(BulkEC_30cm_1, BulkEC_30cm_2, BulkEC_30cm_3, na.rm = TRUE),
         BulkEC_40cm = median(BulkEC_40cm_1, BulkEC_40cm_2, BulkEC_40cm_3, na.rm = TRUE),
         BulkEC_50cm = median(BulkEC_50cm_1, BulkEC_50cm_2, BulkEC_50cm_3, na.rm = TRUE)
  ) |>
  rename(time = TIMESTAMP) |>
  ungroup() |>
  select(!Ka_5cm_1:BulkEC_50cm_3) -> hour_2024_5_14x.tb

# Second batch with logger program TowerViikki-024-08-fast
# Changes only to millisecond table, but cause a memory card reset
# affecting all tables

load("data-rda-partial/hour_2024_8_9.tb.rda")
colnames(hour_2024_8_9.tb) <- gsub("_Avg$", "", colnames(hour_2024_8_9.tb))
colnames(hour_2024_8_9.tb)
hour_2024_8_9.tb |>
  select(-year, -month_of_year, -month_name, -week_of_year, -day_of_year, -time_of_day) -> hour_2024_8_9.tb

# compute medians of the data from the three soil profile sensors
hour_2024_8_9.tb |>
  group_by(TIMESTAMP) %>%
  mutate(T_5cm = median(T_5cm_1, T_5cm_2, T_5cm_3, na.rm = TRUE),
         T_10cm = median(T_10cm_1, T_10cm_2, T_10cm_3, na.rm = TRUE),
         T_20cm = median(T_20cm_1, T_20cm_2, T_20cm_3, na.rm = TRUE),
         T_30cm = median(T_30cm_1, T_30cm_2, T_30cm_3, na.rm = TRUE),
         T_40cm = median(T_40cm_1, T_40cm_2, T_40cm_3, na.rm = TRUE),
         T_50cm = median(T_50cm_1, T_50cm_2, T_50cm_3, na.rm = TRUE),
         VWC_5cm = median(VWC_5cm_1, VWC_5cm_2, VWC_5cm_3, na.rm = TRUE),
         VWC_10cm = median(VWC_10cm_1, VWC_10cm_2, VWC_10cm_3, na.rm = TRUE),
         VWC_20cm = median(VWC_20cm_1, VWC_20cm_2, VWC_20cm_3, na.rm = TRUE),
         VWC_30cm = median(VWC_30cm_1, VWC_30cm_2, VWC_30cm_3, na.rm = TRUE),
         VWC_40cm = median(VWC_40cm_1, VWC_40cm_2, VWC_40cm_3, na.rm = TRUE),
         VWC_50cm = median(VWC_50cm_1, VWC_50cm_2, VWC_50cm_3, na.rm = TRUE),
         Ka_5cm = median(Ka_5cm_1, Ka_5cm_2, Ka_5cm_3, na.rm = TRUE),
         Ka_10cm = median(Ka_10cm_1, Ka_10cm_2, Ka_10cm_3, na.rm = TRUE),
         Ka_20cm = median(Ka_20cm_1, Ka_20cm_2, Ka_20cm_3, na.rm = TRUE),
         Ka_30cm = median(Ka_30cm_1, Ka_30cm_2, Ka_30cm_3, na.rm = TRUE),
         Ka_40cm = median(Ka_40cm_1, Ka_40cm_2, Ka_40cm_3, na.rm = TRUE),
         Ka_50cm = median(Ka_50cm_1, Ka_50cm_2, Ka_50cm_3, na.rm = TRUE),
         BulkEC_5cm = median(BulkEC_5cm_1, BulkEC_5cm_2, BulkEC_5cm_3, na.rm = TRUE),
         BulkEC_10cm = median(BulkEC_10cm_1, BulkEC_10cm_2, BulkEC_10cm_3, na.rm = TRUE),
         BulkEC_20cm = median(BulkEC_20cm_1, BulkEC_20cm_2, BulkEC_20cm_3, na.rm = TRUE),
         BulkEC_30cm = median(BulkEC_30cm_1, BulkEC_30cm_2, BulkEC_30cm_3, na.rm = TRUE),
         BulkEC_40cm = median(BulkEC_40cm_1, BulkEC_40cm_2, BulkEC_40cm_3, na.rm = TRUE),
         BulkEC_50cm = median(BulkEC_50cm_1, BulkEC_50cm_2, BulkEC_50cm_3, na.rm = TRUE)
  ) |>
  rename(time = TIMESTAMP) |>
  ungroup() |>
  select(!Ka_5cm_1:BulkEC_50cm_3) -> hour_2024_8_9x.tb

# Third batch with logger program TowerViikki-024-08-fast
# Changes only to millisecond table, but cause a memory card reset
# affecting all tables

load("data-rda-partial/hour_2024_8_21.tb.rda")
colnames(hour_2024_8_21.tb) <- gsub("_Avg$", "", colnames(hour_2024_8_21.tb))
colnames(hour_2024_8_21.tb)
hour_2024_8_21.tb |>
  select(-year, -month_of_year, -month_name, -week_of_year, -day_of_year, -time_of_day) -> hour_2024_8_21.tb

# compute medians of the data from the three soil profile sensors
hour_2024_8_21.tb |>
  group_by(TIMESTAMP) %>%
  mutate(T_5cm = median(T_5cm_1, T_5cm_2, T_5cm_3, na.rm = TRUE),
         T_10cm = median(T_10cm_1, T_10cm_2, T_10cm_3, na.rm = TRUE),
         T_20cm = median(T_20cm_1, T_20cm_2, T_20cm_3, na.rm = TRUE),
         T_30cm = median(T_30cm_1, T_30cm_2, T_30cm_3, na.rm = TRUE),
         T_40cm = median(T_40cm_1, T_40cm_2, T_40cm_3, na.rm = TRUE),
         T_50cm = median(T_50cm_1, T_50cm_2, T_50cm_3, na.rm = TRUE),
         VWC_5cm = median(VWC_5cm_1, VWC_5cm_2, VWC_5cm_3, na.rm = TRUE),
         VWC_10cm = median(VWC_10cm_1, VWC_10cm_2, VWC_10cm_3, na.rm = TRUE),
         VWC_20cm = median(VWC_20cm_1, VWC_20cm_2, VWC_20cm_3, na.rm = TRUE),
         VWC_30cm = median(VWC_30cm_1, VWC_30cm_2, VWC_30cm_3, na.rm = TRUE),
         VWC_40cm = median(VWC_40cm_1, VWC_40cm_2, VWC_40cm_3, na.rm = TRUE),
         VWC_50cm = median(VWC_50cm_1, VWC_50cm_2, VWC_50cm_3, na.rm = TRUE),
         Ka_5cm = median(Ka_5cm_1, Ka_5cm_2, Ka_5cm_3, na.rm = TRUE),
         Ka_10cm = median(Ka_10cm_1, Ka_10cm_2, Ka_10cm_3, na.rm = TRUE),
         Ka_20cm = median(Ka_20cm_1, Ka_20cm_2, Ka_20cm_3, na.rm = TRUE),
         Ka_30cm = median(Ka_30cm_1, Ka_30cm_2, Ka_30cm_3, na.rm = TRUE),
         Ka_40cm = median(Ka_40cm_1, Ka_40cm_2, Ka_40cm_3, na.rm = TRUE),
         Ka_50cm = median(Ka_50cm_1, Ka_50cm_2, Ka_50cm_3, na.rm = TRUE),
         BulkEC_5cm = median(BulkEC_5cm_1, BulkEC_5cm_2, BulkEC_5cm_3, na.rm = TRUE),
         BulkEC_10cm = median(BulkEC_10cm_1, BulkEC_10cm_2, BulkEC_10cm_3, na.rm = TRUE),
         BulkEC_20cm = median(BulkEC_20cm_1, BulkEC_20cm_2, BulkEC_20cm_3, na.rm = TRUE),
         BulkEC_30cm = median(BulkEC_30cm_1, BulkEC_30cm_2, BulkEC_30cm_3, na.rm = TRUE),
         BulkEC_40cm = median(BulkEC_40cm_1, BulkEC_40cm_2, BulkEC_40cm_3, na.rm = TRUE),
         BulkEC_50cm = median(BulkEC_50cm_1, BulkEC_50cm_2, BulkEC_50cm_3, na.rm = TRUE)
  ) |>
  rename(time = TIMESTAMP) |>
  ungroup() |>
  select(!Ka_5cm_1:BulkEC_50cm_3) -> hour_2024_8_21x.tb

# bind the rows from the two data sets

bind_rows(hour_2024_5_14x.tb, hour_2024_8_9x.tb, hour_2024_8_21x.tb) -> hour_2024_latest.tb

rm(hour_2024_5_14x.tb, hour_2024_8_9x.tb, hour_2024_8_21x.tb)
gc()

# join with hourly means calculated in merge-minute-data-2024.R

load("data-rda/hour_calc_2024_latest.tb.rda")
range(hour_2024_latest.tb$time)
range(hour_calc_2024_latest.tb$time)
colnames(hour_2024_latest.tb)
colnames(hour_calc_2024_latest.tb)

hour_2024_latest.tb <- full_join(hour_calc_2024_latest.tb, hour_2024_latest.tb,
                                 by = c("time" = "time"))
range(hour_2024_latest.tb$time)
colnames(hour_2024_latest.tb)

gc()

range(hour_2024_latest.tb$time, na.rm = TRUE)
sum(is.na(hour_2024_latest.tb$time))

head(hour_2024_latest.tb)
tail(hour_2024_latest.tb)

# sometimes PC400 appends rows already in file
# we delete duplicates
hour_2024_latest.tb <-
  distinct(hour_2024_latest.tb, time, .keep_all = TRUE) %>%
  filter(!is.na(time) & !is.na(time_start) & !is.na(time_end))

anyNA(hour_2024_latest.tb$time)
range(hour_2024_latest.tb$time, na.rm = TRUE)

save(hour_2024_latest.tb, file = "data-rda/hour_2024_latest.tb.rda")

# delete file with summaries computed from 1 min data
# file.remove("data-rda/hour_calc_2024_latest.tb.rda")

