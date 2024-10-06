library(lubridate)
library(dplyr)

load("data-rda/second_calc_2024_latest.tb.rda")
colnames(second_calc_2024_latest.tb)

second_calc_2024_latest.tb |>
  filter(time >= ymd_hm("2024-08-20 12:35") & time < ymd_hm("2024-08-20 13:20")) |>
  select(!time_start:sun_azimuth) |>
  select(time:UVA2fc_umol_max) |>
  select(!contains(c("max", "min"))) |>
  print() -> fast_sensors.tb

colnames(fast_sensors.tb) <- gsub("_umol_mean", "", colnames(fast_sensors.tb))


load("data-rda/second_2024_latest.tb.rda")
colnames(second_2024_latest.tb)

second_2024_latest.tb |>
  filter(time >= ymd_hm("2024-08-20 12:35") & time < ymd_hm("2024-08-20 13:20")) |>
  select(!series_start & !day_of_year:sun_azimuth) |>
  select(time:UVB_umol) |>
  print() -> slow_sensors.tb

colnames(slow_sensors.tb) <- gsub("_umol_mean", "", colnames(fast_sensors.tb))

all_sensors.tb <- left_join(fast_sensors.tb, slow_sensors.tb, by = "time")

all_sensors.tb |>
  transmute(time = time,
            k.PAR = PAR / PAR_umol_CS,
            k.blue = bluef / blue_umol,
            k.blue.sellaro = bluef_sellaro / blue_sellaro_umol,
            k.UVA = UVAf / UVA_umol,
            k.UVB = UVBf / UVB_umol,
            Kc.UVA1 = UVA1f / UVA1_umol,
            Kc.UVA2 = UVAf / UVA2_umol) -> cal_factors.tb

mean_min_max <- list(
  mean = ~mean(.x, na.rm = FALSE),
  #  mean.trimmed = ~mean(.x, trim = 1/3, na.rm = FALSE),
  #  median = ~median(.x, na.rm = FALSE),
  min = ~min(.x, na.rm = FALSE),
  max = ~max(.x, na.rm = FALSE)
)

cal_factors.tb |>
  summarize(across(k.PAR:Kc.UVA2, mean_min_max)) |> t()
