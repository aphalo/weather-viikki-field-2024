# compare Viikki data to FMI Kumpula station

library(dplyr)
library(lubridate)
library(photobiology)
library(photobiologyPlants)
library(ggpmisc)
library(ggdensity)

# NED TO CHECK FMI days vs. UTC days!

# read FMI data for Kumpula (10 min means)
# data downloaded from https://en.ilmatieteenlaitos.fi/download-observations
FMI_Kumpula_2024.df <-
  read.csv("FMI-weather-data-2024/Helsinki Kumpula_ 1.1.2024 - 31.12.2024_b7223f8a-1c06-4c73-8378-4d08e1e02f3a.csv",
           colClasses = "character")

FMI_Kumpula_2024.df |>
  filter(Aika..UTC. == "00:00") |>
  transmute(date.chr = paste(Vuosi, Kuukausi, Päivä, sep = "-"),
            time = ymd_hm(paste(date.chr, Aika..UTC., sep = " "), tz = "UTC"),
            air_temp_C_mean.fmi = as.numeric(Ilman.keskilämpötila...C.),
            rain_mm_day.fmi = as.numeric(gsub("-1", "0.0", Sademäärä..mm., fixed = TRUE)),
            snow_depth.fmi = as.numeric(gsub("-1", "0.0", Lumensyvyys..cm., fixed = TRUE))
            ) -> Kumpula_2024.df

# Read Viikki data (1 min means)

load("data-rda/hour_2024_latest.tb.rda")

hour_2024_latest.tb |>
  select(time,
         day_of_year,
         month_of_year,
         air_temp_C_mean,
         rain_mm_h) -> temp.df

temp.df |>
  group_by(day_of_year, month_of_year) |>
  summarise(time = trunc(time[1], "day"),
            air_temp_C_mean = mean(air_temp_C_mean),
            rain_mm_day = sum(rain_mm_h),
            n = n()) |>
  ungroup() |>
  subset(n == 24) -> Viikki_1d_2024.tb

Viikki_Kumpula_1d_2024.tb <-
  right_join(Kumpula_2024.df, Viikki_1d_2024.tb, by = "time")

Viikki_Kumpula_1d_2024.tb |>
  na.omit() |>
  summarise(start = min(time),
            end = max(time),
            max_rain_day = max(rain_mm_day),
            max_rain_day.fmi = max(rain_mm_day.fmi),
            rain_mm_season = sum(rain_mm_day),
            rain_mm_season.fmi = sum(rain_mm_day.fmi),
            rain.days = sum(rain_mm_day > 0.1),
            rain.days.fmi = sum(rain_mm_day.fmi > 0.1),
            hours = sum(n),
            days = n())


rain_rel.fig <-
  ggplot(subset(Viikki_Kumpula_1d_2024.tb,
                rain_mm_day.fmi > 3 & rain_mm_day > 3),
         aes(time, rain_mm_day.fmi / rain_mm_day)) +
#  geom_abline(slope = 1, colour = "darkgreen") +
  geom_point(aes(colour = month.abb[month_of_year]))
rain_rel.fig

# 1 day is not too short a period!!
# no surprise that correlation is rather low
# however the slope > 2 indicates very strong bias!!
rain.fig <-
  ggplot(subset(Viikki_Kumpula_1d_2024.tb,
                rain_mm_day.fmi > 1 & rain_mm_day > 1),
       aes(rain_mm_day.fmi, rain_mm_day)) +
  geom_abline(slope = 1, colour = "darkgreen") +
  geom_point(aes(colour = month.abb[month_of_year]))

rain.fig +
  stat_poly_line(aes(colour = month.abb[month_of_year]), se = FALSE)

rain.fig +
  stat_poly_line(formula = y ~ x + 0) +
  stat_poly_eq(formula = y ~ x + 0)

rain.fig +
  stat_ma_line() +
  stat_ma_eq()

ggplot(subset(Viikki_Kumpula_1d_2024.tb,
              rain_mm_day.fmi > 2 & rain_mm_day > 2),
       aes(rain_mm_day.fmi, rain_mm_day)) +
  stat_hdr(method = "mvnorm") +
  stat_correlation(use_label("R", "R.CI"), small.r = TRUE)

# the error is huge
ggplot(subset(Viikki_Kumpula_1d_2024.tb,
              rain_mm_day.fmi > 2 & rain_mm_day > 2), aes(rain_mm_day.fmi, rain_mm_day)) +
  geom_point() +
  stat_quant_line(quantiles = 0.5) +
  stat_quant_eq(quantiles = 0.5) +
  geom_abline(slope = 1, colour = "darkgreen")

# the error is huge
ggplot(subset(Viikki_Kumpula_10min_2024.tb,
              rain_mm_h.fmi > 0.1 & rain_mm_h > 0.1), aes(rain_mm_h.fmi, rain_mm_h)) +
  stat_hdr() +
  stat_quant_line() +
  stat_quant_eq() +
  stat_correlation(label.x = "right", label.y = "bottom") +
  facet_wrap(facets = vars(month(time))) +
  scale_x_log10() +
  scale_y_log10() +
  expand_limits(x = c(5e-2, 100), y = c(5e-2, 100))
