# compare Viikki data to FMI Kumpula station

library(dplyr)
library(lubridate)
library(photobiology)
library(photobiologyPlants)
library(ggpmisc)
library(ggdensity)

# read FMI data for Kumpula (10 min means)
# data downloaded from https://en.ilmatieteenlaitos.fi/download-observations
FMI_Kumpula_2024.df <-
  read.csv("FMI-weather-data-2024/Helsinki Kumpula_ 1.1.2024 - 31.12.2024_5c660660-3993-462b-9d29-7f06983cf2ce.csv",
           colClasses = "character")

FMI_Kumpula_2024.df |>
  transmute(date.chr = paste(Vuosi, Kuukausi, Päivä, sep = "-"),
            time = ymd_hm(paste(date.chr, Aika..UTC., sep = " "), tz = "UTC"),
            wind_speed.fmi = as.numeric(Keskituulen.nopeus.keskiarvo..m.s.),
            wind_direction.fmi = as.numeric(Tuulen.suunta.keskiarvo....),
            air_temp_C.fmi = as.numeric(Ilman.lämpötila.keskiarvo...C.),
            air_RH.fmi = as.numeric(Suhteellinen.kosteus.keskiarvo....),
            air_DP.fmi = as.numeric(Kastepistelämpötila.keskiarvo...C.),
            air_vp.fmi = water_vp_sat(air_DP.fmi),
            air_pressure.fmi = as.numeric(Ilmanpaine.merenpinnan.tasolla.keskiarvo..hPa.),
            rain_mm_h.fmi = as.numeric(Sademäärä.keskiarvo..mm.),
            snow_depth.fmi = as.numeric(Lumensyvyys.keskiarvo..cm.),
            visibility_m.fmi = as.numeric(Näkyvyys.keskiarvo..m.),
            cloud_cover_chr.fmi = ifelse(Pilvisyys..1.8. == "-",
                                         "Missing (NA/8)",
                                         Pilvisyys..1.8.)
            ) -> temp.df

temp.df |>
  mutate(cloud_cover_eighths.fmi =
           as.numeric(gsub("/8)", "",
                           unlist(
                             strsplit(cloud_cover_chr.fmi,
                                      split = " (", fixed = TRUE))[c(FALSE, TRUE)],
                           fixed = TRUE))) -> Kumpula_2024.df

# Read Viikki data (1 min means)

load("data-rda/minute_2024_latest.tb.rda")

start_time <- minute_2024_latest.tb[["time"]][1]
minute(start_time) <- round(minute(start_time), digits = -1)
start_time

minute_2024_latest.tb |>
  mutate(time,
         time_since_start = time - start_time,
         period_10min = (as.numeric(time_since_start) - 1L) %/% 10L + 1L) |>
  group_by(period_10min) |>
  summarize(time = max(time),
            across(time_of_day_utc:rain_mm_h, mean),
            n = n()) |>
  ungroup() |>
  subset(n == 10) -> Viikki_10min_2024.tb


Viikki_Kumpula_10min_2024.tb <-
  right_join(Kumpula_2024.df, Viikki_10min_2024.tb, by = "time")

# good match
ggplot(Viikki_Kumpula_10min_2024.tb, aes(air_temp_C.fmi, air_temp_C)) +
  stat_hdr() +
  stat_quant_line() +
  stat_quant_eq()

# good match
ggplot(Viikki_Kumpula_10min_2024.tb, aes(air_DP.fmi, air_DP)) +
  stat_hdr() +
  stat_quant_line() +
  stat_quant_eq()

# good match
ggplot(Viikki_Kumpula_10min_2024.tb, aes(air_vp.fmi, air_vp)) +
  stat_hdr() +
  stat_quant_line() +
  stat_quant_eq()

# strong variation but rather small bias
ggplot(Viikki_Kumpula_10min_2024.tb, aes(air_RH.fmi, air_RH)) +
  stat_hdr() +
  stat_quant_line() +
  stat_quant_eq()

# good match
ggplot(Viikki_Kumpula_10min_2024.tb, aes(air_pressure.fmi, air_pressure)) +
  stat_hdr() +
  stat_quant_line() +
  stat_quant_eq()

# very different measurement height!
# Higher elevation in Kumpula -> >2x faster speeds
ggplot(Viikki_Kumpula_10min_2024.tb, aes(wind_speed.fmi, wind_speed)) +
  stat_hdr() +
  stat_quant_line() +
  stat_quant_eq()

# very different measurement height and different obstacles!
#
ggplot(Viikki_Kumpula_10min_2024.tb, aes(wind_direction.fmi, wind_direction)) +
  stat_hdr() +
  stat_correlation(use_label("R", "R.CI"), small.r = TRUE)

# rain measurement have problems!!
# frequency distributions are different...
ggplot(subset(Viikki_Kumpula_10min_2024.tb, rain_mm_h > 0.1), aes(rain_mm_h)) +
  geom_histogram()

ggplot(subset(Viikki_Kumpula_10min_2024.tb, rain_mm_h.fmi > 0.1), aes(rain_mm_h.fmi)) +
  geom_histogram()

# 10 min is too short a period!!
# no surprise that correlation is rather low
# however the slope > 2 indicates very strong bias!!
ggplot(Viikki_Kumpula_10min_2024.tb, aes(rain_mm_h.fmi, rain_mm_h)) +
  stat_hdr(method = "mvnorm") +
  stat_correlation(use_label("R", "R.CI"), small.r = TRUE)

ggplot(subset(Viikki_Kumpula_10min_2024.tb,
              rain_mm_h.fmi > 0.1 & rain_mm_h > 0.1), aes(rain_mm_h.fmi, rain_mm_h)) +
  stat_hdr(method = "mvnorm") +
  stat_correlation(use_label("R", "R.CI"), small.r = TRUE)

# the error is huge
ggplot(subset(Viikki_Kumpula_10min_2024.tb,
              rain_mm_h.fmi > 0.1 & rain_mm_h > 0.1), aes(rain_mm_h.fmi, rain_mm_h)) +
  stat_hdr(method = "mvnorm") +
  stat_quant_line(quantiles = 0.5) +
  stat_quant_eq(quantiles = 0.5) +
  geom_abline(slope = 1, colour = "darkgreen") +
  facet_wrap(facets = vars(month(time))) +
  expand_limits(x = 30, y = 30)

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
