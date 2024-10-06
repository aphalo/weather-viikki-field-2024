# Plot data for the most recent n days available

library(ggspectra)

load("data-rda/minute_2024_latest.tb.rda")

n_days <- 7

ggplot(tail(minute_2024_latest.tb, 24 * 60 * n_days), aes(time, global_watt)) +
  geom_line()

ggplot(tail(minute_2024_latest.tb, 24 * 60 * n_days), aes(time, PAR_umol)) +
  geom_line()

ggplot(tail(minute_2024_latest.tb, 24 * 60 * n_days), aes(time, UVB_umol)) +
  geom_line()

ggplot(tail(minute_2024_latest.tb, 24 * 60 * n_days), aes(time, UVA2_umol)) +
  geom_line()

ggplot(tail(minute_2024_latest.tb, 24 * 60 * n_days), aes(time, UVA1_umol)) +
  geom_line()

ggplot(tail(minute_2024_latest.tb, 24 * 60 * n_days), aes(time, blue_umol)) +
  geom_line()

ggplot(tail(minute_2024_latest.tb, 24 * 60 * n_days), aes(time, air_temp_C)) +
  geom_line()

ggplot(tail(minute_2024_latest.tb, 24 * 60 * n_days), aes(time, surf_temp_C)) +
  geom_line()

ggplot(tail(minute_2024_latest.tb, 24 * 60 * n_days), aes(time, R_rel)) +
  geom_line()
