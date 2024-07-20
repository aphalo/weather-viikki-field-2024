library(photobiology)
library(photobiologyWavebands)
library(ggplot2)

files <- list.files("sensors-calibrations-2024/sglux-field/", "*\\.Rda", full.names = TRUE)

for (f in files) {
  load(f)
}

all.mspct <- collect2mspct()

spectra_irrads.tb <- q_irrad(all.mspct, c(list(PAR()), Plant_bands()), scale.factor = 1e6, attr2tb = "when.measured")

ggplot(spectra_irrads.tb, aes(when.measured, Q_PAR)) +
  geom_line()

ggplot(spectra_irrads.tb, aes(when.measured, Q_UVB.ISO)) +
  geom_line()

ggplot(spectra_irrads.tb, aes(when.measured, Q_Blue.Sellaro)) +
  geom_line()

ggplot(spectra_irrads.tb, aes(when.measured, Q_Red.Smith20)) +
  geom_line()

spectral_data_range <- range(spectra_irrads.tb$when.measured)
spectral_data_range

load("data-rda/second_2024_latest.tb.rda")

station_readings.tb <- subset(second_2024_latest.tb, time > spectral_data_range[1] & time < spectral_data_range[2])

ggplot(station_readings.tb, aes(time, PAR_umol)) +
  geom_line()

ggplot(station_readings.tb, aes(time, UVB_umol)) +
  geom_line()

ggplot(station_readings.tb, aes(time, UVA2_umol)) +
  geom_line()

ggplot(station_readings.tb, aes(time, UVA1_umol)) +
  geom_line()

