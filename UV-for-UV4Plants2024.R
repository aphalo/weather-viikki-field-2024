library(photobiology)
library(ggpmisc)

load("data-rda/minute_2015_2023.tb.rda")

minutes.df <- subset(minute_2015_2023.tb,
                     !is.na(UVB_umol) &
                       !is.na(UVA2_umol) & !is.na(UVA1_umol) & !is.na(PAR_umol) &
                       !is.na(UVB_umol) & sun_elevation > 5 &
                       month_of_year > 4 & month_of_year < 10 )

nrow(minutes.df)

sum(minutes.df$UVB_umol > 3) / nrow(minutes.df) * 100
sum(minutes.df$UVB_umol > 1) / nrow(minutes.df) * 100
sum(minutes.df$UVB_umol < 0.25) / nrow(minutes.df) * 100
median(minutes.df$UVB_PAR, na.rm = T) * 1000
quantile(minutes.df$UVB_PAR, na.rm = T, probs = c(0.1, 0.5, 0.9)) * 1000
mean(minutes.df$UVB_PAR, na.rm = T, trim = 0.1) * 1000

