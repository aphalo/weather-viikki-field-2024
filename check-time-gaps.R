load("data-rda/millisecond_2024_8_9.tb.rda")
range(millisecond_2024_8_9.tb$TIMESTAMP)

time.diffs <- diff(millisecond_2024_8_9.tb$TIMESTAMP)
millisecond_2024_8_9.tb$TIMESTAMP[which(time.diffs == max(time.diffs)) + 0:1]

sum(diff(millisecond_2024_8_9.tb$TIMESTAMP) > 3600)

load("data-rda/second_2024_latest.tb.rda")
range(second_2024_latest.tb$time)

time.diffs <- diff(second_2024_latest.tb$time)
max(time.diffs)

second_2024_latest.tb$time[which(time.diffs == max(time.diffs)) + 0:1]

sum(diff(millisecond_2024_8_9.tb$TIMESTAMP) > 3600)
