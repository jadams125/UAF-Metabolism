### This script reads in dilution gauging for 2021*** data from googledrive and plots breakthrough curves and calculates discharge #

library(readxl)
library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(gridExtra)
library(tidyr)
library(plyr)


### Import data ###
shov <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSDxW4D6QZIWdlsZw6tBsFwcURqi6YOUjSuPlxC9_r039CMkgIm37OyOTYQ3jTxgJpr8DZdTwS3FQPk/pub?output=csv"
mast <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTyzhi1902o9KO1ljc90SpskESz6v-Tmuo-5oBTs2t6iKcsIvL5Mxrqzz1HdLSkaAyjZ4eekAgcVsmS/pub?output=csv"
craw <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQx2vdNgkX7Sq7XTStTDhBpFsealQyQkSvfE-wo2-PWrnoJkQV8Ys4RJWaWALQqE5pqPQQywcgHZRn4/pub?output=csv"


shov_Dilution <- read.csv(url(shov))
mast_Dilution <- read.csv(url(mast))
craw_Dilution <- read.csv(url(craw))


### Convert DateTime ###
shov_Dilution$Date <- as.Date(as.character(shov_Dilution$Date), format = "%y%m%d", tz = "America/Anchorage")
shov_Dilution$DateTime <- as.POSIXct(paste(shov_Dilution$Date, shov_Dilution$Timestamp), format = "%Y-%m-%d %H:%M:%S", tz = "America/Anchorage")
mast_Dilution$Date <- as.Date(as.character(mast_Dilution$Date), format = "%y%m%d", tz = "America/Anchorage")
mast_Dilution$DateTime <- as.POSIXct(paste(mast_Dilution$Date, mast_Dilution$Timestamp), format = "%Y-%m-%d %H:%M:%S", tz = "America/Anchorage")
craw_Dilution$Date <- as.Date(as.character(craw_Dilution$Date), format = "%y%m%d", tz = "America/Anchorage")
craw_Dilution$DateTime <- as.POSIXct(paste(craw_Dilution$Date, craw_Dilution$Timestamp), format = "%Y-%m-%d %H:%M:%S", tz = "America/Anchorage")




### Rename Column Headers ###
names(shov_Dilution) <- c("Date", "Timestamp", "SPcond", "Difference", "Temp", "Comment", "Site", "Folder", "ID", "Cond", "Volume", "TimeInterval", "Batch", "DateTime")
shov_Dilution <- shov_Dilution %>% fill(Cond) %>% fill(Volume)
names(mast_Dilution) <- c("Date", "Timestamp", "SPcond", "Difference", "Temp", "Comment", "Site", "Folder", "ID", "Cond", "Volume", "TimeInterval", "Batch", "DateTime")
mast_Dilution <- mast_Dilution %>% fill(Cond) %>% fill(Volume)
names(craw_Dilution) <- c("Date", "Timestamp", "SPcond", "Difference", "Temp", "Comment", "Site", "Folder", "ID", "Cond", "Volume", "TimeInterval", "Batch", "DateTime")
craw_Dilution <- craw_Dilution %>% fill(Cond) %>% fill(Volume)



### Formula ###
# shov Discharge # 
for (i in shov_Dilution) {
  shov_date <- ddply(shov_Dilution, .(Date), summarize, daily_mean_SP = mean(SPcond))
  shov_volume <- aggregate(shov_Dilution["Volume"], by = shov_Dilution["Date"], mean) 
  shov_cond <- aggregate(shov_Dilution["Cond"], by = shov_Dilution["Date"], mean)
  shov_num <- shov_volume$Volume/1000 * shov_cond$Cond
  shov_final <- mutate(shov_date, num = shov_num)
  shov_sum_difference <- ddply(shov_Dilution,. (Date), summarize, daily_sum_SP = sum(Difference))
  shov_Q <- shov_final$num / shov_sum_difference$daily_sum_SP
  shov_MeasuredQ <- mutate(shov_date, MeasuredQ = shov_Q)
}
shov_MeasuredQ
# mast Discharge # 
for (i in mast_Dilution) {
  mast_date <- ddply(mast_Dilution, .(Date), summarize, daily_mean_SP = mean(SPcond))
  mast_volume <- aggregate(mast_Dilution["Volume"], by = mast_Dilution["Date"], mean) 
  mast_cond <- aggregate(mast_Dilution["Cond"], by = mast_Dilution["Date"], mean)
  mast_num <- mast_volume$Volume/1000 * mast_cond$Cond
  mast_final <- mutate(mast_date, num = mast_num)
  mast_sum_difference <- ddply(mast_Dilution,. (Date), summarize, daily_sum_SP = sum(Difference))
  mast_Q <- mast_final$num / mast_sum_difference$daily_sum_SP
  mast_MeasuredQ <- mutate(mast_date, MeasuredQ = mast_Q)
}
mast_MeasuredQ
# craw Discharge # 
for (i in craw_Dilution) {
  craw_date <- ddply(craw_Dilution, .(Date), summarize, daily_mean_SP = mean(SPcond))
  craw_volume <- aggregate(craw_Dilution["Volume"], by = craw_Dilution["Date"], mean) 
  craw_cond <- aggregate(craw_Dilution["Cond"], by = craw_Dilution["Date"], mean)
  craw_num <- craw_volume$Volume/1000 * craw_cond$Cond
  craw_final <- mutate(craw_date, num = craw_num)
  craw_sum_difference <- ddply(craw_Dilution,. (Date), summarize, daily_sum_SP = sum(Difference))
  craw_Q <- craw_final$num / craw_sum_difference$daily_sum_SP
  craw_MeasuredQ <- mutate(craw_date, MeasuredQ = craw_Q)
}
craw_MeasuredQ

# plot shov #
ggplot(shov_MeasuredQ) +
  geom_point(aes(x = Date, y = MeasuredQ), size = 3) + 
  ylim(0, 2000) +
  ggtitle("shovel Salt Dilution")

break.through.shov <- shov_Dilution %>% 
  ggplot(aes(x = Timestamp, y = SPcond)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~Date)
break.through.shov
?geom_histogram
# Plot mast #
ggplot(mast_MeasuredQ) +
  geom_point(aes(x = Date, y = MeasuredQ), size = 3) + 
  ylim(0, 3000) +
  ggtitle("mastodon Salt Dilution ")

break.through.mast <- mast_Dilution %>% 
  ggplot(aes(x = Timestamp, y = SPcond)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~Date) +
  ylim(50, 140)
break.through.mast

# Plot craw #
ggplot(craw_MeasuredQ) +
  geom_point(aes(x = Date, y = MeasuredQ), size = 3) + 
  ylim(0, 6000) +
  ggtitle("crawford Salt Dilution ")

break.through.craw <- craw_Dilution %>% 
  ggplot(aes(x = Timestamp, y = SPcond)) +
  geom_point() + 
  facet_wrap(~Date)
break.through.craw



write.csv(shov_MeasuredQ, 'shov_MeasuredQ.csv')

