### Discharge - process 2021 Discarge observations ###
# press Command+Option+O to collapse all sections and get an overview of the workflow! #

#### read me ####
# this code:
# 1) loads, formats, and saves Wading rod and YSI (salt slug) discharge data from raw data input 
# 2) plots Wading rod vs. YSI (salt slug) analysis directly from R 

### Load Libraries ###
library(readr)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(gridExtra)
library(readxl)
library(zoo)
library(here)

### This is my attempt at doing our discharge from shovr ###
setwd(here())

## Read in data from Google Drive
SHOVurl <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSVSc2AC_Qpe6indix73QBOWWcEowSuke2Dscl3uI8PZXgCoaHZA6niK7dLXWqPkW03c4KOYosGKvSV/pub?output=csv"
MASTurl <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTR0uJBkWWaGbxoGT7hvaJ5H4AOAmQXyLt_stT7q_PQLbKaowYN3usdzk_3nR6vxRequLitLJpnxKMh/pub?output=csv"
CRAWurl <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vR5kn4MDVx-EWtv4znCUqkTbhQzup4931gaqWla2W5W4XPw16xg965kaipudFwP9mSDw91YNUU4u9rt/pub?output=csv"


#read in shov discharge sheet
shov_discharge <- read.csv(url(SHOVurl), stringsAsFactors = FALSE)
mast_discharge <- read.csv(url(MASTurl), stringsAsFactors = FALSE) 
craw_discharge <- read.csv(url(CRAWurl), stringsAsFactors = FALSE)

# Rename columns
names(shov_discharge) <- c("Date_cat", "width_cm", "Time", "Distance_cm", "depth_cm", "Vel_m_s", "Vel_halfdepth_m_s", "Vel_twicedepth_m_s", "Velocity_mn_m_s")
names(mast_discharge) <- c("Date_cat", "width_cm", "Time", "Distance_cm", "depth_cm", "Vel_m_s", "Vel_halfdepth_m_s", "Vel_twicedepth_m_s", "Velocity_mn_m_s")
names(craw_discharge) <- c("Date_cat", "width_cm", "Time", "Distance_cm", "depth_cm", "Vel_m_s", "Vel_halfdepth_m_s", "Vel_twicedepth_m_s", "Velocity_mn_m_s")

# Input NA for missing time
shov_discharge$Time[shov_discharge$Time == ""] <- NA
mast_discharge$Time[mast_discharge$Time == ""] <- NA
craw_discharge$Time[craw_discharge$Time == ""] <- NA


# Using fill function from tidyverse
shov_discharge <- shov_discharge %>% fill(width_cm) %>%
  fill(Time) %>% fill(Date_cat)
mast_discharge <- mast_discharge %>% fill(width_cm) %>%
  fill(Time) %>% fill(Date_cat)
craw_discharge <- craw_discharge %>% fill(width_cm) %>%
  fill(Time) %>% fill(Date_cat)



## Create datetime column
# Convert integer date to Date
shov_discharge$Date <- as.Date(as.character(shov_discharge$Date_cat), format = "%y%m%d", tz = "America/Anchorage")
mast_discharge$Date <- as.Date(as.character(mast_discharge$Date_cat), format = "%y%m%d", tz = "America/Anchorage")
craw_discharge$Date <- as.Date(as.character(craw_discharge$Date_cat), format = "%y%m%d", tz = "America/Anchorage")


# Concatenate date & time
shov_discharge$datetime <- as.POSIXct(paste(shov_discharge$Date, shov_discharge$Time), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
mast_discharge$datetime <- as.POSIXct(paste(mast_discharge$Date, mast_discharge$Time), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
craw_discharge$datetime <- as.POSIXct(paste(craw_discharge$Date, craw_discharge$Time), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")


## Calculate mean velocity = meanVel_m_s
shov_discharge <- shov_discharge %>% mutate(meanVel_m_s = ifelse(is.na(Vel_m_s), mean(c(.$Vel_halfdepth_m_s, .$Vel_twicedepth_m_s)), .$Vel_m_s))
mast_discharge <- mast_discharge %>% mutate(meanVel_m_s = ifelse(is.na(Vel_m_s), mean(c(.$Vel_halfdepth_m_s, .$Vel_twicedepth_m_s)), .$Vel_m_s))
craw_discharge <- craw_discharge %>% mutate(meanVel_m_s = ifelse(is.na(Vel_m_s), mean(c(.$Vel_halfdepth_m_s, .$Vel_twicedepth_m_s)), .$Vel_m_s))


write.csv(shov_discharge, "shov_Discharge_2022.csv", row.names = FALSE)
write.csv(mast_discharge, "mast_Discharge_2022.csv", row.names = FALSE)
write.csv(craw_discharge, "craw_Discharge_2022.csv", row.names = FALSE)


## Plot depth & velocity to check for heterogeneity across the channel
depth.pl.shov <- shov_discharge %>% 
  ggplot(aes(x = Distance_cm, y = reorder(depth_cm, desc(depth_cm)))) +
  geom_point() +
  facet_wrap(~Date)
depth.pl.shov

depth.pl.mast <- mast_discharge %>% 
  ggplot(aes(x = Distance_cm, y = reorder(depth_cm, desc(depth_cm)))) +
  geom_point() +
  facet_wrap(~Date)
depth.pl.mast

craw_discharge$depth_cm <- as.numeric(craw_discharge$depth_cm)
str(craw_discharge)
depth.pl.craw <- craw_discharge %>% 
  ggplot(aes(x = Distance_cm, y = reorder(depth_cm, desc(depth_cm)))) +
  geom_point() +
  geom_line() +
  facet_wrap(~Date) 
depth.pl.craw


vel.pl.shov <- shov_discharge %>% 
  ggplot(aes(x = Distance_cm, y = Velocity_mn_m_s)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Date)
vel.pl.shov

vel.pl.mast <- mast_discharge %>% 
  ggplot(aes(x = Distance_cm, y = Velocity_mn_m_s)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Date)
vel.pl.mast

vel.pl.craw <- craw_discharge %>% 
  ggplot(aes(x = Distance_cm, y = Velocity_mn_m_s)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Date)
vel.pl.craw



## Remove data where depth < 10 cm
# Reduce influence of low flow outside of the thalweg
shov_discharge <- shov_discharge %>% filter(depth_cm >= 10) 
mast_discharge <- mast_discharge %>% filter(depth_cm >= 10)
craw_discharge <- craw_discharge %>% filter(depth_cm >= 10)


### This is the formula that Excel uses to calculate the Transect Q column ### 
#shov_transect_Q <- mutate(shov, TransectQ = diff(shov$Distance[c(1:2)] *
#  mean(shov$Depth[c(1:2)]) / 10000) * 
#mean(shov$Mean.Vel[c(1:2)] * 1000))  # I need to apply a rolling window so I can do row 1:2, 2:3, 3:4, etc. 

### Rolling window ### 
# shov #
mean.shov.depth <- zoo(c(shov_discharge$depth_cm, NA)) #mean of the depth
MEAN.DEPTH <- rollapply(as.numeric(mean.shov.depth), 2, mean, na.rm = TRUE) #rolling mean window

diff.shov <- zoo(c(shov_discharge$Distance_cm, NA))
DIFF <- rollapply(as.numeric(diff.shov), 2, diff, na.rm = TRUE)

mean.shov.vel <- zoo(c(shov_discharge$Velocity_mn_m_s, NA))
MEAN.VEL <- rollapply(as.numeric(mean.shov.vel), 2, mean, na.rm = TRUE)

### Function calculating Transect Q ###
raw_to_transect_shov <- function(raw) { 
  transect.q <- ((DIFF *
                    MEAN.DEPTH) / 10000) * 
    (MEAN.VEL * 1000)
  return(transect.q)
}

### Creating a data frame with Transect Q incorporated ###
transect.shov <- mutate(shov_discharge, TransectQ = raw_to_transect_shov())
transect.shov <- transect.shov %>% filter(TransectQ >= 0)

### Grouping by date ###
MeasuredQ_shov <- aggregate(transect.shov["TransectQ"], by = transect.shov["Date"], sum, na.rm = TRUE)
MeasuredQ_shov

# mast #
mean.mast.depth <- zoo(c(mast_discharge$depth_cm, NA))
MEAN.DEPTH.mast <- rollapply(as.numeric(mean.mast.depth), 2, mean, na.rm = TRUE)

diff.mast <- zoo(c(mast_discharge$Distance_cm, NA))
DIFF.mast <- rollapply(as.numeric(diff.mast), 2, diff, na.rm = TRUE)

mean.mast.vel <- zoo(c(mast_discharge$Velocity_mn_m_s, NA))
MEAN.VEL.mast <- rollapply(as.numeric(mean.mast.vel), 2, mean, na.rm = TRUE)

raw_to_transect_mast <- function(raw) { 
  transect.q <- ((DIFF.mast *
                    MEAN.DEPTH.mast) / 10000) * 
    (MEAN.VEL.mast * 1000)
  return(transect.q)
}

transect.mast <- mutate(mast_discharge, TransectQ = raw_to_transect_mast())
transect.mast <- transect.mast %>% filter(TransectQ >=0) # Creates a column for the rolling Transect Q

MeasuredQ_mast <- aggregate(transect.mast["TransectQ"], by = transect.mast["Date"], sum, na.rm = TRUE)
MeasuredQ_mast

# craw #
mean.craw.depth <- zoo(c(craw_discharge$depth_cm, NA))
MEAN.DEPTH.craw <- rollapply(as.numeric(mean.craw.depth), 2, mean, na.rm = TRUE)

diff.craw <- zoo(c(craw_discharge$Distance_cm, NA))
DIFF.craw <- rollapply(as.numeric(diff.craw), 2, diff, na.rm = TRUE)

mean.craw.vel <- zoo(c(craw_discharge$Velocity_mn_m_s, NA))
MEAN.VEL.craw <- rollapply(as.numeric(mean.craw.vel), 2, mean, na.rm = TRUE)

raw_to_transect_craw <- function(raw) { 
  transect.q <- ((DIFF.craw *
                    MEAN.DEPTH.craw) / 10000) * 
    (MEAN.VEL.craw * 1000)
  return(transect.q)
}

transect.craw <- mutate(craw_discharge, TransectQ = raw_to_transect_craw())
transect.craw <- transect.craw %>% filter(TransectQ >= 0)

MeasuredQ_craw <- aggregate(transect.craw["TransectQ"], by = transect.craw["Date"], sum, na.rm = TRUE)
MeasuredQ_craw

### Plot shov Wading Rod ###

shov.plot <- MeasuredQ_shov %>% ggplot(aes(x = Date, y = TransectQ)) +
  geom_point() +
  geom_line() +
  xlab("Date") + 
  ylab("MeasuredQ") +
  ggtitle("shov Wading Rod")
shov.plot
ggsave(path = "plots", file = "shov.plot", width = 5, height = 5, units = "in")


### Plot mast Wading Rod ###
mast.plot <- MeasuredQ_mast %>% ggplot(aes(x = Date, y = TransectQ)) +
  geom_point() +
  geom_line() +
  xlab("Date") + 
  ylab("MeasuredQ") +
  ggtitle("mast Wading Rod")
mast.plot
ggsave(path = "plots", file = "mast.plot", width = 5, height = 5, units = "in")

### Plot craw Wading Rod ###
craw.plot <- MeasuredQ_craw %>% ggplot(aes(x = Date, y = TransectQ)) +
  geom_point() +
  geom_line() +
  xlab("Date") + 
  ylab("MeasuredQ") +
  ggtitle("craw Wading Rod")
craw.plot
ggsave(path = "plots", file = "craw.plot", width = 5, height = 5, units = "in")

### Plot craw Wading Rod ###
craw.plot <- MeasuredQ_craw %>% ggplot(aes(x = Date, y = TransectQ)) +
  geom_point() +
  geom_line() +
  xlab("Date") + 
  ylab("MeasuredQ") +
  ggtitle("craw Wading Rod")
craw.plot
ggsave(path = "plots", file = "craw.plot", width = 5, height = 5, units = "in")
