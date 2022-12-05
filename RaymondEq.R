### Raymond Equations

#Packages I think I might perhaps maybe need...
library(ggpubr)
library(anytime)
library(googlesheets4)
library(ggpmisc)
library(plyr)

library(dplyr)
library(lubridate)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)

library(zoo)
library(xts)
library(forecast)
library(googledrive)
library(streamMetabolizer)
library(readr)
library(data.table)
library(ggpubr)
library(anytime)
library(googlesheets4)
library(ggpmisc)
library(plyr)
library(dplyr)
library(lubridate)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(zoo)
library(xts)
library(forecast)
library(googledrive)
library(streamMetabolizer)
library(readr)
library(here)
library(rstan)
library(imputeTS)
library(itsmr)
library(purrr)

#download flowmeter data
WR_19.url <- "https://drive.google.com/drive/u/1/folders/1bm62_JO1dKrFPyUCz8E88K5q7IHElXPP"
WR.19.1 <- drive_get(as_id(WR_19.url))
moos.wr19_glist <- drive_ls(WR.19.1, pattern = "moos_2019_flowmeter_Q_for_R_JAA.csv")
walk(moos.wr19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
moos_WR_19.Data <- read.csv("moos_2019_flowmeter_Q_for_R_JAA.csv",
                            skip = 1, header = TRUE, na.strings=c("","NA","blank"))

moos_WR_19.Data <- moos_WR_19.Data %>% tidyr::fill(Date, .direction = ("down"))

moos_WR_19.Data <- moos_WR_19.Data %>% tidyr::fill(Time, .direction = ("down"))


moos_WR_19.Data$datetimeAK <- as.POSIXct(paste(moos_WR_19.Data$Date, moos_WR_19.Data$Time), format="%m/%d/%Y %H:%M")


moos_WR_19.Data <- moos_WR_19.Data %>%
  select(Mean.Vel..m.s., datetimeAK)

Moos_Velocity_19 <- ddply(na.omit(moos_WR_19.Data), .(datetimeAK), summarize, medianVelocity = median(as.numeric(Mean.Vel..m.s.)))

velocity.mean <- mean(Moos_Velocity_19$medianVelocity)


#download flowmeter data
WR_19.url <- "https://drive.google.com/drive/u/1/folders/1bm62_JO1dKrFPyUCz8E88K5q7IHElXPP"
WR.19.1 <- drive_get(as_id(WR_19.url))
moos.wr19_glist <- drive_ls(WR.19.1, pattern = "moos_2019_flowmeter_Q_for_R_JAA.csv")
walk(moos.wr19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
moos_WR_19.Data <- read.csv("moos_2019_flowmeter_Q_for_R_JAA.csv",
                            skip = 1, header = TRUE, na.strings=c("","NA","blank"))

moos_WR_19.Data <- moos_WR_19.Data %>% tidyr::fill(Date, .direction = ("down"))

moos_WR_19.Data <- moos_WR_19.Data %>% tidyr::fill(Time, .direction = ("down"))


moos_WR_19.Data$datetimeAK <- as.POSIXct(paste(moos_WR_19.Data$Date, moos_WR_19.Data$Time), format="%m/%d/%Y %H:%M")


moos_WR_19.Data <- moos_WR_19.Data %>%
  select(Depth..cm., datetimeAK)

Moos_depth_19 <- ddply(na.omit(moos_WR_19.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))


depth.moos.mean <- mean(Moos_depth_19$meanDepth) / 100



#slope
moos.slope <- 3.0369/100

#discharge
moos <- read.csv(here("outputs","moos.comb.csv"))
moos.19 <- moos %>% filter(datetimeAK> "2019-05-08 11:15:00" & datetimeAK < "2019-12-08 11:15:00 12:00:00")

moos.discharge <- mean(na.omit(moos.19$discharge))












############# Continuous Depth ############

#All PT data...
PT.2019.url <- "https://drive.google.com/drive/u/1/folders/1VdtpYHtfxSqp2DRyWTCu4NorvQ5bx_i4"
pt.19.1 <- drive_get(as_id(PT.2019.url))
pt.19_glist <- drive_ls(pt.19.1, pattern = "all.pt.2019.csv")
walk(pt.19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
pt.2019.Data <- read.csv("all.pt.2019.csv",
                         skip = 0, header = TRUE)


#MOOS and VAUL do not have a second PT in 2019
moos.data1 <- pt.2019.Data %>% filter(Site == "MOOS1")
moos.data1$DateTime <- as.POSIXct(paste(moos.data1$DateTimeGMT), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
moos.2019.pt <- moos.data1

# # Moos 2019:

#download flowmeter data
WR_19.url <- "https://drive.google.com/drive/u/1/folders/1bm62_JO1dKrFPyUCz8E88K5q7IHElXPP"
WR.19.1 <- drive_get(as_id(WR_19.url))
moos.wr19_glist <- drive_ls(WR.19.1, pattern = "moos_2019_flowmeter_Q_for_R_JAA.csv")
walk(moos.wr19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
moos_WR_19.Data <- read.csv("moos_2019_flowmeter_Q_for_R_JAA.csv",
                            skip = 1, header = TRUE, na.strings=c("","NA","blank"))

moos_WR_19.Data <- moos_WR_19.Data %>% tidyr::fill(Date, .direction = ("down"))

moos_WR_19.Data <- moos_WR_19.Data %>% tidyr::fill(Time, .direction = ("down"))


moos_WR_19.Data$datetimeAK <- as.POSIXct(paste(moos_WR_19.Data$Date, moos_WR_19.Data$Time), format="%m/%d/%Y %H:%M")


moos_WR_19.Data <- moos_WR_19.Data %>%
  select(Depth..cm., datetimeAK)

Moos_depth_19_WR <- ddply(na.omit(moos_WR_19.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# moos_wr.lm <- lm(meanDepth~datetimeAK, Moos_depth_19)


Moos_depth_19_WR <- setDT(Moos_depth_19_WR)

Moos_depth_19_WR <- Moos_depth_19_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

moos.2019.pt <- moos.2019.pt %>%
  dplyr::rename(datetimeAK = DateTime)

moos.2019.pt <- moos.2019.pt %>%
  dplyr::rename(AvgAbsDepth = AbsPTDepth)

moos.2019.pt <- setDT(moos.2019.pt)

setDT(Moos_depth_19_WR)
setDT(moos.2019.pt)

moos.2019.pt$datetimeAK1 <- moos.2019.pt$datetimeAK

setkey( moos.2019.pt, datetimeAK )
setkey( Moos_depth_19_WR, datetimeAK )

#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_moos19 <- moos.2019.pt[ Moos_depth_19_WR, roll = "nearest" ]

rounded.dates_moos19_WR_PT <- rounded.dates_moos19 %>%
  select(datetimeAK, AvgAbsDepth, meanDepth)

#convert to meters
rounded.dates_moos19_WR_PT$meanDepth <- rounded.dates_moos19_WR_PT$meanDepth /100

moos_pt_wr_graph <- ggplot(rounded.dates_moos19_WR_PT, aes(AvgAbsDepth, meanDepth)) +
  geom_point()
# Add regression line
moos_pt_wr_graph + geom_smooth(method = lm) + xlab("Depth (PT)") +ylab ("Depth (WR)")

ggsave("plots/moos_pt_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


moos19_depth_mod <- lm(meanDepth~AvgAbsDepth, data = rounded.dates_moos19_WR_PT)

summary(moos19_depth_mod)

plot(meanDepth~AvgAbsDepth, data = rounded.dates_moos19_WR_PT)
abline(moos19_depth_mod)

#extract slope of model and develop rating curve
moos.2019.pt$RatingCurveDepth <-moos19_depth_mod$coefficients[1]+(moos19_depth_mod$coefficients[2])*moos.2019.pt$AvgAbsDepth



moos.2019.pt$date <- as.Date(moos.2019.pt$datetimeAK)

daily.mean.depth.moos <- moos.2019.pt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveDepth), list(name = mean))


#########





############# Continuous Velocity ############

#All PT data...
PT.2019.url <- "https://drive.google.com/drive/u/1/folders/1VdtpYHtfxSqp2DRyWTCu4NorvQ5bx_i4"
pt.19.1 <- drive_get(as_id(PT.2019.url))
pt.19_glist <- drive_ls(pt.19.1, pattern = "all.pt.2019.csv")
walk(pt.19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
pt.2019.Data <- read.csv("all.pt.2019.csv",
                         skip = 0, header = TRUE)


#MOOS and VAUL do not have a second PT in 2019
moos.data1 <- pt.2019.Data %>% filter(Site == "MOOS1")
moos.data1$DateTime <- as.POSIXct(paste(moos.data1$DateTimeGMT), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
moos.2019.pt <- moos.data1

# # Moos 2019:

#download flowmeter data
WR_19.url <- "https://drive.google.com/drive/u/1/folders/1bm62_JO1dKrFPyUCz8E88K5q7IHElXPP"
WR.19.1 <- drive_get(as_id(WR_19.url))
moos.wr19_glist <- drive_ls(WR.19.1, pattern = "moos_2019_flowmeter_Q_for_R_JAA.csv")
walk(moos.wr19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
moos_WR_19.Data <- read.csv("moos_2019_flowmeter_Q_for_R_JAA.csv",
                            skip = 1, header = TRUE, na.strings=c("","NA","blank"))

moos_WR_19.Data <- moos_WR_19.Data %>% tidyr::fill(Date, .direction = ("down"))

moos_WR_19.Data <- moos_WR_19.Data %>% tidyr::fill(Time, .direction = ("down"))


moos_WR_19.Data$datetimeAK <- as.POSIXct(paste(moos_WR_19.Data$Date, moos_WR_19.Data$Time), format="%m/%d/%Y %H:%M")


moos_WR_19.Data <- moos_WR_19.Data %>%
  select(Mean.Vel..m.s., datetimeAK)

#USE MEDIAN to make rating curve
Moos_velocity_19_WR <- ddply(na.omit(moos_WR_19.Data), .(datetimeAK), summarize, meanVelocity = median(as.numeric(Mean.Vel..m.s.)))

# moos_wr.lm <- lm(meanDepth~datetimeAK, Moos_depth_19)


Moos_velocity_19_WR <- setDT(Moos_velocity_19_WR)

Moos_velocity_19_WR <- Moos_velocity_19_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

moos.2019.pt <- moos.2019.pt %>%
  dplyr::rename(datetimeAK = DateTime)

moos.2019.pt <- moos.2019.pt %>%
  dplyr::rename(AvgAbsDepth = AbsPTDepth)

moos.2019.pt <- setDT(moos.2019.pt)

setDT(Moos_velocity_19_WR)
setDT(moos.2019.pt)

moos.2019.pt$datetimeAK1 <- moos.2019.pt$datetimeAK

setkey( moos.2019.pt, datetimeAK )
setkey( Moos_velocity_19_WR, datetimeAK )

#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_moos19 <- moos.2019.pt[ Moos_velocity_19_WR, roll = "nearest" ]

rounded.dates_moos19_WR_PT <- rounded.dates_moos19 %>%
  select(datetimeAK, AvgAbsDepth, meanVelocity)

moos_pt_wr_graph <- ggplot(rounded.dates_moos19_WR_PT, aes(AvgAbsDepth, meanVelocity)) +
  geom_point()
# Add regression line
moos_pt_wr_graph + geom_smooth(method = lm) + xlab("Depth (PT)") +ylab ("Velocity (WR)")

ggsave("plots/moos_pt_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


moos19_velocity_mod <- lm(meanVelocity~AvgAbsDepth, data = rounded.dates_moos19_WR_PT)

summary(moos19_velocity_mod)

plot(meanVelocity~AvgAbsDepth, data = rounded.dates_moos19_WR_PT)
abline(moos19_velocity_mod)

#extract slope of model and develop rating curve
moos.2019.pt$RatingCurveVelocity <-moos19_velocity_mod$coefficients[1]+(moos19_velocity_mod$coefficients[2])*moos.2019.pt$AvgAbsDepth



moos.2019.pt$date <- as.Date(moos.2019.pt$datetimeAK)

daily.mean.velocity.moos <- moos.2019.pt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveVelocity), list(name = mean))


######### Daily Q ##########

#discharge
moos <- read.csv(here("outputs","moos.comb.csv"))
moos.19 <- moos %>% filter(datetimeAK> "2019-05-08 11:15:00" & datetimeAK < "2019-12-08 11:15:00 12:00:00")



moos.19$date <- as.Date(moos.19$datetimeAK)


moos.discharge <- na.omit(moos.19) %>%
  group_by(date) %>%
  summarise_at(vars(discharge), list(name = mean)) %>% rename(discharge = name)

daily.mean.velocity.moos <- daily.mean.velocity.moos %>% rename(velocity = name)
daily.mean.depth.moos <- daily.mean.depth.moos %>% rename(depth = name)



data <- merge(daily.mean.velocity.moos,daily.mean.depth.moos, by = "date")
data <- merge(data, moos.discharge, by = "date")
view(data)

#Equations

data$k600.1 <-  ((data$velocity * moos.slope)^(0.89)) * (data$depth^(0.54)) * 5037

data$k600.3 <-  1162 * (moos.slope^(0.77)) * (data$velocity^(0.85))

data$k600.4 <- ((data$velocity*moos.slope)^(0.76)) * 951.5

data$k600.5 <- (data$velocity*moos.slope) * 2841 + 2.02

data$k600.6 <- 929*((data$velocity*moos.slope)^(0.75)) * data$discharge^(0.011)

data$k600.7 <- 4725*((data$velocity*moos.slope)^(0.86)) * (data$discharge^(-0.14)) *(data$depth^(0.66))

view(data)

# ALL YEARS
K.plot1 <- data %>% 
  select(date, starts_with('k600')) %>%
  gather(type, k600.value, starts_with('k600')) %>%  ggplot(aes(x=date, y=k600.value, color=type)) + geom_line() + theme_bw() +
  scale_color_discrete('variable') + labs(title = "K600, Moose 2019. From Raymond Equations ")
K.plot1
