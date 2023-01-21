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

############### MOOSE 2019 ############ 


#slope
moos.slope <- 3.0369/100


Q_2019 <- read.csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_Discharge/Predicted_Discharge/2019/Q_2019.csv")
Q_2019$DateTime <-  force_tz(as.POSIXct(ymd_hms(Q_2019$DateTime)), tz = "America/Anchorage")
#### Breaking up into sites ####
MOOS_Q_2019 <- Q_2019 %>% filter(Site == "MOOS")

MOOS.2019.Q <- MOOS_Q_2019 %>%
  select(DateTime, Q, Site)









############# Continuous Depth ############



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

# moos.2019.pt <- moos.2019.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# moos.2019.pt <- moos.2019.pt %>%
#   mutate(across(c(AvgAbsDepth),
#                 ~ifelse(datetimeAK >= "2019-08-26 02:00:00" & datetimeAK <= "2019-09-08 12:15:00", NA, .)))



moos.2019.q.dt <- MOOS.2019.Q
moos.2019.q.dt <- moos.2019.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(Moos_depth_19_WR)
setDT(moos.2019.q.dt)

moos.2019.q.dt$datetimeAK1 <- moos.2019.q.dt$datetimeAK

setkey( moos.2019.q.dt, datetimeAK )
setkey( Moos_depth_19_WR, datetimeAK )

#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_moos19 <- moos.2019.q.dt[ Moos_depth_19_WR, roll = "nearest" ]

rounded.dates_moos19_WR_Q <- rounded.dates_moos19 %>% rename(discharge = Q) %>% 
  select(datetimeAK, discharge, meanDepth)

#convert to meters
rounded.dates_moos19_WR_Q$meanDepth <- rounded.dates_moos19_WR_Q$meanDepth /100

moos_pt_wr_graph <- ggplot(rounded.dates_moos19_WR_Q, aes(discharge, meanDepth)) +
  geom_point()
# Add regression line
moos_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("Depth (WR)")

ggsave("plots/moos_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


moos19_depth_mod <- lm(meanDepth~discharge, data = rounded.dates_moos19_WR_Q)

summary(moos19_depth_mod)

plot(meanDepth~discharge, data = rounded.dates_moos19_WR_Q)
# abline(moos19_depth_mod)

#extract slope of model and develop rating curve
moos.2019.q.dt$RatingCurveDepth <- moos19_depth_mod$coefficients[1]+(moos19_depth_mod$coefficients[2])*moos.2019.q.dt$Q




moos.2019.q.dt$date <- as.Date(moos.2019.q.dt$datetimeAK)

daily.mean.depth.moos <- moos.2019.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveDepth), list(name = mean))



#########




############# Continuous Velocity ############



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

Moos_V_19_WR <- ddply(na.omit(moos_WR_19.Data), .(datetimeAK), summarize, meanVelocity = mean(as.numeric(Mean.Vel..m.s.)))

# moos_wr.lm <- lm(meanDepth~datetimeAK, Moos_depth_19)


Moos_V_19_WR <- setDT(Moos_V_19_WR)

Moos_V_19_WR <- Moos_V_19_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

# moos.2019.pt <- moos.2019.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# moos.2019.pt <- moos.2019.pt %>%
#   mutate(across(c(AvgAbsV),
#                 ~ifelse(datetimeAK >= "2019-08-26 02:00:00" & datetimeAK <= "2019-09-08 12:15:00", NA, .)))



moos.2019.q.dt <- MOOS.2019.Q
moos.2019.q.dt <- moos.2019.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(Moos_V_19_WR)
setDT(moos.2019.q.dt)

moos.2019.q.dt$datetimeAK1 <- moos.2019.q.dt$datetimeAK

setkey( moos.2019.q.dt, datetimeAK )
setkey( Moos_V_19_WR, datetimeAK )

#WR was taken when EXO out of water. round V point to nearest in data record
rounded.dates_moos19 <- moos.2019.q.dt[ Moos_V_19_WR, roll = "nearest" ]

rounded.dates_moos19_WR_Q <- rounded.dates_moos19  %>% rename (discharge = Q) %>% select(datetimeAK, discharge, meanVelocity)


moos_pt_wr_graph <- ggplot(rounded.dates_moos19_WR_Q, aes(discharge, meanVelocity)) +
  geom_point()
# Add regression line
moos_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("V (WR)")

ggsave("plots/moos_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


moos19_V_mod <- lm(meanVelocity~discharge, data = rounded.dates_moos19_WR_Q)

summary(moos19_V_mod)

plot(meanVelocity~discharge, data = rounded.dates_moos19_WR_Q)
# abline(moos19_V_mod)

#extract slope of model and develop rating curve
moos.2019.q.dt$RatingCurveV <- moos19_V_mod$coefficients[1]+(moos19_V_mod$coefficients[2])*moos.2019.q.dt$Q




moos.2019.q.dt$date <- as.Date(moos.2019.q.dt$datetimeAK)

daily.mean.velocity.moos <- moos.2019.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveV), list(name = mean))




######### Daily Q ##########


#discharge
MOOS.2019.Q$date <- as.Date(MOOS.2019.Q$DateTime)
MOOS.2019.Q <- MOOS.2019.Q %>% rename(discharge = Q)


moos.discharge <- na.omit(MOOS.2019.Q) %>%
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

metab.moose <- read.csv(here("outputs","moose2019-Run_2023-01-09.csv"))

SM.K <- metab.moose %>% select(date, K600_daily_mean)
SM.K <- SM.K %>% rename(k600.SM = K600_daily_mean)

SM.K$date <- as.Date(SM.K$date)

data2 <- merge(data,SM.K, by = "date")


# ALL YEARS
K.plot1_moose_2019 <- data2 %>% 
  select(date, starts_with('k600')) %>%
  gather(type, k600.value, starts_with('k600')) %>%  ggplot(aes(x=date, y=k600.value, color=type)) + geom_line() + theme_bw() +
  scale_color_discrete('variable') + labs(title = "K600, Moose 2019. From Raymond Equations ", y = expression(paste("K600 (", d^-1, ")")))
K.plot1_moose_2019










############### MOOSE 2020 ############ 


#slope
moos.slope <- 3.0369/100


Q_2020 <- read.csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_Discharge/Predicted_Discharge/2020/Q_2020.csv")
Q_2020$DateTime <-  force_tz(as.POSIXct(ymd_hms(Q_2020$DateTime)), tz = "America/Anchorage")
#### Breaking up into sites ####
MOOS_Q_2020 <- Q_2020 %>% filter(Site == "MOOS")

MOOS.2020.Q <- MOOS_Q_2020 %>%
  select(DateTime, Q, Site)









############# Continuous Depth ############



# # Moos 2020:

#download flowmeter data

#download flowmeter data
WR_20.url <- "https://drive.google.com/drive/u/1/folders/1O28nv-6gsmC_xsAwUFRjjouY9hS29FtK"
WR.20.1 <- drive_get(as_id(WR_20.url))
moos.wr20_glist <- drive_ls(WR.20.1, pattern = "R_Flowmeter Q calculation_MOOS_for_R_JAA.csv")
walk(moos.wr20_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
moos_WR_20.Data <- read.csv("R_Flowmeter Q calculation_MOOS_for_R_JAA.csv",
                            header = TRUE, na.strings=c("","NA","blank"))%>% rename(Date = ï..Date)


moos_WR_20.Data <- moos_WR_20.Data %>% tidyr::fill(Date, .direction = ("down"))

moos_WR_20.Data <- moos_WR_20.Data %>% tidyr::fill(Time, .direction = ("down"))


moos_WR_20.Data$datetimeAK <- as.POSIXct(paste(as.character(moos_WR_20.Data$Date), moos_WR_20.Data$Time), format="%y%m%d %H:%M")

moos_WR_20.Data <- moos_WR_20.Data %>%
  select(Depth..cm., datetimeAK)

Moos_depth_20_WR <- ddply(na.omit(moos_WR_20.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# moos_wr.lm <- lm(meanDepth~datetimeAK, Moos_depth_20)


Moos_depth_20_WR <- setDT(Moos_depth_20_WR)

Moos_depth_20_WR <- Moos_depth_20_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

# moos.2020.pt <- moos.2020.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# moos.2020.pt <- moos.2020.pt %>%
#   mutate(across(c(AvgAbsDepth),
#                 ~ifelse(datetimeAK >= "2020-08-26 02:00:00" & datetimeAK <= "2020-09-08 12:15:00", NA, .)))



moos.2020.q.dt <- MOOS.2020.Q
moos.2020.q.dt <- moos.2020.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(Moos_depth_20_WR)
setDT(moos.2020.q.dt)

moos.2020.q.dt$datetimeAK1 <- moos.2020.q.dt$datetimeAK

setkey( moos.2020.q.dt, datetimeAK )
setkey( Moos_depth_20_WR, datetimeAK )

#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_moos20 <- moos.2020.q.dt[ Moos_depth_20_WR, roll = "nearest" ]

rounded.dates_moos20_WR_Q <- rounded.dates_moos20 %>% rename(discharge = Q) %>% 
  select(datetimeAK, discharge, meanDepth)

#convert to meters
rounded.dates_moos20_WR_Q$meanDepth <- rounded.dates_moos20_WR_Q$meanDepth /100

moos_pt_wr_graph <- ggplot(rounded.dates_moos20_WR_Q, aes(discharge, meanDepth)) +
  geom_point()
# Add regression line
moos_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("Depth (WR)")

ggsave("plots/moos_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


moos20_depth_mod <- lm(meanDepth~discharge, data = rounded.dates_moos20_WR_Q)

summary(moos20_depth_mod)

plot(meanDepth~discharge, data = rounded.dates_moos20_WR_Q)
# abline(moos20_depth_mod)

#extract slope of model and develop rating curve
moos.2020.q.dt$RatingCurveDepth <- moos20_depth_mod$coefficients[1]+(moos20_depth_mod$coefficients[2])*moos.2020.q.dt$Q




moos.2020.q.dt$date <- as.Date(moos.2020.q.dt$datetimeAK)

daily.mean.depth.moos <- moos.2020.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveDepth), list(name = mean))



#########




############# Continuous Velocity ############



# # Moos 2020:


#download flowmeter data
WR_20.url <- "https://drive.google.com/drive/u/1/folders/1O28nv-6gsmC_xsAwUFRjjouY9hS29FtK"
WR.20.1 <- drive_get(as_id(WR_20.url))
moos.wr20_glist <- drive_ls(WR.20.1, pattern = "R_Flowmeter Q calculation_MOOS_for_R_JAA.csv")
walk(moos.wr20_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
moos_WR_20.Data <- read.csv("R_Flowmeter Q calculation_MOOS_for_R_JAA.csv",
                            header = TRUE, na.strings=c("","NA","blank"))%>% rename(Date = ï..Date)


moos_WR_20.Data <- moos_WR_20.Data %>% tidyr::fill(Date, .direction = ("down"))

moos_WR_20.Data <- moos_WR_20.Data %>% tidyr::fill(Time, .direction = ("down"))



moos_WR_20.Data$datetimeAK <- as.POSIXct(paste(as.character(moos_WR_20.Data$Date), moos_WR_20.Data$Time), format="%y%m%d %H:%M")


moos_WR_20.Data <- moos_WR_20.Data %>%
  select(Mean.Vel..m.s., datetimeAK)

Moos_V_20_WR <- ddply(na.omit(moos_WR_20.Data), .(datetimeAK), summarize, meanVelocity = mean(as.numeric(Mean.Vel..m.s.)))

# moos_wr.lm <- lm(meanDepth~datetimeAK, Moos_depth_20)


Moos_V_20_WR <- setDT(Moos_V_20_WR)

Moos_V_20_WR <- Moos_V_20_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

# moos.2020.pt <- moos.2020.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# moos.2020.pt <- moos.2020.pt %>%
#   mutate(across(c(AvgAbsV),
#                 ~ifelse(datetimeAK >= "2020-08-26 02:00:00" & datetimeAK <= "2020-09-08 12:15:00", NA, .)))



moos.2020.q.dt <- MOOS.2020.Q
moos.2020.q.dt <- moos.2020.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(Moos_V_20_WR)
setDT(moos.2020.q.dt)

moos.2020.q.dt$datetimeAK1 <- moos.2020.q.dt$datetimeAK

setkey( moos.2020.q.dt, datetimeAK )
setkey( Moos_V_20_WR, datetimeAK )

#WR was taken when EXO out of water. round V point to nearest in data record
rounded.dates_moos20 <- moos.2020.q.dt[ Moos_V_20_WR, roll = "nearest" ]

rounded.dates_moos20_WR_Q <- rounded.dates_moos20  %>% rename (discharge = Q) %>% select(datetimeAK, discharge, meanVelocity)


moos_pt_wr_graph <- ggplot(rounded.dates_moos20_WR_Q, aes(discharge, meanVelocity)) +
  geom_point()
# Add regression line
moos_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("V (WR)")

ggsave("plots/moos_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


moos20_V_mod <- lm(meanVelocity~discharge, data = rounded.dates_moos20_WR_Q)

summary(moos20_V_mod)

plot(meanVelocity~discharge, data = rounded.dates_moos20_WR_Q)
# abline(moos20_V_mod)

#extract slope of model and develop rating curve
moos.2020.q.dt$RatingCurveV <- moos20_V_mod$coefficients[1]+(moos20_V_mod$coefficients[2])*moos.2020.q.dt$Q




moos.2020.q.dt$date <- as.Date(moos.2020.q.dt$datetimeAK)

daily.mean.velocity.moos <- moos.2020.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveV), list(name = mean))




######### Daily Q ##########


#discharge
MOOS.2020.Q$date <- as.Date(MOOS.2020.Q$DateTime)
MOOS.2020.Q <- MOOS.2020.Q %>% rename(discharge = Q)


moos.discharge <- na.omit(MOOS.2020.Q) %>%
  group_by(date) %>%
  summarise_at(vars(discharge), list(name = mean)) %>% rename(discharge = name)



daily.mean.velocity.moos <- daily.mean.velocity.moos %>% rename(velocity = name)
daily.mean.depth.moos <- daily.mean.depth.moos %>% rename(depth = name)



data <- merge(daily.mean.velocity.moos,daily.mean.depth.moos, by = "date")
data <- merge(data, moos.discharge, by = "date")

#slope
moos.slope <- 3.0369/100

#Equations

data$k600.1 <-  ((data$velocity * moos.slope)^(0.89)) * (data$depth^(0.54)) * 5037

data$k600.3 <-  1162 * (moos.slope^(0.77)) * (data$velocity^(0.85))

data$k600.4 <- ((data$velocity*moos.slope)^(0.76)) * 951.5

data$k600.5 <- (data$velocity*moos.slope) * 2841 + 2.02

data$k600.6 <- 929*((data$velocity*moos.slope)^(0.75)) * data$discharge^(0.011)

data$k600.7 <- 4725*((data$velocity*moos.slope)^(0.86)) * (data$discharge^(-0.14)) *(data$depth^(0.66))


metab.moose <- read.csv(here("outputs","moose2020-Run_2023-01-09.csv"))

SM.K <- metab.moose %>% select(date, K600_daily_mean)
SM.K <- SM.K %>% rename(k600.SM = K600_daily_mean)

SM.K$date <- as.Date(SM.K$date)

data2 <- merge(data,SM.K, by = "date")


# ALL YEARS
K.plot1_moose_2020 <- data2 %>% 
  select(date, starts_with('k600')) %>%
  gather(type, k600.value, starts_with('k600')) %>%  ggplot(aes(x=date, y=k600.value, color=type)) + geom_line() + theme_bw() +
  scale_color_discrete('variable') + labs(title = "K600, Moose 2020. From Raymond Equations ", y = expression(paste("K600 (", d^-1, ")")))
K.plot1_moose_2020










###### Moos 2021

############### MOOSE 2021 ############ 


#slope
moos.slope <- 3.0369/100


Q_2021 <- read.csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_Discharge/Predicted_Discharge/2021/Q_2021.csv")
Q_2021$DateTime <-  force_tz(as.POSIXct(ymd_hms(Q_2021$DateTime)), tz = "America/Anchorage")
#### Breaking up into sites ####
MOOS_Q_2021 <- Q_2021 %>% filter(Site == "MOOS")

MOOS.2021.Q <- MOOS_Q_2021 %>%
  select(DateTime, Q, Site)









############# Continuous Depth ############



# # Moos 2021:

#download flowmeter data


#download flowmeter data
WR_21.url <- "https://drive.google.com/drive/u/1/folders/1-S_ixEutlA7RKfrvhi15aIBLrjYjg5O_"
WR.21.1 <- drive_get(as_id(WR_21.url))
moos.wr21_glist <- drive_ls(WR.21.1, pattern = "R_Flowmeter Q calculation_MOOS_for_R_JAA_2021.csv")
walk(moos.wr21_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
moos_WR_21.Data <- read.csv("R_Flowmeter Q calculation_MOOS_for_R_JAA_2021.csv",
                            header = TRUE, na.strings=c("","NA","blank")) %>% rename(Date = ï..Date)

moos_WR_21.Data <- moos_WR_21.Data %>% tidyr::fill(Date, .direction = ("down"))

moos_WR_21.Data <- moos_WR_21.Data %>% tidyr::fill(Time, .direction = ("down"))



moos_WR_21.Data$datetimeAK <- as.POSIXct(paste(as.character(moos_WR_21.Data$Date), moos_WR_21.Data$Time), format="%y%m%d %H:%M")


moos_WR_21.Data <- moos_WR_21.Data %>%
  select(Depth..cm., datetimeAK)

Moos_depth_21_WR <- ddply(na.omit(moos_WR_21.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# moos_wr.lm <- lm(meanDepth~datetimeAK, Moos_depth_21)


Moos_depth_21_WR <- setDT(Moos_depth_21_WR)

Moos_depth_21_WR <- Moos_depth_21_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

# moos.2021.pt <- moos.2021.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# moos.2021.pt <- moos.2021.pt %>%
#   mutate(across(c(AvgAbsDepth),
#                 ~ifelse(datetimeAK >= "2021-08-26 02:00:00" & datetimeAK <= "2021-09-08 12:15:00", NA, .)))



moos.2021.q.dt <- MOOS.2021.Q
moos.2021.q.dt <- moos.2021.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(Moos_depth_21_WR)
setDT(moos.2021.q.dt)

moos.2021.q.dt$datetimeAK1 <- moos.2021.q.dt$datetimeAK

setkey( moos.2021.q.dt, datetimeAK )
setkey( Moos_depth_21_WR, datetimeAK )

#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_moos21 <- moos.2021.q.dt[ Moos_depth_21_WR, roll = "nearest" ]

rounded.dates_moos21_WR_Q <- rounded.dates_moos21 %>% rename(discharge = Q) %>% 
  select(datetimeAK, discharge, meanDepth)

#convert to meters
rounded.dates_moos21_WR_Q$meanDepth <- rounded.dates_moos21_WR_Q$meanDepth /100

moos_pt_wr_graph <- ggplot(rounded.dates_moos21_WR_Q, aes(discharge, meanDepth)) +
  geom_point()
# Add regression line
moos_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("Depth (WR)")

ggsave("plots/moos_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


moos21_depth_mod <- lm(meanDepth~discharge, data = rounded.dates_moos21_WR_Q)

summary(moos21_depth_mod)

plot(meanDepth~discharge, data = rounded.dates_moos21_WR_Q)
# abline(moos21_depth_mod)

#extract slope of model and develop rating curve
moos.2021.q.dt$RatingCurveDepth <- moos21_depth_mod$coefficients[1]+(moos21_depth_mod$coefficients[2])*moos.2021.q.dt$Q




moos.2021.q.dt$date <- as.Date(moos.2021.q.dt$datetimeAK)

daily.mean.depth.moos <- moos.2021.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveDepth), list(name = mean))



#########



############# Continuous Velocity ############



# # Moos 2021:


#download flowmeter data
WR_21.url <- "https://drive.google.com/drive/u/1/folders/1-S_ixEutlA7RKfrvhi15aIBLrjYjg5O_"
WR.21.1 <- drive_get(as_id(WR_21.url))
moos.wr21_glist <- drive_ls(WR.21.1, pattern = "R_Flowmeter Q calculation_MOOS_for_R_JAA_2021.csv")
walk(moos.wr21_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
moos_WR_21.Data <- read.csv("R_Flowmeter Q calculation_MOOS_for_R_JAA_2021.csv",
                            header = TRUE, na.strings=c("","NA","blank")) %>% rename(Date = ï..Date)

moos_WR_21.Data <- moos_WR_21.Data %>% tidyr::fill(Date, .direction = ("down"))

moos_WR_21.Data <- moos_WR_21.Data %>% tidyr::fill(Time, .direction = ("down"))



moos_WR_21.Data$datetimeAK <- as.POSIXct(paste(as.character(moos_WR_21.Data$Date), moos_WR_21.Data$Time), format="%y%m%d %H:%M")



moos_WR_21.Data <- moos_WR_21.Data %>%
  select(Mean.Vel..m.s., datetimeAK)

Moos_V_21_WR <- ddply(na.omit(moos_WR_21.Data), .(datetimeAK), summarize, meanVelocity = mean(as.numeric(Mean.Vel..m.s.)))

# moos_wr.lm <- lm(meanDepth~datetimeAK, Moos_depth_21)


Moos_V_21_WR <- setDT(Moos_V_21_WR)

Moos_V_21_WR <- Moos_V_21_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

# moos.2021.pt <- moos.2021.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# moos.2021.pt <- moos.2021.pt %>%
#   mutate(across(c(AvgAbsV),
#                 ~ifelse(datetimeAK >= "2021-08-26 02:00:00" & datetimeAK <= "2021-09-08 12:15:00", NA, .)))



moos.2021.q.dt <- MOOS.2021.Q
moos.2021.q.dt <- moos.2021.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(Moos_V_21_WR)
setDT(moos.2021.q.dt)

moos.2021.q.dt$datetimeAK1 <- moos.2021.q.dt$datetimeAK

setkey( moos.2021.q.dt, datetimeAK )
setkey( Moos_V_21_WR, datetimeAK )

#WR was taken when EXO out of water. round V point to nearest in data record
rounded.dates_moos21 <- moos.2021.q.dt[ Moos_V_21_WR, roll = "nearest" ]

rounded.dates_moos21_WR_Q <- rounded.dates_moos21  %>% rename (discharge = Q) %>% select(datetimeAK, discharge, meanVelocity)


moos_pt_wr_graph <- ggplot(rounded.dates_moos21_WR_Q, aes(discharge, meanVelocity)) +
  geom_point()
# Add regression line
moos_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("V (WR)")

ggsave("plots/moos_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


moos21_V_mod <- lm(meanVelocity~discharge, data = rounded.dates_moos21_WR_Q)

summary(moos21_V_mod)

plot(meanVelocity~discharge, data = rounded.dates_moos21_WR_Q)
# abline(moos21_V_mod)

#extract slope of model and develop rating curve
moos.2021.q.dt$RatingCurveV <- moos21_V_mod$coefficients[1]+(moos21_V_mod$coefficients[2])*moos.2021.q.dt$Q




moos.2021.q.dt$date <- as.Date(moos.2021.q.dt$datetimeAK)

daily.mean.velocity.moos <- moos.2021.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveV), list(name = mean))




######### Daily Q ##########


#discharge
MOOS.2021.Q$date <- as.Date(MOOS.2021.Q$DateTime)
MOOS.2021.Q <- MOOS.2021.Q %>% rename(discharge = Q)


moos.discharge <- na.omit(MOOS.2021.Q) %>%
  group_by(date) %>%
  summarise_at(vars(discharge), list(name = mean)) %>% rename(discharge = name)



daily.mean.velocity.moos <- daily.mean.velocity.moos %>% rename(velocity = name)
daily.mean.depth.moos <- daily.mean.depth.moos %>% rename(depth = name)



data <- merge(daily.mean.velocity.moos,daily.mean.depth.moos, by = "date")
data <- merge(data, moos.discharge, by = "date")

#slope
moos.slope <- 3.0369/100

#Equations

data$k600.1 <-  ((data$velocity * moos.slope)^(0.89)) * (data$depth^(0.54)) * 5037

data$k600.3 <-  1162 * (moos.slope^(0.77)) * (data$velocity^(0.85))

data$k600.4 <- ((data$velocity*moos.slope)^(0.76)) * 951.5

data$k600.5 <- (data$velocity*moos.slope) * 2841 + 2.02

data$k600.6 <- 929*((data$velocity*moos.slope)^(0.75)) * data$discharge^(0.011)

data$k600.7 <- 4725*((data$velocity*moos.slope)^(0.86)) * (data$discharge^(-0.14)) *(data$depth^(0.66))


metab.moose <- read.csv(here("outputs","moose2021-Run_2023-01-09.csv"))

SM.K <- metab.moose %>% select(date, K600_daily_mean)
SM.K <- SM.K %>% rename(k600.SM = K600_daily_mean)

SM.K$date <- as.Date(SM.K$date)

data2 <- merge(data,SM.K, by = "date")


# ALL YEARS
K.plot1_moose_2021 <- data2 %>% 
  select(date, starts_with('k600')) %>%
  gather(type, k600.value, starts_with('k600')) %>%  ggplot(aes(x=date, y=k600.value, color=type)) + geom_line() + theme_bw() +
  scale_color_discrete('variable') + labs(title = "K600, Moose 2021. From Raymond Equations ", y = expression(paste("K600 (", d^-1, ")")))
K.plot1_moose_2021








#################### French ################################


############### french 2019 ############ 


#slope
frch.slope <- 2.8244/100


Q_2019 <- read.csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_Discharge/Predicted_Discharge/2019/Q_2019.csv")
Q_2019$DateTime <-  force_tz(as.POSIXct(ymd_hms(Q_2019$DateTime)), tz = "America/Anchorage")
#### Breaking up into sites ####
frch_Q_2019 <- Q_2019 %>% filter(Site == "FRCH")

frch.2019.Q <- frch_Q_2019 %>%
  select(DateTime, Q, Site)









############# Continuous Depth ############



#download flowmeter data
WR_19.url <- "https://drive.google.com/drive/u/1/folders/1bm62_JO1dKrFPyUCz8E88K5q7IHElXPP"
WR.19.1 <- drive_get(as_id(WR_19.url))
frch.wr19_glist <- drive_ls(WR.19.1, pattern = "Frch_2019_flowmeter_Q_for_R_JAA.csv")
walk(frch.wr19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
frch_WR_19.Data <- read.csv("Frch_2019_flowmeter_Q_for_R_JAA.csv",
                            skip = 1, header = TRUE, na.strings=c("","NA","blank"))

frch_WR_19.Data <- frch_WR_19.Data %>% tidyr::fill(Date, .direction = ("down"))

frch_WR_19.Data <- frch_WR_19.Data %>% tidyr::fill(Time, .direction = ("down"))


frch_WR_19.Data$datetimeAK <- as.POSIXct(paste(frch_WR_19.Data$Date, frch_WR_19.Data$Time), format="%m/%d/%Y %H:%M")




frch_WR_19.Data <- frch_WR_19.Data %>%
  select(Depth..cm., datetimeAK)

frch_depth_19_WR <- ddply(na.omit(frch_WR_19.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# frch_wr.lm <- lm(meanDepth~datetimeAK, frch_depth_19)


frch_depth_19_WR <- setDT(frch_depth_19_WR)

frch_depth_19_WR <- frch_depth_19_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

# frch.2019.pt <- frch.2019.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# frch.2019.pt <- frch.2019.pt %>%
#   mutate(across(c(AvgAbsDepth),
#                 ~ifelse(datetimeAK >= "2019-08-26 02:00:00" & datetimeAK <= "2019-09-08 12:15:00", NA, .)))



frch.2019.q.dt <- frch.2019.Q
frch.2019.q.dt <- frch.2019.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(frch_depth_19_WR)
setDT(frch.2019.q.dt)

frch.2019.q.dt$datetimeAK1 <- frch.2019.q.dt$datetimeAK

setkey( frch.2019.q.dt, datetimeAK )
setkey( frch_depth_19_WR, datetimeAK )

#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_frch19 <- frch.2019.q.dt[ frch_depth_19_WR, roll = "nearest" ]

rounded.dates_frch19_WR_Q <- rounded.dates_frch19 %>% rename(discharge = Q) %>% 
  select(datetimeAK, discharge, meanDepth)

#convert to meters
rounded.dates_frch19_WR_Q$meanDepth <- rounded.dates_frch19_WR_Q$meanDepth /100

frch_pt_wr_graph <- ggplot(rounded.dates_frch19_WR_Q, aes(discharge, meanDepth)) +
  geom_point()
# Add regression line
frch_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("Depth (WR)")

ggsave("plots/frch_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


frch19_depth_mod <- lm(meanDepth~discharge, data = rounded.dates_frch19_WR_Q)

summary(frch19_depth_mod)

plot(meanDepth~discharge, data = rounded.dates_frch19_WR_Q)
# abline(frch19_depth_mod)

#extract slope of model and develop rating curve
frch.2019.q.dt$RatingCurveDepth <- frch19_depth_mod$coefficients[1]+(frch19_depth_mod$coefficients[2])*frch.2019.q.dt$Q




frch.2019.q.dt$date <- as.Date(frch.2019.q.dt$datetimeAK)

daily.mean.depth.frch <- frch.2019.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveDepth), list(name = mean))



#########




############# Continuous Velocity ############



# # frch 2019:

#download flowmeter data
WR_19.url <- "https://drive.google.com/drive/u/1/folders/1bm62_JO1dKrFPyUCz8E88K5q7IHElXPP"
WR.19.1 <- drive_get(as_id(WR_19.url))
frch.wr19_glist <- drive_ls(WR.19.1, pattern = "Frch_2019_flowmeter_Q_for_R_JAA.csv")
walk(frch.wr19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
frch_WR_19.Data <- read.csv("Frch_2019_flowmeter_Q_for_R_JAA.csv",
                            skip = 1, header = TRUE, na.strings=c("","NA","blank"))

frch_WR_19.Data <- frch_WR_19.Data %>% tidyr::fill(Date, .direction = ("down"))

frch_WR_19.Data <- frch_WR_19.Data %>% tidyr::fill(Time, .direction = ("down"))


frch_WR_19.Data$datetimeAK <- as.POSIXct(paste(frch_WR_19.Data$Date, frch_WR_19.Data$Time), format="%m/%d/%Y %H:%M")




frch_WR_19.Data <- frch_WR_19.Data %>%
  select(Mean.Vel..m.s., datetimeAK)

frch_V_19_WR <- ddply(na.omit(frch_WR_19.Data), .(datetimeAK), summarize, meanVelocity = mean(as.numeric(Mean.Vel..m.s.)))

# frch_wr.lm <- lm(meanDepth~datetimeAK, frch_depth_19)


frch_V_19_WR <- setDT(frch_V_19_WR)

frch_V_19_WR <- frch_V_19_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

# frch.2019.pt <- frch.2019.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# frch.2019.pt <- frch.2019.pt %>%
#   mutate(across(c(AvgAbsV),
#                 ~ifelse(datetimeAK >= "2019-08-26 02:00:00" & datetimeAK <= "2019-09-08 12:15:00", NA, .)))



frch.2019.q.dt <- frch.2019.Q
frch.2019.q.dt <- frch.2019.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(frch_V_19_WR)
setDT(frch.2019.q.dt)

frch.2019.q.dt$datetimeAK1 <- frch.2019.q.dt$datetimeAK

setkey( frch.2019.q.dt, datetimeAK )
setkey( frch_V_19_WR, datetimeAK )

#WR was taken when EXO out of water. round V point to nearest in data record
rounded.dates_frch19 <- frch.2019.q.dt[ frch_V_19_WR, roll = "nearest" ]

rounded.dates_frch19_WR_Q <- rounded.dates_frch19  %>% rename (discharge = Q) %>% select(datetimeAK, discharge, meanVelocity)


frch_pt_wr_graph <- ggplot(rounded.dates_frch19_WR_Q, aes(discharge, meanVelocity)) +
  geom_point()
# Add regression line
frch_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("V (WR)")

ggsave("plots/frch_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


frch19_V_mod <- lm(meanVelocity~discharge, data = rounded.dates_frch19_WR_Q)

summary(frch19_V_mod)

plot(meanVelocity~discharge, data = rounded.dates_frch19_WR_Q)
# abline(frch19_V_mod)

#extract slope of model and develop rating curve
frch.2019.q.dt$RatingCurveV <- frch19_V_mod$coefficients[1]+(frch19_V_mod$coefficients[2])*frch.2019.q.dt$Q




frch.2019.q.dt$date <- as.Date(frch.2019.q.dt$datetimeAK)

daily.mean.velocity.frch <- frch.2019.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveV), list(name = mean))




######### Daily Q ##########


#discharge
frch.2019.Q$date <- as.Date(frch.2019.Q$DateTime)
frch.2019.Q <- frch.2019.Q %>% rename(discharge = Q)


frch.discharge <- na.omit(frch.2019.Q) %>%
  group_by(date) %>%
  summarise_at(vars(discharge), list(name = mean)) %>% rename(discharge = name)



daily.mean.velocity.frch <- daily.mean.velocity.frch %>% rename(velocity = name)
daily.mean.depth.frch <- daily.mean.depth.frch %>% rename(depth = name)



data <- merge(daily.mean.velocity.frch,daily.mean.depth.frch, by = "date")
data <- merge(data, frch.discharge, by = "date")
view(data)



#Equations

data$k600.1 <-  ((data$velocity * frch.slope)^(0.89)) * (data$depth^(0.54)) * 5037

data$k600.3 <-  1162 * (frch.slope^(0.77)) * (data$velocity^(0.85))

data$k600.4 <- ((data$velocity*frch.slope)^(0.76)) * 951.5

data$k600.5 <- (data$velocity*frch.slope) * 2841 + 2.02

data$k600.6 <- 929*((data$velocity*frch.slope)^(0.75)) * data$discharge^(0.011)

data$k600.7 <- 4725*((data$velocity*frch.slope)^(0.86)) * (data$discharge^(-0.14)) *(data$depth^(0.66))

view(data)

metab.french <- read.csv(here("outputs","french2019-Run_2023-01-09.csv"))

SM.K <- metab.french %>% select(date, K600_daily_mean)
SM.K <- SM.K %>% rename(k600.SM = K600_daily_mean)

SM.K$date <- as.Date(SM.K$date)

data2 <- merge(data,SM.K, by = "date")


# ALL YEARS
K.plot1_french_2019 <- data2 %>% 
  select(date, starts_with('k600')) %>%
  gather(type, k600.value, starts_with('k600')) %>%  ggplot(aes(x=date, y=k600.value, color=type)) + geom_line() + theme_bw() +
  scale_color_discrete('variable') + labs(title = "K600, french 2019. From Raymond Equations ", y = expression(paste("K600 (", d^-1, ")")))
K.plot1_french_2019










############### french 2020 ############ 


#slope
frch.slope <- 2.8244/100


Q_2020 <- read.csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_Discharge/Predicted_Discharge/2020/Q_2020.csv")
Q_2020$DateTime <-  force_tz(as.POSIXct(ymd_hms(Q_2020$DateTime)), tz = "America/Anchorage")
#### Breaking up into sites ####
frch_Q_2020 <- Q_2020 %>% filter(Site == "FRCH")

frch.2020.Q <- frch_Q_2020 %>%
  select(DateTime, Q, Site)









############# Continuous Depth ############



# # frch 2020:

#download flowmeter data
#download flowmeter data
WR_20.url <- "https://drive.google.com/drive/u/1/folders/1tlcGKOm11j4nPqgBeTa1Hj6DFTrbZTvy"
WR.20.1 <- drive_get(as_id(WR_20.url))
frch.wr20_glist <- drive_ls(WR.20.1, pattern = "R_Flowmeter Q calculation_FRCH_for_R_JAA.csv")
walk(frch.wr20_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
frch_WR_20.Data <- read.csv("R_Flowmeter Q calculation_FRCH_for_R_JAA.csv",
                            header = TRUE, na.strings=c("","NA","blank"))

frch_WR_20.Data <- frch_WR_20.Data %>% tidyr::fill(Date, .direction = ("down"))

frch_WR_20.Data <- frch_WR_20.Data %>% tidyr::fill(Time, .direction = ("down"))


frch_WR_20.Data$datetimeAK <- as.POSIXct(paste(as.character(frch_WR_20.Data$Date), frch_WR_20.Data$Time), format="%y%m%d %H:%M")


frch_WR_20.Data <- frch_WR_20.Data %>%
  select(Depth..cm., datetimeAK)

frch_depth_20_WR <- ddply(na.omit(frch_WR_20.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# frch_wr.lm <- lm(meanDepth~datetimeAK, frch_depth_20)


frch_depth_20_WR <- setDT(frch_depth_20_WR)

frch_depth_20_WR <- frch_depth_20_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

# frch.2020.pt <- frch.2020.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# frch.2020.pt <- frch.2020.pt %>%
#   mutate(across(c(AvgAbsDepth),
#                 ~ifelse(datetimeAK >= "2020-08-26 02:00:00" & datetimeAK <= "2020-09-08 12:15:00", NA, .)))



frch.2020.q.dt <- frch.2020.Q
frch.2020.q.dt <- frch.2020.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(frch_depth_20_WR)
setDT(frch.2020.q.dt)

frch.2020.q.dt$datetimeAK1 <- frch.2020.q.dt$datetimeAK

setkey( frch.2020.q.dt, datetimeAK )
setkey( frch_depth_20_WR, datetimeAK )

#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_frch20 <- frch.2020.q.dt[ frch_depth_20_WR, roll = "nearest" ]

rounded.dates_frch20_WR_Q <- rounded.dates_frch20 %>% rename(discharge = Q) %>% 
  select(datetimeAK, discharge, meanDepth)

#convert to meters
rounded.dates_frch20_WR_Q$meanDepth <- rounded.dates_frch20_WR_Q$meanDepth /100

frch_pt_wr_graph <- ggplot(rounded.dates_frch20_WR_Q, aes(discharge, meanDepth)) +
  geom_point()
# Add regression line
frch_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("Depth (WR)")

ggsave("plots/frch_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


frch20_depth_mod <- lm(meanDepth~discharge, data = rounded.dates_frch20_WR_Q)

summary(frch20_depth_mod)

plot(meanDepth~discharge, data = rounded.dates_frch20_WR_Q)
# abline(frch20_depth_mod)

#extract slope of model and develop rating curve
frch.2020.q.dt$RatingCurveDepth <- frch20_depth_mod$coefficients[1]+(frch20_depth_mod$coefficients[2])*frch.2020.q.dt$Q




frch.2020.q.dt$date <- as.Date(frch.2020.q.dt$datetimeAK)

daily.mean.depth.frch <- frch.2020.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveDepth), list(name = mean))



#########




############# Continuous Velocity ############



# # frch 2020:

#download flowmeter data
WR_20.url <- "https://drive.google.com/drive/u/1/folders/1tlcGKOm11j4nPqgBeTa1Hj6DFTrbZTvy"
WR.20.1 <- drive_get(as_id(WR_20.url))
frch.wr20_glist <- drive_ls(WR.20.1, pattern = "R_Flowmeter Q calculation_FRCH_for_R_JAA.csv")
walk(frch.wr20_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
frch_WR_20.Data <- read.csv("R_Flowmeter Q calculation_FRCH_for_R_JAA.csv",
                            header = TRUE, na.strings=c("","NA","blank"))

frch_WR_20.Data <- frch_WR_20.Data %>% tidyr::fill(Date, .direction = ("down"))

frch_WR_20.Data <- frch_WR_20.Data %>% tidyr::fill(Time, .direction = ("down"))


frch_WR_20.Data$datetimeAK <- as.POSIXct(paste(as.character(frch_WR_20.Data$Date), frch_WR_20.Data$Time), format="%y%m%d %H:%M")

frch_WR_20.Data <- frch_WR_20.Data %>%
  select(Mean.Vel..m.s., datetimeAK)

frch_V_20_WR <- ddply(na.omit(frch_WR_20.Data), .(datetimeAK), summarize, meanVelocity = mean(as.numeric(Mean.Vel..m.s.)))

# frch_wr.lm <- lm(meanDepth~datetimeAK, frch_depth_20)


frch_V_20_WR <- setDT(frch_V_20_WR)

frch_V_20_WR <- frch_V_20_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

# frch.2020.pt <- frch.2020.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# frch.2020.pt <- frch.2020.pt %>%
#   mutate(across(c(AvgAbsV),
#                 ~ifelse(datetimeAK >= "2020-08-26 02:00:00" & datetimeAK <= "2020-09-08 12:15:00", NA, .)))



frch.2020.q.dt <- frch.2020.Q
frch.2020.q.dt <- frch.2020.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(frch_V_20_WR)
setDT(frch.2020.q.dt)

frch.2020.q.dt$datetimeAK1 <- frch.2020.q.dt$datetimeAK

setkey( frch.2020.q.dt, datetimeAK )
setkey( frch_V_20_WR, datetimeAK )

#WR was taken when EXO out of water. round V point to nearest in data record
rounded.dates_frch20 <- frch.2020.q.dt[ frch_V_20_WR, roll = "nearest" ]

rounded.dates_frch20_WR_Q <- rounded.dates_frch20  %>% rename (discharge = Q) %>% select(datetimeAK, discharge, meanVelocity)


frch_pt_wr_graph <- ggplot(rounded.dates_frch20_WR_Q, aes(discharge, meanVelocity)) +
  geom_point()
# Add regression line
frch_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("V (WR)")

ggsave("plots/frch_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


frch20_V_mod <- lm(meanVelocity~discharge, data = rounded.dates_frch20_WR_Q)

summary(frch20_V_mod)

plot(meanVelocity~discharge, data = rounded.dates_frch20_WR_Q)
# abline(frch20_V_mod)

#extract slope of model and develop rating curve
frch.2020.q.dt$RatingCurveV <- frch20_V_mod$coefficients[1]+(frch20_V_mod$coefficients[2])*frch.2020.q.dt$Q




frch.2020.q.dt$date <- as.Date(frch.2020.q.dt$datetimeAK)

daily.mean.velocity.frch <- frch.2020.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveV), list(name = mean))




######### Daily Q ##########


#discharge
frch.2020.Q$date <- as.Date(frch.2020.Q$DateTime)
frch.2020.Q <- frch.2020.Q %>% rename(discharge = Q)


frch.discharge <- na.omit(frch.2020.Q) %>%
  group_by(date) %>%
  summarise_at(vars(discharge), list(name = mean)) %>% rename(discharge = name)



daily.mean.velocity.frch <- daily.mean.velocity.frch %>% rename(velocity = name)
daily.mean.depth.frch <- daily.mean.depth.frch %>% rename(depth = name)



data <- merge(daily.mean.velocity.frch,daily.mean.depth.frch, by = "date")
data <- merge(data, frch.discharge, by = "date")

#slope
frch.slope <- 2.8244/100

#Equations

data$k600.1 <-  ((data$velocity * frch.slope)^(0.89)) * (data$depth^(0.54)) * 5037

data$k600.3 <-  1162 * (frch.slope^(0.77)) * (data$velocity^(0.85))

data$k600.4 <- ((data$velocity*frch.slope)^(0.76)) * 951.5

data$k600.5 <- (data$velocity*frch.slope) * 2841 + 2.02

data$k600.6 <- 929*((data$velocity*frch.slope)^(0.75)) * data$discharge^(0.011)

data$k600.7 <- 4725*((data$velocity*frch.slope)^(0.86)) * (data$discharge^(-0.14)) *(data$depth^(0.66))


metab.french <- read.csv(here("outputs","french2020-Run_2023-01-09.csv"))

SM.K <- metab.french %>% select(date, K600_daily_mean)
SM.K <- SM.K %>% rename(k600.SM = K600_daily_mean)

SM.K$date <- as.Date(SM.K$date)

data2 <- merge(data,SM.K, by = "date")


# ALL YEARS
K.plot1_french_2020 <- data2 %>% 
  select(date, starts_with('k600')) %>%
  gather(type, k600.value, starts_with('k600')) %>%  ggplot(aes(x=date, y=k600.value, color=type)) + geom_line() + theme_bw() +
  scale_color_discrete('variable') + labs(title = "K600, french 2020. From Raymond Equations ", y = expression(paste("K600 (", d^-1, ")")))
K.plot1_french_2020










###### frch 2021

############### french 2021 ############ 


#slope
frch.slope <- 2.8244/100


Q_2021 <- read.csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_Discharge/Predicted_Discharge/2021/Q_2021.csv")
Q_2021$DateTime <-  force_tz(as.POSIXct(ymd_hms(Q_2021$DateTime)), tz = "America/Anchorage")
#### Breaking up into sites ####
frch_Q_2021 <- Q_2021 %>% filter(Site == "FRCH")

frch.2021.Q <- frch_Q_2021 %>%
  select(DateTime, Q, Site)









############# Continuous Depth ############



# # frch 2021:

#download flowmeter data


#download flowmeter data
WR_21.url <- "https://drive.google.com/drive/u/1/folders/1MrFabu9Mzuv3v4naPl2-iCFaMj_DjkZG"
WR.21.1 <- drive_get(as_id(WR_21.url))
frch.wr21_glist <- drive_ls(WR.21.1, pattern = "R_Flowmeter_Q_calulation_FRCH_for_R_JAA_2021.csv")
walk(frch.wr21_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
frch_WR_21.Data <- read.csv("R_Flowmeter_Q_calulation_FRCH_for_R_JAA_2021.csv",
                            header = TRUE, na.strings=c("","NA","blank"))%>% rename(Date = ï..Date)

frch_WR_21.Data <- frch_WR_21.Data %>% tidyr::fill(Date, .direction = ("down"))

frch_WR_21.Data <- frch_WR_21.Data %>% tidyr::fill(Time, .direction = ("down"))


frch_WR_21.Data$datetimeAK <- as.POSIXct(paste(as.character(frch_WR_21.Data$Date), frch_WR_21.Data$Time), format="%y%m%d %H:%M")


frch_WR_21.Data <- frch_WR_21.Data %>%
  select(Depth..cm., datetimeAK)

frch_depth_21_WR <- ddply(na.omit(frch_WR_21.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# frch_wr.lm <- lm(meanDepth~datetimeAK, frch_depth_21)


frch_depth_21_WR <- setDT(frch_depth_21_WR)

frch_depth_21_WR <- frch_depth_21_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

# frch.2021.pt <- frch.2021.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# frch.2021.pt <- frch.2021.pt %>%
#   mutate(across(c(AvgAbsDepth),
#                 ~ifelse(datetimeAK >= "2021-08-26 02:00:00" & datetimeAK <= "2021-09-08 12:15:00", NA, .)))



frch.2021.q.dt <- frch.2021.Q
frch.2021.q.dt <- frch.2021.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(frch_depth_21_WR)
setDT(frch.2021.q.dt)

frch.2021.q.dt$datetimeAK1 <- frch.2021.q.dt$datetimeAK

setkey( frch.2021.q.dt, datetimeAK )
setkey( frch_depth_21_WR, datetimeAK )

#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_frch21 <- frch.2021.q.dt[ frch_depth_21_WR, roll = "nearest" ]

rounded.dates_frch21_WR_Q <- rounded.dates_frch21 %>% rename(discharge = Q) %>% 
  select(datetimeAK, discharge, meanDepth)

#convert to meters
rounded.dates_frch21_WR_Q$meanDepth <- rounded.dates_frch21_WR_Q$meanDepth /100

frch_pt_wr_graph <- ggplot(rounded.dates_frch21_WR_Q, aes(discharge, meanDepth)) +
  geom_point()
# Add regression line
frch_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("Depth (WR)")

ggsave("plots/frch_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


frch21_depth_mod <- lm(meanDepth~discharge, data = rounded.dates_frch21_WR_Q)

summary(frch21_depth_mod)

plot(meanDepth~discharge, data = rounded.dates_frch21_WR_Q)
# abline(frch21_depth_mod)

#extract slope of model and develop rating curve
frch.2021.q.dt$RatingCurveDepth <- frch21_depth_mod$coefficients[1]+(frch21_depth_mod$coefficients[2])*frch.2021.q.dt$Q




frch.2021.q.dt$date <- as.Date(frch.2021.q.dt$datetimeAK)

daily.mean.depth.frch <- frch.2021.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveDepth), list(name = mean))



#########



############# Continuous Velocity ############



# # frch 2021:


#download flowmeter data
WR_21.url <- "https://drive.google.com/drive/u/1/folders/1MrFabu9Mzuv3v4naPl2-iCFaMj_DjkZG"
WR.21.1 <- drive_get(as_id(WR_21.url))
frch.wr21_glist <- drive_ls(WR.21.1, pattern = "R_Flowmeter_Q_calulation_FRCH_for_R_JAA_2021.csv")
walk(frch.wr21_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
frch_WR_21.Data <- read.csv("R_Flowmeter_Q_calulation_FRCH_for_R_JAA_2021.csv",
                            header = TRUE, na.strings=c("","NA","blank"))%>% rename(Date = ï..Date)

frch_WR_21.Data <- frch_WR_21.Data %>% tidyr::fill(Date, .direction = ("down"))

frch_WR_21.Data <- frch_WR_21.Data %>% tidyr::fill(Time, .direction = ("down"))


frch_WR_21.Data$datetimeAK <- as.POSIXct(paste(as.character(frch_WR_21.Data$Date), frch_WR_21.Data$Time), format="%y%m%d %H:%M")

frch_WR_21.Data <- frch_WR_21.Data %>%
  select(Mean.Vel..m.s., datetimeAK)

frch_V_21_WR <- ddply(na.omit(frch_WR_21.Data), .(datetimeAK), summarize, meanVelocity = mean(as.numeric(Mean.Vel..m.s.)))

# frch_wr.lm <- lm(meanDepth~datetimeAK, frch_depth_21)


frch_V_21_WR <- setDT(frch_V_21_WR)

frch_V_21_WR <- frch_V_21_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

# frch.2021.pt <- frch.2021.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# frch.2021.pt <- frch.2021.pt %>%
#   mutate(across(c(AvgAbsV),
#                 ~ifelse(datetimeAK >= "2021-08-26 02:00:00" & datetimeAK <= "2021-09-08 12:15:00", NA, .)))



frch.2021.q.dt <- frch.2021.Q
frch.2021.q.dt <- frch.2021.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(frch_V_21_WR)
setDT(frch.2021.q.dt)

frch.2021.q.dt$datetimeAK1 <- frch.2021.q.dt$datetimeAK

setkey( frch.2021.q.dt, datetimeAK )
setkey( frch_V_21_WR, datetimeAK )

#WR was taken when EXO out of water. round V point to nearest in data record
rounded.dates_frch21 <- frch.2021.q.dt[ frch_V_21_WR, roll = "nearest" ]

rounded.dates_frch21_WR_Q <- rounded.dates_frch21  %>% rename (discharge = Q) %>% select(datetimeAK, discharge, meanVelocity)


frch_pt_wr_graph <- ggplot(rounded.dates_frch21_WR_Q, aes(discharge, meanVelocity)) +
  geom_point()
# Add regression line
frch_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("V (WR)")

ggsave("plots/frch_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


frch21_V_mod <- lm(meanVelocity~discharge, data = rounded.dates_frch21_WR_Q)

summary(frch21_V_mod)

plot(meanVelocity~discharge, data = rounded.dates_frch21_WR_Q)
# abline(frch21_V_mod)

#extract slope of model and develop rating curve
frch.2021.q.dt$RatingCurveV <- frch21_V_mod$coefficients[1]+(frch21_V_mod$coefficients[2])*frch.2021.q.dt$Q




frch.2021.q.dt$date <- as.Date(frch.2021.q.dt$datetimeAK)

daily.mean.velocity.frch <- frch.2021.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveV), list(name = mean))




######### Daily Q ##########


#discharge
frch.2021.Q$date <- as.Date(frch.2021.Q$DateTime)
frch.2021.Q <- frch.2021.Q %>% rename(discharge = Q)


frch.discharge <- na.omit(frch.2021.Q) %>%
  group_by(date) %>%
  summarise_at(vars(discharge), list(name = mean)) %>% rename(discharge = name)



daily.mean.velocity.frch <- daily.mean.velocity.frch %>% rename(velocity = name)
daily.mean.depth.frch <- daily.mean.depth.frch %>% rename(depth = name)



data <- merge(daily.mean.velocity.frch,daily.mean.depth.frch, by = "date")
data <- merge(data, frch.discharge, by = "date")

#slope
frch.slope <- 2.8244/100

#Equations

data$k600.1 <-  ((data$velocity * frch.slope)^(0.89)) * (data$depth^(0.54)) * 5037

data$k600.3 <-  1162 * (frch.slope^(0.77)) * (data$velocity^(0.85))

data$k600.4 <- ((data$velocity*frch.slope)^(0.76)) * 951.5

data$k600.5 <- (data$velocity*frch.slope) * 2841 + 2.02

data$k600.6 <- 929*((data$velocity*frch.slope)^(0.75)) * data$discharge^(0.011)

data$k600.7 <- 4725*((data$velocity*frch.slope)^(0.86)) * (data$discharge^(-0.14)) *(data$depth^(0.66))


metab.french <- read.csv(here("outputs","french2021-Run_2023-01-09.csv"))

SM.K <- metab.french %>% select(date, K600_daily_mean)
SM.K <- SM.K %>% rename(k600.SM = K600_daily_mean)

SM.K$date <- as.Date(SM.K$date)

data2 <- merge(data,SM.K, by = "date")


# ALL YEARS
K.plot1_french_2021 <- data2 %>% 
  select(date, starts_with('k600')) %>%
  gather(type, k600.value, starts_with('k600')) %>%  ggplot(aes(x=date, y=k600.value, color=type)) + geom_line() + theme_bw() +
  scale_color_discrete('variable') + labs(title = "K600, french 2021. From Raymond Equations ", y = expression(paste("K600 (", d^-1, ")")))
K.plot1_french_2021
















#################### vault ################################


############### vault 2019 ############ 


#slope
vaul.slope <- 0.90629/100


Q_2019 <- read.csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_Discharge/Predicted_Discharge/2019/Q_2019.csv")
Q_2019$DateTime <-  force_tz(as.POSIXct(ymd_hms(Q_2019$DateTime)), tz = "America/Anchorage")
#### Breaking up into sites ####
vaul_Q_2019 <- Q_2019 %>% filter(Site == "VAUL")

vaul.2019.Q <- vaul_Q_2019 %>%
  select(DateTime, Q, Site)









############# Continuous Depth ############


#download flowmeter data
WR_19.url <- "https://drive.google.com/drive/u/1/folders/1bm62_JO1dKrFPyUCz8E88K5q7IHElXPP"
WR.19.1 <- drive_get(as_id(WR_19.url))
vaul.wr19_glist <- drive_ls(WR.19.1, pattern = "vaul_2019_flowmeter_Q_for_R_JAA.csv")
walk(vaul.wr19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
vaul_WR_19.Data <- read.csv("vaul_2019_flowmeter_Q_for_R_JAA.csv",
                            skip = 1, header = TRUE, na.strings=c("","NA","blank"))

vaul_WR_19.Data <- vaul_WR_19.Data %>% tidyr::fill(Date, .direction = ("down"))

vaul_WR_19.Data <- vaul_WR_19.Data %>% tidyr::fill(Time, .direction = ("down"))


vaul_WR_19.Data$datetimeAK <- as.POSIXct(paste(vaul_WR_19.Data$Date, vaul_WR_19.Data$Time), format="%m/%d/%Y %H:%M")





vaul_WR_19.Data <- vaul_WR_19.Data %>%
  select(Depth..cm., datetimeAK)

vaul_depth_19_WR <- ddply(na.omit(vaul_WR_19.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# vaul_wr.lm <- lm(meanDepth~datetimeAK, vaul_depth_19)


vaul_depth_19_WR <- setDT(vaul_depth_19_WR)

vaul_depth_19_WR <- vaul_depth_19_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

# vaul.2019.pt <- vaul.2019.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# vaul.2019.pt <- vaul.2019.pt %>%
#   mutate(across(c(AvgAbsDepth),
#                 ~ifelse(datetimeAK >= "2019-08-26 02:00:00" & datetimeAK <= "2019-09-08 12:15:00", NA, .)))



vaul.2019.q.dt <- vaul.2019.Q
vaul.2019.q.dt <- vaul.2019.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(vaul_depth_19_WR)
setDT(vaul.2019.q.dt)

vaul.2019.q.dt$datetimeAK1 <- vaul.2019.q.dt$datetimeAK

setkey( vaul.2019.q.dt, datetimeAK )
setkey( vaul_depth_19_WR, datetimeAK )

#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_vaul19 <- vaul.2019.q.dt[ vaul_depth_19_WR, roll = "nearest" ]

rounded.dates_vaul19_WR_Q <- rounded.dates_vaul19 %>% rename(discharge = Q) %>% 
  select(datetimeAK, discharge, meanDepth)

#convert to meters
rounded.dates_vaul19_WR_Q$meanDepth <- rounded.dates_vaul19_WR_Q$meanDepth /100

vaul_pt_wr_graph <- ggplot(rounded.dates_vaul19_WR_Q, aes(discharge, meanDepth)) +
  geom_point()
# Add regression line
vaul_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("Depth (WR)")

ggsave("plots/vaul_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


vaul19_depth_mod <- lm(meanDepth~discharge, data = rounded.dates_vaul19_WR_Q)

summary(vaul19_depth_mod)

plot(meanDepth~discharge, data = rounded.dates_vaul19_WR_Q)
# abline(vaul19_depth_mod)

#extract slope of model and develop rating curve
vaul.2019.q.dt$RatingCurveDepth <- vaul19_depth_mod$coefficients[1]+(vaul19_depth_mod$coefficients[2])*vaul.2019.q.dt$Q




vaul.2019.q.dt$date <- as.Date(vaul.2019.q.dt$datetimeAK)

daily.mean.depth.vaul <- vaul.2019.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveDepth), list(name = mean))



#########




############# Continuous Velocity ############



# # vaul 2019:
#download flowmeter data
WR_19.url <- "https://drive.google.com/drive/u/1/folders/1bm62_JO1dKrFPyUCz8E88K5q7IHElXPP"
WR.19.1 <- drive_get(as_id(WR_19.url))
vaul.wr19_glist <- drive_ls(WR.19.1, pattern = "vaul_2019_flowmeter_Q_for_R_JAA.csv")
walk(vaul.wr19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
vaul_WR_19.Data <- read.csv("vaul_2019_flowmeter_Q_for_R_JAA.csv",
                            skip = 1, header = TRUE, na.strings=c("","NA","blank"))

vaul_WR_19.Data <- vaul_WR_19.Data %>% tidyr::fill(Date, .direction = ("down"))

vaul_WR_19.Data <- vaul_WR_19.Data %>% tidyr::fill(Time, .direction = ("down"))


vaul_WR_19.Data$datetimeAK <- as.POSIXct(paste(vaul_WR_19.Data$Date, vaul_WR_19.Data$Time), format="%m/%d/%Y %H:%M")





vaul_WR_19.Data <- vaul_WR_19.Data %>%
  select(Mean.Vel..m.s., datetimeAK)

vaul_V_19_WR <- ddply(na.omit(vaul_WR_19.Data), .(datetimeAK), summarize, meanVelocity = mean(as.numeric(Mean.Vel..m.s.)))

# vaul_wr.lm <- lm(meanDepth~datetimeAK, vaul_depth_19)


vaul_V_19_WR <- setDT(vaul_V_19_WR)

vaul_V_19_WR <- vaul_V_19_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

# vaul.2019.pt <- vaul.2019.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# vaul.2019.pt <- vaul.2019.pt %>%
#   mutate(across(c(AvgAbsV),
#                 ~ifelse(datetimeAK >= "2019-08-26 02:00:00" & datetimeAK <= "2019-09-08 12:15:00", NA, .)))



vaul.2019.q.dt <- vaul.2019.Q
vaul.2019.q.dt <- vaul.2019.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(vaul_V_19_WR)
setDT(vaul.2019.q.dt)

vaul.2019.q.dt$datetimeAK1 <- vaul.2019.q.dt$datetimeAK

setkey( vaul.2019.q.dt, datetimeAK )
setkey( vaul_V_19_WR, datetimeAK )

#WR was taken when EXO out of water. round V point to nearest in data record
rounded.dates_vaul19 <- vaul.2019.q.dt[ vaul_V_19_WR, roll = "nearest" ]

rounded.dates_vaul19_WR_Q <- rounded.dates_vaul19  %>% rename (discharge = Q) %>% select(datetimeAK, discharge, meanVelocity)


vaul_pt_wr_graph <- ggplot(rounded.dates_vaul19_WR_Q, aes(discharge, meanVelocity)) +
  geom_point()
# Add regression line
vaul_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("V (WR)")

ggsave("plots/vaul_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


vaul19_V_mod <- lm(meanVelocity~discharge, data = rounded.dates_vaul19_WR_Q)

summary(vaul19_V_mod)

plot(meanVelocity~discharge, data = rounded.dates_vaul19_WR_Q)
# abline(vaul19_V_mod)

#extract slope of model and develop rating curve
vaul.2019.q.dt$RatingCurveV <- vaul19_V_mod$coefficients[1]+(vaul19_V_mod$coefficients[2])*vaul.2019.q.dt$Q




vaul.2019.q.dt$date <- as.Date(vaul.2019.q.dt$datetimeAK)

daily.mean.velocity.vaul <- vaul.2019.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveV), list(name = mean))




######### Daily Q ##########


#discharge
vaul.2019.Q$date <- as.Date(vaul.2019.Q$DateTime)
vaul.2019.Q <- vaul.2019.Q %>% rename(discharge = Q)


vaul.discharge <- na.omit(vaul.2019.Q) %>%
  group_by(date) %>%
  summarise_at(vars(discharge), list(name = mean)) %>% rename(discharge = name)



daily.mean.velocity.vaul <- daily.mean.velocity.vaul %>% rename(velocity = name)
daily.mean.depth.vaul <- daily.mean.depth.vaul %>% rename(depth = name)



data <- merge(daily.mean.velocity.vaul,daily.mean.depth.vaul, by = "date")
data <- merge(data, vaul.discharge, by = "date")
view(data)



#Equations

data$k600.1 <-  ((data$velocity * vaul.slope)^(0.89)) * (data$depth^(0.54)) * 5037

data$k600.3 <-  1162 * (vaul.slope^(0.77)) * (data$velocity^(0.85))

data$k600.4 <- ((data$velocity*vaul.slope)^(0.76)) * 951.5

data$k600.5 <- (data$velocity*vaul.slope) * 2841 + 2.02

data$k600.6 <- 929*((data$velocity*vaul.slope)^(0.75)) * data$discharge^(0.011)

data$k600.7 <- 4725*((data$velocity*vaul.slope)^(0.86)) * (data$discharge^(-0.14)) *(data$depth^(0.66))

view(data)

metab.vault <- read.csv(here("outputs","vault2019-Run_2023-01-09.csv"))

SM.K <- metab.vault %>% select(date, K600_daily_mean)
SM.K <- SM.K %>% rename(k600.SM = K600_daily_mean)

SM.K$date <- as.Date(SM.K$date)

data2 <- merge(data,SM.K, by = "date")


# ALL YEARS
K.plot1_vault_2019 <- data2 %>% 
  select(date, starts_with('k600')) %>%
  gather(type, k600.value, starts_with('k600')) %>%  ggplot(aes(x=date, y=k600.value, color=type)) + geom_line() + theme_bw() +
  scale_color_discrete('variable') + labs(title = "K600, vault 2019. From Raymond Equations ", y = expression(paste("K600 (", d^-1, ")")))
K.plot1_vault_2019










############### vault 2020 ############ 


#slope
vaul.slope <- 0.90629/100


Q_2020 <- read.csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_Discharge/Predicted_Discharge/2020/Q_2020.csv")
Q_2020$DateTime <-  force_tz(as.POSIXct(ymd_hms(Q_2020$DateTime)), tz = "America/Anchorage")
#### Breaking up into sites ####
vaul_Q_2020 <- Q_2020 %>% filter(Site == "VAUL")

vaul.2020.Q <- vaul_Q_2020 %>%
  select(DateTime, Q, Site)









############# Continuous Depth ############



# # vaul 2020:

#download flowmeter data

#download flowmeter data
WR_20.url <- "https://drive.google.com/drive/u/1/folders/1l-QIICuviZvugbNGrvoLfkCDgo1HyDMg"
WR.20.1 <- drive_get(as_id(WR_20.url))
vaul.wr20_glist <- drive_ls(WR.20.1, pattern = "R_Flowmeter Q calculation_VAUL_for_R_JAA.csv")
walk(vaul.wr20_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
vaul_WR_20.Data <- read.csv("R_Flowmeter Q calculation_VAUL_for_R_JAA.csv",
                            header = TRUE, na.strings=c("","NA","blank")) %>% rename(Date = ï..Date)

# vaul_WR_20.Data <- vaul_WR_20.Data %>% rename(Date = ï..Date)

vaul_WR_20.Data <- vaul_WR_20.Data %>% tidyr::fill(Date, .direction = ("down"))

vaul_WR_20.Data <- vaul_WR_20.Data %>% tidyr::fill(Time, .direction = ("down"))

vaul_WR_20.Data$datetimeAK <- as.POSIXct(paste(as.character(vaul_WR_20.Data$Date), vaul_WR_20.Data$Time), format="%y%m%d %H:%M")


vaul_WR_20.Data <- vaul_WR_20.Data %>%
  select(Depth..cm., datetimeAK)

vaul_depth_20_WR <- ddply(na.omit(vaul_WR_20.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# vaul_wr.lm <- lm(meanDepth~datetimeAK, vaul_depth_20)


vaul_depth_20_WR <- setDT(vaul_depth_20_WR)

vaul_depth_20_WR <- vaul_depth_20_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

# vaul.2020.pt <- vaul.2020.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# vaul.2020.pt <- vaul.2020.pt %>%
#   mutate(across(c(AvgAbsDepth),
#                 ~ifelse(datetimeAK >= "2020-08-26 02:00:00" & datetimeAK <= "2020-09-08 12:15:00", NA, .)))



vaul.2020.q.dt <- vaul.2020.Q
vaul.2020.q.dt <- vaul.2020.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(vaul_depth_20_WR)
setDT(vaul.2020.q.dt)

vaul.2020.q.dt$datetimeAK1 <- vaul.2020.q.dt$datetimeAK

setkey( vaul.2020.q.dt, datetimeAK )
setkey( vaul_depth_20_WR, datetimeAK )

#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_vaul20 <- vaul.2020.q.dt[ vaul_depth_20_WR, roll = "nearest" ]

rounded.dates_vaul20_WR_Q <- rounded.dates_vaul20 %>% rename(discharge = Q) %>% 
  select(datetimeAK, discharge, meanDepth)

#convert to meters
rounded.dates_vaul20_WR_Q$meanDepth <- rounded.dates_vaul20_WR_Q$meanDepth /100

vaul_pt_wr_graph <- ggplot(rounded.dates_vaul20_WR_Q, aes(discharge, meanDepth)) +
  geom_point()
# Add regression line
vaul_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("Depth (WR)")

ggsave("plots/vaul_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


vaul20_depth_mod <- lm(meanDepth~discharge, data = rounded.dates_vaul20_WR_Q)

summary(vaul20_depth_mod)

plot(meanDepth~discharge, data = rounded.dates_vaul20_WR_Q)
# abline(vaul20_depth_mod)

#extract slope of model and develop rating curve
vaul.2020.q.dt$RatingCurveDepth <- vaul20_depth_mod$coefficients[1]+(vaul20_depth_mod$coefficients[2])*vaul.2020.q.dt$Q




vaul.2020.q.dt$date <- as.Date(vaul.2020.q.dt$datetimeAK)

daily.mean.depth.vaul <- vaul.2020.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveDepth), list(name = mean))



#########




############# Continuous Velocity ############



# # vaul 2020:


#download flowmeter data
WR_20.url <- "https://drive.google.com/drive/u/1/folders/1l-QIICuviZvugbNGrvoLfkCDgo1HyDMg"
WR.20.1 <- drive_get(as_id(WR_20.url))
vaul.wr20_glist <- drive_ls(WR.20.1, pattern = "R_Flowmeter Q calculation_VAUL_for_R_JAA.csv")
walk(vaul.wr20_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
vaul_WR_20.Data <- read.csv("R_Flowmeter Q calculation_VAUL_for_R_JAA.csv",
                            header = TRUE, na.strings=c("","NA","blank")) %>% rename(Date = ï..Date)

# vaul_WR_20.Data <- vaul_WR_20.Data %>% rename(Date = ï..Date)

vaul_WR_20.Data <- vaul_WR_20.Data %>% tidyr::fill(Date, .direction = ("down"))

vaul_WR_20.Data <- vaul_WR_20.Data %>% tidyr::fill(Time, .direction = ("down"))

vaul_WR_20.Data$datetimeAK <- as.POSIXct(paste(as.character(vaul_WR_20.Data$Date), vaul_WR_20.Data$Time), format="%y%m%d %H:%M")

vaul_WR_20.Data <- vaul_WR_20.Data %>%
  select(Mean.Vel..m.s., datetimeAK)

vaul_V_20_WR <- ddply(na.omit(vaul_WR_20.Data), .(datetimeAK), summarize, meanVelocity = mean(as.numeric(Mean.Vel..m.s.)))

# vaul_wr.lm <- lm(meanDepth~datetimeAK, vaul_depth_20)


vaul_V_20_WR <- setDT(vaul_V_20_WR)

vaul_V_20_WR <- vaul_V_20_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

# vaul.2020.pt <- vaul.2020.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# vaul.2020.pt <- vaul.2020.pt %>%
#   mutate(across(c(AvgAbsV),
#                 ~ifelse(datetimeAK >= "2020-08-26 02:00:00" & datetimeAK <= "2020-09-08 12:15:00", NA, .)))



vaul.2020.q.dt <- vaul.2020.Q
vaul.2020.q.dt <- vaul.2020.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(vaul_V_20_WR)
setDT(vaul.2020.q.dt)

vaul.2020.q.dt$datetimeAK1 <- vaul.2020.q.dt$datetimeAK

setkey( vaul.2020.q.dt, datetimeAK )
setkey( vaul_V_20_WR, datetimeAK )

#WR was taken when EXO out of water. round V point to nearest in data record
rounded.dates_vaul20 <- vaul.2020.q.dt[ vaul_V_20_WR, roll = "nearest" ]

rounded.dates_vaul20_WR_Q <- rounded.dates_vaul20  %>% rename (discharge = Q) %>% select(datetimeAK, discharge, meanVelocity)


vaul_pt_wr_graph <- ggplot(rounded.dates_vaul20_WR_Q, aes(discharge, meanVelocity)) +
  geom_point()
# Add regression line
vaul_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("V (WR)")

ggsave("plots/vaul_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


vaul20_V_mod <- lm(meanVelocity~discharge, data = rounded.dates_vaul20_WR_Q)

summary(vaul20_V_mod)

plot(meanVelocity~discharge, data = rounded.dates_vaul20_WR_Q)
# abline(vaul20_V_mod)

#extract slope of model and develop rating curve
vaul.2020.q.dt$RatingCurveV <- vaul20_V_mod$coefficients[1]+(vaul20_V_mod$coefficients[2])*vaul.2020.q.dt$Q




vaul.2020.q.dt$date <- as.Date(vaul.2020.q.dt$datetimeAK)

daily.mean.velocity.vaul <- vaul.2020.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveV), list(name = mean))




######### Daily Q ##########


#discharge
vaul.2020.Q$date <- as.Date(vaul.2020.Q$DateTime)
vaul.2020.Q <- vaul.2020.Q %>% rename(discharge = Q)


vaul.discharge <- na.omit(vaul.2020.Q) %>%
  group_by(date) %>%
  summarise_at(vars(discharge), list(name = mean)) %>% rename(discharge = name)



daily.mean.velocity.vaul <- daily.mean.velocity.vaul %>% rename(velocity = name)
daily.mean.depth.vaul <- daily.mean.depth.vaul %>% rename(depth = name)



data <- merge(daily.mean.velocity.vaul,daily.mean.depth.vaul, by = "date")
data <- merge(data, vaul.discharge, by = "date")

#slope
vaul.slope <- 0.90629/100

#Equations

data$k600.1 <-  ((data$velocity * vaul.slope)^(0.89)) * (data$depth^(0.54)) * 5037

data$k600.3 <-  1162 * (vaul.slope^(0.77)) * (data$velocity^(0.85))

data$k600.4 <- ((data$velocity*vaul.slope)^(0.76)) * 951.5

data$k600.5 <- (data$velocity*vaul.slope) * 2841 + 2.02

data$k600.6 <- 929*((data$velocity*vaul.slope)^(0.75)) * data$discharge^(0.011)

data$k600.7 <- 4725*((data$velocity*vaul.slope)^(0.86)) * (data$discharge^(-0.14)) *(data$depth^(0.66))


metab.vault <- read.csv(here("outputs","vault2020-Run_2023-01-09.csv"))

SM.K <- metab.vault %>% select(date, K600_daily_mean)
SM.K <- SM.K %>% rename(k600.SM = K600_daily_mean)

SM.K$date <- as.Date(SM.K$date)

data2 <- merge(data,SM.K, by = "date")


# ALL YEARS
K.plot1_vault_2020 <- data2 %>% 
  select(date, starts_with('k600')) %>%
  gather(type, k600.value, starts_with('k600')) %>%  ggplot(aes(x=date, y=k600.value, color=type)) + geom_line() + theme_bw() +
  scale_color_discrete('variable') + labs(title = "K600, vault 2020. From Raymond Equations ", y = expression(paste("K600 (", d^-1, ")")))
K.plot1_vault_2020










###### vaul 2021

############### vault 2021 ############ 


#slope
vaul.slope <- 0.90629/100


Q_2021 <- read.csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_Discharge/Predicted_Discharge/2021/Q_2021.csv")
Q_2021$DateTime <-  force_tz(as.POSIXct(ymd_hms(Q_2021$DateTime)), tz = "America/Anchorage")
#### Breaking up into sites ####
vaul_Q_2021 <- Q_2021 %>% filter(Site == "VAUL")

vaul.2021.Q <- vaul_Q_2021 %>%
  select(DateTime, Q, Site)









############# Continuous Depth ############



# # vaul 2021:

#download flowmeter data



#download flowmeter data
WR_21.url <- "https://drive.google.com/drive/u/1/folders/13avby555rryYttGDgO8sKE7umZPmo5uB"
WR.21.1 <- drive_get(as_id(WR_21.url))
vaul.wr21_glist <- drive_ls(WR.21.1, pattern = "R_Flowmeter Q calculation_VAUL_for_R_JAA_2021.csv")
walk(vaul.wr21_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
vaul_WR_21.Data <- read.csv("R_Flowmeter Q calculation_VAUL_for_R_JAA_2021.csv",
                            header = TRUE, na.strings=c("","NA","blank")) %>% rename(Date = ï..Date)

vaul_WR_21.Data <- vaul_WR_21.Data %>% tidyr::fill(Date, .direction = ("down"))

vaul_WR_21.Data <- vaul_WR_21.Data %>% tidyr::fill(Time, .direction = ("down"))


vaul_WR_21.Data$datetimeAK <- as.POSIXct(paste(as.character(vaul_WR_21.Data$Date), vaul_WR_21.Data$Time), format="%y%m%d %H:%M")


vaul_WR_21.Data <- vaul_WR_21.Data %>%
  select(Depth..cm., datetimeAK)

vaul_depth_21_WR <- ddply(na.omit(vaul_WR_21.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# vaul_wr.lm <- lm(meanDepth~datetimeAK, vaul_depth_21)


vaul_depth_21_WR <- setDT(vaul_depth_21_WR)

vaul_depth_21_WR <- vaul_depth_21_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

# vaul.2021.pt <- vaul.2021.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# vaul.2021.pt <- vaul.2021.pt %>%
#   mutate(across(c(AvgAbsDepth),
#                 ~ifelse(datetimeAK >= "2021-08-26 02:00:00" & datetimeAK <= "2021-09-08 12:15:00", NA, .)))



vaul.2021.q.dt <- vaul.2021.Q
vaul.2021.q.dt <- vaul.2021.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(vaul_depth_21_WR)
setDT(vaul.2021.q.dt)

vaul.2021.q.dt$datetimeAK1 <- vaul.2021.q.dt$datetimeAK

setkey( vaul.2021.q.dt, datetimeAK )
setkey( vaul_depth_21_WR, datetimeAK )

#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_vaul21 <- vaul.2021.q.dt[ vaul_depth_21_WR, roll = "nearest" ]

rounded.dates_vaul21_WR_Q <- rounded.dates_vaul21 %>% rename(discharge = Q) %>% 
  select(datetimeAK, discharge, meanDepth)

#convert to meters
rounded.dates_vaul21_WR_Q$meanDepth <- rounded.dates_vaul21_WR_Q$meanDepth /100

vaul_pt_wr_graph <- ggplot(rounded.dates_vaul21_WR_Q, aes(discharge, meanDepth)) +
  geom_point()
# Add regression line
vaul_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("Depth (WR)")

ggsave("plots/vaul_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


vaul21_depth_mod <- lm(meanDepth~discharge, data = rounded.dates_vaul21_WR_Q)

summary(vaul21_depth_mod)

plot(meanDepth~discharge, data = rounded.dates_vaul21_WR_Q)
# abline(vaul21_depth_mod)

#extract slope of model and develop rating curve
vaul.2021.q.dt$RatingCurveDepth <- vaul21_depth_mod$coefficients[1]+(vaul21_depth_mod$coefficients[2])*vaul.2021.q.dt$Q




vaul.2021.q.dt$date <- as.Date(vaul.2021.q.dt$datetimeAK)

daily.mean.depth.vaul <- vaul.2021.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveDepth), list(name = mean))



#########



############# Continuous Velocity ############



# # vaul 2021:



#download flowmeter data
WR_21.url <- "https://drive.google.com/drive/u/1/folders/13avby555rryYttGDgO8sKE7umZPmo5uB"
WR.21.1 <- drive_get(as_id(WR_21.url))
vaul.wr21_glist <- drive_ls(WR.21.1, pattern = "R_Flowmeter Q calculation_VAUL_for_R_JAA_2021.csv")
walk(vaul.wr21_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
vaul_WR_21.Data <- read.csv("R_Flowmeter Q calculation_VAUL_for_R_JAA_2021.csv",
                            header = TRUE, na.strings=c("","NA","blank")) %>% rename(Date = ï..Date)

vaul_WR_21.Data <- vaul_WR_21.Data %>% tidyr::fill(Date, .direction = ("down"))

vaul_WR_21.Data <- vaul_WR_21.Data %>% tidyr::fill(Time, .direction = ("down"))


vaul_WR_21.Data$datetimeAK <- as.POSIXct(paste(as.character(vaul_WR_21.Data$Date), vaul_WR_21.Data$Time), format="%y%m%d %H:%M")

vaul_WR_21.Data <- vaul_WR_21.Data %>%
  select(Mean.Vel..m.s., datetimeAK)

vaul_V_21_WR <- ddply(na.omit(vaul_WR_21.Data), .(datetimeAK), summarize, meanVelocity = mean(as.numeric(Mean.Vel..m.s.)))

# vaul_wr.lm <- lm(meanDepth~datetimeAK, vaul_depth_21)


vaul_V_21_WR <- setDT(vaul_V_21_WR)

vaul_V_21_WR <- vaul_V_21_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

# vaul.2021.pt <- vaul.2021.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# vaul.2021.pt <- vaul.2021.pt %>%
#   mutate(across(c(AvgAbsV),
#                 ~ifelse(datetimeAK >= "2021-08-26 02:00:00" & datetimeAK <= "2021-09-08 12:15:00", NA, .)))



vaul.2021.q.dt <- vaul.2021.Q
vaul.2021.q.dt <- vaul.2021.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(vaul_V_21_WR)
setDT(vaul.2021.q.dt)

vaul.2021.q.dt$datetimeAK1 <- vaul.2021.q.dt$datetimeAK

setkey( vaul.2021.q.dt, datetimeAK )
setkey( vaul_V_21_WR, datetimeAK )

#WR was taken when EXO out of water. round V point to nearest in data record
rounded.dates_vaul21 <- vaul.2021.q.dt[ vaul_V_21_WR, roll = "nearest" ]

rounded.dates_vaul21_WR_Q <- rounded.dates_vaul21  %>% rename (discharge = Q) %>% select(datetimeAK, discharge, meanVelocity)


vaul_pt_wr_graph <- ggplot(rounded.dates_vaul21_WR_Q, aes(discharge, meanVelocity)) +
  geom_point()
# Add regression line
vaul_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("V (WR)")

ggsave("plots/vaul_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


vaul21_V_mod <- lm(meanVelocity~discharge, data = rounded.dates_vaul21_WR_Q)

summary(vaul21_V_mod)

plot(meanVelocity~discharge, data = rounded.dates_vaul21_WR_Q)
# abline(vaul21_V_mod)

#extract slope of model and develop rating curve
vaul.2021.q.dt$RatingCurveV <- vaul21_V_mod$coefficients[1]+(vaul21_V_mod$coefficients[2])*vaul.2021.q.dt$Q




vaul.2021.q.dt$date <- as.Date(vaul.2021.q.dt$datetimeAK)

daily.mean.velocity.vaul <- vaul.2021.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveV), list(name = mean))




######### Daily Q ##########


#discharge
vaul.2021.Q$date <- as.Date(vaul.2021.Q$DateTime)
vaul.2021.Q <- vaul.2021.Q %>% rename(discharge = Q)


vaul.discharge <- na.omit(vaul.2021.Q) %>%
  group_by(date) %>%
  summarise_at(vars(discharge), list(name = mean)) %>% rename(discharge = name)



daily.mean.velocity.vaul <- daily.mean.velocity.vaul %>% rename(velocity = name)
daily.mean.depth.vaul <- daily.mean.depth.vaul %>% rename(depth = name)



data <- merge(daily.mean.velocity.vaul,daily.mean.depth.vaul, by = "date")
data <- merge(data, vaul.discharge, by = "date")

#slope
vaul.slope <- 0.90629/100

#Equations

data$k600.1 <-  ((data$velocity * vaul.slope)^(0.89)) * (data$depth^(0.54)) * 5037

data$k600.3 <-  1162 * (vaul.slope^(0.77)) * (data$velocity^(0.85))

data$k600.4 <- ((data$velocity*vaul.slope)^(0.76)) * 951.5

data$k600.5 <- (data$velocity*vaul.slope) * 2841 + 2.02

data$k600.6 <- 929*((data$velocity*vaul.slope)^(0.75)) * data$discharge^(0.011)

data$k600.7 <- 4725*((data$velocity*vaul.slope)^(0.86)) * (data$discharge^(-0.14)) *(data$depth^(0.66))


metab.vault <- read.csv(here("outputs","vault2021-Run_2023-01-09.csv"))

SM.K <- metab.vault %>% select(date, K600_daily_mean)
SM.K <- SM.K %>% rename(k600.SM = K600_daily_mean)

SM.K$date <- as.Date(SM.K$date)

data2 <- merge(data,SM.K, by = "date")


# ALL YEARS
K.plot1_vault_2021 <- data2 %>% 
  select(date, starts_with('k600')) %>%
  gather(type, k600.value, starts_with('k600')) %>%  ggplot(aes(x=date, y=k600.value, color=type)) + geom_line() + theme_bw() +
  scale_color_discrete('variable') + labs(title = "K600, vault 2021. From Raymond Equations ", y = expression(paste("K600 (", d^-1, ")")))
K.plot1_vault_2021








####################### Poker #####################



############### poker 2019 ############ 


#slope
poke.slope <- 2.36021/100


Q_2019 <- read.csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_Discharge/Predicted_Discharge/2019/Q_2019.csv")
Q_2019$DateTime <-  force_tz(as.POSIXct(ymd_hms(Q_2019$DateTime)), tz = "America/Anchorage")
#### Breaking up into sites ####
poke_Q_2019 <- Q_2019 %>% filter(Site == "POKE")

poke.2019.Q <- poke_Q_2019 %>%
  select(DateTime, Q, Site)









############# Continuous Depth ############



#download flowmeter data
WR_19.url <- "https://drive.google.com/drive/u/1/folders/1bm62_JO1dKrFPyUCz8E88K5q7IHElXPP"
WR.19.1 <- drive_get(as_id(WR_19.url))
poke.wr19_glist <- drive_ls(WR.19.1, pattern = "poke_2019_flowmeter_Q_for_R_JAA.csv")
walk(poke.wr19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
poke_WR_19.Data <- read.csv("poke_2019_flowmeter_Q_for_R_JAA.csv",
                            skip = 1, header = TRUE, na.strings=c("","NA","blank"))

poke_WR_19.Data <- poke_WR_19.Data %>% tidyr::fill(Date, .direction = ("down"))

poke_WR_19.Data <- poke_WR_19.Data %>% tidyr::fill(Time, .direction = ("down"))


poke_WR_19.Data$datetimeAK <- as.POSIXct(paste(poke_WR_19.Data$Date, poke_WR_19.Data$Time), format="%m/%d/%Y %H:%M")





poke_WR_19.Data <- poke_WR_19.Data %>%
  select(Depth..cm., datetimeAK)

poke_depth_19_WR <- ddply(na.omit(poke_WR_19.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# poke_wr.lm <- lm(meanDepth~datetimeAK, poke_depth_19)


poke_depth_19_WR <- setDT(poke_depth_19_WR)

poke_depth_19_WR <- poke_depth_19_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

# poke.2019.pt <- poke.2019.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# poke.2019.pt <- poke.2019.pt %>%
#   mutate(across(c(AvgAbsDepth),
#                 ~ifelse(datetimeAK >= "2019-08-26 02:00:00" & datetimeAK <= "2019-09-08 12:15:00", NA, .)))



poke.2019.q.dt <- poke.2019.Q
poke.2019.q.dt <- poke.2019.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(poke_depth_19_WR)
setDT(poke.2019.q.dt)

poke.2019.q.dt$datetimeAK1 <- poke.2019.q.dt$datetimeAK

setkey( poke.2019.q.dt, datetimeAK )
setkey( poke_depth_19_WR, datetimeAK )

#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_poke19 <- poke.2019.q.dt[ poke_depth_19_WR, roll = "nearest" ]

rounded.dates_poke19_WR_Q <- rounded.dates_poke19 %>% rename(discharge = Q) %>% 
  select(datetimeAK, discharge, meanDepth)

#convert to meters
rounded.dates_poke19_WR_Q$meanDepth <- rounded.dates_poke19_WR_Q$meanDepth /100

poke_pt_wr_graph <- ggplot(rounded.dates_poke19_WR_Q, aes(discharge, meanDepth)) +
  geom_point()
# Add regression line
poke_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("Depth (WR)")

ggsave("plots/poke_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


poke19_depth_mod <- lm(meanDepth~discharge, data = rounded.dates_poke19_WR_Q)

summary(poke19_depth_mod)

plot(meanDepth~discharge, data = rounded.dates_poke19_WR_Q)
# abline(poke19_depth_mod)

#extract slope of model and develop rating curve
poke.2019.q.dt$RatingCurveDepth <- poke19_depth_mod$coefficients[1]+(poke19_depth_mod$coefficients[2])*poke.2019.q.dt$Q




poke.2019.q.dt$date <- as.Date(poke.2019.q.dt$datetimeAK)

daily.mean.depth.poke <- poke.2019.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveDepth), list(name = mean))



#########




############# Continuous Velocity ############




#download flowmeter data
WR_19.url <- "https://drive.google.com/drive/u/1/folders/1bm62_JO1dKrFPyUCz8E88K5q7IHElXPP"
WR.19.1 <- drive_get(as_id(WR_19.url))
poke.wr19_glist <- drive_ls(WR.19.1, pattern = "poke_2019_flowmeter_Q_for_R_JAA.csv")
walk(poke.wr19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
poke_WR_19.Data <- read.csv("poke_2019_flowmeter_Q_for_R_JAA.csv",
                            skip = 1, header = TRUE, na.strings=c("","NA","blank"))

poke_WR_19.Data <- poke_WR_19.Data %>% tidyr::fill(Date, .direction = ("down"))

poke_WR_19.Data <- poke_WR_19.Data %>% tidyr::fill(Time, .direction = ("down"))


poke_WR_19.Data$datetimeAK <- as.POSIXct(paste(poke_WR_19.Data$Date, poke_WR_19.Data$Time), format="%m/%d/%Y %H:%M")






poke_WR_19.Data <- poke_WR_19.Data %>%
  select(Mean.Vel..m.s., datetimeAK)

poke_V_19_WR <- ddply(na.omit(poke_WR_19.Data), .(datetimeAK), summarize, meanVelocity = mean(as.numeric(Mean.Vel..m.s.)))

# poke_wr.lm <- lm(meanDepth~datetimeAK, poke_depth_19)


poke_V_19_WR <- setDT(poke_V_19_WR)

poke_V_19_WR <- poke_V_19_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

# poke.2019.pt <- poke.2019.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# poke.2019.pt <- poke.2019.pt %>%
#   mutate(across(c(AvgAbsV),
#                 ~ifelse(datetimeAK >= "2019-08-26 02:00:00" & datetimeAK <= "2019-09-08 12:15:00", NA, .)))



poke.2019.q.dt <- poke.2019.Q
poke.2019.q.dt <- poke.2019.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(poke_V_19_WR)
setDT(poke.2019.q.dt)

poke.2019.q.dt$datetimeAK1 <- poke.2019.q.dt$datetimeAK

setkey( poke.2019.q.dt, datetimeAK )
setkey( poke_V_19_WR, datetimeAK )

#WR was taken when EXO out of water. round V point to nearest in data record
rounded.dates_poke19 <- poke.2019.q.dt[ poke_V_19_WR, roll = "nearest" ]

rounded.dates_poke19_WR_Q <- rounded.dates_poke19  %>% rename (discharge = Q) %>% select(datetimeAK, discharge, meanVelocity)


poke_pt_wr_graph <- ggplot(rounded.dates_poke19_WR_Q, aes(discharge, meanVelocity)) +
  geom_point()
# Add regression line
poke_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("V (WR)")

ggsave("plots/poke_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


poke19_V_mod <- lm(meanVelocity~discharge, data = rounded.dates_poke19_WR_Q)

summary(poke19_V_mod)

plot(meanVelocity~discharge, data = rounded.dates_poke19_WR_Q)
# abline(poke19_V_mod)

#extract slope of model and develop rating curve
poke.2019.q.dt$RatingCurveV <- poke19_V_mod$coefficients[1]+(poke19_V_mod$coefficients[2])*poke.2019.q.dt$Q




poke.2019.q.dt$date <- as.Date(poke.2019.q.dt$datetimeAK)

daily.mean.velocity.poke <- poke.2019.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveV), list(name = mean))




######### Daily Q ##########


#discharge
poke.2019.Q$date <- as.Date(poke.2019.Q$DateTime)
poke.2019.Q <- poke.2019.Q %>% rename(discharge = Q)


poke.discharge <- na.omit(poke.2019.Q) %>%
  group_by(date) %>%
  summarise_at(vars(discharge), list(name = mean)) %>% rename(discharge = name)



daily.mean.velocity.poke <- daily.mean.velocity.poke %>% rename(velocity = name)
daily.mean.depth.poke <- daily.mean.depth.poke %>% rename(depth = name)



data <- merge(daily.mean.velocity.poke,daily.mean.depth.poke, by = "date")
data <- merge(data, poke.discharge, by = "date")
view(data)



#Equations

data$k600.1 <-  ((data$velocity * poke.slope)^(0.89)) * (data$depth^(0.54)) * 5037

data$k600.3 <-  1162 * (poke.slope^(0.77)) * (data$velocity^(0.85))

data$k600.4 <- ((data$velocity*poke.slope)^(0.76)) * 951.5

data$k600.5 <- (data$velocity*poke.slope) * 2841 + 2.02

data$k600.6 <- 929*((data$velocity*poke.slope)^(0.75)) * data$discharge^(0.011)

data$k600.7 <- 4725*((data$velocity*poke.slope)^(0.86)) * (data$discharge^(-0.14)) *(data$depth^(0.66))

view(data)

metab.poker <- read.csv(here("outputs","poker2019-Run_2023-01-09.csv"))

SM.K <- metab.poker %>% select(date, K600_daily_mean)
SM.K <- SM.K %>% rename(k600.SM = K600_daily_mean)

SM.K$date <- as.Date(SM.K$date)

data2 <- merge(data,SM.K, by = "date")


# ALL YEARS
K.plot1_poker_2019 <- data2 %>% 
  select(date, starts_with('k600')) %>%
  gather(type, k600.value, starts_with('k600')) %>%  ggplot(aes(x=date, y=k600.value, color=type)) + geom_line() + theme_bw() +
  scale_color_discrete('variable') + labs(title = "K600, poker 2019. From Raymond Equations ", y = expression(paste("K600 (", d^-1, ")")))
K.plot1_poker_2019










############### poker 2020 ############ 


#slope
poke.slope <- 2.36021/100


Q_2020 <- read.csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_Discharge/Predicted_Discharge/2020/Q_2020.csv")
Q_2020$DateTime <-  force_tz(as.POSIXct(ymd_hms(Q_2020$DateTime)), tz = "America/Anchorage")
#### Breaking up into sites ####
poke_Q_2020 <- Q_2020 %>% filter(Site == "POKE")

poke.2020.Q <- poke_Q_2020 %>%
  select(DateTime, Q, Site)









############# Continuous Depth ############



# # poke 2020:

#download flowmeter data


#download flowmeter data
WR_20.url <- "https://drive.google.com/drive/u/1/folders/1S2L8Qg08AIhQo1ZdKdaxlJliz4bi1ttr"
WR.20.1 <- drive_get(as_id(WR_20.url))
poke.wr20_glist <- drive_ls(WR.20.1, pattern = "R_Flowmeter Q calculation_POKE_for_R_JAA.csv")
walk(poke.wr20_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
poke_WR_20.Data <- read.csv("R_Flowmeter Q calculation_POKE_for_R_JAA.csv",
                            header = TRUE, na.strings=c("","NA","blank"))%>% rename(Date = ï..Date)

poke_WR_20.Data <- poke_WR_20.Data %>% tidyr::fill(Date, .direction = ("down"))

poke_WR_20.Data <- poke_WR_20.Data %>% tidyr::fill(Time, .direction = ("down"))



poke_WR_20.Data$datetimeAK <- as.POSIXct(paste(as.character(poke_WR_20.Data$Date), poke_WR_20.Data$Time), format="%y%m%d %H:%M")


poke_WR_20.Data <- poke_WR_20.Data %>%
  select(Depth, datetimeAK)

poke_depth_20_WR <- ddply(na.omit(poke_WR_20.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth)))

# poke_wr.lm <- lm(meanDepth~datetimeAK, poke_depth_20)


poke_depth_20_WR <- setDT(poke_depth_20_WR)

poke_depth_20_WR <- poke_depth_20_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

# poke.2020.pt <- poke.2020.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# poke.2020.pt <- poke.2020.pt %>%
#   mutate(across(c(AvgAbsDepth),
#                 ~ifelse(datetimeAK >= "2020-08-26 02:00:00" & datetimeAK <= "2020-09-08 12:15:00", NA, .)))



poke.2020.q.dt <- poke.2020.Q
poke.2020.q.dt <- poke.2020.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(poke_depth_20_WR)
setDT(poke.2020.q.dt)

poke.2020.q.dt$datetimeAK1 <- poke.2020.q.dt$datetimeAK

setkey( poke.2020.q.dt, datetimeAK )
setkey( poke_depth_20_WR, datetimeAK )

#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_poke20 <- poke.2020.q.dt[ poke_depth_20_WR, roll = "nearest" ]

rounded.dates_poke20_WR_Q <- rounded.dates_poke20 %>% rename(discharge = Q) %>% 
  select(datetimeAK, discharge, meanDepth)

#convert to meters
rounded.dates_poke20_WR_Q$meanDepth <- rounded.dates_poke20_WR_Q$meanDepth /100

poke_pt_wr_graph <- ggplot(rounded.dates_poke20_WR_Q, aes(discharge, meanDepth)) +
  geom_point()
# Add regression line
poke_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("Depth (WR)")

ggsave("plots/poke_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


poke20_depth_mod <- lm(meanDepth~discharge, data = rounded.dates_poke20_WR_Q)

summary(poke20_depth_mod)

plot(meanDepth~discharge, data = rounded.dates_poke20_WR_Q)
# abline(poke20_depth_mod)

#extract slope of model and develop rating curve
poke.2020.q.dt$RatingCurveDepth <- poke20_depth_mod$coefficients[1]+(poke20_depth_mod$coefficients[2])*poke.2020.q.dt$Q




poke.2020.q.dt$date <- as.Date(poke.2020.q.dt$datetimeAK)

daily.mean.depth.poke <- poke.2020.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveDepth), list(name = mean))



#########




############# Continuous Velocity ############



# # poke 2020:



#download flowmeter data
WR_20.url <- "https://drive.google.com/drive/u/1/folders/1S2L8Qg08AIhQo1ZdKdaxlJliz4bi1ttr"
WR.20.1 <- drive_get(as_id(WR_20.url))
poke.wr20_glist <- drive_ls(WR.20.1, pattern = "R_Flowmeter Q calculation_POKE_for_R_JAA.csv")
walk(poke.wr20_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
poke_WR_20.Data <- read.csv("R_Flowmeter Q calculation_POKE_for_R_JAA.csv",
                            header = TRUE, na.strings=c("","NA","blank"))%>% rename(Date = ï..Date)

poke_WR_20.Data <- poke_WR_20.Data %>% tidyr::fill(Date, .direction = ("down"))

poke_WR_20.Data <- poke_WR_20.Data %>% tidyr::fill(Time, .direction = ("down"))



poke_WR_20.Data$datetimeAK <- as.POSIXct(paste(as.character(poke_WR_20.Data$Date), poke_WR_20.Data$Time), format="%y%m%d %H:%M")


poke_WR_20.Data <- poke_WR_20.Data %>%
  select(Mean.Vel, datetimeAK)

poke_V_20_WR <- ddply(na.omit(poke_WR_20.Data), .(datetimeAK), summarize, meanVelocity = mean(as.numeric(Mean.Vel)))

# poke_wr.lm <- lm(meanDepth~datetimeAK, poke_depth_20)


poke_V_20_WR <- setDT(poke_V_20_WR)

poke_V_20_WR <- poke_V_20_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

# poke.2020.pt <- poke.2020.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# poke.2020.pt <- poke.2020.pt %>%
#   mutate(across(c(AvgAbsV),
#                 ~ifelse(datetimeAK >= "2020-08-26 02:00:00" & datetimeAK <= "2020-09-08 12:15:00", NA, .)))



poke.2020.q.dt <- poke.2020.Q
poke.2020.q.dt <- poke.2020.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(poke_V_20_WR)
setDT(poke.2020.q.dt)

poke.2020.q.dt$datetimeAK1 <- poke.2020.q.dt$datetimeAK

setkey( poke.2020.q.dt, datetimeAK )
setkey( poke_V_20_WR, datetimeAK )

#WR was taken when EXO out of water. round V point to nearest in data record
rounded.dates_poke20 <- poke.2020.q.dt[ poke_V_20_WR, roll = "nearest" ]

rounded.dates_poke20_WR_Q <- rounded.dates_poke20  %>% rename (discharge = Q) %>% select(datetimeAK, discharge, meanVelocity)


poke_pt_wr_graph <- ggplot(rounded.dates_poke20_WR_Q, aes(discharge, meanVelocity)) +
  geom_point()
# Add regression line
poke_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("V (WR)")

ggsave("plots/poke_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


poke20_V_mod <- lm(meanVelocity~discharge, data = rounded.dates_poke20_WR_Q)

summary(poke20_V_mod)

plot(meanVelocity~discharge, data = rounded.dates_poke20_WR_Q)
# abline(poke20_V_mod)

#extract slope of model and develop rating curve
poke.2020.q.dt$RatingCurveV <- poke20_V_mod$coefficients[1]+(poke20_V_mod$coefficients[2])*poke.2020.q.dt$Q




poke.2020.q.dt$date <- as.Date(poke.2020.q.dt$datetimeAK)

daily.mean.velocity.poke <- poke.2020.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveV), list(name = mean))




######### Daily Q ##########


#discharge
poke.2020.Q$date <- as.Date(poke.2020.Q$DateTime)
poke.2020.Q <- poke.2020.Q %>% rename(discharge = Q)


poke.discharge <- na.omit(poke.2020.Q) %>%
  group_by(date) %>%
  summarise_at(vars(discharge), list(name = mean)) %>% rename(discharge = name)



daily.mean.velocity.poke <- daily.mean.velocity.poke %>% rename(velocity = name)
daily.mean.depth.poke <- daily.mean.depth.poke %>% rename(depth = name)



data <- merge(daily.mean.velocity.poke,daily.mean.depth.poke, by = "date")
data <- merge(data, poke.discharge, by = "date")

#slope
poke.slope <- 2.36021/100

#Equations

data$k600.1 <-  ((data$velocity * poke.slope)^(0.89)) * (data$depth^(0.54)) * 5037

data$k600.3 <-  1162 * (poke.slope^(0.77)) * (data$velocity^(0.85))

data$k600.4 <- ((data$velocity*poke.slope)^(0.76)) * 951.5

data$k600.5 <- (data$velocity*poke.slope) * 2841 + 2.02

data$k600.6 <- 929*((data$velocity*poke.slope)^(0.75)) * data$discharge^(0.011)

data$k600.7 <- 4725*((data$velocity*poke.slope)^(0.86)) * (data$discharge^(-0.14)) *(data$depth^(0.66))


metab.poker <- read.csv(here("outputs","poker2020-Run_2023-01-09.csv"))

SM.K <- metab.poker %>% select(date, K600_daily_mean)
SM.K <- SM.K %>% rename(k600.SM = K600_daily_mean)

SM.K$date <- as.Date(SM.K$date)

data2 <- merge(data,SM.K, by = "date")


# ALL YEARS
K.plot1_poker_2020 <- data2 %>% 
  select(date, starts_with('k600')) %>%
  gather(type, k600.value, starts_with('k600')) %>%  ggplot(aes(x=date, y=k600.value, color=type)) + geom_line() + theme_bw() +
  scale_color_discrete('variable') + labs(title = "K600, poker 2020. From Raymond Equations ", y = expression(paste("K600 (", d^-1, ")")))
K.plot1_poker_2020










###### poke 2021

############### poker 2021 ############ 


#slope
poke.slope <- 2.36021/100


Q_2021 <- read.csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_Discharge/Predicted_Discharge/2021/Q_2021.csv")
Q_2021$DateTime <-  force_tz(as.POSIXct(ymd_hms(Q_2021$DateTime)), tz = "America/Anchorage")
#### Breaking up into sites ####
poke_Q_2021 <- Q_2021 %>% filter(Site == "POKE")

poke.2021.Q <- poke_Q_2021 %>%
  select(DateTime, Q, Site)









############# Continuous Depth ############



# # poke 2021:

#download flowmeter data




#download flowmeter data
WR_21.url <- "https://drive.google.com/drive/u/1/folders/18z6vSz6SE3DEvUVDyfM8I3gqkGaxQqOl"
WR.21.1 <- drive_get(as_id(WR_21.url))
poke.wr21_glist <- drive_ls(WR.21.1, pattern = "R_Flowmeter Q calculation_POKE_for_R_JAA_2021.csv")
walk(poke.wr21_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
poke_WR_21.Data <- read.csv("R_Flowmeter Q calculation_POKE_for_R_JAA_2021.csv",
                            header = TRUE, na.strings=c("","NA","blank")) %>% rename(Date = ï..Date)

poke_WR_21.Data <- poke_WR_21.Data %>% tidyr::fill(Date, .direction = ("down"))

poke_WR_21.Data <- poke_WR_21.Data %>% tidyr::fill(Time, .direction = ("down"))


poke_WR_21.Data$datetimeAK <- as.POSIXct(paste(as.character(poke_WR_21.Data$Date), poke_WR_21.Data$Time), format="%y%m%d %H:%M")



poke_WR_21.Data <- poke_WR_21.Data %>%
  select(Depth..cm., datetimeAK)

poke_depth_21_WR <- ddply(na.omit(poke_WR_21.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# poke_wr.lm <- lm(meanDepth~datetimeAK, poke_depth_21)


poke_depth_21_WR <- setDT(poke_depth_21_WR)

poke_depth_21_WR <- poke_depth_21_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

# poke.2021.pt <- poke.2021.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# poke.2021.pt <- poke.2021.pt %>%
#   mutate(across(c(AvgAbsDepth),
#                 ~ifelse(datetimeAK >= "2021-08-26 02:00:00" & datetimeAK <= "2021-09-08 12:15:00", NA, .)))



poke.2021.q.dt <- poke.2021.Q
poke.2021.q.dt <- poke.2021.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(poke_depth_21_WR)
setDT(poke.2021.q.dt)

poke.2021.q.dt$datetimeAK1 <- poke.2021.q.dt$datetimeAK

setkey( poke.2021.q.dt, datetimeAK )
setkey( poke_depth_21_WR, datetimeAK )

#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_poke21 <- poke.2021.q.dt[ poke_depth_21_WR, roll = "nearest" ]

rounded.dates_poke21_WR_Q <- rounded.dates_poke21 %>% rename(discharge = Q) %>% 
  select(datetimeAK, discharge, meanDepth)

#convert to meters
rounded.dates_poke21_WR_Q$meanDepth <- rounded.dates_poke21_WR_Q$meanDepth /100

poke_pt_wr_graph <- ggplot(rounded.dates_poke21_WR_Q, aes(discharge, meanDepth)) +
  geom_point()
# Add regression line
poke_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("Depth (WR)")

ggsave("plots/poke_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


poke21_depth_mod <- lm(meanDepth~discharge, data = rounded.dates_poke21_WR_Q)

summary(poke21_depth_mod)

plot(meanDepth~discharge, data = rounded.dates_poke21_WR_Q)
# abline(poke21_depth_mod)

#extract slope of model and develop rating curve
poke.2021.q.dt$RatingCurveDepth <- poke21_depth_mod$coefficients[1]+(poke21_depth_mod$coefficients[2])*poke.2021.q.dt$Q




poke.2021.q.dt$date <- as.Date(poke.2021.q.dt$datetimeAK)

daily.mean.depth.poke <- poke.2021.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveDepth), list(name = mean))



#########



############# Continuous Velocity ############



# # poke 2021:




#download flowmeter data
WR_21.url <- "https://drive.google.com/drive/u/1/folders/18z6vSz6SE3DEvUVDyfM8I3gqkGaxQqOl"
WR.21.1 <- drive_get(as_id(WR_21.url))
poke.wr21_glist <- drive_ls(WR.21.1, pattern = "R_Flowmeter Q calculation_POKE_for_R_JAA_2021.csv")
walk(poke.wr21_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
poke_WR_21.Data <- read.csv("R_Flowmeter Q calculation_POKE_for_R_JAA_2021.csv",
                            header = TRUE, na.strings=c("","NA","blank")) %>% rename(Date = ï..Date)

poke_WR_21.Data <- poke_WR_21.Data %>% tidyr::fill(Date, .direction = ("down"))

poke_WR_21.Data <- poke_WR_21.Data %>% tidyr::fill(Time, .direction = ("down"))


poke_WR_21.Data$datetimeAK <- as.POSIXct(paste(as.character(poke_WR_21.Data$Date), poke_WR_21.Data$Time), format="%y%m%d %H:%M")

poke_WR_21.Data <- poke_WR_21.Data %>%
  select(Mean.Vel..m.s., datetimeAK)

poke_V_21_WR <- ddply(na.omit(poke_WR_21.Data), .(datetimeAK), summarize, meanVelocity = mean(as.numeric(Mean.Vel..m.s.)))

# poke_wr.lm <- lm(meanDepth~datetimeAK, poke_depth_21)


poke_V_21_WR <- setDT(poke_V_21_WR)

poke_V_21_WR <- poke_V_21_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

# poke.2021.pt <- poke.2021.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# poke.2021.pt <- poke.2021.pt %>%
#   mutate(across(c(AvgAbsV),
#                 ~ifelse(datetimeAK >= "2021-08-26 02:00:00" & datetimeAK <= "2021-09-08 12:15:00", NA, .)))



poke.2021.q.dt <- poke.2021.Q
poke.2021.q.dt <- poke.2021.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(poke_V_21_WR)
setDT(poke.2021.q.dt)

poke.2021.q.dt$datetimeAK1 <- poke.2021.q.dt$datetimeAK

setkey( poke.2021.q.dt, datetimeAK )
setkey( poke_V_21_WR, datetimeAK )

#WR was taken when EXO out of water. round V point to nearest in data record
rounded.dates_poke21 <- poke.2021.q.dt[ poke_V_21_WR, roll = "nearest" ]

rounded.dates_poke21_WR_Q <- rounded.dates_poke21  %>% rename (discharge = Q) %>% select(datetimeAK, discharge, meanVelocity)


poke_pt_wr_graph <- ggplot(rounded.dates_poke21_WR_Q, aes(discharge, meanVelocity)) +
  geom_point()
# Add regression line
poke_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("V (WR)")

ggsave("plots/poke_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


poke21_V_mod <- lm(meanVelocity~discharge, data = rounded.dates_poke21_WR_Q)

summary(poke21_V_mod)

plot(meanVelocity~discharge, data = rounded.dates_poke21_WR_Q)
# abline(poke21_V_mod)

#extract slope of model and develop rating curve
poke.2021.q.dt$RatingCurveV <- poke21_V_mod$coefficients[1]+(poke21_V_mod$coefficients[2])*poke.2021.q.dt$Q




poke.2021.q.dt$date <- as.Date(poke.2021.q.dt$datetimeAK)

daily.mean.velocity.poke <- poke.2021.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveV), list(name = mean))




######### Daily Q ##########


#discharge
poke.2021.Q$date <- as.Date(poke.2021.Q$DateTime)
poke.2021.Q <- poke.2021.Q %>% rename(discharge = Q)


poke.discharge <- na.omit(poke.2021.Q) %>%
  group_by(date) %>%
  summarise_at(vars(discharge), list(name = mean)) %>% rename(discharge = name)



daily.mean.velocity.poke <- daily.mean.velocity.poke %>% rename(velocity = name)
daily.mean.depth.poke <- daily.mean.depth.poke %>% rename(depth = name)



data <- merge(daily.mean.velocity.poke,daily.mean.depth.poke, by = "date")
data <- merge(data, poke.discharge, by = "date")

#slope
poke.slope <- 2.36021/100

#Equations

data$k600.1 <-  ((data$velocity * poke.slope)^(0.89)) * (data$depth^(0.54)) * 5037

data$k600.3 <-  1162 * (poke.slope^(0.77)) * (data$velocity^(0.85))

data$k600.4 <- ((data$velocity*poke.slope)^(0.76)) * 951.5

data$k600.5 <- (data$velocity*poke.slope) * 2841 + 2.02

data$k600.6 <- 929*((data$velocity*poke.slope)^(0.75)) * data$discharge^(0.011)

data$k600.7 <- 4725*((data$velocity*poke.slope)^(0.86)) * (data$discharge^(-0.14)) *(data$depth^(0.66))


metab.poker <- read.csv(here("outputs","poker2021-Run_2023-01-09.csv"))

SM.K <- metab.poker %>% select(date, K600_daily_mean)
SM.K <- SM.K %>% rename(k600.SM = K600_daily_mean)

SM.K$date <- as.Date(SM.K$date)

data2 <- merge(data,SM.K, by = "date")


# ALL YEARS
K.plot1_poker_2021 <- data2 %>% 
  select(date, starts_with('k600')) %>%
  gather(type, k600.value, starts_with('k600')) %>%  ggplot(aes(x=date, y=k600.value, color=type)) + geom_line() + theme_bw() +
  scale_color_discrete('variable') + labs(title = "K600, poker 2021. From Raymond Equations ", y = expression(paste("K600 (", d^-1, ")")))
K.plot1_poker_2021



















####################### Stuart ################






#################### stuart ################################


############### stuart 2019 ############ 


#slope
strt.slope <- 2.13534/100


Q_2019 <- read.csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_Discharge/Predicted_Discharge/2019/Q_2019.csv")
Q_2019$DateTime <-  force_tz(as.POSIXct(ymd_hms(Q_2019$DateTime)), tz = "America/Anchorage")
#### Breaking up into sites ####
strt_Q_2019 <- Q_2019 %>% filter(Site == "STRT")

strt.2019.Q <- strt_Q_2019 %>%
  select(DateTime, Q, Site)









############# Continuous Depth ############


#download flowmeter data
WR_19.url <- "https://drive.google.com/drive/u/1/folders/1bm62_JO1dKrFPyUCz8E88K5q7IHElXPP"
WR.19.1 <- drive_get(as_id(WR_19.url))
strt.wr19_glist <- drive_ls(WR.19.1, pattern = "STRT_2019_flowmeter_Q_for_R_JAA.csv")
walk(strt.wr19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
strt_WR_19.Data <- read.csv("STRT_2019_flowmeter_Q_for_R_JAA.csv",
                            header = TRUE, na.strings=c("","NA","blank"))

strt_WR_19.Data <- strt_WR_19.Data %>% tidyr::fill(Date, .direction = ("down"))

strt_WR_19.Data <- strt_WR_19.Data %>% tidyr::fill(Time, .direction = ("down"))


strt_WR_19.Data$datetimeAK <- as.POSIXct(paste(strt_WR_19.Data$Date, strt_WR_19.Data$Time), format="%m/%d/%Y %H:%M")




strt_WR_19.Data <- strt_WR_19.Data %>%
  select(Depth..cm., datetimeAK)

strt_depth_19_WR <- ddply(na.omit(strt_WR_19.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# strt_wr.lm <- lm(meanDepth~datetimeAK, strt_depth_19)


strt_depth_19_WR <- setDT(strt_depth_19_WR)

strt_depth_19_WR <- strt_depth_19_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

# strt.2019.pt <- strt.2019.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# strt.2019.pt <- strt.2019.pt %>%
#   mutate(across(c(AvgAbsDepth),
#                 ~ifelse(datetimeAK >= "2019-08-26 02:00:00" & datetimeAK <= "2019-09-08 12:15:00", NA, .)))



strt.2019.q.dt <- strt.2019.Q
strt.2019.q.dt <- strt.2019.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(strt_depth_19_WR)
setDT(strt.2019.q.dt)

strt.2019.q.dt$datetimeAK1 <- strt.2019.q.dt$datetimeAK

setkey( strt.2019.q.dt, datetimeAK )
setkey( strt_depth_19_WR, datetimeAK )

#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_strt19 <- strt.2019.q.dt[ strt_depth_19_WR, roll = "nearest" ]

rounded.dates_strt19_WR_Q <- rounded.dates_strt19 %>% rename(discharge = Q) %>% 
  select(datetimeAK, discharge, meanDepth)

#convert to meters
rounded.dates_strt19_WR_Q$meanDepth <- rounded.dates_strt19_WR_Q$meanDepth /100

strt_pt_wr_graph <- ggplot(rounded.dates_strt19_WR_Q, aes(discharge, meanDepth)) +
  geom_point()
# Add regression line
strt_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("Depth (WR)")

ggsave("plots/strt_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


strt19_depth_mod <- lm(meanDepth~discharge, data = rounded.dates_strt19_WR_Q)

summary(strt19_depth_mod)

plot(meanDepth~discharge, data = rounded.dates_strt19_WR_Q)
# abline(strt19_depth_mod)

#extract slope of model and develop rating curve
strt.2019.q.dt$RatingCurveDepth <- strt19_depth_mod$coefficients[1]+(strt19_depth_mod$coefficients[2])*strt.2019.q.dt$Q




strt.2019.q.dt$date <- as.Date(strt.2019.q.dt$datetimeAK)

daily.mean.depth.strt <- strt.2019.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveDepth), list(name = mean))



#########




############# Continuous Velocity ############



#download flowmeter data
WR_19.url <- "https://drive.google.com/drive/u/1/folders/1bm62_JO1dKrFPyUCz8E88K5q7IHElXPP"
WR.19.1 <- drive_get(as_id(WR_19.url))
strt.wr19_glist <- drive_ls(WR.19.1, pattern = "STRT_2019_flowmeter_Q_for_R_JAA.csv")
walk(strt.wr19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
strt_WR_19.Data <- read.csv("STRT_2019_flowmeter_Q_for_R_JAA.csv",
                            header = TRUE, na.strings=c("","NA","blank"))

strt_WR_19.Data <- strt_WR_19.Data %>% tidyr::fill(Date, .direction = ("down"))

strt_WR_19.Data <- strt_WR_19.Data %>% tidyr::fill(Time, .direction = ("down"))


strt_WR_19.Data$datetimeAK <- as.POSIXct(paste(strt_WR_19.Data$Date, strt_WR_19.Data$Time), format="%m/%d/%Y %H:%M")





strt_WR_19.Data <- strt_WR_19.Data %>%
  select(Mean.Vel..m.s., datetimeAK)

strt_V_19_WR <- ddply(na.omit(strt_WR_19.Data), .(datetimeAK), summarize, meanVelocity = mean(as.numeric(Mean.Vel..m.s.)))

# strt_wr.lm <- lm(meanDepth~datetimeAK, strt_depth_19)


strt_V_19_WR <- setDT(strt_V_19_WR)

strt_V_19_WR <- strt_V_19_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

# strt.2019.pt <- strt.2019.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# strt.2019.pt <- strt.2019.pt %>%
#   mutate(across(c(AvgAbsV),
#                 ~ifelse(datetimeAK >= "2019-08-26 02:00:00" & datetimeAK <= "2019-09-08 12:15:00", NA, .)))



strt.2019.q.dt <- strt.2019.Q
strt.2019.q.dt <- strt.2019.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(strt_V_19_WR)
setDT(strt.2019.q.dt)

strt.2019.q.dt$datetimeAK1 <- strt.2019.q.dt$datetimeAK

setkey( strt.2019.q.dt, datetimeAK )
setkey( strt_V_19_WR, datetimeAK )

#WR was taken when EXO out of water. round V point to nearest in data record
rounded.dates_strt19 <- strt.2019.q.dt[ strt_V_19_WR, roll = "nearest" ]

rounded.dates_strt19_WR_Q <- rounded.dates_strt19  %>% rename (discharge = Q) %>% select(datetimeAK, discharge, meanVelocity)


strt_pt_wr_graph <- ggplot(rounded.dates_strt19_WR_Q, aes(discharge, meanVelocity)) +
  geom_point()
# Add regression line
strt_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("V (WR)")

ggsave("plots/strt_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


strt19_V_mod <- lm(meanVelocity~discharge, data = rounded.dates_strt19_WR_Q)

summary(strt19_V_mod)

plot(meanVelocity~discharge, data = rounded.dates_strt19_WR_Q)
# abline(strt19_V_mod)

#extract slope of model and develop rating curve
strt.2019.q.dt$RatingCurveV <- strt19_V_mod$coefficients[1]+(strt19_V_mod$coefficients[2])*strt.2019.q.dt$Q




strt.2019.q.dt$date <- as.Date(strt.2019.q.dt$datetimeAK)

daily.mean.velocity.strt <- strt.2019.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveV), list(name = mean))




######### Daily Q ##########


#discharge
strt.2019.Q$date <- as.Date(strt.2019.Q$DateTime)
strt.2019.Q <- strt.2019.Q %>% rename(discharge = Q)


strt.discharge <- na.omit(strt.2019.Q) %>%
  group_by(date) %>%
  summarise_at(vars(discharge), list(name = mean)) %>% rename(discharge = name)



daily.mean.velocity.strt <- daily.mean.velocity.strt %>% rename(velocity = name)
daily.mean.depth.strt <- daily.mean.depth.strt %>% rename(depth = name)



data <- merge(daily.mean.velocity.strt,daily.mean.depth.strt, by = "date")
data <- merge(data, strt.discharge, by = "date")
view(data)



#Equations

data$k600.1 <-  ((data$velocity * strt.slope)^(0.89)) * (data$depth^(0.54)) * 5037

data$k600.3 <-  1162 * (strt.slope^(0.77)) * (data$velocity^(0.85))

data$k600.4 <- ((data$velocity*strt.slope)^(0.76)) * 951.5

data$k600.5 <- (data$velocity*strt.slope) * 2841 + 2.02

data$k600.6 <- 929*((data$velocity*strt.slope)^(0.75)) * data$discharge^(0.011)

data$k600.7 <- 4725*((data$velocity*strt.slope)^(0.86)) * (data$discharge^(-0.14)) *(data$depth^(0.66))

view(data)

metab.stuart <- read.csv(here("outputs","stuart2019-Run_2023-01-09.csv"))

SM.K <- metab.stuart %>% select(date, K600_daily_mean)
SM.K <- SM.K %>% rename(k600.SM = K600_daily_mean)

SM.K$date <- as.Date(SM.K$date)

data2 <- merge(data,SM.K, by = "date")


# ALL YEARS
K.plot1_stuart_2019 <- data2 %>% 
  select(date, starts_with('k600')) %>%
  gather(type, k600.value, starts_with('k600')) %>%  ggplot(aes(x=date, y=k600.value, color=type)) + geom_line() + theme_bw() +
  scale_color_discrete('variable') + labs(title = "K600, stuart 2019. From Raymond Equations ", y = expression(paste("K600 (", d^-1, ")")))
K.plot1_stuart_2019










############### stuart 2020 ############ 


#slope
strt.slope <- 2.13534/100


Q_2020 <- read.csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_Discharge/Predicted_Discharge/2020/Q_2020.csv")
Q_2020$DateTime <-  force_tz(as.POSIXct(ymd_hms(Q_2020$DateTime)), tz = "America/Anchorage")
#### Breaking up into sites ####
strt_Q_2020 <- Q_2020 %>% filter(Site == "STRT")

strt.2020.Q <- strt_Q_2020 %>%
  select(DateTime, Q, Site)









############# Continuous Depth ############



# # strt 2020:

#download flowmeter data


#download flowmeter data
WR_20.url <- "https://drive.google.com/drive/u/1/folders/1x_E4gaPvjRLDcrM8lN2ao4_o0bzdMBrb"
WR.20.1 <- drive_get(as_id(WR_20.url))
strt.wr20_glist <- drive_ls(WR.20.1, pattern = "R_Flowmeter Q calculation_STRT_for_R_JAA.csv")
walk(strt.wr20_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
strt_WR_20.Data <- read.csv("R_Flowmeter Q calculation_STRT_for_R_JAA.csv",
                            header = TRUE, na.strings=c("","NA","blank"))%>% rename(Date = ï..Date)

strt_WR_20.Data <- strt_WR_20.Data %>% tidyr::fill(Date, .direction = ("down"))

strt_WR_20.Data <- strt_WR_20.Data %>% tidyr::fill(Time, .direction = ("down"))


strt_WR_20.Data$datetimeAK <- as.POSIXct(paste(as.character(strt_WR_20.Data$Date), strt_WR_20.Data$Time), format="%y%m%d %H:%M")


strt_WR_20.Data <- strt_WR_20.Data %>%
  select(Depth..cm., datetimeAK)

strt_depth_20_WR <- ddply(na.omit(strt_WR_20.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# strt_wr.lm <- lm(meanDepth~datetimeAK, strt_depth_20)


strt_depth_20_WR <- setDT(strt_depth_20_WR)

strt_depth_20_WR <- strt_depth_20_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

# strt.2020.pt <- strt.2020.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# strt.2020.pt <- strt.2020.pt %>%
#   mutate(across(c(AvgAbsDepth),
#                 ~ifelse(datetimeAK >= "2020-08-26 02:00:00" & datetimeAK <= "2020-09-08 12:15:00", NA, .)))



strt.2020.q.dt <- strt.2020.Q
strt.2020.q.dt <- strt.2020.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(strt_depth_20_WR)
setDT(strt.2020.q.dt)

strt.2020.q.dt$datetimeAK1 <- strt.2020.q.dt$datetimeAK

setkey( strt.2020.q.dt, datetimeAK )
setkey( strt_depth_20_WR, datetimeAK )

#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_strt20 <- strt.2020.q.dt[ strt_depth_20_WR, roll = "nearest" ]

rounded.dates_strt20_WR_Q <- rounded.dates_strt20 %>% rename(discharge = Q) %>% 
  select(datetimeAK, discharge, meanDepth)

#convert to meters
rounded.dates_strt20_WR_Q$meanDepth <- rounded.dates_strt20_WR_Q$meanDepth /100

strt_pt_wr_graph <- ggplot(rounded.dates_strt20_WR_Q, aes(discharge, meanDepth)) +
  geom_point()
# Add regression line
strt_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("Depth (WR)")

ggsave("plots/strt_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


strt20_depth_mod <- lm(meanDepth~discharge, data = rounded.dates_strt20_WR_Q)

summary(strt20_depth_mod)

plot(meanDepth~discharge, data = rounded.dates_strt20_WR_Q)
# abline(strt20_depth_mod)

#extract slope of model and develop rating curve
strt.2020.q.dt$RatingCurveDepth <- strt20_depth_mod$coefficients[1]+(strt20_depth_mod$coefficients[2])*strt.2020.q.dt$Q




strt.2020.q.dt$date <- as.Date(strt.2020.q.dt$datetimeAK)

daily.mean.depth.strt <- strt.2020.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveDepth), list(name = mean))



#########




############# Continuous Velocity ############



# # strt 2020:



#download flowmeter data
WR_20.url <- "https://drive.google.com/drive/u/1/folders/1x_E4gaPvjRLDcrM8lN2ao4_o0bzdMBrb"
WR.20.1 <- drive_get(as_id(WR_20.url))
strt.wr20_glist <- drive_ls(WR.20.1, pattern = "R_Flowmeter Q calculation_STRT_for_R_JAA.csv")
walk(strt.wr20_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
strt_WR_20.Data <- read.csv("R_Flowmeter Q calculation_STRT_for_R_JAA.csv",
                            header = TRUE, na.strings=c("","NA","blank"))%>% rename(Date = ï..Date)

strt_WR_20.Data <- strt_WR_20.Data %>% tidyr::fill(Date, .direction = ("down"))

strt_WR_20.Data <- strt_WR_20.Data %>% tidyr::fill(Time, .direction = ("down"))


strt_WR_20.Data$datetimeAK <- as.POSIXct(paste(as.character(strt_WR_20.Data$Date), strt_WR_20.Data$Time), format="%y%m%d %H:%M")

strt_WR_20.Data <- strt_WR_20.Data %>%
  select(Mean.Vel..m.s., datetimeAK)

strt_V_20_WR <- ddply(na.omit(strt_WR_20.Data), .(datetimeAK), summarize, meanVelocity = mean(as.numeric(Mean.Vel..m.s.)))

# strt_wr.lm <- lm(meanDepth~datetimeAK, strt_depth_20)


strt_V_20_WR <- setDT(strt_V_20_WR)

strt_V_20_WR <- strt_V_20_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

# strt.2020.pt <- strt.2020.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# strt.2020.pt <- strt.2020.pt %>%
#   mutate(across(c(AvgAbsV),
#                 ~ifelse(datetimeAK >= "2020-08-26 02:00:00" & datetimeAK <= "2020-09-08 12:15:00", NA, .)))



strt.2020.q.dt <- strt.2020.Q
strt.2020.q.dt <- strt.2020.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(strt_V_20_WR)
setDT(strt.2020.q.dt)

strt.2020.q.dt$datetimeAK1 <- strt.2020.q.dt$datetimeAK

setkey( strt.2020.q.dt, datetimeAK )
setkey( strt_V_20_WR, datetimeAK )

#WR was taken when EXO out of water. round V point to nearest in data record
rounded.dates_strt20 <- strt.2020.q.dt[ strt_V_20_WR, roll = "nearest" ]

rounded.dates_strt20_WR_Q <- rounded.dates_strt20  %>% rename (discharge = Q) %>% select(datetimeAK, discharge, meanVelocity)


strt_pt_wr_graph <- ggplot(rounded.dates_strt20_WR_Q, aes(discharge, meanVelocity)) +
  geom_point()
# Add regression line
strt_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("V (WR)")

ggsave("plots/strt_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


strt20_V_mod <- lm(meanVelocity~discharge, data = rounded.dates_strt20_WR_Q)

summary(strt20_V_mod)

plot(meanVelocity~discharge, data = rounded.dates_strt20_WR_Q)
# abline(strt20_V_mod)

#extract slope of model and develop rating curve
strt.2020.q.dt$RatingCurveV <- strt20_V_mod$coefficients[1]+(strt20_V_mod$coefficients[2])*strt.2020.q.dt$Q




strt.2020.q.dt$date <- as.Date(strt.2020.q.dt$datetimeAK)

daily.mean.velocity.strt <- strt.2020.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveV), list(name = mean))




######### Daily Q ##########


#discharge
strt.2020.Q$date <- as.Date(strt.2020.Q$DateTime)
strt.2020.Q <- strt.2020.Q %>% rename(discharge = Q)


strt.discharge <- na.omit(strt.2020.Q) %>%
  group_by(date) %>%
  summarise_at(vars(discharge), list(name = mean)) %>% rename(discharge = name)



daily.mean.velocity.strt <- daily.mean.velocity.strt %>% rename(velocity = name)
daily.mean.depth.strt <- daily.mean.depth.strt %>% rename(depth = name)



data <- merge(daily.mean.velocity.strt,daily.mean.depth.strt, by = "date")
data <- merge(data, strt.discharge, by = "date")

#slope
strt.slope <- 2.13534/100

#Equations

data$k600.1 <-  ((data$velocity * strt.slope)^(0.89)) * (data$depth^(0.54)) * 5037

data$k600.3 <-  1162 * (strt.slope^(0.77)) * (data$velocity^(0.85))

data$k600.4 <- ((data$velocity*strt.slope)^(0.76)) * 951.5

data$k600.5 <- (data$velocity*strt.slope) * 2841 + 2.02

data$k600.6 <- 929*((data$velocity*strt.slope)^(0.75)) * data$discharge^(0.011)

data$k600.7 <- 4725*((data$velocity*strt.slope)^(0.86)) * (data$discharge^(-0.14)) *(data$depth^(0.66))


metab.stuart <- read.csv(here("outputs","stuart2020-Run_2023-01-09.csv"))

SM.K <- metab.stuart %>% select(date, K600_daily_mean)
SM.K <- SM.K %>% rename(k600.SM = K600_daily_mean)

SM.K$date <- as.Date(SM.K$date)

data2 <- merge(data,SM.K, by = "date")


# ALL YEARS
K.plot1_stuart_2020 <- data2 %>% 
  select(date, starts_with('k600')) %>%
  gather(type, k600.value, starts_with('k600')) %>%  ggplot(aes(x=date, y=k600.value, color=type)) + geom_line() + theme_bw() +
  scale_color_discrete('variable') + labs(title = "K600, stuart 2020. From Raymond Equations ", y = expression(paste("K600 (", d^-1, ")")))
K.plot1_stuart_2020










###### strt 2021

############### stuart 2021 ############ 


#slope
strt.slope <- 2.13534/100


Q_2021 <- read.csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_Discharge/Predicted_Discharge/2021/Q_2021.csv")
Q_2021$DateTime <-  force_tz(as.POSIXct(ymd_hms(Q_2021$DateTime)), tz = "America/Anchorage")
#### Breaking up into sites ####
strt_Q_2021 <- Q_2021 %>% filter(Site == "STRT")

strt.2021.Q <- strt_Q_2021 %>%
  select(DateTime, Q, Site)









############# Continuous Depth ############



# # strt 2021:

#download flowmeter data




#download flowmeter data
WR_21.url <- "https://drive.google.com/drive/u/1/folders/1LTD4EFX3_Yas0ZCF8rKLl6dxSeZDvkDY"
WR.21.1 <- drive_get(as_id(WR_21.url))
strt.wr21_glist <- drive_ls(WR.21.1, pattern = "R_Flowmeter Q calculation_STRT_for_R_JAA_2021.csv")
walk(strt.wr21_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
strt_WR_21.Data <- read.csv("R_Flowmeter Q calculation_STRT_for_R_JAA_2021.csv",
                            header = TRUE, na.strings=c("","NA","blank"))
strt_WR_21.Data <- strt_WR_21.Data %>% tidyr::fill(Date, .direction = ("down"))

strt_WR_21.Data <- strt_WR_21.Data %>% tidyr::fill(Time, .direction = ("down"))



strt_WR_21.Data$datetimeAK <- as.POSIXct(paste(as.character(strt_WR_21.Data$Date), strt_WR_21.Data$Time), format="%y%m%d %H:%M")



strt_WR_21.Data <- strt_WR_21.Data %>%
  select(Depth..cm., datetimeAK)

strt_depth_21_WR <- ddply(na.omit(strt_WR_21.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# strt_wr.lm <- lm(meanDepth~datetimeAK, strt_depth_21)


strt_depth_21_WR <- setDT(strt_depth_21_WR)

strt_depth_21_WR <- strt_depth_21_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

# strt.2021.pt <- strt.2021.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# strt.2021.pt <- strt.2021.pt %>%
#   mutate(across(c(AvgAbsDepth),
#                 ~ifelse(datetimeAK >= "2021-08-26 02:00:00" & datetimeAK <= "2021-09-08 12:15:00", NA, .)))



strt.2021.q.dt <- strt.2021.Q
strt.2021.q.dt <- strt.2021.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(strt_depth_21_WR)
setDT(strt.2021.q.dt)

strt.2021.q.dt$datetimeAK1 <- strt.2021.q.dt$datetimeAK

setkey( strt.2021.q.dt, datetimeAK )
setkey( strt_depth_21_WR, datetimeAK )

#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_strt21 <- strt.2021.q.dt[ strt_depth_21_WR, roll = "nearest" ]

rounded.dates_strt21_WR_Q <- rounded.dates_strt21 %>% rename(discharge = Q) %>% 
  select(datetimeAK, discharge, meanDepth)

#convert to meters
rounded.dates_strt21_WR_Q$meanDepth <- rounded.dates_strt21_WR_Q$meanDepth /100

strt_pt_wr_graph <- ggplot(rounded.dates_strt21_WR_Q, aes(discharge, meanDepth)) +
  geom_point()
# Add regression line
strt_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("Depth (WR)")

ggsave("plots/strt_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


strt21_depth_mod <- lm(meanDepth~discharge, data = rounded.dates_strt21_WR_Q)

summary(strt21_depth_mod)

plot(meanDepth~discharge, data = rounded.dates_strt21_WR_Q)
# abline(strt21_depth_mod)

#extract slope of model and develop rating curve
strt.2021.q.dt$RatingCurveDepth <- strt21_depth_mod$coefficients[1]+(strt21_depth_mod$coefficients[2])*strt.2021.q.dt$Q




strt.2021.q.dt$date <- as.Date(strt.2021.q.dt$datetimeAK)

daily.mean.depth.strt <- strt.2021.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveDepth), list(name = mean))



#########



############# Continuous Velocity ############



# # strt 2021:




#download flowmeter data
WR_21.url <- "https://drive.google.com/drive/u/1/folders/1LTD4EFX3_Yas0ZCF8rKLl6dxSeZDvkDY"
WR.21.1 <- drive_get(as_id(WR_21.url))
strt.wr21_glist <- drive_ls(WR.21.1, pattern = "R_Flowmeter Q calculation_STRT_for_R_JAA_2021.csv")
walk(strt.wr21_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
strt_WR_21.Data <- read.csv("R_Flowmeter Q calculation_STRT_for_R_JAA_2021.csv",
                            header = TRUE, na.strings=c("","NA","blank"))
strt_WR_21.Data <- strt_WR_21.Data %>% tidyr::fill(Date, .direction = ("down"))

strt_WR_21.Data <- strt_WR_21.Data %>% tidyr::fill(Time, .direction = ("down"))



strt_WR_21.Data$datetimeAK <- as.POSIXct(paste(as.character(strt_WR_21.Data$Date), strt_WR_21.Data$Time), format="%y%m%d %H:%M")

strt_WR_21.Data <- strt_WR_21.Data %>%
  select(Mean.Vel..m.s., datetimeAK)

strt_V_21_WR <- ddply(na.omit(strt_WR_21.Data), .(datetimeAK), summarize, meanVelocity = mean(as.numeric(Mean.Vel..m.s.)))

# strt_wr.lm <- lm(meanDepth~datetimeAK, strt_depth_21)


strt_V_21_WR <- setDT(strt_V_21_WR)

strt_V_21_WR <- strt_V_21_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

# strt.2021.pt <- strt.2021.pt %>%
# dplyr::rename(datetimeAK = DateTime)





# strt.2021.pt <- strt.2021.pt %>%
#   mutate(across(c(AvgAbsV),
#                 ~ifelse(datetimeAK >= "2021-08-26 02:00:00" & datetimeAK <= "2021-09-08 12:15:00", NA, .)))



strt.2021.q.dt <- strt.2021.Q
strt.2021.q.dt <- strt.2021.q.dt %>%
  dplyr::rename(datetimeAK = DateTime)

setDT(strt_V_21_WR)
setDT(strt.2021.q.dt)

strt.2021.q.dt$datetimeAK1 <- strt.2021.q.dt$datetimeAK

setkey( strt.2021.q.dt, datetimeAK )
setkey( strt_V_21_WR, datetimeAK )

#WR was taken when EXO out of water. round V point to nearest in data record
rounded.dates_strt21 <- strt.2021.q.dt[ strt_V_21_WR, roll = "nearest" ]

rounded.dates_strt21_WR_Q <- rounded.dates_strt21  %>% rename (discharge = Q) %>% select(datetimeAK, discharge, meanVelocity)


strt_pt_wr_graph <- ggplot(rounded.dates_strt21_WR_Q, aes(discharge, meanVelocity)) +
  geom_point()
# Add regression line
strt_pt_wr_graph + geom_smooth(method = lm) + xlab("Discharge") +ylab ("V (WR)")

ggsave("plots/strt_Q_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


strt21_V_mod <- lm(meanVelocity~discharge, data = rounded.dates_strt21_WR_Q)

summary(strt21_V_mod)

plot(meanVelocity~discharge, data = rounded.dates_strt21_WR_Q)
# abline(strt21_V_mod)

#extract slope of model and develop rating curve
strt.2021.q.dt$RatingCurveV <- strt21_V_mod$coefficients[1]+(strt21_V_mod$coefficients[2])*strt.2021.q.dt$Q




strt.2021.q.dt$date <- as.Date(strt.2021.q.dt$datetimeAK)

daily.mean.velocity.strt <- strt.2021.q.dt %>%
  group_by(date) %>%
  summarise_at(vars(RatingCurveV), list(name = mean))




######### Daily Q ##########


#discharge
strt.2021.Q$date <- as.Date(strt.2021.Q$DateTime)
strt.2021.Q <- strt.2021.Q %>% rename(discharge = Q)


strt.discharge <- na.omit(strt.2021.Q) %>%
  group_by(date) %>%
  summarise_at(vars(discharge), list(name = mean)) %>% rename(discharge = name)



daily.mean.velocity.strt <- daily.mean.velocity.strt %>% rename(velocity = name)
daily.mean.depth.strt <- daily.mean.depth.strt %>% rename(depth = name)



data <- merge(daily.mean.velocity.strt,daily.mean.depth.strt, by = "date")
data <- merge(data, strt.discharge, by = "date")

#slope
strt.slope <- 2.13534/100

#Equations

data$k600.1 <-  ((data$velocity * strt.slope)^(0.89)) * (data$depth^(0.54)) * 5037

data$k600.3 <-  1162 * (strt.slope^(0.77)) * (data$velocity^(0.85))

data$k600.4 <- ((data$velocity*strt.slope)^(0.76)) * 951.5

data$k600.5 <- (data$velocity*strt.slope) * 2841 + 2.02

data$k600.6 <- 929*((data$velocity*strt.slope)^(0.75)) * data$discharge^(0.011)

data$k600.7 <- 4725*((data$velocity*strt.slope)^(0.86)) * (data$discharge^(-0.14)) *(data$depth^(0.66))


metab.stuart <- read.csv(here("outputs","stuart2021-Run_2023-01-09.csv"))

SM.K <- metab.stuart %>% select(date, K600_daily_mean)
SM.K <- SM.K %>% rename(k600.SM = K600_daily_mean)

SM.K$date <- as.Date(SM.K$date)

data2 <- merge(data,SM.K, by = "date")


# ALL YEARS
K.plot1_stuart_2021 <- data2 %>% 
  select(date, starts_with('k600')) %>%
  gather(type, k600.value, starts_with('k600')) %>%  ggplot(aes(x=date, y=k600.value, color=type)) + geom_line() + theme_bw() +
  scale_color_discrete('variable') + labs(title = "K600, stuart 2021. From Raymond Equations ", y = expression(paste("K600 (", d^-1, ")")))
K.plot1_stuart_2021



grid.arrange(K.plot1_stuart_2019,K.plot1_stuart_2020,K.plot1_stuart_2021,K.plot1_poker_2019,K.plot1_poker_2020,K.plot1_poker_2021,K.plot1_vault_2019,K.plot1_vault_2020,K.plot1_vault_2021,K.plot1_french_2019,K.plot1_french_2020,K.plot1_french_2021,K.plot1_moose_2019,K.plot1_moose_2020,K.plot1_moose_2021, ncol = 3)



















