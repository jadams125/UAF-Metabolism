# install devtools package
# install.packages(c("devtools"))
library(devtools)

# install BASEmetab package
devtools::install_github("dgiling/BASEmetab")

# Remove the package zip after installation
unlink("BASEmetab.zip")

#load library
library(BASEmetab)

library(lubridate)
library(rjags)
library(imputeTS)

#load R2jags to connect to JAGS
library(R2jags)

library(here())
library(tidyverse)
library(naniar)
library(streamMetabolizer)

#MOOS
######### moos
data.moos.mm.all <- read.csv(here("outputs", "clean.moos.2019.full.csv"))

#model light to make it comparable to SM result
data.moos.mm.all$light <- calc_light( as.POSIXct( data.moos.mm.all$solar.time, tz = "UTC"), 64.606673786354, -146.916364004037)


#change Discharge to m3/s
data.moos.mm.all$discharge <- data.moos.mm.all$discharge / 1000

data.moos.mm.all$datetimeAK <- as.POSIXct(data.moos.mm.all$datetimeAK, tz = "America/Anchorage")

data.moos.mm.all$Date <- as.character(as.Date(data.moos.mm.all$datetimeAK))

data.moos.mm.all$Time <- (format(as.POSIXct(data.moos.mm.all$datetimeAK, tz = "America/Anchorage"), "%H:%M:%S"))


data.moos.mm.all <- data.moos.mm.all %>% rename(tempC = temp.water, DO.meas = DO.obs, I = light) %>% mutate(salinity = 0)







####################



library(neonUtilities)
NEON_air <-loadByProduct(dpID = "DP1.00004.001", site=c("BONA"),startdate="2019-05", enddate="2019-11")
NEON_bp <- NEON_air$BP_1min %>% filter(staPresFinalQF != 1)


NEON_bp$DateTimeAK <- as.POSIXct(NEON_bp$startDateTime, format = "%Y-%m-%d %H:%M", tz = "GMT")
attr(NEON_bp$DateTimeAK, "tzone") <- "America/Anchorage"

## Round time to 15 minutes and take average
NEON_bp$DateTimeAK <- lubridate::round_date(NEON_bp$DateTimeAK, "15 minutes") 
NEON_bp <- NEON_bp %>% group_by(DateTimeAK) %>% summarise(bp_sea = mean(corPres, na.rm = TRUE))

# Fill gaps in data
DateTimeFill <- data.frame(DateTimeAK = seq(ymd_hm("	
2019-05-01 00:00", tz = "America/Anchorage"),ymd_hm("2019-11-15 00:00", tz = "America/Anchorage"), by = '15 mins'))
NEON_bp <- full_join(NEON_bp, DateTimeFill)
NEON_bp <- NEON_bp[order(NEON_bp$DateTimeAK),]
NEON_bp$bp_sea <- na_kalman(NEON_bp$bp_sea, maxgap = 10)

NEON_bp %>% #filter(DateTimeAK > "2022-06-01" & DateTimeAK < "2022-07-01") %>% 
  ggplot(aes(DateTimeAK, bp_sea)) + geom_miss_point()






# Eielson atmospheric pressure data will be used to fill gaps in air pressure.

eielson.atmo.2019 <- read.csv(here("PAEI_2019.csv"), skip = 6)

# eielson.atmo.2019 <- read.csv(url(eielson.atmo.2022.url), skip = 6) 
eielson.atmo.2019 <- eielson.atmo.2019[-1,]

names(eielson.atmo.2019) <- c("Site", "DateTimeAK", "sea_level_pressure", "AirPressure")

eielson.atmo.2019$DateTimeAK <- mdy_hm(eielson.atmo.2019$DateTimeAK)

# Correct time zone
# eielson.atmo.2019$DateTimeAK <- eielson.atmo.2019$DateTimeAK - 28800
eielson.atmo.2019$DateTimeAK <- force_tz(eielson.atmo.2019$DateTimeAK, tz = "America/Anchorage")

eielson.atmo.2019$DateTimeAK <- lubridate::round_date(eielson.atmo.2019$DateTimeAK, "15 minutes")

eielson.atmo.2019$AirPressure <- as.numeric(eielson.atmo.2019$AirPressure)
# eielson.atmo.2019$sea_level_pressure <- as.numeric(eielson.atmo.2019$sea_level_pressure)

# Convert to kPa
eielson.atmo.2019$AirPressure <- eielson.atmo.2019$AirPressure*3.38639




############################

NEON_bp %>% ggplot(aes(DateTimeAK, bp_sea)) + geom_point() + geom_point(data = eielson.atmo.2019, aes(DateTimeAK, AirPressure), color = "red") 

# They are similar, so I will fill in the gaps in NEON with Eielson
# Fill in gaps
Air_combo <- left_join(NEON_bp, eielson.atmo.2019)


# Air_combo$AirPressure <- na_kalman(Air_combo$AirPressure, type = "level")

#fill missing rows with calculated MGL
Air_combo <- Air_combo %>% 
  mutate(bp_combo = coalesce(bp_sea,AirPressure))

# Air_combo <- Air_combo %>% rename(bp_combo = bp_sea)

# Air_combo <- Air_combo %>% mutate(bp_combo = ifelse(bp_sea %in% NA, eielson.atmo.2019, AirPressure)) 

Air_combo$bp_combo <- as.numeric(as.character(Air_combo$bp_combo))


Air_combo %>% #filter(DateTimeAK > "2022-06-01" & DateTimeAK < "2022-07-01") %>% 
  ggplot(aes(DateTimeAK, bp_combo)) + geom_miss_point()

Air_combo$bp_combo <- na_kalman(Air_combo$bp_combo, type = "level")


Air_combo %>% #filter(DateTimeAK > "2022-06-01" & DateTimeAK < "2022-07-01") %>% 
  ggplot(aes(DateTimeAK, bp_combo)) + geom_miss_point()




# MOOS
Air_combo$AirPressureCorrectedMOOS <- (Air_combo$bp_combo*7.50062 - (2.5*601/100)) / 7.50062


# write.csv(NEON_bp, "NEON_bp_2022.csv")
Air_combo$DateTimeAK <- as.POSIXct(Air_combo$DateTimeAK, tz = "America/Anchorage")


Air_combo_short <- Air_combo %>% select(DateTimeAK, AirPressureCorrectedMOOS) %>% rename(datetimeAK = DateTimeAK) %>% mutate(datetimeAK = as.POSIXct(datetimeAK, tz = "America/Anchorage"))

data.moos.mm.all <- data.moos.mm.all %>% mutate(datetimeAK = as.POSIXct(datetimeAK, tz = "America/Anchorage"))


moos.bayes <- full_join(data.moos.mm.all,Air_combo_short, by = "datetimeAK")

moos.bayes$Date <- as.Date(as.character(as.POSIXct(moos.bayes$datetimeAK, tz = "America/Anchorage")))


moos.bayes <- moos.bayes %>% select(datetimeAK, Date,Time,tempC, I, DO.meas,AirPressureCorrectedMOOS, salinity) %>% rename(atmo.pressure =AirPressureCorrectedMOOS) %>% mutate(atmo.pressure = atmo.pressure/100)



moos.bayes$DOY <- as.character(yday(as.POSIXct(moos.bayes$Date)))

#highest minimum for all years (0ne over so we can correctly subset)
moos.bayes.2019.stw <-moos.bayes %>% filter(as.character(DOY)>= "167" & as.character(DOY)<="275") 

moos.bayes.2019.stw <- moos.bayes.2019.stw %>% filter(datetimeAK <= "2019-10-02 00:00:00")%>% select(-c(DOY)) %>% select(datetimeAK, Date,Time,I,tempC,DO.meas,atmo.pressure,salinity)

DateTimeFill <- data.frame(DateTimeAK = seq(ymd_hm("	
2019-05-01 00:00", tz = "America/Anchorage"),ymd_hm("2019-11-15 00:00", tz = "America/Anchorage"), by = '15 mins'))

DateTimeFill <- DateTimeFill %>% rename(datetimeAK = DateTimeAK)
DateTimeFill$datetimeAK <- as.POSIXct(DateTimeFill$datetimeAK, tz = "America/Anchorage")

DateTimeFill$DOY <- yday(as.character(DateTimeFill$datetimeAK))

#highest minimum for all years (0ne over so we can correctly subset)
DateTimeFill <-DateTimeFill %>% filter(DOY>= "167" & DOY<="275") 


moos.fill2019 <- full_join(moos.bayes.2019.stw, DateTimeFill, by = "datetimeAK")

moos.fill2019$datetimeAK <- as.POSIXct(moos.fill2019$datetimeAK, tz = "America/Anchorage")

moos.fill2019 <- moos.fill2019 %>% arrange((moos.fill2019$datetimeAK))

moos.fill2019 <-moos.fill2019 %>% filter(DOY>= "168") 



moos.fill2019$Date <- (as.Date (as.character(moos.fill2019$datetimeAK)))

moos.fill2019$Time <- (format(as.POSIXct(moos.fill2019$datetimeAK, tz = "America/Anchorage"), "%H:%M:%S"))


moos.fill2019 <- unique(moos.fill2019)

#gap fill
moos.fill2019 %>% ggplot(aes(x=datetimeAK, y=DO.meas))+geom_miss_point()
moos.fill2019$DO.meas <- na_kalman(moos.fill2019$DO.meas, type = "level")
moos.fill2019 %>% ggplot(aes(x=datetimeAK, y=DO.meas))+geom_miss_point()




moos.fill2019 %>% ggplot(aes(x=datetimeAK, y=I))+geom_miss_point()
moos.fill2019$I <- na_kalman(moos.fill2019$I, type = "level")
moos.fill2019 %>% ggplot(aes(x=datetimeAK, y=I))+geom_miss_point()


moos.fill2019 %>% ggplot(aes(x=datetimeAK, y=tempC))+geom_miss_point()
moos.fill2019$tempC <- na_kalman(moos.fill2019$tempC,  type = "level")
moos.fill2019 %>% ggplot(aes(x=datetimeAK, y=tempC))+geom_miss_point()

moos.fill2019 %>% ggplot(aes(x=datetimeAK, y=atmo.pressure))+geom_miss_point()
moos.fill2019$atmo.pressure <- na_kalman(moos.fill2019$atmo.pressure, type = "level")
moos.fill2019 %>% ggplot(aes(x=datetimeAK, y=atmo.pressure))+geom_miss_point()

moos.fill2019 %>% ggplot(aes(x=datetimeAK, y=salinity))+geom_miss_point()
moos.fill2019$salinity <- 0
moos.fill2019 %>% ggplot(aes(x=datetimeAK, y=salinity))+geom_miss_point()

moos.fill2019 <- moos.fill2019 %>% filter(datetimeAK <= "2019-10-02 00:00:00")
moos.fill2019 %>% ggplot(aes(x=datetimeAK, y=DO.meas))+geom_miss_point()


moos.fill2019 <- moos.fill2019 %>% select(-datetimeAK)

moos.fill2019 <- moos.fill2019 %>% select(-DOY)

write.csv(moos.fill2019, here("base_test_storage","moos.2019.bayes.test.csv"),fileEncoding = "UTF-8", col.names = FALSE, row.names = FALSE)


moos.fill2019.look <- data.table::as.data.table(moos.fill2019) %>% mutate(Date = as.POSIXct(Date))

# moos.fill2019.look[, .N, by=.(year(date), month(date))] 



testtest <- read.csv(here("base_test_storage","moos.2019.bayes.test.csv"))


moos.bayes.link <-  here("base_test_storage")

results.dir <- here("base_test")

interval = as.integer(900)

n.iter = 20000
n.burnin = n.iter * 0.5

bayesmetab(moos.bayes.link, results.dir, interval )


result1 <- read.csv(here("base_test","BASE_results_2023-02-28 150759.csv"))

# convert to same units as SM, multiply by depth


depthhelper <- data.moos.mm.all

depthhelper$Date <- (as.Date (as.character(depthhelper$datetimeAK)))




depthhelper <- depthhelper %>% select(Date, depth, discharge) %>% 
  group_by(Date) %>%
  summarise(across(-c(discharge), mean, na.rm = TRUE)) %>% mutate(Date = as.character(Date))

# result1$Date <- strptime(result1$Date, "%m-%d-%Y")


units_SM_results <- full_join(result1, depthhelper, by = "Date")

units_SM_results$GPP_mean_BASE <- units_SM_results$GPP.mean *units_SM_results$depth

units_SM_results$ER_mean_BASE <- units_SM_results$ER.mean *units_SM_results$depth *-1


SM.moose.2019 <- read.csv(here("outputs", "moose2019-Run_2023-Full.rerun.02.04.csv"))%>% select(date, GPP_mean, ER_mean, K600_daily_mean) %>% rename(Date = date, GPP_mean_SM = GPP_mean, ER_mean_SM = ER_mean, K600_mean_SM = K600_daily_mean)


metab.moos.2019 <- full_join(SM.moose.2019, units_SM_results, by = "Date")




gpp <- ggplot(data=metab.moos.2019, aes(x=as.POSIXct(Date))) + 
  geom_point(aes(y=GPP_mean_SM), color = "chartreuse4") + 
  geom_point(aes(y=GPP_mean_BASE), color = "blue")+ 
  labs(y = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ ggtitle("Poker 2019 - Base (blue) and StreamMetabolizer (green) ")+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 20, face = "bold")) 



er <- ggplot(data = metab.moos.2019, aes(x = as.POSIXct(Date))) +
  geom_point(aes(y = ER_mean_SM), color = "firebrick3") + 
  geom_point(aes(y = ER_mean_BASE), color = "blue") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20)) + ggtitle("Poker 2019 - Base (blue) and StreamMetabolizer (red) ")+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 20, face = "bold")) 


K600 <- ggplot(data = metab.moos.2019, aes(x = as.POSIXct(Date))) +
  geom_point(aes(y = K600_mean_SM), color = "orange") + 
  geom_point(aes(y = K.mean), color = "blue")+ labs(y = expression(paste("K600 ", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20)) + ggtitle("Poker 2019 - Base (blue) and StreamMetabolizer (orange) ")+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 20, face = "bold")) 



library(ggpmisc)


metab.moos.2019 <- metab.moos.2019 %>% filter(Date != ("2019-08-16"))
metab.moos.2019 %>% ggplot(aes(x=GPP_mean_SM, y=GPP_mean_BASE))+geom_point()+stat_poly_line()+stat_poly_eq()
metab.moos.2019 %>% ggplot(aes(x=ER_mean_SM, y=ER_mean_BASE))+geom_point()+stat_poly_line()+stat_poly_eq()

metab.moos.2019 %>% ggplot(aes(x=K600_mean_SM, y=K.mean))+geom_point()+stat_poly_line()+stat_poly_eq()
