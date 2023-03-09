# neon metab prep

library(tidyverse)
library(neonUtilities)
library(streamMetabolizer)
library(lubridate)
library(here)
library(ggpubr)
library(naniar)
library(imputeTS)

NEON_water <-loadByProduct(dpID = "DP1.20288.001", site=c("CARI"),startdate="2019-01", enddate="2022-12",check.size=F, package = "expanded")

NEON_water_data <- NEON_water$waq_instantaneous



#water temp 
NEON_water_temp <-loadByProduct(dpID = "DP1.20053.001", site=c("CARI"),startdate="2019-01", enddate="2022-12",check.size=F)


NEON_water_temp <- NEON_water_temp$TSW_5min

NEON_water_temp$startDateTime <- as.POSIXct(NEON_water_temp$startDateTime, tz = "UTC")



# Chems DP1.20093.001

NEON_water_chems <-loadByProduct(dpID = "DP1.20093.001", site=c("CARI"),startdate="2019-01", enddate="2022-12",check.size=F)

chems_data <- NEON_water_chems$swc_externalLabDataByAnalyte

chems_data$startDate <- as.POSIXct(chems_data$startDate, tz = "UTC")


chems_data_TDP <-chems_data %>%  filter(analyte == "TDP")

chems_data_P_small <- chems_data_TDP %>% select(startDate,analyte,analyteConcentration, belowDetectionQF)

chems_data_P_small$date <- as.Date(chems_data_P_small$startDate)


chems_P_wide_mn <- chems_data_P_small %>% pivot_wider(names_from = analyte, values_fn = {mean},
                                                   values_from = analyteConcentration )



chems_data_DOC <-chems_data %>%  filter(analyte == "DOC")

chems_data_DOC_small <- chems_data_DOC %>% select(startDate,analyte,analyteConcentration, belowDetectionQF)

chems_data_DOC_small$date <- as.Date(chems_data_DOC_small$startDate)


chems_DOC_wide_mn <- chems_data_DOC_small %>% pivot_wider(names_from = analyte, values_fn = {mean},
                                                          values_from = analyteConcentration )
                                                      

#USE SUNA FOR NITRATE 

NEON_water_SUNA <-loadByProduct(dpID = "DP1.20033.001", site=c("CARI"),startdate="2019-01", enddate="2022-12",check.size=F)

NEON_SUNA_DATA <- NEON_water_SUNA$NSW_15_minute

NEON_SUNA_DATA$startDateTime <- as.POSIXct(NEON_SUNA_DATA$startDateTime, tz = "UTC")


# 
# #Guage Hieght / Depth 
# neon_guage_height<-loadByProduct(dpID = "DP1.20267.001", site=c("CARI"),startdate="2019-01", enddate="2022-12",check.size=F)
# 
# 
# neon_guage_height$readme_20267



#neon DO data is suspect - need better data on calibration


#Discharge 
NEON_Q <-loadByProduct(dpID = "DP4.00130.001", site=c("CARI"),startdate="2019-01", enddate="2022-12",check.size=F,package="expanded")


NEON_Q_cont <- NEON_Q$csd_continuousDischarge

NEON_Q_cont$endDate <- as.POSIXct(NEON_Q_cont$endDate, tz = "UTC")

NEON_Q_15 <- NEON_Q_cont %>%
  group_by(endDate = cut(endDate, breaks="15 min")) %>%
  summarize(equivalentStage= mean(equivalentStage), withParaUncQMedian = mean(withParaUncQMedian))


NEON_Q_15$endDate <-  strptime(as.character(NEON_Q_15$endDate),"%Y-%m-%d %H:%M:%S", tz = "UTC")

NEON_Q_15$DateTimeUTC <- as.POSIXct(NEON_Q_15$endDate, tz = "UTC")








NEON_water_data_small <- NEON_water_data %>% select(startDateTime, dissolvedOxygen, seaLevelDissolvedOxygenSat, localDissolvedOxygenSat, turbidity, fDOM, )


NEON_Water_15 <- NEON_water_data_small %>%
  group_by(startDateTime = cut(startDateTime, breaks="15 min")) %>%
  summarize(dissolvedOxygen= mean(dissolvedOxygen), seaLevelDissolvedOxygenSat 
=mean(seaLevelDissolvedOxygenSat), localDissolvedOxygenSat = mean(localDissolvedOxygenSat), turbidity = mean(turbidity), fDOM = mean(fDOM)
)


NEON_Water_15$startDateTime <-  strptime(as.character(NEON_Water_15$startDateTime),"%Y-%m-%d %H:%M:%S", tz = "UTC")
NEON_Water_15$DateTimeUTC <- as.POSIXct(NEON_Water_15$startDateTime, tz = "UTC")


NEON_water_temp_15<- NEON_water_temp %>%
  group_by(startDateTime = cut(startDateTime, breaks="15 min")) %>%
  summarize(surfWaterTempMean= mean(surfWaterTempMean))


NEON_water_temp_15$startDateTime <-  strptime(as.character(NEON_water_temp_15$startDateTime),"%Y-%m-%d %H:%M:%S", tz = "UTC")


NEON_water_temp_15$DateTimeUTC <- as.POSIXct(NEON_water_temp_15$startDateTime, tz = "UTC")




################################# Combine and Plot #################################


NEON_data_all <- left_join(NEON_Water_15,NEON_Q_15, by = "DateTimeUTC" )
NEON_data_all <- full_join(NEON_data_all,NEON_water_temp_15, by = "DateTimeUTC" )

NEON_data_all$datetimeAK <- with_tz(NEON_data_all$DateTimeUTC, tzone = "America/Anchorage")


NEON_data_all <- NEON_data_all %>% select(datetimeAK, dissolvedOxygen, seaLevelDissolvedOxygenSat, localDissolvedOxygenSat, surfWaterTempMean, withParaUncQMedian, equivalentStage ,turbidity, fDOM) 






cari.comb<- NEON_data_all%>% rename(DO.obs =dissolvedOxygen, ODO.Psat = seaLevelDissolvedOxygenSat, temp.water =surfWaterTempMean, discharge = withParaUncQMedian, depth = equivalentStage)


cari.comb$solar.time <- calc_solar_time(cari.comb$datetimeAK,-147.486655)

cari.comb$solar.time <- as.POSIXct(cari.comb$solar.time, tz = "UTC")
# DO SAT CALCS Summary
cari.comb$DO.sat.EXO = cari.comb$DO.obs /(cari.comb$ODO.Psat/100)


cari.comb <- distinct(cari.comb)


cari.comb$light <- calc_light(cari.comb$solar.time, 65.152855, -147.486655)

write.csv(cari.comb, here("outputs", "cari.comb.csv"))



# 



cari.comb$year <- year(cari.comb$solar.time)
cari.comb$DOY <- as.character(as.factor(yday(cari.comb$solar.time)))
# 
# 
# cari.comb.2019 <-cari.comb %>% ungroup()%>% filter(year =="2019") %>% filter(DOY>= "167" & DOY<="274")
# cari.comb.2020 <-cari.comb %>% ungroup()%>% filter(year =="2020") %>% filter(DOY>= "169" & DOY<="276")
# cari.comb.2021 <-cari.comb %>% ungroup()%>% filter(year =="2021") %>% filter(DOY>= "163" & DOY<="270")
# cari.comb.2022 <-cari.comb %>% ungroup()%>% filter(year =="2022") %>% filter(DOY>= "164" & DOY<="271")
# 
# 
# 
# cari.comb <- rbind(cari.comb.2019,cari.comb.2020,cari.comb.2021,cari.comb.2022) 

cari.comb <- cari.comb %>% mutate(month = month(cari.comb$solar.time)) %>% filter(month >= "3")

cari.comb <- cari.comb %>% select(-c(DOY,year))



# ALL YEARS
cari.plot1 <- cari.comb %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat.EXO)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable') + labs(title = "cari ODO, 2019 - 2022")

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge='m^3/sec')
cariPlot2 <- cari.comb %>% 
  select(solar.time, depth, temp.water, light, discharge) %>%
  gather(type, value, depth, temp.water, light, discharge) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light','discharge')),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/UAF-Metabolism/Plots/cari_comb_2019-2022.pdf", height= 8.5)
ggarrange(cari.plot1, cariPlot2, ncol = 1, nrow = 2)
dev.off()


#2019

#
cari.plot1.19 <- cari.comb %>% filter(datetimeAK <= "2020-01-01 00:00:00") %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat.EXO)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable') + labs(title = "cari ODO, 2019")

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge= 'm^3/sec')
cariPlot2.19 <- cari.comb %>% filter(datetimeAK <= "2020-01-01 00:00:00") %>% 
  select(solar.time, depth, temp.water, light, discharge) %>%
  gather(type, value, depth, temp.water, light, discharge) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light','discharge', 'air.pressure.mbar')),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/UAF-Metabolism/Plots/cari_comb.2019.pdf", height= 8.5)
ggarrange(cari.plot1.19, cariPlot2.19, ncol = 1, nrow = 2)
dev.off()

#2020

#
cari.plot1.20 <- cari.comb %>% filter(datetimeAK >= "2020-01-01 00:00:00") %>% filter(datetimeAK <= "2021-01-01 00:00:00") %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat.EXO)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable') + labs(title = "cari ODO, 2020")

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge= 'm^3/sec')
cariPlot2.20 <- cari.comb %>% filter(datetimeAK >= "2020-01-01 00:00:00") %>% filter(datetimeAK <= "2021-01-01 00:00:00") %>%
  select(solar.time, depth, temp.water, light, discharge) %>%
  gather(type, value, depth, temp.water, light, discharge) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light', 'discharge', 'air.pressure.mbar')),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/UAF-Metabolism/Plots/cari_comb.2020.pdf", height= 8.5)
ggarrange(cari.plot1.20, cariPlot2.20, ncol = 1, nrow = 2)
dev.off()

#2021

#
cari.plot1.21 <- cari.comb %>% filter(datetimeAK >= "2021-01-01 00:00:00") %>% filter(datetimeAK <= "2022-01-01 00:00:00") %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat.EXO)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable') + labs(title = "cari ODO, 2021")

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge= 'm^3/sec')
cariPlot2.21 <- cari.comb %>% filter(datetimeAK >= "2021-01-01 00:00:00") %>% filter(datetimeAK <= "2022-01-01 00:00:00") %>% 
  select(solar.time, depth, temp.water, light, discharge) %>%
  gather(type, value, depth, temp.water, light, discharge) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light','discharge', "air.pressure.mbar")),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/UAF-Metabolism/Plots/cari_comb.2021.pdf", height= 8.5)
ggarrange(cari.plot1.21, cariPlot2.21, ncol = 1, nrow = 2)
dev.off()

#2022

#
cari.plot1.22 <- cari.comb %>% filter(datetimeAK >= "2022-01-01 00:00:00") %>% filter(datetimeAK <= "2023-01-01 00:00:00") %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat.EXO)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable') + labs(title = "cari ODO, 2022")

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge= 'm^3/sec')
cariPlot2.22 <- cari.comb %>% filter(datetimeAK >= "2022-01-01 00:00:00") %>% filter(datetimeAK <= "2023-01-01 00:00:00") %>% 
  select(solar.time, depth, temp.water, light, discharge) %>%
  gather(type, value, depth, temp.water, light, discharge) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light','discharge', "air.pressure.mbar")),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/UAF-Metabolism/Plots/cari_comb.2022.pdf", height= 8.5)
ggarrange(cari.plot1.22, cariPlot2.22, ncol = 1, nrow = 2)
dev.off()




######################## GAP FILL ###########################


#Set year of 2019
cari.comb.in <- read.csv(here("outputs", "cari.comb.csv"))


cari.comb.in <- cari.comb.in %>% filter(datetimeAK >= "2019-05-19 15:00:00" & datetimeAK < "2019-09-24 15:45:00")

cari.comb.in$datetimeAK <- as.POSIXct(cari.comb.in$datetimeAK, tz = "America/Anchorage")
cari.comb.in$solar.time <- as.POSIXct(cari.comb.in$solar.time, tz = "UTC")

anyNA(cari.comb.in, recursive = TRUE)
lapply(cari.comb.in, summary)

#Create NAs where there arent any 

ts_2019.cari <- data.frame(datetimeAK = force_tz(as.POSIXct(seq(ymd_hm("2019-05-19 15:00"),ymd_hm("2019-09-24 15:30"), by = '15 mins')), tz = "America/Anchorage"))
cari.2019_comb_ts <- full_join(cari.comb.in, ts_2019.cari, by = "datetimeAK")

cari.2019_comb_ts %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = DO.obs)) + geom_miss_point()
cari.2019_comb_ts %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = temp.water)) + geom_miss_point()
cari.2019_comb_ts %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = DO.sat.EXO)) + geom_miss_point()
cari.2019_comb_ts %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = depth)) + geom_miss_point()
cari.2019_comb_ts %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = light)) + geom_miss_point()
cari.2019_comb_ts %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = discharge)) + geom_miss_point()

cari.comb.in <- cari.2019_comb_ts

cari.comb.in$DO.obs <- na_kalman(cari.comb.in$DO.obs, maxgap = 48, type = "level")
cari.comb.in$DO.sat.EXO <- na_kalman(cari.comb.in$DO.sat.EXO, maxgap = 48, type = "level")
cari.comb.in$temp.water <- na_kalman(cari.comb.in$temp.water, maxgap = 48, type = "level")
cari.comb.in$depth <- na_kalman(cari.comb.in$depth, maxgap = 48)
cari.comb.in$discharge <- na_kalman(cari.comb.in$discharge, maxgap = 48)

cari.comb.in %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = DO.obs)) + geom_miss_point()
cari.comb.in %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = temp.water)) + geom_miss_point()
cari.comb.in %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = DO.sat.EXO)) + geom_miss_point()
cari.comb.in %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = depth)) + geom_miss_point()
cari.comb.in %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = light)) + geom_miss_point()
cari.comb.in %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = discharge)) + geom_miss_point()

# All gap filled correctly 

write.csv(cari.comb.in, here("outputs", "clean.cari.2019.full.csv"))






#Set year of 2020
cari.comb.in <- read.csv(here("outputs", "cari.comb.csv"))


cari.comb.in <- cari.comb.in %>% filter(datetimeAK >= "2020-06-16 18:30:00" & datetimeAK < "2020-09-08 15:45:00")

cari.comb.in$datetimeAK <- as.POSIXct(cari.comb.in$datetimeAK, tz = "America/Anchorage")
cari.comb.in$solar.time <- as.POSIXct(cari.comb.in$solar.time, tz = "UTC")

anyNA(cari.comb.in, recursive = TRUE)
lapply(cari.comb.in, summary)

#Create NAs where there arent any 

ts_2020.cari <- data.frame(datetimeAK = force_tz(as.POSIXct(seq(ymd_hm("2020-06-16 18:30"),ymd_hm("2020-09-08 15:30"), by = '15 mins')), tz = "America/Anchorage"))
cari.2020_comb_ts <- full_join(cari.comb.in, ts_2020.cari, by = "datetimeAK")

cari.2020_comb_ts %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = DO.obs)) + geom_miss_point()
cari.2020_comb_ts %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = temp.water)) + geom_miss_point()
cari.2020_comb_ts %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = DO.sat.EXO)) + geom_miss_point()
cari.2020_comb_ts %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = depth)) + geom_miss_point()
cari.2020_comb_ts %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = light)) + geom_miss_point()
cari.2020_comb_ts %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = discharge)) + geom_miss_point()

cari.comb.in <- cari.2020_comb_ts

cari.comb.in$DO.obs <- na_kalman(cari.comb.in$DO.obs, maxgap = 48, type = "level")
cari.comb.in$DO.sat.EXO <- na_kalman(cari.comb.in$DO.sat.EXO, maxgap = 48, type = "level")
cari.comb.in$temp.water <- na_kalman(cari.comb.in$temp.water, maxgap = 48, type = "level")
cari.comb.in$depth <- na_kalman(cari.comb.in$depth, maxgap = 48, type = "level")
cari.comb.in$discharge <- na_kalman(cari.comb.in$discharge, maxgap = 48, type = "level")

cari.comb.in %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = DO.obs)) + geom_miss_point()
cari.comb.in %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = temp.water)) + geom_miss_point()
cari.comb.in %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = DO.sat.EXO)) + geom_miss_point()
cari.comb.in %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = depth)) + geom_miss_point()
cari.comb.in %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = light)) + geom_miss_point()
cari.comb.in %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = discharge)) + geom_miss_point()

# All gap filled correctly 

write.csv(cari.comb.in, here("outputs", "clean.cari.2020.full.csv"))







#Set year of 2021
cari.comb.in <- read.csv(here("outputs", "cari.comb.csv"))


cari.comb.in <- cari.comb.in %>% filter(datetimeAK >= "2021-05-11 18:00:00" & datetimeAK < "2021-10-05 08:00:00")

cari.comb.in$datetimeAK <- as.POSIXct(cari.comb.in$datetimeAK, tz = "America/Anchorage")
cari.comb.in$solar.time <- as.POSIXct(cari.comb.in$solar.time, tz = "UTC")

anyNA(cari.comb.in, recursive = TRUE)
lapply(cari.comb.in, summary)

#Create NAs where there arent any 

ts_2021.cari <- data.frame(datetimeAK = force_tz(as.POSIXct(seq(ymd_hm("2021-05-11 18:00"),ymd_hm("2021-10-05 07:45"), by = '15 mins')), tz = "America/Anchorage"))
cari.2021_comb_ts <- full_join(cari.comb.in, ts_2021.cari, by = "datetimeAK")

cari.2021_comb_ts %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = DO.obs)) + geom_miss_point()
cari.2021_comb_ts %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = temp.water)) + geom_miss_point()
cari.2021_comb_ts %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = DO.sat.EXO)) + geom_miss_point()
cari.2021_comb_ts %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = depth)) + geom_miss_point()
cari.2021_comb_ts %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = light)) + geom_miss_point()
cari.2021_comb_ts %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = discharge)) + geom_miss_point()

cari.comb.in <- cari.2021_comb_ts

cari.comb.in$DO.obs <- na_kalman(cari.comb.in$DO.obs, maxgap = 48, type = "level")
cari.comb.in$DO.sat.EXO <- na_kalman(cari.comb.in$DO.sat.EXO, maxgap = 48, type = "level")
cari.comb.in$temp.water <- na_kalman(cari.comb.in$temp.water, maxgap = 48, type = "level")
cari.comb.in$depth <- na_kalman(cari.comb.in$depth, maxgap = 48)
cari.comb.in$discharge <- na_kalman(cari.comb.in$discharge, maxgap = 48, type = "level")

cari.comb.in %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = DO.obs)) + geom_miss_point()
cari.comb.in %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = temp.water)) + geom_miss_point()
cari.comb.in %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = DO.sat.EXO)) + geom_miss_point()
cari.comb.in %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = depth)) + geom_miss_point()
cari.comb.in %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = light)) + geom_miss_point()
cari.comb.in %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = discharge)) + geom_miss_point()

# All gap filled correctly 

write.csv(cari.comb.in, here("outputs", "clean.cari.2021.full.csv"))




rm(list=ls())
gc()








######################### RUN THE MODEL ##########################

mm.cari.2019.data <- read.csv(here("outputs", "clean.cari.2019.full.csv"))

mm.cari.2019.data <- mm.cari.2019.data %>% select(solar.time, DO.obs, DO.sat.EXO, temp.water, discharge, depth, light) %>% rename(DO.sat = DO.sat.EXO) %>% mutate(discharge = )









