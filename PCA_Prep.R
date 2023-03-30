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
library(naniar)
library(data.table)
library(grid)
library(ggtext)
library(neonUtilities)

library(here)


# metab_all <- read.csv(here("outputs","DoD_metab_TS.csv"))

metab_all <- read.csv(here("outputs","dod.daily.means.with.cari.csv"))

metab_all <- metab_all %>% arrange(ymd(metab_all$date)) %>% select(-c(burn,PF))

site_factors <- na.omit(unique(read.csv(here("outputs","dod.daily.means.with.cari.csv")) %>% select(site,PF,burn)))

metab_all <- full_join(metab_all,site_factors, by = "site")


metab_all <- metab_all %>% rename(ER_daily_mean = ER_mean)

metab_all$NEP_mean <- metab_all$GPP_daily_mean + metab_all$ER_daily_mean


metab_all.cat <- metab_all %>% select(site, date, PF, burn, GPP_daily_mean, ER_daily_mean, NEP_mean) %>% 
  group_by(PF) %>%
  summarise(across(-c(date,site,burn), mean, na.rm = TRUE)) 



###########

clean.frch.2019 <- read.csv(here("outputs","clean.frch.2019.full.csv")) %>% mutate(site = "FRCH") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge) %>% rename(DO.sat = DO.sat.EXO)

clean.frch.2020 <- read.csv(here("outputs","clean.frch.2020.full.csv")) %>% mutate(site = "FRCH") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge) %>% rename(DO.sat = DO.sat.EXO)

clean.frch.2021 <- read.csv(here("outputs","clean.frch.2021.full.csv")) %>% mutate(site = "FRCH") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge) %>% rename(DO.sat = DO.sat.EXO)

clean.frch.2022 <- read.csv(here("outputs","clean.frch.2022.full.csv")) %>% mutate(site = "FRCH") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge) %>% rename(DO.sat = DO.sat.EXO)



clean.moos.2019 <- read.csv(here("outputs","clean.moos.2019.full.csv")) %>% mutate(site = "MOOS") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge) %>% rename(DO.sat = DO.sat.EXO)

clean.moos.2020 <- read.csv(here("outputs","clean.moos.2020.full.csv")) %>% mutate(site = "MOOS") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge) %>% rename(DO.sat = DO.sat.EXO)

clean.moos.2021 <- read.csv(here("outputs","clean.moos.2021.full.csv")) %>% mutate(site = "MOOS") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge) %>% rename(DO.sat = DO.sat.EXO)

clean.moos.2022 <- read.csv(here("outputs","clean.moos.2022.full.csv")) %>% mutate(site = "MOOS") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge) %>% rename(DO.sat = DO.sat.EXO)



clean.poke.2019 <- read.csv(here("outputs","clean.poke.2019.full.csv")) %>% mutate(site = "POKE") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge) %>% rename(DO.sat = DO.sat.EXO)

clean.poke.2020 <- read.csv(here("outputs","clean.poke.2020.full.csv")) %>% mutate(site = "POKE") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge) %>% rename(DO.sat = DO.sat.EXO)

clean.poke.2021 <- read.csv(here("outputs","clean.poke.2021.full.csv")) %>% mutate(site = "POKE") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge) %>% rename(DO.sat = DO.sat.EXO)

clean.poke.2022 <- read.csv(here("outputs","clean.poke.2022.full.csv")) %>% mutate(site = "POKE") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge) %>% rename(DO.sat = DO.sat.EXO)


clean.vaul.2019 <- read.csv(here("outputs","clean.vaul.2019.full.csv")) %>% mutate(site = "VAUL") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge) %>% rename(DO.sat = DO.sat.EXO)

clean.vaul.2020 <- read.csv(here("outputs","clean.vaul.2020.full.csv")) %>% mutate(site = "VAUL") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge) %>% rename(DO.sat = DO.sat.EXO)

clean.vaul.2021 <- read.csv(here("outputs","clean.vaul.2021.full.csv")) %>% mutate(site = "VAUL") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge) %>% rename(DO.sat = DO.sat.EXO)

clean.vaul.2022 <- read.csv(here("outputs","clean.vaul.2022.full.csv")) %>% mutate(site = "VAUL") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge) %>% rename(DO.sat = DO.sat.EXO)


clean.strt.2019 <- read.csv(here("outputs","clean.strt.2019.full.csv")) %>% mutate(site = "STRT") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge) %>% rename(DO.sat = DO.sat.EXO)

clean.strt.2020 <- read.csv(here("outputs","clean.strt.2020.full.csv")) %>% mutate(site = "STRT") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge) %>% rename(DO.sat = DO.sat.EXO)

clean.strt.2021 <- read.csv(here("outputs","clean.strt.2021.full.csv")) %>% mutate(site = "STRT") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge) %>% rename(DO.sat = DO.sat.EXO)

clean.strt.2022 <- read.csv(here("outputs","clean.strt.2022.full.csv")) %>% mutate(site = "STRT") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time)) %>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge) %>% rename(DO.sat = DO.sat.EXO)

clean.cari.2019 <- read.csv(here("outputs","clean.cari.2019.full.csv")) %>% mutate(site = "CARI") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge) %>% rename(DO.sat = DO.sat.EXO)

clean.cari.2020 <- read.csv(here("outputs","clean.cari.2020.full.csv")) %>% mutate(site = "CARI") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge) %>% rename(DO.sat = DO.sat.EXO)

clean.cari.2021 <- read.csv(here("outputs","clean.cari.2021.full.csv")) %>% mutate(site = "CARI") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge) %>% rename(DO.sat = DO.sat.EXO)


clean.craw.2022 <- read.csv(here("outputs","clean.craw.2022.csv")) %>% mutate(site = "CRAW") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time)) %>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat, temp.water, depth, discharge) %>% mutate(depth = (depth/100)) %>% mutate(discharge = (discharge/1000))


clean.mast.2022 <- read.csv(here("outputs","clean.mast.2022.csv")) %>% mutate(site = "MAST") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time)) %>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat, temp.water, depth, discharge) %>% mutate(depth = (depth/100))%>% mutate(discharge = (discharge/1000))


clean.shov.2022 <- read.csv(here("outputs","clean.shov.2022.csv")) %>% mutate(site = "SHOV") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time)) %>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat, temp.water, depth, discharge) %>% mutate(depth = (depth/100))%>% mutate(discharge = (discharge/1000))



DoD.data.all <- rbind(clean.frch.2019,clean.frch.2020,clean.frch.2021,clean.frch.2022,clean.moos.2019,clean.moos.2020,clean.moos.2021,clean.moos.2022, clean.poke.2019,clean.poke.2020,clean.poke.2021,clean.poke.2022,clean.vaul.2019,clean.vaul.2020,clean.vaul.2021,clean.vaul.2022,clean.strt.2019,clean.strt.2020,clean.strt.2021,clean.strt.2022,clean.craw.2022,clean.mast.2022, clean.shov.2022, clean.cari.2019, clean.cari.2020,clean.cari.2021)


DoD.data.all <- as.data.frame(DoD.data.all) %>% mutate (date = as.character(date), site = as.character(site), year = as.numeric(year))

DoD.data.all$datetimeAK <- as.POSIXct(DoD.data.all$datetimeAK, tz = "America/Anchorage")


DoD.data.all.SD_Q <- DoD.data.all %>% select(site, date, discharge) %>%
  group_by(date,site) %>%
  summarise(across(discharge, sd, na.rm = TRUE)) %>% rename(discharge_SD = discharge)


DoD.data.all <- full_join(DoD.data.all,DoD.data.all.SD_Q, by= c("date","site"))

################### EPSCOR CHEMS ######################

epscor_chems <- read.csv(here("outputs", "all.chems.Epscor.csv")) %>% filter(Site == "CRAW" | Site == "MAST"| Site == "SHOV")

# convert to Nitrate_N to Nitrate uM units which


epscor_chems$Nitrate_uM <-  (epscor_chems$NitrateN_mgL / 14) * 1000



##########################################################

### Add in Other data ###
suna_exo_2019 <- read.csv("C:/Users/Jacob/OneDrive - University of Alaska/GitHub/DoD_2019/processed_sensor_dat/SUNA.EXO.int.corr.lab_2019.csv")

suna_exo_2020 <- read.csv("C:/Users/Jacob/OneDrive - University of Alaska/GitHub/DoD_2020/processed_sensor_dat/SUNA.EXO.int.corr.lab_2020.csv")

suna_exo_2021 <- read.csv("C:/Users/Jacob/OneDrive - University of Alaska/GitHub/DoD_2021/processed_sensor_dat/SUNA.EXO.int.corr.lab_2021.csv")

suna_exo_2022 <- read.csv("C:/Users/Jacob/OneDrive - University of Alaska/GitHub/DoD_2022/processed_sensor_dat/SUNA.EXO.int.corr.lab_2022.csv")



suna_exo_2019.select <- suna_exo_2019 %>% select(min, fDOM.QSU.int, Turbidity.FNU.mn.adj, nitrateuM.adj.mn,Site) %>% mutate(date = as.Date(min))

suna_exo_2020.select <- suna_exo_2020 %>% select(min, fDOM.QSU.int, Turbidity.FNU.mn.adj, nitrateuM.adj.mn,Site) %>% mutate(date = as.Date(min))

suna_exo_2021.select <- suna_exo_2021 %>% select(min, fDOM.QSU.int, Turbidity.FNU.mn.adj, nitrateuM.adj.mn,Site) %>% mutate(date = as.Date(min))

suna_exo_2022.select <- suna_exo_2022 %>% select(min, fDOM.QSU.int, Turbidity.FNU.mn.adj, nitrateuM.adj.mn,Site) %>% mutate(date = as.Date(min))


SunaExoSelect_all <- rbind(suna_exo_2019.select,suna_exo_2020.select,suna_exo_2021.select,suna_exo_2022.select )



Suna.Exo.Select.All.mean <- SunaExoSelect_all  %>%
  group_by(date,Site) %>%
  summarise(across(-min, mean, na.rm = TRUE)) %>% rename(site = Site) %>% mutate(date = as.Date(date))


neon_fdom_no3_turb <- read.csv(here("outputs","dod.daily.means.with.cari.csv"))

neon_fdom_no3_turb <- neon_fdom_no3_turb %>% select(date,site, Turbidity.FNU.mn.adj,fDOM.QSU.int,nitrateuM.adj.mn) %>%  mutate(date = as.Date(date)) 

Suna.Exo.Select.All.mean.with.CARI <-  rbind(Suna.Exo.Select.All.mean,neon_fdom_no3_turb)

Suna.Exo.Select.All.mean.with.CARI <- Suna.Exo.Select.All.mean.with.CARI %>% arrange(ymd(Suna.Exo.Select.All.mean.with.CARI$date))


#######################


chems_data <- read.csv("C:/Users/Jacob/OneDrive - University of Alaska/GitHub/DoD_chems_meta/chems_meta_DoD.csv")

chems_data_short <- chems_data %>% select(Date, Site, NitrateN_mgL, TDP_uM, DOC_mgL) %>% rename(date = Date, site = Site) %>% mutate(date = as.Date(date)) 


#TEMP : Make MOOS and FRCH  2022 TDP the same as 2021
chems_TDP_short_moos_2021 <- chems_data_short %>% filter(year(date) == "2021" & site =="MOOS") %>% select(date, TDP_uM, site)

year(chems_TDP_short_moos_2021$date) <- 2022

chems_TDP_short_moos_2022 <- chems_TDP_short_moos_2021


chems_data_short <- full_join(chems_data_short,chems_TDP_short_moos_2022, by = c("date","site"))


chems_data_short <- chems_data_short %>%
  mutate(TDP_uM.x = coalesce(TDP_uM.x,  TDP_uM.y)) %>%
  rename(TDP_uM = TDP_uM.x) %>%
  select(-c(TDP_uM.y))

chems_TDP_short_frch_2021 <- chems_data_short %>% filter(year(date) == "2021" & site =="FRCH") %>% select(date, TDP_uM, site)

year(chems_TDP_short_frch_2021$date) <- 2022

chems_TDP_short_frch_2022 <- chems_TDP_short_frch_2021


chems_data_short <- full_join(chems_data_short,chems_TDP_short_frch_2022, by = c("date","site"))
chems_data_short <- chems_data_short %>%
  mutate(TDP_uM.x = coalesce(TDP_uM.x,  TDP_uM.y)) %>%
  rename(TDP_uM = TDP_uM.x) %>%
  select(-c(TDP_uM.y))








#### CARI

NEON_water_chems <-loadByProduct(dpID = "DP1.20093.001", site=c("CARI"),startdate="2019-01", enddate="2022-12",check.size=F)

chems_data <- NEON_water_chems$swc_externalLabDataByAnalyte 

chems_data <- chems_data %>% filter(shipmentWarmQF == "0")

chems_data$startDate <- as.POSIXct(chems_data$startDate, tz = "UTC")


chems_data_TDP <-chems_data %>%  filter(analyte == "TDP")

chems_data_P_small <- chems_data_TDP %>% select(startDate,analyte,analyteConcentration, belowDetectionQF)

chems_data_P_small$date <- as.Date(chems_data_P_small$startDate)


chems_P_wide_mn <- chems_data_P_small %>% pivot_wider(names_from = analyte, values_fn = {mean},
                                                      values_from = analyteConcentration )

# convert TDP to uM units

#molar mass phosphorous 30.973762 
chems_P_wide_mn$TDP_uM <-  (chems_P_wide_mn$TDP / 30.973762) * 1000


chems_cari_P <- chems_P_wide_mn %>% select(date,TDP_uM) %>% mutate(site = "CARI")



chems_data_DOC <-chems_data %>%  filter(analyte == "DOC")

chems_data_DOC_small <- chems_data_DOC %>% select(startDate,analyte,analyteConcentration, belowDetectionQF)

chems_data_DOC_small$date <- as.Date(chems_data_DOC_small$startDate)


chems_DOC_wide_mn <- chems_data_DOC_small %>% pivot_wider(names_from = analyte, values_fn = {mean},
                                                          values_from = analyteConcentration ) %>% rename(DOC_mgL = DOC)


chems_cari_DOC <- chems_DOC_wide_mn %>% select(date,DOC_mgL) %>% mutate(site = "CARI")

#USE SUNA FOR NITRATE 



#######################
DoD.data.all.mean <- DoD.data.all %>% select( site, year, date, DO.obs, DO.sat, temp.water, depth, discharge, discharge_SD) %>% 
  group_by(date,site) %>%
  summarise(across(-year, mean, na.rm = TRUE)) %>% mutate(date = as.Date(date))


DoD.data.all.mean.SD_Q <- DoD.data.all.mean %>% select(site, date, discharge) %>% mutate(year = year(date)) %>% group_by(year,site) %>%
  summarise(across(discharge, sd, na.rm = TRUE)) %>% rename(discharge_SD = discharge)

DoD.data.all.mean <- DoD.data.all.mean %>% mutate(year = year(date))

DoD.data.all.mean <- full_join(DoD.data.all.mean,DoD.data.all.mean.SD_Q, by= c("year","site"))

DoD.data.all.mean <- DoD.data.all.mean %>% select(-discharge_SD.x) %>% rename(discharge_SD = discharge_SD.y)

###################

chems_cari <- full_join(chems_cari_P,chems_cari_DOC, by = c("date", "site")) %>% mutate(NitrateN_mgL = NA)


chems_data_short2 <- rbind(chems_data_short, chems_cari)

chems_data_short2 <- chems_data_short2 %>% arrange(ymd(chems_data_short2$date))

chems_data_short <- chems_data_short2


######################

frchPAR <- read.csv(here("outputs","frch.combinded.par.csv")) %>% mutate(site = "FRCH") %>% mutate(date = as.Date(DateTime))
pokePAR <- read.csv(here("outputs","poke.combinded.par.csv"))%>% mutate(site = "POKE") %>% mutate(date = as.Date(DateTime))
moosPAR <- read.csv(here("outputs","moos.combinded.par.csv"))%>% mutate(site = "MOOS") %>% mutate(date = as.Date(DateTime))
vaulPAR <- read.csv(here("outputs","vaul.combinded.par.csv"))%>% mutate(site = "VAUL") %>% mutate(date = as.Date(DateTime))
strtPAR <- read.csv(here("outputs","strt.combinded.par.csv"))%>% mutate(site = "STRT") %>% mutate(date = as.Date(DateTime))


DoD_PAR <- rbind(frchPAR,pokePAR,moosPAR,vaulPAR,strtPAR) %>% rename(light = Calibrated.Value) %>% mutate(date = as.Date(date))



# Cari (NEON) PAR
NEON_PAR <-loadByProduct(dpID = "DP1.00024.001", site=c("CARI"),startdate="2019-01", enddate="2022-12",check.size=F,package="basic")

NEON_PAR_data <- NEON_PAR$PARPAR_1min

NEON_PAR_data$datetimeAK <- with_tz( as.POSIXct(NEON_PAR_data$startDateTime, tz = "UTC"), tz = "America/Anchorage")

NEON_PAR_data_15 <- NEON_PAR_data %>%
  group_by(datetimeAK = cut(datetimeAK, breaks="15 min")) %>%
  summarize(PARMean_15 = mean(PARMean))

NEON_PAR_data_15 <- NEON_PAR_data_15 %>% rename(light = PARMean_15) %>% mutate(site = "CARI")

NEON_PAR_data_15 <- NEON_PAR_data_15 %>%  mutate(datetimeAK = as.POSIXct(datetimeAK, tz = "America/Anchorage")) %>% arrange(ymd_hms(NEON_PAR_data_15$datetimeAK)) %>% filter(datetimeAK<="2022-11-30 23:00:00")


NEON_PAR_data_15$light <- na_kalman(NEON_PAR_data_15$light, maxgap = 48)


NEON_PAR_data_15 %>% ggplot(aes(x=datetimeAK, y=light))+geom_miss_point()

#Fill gaps with POKER par data

NEON_PAR_data_15_cari <- NEON_PAR_data_15%>% rename(light.cari = light) %>% select(-site)


DoD_PAR_poke <- pokePAR %>% filter(site == "POKE") %>% rename(light.poke = Calibrated.Value)  %>% mutate(datetimeAK = as.POSIXct(DateTime, tz = "America/Anchorage"))%>% select(-c(site,DateTime,date))


poke_cari_light <- full_join(NEON_PAR_data_15_cari,DoD_PAR_poke, by = "datetimeAK")


poke_cari_light <-  poke_cari_light %>%  mutate(light.cari = coalesce(light.cari,  light.poke)) 


poke_cari_light %>% ggplot(aes(x=datetimeAK, y=light.cari))+geom_miss_point()

poke_cari_light <- poke_cari_light %>% rename(light = light.cari) %>% select(-light.poke)
poke_cari_light$site <- "CARI"
poke_cari_light$date <- as.Date(poke_cari_light$datetimeAK)

DoD_PAR <-  DoD_PAR %>% mutate(datetimeAK = as.POSIXct(DateTime, tz = "America/Anchorage")) %>% select(-DateTime)

DoD_PAR_cari <- rbind(DoD_PAR,poke_cari_light)


DoD_PAR_meaned <- DoD_PAR_cari %>% group_by(date,site) %>%
  summarise(across(-datetimeAK, mean, na.rm = TRUE))




#EPSCOR PAR:

#### CRAW 2022 #### (Logger file name says 5366 but it is 6366)


PAR.2022.url <- "https://drive.google.com/drive/u/1/folders/1k14Iky5K6UU8OMxjA4yalup7GPVFRws3"
PAR_22.prt1 <- drive_get(as_id(PAR.2022.url))
craw_par_glist <- drive_ls(PAR_22.prt1, pattern = "220613_5366_123_003.CSV")
walk(craw_par_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
craw.par.2022.Data.1 <- read.csv("220613_5366_123_003.CSV",
                               skip = 8, header = FALSE)
craw.par.2022.Data.1 <- craw.par.2022.Data.1 %>%
  dplyr::rename(ScanNumber = V1, Date = V2, Time = V3, RawValue = V4, CalibratedValue = V5)

#Fix Date Time
craw.par.2022.Data.1$DateTime <- paste(craw.par.2022.Data.1$Date, craw.par.2022.Data.1$Time, sep="")

craw.par.2022.Data.1$DateTime <-  dmy_hms(craw.par.2022.Data.1$DateTime)
craw.par.2022.Data.1$DateTime <- force_tz(craw.par.2022.Data.1$DateTime, "America/Anchorage")

#Calibrate logger 6366 to LICOR -using 5min measurements from AZ new
craw.par.2022.Data.1$CalibratedValue <- craw.par.2022.Data.1$CalibratedValue * 0.1588




PAR.2022.url <- "https://drive.google.com/drive/u/1/folders/1k14Iky5K6UU8OMxjA4yalup7GPVFRws3"
PAR_22.prt1 <- drive_get(as_id(PAR.2022.url))
craw_par_glist <- drive_ls(PAR_22.prt1, pattern = "220712_5366_123_004.CSV")
walk(craw_par_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
craw.par.2022.Data.2 <- read.csv("220712_5366_123_004.CSV",
                                 skip = 8, header = FALSE)
craw.par.2022.Data.2 <- craw.par.2022.Data.2 %>%
  dplyr::rename(ScanNumber = V1, Date = V2, Time = V3, RawValue = V4, CalibratedValue = V5)

#Fix Date Time
craw.par.2022.Data.2$DateTime <- paste(craw.par.2022.Data.2$Date, craw.par.2022.Data.2$Time, sep="")

craw.par.2022.Data.2$DateTime <-  dmy_hms(craw.par.2022.Data.2$DateTime)
craw.par.2022.Data.2$DateTime <- force_tz(craw.par.2022.Data.2$DateTime, "America/Anchorage")

#Calibrate logger 6366 to LICOR -using 5min measurements from AZ new
craw.par.2022.Data.2$CalibratedValue <- craw.par.2022.Data.2$CalibratedValue * 0.1588



PAR.2022.url <- "https://drive.google.com/drive/u/1/folders/1k14Iky5K6UU8OMxjA4yalup7GPVFRws3"
PAR_22.prt1 <- drive_get(as_id(PAR.2022.url))
craw_par_glist <- drive_ls(PAR_22.prt1, pattern = "220818_5366_123_004.CSV")
walk(craw_par_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
craw.par.2022.Data.3 <- read.csv("220818_5366_123_004.CSV",
                                 skip = 8, header = FALSE)
craw.par.2022.Data.3 <- craw.par.2022.Data.3 %>%
  dplyr::rename(ScanNumber = V1, Date = V2, Time = V3, RawValue = V4, CalibratedValue = V5)

#Fix Date Time
craw.par.2022.Data.3$DateTime <- paste(craw.par.2022.Data.3$Date, craw.par.2022.Data.3$Time, sep="")

craw.par.2022.Data.3$DateTime <-  dmy_hms(craw.par.2022.Data.3$DateTime)
craw.par.2022.Data.3$DateTime <- force_tz(craw.par.2022.Data.3$DateTime, "America/Anchorage")

#Calibrate logger 6366 to LICOR -using 5min measurements from AZ new
craw.par.2022.Data.3$CalibratedValue <- craw.par.2022.Data.3$CalibratedValue * 0.1588




PAR.2022.url <- "https://drive.google.com/drive/u/1/folders/1k14Iky5K6UU8OMxjA4yalup7GPVFRws3"
PAR_22.prt1 <- drive_get(as_id(PAR.2022.url))
craw_par_glist <- drive_ls(PAR_22.prt1, pattern = "221006_5366_123_004.CSV")
walk(craw_par_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
craw.par.2022.Data.4 <- read.csv("221006_5366_123_004.CSV",
                                 skip = 8, header = FALSE)
craw.par.2022.Data.4 <- craw.par.2022.Data.4 %>%
  dplyr::rename(ScanNumber = V1, Date = V2, Time = V3, RawValue = V4, CalibratedValue = V5)

#Fix Date Time
craw.par.2022.Data.4$DateTime <- paste(craw.par.2022.Data.4$Date, craw.par.2022.Data.4$Time, sep="")

craw.par.2022.Data.4$DateTime <-  dmy_hms(craw.par.2022.Data.4$DateTime)
craw.par.2022.Data.4$DateTime <- force_tz(craw.par.2022.Data.4$DateTime, "America/Anchorage")

#Calibrate logger 6366 to LICOR -using 5min measurements from AZ new
craw.par.2022.Data.4$CalibratedValue <- craw.par.2022.Data.4$CalibratedValue * 0.1588




craw.par.2022.Data <- rbind(craw.par.2022.Data.1,craw.par.2022.Data.2,craw.par.2022.Data.3,craw.par.2022.Data.4) %>% mutate(site = "CRAW")









#### mast 2022 #### 


PAR.2022.url <- "https://drive.google.com/drive/u/1/folders/1pPYgANOsaQbxsDEObbTenSYmJeiRsWld"
PAR_22.prt1 <- drive_get(as_id(PAR.2022.url))
mast_par_glist <- drive_ls(PAR_22.prt1, pattern = "220613_6365_123_003.CSV")
walk(mast_par_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
mast.par.2022.Data.1 <- read.csv("220613_6365_123_003.CSV",
                                 skip = 8, header = FALSE)
mast.par.2022.Data.1 <- mast.par.2022.Data.1 %>%
  dplyr::rename(ScanNumber = V1, Date = V2, Time = V3, RawValue = V4, CalibratedValue = V5)

#Fix Date Time
mast.par.2022.Data.1$DateTime <- paste(mast.par.2022.Data.1$Date, mast.par.2022.Data.1$Time, sep="")

mast.par.2022.Data.1$DateTime <-  dmy_hms(mast.par.2022.Data.1$DateTime)
mast.par.2022.Data.1$DateTime <- force_tz(mast.par.2022.Data.1$DateTime, "America/Anchorage")

#Calibrate logger 6365 to LICOR -using 5min measurements from AZ new
mast.par.2022.Data.1$CalibratedValue <- mast.par.2022.Data.1$CalibratedValue * 0.1512




PAR.2022.url <- "https://drive.google.com/drive/u/1/folders/1pPYgANOsaQbxsDEObbTenSYmJeiRsWld"
PAR_22.prt1 <- drive_get(as_id(PAR.2022.url))
mast_par_glist <- drive_ls(PAR_22.prt1, pattern = "220712_6365_123_004.CSV")
walk(mast_par_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
mast.par.2022.Data.2 <- read.csv("220712_6365_123_004.CSV",
                                 skip = 8, header = FALSE)
mast.par.2022.Data.2 <- mast.par.2022.Data.2 %>%
  dplyr::rename(ScanNumber = V1, Date = V2, Time = V3, RawValue = V4, CalibratedValue = V5)

#Fix Date Time
mast.par.2022.Data.2$DateTime <- paste(mast.par.2022.Data.2$Date, mast.par.2022.Data.2$Time, sep="")

mast.par.2022.Data.2$DateTime <-  dmy_hms(mast.par.2022.Data.2$DateTime)
mast.par.2022.Data.2$DateTime <- force_tz(mast.par.2022.Data.2$DateTime, "America/Anchorage")

#Calibrate logger 6365 to LICOR -using 5min measurements from AZ new
mast.par.2022.Data.2$CalibratedValue <- mast.par.2022.Data.2$CalibratedValue * 0.1512



PAR.2022.url <- "https://drive.google.com/drive/u/1/folders/1pPYgANOsaQbxsDEObbTenSYmJeiRsWld"
PAR_22.prt1 <- drive_get(as_id(PAR.2022.url))
mast_par_glist <- drive_ls(PAR_22.prt1, pattern = "220818_6365_123_004.CSV")
walk(mast_par_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
mast.par.2022.Data.3 <- read.csv("220818_6365_123_004.CSV",
                                 skip = 8, header = FALSE)
mast.par.2022.Data.3 <- mast.par.2022.Data.3 %>%
  dplyr::rename(ScanNumber = V1, Date = V2, Time = V3, RawValue = V4, CalibratedValue = V5)

#Fix Date Time
mast.par.2022.Data.3$DateTime <- paste(mast.par.2022.Data.3$Date, mast.par.2022.Data.3$Time, sep="")

mast.par.2022.Data.3$DateTime <-  dmy_hms(mast.par.2022.Data.3$DateTime)
mast.par.2022.Data.3$DateTime <- force_tz(mast.par.2022.Data.3$DateTime, "America/Anchorage")

#Calibrate logger 6365 to LICOR -using 5min measurements from AZ new
mast.par.2022.Data.3$CalibratedValue <- mast.par.2022.Data.3$CalibratedValue * 0.1512




PAR.2022.url <- "https://drive.google.com/drive/u/1/folders/1pPYgANOsaQbxsDEObbTenSYmJeiRsWld"
PAR_22.prt1 <- drive_get(as_id(PAR.2022.url))
mast_par_glist <- drive_ls(PAR_22.prt1, pattern = "221006_6365_123_004.CSV")
walk(mast_par_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
mast.par.2022.Data.4 <- read.csv("221006_6365_123_004.CSV",
                                 skip = 8, header = FALSE)
mast.par.2022.Data.4 <- mast.par.2022.Data.4 %>%
  dplyr::rename(ScanNumber = V1, Date = V2, Time = V3, RawValue = V4, CalibratedValue = V5)

#Fix Date Time
mast.par.2022.Data.4$DateTime <- paste(mast.par.2022.Data.4$Date, mast.par.2022.Data.4$Time, sep="")

mast.par.2022.Data.4$DateTime <-  dmy_hms(mast.par.2022.Data.4$DateTime)
mast.par.2022.Data.4$DateTime <- force_tz(mast.par.2022.Data.4$DateTime, "America/Anchorage")

#Calibrate logger 6365 to LICOR -using 5min measurements from AZ new
mast.par.2022.Data.4$CalibratedValue <- mast.par.2022.Data.4$CalibratedValue * 0.1512




mast.par.2022.Data <- rbind(mast.par.2022.Data.1,mast.par.2022.Data.2,mast.par.2022.Data.3,mast.par.2022.Data.4)%>% mutate(site = "MAST")






#### shov 2022 #### 



PAR.2022.url <- "https://drive.google.com/drive/u/1/folders/16dNWKCifboRLimq_n6pVf_W8UfeYpWlu"
PAR_22.prt1 <- drive_get(as_id(PAR.2022.url))
shov_par_glist <- drive_ls(PAR_22.prt1, pattern = "220715_shov_7922_123_003.CSV")
walk(shov_par_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
shov.par.2022.Data.2 <- read.csv("220715_shov_7922_123_003.CSV",
                                 skip = 8, header = FALSE)
shov.par.2022.Data.2 <- shov.par.2022.Data.2 %>%
  dplyr::rename(ScanNumber = V1, Date = V2, Time = V3, RawValue = V4, CalibratedValue = V5)

#Fix Date Time
shov.par.2022.Data.2$DateTime <- paste(shov.par.2022.Data.2$Date, shov.par.2022.Data.2$Time, sep="")

shov.par.2022.Data.2$DateTime <-  dmy_hms(shov.par.2022.Data.2$DateTime)
shov.par.2022.Data.2$DateTime <- force_tz(shov.par.2022.Data.2$DateTime, "America/Anchorage")

#Calibrate logger 7922 to LICOR -using 5min measurements from AZ new
shov.par.2022.Data.2$CalibratedValue <- shov.par.2022.Data.2$CalibratedValue * 0.1024



PAR.2022.url <- "https://drive.google.com/drive/u/1/folders/16dNWKCifboRLimq_n6pVf_W8UfeYpWlu"
PAR_22.prt1 <- drive_get(as_id(PAR.2022.url))
shov_par_glist <- drive_ls(PAR_22.prt1, pattern = "220808_shov_7922_123_003.CSV")
walk(shov_par_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
shov.par.2022.Data.3 <- read.csv("220808_shov_7922_123_003.CSV",
                                 skip = 8, header = FALSE)
shov.par.2022.Data.3 <- shov.par.2022.Data.3 %>%
  dplyr::rename(ScanNumber = V1, Date = V2, Time = V3, RawValue = V4, CalibratedValue = V5)

#Fix Date Time
shov.par.2022.Data.3$DateTime <- paste(shov.par.2022.Data.3$Date, shov.par.2022.Data.3$Time, sep="")

shov.par.2022.Data.3$DateTime <-  dmy_hms(shov.par.2022.Data.3$DateTime)
shov.par.2022.Data.3$DateTime <- force_tz(shov.par.2022.Data.3$DateTime, "America/Anchorage")

#Calibrate logger 7922 to LICOR -using 5min measurements from AZ new
shov.par.2022.Data.3$CalibratedValue <- shov.par.2022.Data.3$CalibratedValue * 0.1024




PAR.2022.url <- "https://drive.google.com/drive/u/1/folders/16dNWKCifboRLimq_n6pVf_W8UfeYpWlu"
PAR_22.prt1 <- drive_get(as_id(PAR.2022.url))
shov_par_glist <- drive_ls(PAR_22.prt1, pattern = "221012_shov_7922_123_003.CSV")
walk(shov_par_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
shov.par.2022.Data.4 <- read.csv("221012_shov_7922_123_003.CSV",
                                 skip = 8, header = FALSE)
shov.par.2022.Data.4 <- shov.par.2022.Data.4 %>%
  dplyr::rename(ScanNumber = V1, Date = V2, Time = V3, RawValue = V4, CalibratedValue = V5)

#Fix Date Time
shov.par.2022.Data.4$DateTime <- paste(shov.par.2022.Data.4$Date, shov.par.2022.Data.4$Time, sep="")

shov.par.2022.Data.4$DateTime <-  dmy_hms(shov.par.2022.Data.4$DateTime)
shov.par.2022.Data.4$DateTime <- force_tz(shov.par.2022.Data.4$DateTime, "America/Anchorage")

#Calibrate logger 7922 to LICOR -using 5min measurements from AZ new
shov.par.2022.Data.4$CalibratedValue <- shov.par.2022.Data.4$CalibratedValue * 0.1024




shov.par.2022.Data <- rbind(shov.par.2022.Data.2,shov.par.2022.Data.3,shov.par.2022.Data.4) %>% mutate(site = "SHOV")




Epscor_PAR <- rbind(craw.par.2022.Data,mast.par.2022.Data,shov.par.2022.Data)

########################

### Mean Epscor Light

Epscor_PAR.mean <- Epscor_PAR %>% mutate(date = as.character(as.Date(DateTime))) %>% select( site, CalibratedValue, DateTime, date) %>% 
  group_by(date,site) %>%
  summarise(across(-DateTime, mean, na.rm = TRUE))


## MERGE EPSCOR DATA ##

epscor_chems <- epscor_chems %>% rename(date = Date)
epscor_chems$date = as.character(epscor_chems$date)

epscor_chems.mean <- epscor_chems  %>% 
  group_by(date,Site) %>%
  summarise(across(-X, mean, na.rm = TRUE))

epscor_chems.mean$site <- epscor_chems.mean$Site

Epscor_Covariates <- full_join(Epscor_PAR.mean, epscor_chems.mean, by = c("date", "site"))


Epscor_Covariates <- Epscor_Covariates %>% rename(nitrateuM.adj.mn = Nitrate_uM)

Epscor_Covariates <- Epscor_Covariates %>% rename(light = CalibratedValue)

Epscor_Covariates <- Epscor_Covariates %>% mutate(date = as.Date(date))

metab_all.epscor <- read.csv(here("outputs","DoD_metab_TS.csv")) %>% filter(site == 'MAST'| site =="CRAW" | site=="SHOV") 
metab_all.epscor <- na.omit(metab_all.epscor %>% select(date, site, year, GPP_daily_mean, ER_daily_mean, K600_daily_mean) %>% mutate(date = as.Date(date)))


Epscor_all <- full_join(Epscor_Covariates,metab_all.epscor, by = c("site","date"))


Epscor_all <- Epscor_all %>% mutate(year = as.character(year))

####################################################################################


# Precipitaion data 


precip <- read.csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_Precip/data_summaries/ppt_ts_sums.csv")

precip$datetimeAK <- force_tz(as.POSIXct(precip$datetimeAK), tz = "America/Anchorage")

precip$year <- year(precip$datetimeAK)
precip$DOY <- yday(precip$datetimeAK)

precip %>% ggplot(aes(x=DOY, y= ppt_day_mm, color = Site))+geom_point()+facet_wrap(~year, ncol = 2)


precip

######################
metab_all <- metab_all 

q_sd_table <- DoD.data.all.mean %>% select(date, site, discharge_SD)%>% mutate(date = as.Date(date))

DoD.data.all.mean

metab_all <- metab_all %>% mutate(date = as.Date(date))

metab_all <- inner_join(metab_all,q_sd_table, by = c("date", "site"))


DoD.data.all.mean.epscor <- DoD.data.all.mean %>% filter(site == "CRAW" |site == "SHOV" |site == "MAST" ) %>% select(-discharge_SD)

metab_all.2 <- full_join(metab_all, DoD.data.all.mean.epscor, by = c("site","date"))

metab_all.3 <- metab_all.2 %>%
  mutate(temp.water.x = coalesce(temp.water.x,  temp.water.y)) %>%
  mutate(DO.obs.x = coalesce(DO.obs.x,DO.obs.y)) %>%
  mutate(DO.sat.x = coalesce(DO.sat.x,DO.sat.y)) %>%
  rename(temp.water = temp.water.x) %>%
  rename(DO.obs = DO.obs.x) %>%
  rename(DO.sat = DO.sat.x) %>%
  mutate(depth.x = coalesce(depth.x,  depth.y)) %>%
  mutate(discharge.x = coalesce(discharge.x,discharge.y)) %>%
  rename(depth = depth.x) %>%
  rename(discharge = discharge.x) %>%
  select(-c(temp.water.y, DO.obs.y, DO.sat.y,depth.y,discharge.y))


DoD_ALL <- metab_all.3

# DoD_ALL <- metab_all %>% select(-site.y) %>% rename(site = site.x)


DoD_ALL <- full_join(DoD_ALL, chems_data_short, by = c("date", "site"))

DoD_ALL <- full_join(DoD_ALL, DoD_PAR_meaned, by = c("date", "site"))


# metab_all.short <- metab_all %>% mutate(date = as.Date(date)) %>% select(date, site,GPP_daily_mean, ER_daily_mean, K600_daily_mean)

# metab_all <- metab_all %>% mutate(date = as.Date(date))

# DoD_ALL_metab <- full_join(DoD_ALL, metab_all, by = c("date", "site"))


# DoD_ALL_metab <- DoD_ALL_metab %>% 
#   mutate(Turbidity.FNU.mn.adj.x = coalesce(Turbidity.FNU.mn.adj.x,  Turbidity.FNU.mn.adj.y)) %>% 
#   mutate(nitrateuM.adj.mn.x = coalesce(nitrateuM.adj.mn.x,nitrateuM.adj.mn.y)) %>% 
#   mutate(fDOM.QSU.int.x = coalesce(fDOM.QSU.int.x,fDOM.QSU.int.y)) %>% 
#   rename(Turbidity.FNU.mn.adj = Turbidity.FNU.mn.adj.x) %>% 
#   rename(nitrateuM.adj.mn = nitrateuM.adj.mn.x) %>% 
#   rename(fDOM.QSU.int = fDOM.QSU.int.x) %>% 
#   select(-c(Turbidity.FNU.mn.adj.y, nitrateuM.adj.mn.y, fDOM.QSU.int.y))




all.project <- rbind.fill(DoD_ALL, Epscor_all) %>% ungroup

all.project$DOY <- yday(all.project$date)
all.project$year <- year(all.project$date)

#highest minimum for all years 
metab_all.2019 <-all.project %>% filter(year =="2019") %>% filter(DOY>= "167" & DOY<="274")
metab_all.2020 <-all.project %>% filter(year =="2020") %>% filter(DOY>= "169" & DOY<="276")
metab_all.2021 <-all.project %>% filter(year =="2021") %>% filter(DOY>= "163" & DOY<="270")
metab_all.2022 <-all.project %>% filter(year =="2022") %>% filter(DOY>= "164" & DOY<="271")

all.project_stw <- rbind(metab_all.2019, metab_all.2020, metab_all.2021, metab_all.2022) %>% mutate(month = month(date)) %>% filter(month >"3") %>% select(-c(PF,burn))



site_factors <- na.omit(unique(read.csv(here("outputs","dod.daily.means.with.cari.csv")) %>% select(site,PF,burn)))

all.project_stw <- full_join(all.project_stw,site_factors, by = "site")


all.project <- all.project_stw

all.project <- all.project  %>% 
  group_by(date,site) %>%
  summarise(across(-c("year","Site"), mean, na.rm = TRUE)) %>% filter(site == "POKE"| site == "VAUL"| site == "MOOS"| site == "FRCH"| site == "STRT"| site == "MAST"| site == "CRAW"| site == "SHOV"|site == "CARI")

all.project <- all.project %>% select(-c(PF,burn))
site_factors <- na.omit(unique(read.csv(here("outputs","dod.daily.means.with.cari.csv")) %>% select(site,PF,burn)))
all.project <- full_join(all.project,site_factors, by = "site")



all.project <- all.project %>% mutate_all(~ifelse(is.nan(.), NA, .))



all.project.SD_Q <- all.project %>% select(site, date, discharge) %>% mutate(year = year(date)) %>% group_by(year,site) %>%
  summarise(across(discharge, sd, na.rm = TRUE)) %>% rename(discharge_SD = discharge)

all.project <- all.project %>% mutate(year = year(date))

all.project <- full_join(all.project,all.project.SD_Q, by= c("year","site"))

all.project <- all.project %>% select(-discharge_SD.x) %>% rename(discharge_SD = discharge_SD.y)

write.csv(all.project, here("outputs",'daily.metab.chems.data.csv'))

all.project <- read.csv(here("outputs",'daily.metab.chems.data.csv'))



library(here)
library(tidyverse)
library(devtools)
# install_github("vqv/ggbiplot")
library(ggbiplot)
library(FactoMineR)
library(factoextra)
library(corrplot)

#SD of Q






# testPCA <- prcomp(~depth + discharge_SD + fDOM.QSU.int + Turbidity.FNU.mn.adj+ nitrateuM.adj.mn+light, data = all.project ,center = TRUE, scale = TRUE)
# 
# 
# #  TDP_uM + DOC_mgL+
# 
# 
# summary(testPCA)  #print proportion variance explained by each axis
# loadings(testPCA)  #print factor loadings
# 
# plot(testPCA)  # “scree” plot (convenient way of visualizing variance explained by components)
# 
# pairs(~ depth + discharge_SD + fDOM.QSU.int + Turbidity.FNU.mn.adj+ nitrateuM.adj.mn + light,data = all.project,center = TRUE, scale = TRUE)
# 
# 
# # 
# 
# testscores <- testPCA$scores 
# 
# 
# # plot(testscores[,1], testscores[,2], col=DoD_ALL_metab$date, xlab="PC1", ylab="PC2")



all.project.prep <- all.project %>% select(date, site, GPP_daily_mean, ER_daily_mean, K600_daily_mean,TDP_uM,DOC_mgL ,depth , discharge_SD , fDOM.QSU.int , Turbidity.FNU.mn.adj,nitrateuM.adj.mn,light, temp.water)


catag <- all.project %>% mutate(year= year(date)) %>% select(date,site,burn,PF,year) %>% mutate(date = as.Date(date))

all.project.PCA.MEM <- full_join(all.project.prep ,catag, by = c("date","site"))

all.project.PCA.MEM <- all.project.PCA.MEM %>% select(site, date, year, PF, burn, GPP_daily_mean, ER_daily_mean, K600_daily_mean, DOC_mgL, nitrateuM.adj.mn, depth, discharge_SD, TDP_uM, light,temp.water)


all.project.PCA.MEM$year <- ((as.character(all.project.PCA.MEM$year)))

all.project.PCA.MEM.year <- all.project.PCA.MEM %>% 
  group_by(year,site) %>%
  summarise(across(-c(date,PF,burn), mean, na.rm = TRUE))


all.project.PCA.MEM.year <-  na.omit (all.project.PCA.MEM.year %>% mutate_all(~ifelse(is.nan(.), NA, .)))


# 
# testPCA <- prcomp(~depth + discharge_SD + DOC_mgL +TDP_uM + nitrateuM.adj.mn+light, data = all.project.PCA.MEM.year ,center = TRUE, scale = TRUE)




testPCA <- prcomp(~depth + discharge_SD + DOC_mgL +TDP_uM + nitrateuM.adj.mn+light +temp.water, data = all.project.PCA.MEM.year ,center = TRUE, scale = TRUE)

plot(testPCA)


varPCA <- get_pca_var(testPCA)
contrib <- varPCA$contrib
corrplot(contrib, is.corr = FALSE)

biplot <- ggbiplot(testPCA, choices = 1:2)+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20),axis.title.x = element_text(size = 20) )+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+ylim(-2.25,2.25)+xlim(-2.25, 2.25)

biplot

testPCA_scores2 <- testPCA$x[,1:2]



all.project.PCA.MEM.year.B <- all.project.PCA.MEM.year %>% select(c(year, site, GPP_daily_mean, ER_daily_mean, K600_daily_mean ,depth , discharge_SD , DOC_mgL ,nitrateuM.adj.mn,TDP_uM,light,temp.water))

all.project.PCA.MEM.year.PCA <- all.project.PCA.MEM.year.B[complete.cases(all.project.PCA.MEM.year.B),]

all.project.PCA <- cbind(all.project.PCA.MEM.year.PCA, testPCA_scores2)

catag$year <- as.character(catag$year)

catag <- catag %>% ungroup() %>% select(-date)

all.project.PCA.MEM.complete <- full_join(all.project.PCA ,catag, by = c("year","site"))

all.project.PCA.MEM.complete <- na.omit(distinct(all.project.PCA.MEM.complete))

############################################


metab_all.short



library(lme4)
library(lmerTest)



# PC model
basic_mixed_model <- lmer(data = all.project.PCA.MEM.complete, (GPP_daily_mean) ~PC1+  PC2 +(1|year), REML = TRUE)


plot(basic_mixed_model)

qqnorm(resid(basic_mixed_model))
qqline(resid(basic_mixed_model))
# Normal Q-Q plot does NOT look good


ggqqplot(residuals(basic_mixed_model))+ggtitle("GPP PC1 PC2 Model")



summary(basic_mixed_model)



all.project.PCA.MEM.complete %>%  ggplot(aes(x=site, y=GPP_daily_mean)) + facet_wrap(~year, nrow = 2, dir = "v") +  geom_boxplot(aes(fill=as.factor(PF)))+
  facet_grid(. ~ burn , space="free_x", scales="free_x", switch="x") +
  theme_classic() +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill=NA, colour="grey50"),
        panel.spacing.x=unit(0,"cm"))+theme(plot.title = element_text(hjust = 1))+scale_fill_manual(values = c("#71A6D2", "#A13D2D"))+theme(axis.title.x=element_blank() )+scale_fill_manual(values = c("#71A6D2", "#A13D2D"), labels=c('High Permafrost', 'Low Permafrost'))+  theme(legend.title=element_blank())+theme(axis.title.x=element_blank() )+  theme(legend.title=element_blank())+theme(legend.position = c(0.6, 0.8))+labs(y = expression(paste("Mean Yearly GPP (g ", O[2] ," ", m^-2, d^-1, ")")))







basic_mixed_model_ER <- lmer(data = all.project.PCA.MEM.complete, (ER_daily_mean) ~ PC1 + PC2 +(1|year), REML = TRUE)


plot(basic_mixed_model_ER)

qqnorm(resid(basic_mixed_model_ER))
qqline(resid(basic_mixed_model_ER))
# Normal Q-Q plot does NOT look good


ggqqplot(residuals(basic_mixed_model_ER))+ggtitle("ER Mixed Effects Model")



summary(basic_mixed_model_ER)



all.project.PCA.MEM.complete %>%  ggplot(aes(x=site, y=ER_daily_mean)) + facet_wrap(~year, nrow = 2, dir = "v") +  geom_boxplot(aes(fill=as.factor(PF)))+
  facet_grid(. ~ burn , space="free_x", scales="free_x", switch="x") +
  theme_classic() +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill=NA, colour="grey50"),
        panel.spacing.x=unit(0,"cm"))+theme(plot.title = element_text(hjust = 1))+scale_fill_manual(values = c("#71A6D2", "#A13D2D"))+theme(axis.title.x=element_blank() )+scale_fill_manual(values = c("#71A6D2", "#A13D2D"), labels=c('High Permafrost', 'Low Permafrost'))+  theme(legend.title=element_blank())+theme(axis.title.x=element_blank() )+  theme(legend.title=element_blank())+theme(legend.position = c(0.2, 0.2))+labs(y = expression(paste("Mean Yearly ER (g ", O[2] ," ", m^-2, d^-1, ")")))












# burn + PF model
basic_mixed_model <- lmer(data = all.project.PCA.MEM.complete, (ER_daily_mean) ~ burn + PF +(1|year), REML = TRUE)
summary(basic_mixed_model)


plot(basic_mixed_model)

qqnorm(resid(basic_mixed_model))
qqline(resid(basic_mixed_model))
# Normal Q-Q plot does NOT look good


ggqqplot(residuals(basic_mixed_model))+ggtitle("GPP PC1 PC2 Model")






GPPPC1 <- all.project.PCA.MEM.complete %>% ggplot(aes(x=GPP_daily_mean, y=PC1,color = year)) + ggtitle("GPP & PC1")+geom_point()
GPPPC2 <- all.project.PCA.MEM.complete %>% ggplot(aes(x=GPP_daily_mean, y=PC2,color = year)) + ggtitle("GPP & PC2")+geom_point()
ERPC1 <- all.project.PCA.MEM.complete %>% ggplot(aes(x=ER_daily_mean, y=PC1,color = year)) + ggtitle("ER & PC1")+geom_point()
ERPC2 <- all.project.PCA.MEM.complete %>% ggplot(aes(x=ER_daily_mean, y=PC2, color = year)) + ggtitle("ER & PC2")+geom_point()


ggarrange(GPPPC1,GPPPC2, ERPC1, ERPC2)




all.project.prep

all.project.prep %>% mutate(year = year(date), DOY = yday(date)) %>%  ggplot(aes(x=DOY, y=GPP_daily_mean, color = site)) + ggtitle("Daily mean GPP") + geom_point()+ facet_grid(year~.)



DoD.data.all.mean %>% mutate(year = year(date), DOY = yday(date)) %>%  ggplot(aes(x=DOY, y=discharge_SD, color = site)) + ggtitle("Daily mean GPP") + geom_point()+ facet_grid(year~.)



DoD.data.all.mean %>% mutate(year = year(date), DOY = yday(date)) %>%  ggplot(aes(x=DOY, y=discharge, color = site)) + ggtitle("Daily mean GPP") + geom_point()+ facet_grid(year~.)





select(c(year, site, GPP_daily_mean, ER_daily_mean, K600_daily_mean ,depth , discharge_SD , DOC_mgL ,nitrateuM.adj.mn,TDP_uM,light,temp.water))


all.project.PCA.MEM.complete %>%  ggplot(aes(x=DOC_mgL, y=PC1, color = year)) + geom_point()

testpair <- all.project.PCA.MEM.complete %>% ungroup() %>% select(-c("site","burn","PF","year")) 

pairs(testpair, )




all.project.PCA.MEM.complete %>% ggplot(aes(x=year, y = discharge_SD, color = site))+geom_point(size = 10)

#######################################













all.project.PCA.MEM.complete$NEP_mean = all.project.PCA.MEM.complete$GPP_daily_mean +all.project.PCA.MEM.complete$ER_daily_mean

basic_mixed_model_NEP <- lmer(data = all.project.PCA.MEM.complete, (NEP_mean) ~ PC1 + burn + PF +(1|year), REML = TRUE)


plot(basic_mixed_model_NEP)

qqnorm(resid(basic_mixed_model_NEP))
qqline(resid(basic_mixed_model_NEP))
# Normal Q-Q plot does NOT look good


ggqqplot(residuals(basic_mixed_model_NEP))+ggtitle("ER Mixed Effects Model")



summary(basic_mixed_model_NEP)



all.project.PCA.MEM.complete %>%  ggplot(aes(x=site, y=NEP_mean)) + facet_wrap(~year, nrow = 2, dir = "v") +  geom_boxplot(aes(fill=as.factor(PF)))+
  facet_grid(. ~ burn , space="free_x", scales="free_x", switch="x") +
  theme_classic() +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill=NA, colour="grey50"),
        panel.spacing.x=unit(0,"cm"))+theme(plot.title = element_text(hjust = 1))+scale_fill_manual(values = c("#71A6D2", "#A13D2D"))+theme(axis.title.x=element_blank() )+scale_fill_manual(values = c("#71A6D2", "#A13D2D"), labels=c('High Permafrost', 'Low Permafrost'))+  theme(legend.title=element_blank())+theme(axis.title.x=element_blank() )+  theme(legend.title=element_blank())+theme(legend.position = c(0.2, 0.2))+labs(y = expression(paste("Mean Yearly MEP (g ", O[2] ," ", m^-2, d^-1, ")")))
















########################################################################



metab_all

library(lme4)
library(lmerTest)


  
  metab_all$year <- as.factor (metab_all$year)
  metab_all$burn <- as.factor(metab_all$burn)
  metab_all$PF <- as.factor(metab_all$PF)
  
  


metab_all.test <- metab_all


# 
# catag <- metab_all %>% select(date,site,burn,PF,year) %>% mutate(date = as.Date(date))
# 
# all.project.PCA.MEM <- full_join(all.project.PCA,catag, by = c("date","site"))


hist(log(metab_all$GPP_mean))

metab_all$NEP <- metab_all$ER_daily_mean +metab_all$GPP_daily_mean


basic_mixed_model <- lmer(data = metab_all, log(GPP_daily_mean) ~ burn+PF +(1|year), REML = TRUE)


plot(basic_mixed_model)

qqnorm(resid(basic_mixed_model))
qqline(resid(basic_mixed_model))
# long tail when log transformed


contrasts(metab_all$burn)


ggqqplot(residuals(basic_mixed_model))



summary(basic_mixed_model)
basic_mixed_model
#LOG
#when you go from burn to unburn: increses bu 0.43 log gpp, or 

exp(0.43)
exp(0.38)





all.project.PCA.MEM.testPFmean <- all.project.PCA.MEM %>% mutate(NEP = GPP_daily_mean +ER_daily_mean) %>% 
  group_by(PF) %>%
  summarise(across(-c(site,burn,year), mean, na.rm = TRUE))

all.project.PCA.MEM.testPFsd <- all.project.PCA.MEM%>% mutate(NEP = GPP_daily_mean +ER_daily_mean) %>% 
  group_by(PF) %>%
  summarise(across(-c(site,burn,year), sd, na.rm = TRUE))





all.project.PCA.MEM.testPFmean <- all.project.PCA.MEM %>% mutate(NEP = GPP_daily_mean +ER_daily_mean) %>% 
  group_by(burn) %>%
  summarise(across(-c(site,PF,year), mean, na.rm = TRUE))

all.project.PCA.MEM.testPFsd <- all.project.PCA.MEM%>% mutate(NEP = GPP_daily_mean +ER_daily_mean) %>% 
  group_by(burn) %>%
  summarise(across(-c(site,PF,year), sd, na.rm = TRUE))













all.project.PCA.MEM %>% aggregate(x = GPP_daily_mean,                # Specify data column
          by = list(year,burn,PF),              # Specify group indicator
          FUN = mean)                           # Specify function (i.e. mean)







