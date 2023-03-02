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

library(here)


metab_all <- read.csv(here("outputs","DoD_metab_TS.csv"))

metab_all$NEP_mean <- metab_all$GPP_daily_mean + metab_all$ER_daily_mean

metab_all.cat <- metab_all %>% select(site, date, PF, burn, GPP_daily_mean, ER_daily_mean, NEP_mean) %>% 
  group_by(PF) %>%
  summarise(across(-c(date,site,burn), sd, na.rm = TRUE)) 



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



clean.craw.2022 <- read.csv(here("outputs","clean.craw.2022.csv")) %>% mutate(site = "CRAW") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time)) %>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat, temp.water, depth, discharge) %>% mutate(depth = (depth/100)) %>% mutate(discharge = (discharge/1000))


clean.mast.2022 <- read.csv(here("outputs","clean.mast.2022.csv")) %>% mutate(site = "MAST") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time)) %>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat, temp.water, depth, discharge) %>% mutate(depth = (depth/100))%>% mutate(discharge = (discharge/1000))


clean.shov.2022 <- read.csv(here("outputs","clean.shov.2022.csv")) %>% mutate(site = "SHOV") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time)) %>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat, temp.water, depth, discharge) %>% mutate(depth = (depth/100))%>% mutate(discharge = (discharge/1000))



DoD.data.all <- rbind(clean.frch.2019,clean.frch.2020,clean.frch.2021,clean.frch.2022,clean.moos.2019,clean.moos.2020,clean.moos.2021,clean.moos.2022, clean.poke.2019,clean.poke.2020,clean.poke.2021,clean.poke.2022,clean.vaul.2019,clean.vaul.2020,clean.vaul.2021,clean.vaul.2022,clean.strt.2019,clean.strt.2020,clean.strt.2021,clean.strt.2022,clean.craw.2022,clean.mast.2022, clean.shov.2022)


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

#######################


chems_data <- read.csv("C:/Users/Jacob/OneDrive - University of Alaska/GitHub/DoD_chems_meta/chems_meta_DoD.csv")

chems_data_short <- chems_data %>% select(Date, Site, NitrateN_mgL, TDP_uM, DOC_mgL) %>% rename(date = Date, site = Site) %>% mutate(date = as.Date(date))







#######################
DoD.data.all.mean <- DoD.data.all %>% select( site, year, date, DO.obs, DO.sat, temp.water, depth, discharge, discharge_SD) %>% 
  group_by(date,site) %>%
  summarise(across(-year, mean, na.rm = TRUE)) %>% mutate(date = as.Date(date))


###################


chems_data_short





######################

frchPAR <- read.csv(here("outputs","frch.combinded.par.csv")) %>% mutate(site = "FRCH") %>% mutate(date = as.Date(DateTime))
pokePAR <- read.csv(here("outputs","poke.combinded.par.csv"))%>% mutate(site = "POKE") %>% mutate(date = as.Date(DateTime))
moosPAR <- read.csv(here("outputs","moos.combinded.par.csv"))%>% mutate(site = "MOOS") %>% mutate(date = as.Date(DateTime))
vaulPAR <- read.csv(here("outputs","vaul.combinded.par.csv"))%>% mutate(site = "VAUL") %>% mutate(date = as.Date(DateTime))
strtPAR <- read.csv(here("outputs","strt.combinded.par.csv"))%>% mutate(site = "STRT") %>% mutate(date = as.Date(DateTime))


DoD_PAR <- rbind(frchPAR,pokePAR,moosPAR,vaulPAR,strtPAR) %>% rename(light = Calibrated.Value) %>% mutate(date = as.Date(date))


DoD_PAR_meaned <- DoD_PAR %>% group_by(date,site) %>%
  summarise(across(-DateTime, mean, na.rm = TRUE))





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





######################


DoD_ALL <- full_join(DoD.data.all.mean, Suna.Exo.Select.All.mean, by = c("date", "site"))

DoD_ALL <- full_join(DoD_ALL, chems_data_short, by = c("date", "site"))

DoD_ALL <- full_join(DoD_ALL, DoD_PAR_meaned, by = c("date", "site"))


metab_all.short <- metab_all %>% mutate(date = as.Date(date)) %>% select(date, site,GPP_daily_mean, ER_daily_mean, K600_daily_mean)

DoD_ALL_metab <- full_join(DoD_ALL, metab_all.short, by = c("date", "site"))






all.project <- rbind.fill(DoD_ALL_metab, Epscor_all)


all.project <- all.project  %>% 
  group_by(date,site) %>%
  summarise(across(-c("year","Site"), mean, na.rm = TRUE)) %>% filter(site == "POKE"| site == "VAUL"| site == "MOOS"| site == "FRCH"| site == "STRT"| site == "MAST"| site == "CRAW"| site == "SHOV")


write.csv(all.project, here("outputs",'daily.metab.chems.data.csv'))



all.project <- all.project %>% mutate_all(~ifelse(is.nan(.), NA, .))



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
plot(testPCA)  # “scree” plot (convenient way of visualizing variance explained by components)
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



all.project.prep <- all.project %>% select(date, site, GPP_daily_mean, ER_daily_mean, K600_daily_mean,TDP_uM,DOC_mgL ,depth , discharge_SD , fDOM.QSU.int , Turbidity.FNU.mn.adj,nitrateuM.adj.mn,light)


catag <- metab_all %>% select(date,site,burn,PF,year) %>% mutate(date = as.Date(date))

all.project.PCA.MEM <- full_join(all.project.prep ,catag, by = c("date","site"))

all.project.PCA.MEM <- all.project.PCA.MEM %>% select(site, date, year, PF, burn, GPP_daily_mean, ER_daily_mean, K600_daily_mean, DOC_mgL, nitrateuM.adj.mn, depth, discharge_SD, TDP_uM, light)


all.project.PCA.MEM$year <- ((as.character(all.project.PCA.MEM$year)))

all.project.PCA.MEM.year <- all.project.PCA.MEM %>% 
  group_by(year,site) %>%
  summarise(across(-c(date,PF,burn), mean, na.rm = TRUE))


all.project.PCA.MEM.year <-  na.omit (all.project.PCA.MEM.year %>% mutate_all(~ifelse(is.nan(.), NA, .)))


# 
# testPCA <- prcomp(~depth + discharge_SD + DOC_mgL +TDP_uM + nitrateuM.adj.mn+light, data = all.project.PCA.MEM.year ,center = TRUE, scale = TRUE)



plot(testPCA)

testPCA <- prcomp(~depth + discharge_SD + DOC_mgL +TDP_uM + nitrateuM.adj.mn+light, data = all.project.PCA.MEM.year ,center = TRUE, scale = TRUE)

varPCA <- get_pca_var(testPCA)

contrib <- varPCA$contrib
corrplot(contrib, is.corr = FALSE)

ggbiplot(testPCA, choices = 1:2)

testPCA_scores2 <- testPCA$x[,1:2]



all.project.PCA.MEM.year.B <- all.project.PCA.MEM.year %>% select(c(year, site, GPP_daily_mean, ER_daily_mean, K600_daily_mean ,depth , discharge_SD , DOC_mgL ,nitrateuM.adj.mn,TDP_uM,light))

all.project.PCA.MEM.year.PCA <- all.project.PCA.MEM.year.B[complete.cases(all.project.PCA.MEM.year.B),]

all.project.PCA <- cbind(all.project.PCA.MEM.year.PCA, testPCA_scores2)

catag$year <- as.character(catag$year)

catag <- catag %>% select(-date)

all.project.PCA.MEM.complete <- full_join(all.project.PCA ,catag, by = c("year","site"))

all.project.PCA.MEM.complete <- na.omit(distinct(all.project.PCA.MEM.complete))

############################################


metab_all.short



library(lme4)
library(lmerTest)




basic_mixed_model <- lmer(data = all.project.PCA.MEM.complete, (GPP_daily_mean) ~ PC1 + burn + PF +(1|year), REML = TRUE)


plot(basic_mixed_model)

qqnorm(resid(basic_mixed_model))
qqline(resid(basic_mixed_model))
# Normal Q-Q plot does NOT look good


ggqqplot(residuals(basic_mixed_model))+ggtitle("GPP Mixed Effects Model")



summary(basic_mixed_model)



all.project.PCA.MEM.complete %>%  ggplot(aes(x=site, y=GPP_daily_mean)) + facet_wrap(~year, nrow = 2, dir = "v") +  geom_boxplot(aes(fill=as.factor(PF)))+
  facet_grid(. ~ burn , space="free_x", scales="free_x", switch="x") +
  theme_classic() +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill=NA, colour="grey50"),
        panel.spacing.x=unit(0,"cm"))+theme(plot.title = element_text(hjust = 1))+scale_fill_manual(values = c("#71A6D2", "#A13D2D"))+theme(axis.title.x=element_blank() )+scale_fill_manual(values = c("#71A6D2", "#A13D2D"), labels=c('High Permafrost', 'Low Permafrost'))+  theme(legend.title=element_blank())+theme(axis.title.x=element_blank() )+  theme(legend.title=element_blank())+theme(legend.position = c(0.6, 0.8))+labs(y = expression(paste("Mean Yearly GPP (g ", O[2] ," ", m^-2, d^-1, ")")))










basic_mixed_model_ER <- lmer(data = all.project.PCA.MEM.complete, (ER_daily_mean) ~ PC1 + burn + PF +(1|year), REML = TRUE)


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
        panel.spacing.x=unit(0,"cm"))+theme(plot.title = element_text(hjust = 1))+scale_fill_manual(values = c("#71A6D2", "#A13D2D"))+theme(axis.title.x=element_blank() )+scale_fill_manual(values = c("#71A6D2", "#A13D2D"), labels=c('High Permafrost', 'Low Permafrost'))+  theme(legend.title=element_blank())+theme(axis.title.x=element_blank() )+  theme(legend.title=element_blank())+theme(legend.position = c(0.2, 0.2))+labs(y = expression(paste("Mean Yearly ER (g ", O[2] ," ", m^-2, d^-1, ")")))
















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



