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


###########

clean.frch.2019 <- read.csv(here("outputs","clean.frch.2019.full.csv")) %>% mutate(site = "FRCH") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge)

clean.frch.2020 <- read.csv(here("outputs","clean.frch.2020.full.csv")) %>% mutate(site = "FRCH") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge)

clean.frch.2021 <- read.csv(here("outputs","clean.frch.2021.full.csv")) %>% mutate(site = "FRCH") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge)

clean.frch.2022 <- read.csv(here("outputs","clean.frch.2022.full.csv")) %>% mutate(site = "FRCH") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge)



clean.moos.2019 <- read.csv(here("outputs","clean.moos.2019.full.csv")) %>% mutate(site = "MOOS") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge)

clean.moos.2020 <- read.csv(here("outputs","clean.moos.2020.full.csv")) %>% mutate(site = "MOOS") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge)

clean.moos.2021 <- read.csv(here("outputs","clean.moos.2021.full.csv")) %>% mutate(site = "MOOS") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge)

clean.moos.2022 <- read.csv(here("outputs","clean.moos.2022.full.csv")) %>% mutate(site = "MOOS") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge)



clean.poke.2019 <- read.csv(here("outputs","clean.poke.2019.full.csv")) %>% mutate(site = "POKE") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge)

clean.poke.2020 <- read.csv(here("outputs","clean.poke.2020.full.csv")) %>% mutate(site = "POKE") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge)

clean.poke.2021 <- read.csv(here("outputs","clean.poke.2021.full.csv")) %>% mutate(site = "POKE") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge)

clean.poke.2022 <- read.csv(here("outputs","clean.poke.2022.full.csv")) %>% mutate(site = "POKE") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge)


clean.vaul.2019 <- read.csv(here("outputs","clean.vaul.2019.full.csv")) %>% mutate(site = "VAUL") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge)

clean.vaul.2020 <- read.csv(here("outputs","clean.vaul.2020.full.csv")) %>% mutate(site = "VAUL") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge)

clean.vaul.2021 <- read.csv(here("outputs","clean.vaul.2021.full.csv")) %>% mutate(site = "VAUL") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge)

clean.vaul.2022 <- read.csv(here("outputs","clean.vaul.2022.full.csv")) %>% mutate(site = "VAUL") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge)


clean.strt.2019 <- read.csv(here("outputs","clean.strt.2019.full.csv")) %>% mutate(site = "STRT") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge)

clean.strt.2020 <- read.csv(here("outputs","clean.strt.2020.full.csv")) %>% mutate(site = "STRT") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge)

clean.strt.2021 <- read.csv(here("outputs","clean.strt.2021.full.csv")) %>% mutate(site = "STRT") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time))%>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge)

clean.strt.2022 <- read.csv(here("outputs","clean.strt.2022.full.csv")) %>% mutate(site = "STRT") %>% mutate(DOY = yday(as.POSIXct(solar.time)) ) %>%  mutate(year = year(solar.time) ) %>% mutate(date = as.Date(solar.time)) %>% select(datetimeAK, solar.time, site, year, date, DO.obs, DO.sat.EXO, temp.water, depth, discharge)




DoD.data.all <- rbind(clean.frch.2019,clean.frch.2020,clean.frch.2021,clean.frch.2022,clean.moos.2019,clean.moos.2020,clean.moos.2021,clean.moos.2022, clean.poke.2019,clean.poke.2020,clean.poke.2021,clean.poke.2022,clean.vaul.2019,clean.vaul.2020,clean.vaul.2021,clean.vaul.2022,clean.strt.2019,clean.strt.2020,clean.strt.2021,clean.strt.2022) %>% rename(DO.sat = DO.sat.EXO)


DoD.data.all <- as.data.frame(DoD.data.all) %>% mutate (date = as.character(date), site = as.character(site), year = as.numeric(year))



### Add in Other data ###
suna_exo_2019 <- read.csv("C:/Users/jadams125/Documents/GitHub/DoD_2019/processed_sensor_dat/SUNA.EXO.int.corr.lab_2019.csv")

suna_exo_2020 <- read.csv("C:/Users/jadams125/Documents/GitHub/DoD_2020/processed_sensor_dat/SUNA.EXO.int.corr.lab_2020.csv")

suna_exo_2021 <- read.csv("C:/Users/jadams125/Documents/GitHub/DoD_2021/processed_sensor_dat/SUNA.EXO.int.corr.lab_2021.csv")

suna_exo_2022 <- read.csv("C:/Users/jadams125/Documents/GitHub/DoD_2022/processed_sensor_dat/SUNA.EXO.int.corr.lab_2022.csv")



suna_exo_2019.select <- suna_exo_2019 %>% select(min, fDOM.QSU.int, Turbidity.FNU.mn.adj, nitrateuM.adj.mn,Site) %>% mutate(date = as.Date(min))

suna_exo_2020.select <- suna_exo_2020 %>% select(min, fDOM.QSU.int, Turbidity.FNU.mn.adj, nitrateuM.adj.mn,Site) %>% mutate(date = as.Date(min))

suna_exo_2021.select <- suna_exo_2021 %>% select(min, fDOM.QSU.int, Turbidity.FNU.mn.adj, nitrateuM.adj.mn,Site) %>% mutate(date = as.Date(min))

suna_exo_2022.select <- suna_exo_2022 %>% select(min, fDOM.QSU.int, Turbidity.FNU.mn.adj, nitrateuM.adj.mn,Site) %>% mutate(date = as.Date(min))


SunaExoSelect_all <- rbind(suna_exo_2019.select,suna_exo_2020.select,suna_exo_2021.select,suna_exo_2022.select )


Suna.Exo.Select.All.mean <- SunaExoSelect_all  %>% 
  group_by(date,Site) %>%
  summarise(across(-min, mean, na.rm = TRUE)) %>% rename(site = Site) %>% mutate(date = as.Date(date))

#######################


chems_data <- read.csv("C:/Users/jadams125/Documents/GitHub/DoD_chems_meta/chems_meta_DoD.csv")

chems_data_short <- chems_data %>% select(Date, Site, NitrateN_mgL, TDP_uM, DOC_mgL) %>% rename(date = Date, site = Site) %>% mutate(date = as.Date(date))







#######################
DoD.data.all.mean <- DoD.data.all %>% select( site, year, date, DO.obs, DO.sat, temp.water, depth, discharge) %>% 
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


######################

DoD_ALL <- full_join(DoD.data.all.mean, Suna.Exo.Select.All.mean, by = c("date", "site"))

DoD_ALL <- full_join(DoD_ALL, chems_data_short, by = c("date", "site"))

DoD_ALL <- full_join(DoD_ALL, DoD_PAR_meaned, by = c("date", "site"))


metab_all.short <- metab_all %>% mutate(date = as.Date(date)) %>% select(date, site, GPP_daily_mean, ER_mean, K600_daily_mean)

DoD_ALL_metab <- full_join(DoD_ALL, metab_all.short, by = c("date", "site"))

library(here)
library(tidyverse)
library(devtools)
# install_github("vqv/ggbiplot")
library(ggbiplot)
library(FactoMineR)
library(factoextra)
library(corrplot)

testPCA <- princomp(~GPP_daily_mean + depth + discharge + fDOM.QSU.int + Turbidity.FNU.mn.adj+ nitrateuM.adj.mn+ TDP_uM + DOC_mgL+light ,data = DoD_ALL_metab , cor=TRUE)


summary(testPCA)  #print proportion variance explained by each axis
loadings(testPCA)  #print factor loadings

plot(testPCA)  # “scree” plot (convenient way of visualizing variance explained by components)

pairs(~GPP_daily_mean + depth + discharge + fDOM.QSU.int + Turbidity.FNU.mn.adj+ nitrateuM.adj.mn+ TDP_uM + DOC_mgL + light,data = DoD_ALL_metab)

testscores <- testPCA$scores 


plot(testscores[,1], testscores[,2], col=DoD_ALL_metab$index, xlab="PC1", ylab="PC2")
