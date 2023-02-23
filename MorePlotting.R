#sites in the literature 
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
DoD.data.all.mean <- DoD.data.all %>% select( site, year, date, DO.obs, DO.sat, temp.water, depth, discharge) %>% 
  group_by(date,site) %>%
  summarise(across(-year, mean, na.rm = TRUE)) %>% mutate(date = as.Date(date))


###################


DoD_ALL <- full_join(DoD.data.all.mean, Suna.Exo.Select.All.mean, by = c("date", "site"))

metab_all.short <- metab_all %>% mutate(date = as.Date(date)) %>% select(date, site, GPP_daily_mean, ER_mean, K600_daily_mean)


DoD_ALL_metab <- full_join(DoD_ALL, metab_all.short, by = c("date", "site"))



library(dplyr)
metab_all.min <- group_by(metab_all, year, site)
df.min <- summarise(metab_all.min, min.DOY = min(DOY))


max(df.minYear$DOY)


metab_all.max <- group_by(metab_all, year, site)
df.max <- summarise(metab_all.max, max.DOY = max(DOY))


#highest minimum for all years 
metab_all.2019 <-metab_all %>% filter(year =="2019") %>% filter(DOY>= "167" & DOY<="274")
metab_all.2020 <-metab_all %>% filter(year =="2020") %>% filter(DOY>= "169" & DOY<="276")
metab_all.2021 <-metab_all %>% filter(year =="2021") %>% filter(DOY>= "163" & DOY<="270")
metab_all.2022 <-metab_all %>% filter(year =="2022") %>% filter(DOY>= "164" & DOY<="271")

metab_all_stw <- rbind(metab_all.2019, metab_all.2020, metab_all.2021, metab_all.2022)


summary(na.omit(metab_all_stw$GPP_mean))

mean(na.omit(metab_all_stw$GPP_mean))

median_cl_quantile <- function(x, q = c(0.25, 0.75), na.rm = TRUE){
  dat <- data.frame(y = median(x, na.rm = na.rm),
                    ymin = quantile(x, probs = q[1], na.rm = na.rm),
                    ymax = quantile(x, probs = q[2], na.rm = na.rm))
  return(dat)
}

ggplot(metab_all_stw,aes(DOY, GPP_mean)) +
  stat_summary(geom = "line", fun.y = median) +
  stat_summary(geom = "ribbon", fun.data = median_cl_quantile, alpha = 0.3, color = "green", fill = "green")+labs(y = expression(paste("Median GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(x = expression(paste("Day of the Year")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))




geom_smooth(stat = 'summary', alpha = 0.2, fill = 'red', color = 'red',
            fun.data = median_hilow, fun.args = list(conf.int = 1))





median(na.omit(metab_all_stw$GPP_mean))
mean(na.omit(metab_all_stw$GPP_mean))


summary(metab_all_stw$GPP_mean)


##### Q and and also tmep and DO



moos.metab <- read.csv(here("outputs", "moose2019-Run_2023-Full.rerun.02.04.csv"))
moos.metab$date <- as.POSIXct(moos.metab$date)


# grob <- grobTree(textGrob("2019", x="2019-09-01", y=4, hjust=0,
#   gp=gpar(col="black", fontsize=20, fontface="italic")))
# Plo





gpp2019 <- ggplot(data=moos.metab, aes(x=(date), y=GPP_mean)) + geom_point(color = "chartreuse4") + geom_errorbar(aes(ymin=GPP_mean-GPP_sd, ymax=GPP_mean+GPP_sd), width=.2, position=position_dodge(0.05), color = "chartreuse4")+ labs(y = expression(paste("GPP (g ", O[2] ," ", m^-2, d^-1, ")")))+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 20, face = "bold")) +ylim(0, 7)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+scale_x_datetime(limits = ymd_hm(c("2019-05-25 00:00", "2019-10-15 00:00")))+ annotate("text", x= as.POSIXct("2019-09-11"), y=max(moos.metab$GPP_mean),label="2019")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ annotation_custom(grobTree(textGrob("2019", x=0.8, hjust=0, y=0.8, vjust=0, gp=gpar(col="black", fontsize=30)))) +ggtitle("Moose Creek") +theme(plot.title = element_text(hjust = 0, size = 50))









moos.comb <- read.csv(here("outputs", "clean.moos.2019.full.csv"))
moos.comb$solar.time <- as.POSIXct(moos.comb$solar.time, tz = "UTC")

moos.comb$discharge  <- moos.comb$discharge 

DO.fig2019 <-  moos.comb %>% filter(datetimeAK >= min(moos.metab$date)) %>% filter(datetimeAK <= max(moos.metab$date)) %>% ggplot(aes(x = as.POSIXct(solar.time))) +geom_point(aes(y=DO.obs), color = "orange", size=0.4)+ labs(y = expression(paste("Dissolved Oxygen (mg ", L^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(axis.title.x=element_blank())+theme(text = element_text(size=20)) +scale_x_datetime(limits = ymd_hm(c("2019-05-25 00:00", "2019-10-15 00:00")))+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )

Temp.fig2019 <-  moos.comb %>% filter(datetimeAK >= min(moos.metab$date)) %>% filter(datetimeAK <= max(moos.metab$date)) %>% ggplot(aes(x = as.POSIXct(solar.time))) +geom_point(aes(y=temp.water), color = "red", size=0.4)+ labs(y = expression(paste("Temperature (°C", ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(axis.title.x=element_blank())+theme(text = element_text(size=20)) +scale_x_datetime(limits = ymd_hm(c("2019-05-25 00:00", "2019-10-15 00:00")))+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )

q.fig2019 <-  moos.comb %>% filter(datetimeAK >= min(moos.metab$date)) %>% filter(datetimeAK <= max(moos.metab$date)) %>% ggplot(aes(x = as.POSIXct(solar.time))) +geom_point(aes(y=discharge), color = "slateblue4", size=0.4)+ labs(y = expression(paste("Discharge (L ", s^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ylim(0, 9000)+theme(axis.title.x=element_blank())+theme(text = element_text(size=20)) +scale_x_datetime(limits = ymd_hm(c("2019-05-25 00:00", "2019-10-15 00:00")))


#L.fig <- moos.comb  %>% filter(datetimeAK >= min(moos.metab$date)) %>% filter(datetimeAK <= max(moos.metab$date)) %>% ggplot(aes(x = solar.time)) +geom_point(aes(y=light), color = "orange", size=0.4)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("PAR (umol ", m^-2, s^-1,")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ylim(0, max(moos.comb$light))


panGPP <- ggplotGrob(gpp2019)
panDO <- ggplotGrob(DO.fig2019)
panTemp <- ggplotGrob(Temp.fig2019)
panQ <- ggplotGrob(q.fig2019)


grid::grid.newpage()
grid.draw(rbind(panGPP, panDO, panQ,panTemp, size="max"))
fig2019 <- arrangeGrob(rbind(panGPP, panDO, panQ,panTemp,size="max"))
ggsave(path = here("plots"), file = "moos2019.gpp.q.pdf", fig2019, width = 12,  height = 6, units = "in", limitsize = FALSE)







##### Q and and also tmep and DO



moos.metab <- read.csv(here("outputs", "moose2020-Run_2023-Full.rerun.02.04.csv"))
moos.metab$date <- as.POSIXct(moos.metab$date)


# grob <- grobTree(textGrob("2020", x="2020-09-01", y=4, hjust=0,
#   gp=gpar(col="black", fontsize=20, fontface="italic")))
# Plo





gpp2020 <- ggplot(data=moos.metab, aes(x=(date), y=GPP_mean)) + geom_point(color = "chartreuse4") + geom_errorbar(aes(ymin=GPP_mean-GPP_sd, ymax=GPP_mean+GPP_sd), width=.2, position=position_dodge(0.05), color = "chartreuse4")+ labs(y = expression(paste("GPP (g ", O[2] ," ", m^-2, d^-1, ")")))+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 20, face = "bold")) +ylim(0, 7)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+scale_x_datetime(limits = ymd_hm(c("2020-05-25 00:00", "2020-10-15 00:00")))+ annotate("text", x= as.POSIXct("2020-09-11"), y=max(moos.metab$GPP_mean),label="2020")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ annotation_custom(grobTree(textGrob("2020", x=0.8, hjust=0, y=0.8, vjust=0, gp=gpar(col="black", fontsize=30))))+theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank() )









moos.comb <- read.csv(here("outputs", "clean.moos.2020.full.csv"))
moos.comb$solar.time <- as.POSIXct(moos.comb$solar.time, tz = "UTC")

moos.comb$discharge  <- moos.comb$discharge 

DO.fig2020 <-  moos.comb %>% filter(datetimeAK >= min(moos.metab$date)) %>% filter(datetimeAK <= max(moos.metab$date)) %>% ggplot(aes(x = as.POSIXct(solar.time))) +geom_point(aes(y=DO.obs), color = "orange", size=0.4)+ labs(y = expression(paste("Dissolved Oxygen (mg ", L^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(axis.title.x=element_blank())+theme(text = element_text(size=20)) +scale_x_datetime(limits = ymd_hm(c("2020-05-25 00:00", "2020-10-15 00:00")))+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank() )

Temp.fig2020 <-  moos.comb %>% filter(datetimeAK >= min(moos.metab$date)) %>% filter(datetimeAK <= max(moos.metab$date)) %>% ggplot(aes(x = as.POSIXct(solar.time))) +geom_point(aes(y=temp.water), color = "red", size=0.4)+ labs(y = expression(paste("Temperature (°C", ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(axis.title.x=element_blank())+theme(text = element_text(size=20)) +scale_x_datetime(limits = ymd_hm(c("2020-05-25 00:00", "2020-10-15 00:00")))+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank() )

q.fig2020 <-  moos.comb %>% filter(datetimeAK >= min(moos.metab$date)) %>% filter(datetimeAK <= max(moos.metab$date)) %>% ggplot(aes(x = as.POSIXct(solar.time))) +geom_point(aes(y=discharge), color = "slateblue4", size=0.4)+ labs(y = expression(paste("Discharge (L ", s^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ylim(0, 9000)+theme(axis.title.x=element_blank())+theme(text = element_text(size=20)) +scale_x_datetime(limits = ymd_hm(c("2020-05-25 00:00", "2020-10-15 00:00")))+theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank() )


#L.fig <- moos.comb  %>% filter(datetimeAK >= min(moos.metab$date)) %>% filter(datetimeAK <= max(moos.metab$date)) %>% ggplot(aes(x = solar.time)) +geom_point(aes(y=light), color = "orange", size=0.4)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("PAR (umol ", m^-2, s^-1,")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ylim(0, max(moos.comb$light))


panGPP <- ggplotGrob(gpp2020)
panDO <- ggplotGrob(DO.fig2020)
panTemp <- ggplotGrob(Temp.fig2020)
panQ <- ggplotGrob(q.fig2020)


grid::grid.newpage()
grid.draw(rbind(panGPP, panDO, panQ,panTemp, size="max"))
fig2020 <- arrangeGrob(rbind(panGPP, panDO, panQ,panTemp,size="max"))
ggsave(path = here("plots"), file = "moos2020.gpp.q.pdf", fig2020, width = 12,  height = 6, units = "in", limitsize = FALSE)







##### Q and and also tmep and DO



moos.metab <- read.csv(here("outputs", "moose2021-Run_2023-Full.rerun.02.04.csv"))
moos.metab$date <- as.POSIXct(moos.metab$date)


# grob <- grobTree(textGrob("2021", x="2021-09-01", y=4, hjust=0,
#   gp=gpar(col="black", fontsize=20, fontface="italic")))
# Plo





gpp2021 <- ggplot(data=moos.metab, aes(x=(date), y=GPP_mean)) + geom_point(color = "chartreuse4") + geom_errorbar(aes(ymin=GPP_mean-GPP_sd, ymax=GPP_mean+GPP_sd), width=.2, position=position_dodge(0.05), color = "chartreuse4")+ labs(y = expression(paste("GPP (g ", O[2] ," ", m^-2, d^-1, ")")))+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 20, face = "bold")) +ylim(0, 7)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+scale_x_datetime(limits = ymd_hm(c("2021-05-25 00:00", "2021-10-15 00:00")))+ annotate("text", x= as.POSIXct("2021-09-11"), y=max(moos.metab$GPP_mean),label="2021")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ annotation_custom(grobTree(textGrob("2021", x=0.8, hjust=0, y=0.8, vjust=0, gp=gpar(col="black", fontsize=30))))+theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank() )









moos.comb <- read.csv(here("outputs", "clean.moos.2021.full.csv"))
moos.comb$solar.time <- as.POSIXct(moos.comb$solar.time, tz = "UTC")

moos.comb$discharge  <- moos.comb$discharge 

DO.fig2021 <-  moos.comb %>% filter(datetimeAK >= min(moos.metab$date)) %>% filter(datetimeAK <= max(moos.metab$date)) %>% ggplot(aes(x = as.POSIXct(solar.time))) +geom_point(aes(y=DO.obs), color = "orange", size=0.4)+ labs(y = expression(paste("Dissolved Oxygen (mg ", L^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(axis.title.x=element_blank())+theme(text = element_text(size=20)) +scale_x_datetime(limits = ymd_hm(c("2021-05-25 00:00", "2021-10-15 00:00")))+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank() )

Temp.fig2021 <-  moos.comb %>% filter(datetimeAK >= min(moos.metab$date)) %>% filter(datetimeAK <= max(moos.metab$date)) %>% ggplot(aes(x = as.POSIXct(solar.time))) +geom_point(aes(y=temp.water), color = "red", size=0.4)+ labs(y = expression(paste("Temperature (°C", ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(axis.title.x=element_blank())+theme(text = element_text(size=20)) +scale_x_datetime(limits = ymd_hm(c("2021-05-25 00:00", "2021-10-15 00:00")))+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank() )

q.fig2021 <-  moos.comb %>% filter(datetimeAK >= min(moos.metab$date)) %>% filter(datetimeAK <= max(moos.metab$date)) %>% ggplot(aes(x = as.POSIXct(solar.time))) +geom_point(aes(y=discharge), color = "slateblue4", size=0.4)+ labs(y = expression(paste("Discharge (L ", s^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ylim(0, 9000)+theme(axis.title.x=element_blank())+theme(text = element_text(size=20)) +scale_x_datetime(limits = ymd_hm(c("2021-05-25 00:00", "2021-10-15 00:00")))+theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank() )

#L.fig <- moos.comb  %>% filter(datetimeAK >= min(moos.metab$date)) %>% filter(datetimeAK <= max(moos.metab$date)) %>% ggplot(aes(x = solar.time)) +geom_point(aes(y=light), color = "orange", size=0.4)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("PAR (umol ", m^-2, s^-1,")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ylim(0, max(moos.comb$light))


panGPP <- ggplotGrob(gpp2021)
panDO <- ggplotGrob(DO.fig2021)
panTemp <- ggplotGrob(Temp.fig2021)
panQ <- ggplotGrob(q.fig2021)


grid::grid.newpage()
grid.draw(rbind(panGPP, panDO, panQ,panTemp, size="max"))
fig2021 <- arrangeGrob(rbind(panGPP, panDO, panQ,panTemp,size="max"))
ggsave(path = here("plots"), file = "moos2021.gpp.q.pdf", fig2021, width = 12,  height = 6, units = "in", limitsize = FALSE)










##### Q and and also tmep and DO



moos.metab <- read.csv(here("outputs", "moose2022-Run_2023-Full.rerun.02.04.csv"))
moos.metab$date <- as.POSIXct(moos.metab$date)


# grob <- grobTree(textGrob("2022", x="2022-09-01", y=4, hjust=0,
#   gp=gpar(col="black", fontsize=20, fontface="italic")))
# Plo





gpp2022 <- ggplot(data=moos.metab, aes(x=(date), y=GPP_mean)) + geom_point(color = "chartreuse4") + geom_errorbar(aes(ymin=GPP_mean-GPP_sd, ymax=GPP_mean+GPP_sd), width=.2, position=position_dodge(0.05), color = "chartreuse4")+ labs(y = expression(paste("GPP (g ", O[2] ," ", m^-2, d^-1, ")")))+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 20, face = "bold")) +ylim(0, 7)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+scale_x_datetime(limits = ymd_hm(c("2022-05-25 00:00", "2022-10-15 00:00")))+ annotate("text", x= as.POSIXct("2022-09-11"), y=max(moos.metab$GPP_mean),label="2022")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))









moos.comb <- read.csv(here("outputs", "clean.moos.2022.full.csv"))
moos.comb$solar.time <- as.POSIXct(moos.comb$solar.time, tz = "UTC")

moos.comb$discharge  <- moos.comb$discharge 

DO.fig2022 <-  moos.comb %>% filter(datetimeAK >= min(moos.metab$date)) %>% filter(datetimeAK <= max(moos.metab$date)) %>% ggplot(aes(x = as.POSIXct(solar.time))) +geom_point(aes(y=DO.obs), color = "orange", size=0.4)+ labs(y = expression(paste("Dissolved Oxygen (mg ", L^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(axis.title.x=element_blank())+theme(text = element_text(size=20)) +scale_x_datetime(limits = ymd_hm(c("2022-05-25 00:00", "2022-10-15 00:00")))+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )

Temp.fig2022 <-  moos.comb %>% filter(datetimeAK >= min(moos.metab$date)) %>% filter(datetimeAK <= max(moos.metab$date)) %>% ggplot(aes(x = as.POSIXct(solar.time))) +geom_point(aes(y=temp.water), color = "red", size=0.4)+ labs(y = expression(paste("Temperature (°C", ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(axis.title.x=element_blank())+theme(text = element_text(size=20)) +scale_x_datetime(limits = ymd_hm(c("2022-05-25 00:00", "2022-10-15 00:00")))+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )

q.fig2022 <-  moos.comb %>% filter(datetimeAK >= min(moos.metab$date)) %>% filter(datetimeAK <= max(moos.metab$date)) %>% ggplot(aes(x = as.POSIXct(solar.time))) +geom_point(aes(y=discharge), color = "slateblue4", size=0.4)+ labs(y = expression(paste("Discharge (L ", s^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ylim(0, 9000)+theme(axis.title.x=element_blank())+theme(text = element_text(size=20)) +scale_x_datetime(limits = ymd_hm(c("2022-05-25 00:00", "2022-10-15 00:00")))

#L.fig <- moos.comb  %>% filter(datetimeAK >= min(moos.metab$date)) %>% filter(datetimeAK <= max(moos.metab$date)) %>% ggplot(aes(x = solar.time)) +geom_point(aes(y=light), color = "orange", size=0.4)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("PAR (umol ", m^-2, s^-1,")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ylim(0, max(moos.comb$light))


panGPP <- ggplotGrob(gpp2022)
panDO <- ggplotGrob(DO.fig2022)
panTemp <- ggplotGrob(Temp.fig2022)
panQ <- ggplotGrob(q.fig2022)


grid::grid.newpage()
grid.draw(rbind(panGPP, panDO, panQ,panTemp, size="max"))


fig2022 <- arrangeGrob(rbind(panGPP, panDO, panQ,panTemp,size="max"))
ggsave(path = here("plots"), file = "moos2022.gpp.q.pdf", fig2022, width = 12,  height = 6, units = "in", limitsize = FALSE)



pdf(here('plots',"MOOS.DoD.plot.pdf"), height = 15, width = 25)

wrap_plots(gpp2019,gpp2020,gpp2021,gpp2022, DO.fig2019, DO.fig2020,DO.fig2021,DO.fig2022, Temp.fig2019,Temp.fig2020,Temp.fig2021,Temp.fig2022,q.fig2019, q.fig2020, q.fig2021, q.fig2022, ncol = 4, nrow = 4)
dev.off()




# Frankenstien plot

moos.merge <- moos.comb

moos.merge$date <- as.Date(moos.comb$datetimeAK)

moos.merge <- full_join(moos.merge, moos.metab, by = "date")

Temp.DO.fig2022 <-  moos.merge %>% filter(datetimeAK >= min(moos.metab$date)) %>% filter(datetimeAK <= max(moos.metab$date)) %>% ggplot(aes(x = as.POSIXct(solar.time))) +
  geom_point(aes(y=temp.water), color = "red", size=0.4)+  geom_point(aes(x=solar.time,y=DO.obs/0.7), color = "dark blue") +scale_y_continuous(name = (expression(paste("Temperature (°C", ")"))), sec.axis = sec_axis(name="Dissolved Oxygen (mg/L)", trans = ~.*0.7) ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(axis.title.x=element_blank())+theme(text = element_text(size=20)) +scale_x_datetime(limits = ymd_hm(c("2022-05-25 00:00", "2022-10-15 00:00")))+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )



  
p1 <- ggplot(moos.merge, aes(as.POSIXct(datetimeAK, tz = "America/Anchorage"), temp.water)) + 
  geom_point() + theme(axis.line = element_line())+theme(axis.title.x=element_blank() )+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+  
  
  geom_point(aes(x=solar.time,y=GPP_mean), color = "dark blue") +scale_y_continuous(name = (expression(paste("Temperature (°C", ")"))), sec.axis = sec_axis(name="GPP", trans = ~.*0.7) )

p2 <- ggplot(moos.merge, aes(as.POSIXct(datetimeAK, tz = "America/Anchorage"), DO.obs)) + geom_point() +
  theme(axis.line = element_line())+theme(axis.title.x=element_blank() )+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))

p3 <- ggplot(moos.merge, aes(as.POSIXct(datetimeAK,  tz = "America/Anchorage"))) + 
  geom_point(aes(y= temp.water, color = "Temp")) +
  geom_point(aes(y = DO.obs, color = (paste("Dissolved Oxygen (mg/L)")))) +
  theme(axis.line = element_line(),
        plot.margin = margin(10, 10, 10, 30))+theme(axis.title.x=element_blank() )+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))

wrap_elements(get_plot_component(p1, "ylab-l")) +
  wrap_elements(get_y_axis(p1)) +
  wrap_elements(get_plot_component(p2, "ylab-l")) +
  wrap_elements(get_y_axis(p2)) +
  p3 + 
  plot_layout(widths = c(3, 1, 3, 1, 40))+theme(axis.title.x=element_blank() )+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))


pdf(here('plots',"MOOS.DoD.plot.2022.Karen.pdf"), height = 10, width = 15)

moos.merge %>% filter(datetimeAK >= min(moos.metab$date)) %>% filter(datetimeAK <= max(moos.metab$date)) %>% ggplot(aes(x = as.POSIXct(solar.time)))+geom_point(aes(y=GPP_mean)) +
  geom_point(aes(y=temp.water), size=0.4)+  geom_point(aes(y=DO.obs),size = 0.4) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(axis.title.x=element_blank())+theme(text = element_text(size=20)) +scale_x_datetime(limits = ymd_hm(c("2022-05-25 00:00", "2022-10-15 00:00")))+theme( axis.title.x=element_blank() )+theme(axis.title.y=element_blank() ) +theme(legend.text.align = 0)+theme(legend.title=element_blank())+ scale_fill_discrete(guide = guide_legend(), labels=c(expression(paste("Dissolved Oxygen (mg ", L^-1, ")")), expression(paste("GPP (g ", O[2] ," ", m^-2, d^-1, ")")), expression(paste("Temperature (°C", ")")))) +
  theme(legend.position="bottom")+ theme(legend.background=element_blank())

dev.off()



gpp.DO.2022 <- ggplot() + geom_point(aes(x=(date), y=GPP_mean), moos.metab, color = "chartreuse4") + geom_errorbar(aes(ymin=GPP_mean-GPP_sd, ymax=GPP_mean+GPP_sd), width=.2, position=position_dodge(0.05), color = "chartreuse4")+ labs(y = expression(paste("GPP (g ", O[2] ," ", m^-2, d^-1, ")")))+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 20, face = "bold")) +ylim(0, 7)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+scale_x_datetime(limits = ymd_hm(c("2022-05-25 00:00", "2022-10-15 00:00")))+ annotate("text", x= as.POSIXct("2022-09-11"), y=max(moos.metab$GPP_mean),label="2022")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ geom_point(aes(x=solar.time,y=DO.obs/0.7), moos.comb, color = "dark blue")+
  scale_y_continuous(sec.axis = sec_axis(trans = ~.*0.7), )






pdf(here('plots',"MOOS.DoD.plot.2022.pdf"), height = 15, width = 10)

wrap_plots(gpp2022,DO.fig2022, Temp.fig2022, q.fig2022, ncol = 1, nrow = 4)
dev.off()




library("RColorBrewer") 
library(patchwork)
plot_layout(fig2019+fig2020+fig2021+fig2022, nrow=2, ncol = 2)

pdf(here('plots',"GPP.plot.pdf"), height = 4.5*1.2, width = 5*1.2)
plot_layout(plot2019+plot2020+plot2021+plot2022, nrow=2, ncol = 2)
dev.off()







