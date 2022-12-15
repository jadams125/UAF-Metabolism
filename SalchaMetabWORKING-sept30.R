
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

library(imputeTS)
library(itsmr)
library(purrr)


#Salcha Metabolism Remade


Salcha.exo.2021 <- read.csv(file=file.path("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Salcha/EXO_processed/SALCHA.EXO.cl.corr.2021.csv"))

ts_2021.salcha <- data.frame(datetimeAK = force_tz(as.POSIXct(seq(ymd_hm("2021-05-03 10:30"),ymd_hm("2021-10-08 09:00"), by = '15 mins')), tz = "America/Anchorage"))

Salcha.exo.2021$datetimeAK <- as.POSIXct(Salcha.exo.2021$datetimeAK, tz = "America/Anchorage")

test <- full_join(ts_2021.salcha, Salcha.exo.2021, by = "datetimeAK")

write.csv(test, "C:/Users/jacob/OneDrive - University of Alaska/GitHub/UAF-Metabolism/salcha.2021.full.ts.csv")

setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/")

library(naniar)

test %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = ODO.mgL.mn)) + geom_miss_point()

library(data.table)
#Remove NA periods longer than 12 hours
test <- test %>%
  mutate(datetimeAK = lubridate::ymd_hms(datetimeAK)) %>%
  group_by(datetimeAK, new = rleid(is.na(ODO.mgL.mn))) %>% 
  ungroup() %>% 
  group_by(ODO.mgL.mn,new) %>% 
  filter(n()<48) %>% 
  select(-new)

test %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = ODO.mgL.mn)) + geom_miss_point()
test$ODO.mgL.mn <- na_kalman(test$ODO.mgL.mn)
test %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = ODO.mgL.mn)) + geom_miss_point()


test %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = ODO.Psat.mn.adj)) + geom_miss_point()
test$ODO.Psat.mn.adj <- na_kalman(test$ODO.Psat.mn.adj)
test %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = ODO.Psat.mn.adj)) + geom_miss_point()


test %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = Temp.C.int)) + geom_miss_point()
test$Temp.C.int <- na_kalman(test$Temp.C.int, type = "level")
test %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = Temp.C.int)) + geom_miss_point()





# Discharge and Depth
dischargeSalcha <- read.csv("UAF-Metabolism/dischargeSalchaTXT.csv")
dischargeSalcha <- dischargeSalcha[-c(1), ]



dischargeSalcha$datetimeAK <- as.POSIXct(mdy_hm(dischargeSalcha$datetime))

dischargeSalcha$datetimeAK <- force_tz(dischargeSalcha$datetimeAK, "America/Anchorage")

dischargeSalcha <- dischargeSalcha %>% rename(depth = X1761_00065)
dischargeSalcha <- dischargeSalcha %>% rename(discharge = X1760_00060)

dischargeSalcha <- dischargeSalcha %>% select(discharge, depth, datetimeAK)

# convert to Meters 

#correction factor
dischargeSalcha$depth <- (as.numeric(dischargeSalcha$depth)- 4.9) * 0.3048
dischargeSalcha$discharge <- as.numeric(dischargeSalcha$discharge) * 0.0283168


dischargeSalcha <- full_join(ts_2021.salcha, dischargeSalcha, by = "datetimeAK")

write.csv(dischargeSalcha, ("C:/Users/jacob/OneDrive - University of Alaska/GitHub/UAF-Metabolism/q_data.csv"))

dischargeSalcha <- dischargeSalcha %>%
  mutate(datetimeAK = lubridate::ymd_hms(datetimeAK)) %>%
  group_by(datetimeAK, new = rleid(is.na(discharge))) %>% 
  ungroup() %>% 
  group_by(discharge,new) %>% 
  filter(n()<48) %>% 
  select(-new)


dischargeSalcha %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = discharge)) + geom_miss_point()

dischargeSalcha$discharge <- na_kalman(dischargeSalcha$discharge)

dischargeSalcha %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = discharge)) + geom_miss_point()



dischargeSalcha %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = depth)) + geom_miss_point()

dischargeSalcha$depth <- na_kalman(dischargeSalcha$depth, type = "level")

dischargeSalcha %>% ggplot(aes(x = as.POSIXct(datetimeAK), y = depth)) + geom_miss_point()





#####

salchaData <- test

salchaData$datetimeAK <- as.POSIXct(salchaData$datetimeAK) 

#Solar Time and Light

salchaData$solar.time <- calc_solar_time(as.POSIXct(salchaData$datetimeAK, tz = "Alaska/Anchorage"),-146.915323)

#CRREL Met Tower
CRREL <- read.csv(here("MetTowerData.csv"), skip = 5)

CRREL$datetimeAK <- as.POSIXct(CRREL$Time, tz = "America/Anchorage")

CRREL <- CRREL %>% rename(ParWm2 = CPCRW.CRREL.Main.Met.Station..PAR_Den_AVG.W.m2.)


###### 2021 #####
PAR.2021.url <- "https://drive.google.com/drive/u/1/folders/1EPjHLDmfbCo5n12AKj7QpN2vXRCPQtg5"
PAR_2021.prt1 <- drive_get(as_id(PAR.2021.url))
par2021_glist <- drive_ls(PAR_2021.prt1, pattern = "all.dates.par.2021.csv")
walk(par2021_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
par2021.Data <- read.csv("all.dates.par.2021.csv",
                         skip = 0, header = TRUE)
# Sort into sites
poke.par2021.Data <- par2021.Data %>% filter(site == "poke")

poke.par2021.Data <- poke.par2021.Data %>%
  dplyr::rename(RawValue = V5)

poke.par2021.Data$DateTime <- as.POSIXct(poke.par2021.Data$DateTime)

#Calibrate loggers to LICOR
poke.par2021.Data$Calibrated.Value <- poke.par2021.Data$Calibrated.Value * 0.035



#2021
CRREL_SALCHA <- left_join(salchaData,CRREL, by = "datetimeAK")

CRREL_SALCHA$ParWm2 <- na_kalman(CRREL_SALCHA$ParWm2)


max.par.poke.2021 <- max(poke.par2021.Data$Calibrated.Value)

max.par.crrel.2021 <- max(CRREL_SALCHA$ParWm2)

multiplyFactor.2021 <- max.par.poke.2021/max.par.crrel.2021


CRREL_SALCHA$ParMetCor <- CRREL_SALCHA$ParWm2 *multiplyFactor.2021




#DO SAT


CRREL_SALCHA$DO.sat = CRREL_SALCHA$ODO.mgL.mn /(CRREL_SALCHA$ODO.Psat.mn.adj/ 100)

CRREL_SALCHA$DO.obs = CRREL_SALCHA$ODO.mgL.mn

CRREL_SALCHA$temp.water = CRREL_SALCHA$Temp.C.int



# CRREL_SALCHA$pressure.air.mmHg <- CRREL_SALCHA$pressure.air.mbar / 1.3332239
# 
# CRREL_SALCHA$ODO.Psat.new <- CRREL_SALCHA$ODO.Psat.mn / 754.0 * CRREL_SALCHA$pressure.air.mmHg
# 
# 
# 

# CRREL_SALCHA$ODO.Psat.new.filled <- na_kalman(CRREL_SALCHA$ODO.Psat.new, type = "level")

# 
# 
# 
# CRREL_SALCHA %>% filter(datetimeAK >= "2021-07-01 00:00:00" & datetimeAK <= "2021-08-01 00:00:00" ) %>% ggplot(aes(x=solar.time)) + geom_line(aes(y=ODO.Psat.new), colour= "red")


# CRREL_SALCHA %>% filter(datetimeAK >= "2021-07-01 00:00:00" & datetimeAK <= "2021-08-01 00:00:00" ) %>% ggplot(aes(x=solar.time)) + geom_line(aes(y=DO.sat), colour= "red") + geom_line(aes(y = DO.sat2), colour = 'blue')+ labs(colour="Datasets")

CRREL_SALCHA <- merge(CRREL_SALCHA,dischargeSalcha, by = "datetimeAK")

final.salcha.DT <- (CRREL_SALCHA) %>%
  select(discharge, ParMetCor, temp.water, DO.sat,DO.obs,depth,solar.time)

final.salcha.DT <- final.salcha.DT %>% rename(light = ParMetCor)


final.salcha.DT <- na.omit(final.salcha.DT)

str(final.salcha.DT)


save <- final.salcha.DT

write.csv(save, "C:/Users/jacob/OneDrive - University of Alaska/GitHub/UAF-Metabolism/outputs/salcha2021.comb")
# write.csv(final.salcha.DT, here("UAF-Metabolism", "outputs", "correctedSalchaDT.csv"))


final.salcha.DT <- final.salcha.DT %>% 
  filter(as.character(solar.time) >= "2021-05-18 02:13:57") %>% filter(as.character(solar.time) <"2021-07-01 02:13:57")

final.salcha.DT$solar.time <- as.POSIXct(final.salcha.DT$solar.time)

str(final.salcha.DT)



# 2. Configuring the model


bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)
bayes_name

bayes_name_AppEtAL <- "b_Kb_oipi_tr_plrckm.stan"
bayes_name_AppEtAL


bayes_specs <- specs(bayes_name, 
                     burnin_steps=13000, saved_steps=2000, n_cores=8, n_chains = 4, GPP_daily_lower = 0, ER_daily_upper = 0
)


data.salcha.mm.all <- final.salcha.DT

data.salcha.mm.all$light <- signif(data.salcha.mm.all$light, digits = 4)



startTime <- as.POSIXct(Sys.time())
mm.test.salcha <- metab(bayes_specs, data=data.salcha.mm.all)
endTime <- as.POSIXct(Sys.time())

difftime(endTime, startTime)
save(mm.test.salcha, file = ("C:/Users/jacob/OneDrive - University of Alaska/GitHub/UAF-Metabolism/Outputs/salcha.2019.1.RData"))
load(here("Outputs", "salcha.2019.1.RData"))

fit.salcha <- get_fit(mm.test.salcha)
fit.daily <- fit.salcha$daily
write.csv(fit.daily, here("outputs", "salcha.2019.1.full.csv"))


#Plots
gpp <- ggplot(data=fit.salcha$daily, aes(x=date, y=GPP_mean)) + geom_point(color = "chartreuse4") + geom_errorbar(aes(ymin=GPP_mean-GPP_sd, ymax=GPP_mean+GPP_sd), width=.2, position=position_dodge(0.05), color = "chartreuse4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )

er <- ggplot(data = fit.daily, aes(x = date)) +geom_point(aes(y = ER_mean), color = "firebrick3") + geom_errorbar(aes(ymin=ER_mean-ER_sd, ymax=ER_mean+ER_sd), width=.2, position=position_dodge(0.05), color = "firebrick3")

k600 <- ggplot(data=fit.salcha$daily, aes(x=date, y=K600_daily_mean)) + geom_point(color = "orange") + geom_errorbar(aes(ymin=K600_daily_mean-K600_daily_sd, ymax=K600_daily_mean+K600_daily_sd), width=.2, position=position_dodge(0.05), color = "orange") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )

rhat <- ggplot(data=fit.salcha$daily, aes(x=date)) + geom_point(aes(y=GPP_Rhat, colour = "GPP")) + geom_point(aes(y=as.numeric(ER_Rhat), colour = "ER")) + geom_point(aes(y=K600_daily_Rhat, colour = "K600")) +
  labs(y = "RHAT") + theme(legend.position="top") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )


ggplot(data=fit.daily, aes(x=GPP_mean, y=ER_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq() 
ggplot(data=fit.daily, aes(x=GPP_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq() 

ggplot(data=fit.daily, aes(x=ER_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq() 
dev.set(dev.next())

mcmc <- get_mcmc(mm.test.salcha)
k600mcmc <- rstan::traceplot(mcmc, pars='K600_daily', nrow=3)
gppmcmc <- rstan::traceplot(mcmc, pars='GPP_daily', nrow=3)
ermcmc <- rstan::traceplot(mcmc, pars='ER_daily', nrow=3)

library(grid)

grid.newpage()
grid.draw(rbind(ggplotGrob(k600mcmc), ggplotGrob(gppmcmc), ggplotGrob(ermcmc)))


#DO r2
salcha.DO.pred <- predict_DO(mm.test.salcha)
salcha.DO.pred <- na.omit(salcha.DO.pred)
lm(DO.obs~ DO.mod, data = salcha.DO.pred)
salcha.DO.pred$SM.date <- as.Date(salcha.DO.pred$solar.time - 4*60*60)
salcha.DO.pred$SM.date <- as.factor(salcha.DO.pred$SM.date)
fit_model <- function(x, y) summary(lm(x ~ y))$adj.r.squared
test.run <- salcha.DO.pred %>% group_by(SM.date) %>% summarise(adj.R2 = fit_model(DO.obs,DO.mod))
salcha.DO.pred <- full_join(salcha.DO.pred, test.run)
rsq <-  summary(lm(salcha.DO.pred$DO.mod~salcha.DO.pred$DO.obs))$r.squared
library(ggplot2)
p1 <- ggplot(salcha.DO.pred, aes(x = solar.time)) + geom_point(aes(y=DO.obs, colour = "Observed DO"), color = "darkcyan") + geom_line(aes(y = DO.mod, colour= "Modeled DO"), color = "blue4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() ) + labs(colour="Green", y = "DO (mg/L)") +  theme(legend.position="none") + ggtitle("Observed (points) and Modeled (lines)")
p2 <- ggplot(salcha.DO.pred, aes(x = solar.time)) + geom_point(aes(y=adj.R2))
q.fig <- ggplot(data = data.salcha.mm.all, aes(x = solar.time)) +geom_point(aes(y=discharge))


grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(rhat),ggplotGrob(q.fig), ggplotGrob(k600),ggplotGrob(gpp), ggplotGrob(er)))


# rm(list = ls())




