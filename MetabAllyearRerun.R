


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

######## strt
data.strt.mm.all <- read.csv(here("outputs", "clean.strt.2019.full.csv"))


#change Discharge to m3/s
data.strt.mm.all$discharge <- data.strt.mm.all$discharge / 1000


bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name,
                     burnin_steps=8000, saved_steps=1500, n_cores=4,
                     GPP_daily_lower = 0, ER_daily_upper = 0,
                     
)


#Change light to modeled light
data.strt.mm.all$solar.time <- as.POSIXct(data.strt.mm.all$solar.time, tz = "UTC")

data.strt.mm.all$light <-  calc_light(data.strt.mm.all$solar.time, 64.75408, -146.479933)



data.strt.mm.all <- data.strt.mm.all %>%  rename(DO.sat = DO.sat.EXO)
data.strt.mm.all <- data.strt.mm.all %>% select(solar.time, DO.obs, DO.sat, light, discharge, depth, temp.water)

startTime <- as.POSIXct(Sys.time())
mm.test.strt <- metab(bayes_specs, data=data.strt.mm.all)
endTime <- as.POSIXct(Sys.time())
# 
difftime(endTime, startTime)
save(mm.test.strt, file = here("Outputs", "stuart2019-Run_2023-Full.rerun.02.04.RData"))
# 
fit.strt <- get_fit(mm.test.strt)
fit.daily <- fit.strt$daily
write.csv(fit.daily, here("outputs", "stuart2019-Run_2023-Full.rerun.02.04.csv"))

model_data <- get_data(mm.test.strt)

######### Plot


gpp <- ggplot(data=fit.daily, aes(x=date, y=GPP_mean)) + geom_point(color = "chartreuse4") + geom_errorbar(aes(ymin=GPP_mean-GPP_sd, ymax=GPP_mean+GPP_sd), width=.2, position=position_dodge(0.05), color = "chartreuse4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ ggtitle("stuart2019")+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) +ylim(0, max(fit.daily$GPP_mean))

er <- ggplot(data = fit.daily, aes(x = date)) +geom_point(aes(y = ER_mean), color = "firebrick3") + geom_errorbar(aes(ymin=ER_mean-ER_sd, ymax=ER_mean+ER_sd), width=.2, position=position_dodge(0.05), color = "firebrick3")+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20)) +ylim(min(fit.daily$ER_daily_mean), 0)

k600 <- ggplot(data=fit.daily, aes(x=date, y=K600_daily_mean)) + geom_point(color = "orange") + geom_errorbar(aes(ymin=K600_daily_mean-K600_daily_sd, ymax=K600_daily_mean+K600_daily_sd), width=.2, position=position_dodge(0.05), color = "orange") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) 

rhat <- ggplot(data=fit.daily, aes(x=date)) + geom_point(aes(y=GPP_Rhat, colour = "GPP")) + geom_point(aes(y=as.numeric(ER_Rhat), colour = "ER")) + geom_point(aes(y=K600_daily_Rhat, colour = "K600")) +
  labs(y = "RHAT") + theme(legend.position="top") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ geom_hline(yintercept=1.01, color = "dark blue")+ geom_hline(yintercept=1.1, color = "dark red", linetype = "dashed")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))

er_k <- ggplot(data=fit.daily, aes(x=ER_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))


q.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=discharge), color = "sienna4", size=0.4)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("Discharge ( ", m^3, s^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ylim(0, max(model_data$discharge))

T.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=temp.water), color = "slateblue4", size=0.4)+ theme(axis.title.x=element_blank() )+ labs(y = "Temperature (°C)")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+ylim(-0.004, 20)



strt.DO.pred <- predict_DO(mm.test.strt)
strt.DO.pred <- na.omit(strt.DO.pred)
lm(DO.obs~ DO.mod, data = strt.DO.pred)
strt.DO.pred$SM.date <- as.Date(strt.DO.pred$solar.time - 4*60*60)
strt.DO.pred$SM.date <- as.factor(strt.DO.pred$SM.date)
fit_model <- function(x, y) summary(lm(x ~ y))$adj.r.squared
test.run <- strt.DO.pred %>% group_by(SM.date) %>% summarise(adj.R2 = fit_model(DO.obs,DO.mod))
strt.DO.pred <- full_join(strt.DO.pred, test.run)
rsq <-  summary(lm(strt.DO.pred$DO.mod~strt.DO.pred$DO.obs))$r.squared

p1 <- ggplot(strt.DO.pred, aes(x = solar.time)) + geom_point(aes(y=DO.obs, colour = "Observed DO"), color = "darkcyan") + geom_line(aes(y = DO.mod, colour= "Modeled DO"), color = "blue4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() ) + labs(colour="Green", y = "DO (mg/L)") +  theme(legend.position="none") + ggtitle("Observed (points) and Modeled (lines)")

p2 <- ggplot(strt.DO.pred, aes(x = solar.time)) + geom_point(aes(y=adj.R2))


library(grid)
library(ggeffects)
library(gridExtra)
library(gtable)
# If that doesnt do it, add grid
# Make each of the plots

# Two-panel figure: effect sizes
panA <- ggplotGrob(gpp)
panB <- ggplotGrob(er)
panK600 <- ggplotGrob(k600)
panC <- ggplotGrob(q.fig)
panD <- ggplotGrob(T.fig)
panE <- ggplotGrob(p1)
panp2 <- ggplotGrob(p2)
panF <- ggplotGrob(rhat)
panG <- ggplotGrob(er_k)
grid::grid.newpage()
grid.draw(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF, panG,size="max"))
fig2019 <- arrangeGrob(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF,panG, size="max"))
ggsave(path = here("outputs"), file = "strt2019.pdf", fig2019, width = 30, height = 55, units = "in", limitsize = FALSE)













rm(list=ls())











#STRT

#STRT




######### strt
data.strt.mm.all <- read.csv(here("outputs", "clean.strt.2020.full.csv"))


#change Discharge to m3/s
data.strt.mm.all$discharge <- data.strt.mm.all$discharge / 1000

bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name,
                     burnin_steps=8000, saved_steps=1500, n_cores=4,
                     GPP_daily_lower = 0, ER_daily_upper = 0,
                     
)


#Change light to modeled light
data.strt.mm.all$solar.time <- as.POSIXct(data.strt.mm.all$solar.time, tz = "UTC")

data.strt.mm.all$light <-  calc_light(data.strt.mm.all$solar.time, 64.75408, -146.479933)



data.strt.mm.all <- data.strt.mm.all %>%  rename(DO.sat = DO.sat.EXO)
data.strt.mm.all <- data.strt.mm.all %>% select(solar.time, DO.obs, DO.sat, light, discharge, depth, temp.water)

startTime <- as.POSIXct(Sys.time())
mm.test.strt <- metab(bayes_specs, data=data.strt.mm.all)
endTime <- as.POSIXct(Sys.time())
# 
difftime(endTime, startTime)
save(mm.test.strt, file = here("Outputs", "stuart2020-Run_2023-Full.rerun.02.04.RData"))
# 
fit.strt <- get_fit(mm.test.strt)
fit.daily <- fit.strt$daily
write.csv(fit.daily, here("outputs", "stuart2020-Run_2023-Full.rerun.02.04.csv"))

model_data <- get_data(mm.test.strt)

######### Plot


gpp <- ggplot(data=fit.daily, aes(x=date, y=GPP_mean)) + geom_point(color = "chartreuse4") + geom_errorbar(aes(ymin=GPP_mean-GPP_sd, ymax=GPP_mean+GPP_sd), width=.2, position=position_dodge(0.05), color = "chartreuse4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ ggtitle("stuart2020")+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) +ylim(0, max(fit.daily$GPP_mean))

er <- ggplot(data = fit.daily, aes(x = date)) +geom_point(aes(y = ER_mean), color = "firebrick3") + geom_errorbar(aes(ymin=ER_mean-ER_sd, ymax=ER_mean+ER_sd), width=.2, position=position_dodge(0.05), color = "firebrick3")+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20)) +ylim(min(fit.daily$ER_daily_mean), 0)

k600 <- ggplot(data=fit.daily, aes(x=date, y=K600_daily_mean)) + geom_point(color = "orange") + geom_errorbar(aes(ymin=K600_daily_mean-K600_daily_sd, ymax=K600_daily_mean+K600_daily_sd), width=.2, position=position_dodge(0.05), color = "orange") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) 

rhat <- ggplot(data=fit.daily, aes(x=date)) + geom_point(aes(y=GPP_Rhat, colour = "GPP")) + geom_point(aes(y=as.numeric(ER_Rhat), colour = "ER")) + geom_point(aes(y=K600_daily_Rhat, colour = "K600")) +
  labs(y = "RHAT") + theme(legend.position="top") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ geom_hline(yintercept=1.01, color = "dark blue")+ geom_hline(yintercept=1.1, color = "dark red", linetype = "dashed")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))

er_k <- ggplot(data=fit.daily, aes(x=ER_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))


q.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=discharge), color = "sienna4", size=0.4)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("Discharge ( ", m^3, s^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ylim(0, max(model_data$discharge))

T.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=temp.water), color = "slateblue4", size=0.4)+ theme(axis.title.x=element_blank() )+ labs(y = "Temperature (°C)")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+ylim(-0.004, 20)



strt.DO.pred <- predict_DO(mm.test.strt)
strt.DO.pred <- na.omit(strt.DO.pred)
lm(DO.obs~ DO.mod, data = strt.DO.pred)
strt.DO.pred$SM.date <- as.Date(strt.DO.pred$solar.time - 4*60*60)
strt.DO.pred$SM.date <- as.factor(strt.DO.pred$SM.date)
fit_model <- function(x, y) summary(lm(x ~ y))$adj.r.squared
test.run <- strt.DO.pred %>% group_by(SM.date) %>% summarise(adj.R2 = fit_model(DO.obs,DO.mod))
strt.DO.pred <- full_join(strt.DO.pred, test.run)
rsq <-  summary(lm(strt.DO.pred$DO.mod~strt.DO.pred$DO.obs))$r.squared

p1 <- ggplot(strt.DO.pred, aes(x = solar.time)) + geom_point(aes(y=DO.obs, colour = "Observed DO"), color = "darkcyan") + geom_line(aes(y = DO.mod, colour= "Modeled DO"), color = "blue4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() ) + labs(colour="Green", y = "DO (mg/L)") +  theme(legend.position="none") + ggtitle("Observed (points) and Modeled (lines)")

p2 <- ggplot(strt.DO.pred, aes(x = solar.time)) + geom_point(aes(y=adj.R2))


library(grid)
library(ggeffects)
library(gridExtra)
library(gtable)
# If that doesnt do it, add grid
# Make each of the plots

# Two-panel figure: effect sizes
panA <- ggplotGrob(gpp)
panB <- ggplotGrob(er)
panK600 <- ggplotGrob(k600)
panC <- ggplotGrob(q.fig)
panD <- ggplotGrob(T.fig)
panE <- ggplotGrob(p1)
panp2 <- ggplotGrob(p2)
panF <- ggplotGrob(rhat)
panG <- ggplotGrob(er_k)
grid::grid.newpage()
grid.draw(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF, panG,size="max"))
fig2020 <- arrangeGrob(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF,panG, size="max"))
ggsave(path = here("outputs"), file = "strt2020.pdf", fig2020, width = 30, height = 55, units = "in", limitsize = FALSE)












rm(list=ls())










######### strt
data.strt.mm.all <- read.csv(here("outputs", "clean.strt.2021.full.csv"))


#change Discharge to m3/s
data.strt.mm.all$discharge <- data.strt.mm.all$discharge / 1000


bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name,
                     burnin_steps=8000, saved_steps=1500, n_cores=4,
                     GPP_daily_lower = 0, ER_daily_upper = 0,
                     
)


#Change light to modeled light
data.strt.mm.all$solar.time <- as.POSIXct(data.strt.mm.all$solar.time, tz = "UTC")

data.strt.mm.all$light <-  calc_light(data.strt.mm.all$solar.time, 64.75408, -146.479933)



data.strt.mm.all <- data.strt.mm.all %>%  rename(DO.sat = DO.sat.EXO)
data.strt.mm.all <- data.strt.mm.all %>% select(solar.time, DO.obs, DO.sat, light, discharge, depth, temp.water)

startTime <- as.POSIXct(Sys.time())
mm.test.strt <- metab(bayes_specs, data=data.strt.mm.all)
endTime <- as.POSIXct(Sys.time())
# 
difftime(endTime, startTime)
save(mm.test.strt, file = here("Outputs", "stuart2021-Run_2023-Full.rerun.02.04.RData"))
# 
fit.strt <- get_fit(mm.test.strt)
fit.daily <- fit.strt$daily
write.csv(fit.daily, here("outputs", "stuart2021-Run_2023-Full.rerun.02.04.csv"))

model_data <- get_data(mm.test.strt)

######### Plot


gpp <- ggplot(data=fit.daily, aes(x=date, y=GPP_mean)) + geom_point(color = "chartreuse4") + geom_errorbar(aes(ymin=GPP_mean-GPP_sd, ymax=GPP_mean+GPP_sd), width=.2, position=position_dodge(0.05), color = "chartreuse4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ ggtitle("stuart2021")+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) +ylim(0, max(fit.daily$GPP_mean))

er <- ggplot(data = fit.daily, aes(x = date)) +geom_point(aes(y = ER_mean), color = "firebrick3") + geom_errorbar(aes(ymin=ER_mean-ER_sd, ymax=ER_mean+ER_sd), width=.2, position=position_dodge(0.05), color = "firebrick3")+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20)) +ylim(min(fit.daily$ER_daily_mean), 0)

k600 <- ggplot(data=fit.daily, aes(x=date, y=K600_daily_mean)) + geom_point(color = "orange") + geom_errorbar(aes(ymin=K600_daily_mean-K600_daily_sd, ymax=K600_daily_mean+K600_daily_sd), width=.2, position=position_dodge(0.05), color = "orange") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) 

rhat <- ggplot(data=fit.daily, aes(x=date)) + geom_point(aes(y=GPP_Rhat, colour = "GPP")) + geom_point(aes(y=as.numeric(ER_Rhat), colour = "ER")) + geom_point(aes(y=K600_daily_Rhat, colour = "K600")) +
  labs(y = "RHAT") + theme(legend.position="top") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ geom_hline(yintercept=1.01, color = "dark blue")+ geom_hline(yintercept=1.1, color = "dark red", linetype = "dashed")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))

er_k <- ggplot(data=fit.daily, aes(x=ER_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))


q.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=discharge), color = "sienna4", size=0.4)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("Discharge ( ", m^3, s^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ylim(0, max(model_data$discharge))

T.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=temp.water), color = "slateblue4", size=0.4)+ theme(axis.title.x=element_blank() )+ labs(y = "Temperature (°C)")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+ylim(-0.004, 20)



strt.DO.pred <- predict_DO(mm.test.strt)
strt.DO.pred <- na.omit(strt.DO.pred)
lm(DO.obs~ DO.mod, data = strt.DO.pred)
strt.DO.pred$SM.date <- as.Date(strt.DO.pred$solar.time - 4*60*60) 
strt.DO.pred$SM.date <- as.factor(strt.DO.pred$SM.date)
fit_model <- function(x, y) summary(lm(x ~ y))$adj.r.squared
test.run <- strt.DO.pred %>% group_by(SM.date) %>% summarise(adj.R2 = fit_model(DO.obs,DO.mod))
strt.DO.pred <- full_join(strt.DO.pred, test.run)
rsq <-  summary(lm(strt.DO.pred$DO.mod~strt.DO.pred$DO.obs))$r.squared

p1 <- ggplot(strt.DO.pred, aes(x = solar.time)) + geom_point(aes(y=DO.obs, colour = "Observed DO"), color = "darkcyan") + geom_line(aes(y = DO.mod, colour= "Modeled DO"), color = "blue4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() ) + labs(colour="Green", y = "DO (mg/L)") +  theme(legend.position="none") + ggtitle("Observed (points) and Modeled (lines)")

p2 <- ggplot(strt.DO.pred, aes(x = solar.time)) + geom_point(aes(y=adj.R2))


library(grid)
library(ggeffects)
library(gridExtra)
library(gtable)
# If that doesnt do it, add grid
# Make each of the plots

# Two-panel figure: effect sizes
panA <- ggplotGrob(gpp)
panB <- ggplotGrob(er)
panK600 <- ggplotGrob(k600)
panC <- ggplotGrob(q.fig)
panD <- ggplotGrob(T.fig)
panE <- ggplotGrob(p1)
panp2 <- ggplotGrob(p2)
panF <- ggplotGrob(rhat)
panG <- ggplotGrob(er_k)
grid::grid.newpage()
grid.draw(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF, panG,size="max"))
fig2021 <- arrangeGrob(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF,panG, size="max"))
ggsave(path = here("outputs"), file = "strt2021.pdf", fig2021, width = 30, height = 55, units = "in", limitsize = FALSE)








rm(list=ls())



#STRT



######### strt
data.strt.mm.all <- read.csv(here("outputs", "clean.strt.2022.full.csv"))


#change Discharge to m3/s
data.strt.mm.all$discharge <- data.strt.mm.all$discharge / 1000

bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name,
                     burnin_steps=8000, saved_steps=1500, n_cores=4,
                     GPP_daily_lower = 0, ER_daily_upper = 0,
                     
)


#Change light to modeled light
data.strt.mm.all$solar.time <- as.POSIXct(data.strt.mm.all$solar.time, tz = "UTC")

data.strt.mm.all$light <-  calc_light(data.strt.mm.all$solar.time, 64.75408, -146.479933)



data.strt.mm.all <- data.strt.mm.all %>%  rename(DO.sat = DO.sat.EXO)
data.strt.mm.all <- data.strt.mm.all %>% select(solar.time, DO.obs, DO.sat, light, discharge, depth, temp.water)

startTime <- as.POSIXct(Sys.time())
mm.test.strt <- metab(bayes_specs, data=data.strt.mm.all)
endTime <- as.POSIXct(Sys.time())
# 
difftime(endTime, startTime)
save(mm.test.strt, file = here("Outputs", "stuart2022-Run_2023-Full.rerun.02.04.RData"))
# 
fit.strt <- get_fit(mm.test.strt)
fit.daily <- fit.strt$daily
write.csv(fit.daily, here("outputs", "stuart2022-Run_2023-Full.rerun.02.04.csv"))

model_data <- get_data(mm.test.strt)

######### Plot


gpp <- ggplot(data=fit.daily, aes(x=date, y=GPP_mean)) + geom_point(color = "chartreuse4") + geom_errorbar(aes(ymin=GPP_mean-GPP_sd, ymax=GPP_mean+GPP_sd), width=.2, position=position_dodge(0.05), color = "chartreuse4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ ggtitle("stuart2022")+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) +ylim(0, max(fit.daily$GPP_mean))

er <- ggplot(data = fit.daily, aes(x = date)) +geom_point(aes(y = ER_mean), color = "firebrick3") + geom_errorbar(aes(ymin=ER_mean-ER_sd, ymax=ER_mean+ER_sd), width=.2, position=position_dodge(0.05), color = "firebrick3")+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20)) +ylim(min(fit.daily$ER_daily_mean), 0)

k600 <- ggplot(data=fit.daily, aes(x=date, y=K600_daily_mean)) + geom_point(color = "orange") + geom_errorbar(aes(ymin=K600_daily_mean-K600_daily_sd, ymax=K600_daily_mean+K600_daily_sd), width=.2, position=position_dodge(0.05), color = "orange") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) 

rhat <- ggplot(data=fit.daily, aes(x=date)) + geom_point(aes(y=GPP_Rhat, colour = "GPP")) + geom_point(aes(y=as.numeric(ER_Rhat), colour = "ER")) + geom_point(aes(y=K600_daily_Rhat, colour = "K600")) +
  labs(y = "RHAT") + theme(legend.position="top") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ geom_hline(yintercept=1.01, color = "dark blue")+ geom_hline(yintercept=1.1, color = "dark red", linetype = "dashed")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))

er_k <- ggplot(data=fit.daily, aes(x=ER_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))


q.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=discharge), color = "sienna4", size=0.4)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("Discharge ( ", m^3, s^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ylim(0, max(model_data$discharge))

T.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=temp.water), color = "slateblue4", size=0.4)+ theme(axis.title.x=element_blank() )+ labs(y = "Temperature (°C)")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+ylim(-0.004, 20)



strt.DO.pred <- predict_DO(mm.test.strt)
strt.DO.pred <- na.omit(strt.DO.pred)
lm(DO.obs~ DO.mod, data = strt.DO.pred)
strt.DO.pred$SM.date <- as.Date(strt.DO.pred$solar.time - 4*60*60)
strt.DO.pred$SM.date <- as.factor(strt.DO.pred$SM.date)
fit_model <- function(x, y) summary(lm(x ~ y))$adj.r.squared
test.run <- strt.DO.pred %>% group_by(SM.date) %>% summarise(adj.R2 = fit_model(DO.obs,DO.mod))
strt.DO.pred <- full_join(strt.DO.pred, test.run)
rsq <-  summary(lm(strt.DO.pred$DO.mod~strt.DO.pred$DO.obs))$r.squared

p1 <- ggplot(strt.DO.pred, aes(x = solar.time)) + geom_point(aes(y=DO.obs, colour = "Observed DO"), color = "darkcyan") + geom_line(aes(y = DO.mod, colour= "Modeled DO"), color = "blue4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() ) + labs(colour="Green", y = "DO (mg/L)") +  theme(legend.position="none") + ggtitle("Observed (points) and Modeled (lines)")

p2 <- ggplot(strt.DO.pred, aes(x = solar.time)) + geom_point(aes(y=adj.R2))


library(grid)
library(ggeffects)
library(gridExtra)
library(gtable)
# If that doesnt do it, add grid
# Make each of the plots

# Two-panel figure: effect sizes
panA <- ggplotGrob(gpp)
panB <- ggplotGrob(er)
panK600 <- ggplotGrob(k600)
panC <- ggplotGrob(q.fig)
panD <- ggplotGrob(T.fig)
panE <- ggplotGrob(p1)
panp2 <- ggplotGrob(p2)
panF <- ggplotGrob(rhat)
panG <- ggplotGrob(er_k)
grid::grid.newpage()
grid.draw(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF, panG,size="max"))
fig2022 <- arrangeGrob(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF,panG, size="max"))
ggsave(path = here("outputs"), file = "strt2022.pdf", fig2022, width = 30, height = 55, units = "in", limitsize = FALSE)

rm(list=ls())



######## vaul
data.vaul.mm.all <- read.csv(here("outputs", "clean.vaul.2019.full.csv"))


#change Discharge to m3/s
data.vaul.mm.all$discharge <- data.vaul.mm.all$discharge / 1000

bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name,
                     burnin_steps=8000, saved_steps=1500, n_cores=4,
                     GPP_daily_lower = 0, ER_daily_upper = 0,
                     
)
#Change light to modeled light
data.vaul.mm.all$solar.time <- as.POSIXct(data.vaul.mm.all$solar.time, tz = "UTC")

data.vaul.mm.all$light <-  calc_light(data.vaul.mm.all$solar.time, 65.029315, -147.714949)



data.vaul.mm.all <- data.vaul.mm.all %>%  rename(DO.sat = DO.sat.EXO)
data.vaul.mm.all <- data.vaul.mm.all %>% select(solar.time, DO.obs, DO.sat, light, discharge, depth, temp.water)

startTime <- as.POSIXct(Sys.time())
mm.test.vaul <- metab(bayes_specs, data=data.vaul.mm.all)
endTime <- as.POSIXct(Sys.time())
# 
difftime(endTime, startTime)
save(mm.test.vaul, file = here("Outputs", "vault2019-Run_2023-Full.rerun.02.04.RData"))
# 
fit.vaul <- get_fit(mm.test.vaul)
fit.daily <- fit.vaul$daily
write.csv(fit.daily, here("outputs", "vault2019-Run_2023-Full.rerun.02.04.csv"))

model_data <- get_data(mm.test.vaul)

######### Plot


gpp <- ggplot(data=fit.daily, aes(x=date, y=GPP_mean)) + geom_point(color = "chartreuse4") + geom_errorbar(aes(ymin=GPP_mean-GPP_sd, ymax=GPP_mean+GPP_sd), width=.2, position=position_dodge(0.05), color = "chartreuse4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ ggtitle("vault2019")+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) +ylim(0, max(fit.daily$GPP_mean))

er <- ggplot(data = fit.daily, aes(x = date)) +geom_point(aes(y = ER_mean), color = "firebrick3") + geom_errorbar(aes(ymin=ER_mean-ER_sd, ymax=ER_mean+ER_sd), width=.2, position=position_dodge(0.05), color = "firebrick3")+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20)) +ylim(min(fit.daily$ER_daily_mean), 0)

k600 <- ggplot(data=fit.daily, aes(x=date, y=K600_daily_mean)) + geom_point(color = "orange") + geom_errorbar(aes(ymin=K600_daily_mean-K600_daily_sd, ymax=K600_daily_mean+K600_daily_sd), width=.2, position=position_dodge(0.05), color = "orange") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) 

rhat <- ggplot(data=fit.daily, aes(x=date)) + geom_point(aes(y=GPP_Rhat, colour = "GPP")) + geom_point(aes(y=as.numeric(ER_Rhat), colour = "ER")) + geom_point(aes(y=K600_daily_Rhat, colour = "K600")) +
  labs(y = "RHAT") + theme(legend.position="top") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ geom_hline(yintercept=1.01, color = "dark blue")+ geom_hline(yintercept=1.1, color = "dark red", linetype = "dashed")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))

er_k <- ggplot(data=fit.daily, aes(x=ER_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))


q.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=discharge), color = "sienna4", size=0.4)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("Discharge ( ", m^3, s^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ylim(0, max(model_data$discharge))

T.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=temp.water), color = "slateblue4", size=0.4)+ theme(axis.title.x=element_blank() )+ labs(y = "Temperature (°C)")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+ylim(-0.004, 20)



vaul.DO.pred <- predict_DO(mm.test.vaul)
vaul.DO.pred <- na.omit(vaul.DO.pred)
lm(DO.obs~ DO.mod, data = vaul.DO.pred)
vaul.DO.pred$SM.date <- as.Date(vaul.DO.pred$solar.time - 4*60*60)
vaul.DO.pred$SM.date <- as.factor(vaul.DO.pred$SM.date)
fit_model <- function(x, y) summary(lm(x ~ y))$adj.r.squared
test.run <- vaul.DO.pred %>% group_by(SM.date) %>% summarise(adj.R2 = fit_model(DO.obs,DO.mod))
vaul.DO.pred <- full_join(vaul.DO.pred, test.run)
rsq <-  summary(lm(vaul.DO.pred$DO.mod~vaul.DO.pred$DO.obs))$r.squared

p1 <- ggplot(vaul.DO.pred, aes(x = solar.time)) + geom_point(aes(y=DO.obs, colour = "Observed DO"), color = "darkcyan") + geom_line(aes(y = DO.mod, colour= "Modeled DO"), color = "blue4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() ) + labs(colour="Green", y = "DO (mg/L)") +  theme(legend.position="none") + ggtitle("Observed (points) and Modeled (lines)")

p2 <- ggplot(vaul.DO.pred, aes(x = solar.time)) + geom_point(aes(y=adj.R2))



library(grid)
library(ggeffects)
library(gridExtra)
library(gtable)
# If that doesnt do it, add grid
# Make each of the plots

# Two-panel figure: effect sizes
panA <- ggplotGrob(gpp)
panB <- ggplotGrob(er)
panK600 <- ggplotGrob(k600)
panC <- ggplotGrob(q.fig)
panD <- ggplotGrob(T.fig)
panE <- ggplotGrob(p1)
panp2 <- ggplotGrob(p2)
panF <- ggplotGrob(rhat)
panG <- ggplotGrob(er_k)
grid::grid.newpage()
grid.draw(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF, panG,size="max"))
fig2019 <- arrangeGrob(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF,panG, size="max"))
ggsave(path = here("outputs"), file = "vaul2019.pdf", fig2019, width = 30, height = 55, units = "in", limitsize = FALSE)





















rm(list=ls())



#VAUL

#VAUL




######### vaul
data.vaul.mm.all <- na.omit(read.csv(here("outputs", "clean.vaul.2020.full.csv")))


#change Discharge to m3/s
data.vaul.mm.all$discharge <- data.vaul.mm.all$discharge / 1000

bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name,
                     burnin_steps=8000, saved_steps=1500, n_cores=4,
                     GPP_daily_lower = 0, ER_daily_upper = 0,
                     
)


#Change light to modeled light
data.vaul.mm.all$solar.time <- as.POSIXct(data.vaul.mm.all$solar.time, tz = "UTC")

data.vaul.mm.all$light <-  calc_light(data.vaul.mm.all$solar.time, 65.029315, -147.714949)



data.vaul.mm.all <- data.vaul.mm.all %>%  rename(DO.sat = DO.sat.EXO)
data.vaul.mm.all <- data.vaul.mm.all %>% select(solar.time, DO.obs, DO.sat, light, discharge, depth, temp.water)

startTime <- as.POSIXct(Sys.time())
mm.test.vaul <- metab(bayes_specs, data=data.vaul.mm.all)
endTime <- as.POSIXct(Sys.time())
# 
difftime(endTime, startTime)
save(mm.test.vaul, file = here("Outputs", "vault2020-Run_2023-Full.rerun.02.04.RData"))
# 
fit.vaul <- get_fit(mm.test.vaul)
fit.daily <- fit.vaul$daily
write.csv(fit.daily, here("outputs", "vault2020-Run_2023-Full.rerun.02.04.csv"))

model_data <- get_data(mm.test.vaul)

######### Plot


gpp <- ggplot(data=fit.daily, aes(x=date, y=GPP_mean)) + geom_point(color = "chartreuse4") + geom_errorbar(aes(ymin=GPP_mean-GPP_sd, ymax=GPP_mean+GPP_sd), width=.2, position=position_dodge(0.05), color = "chartreuse4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ ggtitle("vault2020")+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) +ylim(0, max(fit.daily$GPP_mean))

er <- ggplot(data = fit.daily, aes(x = date)) +geom_point(aes(y = ER_mean), color = "firebrick3") + geom_errorbar(aes(ymin=ER_mean-ER_sd, ymax=ER_mean+ER_sd), width=.2, position=position_dodge(0.05), color = "firebrick3")+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20)) +ylim(min(fit.daily$ER_daily_mean), 0)

k600 <- ggplot(data=fit.daily, aes(x=date, y=K600_daily_mean)) + geom_point(color = "orange") + geom_errorbar(aes(ymin=K600_daily_mean-K600_daily_sd, ymax=K600_daily_mean+K600_daily_sd), width=.2, position=position_dodge(0.05), color = "orange") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) 

rhat <- ggplot(data=fit.daily, aes(x=date)) + geom_point(aes(y=GPP_Rhat, colour = "GPP")) + geom_point(aes(y=as.numeric(ER_Rhat), colour = "ER")) + geom_point(aes(y=K600_daily_Rhat, colour = "K600")) +
  labs(y = "RHAT") + theme(legend.position="top") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ geom_hline(yintercept=1.01, color = "dark blue")+ geom_hline(yintercept=1.1, color = "dark red", linetype = "dashed")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))

er_k <- ggplot(data=fit.daily, aes(x=ER_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))


q.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=discharge), color = "sienna4", size=0.4)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("Discharge ( ", m^3, s^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ylim(0, max(model_data$discharge))

T.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=temp.water), color = "slateblue4", size=0.4)+ theme(axis.title.x=element_blank() )+ labs(y = "Temperature (°C)")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+ylim(-0.004, 20)



vaul.DO.pred <- predict_DO(mm.test.vaul)
vaul.DO.pred <- na.omit(vaul.DO.pred)
lm(DO.obs~ DO.mod, data = vaul.DO.pred)
vaul.DO.pred$SM.date <- as.Date(vaul.DO.pred$solar.time - 4*60*60)
vaul.DO.pred$SM.date <- as.factor(vaul.DO.pred$SM.date)
fit_model <- function(x, y) summary(lm(x ~ y))$adj.r.squared
test.run <- vaul.DO.pred %>% group_by(SM.date) %>% summarise(adj.R2 = fit_model(DO.obs,DO.mod))
vaul.DO.pred <- full_join(vaul.DO.pred, test.run)
rsq <-  summary(lm(vaul.DO.pred$DO.mod~vaul.DO.pred$DO.obs))$r.squared

p1 <- ggplot(vaul.DO.pred, aes(x = solar.time)) + geom_point(aes(y=DO.obs, colour = "Observed DO"), color = "darkcyan") + geom_line(aes(y = DO.mod, colour= "Modeled DO"), color = "blue4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() ) + labs(colour="Green", y = "DO (mg/L)") +  theme(legend.position="none") + ggtitle("Observed (points) and Modeled (lines)")

p2 <- ggplot(vaul.DO.pred, aes(x = solar.time)) + geom_point(aes(y=adj.R2))


library(grid)
library(ggeffects)
library(gridExtra)
library(gtable)
# If that doesnt do it, add grid
# Make each of the plots

# Two-panel figure: effect sizes
panA <- ggplotGrob(gpp)
panB <- ggplotGrob(er)
panK600 <- ggplotGrob(k600)
panC <- ggplotGrob(q.fig)
panD <- ggplotGrob(T.fig)
panE <- ggplotGrob(p1)
panp2 <- ggplotGrob(p2)
panF <- ggplotGrob(rhat)
panG <- ggplotGrob(er_k)
grid::grid.newpage()
grid.draw(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF, panG,size="max"))
fig2020 <- arrangeGrob(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF,panG, size="max"))
ggsave(path = here("outputs"), file = "vaul2020.pdf", fig2020, width = 30, height = 55, units = "in", limitsize = FALSE)








rm(list=ls())














######### vaul
data.vaul.mm.all <- read.csv(here("outputs", "clean.vaul.2021.full.csv"))


#change Discharge to m3/s
data.vaul.mm.all$discharge <- data.vaul.mm.all$discharge / 1000

bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name,
                     burnin_steps=8000, saved_steps=1500, n_cores=4,
                     GPP_daily_lower = 0, ER_daily_upper = 0,
                     
)

#Change light to modeled light
data.vaul.mm.all$solar.time <- as.POSIXct(data.vaul.mm.all$solar.time, tz = "UTC")

data.vaul.mm.all$light <-  calc_light(data.vaul.mm.all$solar.time, 65.029315, -147.714949)



data.vaul.mm.all <- data.vaul.mm.all %>%  rename(DO.sat = DO.sat.EXO)
data.vaul.mm.all <- data.vaul.mm.all %>% select(solar.time, DO.obs, DO.sat, light, discharge, depth, temp.water)

startTime <- as.POSIXct(Sys.time())
mm.test.vaul <- metab(bayes_specs, data=data.vaul.mm.all)
endTime <- as.POSIXct(Sys.time())
# 
difftime(endTime, startTime)
save(mm.test.vaul, file = here("Outputs", "vault2021-Run_2023-Full.rerun.02.04.RData"))
# 
fit.vaul <- get_fit(mm.test.vaul)
fit.daily <- fit.vaul$daily
write.csv(fit.daily, here("outputs", "vault2021-Run_2023-Full.rerun.02.04.csv"))

model_data <- get_data(mm.test.vaul)

######### Plot


gpp <- ggplot(data=fit.daily, aes(x=date, y=GPP_mean)) + geom_point(color = "chartreuse4") + geom_errorbar(aes(ymin=GPP_mean-GPP_sd, ymax=GPP_mean+GPP_sd), width=.2, position=position_dodge(0.05), color = "chartreuse4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ ggtitle("vault2021")+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) +ylim(0, max(fit.daily$GPP_mean))

er <- ggplot(data = fit.daily, aes(x = date)) +geom_point(aes(y = ER_mean), color = "firebrick3") + geom_errorbar(aes(ymin=ER_mean-ER_sd, ymax=ER_mean+ER_sd), width=.2, position=position_dodge(0.05), color = "firebrick3")+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20)) +ylim(min(fit.daily$ER_daily_mean), 0)

k600 <- ggplot(data=fit.daily, aes(x=date, y=K600_daily_mean)) + geom_point(color = "orange") + geom_errorbar(aes(ymin=K600_daily_mean-K600_daily_sd, ymax=K600_daily_mean+K600_daily_sd), width=.2, position=position_dodge(0.05), color = "orange") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) 

rhat <- ggplot(data=fit.daily, aes(x=date)) + geom_point(aes(y=GPP_Rhat, colour = "GPP")) + geom_point(aes(y=as.numeric(ER_Rhat), colour = "ER")) + geom_point(aes(y=K600_daily_Rhat, colour = "K600")) +
  labs(y = "RHAT") + theme(legend.position="top") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ geom_hline(yintercept=1.01, color = "dark blue")+ geom_hline(yintercept=1.1, color = "dark red", linetype = "dashed")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))

er_k <- ggplot(data=fit.daily, aes(x=ER_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))


q.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=discharge), color = "sienna4", size=0.4)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("Discharge ( ", m^3, s^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ylim(0, max(model_data$discharge))

T.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=temp.water), color = "slateblue4", size=0.4)+ theme(axis.title.x=element_blank() )+ labs(y = "Temperature (°C)")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+ylim(-0.004, 20)



vaul.DO.pred <- predict_DO(mm.test.vaul)
vaul.DO.pred <- na.omit(vaul.DO.pred)
lm(DO.obs~ DO.mod, data = vaul.DO.pred)
vaul.DO.pred$SM.date <- as.Date(vaul.DO.pred$solar.time - 4*60*60)
vaul.DO.pred$SM.date <- as.factor(vaul.DO.pred$SM.date)
fit_model <- function(x, y) summary(lm(x ~ y))$adj.r.squared
test.run <- vaul.DO.pred %>% group_by(SM.date) %>% summarise(adj.R2 = fit_model(DO.obs,DO.mod))
vaul.DO.pred <- full_join(vaul.DO.pred, test.run)
rsq <-  summary(lm(vaul.DO.pred$DO.mod~vaul.DO.pred$DO.obs))$r.squared

p1 <- ggplot(vaul.DO.pred, aes(x = solar.time)) + geom_point(aes(y=DO.obs, colour = "Observed DO"), color = "darkcyan") + geom_line(aes(y = DO.mod, colour= "Modeled DO"), color = "blue4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() ) + labs(colour="Green", y = "DO (mg/L)") +  theme(legend.position="none") + ggtitle("Observed (points) and Modeled (lines)")

p2 <- ggplot(vaul.DO.pred, aes(x = solar.time)) + geom_point(aes(y=adj.R2))


library(grid)
library(ggeffects)
library(gridExtra)
library(gtable)
# If that doesnt do it, add grid
# Make each of the plots

# Two-panel figure: effect sizes
panA <- ggplotGrob(gpp)
panB <- ggplotGrob(er)
panK600 <- ggplotGrob(k600)
panC <- ggplotGrob(q.fig)
panD <- ggplotGrob(T.fig)
panE <- ggplotGrob(p1)
panp2 <- ggplotGrob(p2)
panF <- ggplotGrob(rhat)
panG <- ggplotGrob(er_k)
grid::grid.newpage()
grid.draw(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF, panG,size="max"))
fig2021 <- arrangeGrob(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF,panG, size="max"))
ggsave(path = here("outputs"), file = "vaul2021.pdf", fig2021, width = 30, height = 55, units = "in", limitsize = FALSE)


rm(list=ls())









#VAUL



######### vaul
data.vaul.mm.all <- read.csv(here("outputs", "clean.vaul.2022.full.csv"))


#change Discharge to m3/s
data.vaul.mm.all$discharge <- data.vaul.mm.all$discharge / 1000

bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name,
                     burnin_steps=8000, saved_steps=1500, n_cores=4,
                     GPP_daily_lower = 0, ER_daily_upper = 0,
                     
)

#Change light to modeled light
data.vaul.mm.all$solar.time <- as.POSIXct(data.vaul.mm.all$solar.time, tz = "UTC")

data.vaul.mm.all$light <-  calc_light(data.vaul.mm.all$solar.time, 65.029315, -147.714949)



data.vaul.mm.all <- data.vaul.mm.all %>%  rename(DO.sat = DO.sat.EXO)
data.vaul.mm.all <- data.vaul.mm.all %>% select(solar.time, DO.obs, DO.sat, light, discharge, depth, temp.water)

startTime <- as.POSIXct(Sys.time())
mm.test.vaul <- metab(bayes_specs, data=data.vaul.mm.all)
endTime <- as.POSIXct(Sys.time())
# 
difftime(endTime, startTime)
save(mm.test.vaul, file = here("Outputs", "vault2022-Run_2023-Full.rerun.02.04.RData"))
# 
fit.vaul <- get_fit(mm.test.vaul)
fit.daily <- fit.vaul$daily
write.csv(fit.daily, here("outputs", "vault2022-Run_2023-Full.rerun.02.04.csv"))

model_data <- get_data(mm.test.vaul)

######### Plot


gpp <- ggplot(data=fit.daily, aes(x=date, y=GPP_mean)) + geom_point(color = "chartreuse4") + geom_errorbar(aes(ymin=GPP_mean-GPP_sd, ymax=GPP_mean+GPP_sd), width=.2, position=position_dodge(0.05), color = "chartreuse4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ ggtitle("vault2022")+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) +ylim(0, max(fit.daily$GPP_mean))

er <- ggplot(data = fit.daily, aes(x = date)) +geom_point(aes(y = ER_mean), color = "firebrick3") + geom_errorbar(aes(ymin=ER_mean-ER_sd, ymax=ER_mean+ER_sd), width=.2, position=position_dodge(0.05), color = "firebrick3")+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20)) +ylim(min(fit.daily$ER_daily_mean), 0)

k600 <- ggplot(data=fit.daily, aes(x=date, y=K600_daily_mean)) + geom_point(color = "orange") + geom_errorbar(aes(ymin=K600_daily_mean-K600_daily_sd, ymax=K600_daily_mean+K600_daily_sd), width=.2, position=position_dodge(0.05), color = "orange") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) 

rhat <- ggplot(data=fit.daily, aes(x=date)) + geom_point(aes(y=GPP_Rhat, colour = "GPP")) + geom_point(aes(y=as.numeric(ER_Rhat), colour = "ER")) + geom_point(aes(y=K600_daily_Rhat, colour = "K600")) +
  labs(y = "RHAT") + theme(legend.position="top") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ geom_hline(yintercept=1.01, color = "dark blue")+ geom_hline(yintercept=1.1, color = "dark red", linetype = "dashed")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))

er_k <- ggplot(data=fit.daily, aes(x=ER_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))


q.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=discharge), color = "sienna4", size=0.4)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("Discharge ( ", m^3, s^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ylim(0, max(model_data$discharge))

T.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=temp.water), color = "slateblue4", size=0.4)+ theme(axis.title.x=element_blank() )+ labs(y = "Temperature (°C)")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+ylim(-0.004, 20)



vaul.DO.pred <- predict_DO(mm.test.vaul)
vaul.DO.pred <- na.omit(vaul.DO.pred)
lm(DO.obs~ DO.mod, data = vaul.DO.pred)
vaul.DO.pred$SM.date <- as.Date(vaul.DO.pred$solar.time - 4*60*60)
vaul.DO.pred$SM.date <- as.factor(vaul.DO.pred$SM.date)
fit_model <- function(x, y) summary(lm(x ~ y))$adj.r.squared
test.run <- vaul.DO.pred %>% group_by(SM.date) %>% summarise(adj.R2 = fit_model(DO.obs,DO.mod))
vaul.DO.pred <- full_join(vaul.DO.pred, test.run)
rsq <-  summary(lm(vaul.DO.pred$DO.mod~vaul.DO.pred$DO.obs))$r.squared

p1 <- ggplot(vaul.DO.pred, aes(x = solar.time)) + geom_point(aes(y=DO.obs, colour = "Observed DO"), color = "darkcyan") + geom_line(aes(y = DO.mod, colour= "Modeled DO"), color = "blue4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() ) + labs(colour="Green", y = "DO (mg/L)") +  theme(legend.position="none") + ggtitle("Observed (points) and Modeled (lines)")

p2 <- ggplot(vaul.DO.pred, aes(x = solar.time)) + geom_point(aes(y=adj.R2))


library(grid)
library(ggeffects)
library(gridExtra)
library(gtable)
# If that doesnt do it, add grid
# Make each of the plots

# Two-panel figure: effect sizes
panA <- ggplotGrob(gpp)
panB <- ggplotGrob(er)
panK600 <- ggplotGrob(k600)
panC <- ggplotGrob(q.fig)
panD <- ggplotGrob(T.fig)
panE <- ggplotGrob(p1)
panp2 <- ggplotGrob(p2)
panF <- ggplotGrob(rhat)
panG <- ggplotGrob(er_k)
grid::grid.newpage()
grid.draw(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF, panG,size="max"))
fig2022 <- arrangeGrob(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF,panG, size="max"))
ggsave(path = here("outputs"), file = "vaul2022.pdf", fig2022, width = 30, height = 55, units = "in", limitsize = FALSE)



rm(list=ls())



######### poke
data.poke.mm.all <- read.csv(here("outputs", "clean.poke.2019.full.csv"))


#change Discharge to m3/s
data.poke.mm.all$discharge <- data.poke.mm.all$discharge / 1000

bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name,
                     burnin_steps=8000, saved_steps=1500, n_cores=4,
                     GPP_daily_lower = 0, ER_daily_upper = 0,
                     
)
#Change light to modeled light
data.poke.mm.all$solar.time <- as.POSIXct(data.poke.mm.all$solar.time, tz = "UTC")

data.poke.mm.all$light <-  calc_light(data.poke.mm.all$solar.time, 65.152855, -147.486655)



data.poke.mm.all <- data.poke.mm.all %>%  rename(DO.sat = DO.sat.EXO)
data.poke.mm.all <- data.poke.mm.all %>% select(solar.time, DO.obs, DO.sat, light, discharge, depth, temp.water)

startTime <- as.POSIXct(Sys.time())
mm.test.poke <- metab(bayes_specs, data=data.poke.mm.all)
endTime <- as.POSIXct(Sys.time())
# 
difftime(endTime, startTime)
save(mm.test.poke, file = here("Outputs", "poker2019-Run_2023-Full.rerun.02.04.RData"))
# 
fit.poke <- get_fit(mm.test.poke)
fit.daily <- fit.poke$daily
write.csv(fit.daily, here("outputs", "poker2019-Run_2023-Full.rerun.02.04.csv"))

model_data <- get_data(mm.test.poke)

######### Plot


gpp <- ggplot(data=fit.daily, aes(x=date, y=GPP_mean)) + geom_point(color = "chartreuse4") + geom_errorbar(aes(ymin=GPP_mean-GPP_sd, ymax=GPP_mean+GPP_sd), width=.2, position=position_dodge(0.05), color = "chartreuse4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ ggtitle("poker2019")+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) +ylim(0, max(fit.daily$GPP_mean))

er <- ggplot(data = fit.daily, aes(x = date)) +geom_point(aes(y = ER_mean), color = "firebrick3") + geom_errorbar(aes(ymin=ER_mean-ER_sd, ymax=ER_mean+ER_sd), width=.2, position=position_dodge(0.05), color = "firebrick3")+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20)) +ylim(min(fit.daily$ER_daily_mean), 0)

k600 <- ggplot(data=fit.daily, aes(x=date, y=K600_daily_mean)) + geom_point(color = "orange") + geom_errorbar(aes(ymin=K600_daily_mean-K600_daily_sd, ymax=K600_daily_mean+K600_daily_sd), width=.2, position=position_dodge(0.05), color = "orange") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) 

rhat <- ggplot(data=fit.daily, aes(x=date)) + geom_point(aes(y=GPP_Rhat, colour = "GPP")) + geom_point(aes(y=as.numeric(ER_Rhat), colour = "ER")) + geom_point(aes(y=K600_daily_Rhat, colour = "K600")) +
  labs(y = "RHAT") + theme(legend.position="top") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ geom_hline(yintercept=1.01, color = "dark blue")+ geom_hline(yintercept=1.1, color = "dark red", linetype = "dashed")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))

er_k <- ggplot(data=fit.daily, aes(x=ER_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))


q.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=discharge), color = "sienna4", size=0.4)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("Discharge ( ", m^3, s^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ylim(0, max(model_data$discharge))

T.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=temp.water), color = "slateblue4", size=0.4)+ theme(axis.title.x=element_blank() )+ labs(y = "Temperature (°C)")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+ylim(-0.004, 20)



poke.DO.pred <- predict_DO(mm.test.poke)
poke.DO.pred <- na.omit(poke.DO.pred)
lm(DO.obs~ DO.mod, data = poke.DO.pred)
poke.DO.pred$SM.date <- as.Date(poke.DO.pred$solar.time - 4*60*60)
poke.DO.pred$SM.date <- as.factor(poke.DO.pred$SM.date)
fit_model <- function(x, y) summary(lm(x ~ y))$adj.r.squared
test.run <- poke.DO.pred %>% group_by(SM.date) %>% summarise(adj.R2 = fit_model(DO.obs,DO.mod))
poke.DO.pred <- full_join(poke.DO.pred, test.run)
rsq <-  summary(lm(poke.DO.pred$DO.mod~poke.DO.pred$DO.obs))$r.squared

p1 <- ggplot(poke.DO.pred, aes(x = solar.time)) + geom_point(aes(y=DO.obs, colour = "Observed DO"), color = "darkcyan") + geom_line(aes(y = DO.mod, colour= "Modeled DO"), color = "blue4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() ) + labs(colour="Green", y = "DO (mg/L)") +  theme(legend.position="none") + ggtitle("Observed (points) and Modeled (lines)")

p2 <- ggplot(poke.DO.pred, aes(x = solar.time)) + geom_point(aes(y=adj.R2))


library(grid)
library(ggeffects)
library(gridExtra)
library(gtable)
# If that doesnt do it, add grid
# Make each of the plots

# Two-panel figure: effect sizes
panA <- ggplotGrob(gpp)
panB <- ggplotGrob(er)
panK600 <- ggplotGrob(k600)
panC <- ggplotGrob(q.fig)
panD <- ggplotGrob(T.fig)
panE <- ggplotGrob(p1)
panp2 <- ggplotGrob(p2)
panF <- ggplotGrob(rhat)
panG <- ggplotGrob(er_k)
grid::grid.newpage()
grid.draw(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF, panG,size="max"))
fig2019 <- arrangeGrob(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF,panG, size="max"))
ggsave(path = here("outputs"), file = "poke2019.pdf", fig2019, width = 30, height = 55, units = "in", limitsize = FALSE)






rm(list=ls())


















#POKE

#POKE




######### poke
data.poke.mm.all <- read.csv(here("outputs", "clean.poke.2020.full.csv"))


#change Discharge to m3/s
data.poke.mm.all$discharge <- data.poke.mm.all$discharge / 1000


bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name,
                     burnin_steps=8000, saved_steps=1500, n_cores=4,
                     GPP_daily_lower = 0, ER_daily_upper = 0,
                     
)

#Change light to modeled light
data.poke.mm.all$solar.time <- as.POSIXct(data.poke.mm.all$solar.time, tz = "UTC")

data.poke.mm.all$light <-  calc_light(data.poke.mm.all$solar.time, 65.152855, -147.486655)



data.poke.mm.all <- data.poke.mm.all %>%  rename(DO.sat = DO.sat.EXO)
data.poke.mm.all <- data.poke.mm.all %>% select(solar.time, DO.obs, DO.sat, light, discharge, depth, temp.water)

startTime <- as.POSIXct(Sys.time())
mm.test.poke <- metab(bayes_specs, data=data.poke.mm.all)
endTime <- as.POSIXct(Sys.time())
# 
difftime(endTime, startTime)
save(mm.test.poke, file = here("Outputs", "poker2020-Run_2023-Full.rerun.02.04.RData"))
# 
fit.poke <- get_fit(mm.test.poke)
fit.daily <- fit.poke$daily
write.csv(fit.daily, here("outputs", "poker2020-Run_2023-Full.rerun.02.04.csv"))

model_data <- get_data(mm.test.poke)

######### Plot


gpp <- ggplot(data=fit.daily, aes(x=date, y=GPP_mean)) + geom_point(color = "chartreuse4") + geom_errorbar(aes(ymin=GPP_mean-GPP_sd, ymax=GPP_mean+GPP_sd), width=.2, position=position_dodge(0.05), color = "chartreuse4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ ggtitle("poker2020")+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) +ylim(0, max(fit.daily$GPP_mean))

er <- ggplot(data = fit.daily, aes(x = date)) +geom_point(aes(y = ER_mean), color = "firebrick3") + geom_errorbar(aes(ymin=ER_mean-ER_sd, ymax=ER_mean+ER_sd), width=.2, position=position_dodge(0.05), color = "firebrick3")+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20)) +ylim(min(fit.daily$ER_daily_mean), 0)

k600 <- ggplot(data=fit.daily, aes(x=date, y=K600_daily_mean)) + geom_point(color = "orange") + geom_errorbar(aes(ymin=K600_daily_mean-K600_daily_sd, ymax=K600_daily_mean+K600_daily_sd), width=.2, position=position_dodge(0.05), color = "orange") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) 

rhat <- ggplot(data=fit.daily, aes(x=date)) + geom_point(aes(y=GPP_Rhat, colour = "GPP")) + geom_point(aes(y=as.numeric(ER_Rhat), colour = "ER")) + geom_point(aes(y=K600_daily_Rhat, colour = "K600")) +
  labs(y = "RHAT") + theme(legend.position="top") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ geom_hline(yintercept=1.01, color = "dark blue")+ geom_hline(yintercept=1.1, color = "dark red", linetype = "dashed")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))

er_k <- ggplot(data=fit.daily, aes(x=ER_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))


q.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=discharge), color = "sienna4", size=0.4)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("Discharge ( ", m^3, s^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ylim(0, max(model_data$discharge))

T.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=temp.water), color = "slateblue4", size=0.4)+ theme(axis.title.x=element_blank() )+ labs(y = "Temperature (°C)")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+ylim(-0.004, 20)



poke.DO.pred <- predict_DO(mm.test.poke)
poke.DO.pred <- na.omit(poke.DO.pred)
lm(DO.obs~ DO.mod, data = poke.DO.pred)
poke.DO.pred$SM.date <- as.Date(poke.DO.pred$solar.time - 4*60*60)
poke.DO.pred$SM.date <- as.factor(poke.DO.pred$SM.date)
fit_model <- function(x, y) summary(lm(x ~ y))$adj.r.squared
test.run <- poke.DO.pred %>% group_by(SM.date) %>% summarise(adj.R2 = fit_model(DO.obs,DO.mod))
poke.DO.pred <- full_join(poke.DO.pred, test.run)
rsq <-  summary(lm(poke.DO.pred$DO.mod~poke.DO.pred$DO.obs))$r.squared

p1 <- ggplot(poke.DO.pred, aes(x = solar.time)) + geom_point(aes(y=DO.obs, colour = "Observed DO"), color = "darkcyan") + geom_line(aes(y = DO.mod, colour= "Modeled DO"), color = "blue4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() ) + labs(colour="Green", y = "DO (mg/L)") +  theme(legend.position="none") + ggtitle("Observed (points) and Modeled (lines)")

p2 <- ggplot(poke.DO.pred, aes(x = solar.time)) + geom_point(aes(y=adj.R2))


library(grid)
library(ggeffects)
library(gridExtra)
library(gtable)
# If that doesnt do it, add grid
# Make each of the plots

# Two-panel figure: effect sizes
panA <- ggplotGrob(gpp)
panB <- ggplotGrob(er)
panK600 <- ggplotGrob(k600)
panC <- ggplotGrob(q.fig)
panD <- ggplotGrob(T.fig)
panE <- ggplotGrob(p1)
panp2 <- ggplotGrob(p2)
panF <- ggplotGrob(rhat)
panG <- ggplotGrob(er_k)
grid::grid.newpage()
grid.draw(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF, panG,size="max"))
fig2020 <- arrangeGrob(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF,panG, size="max"))
ggsave(path = here("outputs"), file = "poke2020.pdf", fig2020, width = 30, height = 55, units = "in", limitsize = FALSE)











rm(list=ls())











######### poke
data.poke.mm.all <- read.csv(here("outputs", "clean.poke.2021.full.csv"))


#change Discharge to m3/s
data.poke.mm.all$discharge <- data.poke.mm.all$discharge / 1000


bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name,
                     burnin_steps=8000, saved_steps=1500, n_cores=4,
                     GPP_daily_lower = 0, ER_daily_upper = 0,
                     
)

#Change light to modeled light
data.poke.mm.all$solar.time <- as.POSIXct(data.poke.mm.all$solar.time, tz = "UTC")

data.poke.mm.all$light <-  calc_light(data.poke.mm.all$solar.time, 65.152855, -147.486655)



data.poke.mm.all <- data.poke.mm.all %>%  rename(DO.sat = DO.sat.EXO)
data.poke.mm.all <- data.poke.mm.all %>% select(solar.time, DO.obs, DO.sat, light, discharge, depth, temp.water)

startTime <- as.POSIXct(Sys.time())
mm.test.poke <- metab(bayes_specs, data=data.poke.mm.all)
endTime <- as.POSIXct(Sys.time())
# 
difftime(endTime, startTime)
save(mm.test.poke, file = here("Outputs", "poker2021-Run_2023-Full.rerun.02.04.RData"))
# 
fit.poke <- get_fit(mm.test.poke)
fit.daily <- fit.poke$daily
write.csv(fit.daily, here("outputs", "poker2021-Run_2023-Full.rerun.02.04.csv"))

model_data <- get_data(mm.test.poke)

######### Plot


gpp <- ggplot(data=fit.daily, aes(x=date, y=GPP_mean)) + geom_point(color = "chartreuse4") + geom_errorbar(aes(ymin=GPP_mean-GPP_sd, ymax=GPP_mean+GPP_sd), width=.2, position=position_dodge(0.05), color = "chartreuse4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ ggtitle("poker2021")+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) +ylim(0, max(fit.daily$GPP_mean))

er <- ggplot(data = fit.daily, aes(x = date)) +geom_point(aes(y = ER_mean), color = "firebrick3") + geom_errorbar(aes(ymin=ER_mean-ER_sd, ymax=ER_mean+ER_sd), width=.2, position=position_dodge(0.05), color = "firebrick3")+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20)) +ylim(min(fit.daily$ER_daily_mean), 0)

k600 <- ggplot(data=fit.daily, aes(x=date, y=K600_daily_mean)) + geom_point(color = "orange") + geom_errorbar(aes(ymin=K600_daily_mean-K600_daily_sd, ymax=K600_daily_mean+K600_daily_sd), width=.2, position=position_dodge(0.05), color = "orange") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) 

rhat <- ggplot(data=fit.daily, aes(x=date)) + geom_point(aes(y=GPP_Rhat, colour = "GPP")) + geom_point(aes(y=as.numeric(ER_Rhat), colour = "ER")) + geom_point(aes(y=K600_daily_Rhat, colour = "K600")) +
  labs(y = "RHAT") + theme(legend.position="top") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ geom_hline(yintercept=1.01, color = "dark blue")+ geom_hline(yintercept=1.1, color = "dark red", linetype = "dashed")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))

er_k <- ggplot(data=fit.daily, aes(x=ER_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))


q.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=discharge), color = "sienna4", size=0.4)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("Discharge ( ", m^3, s^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ylim(0, max(model_data$discharge))

T.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=temp.water), color = "slateblue4", size=0.4)+ theme(axis.title.x=element_blank() )+ labs(y = "Temperature (°C)")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+ylim(-0.004, 20)



poke.DO.pred <- predict_DO(mm.test.poke)
poke.DO.pred <- na.omit(poke.DO.pred)
lm(DO.obs~ DO.mod, data = poke.DO.pred)
poke.DO.pred$SM.date <- as.Date(poke.DO.pred$solar.time - 4*60*60)
poke.DO.pred$SM.date <- as.factor(poke.DO.pred$SM.date)
fit_model <- function(x, y) summary(lm(x ~ y))$adj.r.squared
test.run <- poke.DO.pred %>% group_by(SM.date) %>% summarise(adj.R2 = fit_model(DO.obs,DO.mod))
poke.DO.pred <- full_join(poke.DO.pred, test.run)
rsq <-  summary(lm(poke.DO.pred$DO.mod~poke.DO.pred$DO.obs))$r.squared

p1 <- ggplot(poke.DO.pred, aes(x = solar.time)) + geom_point(aes(y=DO.obs, colour = "Observed DO"), color = "darkcyan") + geom_line(aes(y = DO.mod, colour= "Modeled DO"), color = "blue4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() ) + labs(colour="Green", y = "DO (mg/L)") +  theme(legend.position="none") + ggtitle("Observed (points) and Modeled (lines)")

p2 <- ggplot(poke.DO.pred, aes(x = solar.time)) + geom_point(aes(y=adj.R2))


library(grid)
library(ggeffects)
library(gridExtra)
library(gtable)
# If that doesnt do it, add grid
# Make each of the plots

# Two-panel figure: effect sizes
panA <- ggplotGrob(gpp)
panB <- ggplotGrob(er)
panK600 <- ggplotGrob(k600)
panC <- ggplotGrob(q.fig)
panD <- ggplotGrob(T.fig)
panE <- ggplotGrob(p1)
panp2 <- ggplotGrob(p2)
panF <- ggplotGrob(rhat)
panG <- ggplotGrob(er_k)
grid::grid.newpage()
grid.draw(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF, panG,size="max"))
fig2021 <- arrangeGrob(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF,panG, size="max"))
ggsave(path = here("outputs"), file = "poke2021.pdf", fig2021, width = 30, height = 55, units = "in", limitsize = FALSE)







rm(list=ls())




#POKE



######### poke
data.poke.mm.all <- read.csv(here("outputs", "clean.poke.2022.full.csv"))


#change Discharge to m3/s
data.poke.mm.all$discharge <- data.poke.mm.all$discharge / 1000

bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name,
                     burnin_steps=8000, saved_steps=1500, n_cores=4,
                     GPP_daily_lower = 0, ER_daily_upper = 0,
                     
)

#Change light to modeled light
data.poke.mm.all$solar.time <- as.POSIXct(data.poke.mm.all$solar.time, tz = "UTC")

data.poke.mm.all$light <-  calc_light(data.poke.mm.all$solar.time, 65.152855, -147.486655)



data.poke.mm.all <- data.poke.mm.all %>%  rename(DO.sat = DO.sat.EXO)
data.poke.mm.all <- data.poke.mm.all %>% select(solar.time, DO.obs, DO.sat, light, discharge, depth, temp.water)

startTime <- as.POSIXct(Sys.time())
mm.test.poke <- metab(bayes_specs, data=data.poke.mm.all)
endTime <- as.POSIXct(Sys.time())
# 
difftime(endTime, startTime)
save(mm.test.poke, file = here("Outputs", "poker2022-Run_2023-Full.rerun.02.04.RData"))
# 
fit.poke <- get_fit(mm.test.poke)
fit.daily <- fit.poke$daily
write.csv(fit.daily, here("outputs", "poker2022-Run_2023-Full.rerun.02.04.csv"))

model_data <- get_data(mm.test.poke)

######### Plot


gpp <- ggplot(data=fit.daily, aes(x=date, y=GPP_mean)) + geom_point(color = "chartreuse4") + geom_errorbar(aes(ymin=GPP_mean-GPP_sd, ymax=GPP_mean+GPP_sd), width=.2, position=position_dodge(0.05), color = "chartreuse4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ ggtitle("poker2022")+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) +ylim(0, max(fit.daily$GPP_mean))

er <- ggplot(data = fit.daily, aes(x = date)) +geom_point(aes(y = ER_mean), color = "firebrick3") + geom_errorbar(aes(ymin=ER_mean-ER_sd, ymax=ER_mean+ER_sd), width=.2, position=position_dodge(0.05), color = "firebrick3")+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20)) +ylim(min(fit.daily$ER_daily_mean), 0)

k600 <- ggplot(data=fit.daily, aes(x=date, y=K600_daily_mean)) + geom_point(color = "orange") + geom_errorbar(aes(ymin=K600_daily_mean-K600_daily_sd, ymax=K600_daily_mean+K600_daily_sd), width=.2, position=position_dodge(0.05), color = "orange") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) 

rhat <- ggplot(data=fit.daily, aes(x=date)) + geom_point(aes(y=GPP_Rhat, colour = "GPP")) + geom_point(aes(y=as.numeric(ER_Rhat), colour = "ER")) + geom_point(aes(y=K600_daily_Rhat, colour = "K600")) +
  labs(y = "RHAT") + theme(legend.position="top") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ geom_hline(yintercept=1.01, color = "dark blue")+ geom_hline(yintercept=1.1, color = "dark red", linetype = "dashed")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))

er_k <- ggplot(data=fit.daily, aes(x=ER_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))


q.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=discharge), color = "sienna4", size=0.4)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("Discharge ( ", m^3, s^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ylim(0, max(model_data$discharge))

T.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=temp.water), color = "slateblue4", size=0.4)+ theme(axis.title.x=element_blank() )+ labs(y = "Temperature (°C)")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+ylim(-0.004, 20)



poke.DO.pred <- predict_DO(mm.test.poke)
poke.DO.pred <- na.omit(poke.DO.pred)
lm(DO.obs~ DO.mod, data = poke.DO.pred)
poke.DO.pred$SM.date <- as.Date(poke.DO.pred$solar.time - 4*60*60)
poke.DO.pred$SM.date <- as.factor(poke.DO.pred$SM.date)
fit_model <- function(x, y) summary(lm(x ~ y))$adj.r.squared
test.run <- poke.DO.pred %>% group_by(SM.date) %>% summarise(adj.R2 = fit_model(DO.obs,DO.mod))
poke.DO.pred <- full_join(poke.DO.pred, test.run)
rsq <-  summary(lm(poke.DO.pred$DO.mod~poke.DO.pred$DO.obs))$r.squared

p1 <- ggplot(poke.DO.pred, aes(x = solar.time)) + geom_point(aes(y=DO.obs, colour = "Observed DO"), color = "darkcyan") + geom_line(aes(y = DO.mod, colour= "Modeled DO"), color = "blue4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() ) + labs(colour="Green", y = "DO (mg/L)") +  theme(legend.position="none") + ggtitle("Observed (points) and Modeled (lines)")

p2 <- ggplot(poke.DO.pred, aes(x = solar.time)) + geom_point(aes(y=adj.R2))


library(grid)
library(ggeffects)
library(gridExtra)
library(gtable)
# If that doesnt do it, add grid
# Make each of the plots

# Two-panel figure: effect sizes
panA <- ggplotGrob(gpp)
panB <- ggplotGrob(er)
panK600 <- ggplotGrob(k600)
panC <- ggplotGrob(q.fig)
panD <- ggplotGrob(T.fig)
panE <- ggplotGrob(p1)
panp2 <- ggplotGrob(p2)
panF <- ggplotGrob(rhat)
panG <- ggplotGrob(er_k)
grid::grid.newpage()
grid.draw(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF, panG,size="max"))
fig2022 <- arrangeGrob(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF,panG, size="max"))
ggsave(path = here("outputs"), file = "poke2022.pdf", fig2022, width = 30, height = 55, units = "in", limitsize = FALSE)







rm(list=ls())






#FRCH



######### frch
data.frch.mm.all <- read.csv(here("outputs", "clean.frch.2019.full.csv"))


#change Discharge to m3/s
data.frch.mm.all$discharge <- data.frch.mm.all$discharge / 1000

bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name,
                     burnin_steps=8000, saved_steps=1500, n_cores=4,
                     GPP_daily_lower = 0, ER_daily_upper = 0,
                     
)

#Change light to modeled light
data.frch.mm.all$solar.time <- as.POSIXct(data.frch.mm.all$solar.time, tz = "UTC")

data.frch.mm.all$light <-  calc_light(data.frch.mm.all$solar.time, 64.606673786354, -146.916364004037)



data.frch.mm.all <- data.frch.mm.all %>%  rename(DO.sat = DO.sat.EXO)
data.frch.mm.all <- data.frch.mm.all %>% select(solar.time, DO.obs, DO.sat, light, discharge, depth, temp.water)

startTime <- as.POSIXct(Sys.time())
mm.test.frch <- metab(bayes_specs, data=data.frch.mm.all)
endTime <- as.POSIXct(Sys.time())
# 
difftime(endTime, startTime)
save(mm.test.frch, file = here("Outputs", "french2019-Run_2023-Full.rerun.02.04.RData"))
# 
fit.frch <- get_fit(mm.test.frch)
fit.daily <- fit.frch$daily
write.csv(fit.daily, here("outputs", "french2019-Run_2023-Full.rerun.02.04.csv"))

model_data <- get_data(mm.test.frch)

######### Plot


gpp <- ggplot(data=fit.daily, aes(x=date, y=GPP_mean)) + geom_point(color = "chartreuse4") + geom_errorbar(aes(ymin=GPP_mean-GPP_sd, ymax=GPP_mean+GPP_sd), width=.2, position=position_dodge(0.05), color = "chartreuse4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ ggtitle("french2019")+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) +ylim(0, max(fit.daily$GPP_mean))

er <- ggplot(data = fit.daily, aes(x = date)) +geom_point(aes(y = ER_mean), color = "firebrick3") + geom_errorbar(aes(ymin=ER_mean-ER_sd, ymax=ER_mean+ER_sd), width=.2, position=position_dodge(0.05), color = "firebrick3")+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20)) +ylim(min(fit.daily$ER_daily_mean), 0)

k600 <- ggplot(data=fit.daily, aes(x=date, y=K600_daily_mean)) + geom_point(color = "orange") + geom_errorbar(aes(ymin=K600_daily_mean-K600_daily_sd, ymax=K600_daily_mean+K600_daily_sd), width=.2, position=position_dodge(0.05), color = "orange") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) 

rhat <- ggplot(data=fit.daily, aes(x=date)) + geom_point(aes(y=GPP_Rhat, colour = "GPP")) + geom_point(aes(y=as.numeric(ER_Rhat), colour = "ER")) + geom_point(aes(y=K600_daily_Rhat, colour = "K600")) +
  labs(y = "RHAT") + theme(legend.position="top") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ geom_hline(yintercept=1.01, color = "dark blue")+ geom_hline(yintercept=1.1, color = "dark red", linetype = "dashed")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))

er_k <- ggplot(data=fit.daily, aes(x=ER_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))


q.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=discharge), color = "sienna4", size=0.4)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("Discharge ( ", m^3, s^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ylim(0, max(model_data$discharge))

T.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=temp.water), color = "slateblue4", size=0.4)+ theme(axis.title.x=element_blank() )+ labs(y = "Temperature (°C)")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+ylim(-0.004, 20)



frch.DO.pred <- predict_DO(mm.test.frch)
frch.DO.pred <- na.omit(frch.DO.pred)
lm(DO.obs~ DO.mod, data = frch.DO.pred)
frch.DO.pred$SM.date <- as.Date(frch.DO.pred$solar.time - 4*60*60)
frch.DO.pred$SM.date <- as.factor(frch.DO.pred$SM.date)
fit_model <- function(x, y) summary(lm(x ~ y))$adj.r.squared
test.run <- frch.DO.pred %>% group_by(SM.date) %>% summarise(adj.R2 = fit_model(DO.obs,DO.mod))
frch.DO.pred <- full_join(frch.DO.pred, test.run)
rsq <-  summary(lm(frch.DO.pred$DO.mod~frch.DO.pred$DO.obs))$r.squared

p1 <- ggplot(frch.DO.pred, aes(x = solar.time)) + geom_point(aes(y=DO.obs, colour = "Observed DO"), color = "darkcyan") + geom_line(aes(y = DO.mod, colour= "Modeled DO"), color = "blue4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() ) + labs(colour="Green", y = "DO (mg/L)") +  theme(legend.position="none") + ggtitle("Observed (points) and Modeled (lines)")

p2 <- ggplot(frch.DO.pred, aes(x = solar.time)) + geom_point(aes(y=adj.R2))


library(grid)
library(ggeffects)
library(gridExtra)
library(gtable)
# If that doesnt do it, add grid
# Make each of the plots

# Two-panel figure: effect sizes
panA <- ggplotGrob(gpp)
panB <- ggplotGrob(er)
panK600 <- ggplotGrob(k600)
panC <- ggplotGrob(q.fig)
panD <- ggplotGrob(T.fig)
panE <- ggplotGrob(p1)
panp2 <- ggplotGrob(p2)
panF <- ggplotGrob(rhat)
panG <- ggplotGrob(er_k)
grid::grid.newpage()
grid.draw(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF, panG,size="max"))
fig2019 <- arrangeGrob(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF,panG, size="max"))
ggsave(path = here("outputs"), file = "frch2019.pdf", fig2019, width = 30, height = 55, units = "in", limitsize = FALSE)








rm(list=ls())




######### frch
data.frch.mm.all <- read.csv(here("outputs", "clean.frch.2020.full.csv"))


#change Discharge to m3/s
data.frch.mm.all$discharge <- data.frch.mm.all$discharge / 1000

bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name,
                     burnin_steps=8000, saved_steps=1500, n_cores=4,
                     GPP_daily_lower = 0, ER_daily_upper = 0,
                     
)

#Change light to modeled light
data.frch.mm.all$solar.time <- as.POSIXct(data.frch.mm.all$solar.time, tz = "UTC")

data.frch.mm.all$light <-  calc_light(data.frch.mm.all$solar.time, 64.606673786354, -146.916364004037)



data.frch.mm.all <- data.frch.mm.all %>%  rename(DO.sat = DO.sat.EXO)
data.frch.mm.all <- data.frch.mm.all %>% select(solar.time, DO.obs, DO.sat, light, discharge, depth, temp.water)

startTime <- as.POSIXct(Sys.time())
mm.test.frch <- metab(bayes_specs, data=data.frch.mm.all)
endTime <- as.POSIXct(Sys.time())
# 
difftime(endTime, startTime)
save(mm.test.frch, file = here("Outputs", "french2020-Run_2023-Full.rerun.02.04.RData"))
# 
fit.frch <- get_fit(mm.test.frch)
fit.daily <- fit.frch$daily
write.csv(fit.daily, here("outputs", "french2020-Run_2023-Full.rerun.02.04.csv"))

model_data <- get_data(mm.test.frch)

######### Plot


gpp <- ggplot(data=fit.daily, aes(x=date, y=GPP_mean)) + geom_point(color = "chartreuse4") + geom_errorbar(aes(ymin=GPP_mean-GPP_sd, ymax=GPP_mean+GPP_sd), width=.2, position=position_dodge(0.05), color = "chartreuse4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ ggtitle("french2020")+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) +ylim(0, max(fit.daily$GPP_mean))

er <- ggplot(data = fit.daily, aes(x = date)) +geom_point(aes(y = ER_mean), color = "firebrick3") + geom_errorbar(aes(ymin=ER_mean-ER_sd, ymax=ER_mean+ER_sd), width=.2, position=position_dodge(0.05), color = "firebrick3")+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20)) +ylim(min(fit.daily$ER_daily_mean), 0)

k600 <- ggplot(data=fit.daily, aes(x=date, y=K600_daily_mean)) + geom_point(color = "orange") + geom_errorbar(aes(ymin=K600_daily_mean-K600_daily_sd, ymax=K600_daily_mean+K600_daily_sd), width=.2, position=position_dodge(0.05), color = "orange") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) 

rhat <- ggplot(data=fit.daily, aes(x=date)) + geom_point(aes(y=GPP_Rhat, colour = "GPP")) + geom_point(aes(y=as.numeric(ER_Rhat), colour = "ER")) + geom_point(aes(y=K600_daily_Rhat, colour = "K600")) +
  labs(y = "RHAT") + theme(legend.position="top") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ geom_hline(yintercept=1.01, color = "dark blue")+ geom_hline(yintercept=1.1, color = "dark red", linetype = "dashed")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))

er_k <- ggplot(data=fit.daily, aes(x=ER_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))


q.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=discharge), color = "sienna4", size=0.4)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("Discharge ( ", m^3, s^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ylim(0, max(model_data$discharge))

T.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=temp.water), color = "slateblue4", size=0.4)+ theme(axis.title.x=element_blank() )+ labs(y = "Temperature (°C)")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+ylim(-0.004, 20)



frch.DO.pred <- predict_DO(mm.test.frch)
frch.DO.pred <- na.omit(frch.DO.pred)
lm(DO.obs~ DO.mod, data = frch.DO.pred)
frch.DO.pred$SM.date <- as.Date(frch.DO.pred$solar.time - 4*60*60)
frch.DO.pred$SM.date <- as.factor(frch.DO.pred$SM.date)
fit_model <- function(x, y) summary(lm(x ~ y))$adj.r.squared
test.run <- frch.DO.pred %>% group_by(SM.date) %>% summarise(adj.R2 = fit_model(DO.obs,DO.mod))
frch.DO.pred <- full_join(frch.DO.pred, test.run)
rsq <-  summary(lm(frch.DO.pred$DO.mod~frch.DO.pred$DO.obs))$r.squared

p1 <- ggplot(frch.DO.pred, aes(x = solar.time)) + geom_point(aes(y=DO.obs, colour = "Observed DO"), color = "darkcyan") + geom_line(aes(y = DO.mod, colour= "Modeled DO"), color = "blue4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() ) + labs(colour="Green", y = "DO (mg/L)") +  theme(legend.position="none") + ggtitle("Observed (points) and Modeled (lines)")

p2 <- ggplot(frch.DO.pred, aes(x = solar.time)) + geom_point(aes(y=adj.R2))


library(grid)
library(ggeffects)
library(gridExtra)
library(gtable)
# If that doesnt do it, add grid
# Make each of the plots

# Two-panel figure: effect sizes
panA <- ggplotGrob(gpp)
panB <- ggplotGrob(er)
panK600 <- ggplotGrob(k600)
panC <- ggplotGrob(q.fig)
panD <- ggplotGrob(T.fig)
panE <- ggplotGrob(p1)
panp2 <- ggplotGrob(p2)
panF <- ggplotGrob(rhat)
panG <- ggplotGrob(er_k)
grid::grid.newpage()
grid.draw(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF, panG,size="max"))
fig2020 <- arrangeGrob(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF,panG, size="max"))
ggsave(path = here("outputs"), file = "frch2020.pdf", fig2020, width = 30, height = 55, units = "in", limitsize = FALSE)








rm(list=ls())




######### frch
data.frch.mm.all <- read.csv(here("outputs", "clean.frch.2021.full.csv"))


#change Discharge to m3/s
data.frch.mm.all$discharge <- data.frch.mm.all$discharge / 1000

bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name,
                     burnin_steps=8000, saved_steps=1500, n_cores=4,
                     GPP_daily_lower = 0, ER_daily_upper = 0,
                     
)

#Change light to modeled light
data.frch.mm.all$solar.time <- as.POSIXct(data.frch.mm.all$solar.time, tz = "UTC")

data.frch.mm.all$light <-  calc_light(data.frch.mm.all$solar.time, 64.606673786354, -146.916364004037)



data.frch.mm.all <- data.frch.mm.all %>%  rename(DO.sat = DO.sat.EXO)
data.frch.mm.all <- data.frch.mm.all %>% select(solar.time, DO.obs, DO.sat, light, discharge, depth, temp.water)

startTime <- as.POSIXct(Sys.time())
mm.test.frch <- metab(bayes_specs, data=data.frch.mm.all)
endTime <- as.POSIXct(Sys.time())
# 
difftime(endTime, startTime)
save(mm.test.frch, file = here("Outputs", "french2021-Run_2023-Full.rerun.02.04.RData"))
# 
fit.frch <- get_fit(mm.test.frch)
fit.daily <- fit.frch$daily
write.csv(fit.daily, here("outputs", "french2021-Run_2023-Full.rerun.02.04.csv"))

model_data <- get_data(mm.test.frch)

######### Plot


gpp <- ggplot(data=fit.daily, aes(x=date, y=GPP_mean)) + geom_point(color = "chartreuse4") + geom_errorbar(aes(ymin=GPP_mean-GPP_sd, ymax=GPP_mean+GPP_sd), width=.2, position=position_dodge(0.05), color = "chartreuse4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ ggtitle("french2021")+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) +ylim(0, max(fit.daily$GPP_mean))

er <- ggplot(data = fit.daily, aes(x = date)) +geom_point(aes(y = ER_mean), color = "firebrick3") + geom_errorbar(aes(ymin=ER_mean-ER_sd, ymax=ER_mean+ER_sd), width=.2, position=position_dodge(0.05), color = "firebrick3")+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20)) +ylim(min(fit.daily$ER_daily_mean), 0)

k600 <- ggplot(data=fit.daily, aes(x=date, y=K600_daily_mean)) + geom_point(color = "orange") + geom_errorbar(aes(ymin=K600_daily_mean-K600_daily_sd, ymax=K600_daily_mean+K600_daily_sd), width=.2, position=position_dodge(0.05), color = "orange") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) 

rhat <- ggplot(data=fit.daily, aes(x=date)) + geom_point(aes(y=GPP_Rhat, colour = "GPP")) + geom_point(aes(y=as.numeric(ER_Rhat), colour = "ER")) + geom_point(aes(y=K600_daily_Rhat, colour = "K600")) +
  labs(y = "RHAT") + theme(legend.position="top") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ geom_hline(yintercept=1.01, color = "dark blue")+ geom_hline(yintercept=1.1, color = "dark red", linetype = "dashed")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))

er_k <- ggplot(data=fit.daily, aes(x=ER_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))


q.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=discharge), color = "sienna4", size=0.4)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("Discharge ( ", m^3, s^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ylim(0, max(model_data$discharge))

T.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=temp.water), color = "slateblue4", size=0.4)+ theme(axis.title.x=element_blank() )+ labs(y = "Temperature (°C)")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+ylim(-0.004, 20)



frch.DO.pred <- predict_DO(mm.test.frch)
frch.DO.pred <- na.omit(frch.DO.pred)
lm(DO.obs~ DO.mod, data = frch.DO.pred)
frch.DO.pred$SM.date <- as.Date(frch.DO.pred$solar.time - 4*60*60)
frch.DO.pred$SM.date <- as.factor(frch.DO.pred$SM.date)
fit_model <- function(x, y) summary(lm(x ~ y))$adj.r.squared
test.run <- frch.DO.pred %>% group_by(SM.date) %>% summarise(adj.R2 = fit_model(DO.obs,DO.mod))
frch.DO.pred <- full_join(frch.DO.pred, test.run)
rsq <-  summary(lm(frch.DO.pred$DO.mod~frch.DO.pred$DO.obs))$r.squared

p1 <- ggplot(frch.DO.pred, aes(x = solar.time)) + geom_point(aes(y=DO.obs, colour = "Observed DO"), color = "darkcyan") + geom_line(aes(y = DO.mod, colour= "Modeled DO"), color = "blue4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() ) + labs(colour="Green", y = "DO (mg/L)") +  theme(legend.position="none") + ggtitle("Observed (points) and Modeled (lines)")

p2 <- ggplot(frch.DO.pred, aes(x = solar.time)) + geom_point(aes(y=adj.R2))


library(grid)
library(ggeffects)
library(gridExtra)
library(gtable)
# If that doesnt do it, add grid
# Make each of the plots

# Two-panel figure: effect sizes
panA <- ggplotGrob(gpp)
panB <- ggplotGrob(er)
panK600 <- ggplotGrob(k600)
panC <- ggplotGrob(q.fig)
panD <- ggplotGrob(T.fig)
panE <- ggplotGrob(p1)
panp2 <- ggplotGrob(p2)
panF <- ggplotGrob(rhat)
panG <- ggplotGrob(er_k)
grid::grid.newpage()
grid.draw(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF, panG,size="max"))
fig2021 <- arrangeGrob(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF,panG, size="max"))
ggsave(path = here("outputs"), file = "frch2021.pdf", fig2021, width = 30, height = 55, units = "in", limitsize = FALSE)




rm(list=ls())








######### frch
data.frch.mm.all <- read.csv(here("outputs", "clean.frch.2022.full.csv"))


#change Discharge to m3/s
data.frch.mm.all$discharge <- data.frch.mm.all$discharge / 1000


bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name,
                     burnin_steps=8000, saved_steps=1500, n_cores=4,
                     GPP_daily_lower = 0, ER_daily_upper = 0,
                     
)


#Change light to modeled light
data.frch.mm.all$solar.time <- as.POSIXct(data.frch.mm.all$solar.time, tz = "UTC")

data.frch.mm.all$light <-  calc_light(data.frch.mm.all$solar.time, 64.606673786354, -146.916364004037)



data.frch.mm.all <- data.frch.mm.all %>%  rename(DO.sat = DO.sat.EXO)
data.frch.mm.all <- data.frch.mm.all %>% select(solar.time, DO.obs, DO.sat, light, discharge, depth, temp.water)

startTime <- as.POSIXct(Sys.time())
mm.test.frch <- metab(bayes_specs, data=data.frch.mm.all)
endTime <- as.POSIXct(Sys.time())
# 
difftime(endTime, startTime)
save(mm.test.frch, file = here("Outputs", "french2022-Run_2023-Full.rerun.02.04.RData"))
# 
fit.frch <- get_fit(mm.test.frch)
fit.daily <- fit.frch$daily
write.csv(fit.daily, here("outputs", "french2022-Run_2023-Full.rerun.02.04.csv"))

model_data <- get_data(mm.test.frch)

######### Plot


gpp <- ggplot(data=fit.daily, aes(x=date, y=GPP_mean)) + geom_point(color = "chartreuse4") + geom_errorbar(aes(ymin=GPP_mean-GPP_sd, ymax=GPP_mean+GPP_sd), width=.2, position=position_dodge(0.05), color = "chartreuse4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ ggtitle("french2022")+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) +ylim(0, max(fit.daily$GPP_mean))

er <- ggplot(data = fit.daily, aes(x = date)) +geom_point(aes(y = ER_mean), color = "firebrick3") + geom_errorbar(aes(ymin=ER_mean-ER_sd, ymax=ER_mean+ER_sd), width=.2, position=position_dodge(0.05), color = "firebrick3")+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20)) +ylim(min(fit.daily$ER_daily_mean), 0)

k600 <- ggplot(data=fit.daily, aes(x=date, y=K600_daily_mean)) + geom_point(color = "orange") + geom_errorbar(aes(ymin=K600_daily_mean-K600_daily_sd, ymax=K600_daily_mean+K600_daily_sd), width=.2, position=position_dodge(0.05), color = "orange") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) 

rhat <- ggplot(data=fit.daily, aes(x=date)) + geom_point(aes(y=GPP_Rhat, colour = "GPP")) + geom_point(aes(y=as.numeric(ER_Rhat), colour = "ER")) + geom_point(aes(y=K600_daily_Rhat, colour = "K600")) +
  labs(y = "RHAT") + theme(legend.position="top") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ geom_hline(yintercept=1.01, color = "dark blue")+ geom_hline(yintercept=1.1, color = "dark red", linetype = "dashed")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))

er_k <- ggplot(data=fit.daily, aes(x=ER_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))


q.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=discharge), color = "sienna4", size=0.4)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("Discharge ( ", m^3, s^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ylim(0, max(model_data$discharge))

T.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=temp.water), color = "slateblue4", size=0.4)+ theme(axis.title.x=element_blank() )+ labs(y = "Temperature (°C)")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+ylim(-0.004, 20)



frch.DO.pred <- predict_DO(mm.test.frch)
frch.DO.pred <- na.omit(frch.DO.pred)
lm(DO.obs~ DO.mod, data = frch.DO.pred)
frch.DO.pred$SM.date <- as.Date(frch.DO.pred$solar.time - 4*60*60)
frch.DO.pred$SM.date <- as.factor(frch.DO.pred$SM.date)
fit_model <- function(x, y) summary(lm(x ~ y))$adj.r.squared
test.run <- frch.DO.pred %>% group_by(SM.date) %>% summarise(adj.R2 = fit_model(DO.obs,DO.mod))
frch.DO.pred <- full_join(frch.DO.pred, test.run)
rsq <-  summary(lm(frch.DO.pred$DO.mod~frch.DO.pred$DO.obs))$r.squared

p1 <- ggplot(frch.DO.pred, aes(x = solar.time)) + geom_point(aes(y=DO.obs, colour = "Observed DO"), color = "darkcyan") + geom_line(aes(y = DO.mod, colour= "Modeled DO"), color = "blue4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() ) + labs(colour="Green", y = "DO (mg/L)") +  theme(legend.position="none") + ggtitle("Observed (points) and Modeled (lines)")

p2 <- ggplot(frch.DO.pred, aes(x = solar.time)) + geom_point(aes(y=adj.R2))


library(grid)
library(ggeffects)
library(gridExtra)
library(gtable)
# If that doesnt do it, add grid
# Make each of the plots

# Two-panel figure: effect sizes
panA <- ggplotGrob(gpp)
panB <- ggplotGrob(er)
panK600 <- ggplotGrob(k600)
panC <- ggplotGrob(q.fig)
panD <- ggplotGrob(T.fig)
panE <- ggplotGrob(p1)
panp2 <- ggplotGrob(p2)
panF <- ggplotGrob(rhat)
panG <- ggplotGrob(er_k)
grid::grid.newpage()
grid.draw(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF, panG,size="max"))
fig2022 <- arrangeGrob(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF,panG, size="max"))
ggsave(path = here("outputs"), file = "frch2022.pdf", fig2022, width = 30, height = 55, units = "in", limitsize = FALSE)


rm(list=ls())








######### moos
data.moos.mm.all <- read.csv(here("outputs", "clean.moos.2019.full.csv"))


#change Discharge to m3/s
data.moos.mm.all$discharge <- data.moos.mm.all$discharge / 1000


bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name,
                     burnin_steps=8000, saved_steps=1500, n_cores=4,
                     GPP_daily_lower = 0, ER_daily_upper = 0,
                     
)

#Change light to modeled light
data.moos.mm.all$solar.time <- as.POSIXct(data.moos.mm.all$solar.time, tz = "UTC")

data.moos.mm.all$light <-  calc_light(data.moos.mm.all$solar.time, 64.7141530797916, -147.053919481172)



data.moos.mm.all <- data.moos.mm.all %>%  rename(DO.sat = DO.sat.EXO)
data.moos.mm.all <- data.moos.mm.all %>% select(solar.time, DO.obs, DO.sat, light, discharge, depth, temp.water)

startTime <- as.POSIXct(Sys.time())
mm.test.moos <- metab(bayes_specs, data=data.moos.mm.all)
endTime <- as.POSIXct(Sys.time())
# 
difftime(endTime, startTime)
save(mm.test.moos, file = here("Outputs", "moose2019-Run_2023-Full.rerun.02.04.RData"))
# 
fit.moos <- get_fit(mm.test.moos)
fit.daily <- fit.moos$daily
write.csv(fit.daily, here("outputs", "moose2019-Run_2023-Full.rerun.02.04.csv"))

model_data <- get_data(mm.test.moos)

######### Plot


gpp <- ggplot(data=fit.daily, aes(x=date, y=GPP_mean)) + geom_point(color = "chartreuse4") + geom_errorbar(aes(ymin=GPP_mean-GPP_sd, ymax=GPP_mean+GPP_sd), width=.2, position=position_dodge(0.05), color = "chartreuse4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ ggtitle("Moose2019")+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) +ylim(0, max(fit.daily$GPP_mean))

er <- ggplot(data = fit.daily, aes(x = date)) +geom_point(aes(y = ER_mean), color = "firebrick3") + geom_errorbar(aes(ymin=ER_mean-ER_sd, ymax=ER_mean+ER_sd), width=.2, position=position_dodge(0.05), color = "firebrick3")+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20)) +ylim(min(fit.daily$ER_daily_mean), 0)

k600 <- ggplot(data=fit.daily, aes(x=date, y=K600_daily_mean)) + geom_point(color = "orange") + geom_errorbar(aes(ymin=K600_daily_mean-K600_daily_sd, ymax=K600_daily_mean+K600_daily_sd), width=.2, position=position_dodge(0.05), color = "orange") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) 

rhat <- ggplot(data=fit.daily, aes(x=date)) + geom_point(aes(y=GPP_Rhat, colour = "GPP")) + geom_point(aes(y=as.numeric(ER_Rhat), colour = "ER")) + geom_point(aes(y=K600_daily_Rhat, colour = "K600")) +
  labs(y = "RHAT") + theme(legend.position="top") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ geom_hline(yintercept=1.01, color = "dark blue")+ geom_hline(yintercept=1.1, color = "dark red", linetype = "dashed")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))

er_k <- ggplot(data=fit.daily, aes(x=ER_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))


q.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=discharge), color = "sienna4", size=0.4)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("Discharge ( ", m^3, s^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ylim(0, max(model_data$discharge))

T.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=temp.water), color = "slateblue4", size=0.4)+ theme(axis.title.x=element_blank() )+ labs(y = "Temperature (°C)")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+ylim(-0.004, 20)



moos.DO.pred <- predict_DO(mm.test.moos)
moos.DO.pred <- na.omit(moos.DO.pred)
lm(DO.obs~ DO.mod, data = moos.DO.pred)
moos.DO.pred$SM.date <- as.Date(moos.DO.pred$solar.time - 4*60*60)
moos.DO.pred$SM.date <- as.factor(moos.DO.pred$SM.date)
fit_model <- function(x, y) summary(lm(x ~ y))$adj.r.squared
test.run <- moos.DO.pred %>% group_by(SM.date) %>% summarise(adj.R2 = fit_model(DO.obs,DO.mod))
moos.DO.pred <- full_join(moos.DO.pred, test.run)
rsq <-  summary(lm(moos.DO.pred$DO.mod~moos.DO.pred$DO.obs))$r.squared

p1 <- ggplot(moos.DO.pred, aes(x = solar.time)) + geom_point(aes(y=DO.obs, colour = "Observed DO"), color = "darkcyan") + geom_line(aes(y = DO.mod, colour= "Modeled DO"), color = "blue4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() ) + labs(colour="Green", y = "DO (mg/L)") +  theme(legend.position="none") + ggtitle("Observed (points) and Modeled (lines)")

p2 <- ggplot(moos.DO.pred, aes(x = solar.time)) + geom_point(aes(y=adj.R2))


library(grid)
library(ggeffects)
library(gridExtra)
library(gtable)
# If that doesnt do it, add grid
# Make each of the plots

# Two-panel figure: effect sizes
panA <- ggplotGrob(gpp)
panB <- ggplotGrob(er)
panK600 <- ggplotGrob(k600)
panC <- ggplotGrob(q.fig)
panD <- ggplotGrob(T.fig)
panE <- ggplotGrob(p1)
panp2 <- ggplotGrob(p2)
panF <- ggplotGrob(rhat)
panG <- ggplotGrob(er_k)
grid::grid.newpage()
grid.draw(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF, panG,size="max"))
fig2019 <- arrangeGrob(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF,panG, size="max"))
ggsave(path = here("outputs"), file = "moos2019.pdf", fig2019, width = 30, height = 55, units = "in", limitsize = FALSE)







rm(list=ls())








######### moos
data.moos.mm.all <- read.csv(here("outputs", "clean.moos.2020.full.csv"))


#change Discharge to m3/s
data.moos.mm.all$discharge <- data.moos.mm.all$discharge / 1000

bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name,
                     burnin_steps=8000, saved_steps=1500, n_cores=4,
                     GPP_daily_lower = 0, ER_daily_upper = 0,
                     
)

#Change light to modeled light
data.moos.mm.all$solar.time <- as.POSIXct(data.moos.mm.all$solar.time, tz = "UTC")

data.moos.mm.all$light <-  calc_light(data.moos.mm.all$solar.time, 64.7141530797916, -147.053919481172)



data.moos.mm.all <- data.moos.mm.all %>%  rename(DO.sat = DO.sat.EXO)
data.moos.mm.all <- data.moos.mm.all %>% select(solar.time, DO.obs, DO.sat, light, discharge, depth, temp.water)

startTime <- as.POSIXct(Sys.time())
mm.test.moos <- metab(bayes_specs, data=data.moos.mm.all)
endTime <- as.POSIXct(Sys.time())
# 
difftime(endTime, startTime)
save(mm.test.moos, file = here("Outputs", "moose2020-Run_2023-Full.rerun.02.04.RData"))
# 
fit.moos <- get_fit(mm.test.moos)
fit.daily <- fit.moos$daily
write.csv(fit.daily, here("outputs", "moose2020-Run_2023-Full.rerun.02.04.csv"))

model_data <- get_data(mm.test.moos)

######### Plot


gpp <- ggplot(data=fit.daily, aes(x=date, y=GPP_mean)) + geom_point(color = "chartreuse4") + geom_errorbar(aes(ymin=GPP_mean-GPP_sd, ymax=GPP_mean+GPP_sd), width=.2, position=position_dodge(0.05), color = "chartreuse4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ ggtitle("Moose2020")+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) +ylim(0, max(fit.daily$GPP_mean))

er <- ggplot(data = fit.daily, aes(x = date)) +geom_point(aes(y = ER_mean), color = "firebrick3") + geom_errorbar(aes(ymin=ER_mean-ER_sd, ymax=ER_mean+ER_sd), width=.2, position=position_dodge(0.05), color = "firebrick3")+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20)) +ylim(min(fit.daily$ER_daily_mean), 0)

k600 <- ggplot(data=fit.daily, aes(x=date, y=K600_daily_mean)) + geom_point(color = "orange") + geom_errorbar(aes(ymin=K600_daily_mean-K600_daily_sd, ymax=K600_daily_mean+K600_daily_sd), width=.2, position=position_dodge(0.05), color = "orange") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) 

rhat <- ggplot(data=fit.daily, aes(x=date)) + geom_point(aes(y=GPP_Rhat, colour = "GPP")) + geom_point(aes(y=as.numeric(ER_Rhat), colour = "ER")) + geom_point(aes(y=K600_daily_Rhat, colour = "K600")) +
  labs(y = "RHAT") + theme(legend.position="top") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ geom_hline(yintercept=1.01, color = "dark blue")+ geom_hline(yintercept=1.1, color = "dark red", linetype = "dashed")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))

er_k <- ggplot(data=fit.daily, aes(x=ER_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))


q.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=discharge), color = "sienna4", size=0.4)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("Discharge ( ", m^3, s^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ylim(0, max(model_data$discharge))

T.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=temp.water), color = "slateblue4", size=0.4)+ theme(axis.title.x=element_blank() )+ labs(y = "Temperature (°C)")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+ylim(-0.004, 20)



moos.DO.pred <- predict_DO(mm.test.moos)
moos.DO.pred <- na.omit(moos.DO.pred)
lm(DO.obs~ DO.mod, data = moos.DO.pred)
moos.DO.pred$SM.date <- as.Date(moos.DO.pred$solar.time - 4*60*60)
moos.DO.pred$SM.date <- as.factor(moos.DO.pred$SM.date)
fit_model <- function(x, y) summary(lm(x ~ y))$adj.r.squared
test.run <- moos.DO.pred %>% group_by(SM.date) %>% summarise(adj.R2 = fit_model(DO.obs,DO.mod))
moos.DO.pred <- full_join(moos.DO.pred, test.run)
rsq <-  summary(lm(moos.DO.pred$DO.mod~moos.DO.pred$DO.obs))$r.squared

p1 <- ggplot(moos.DO.pred, aes(x = solar.time)) + geom_point(aes(y=DO.obs, colour = "Observed DO"), color = "darkcyan") + geom_line(aes(y = DO.mod, colour= "Modeled DO"), color = "blue4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() ) + labs(colour="Green", y = "DO (mg/L)") +  theme(legend.position="none") + ggtitle("Observed (points) and Modeled (lines)")

p2 <- ggplot(moos.DO.pred, aes(x = solar.time)) + geom_point(aes(y=adj.R2))


library(grid)
library(ggeffects)
library(gridExtra)
library(gtable)
# If that doesnt do it, add grid
# Make each of the plots

# Two-panel figure: effect sizes
panA <- ggplotGrob(gpp)
panB <- ggplotGrob(er)
panK600 <- ggplotGrob(k600)
panC <- ggplotGrob(q.fig)
panD <- ggplotGrob(T.fig)
panE <- ggplotGrob(p1)
panp2 <- ggplotGrob(p2)
panF <- ggplotGrob(rhat)
panG <- ggplotGrob(er_k)
grid::grid.newpage()
grid.draw(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF, panG,size="max"))
fig2020 <- arrangeGrob(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF,panG, size="max"))
ggsave(path = here("outputs"), file = "moos2020.pdf", fig2020, width = 30, height = 55, units = "in", limitsize = FALSE)







rm(list=ls())








######### moos
data.moos.mm.all <- read.csv(here("outputs", "clean.moos.2021.full.csv"))


#change Discharge to m3/s
data.moos.mm.all$discharge <- data.moos.mm.all$discharge / 1000
bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name,
                     burnin_steps=8000, saved_steps=1500, n_cores=4,
                     GPP_daily_lower = 0, ER_daily_upper = 0,
                     
)

#Change light to modeled light
data.moos.mm.all$solar.time <- as.POSIXct(data.moos.mm.all$solar.time, tz = "UTC")

data.moos.mm.all$light <-  calc_light(data.moos.mm.all$solar.time, 64.7141530797916, -147.053919481172)



data.moos.mm.all <- data.moos.mm.all %>%  rename(DO.sat = DO.sat.EXO)
data.moos.mm.all <- data.moos.mm.all %>% select(solar.time, DO.obs, DO.sat, light, discharge, depth, temp.water)

startTime <- as.POSIXct(Sys.time())
mm.test.moos <- metab(bayes_specs, data=data.moos.mm.all)
endTime <- as.POSIXct(Sys.time())
# 
difftime(endTime, startTime)
save(mm.test.moos, file = here("Outputs", "moose2021-Run_2023-Full.rerun.02.04.RData"))
# 
fit.moos <- get_fit(mm.test.moos)
fit.daily <- fit.moos$daily
write.csv(fit.daily, here("outputs", "moose2021-Run_2023-Full.rerun.02.04.csv"))

model_data <- get_data(mm.test.moos)

######### Plot


gpp <- ggplot(data=fit.daily, aes(x=date, y=GPP_mean)) + geom_point(color = "chartreuse4") + geom_errorbar(aes(ymin=GPP_mean-GPP_sd, ymax=GPP_mean+GPP_sd), width=.2, position=position_dodge(0.05), color = "chartreuse4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ ggtitle("Moose2021")+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) +ylim(0, max(fit.daily$GPP_mean))

er <- ggplot(data = fit.daily, aes(x = date)) +geom_point(aes(y = ER_mean), color = "firebrick3") + geom_errorbar(aes(ymin=ER_mean-ER_sd, ymax=ER_mean+ER_sd), width=.2, position=position_dodge(0.05), color = "firebrick3")+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20)) +ylim(min(fit.daily$ER_daily_mean), 0)

k600 <- ggplot(data=fit.daily, aes(x=date, y=K600_daily_mean)) + geom_point(color = "orange") + geom_errorbar(aes(ymin=K600_daily_mean-K600_daily_sd, ymax=K600_daily_mean+K600_daily_sd), width=.2, position=position_dodge(0.05), color = "orange") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) 

rhat <- ggplot(data=fit.daily, aes(x=date)) + geom_point(aes(y=GPP_Rhat, colour = "GPP")) + geom_point(aes(y=as.numeric(ER_Rhat), colour = "ER")) + geom_point(aes(y=K600_daily_Rhat, colour = "K600")) +
  labs(y = "RHAT") + theme(legend.position="top") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ geom_hline(yintercept=1.01, color = "dark blue")+ geom_hline(yintercept=1.1, color = "dark red", linetype = "dashed")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))

er_k <- ggplot(data=fit.daily, aes(x=ER_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))


q.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=discharge), color = "sienna4", size=0.4)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("Discharge ( ", m^3, s^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ylim(0, max(model_data$discharge))

T.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=temp.water), color = "slateblue4", size=0.4)+ theme(axis.title.x=element_blank() )+ labs(y = "Temperature (°C)")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+ylim(-0.004, 20)



moos.DO.pred <- predict_DO(mm.test.moos)
moos.DO.pred <- na.omit(moos.DO.pred)
lm(DO.obs~ DO.mod, data = moos.DO.pred)
moos.DO.pred$SM.date <- as.Date(moos.DO.pred$solar.time - 4*60*60)
moos.DO.pred$SM.date <- as.factor(moos.DO.pred$SM.date)
fit_model <- function(x, y) summary(lm(x ~ y))$adj.r.squared
test.run <- moos.DO.pred %>% group_by(SM.date) %>% summarise(adj.R2 = fit_model(DO.obs,DO.mod))
moos.DO.pred <- full_join(moos.DO.pred, test.run)
rsq <-  summary(lm(moos.DO.pred$DO.mod~moos.DO.pred$DO.obs))$r.squared

p1 <- ggplot(moos.DO.pred, aes(x = solar.time)) + geom_point(aes(y=DO.obs, colour = "Observed DO"), color = "darkcyan") + geom_line(aes(y = DO.mod, colour= "Modeled DO"), color = "blue4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() ) + labs(colour="Green", y = "DO (mg/L)") +  theme(legend.position="none") + ggtitle("Observed (points) and Modeled (lines)")

p2 <- ggplot(moos.DO.pred, aes(x = solar.time)) + geom_point(aes(y=adj.R2))


library(grid)
library(ggeffects)
library(gridExtra)
library(gtable)
# If that doesnt do it, add grid
# Make each of the plots

# Two-panel figure: effect sizes
panA <- ggplotGrob(gpp)
panB <- ggplotGrob(er)
panK600 <- ggplotGrob(k600)
panC <- ggplotGrob(q.fig)
panD <- ggplotGrob(T.fig)
panE <- ggplotGrob(p1)
panp2 <- ggplotGrob(p2)
panF <- ggplotGrob(rhat)
panG <- ggplotGrob(er_k)
grid::grid.newpage()
grid.draw(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF, panG,size="max"))
fig2021 <- arrangeGrob(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF,panG, size="max"))
ggsave(path = here("outputs"), file = "moos2021.pdf", fig2021, width = 30, height = 55, units = "in", limitsize = FALSE)







rm(list=ls())








######### moos
data.moos.mm.all <- read.csv(here("outputs", "clean.moos.2022.full.csv"))


#change Discharge to m3/s
data.moos.mm.all$discharge <- data.moos.mm.all$discharge / 1000


bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name,
                     burnin_steps=8000, saved_steps=1500, n_cores=4,
                     GPP_daily_lower = 0, ER_daily_upper = 0,
                     
)


#Change light to modeled light
data.moos.mm.all$solar.time <- as.POSIXct(data.moos.mm.all$solar.time, tz = "UTC")

data.moos.mm.all$light <-  calc_light(data.moos.mm.all$solar.time, 64.7141530797916, -147.053919481172)



data.moos.mm.all <- data.moos.mm.all %>%  rename(DO.sat = DO.sat.EXO)
data.moos.mm.all <- data.moos.mm.all %>% select(solar.time, DO.obs, DO.sat, light, discharge, depth, temp.water)

startTime <- as.POSIXct(Sys.time())
mm.test.moos <- metab(bayes_specs, data=data.moos.mm.all)
endTime <- as.POSIXct(Sys.time())
# 
difftime(endTime, startTime)
save(mm.test.moos, file = here("Outputs", "moose2022-Run_2023-Full.rerun.02.04.RData"))
# 
fit.moos <- get_fit(mm.test.moos)
fit.daily <- fit.moos$daily
write.csv(fit.daily, here("outputs", "moose2022-Run_2023-Full.rerun.02.04.csv"))

model_data <- get_data(mm.test.moos)

######### Plot


gpp <- ggplot(data=fit.daily, aes(x=date, y=GPP_mean)) + geom_point(color = "chartreuse4") + geom_errorbar(aes(ymin=GPP_mean-GPP_sd, ymax=GPP_mean+GPP_sd), width=.2, position=position_dodge(0.05), color = "chartreuse4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ ggtitle("Moose2022")+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) +ylim(0, max(fit.daily$GPP_mean))

er <- ggplot(data = fit.daily, aes(x = date)) +geom_point(aes(y = ER_mean), color = "firebrick3") + geom_errorbar(aes(ymin=ER_mean-ER_sd, ymax=ER_mean+ER_sd), width=.2, position=position_dodge(0.05), color = "firebrick3")+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20)) +ylim(min(fit.daily$ER_daily_mean), 0)

k600 <- ggplot(data=fit.daily, aes(x=date, y=K600_daily_mean)) + geom_point(color = "orange") + geom_errorbar(aes(ymin=K600_daily_mean-K600_daily_sd, ymax=K600_daily_mean+K600_daily_sd), width=.2, position=position_dodge(0.05), color = "orange") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold")) 

rhat <- ggplot(data=fit.daily, aes(x=date)) + geom_point(aes(y=GPP_Rhat, colour = "GPP")) + geom_point(aes(y=as.numeric(ER_Rhat), colour = "ER")) + geom_point(aes(y=K600_daily_Rhat, colour = "K600")) +
  labs(y = "RHAT") + theme(legend.position="top") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ geom_hline(yintercept=1.01, color = "dark blue")+ geom_hline(yintercept=1.1, color = "dark red", linetype = "dashed")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))

er_k <- ggplot(data=fit.daily, aes(x=ER_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("ER (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (", d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))


q.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=discharge), color = "sienna4", size=0.4)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("Discharge ( ", m^3, s^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+ylim(0, max(model_data$discharge))

T.fig <- ggplot(data = model_data, aes(x = solar.time)) +geom_point(aes(y=temp.water), color = "slateblue4", size=0.4)+ theme(axis.title.x=element_blank() )+ labs(y = "Temperature (°C)")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+ylim(-0.004, 20)



moos.DO.pred <- predict_DO(mm.test.moos)
moos.DO.pred <- na.omit(moos.DO.pred)
lm(DO.obs~ DO.mod, data = moos.DO.pred)
moos.DO.pred$SM.date <- as.Date(moos.DO.pred$solar.time - 4*60*60)
moos.DO.pred$SM.date <- as.factor(moos.DO.pred$SM.date)
fit_model <- function(x, y) summary(lm(x ~ y))$adj.r.squared
test.run <- moos.DO.pred %>% group_by(SM.date) %>% summarise(adj.R2 = fit_model(DO.obs,DO.mod))
moos.DO.pred <- full_join(moos.DO.pred, test.run)
rsq <-  summary(lm(moos.DO.pred$DO.mod~moos.DO.pred$DO.obs))$r.squared

p1 <- ggplot(moos.DO.pred, aes(x = solar.time)) + geom_point(aes(y=DO.obs, colour = "Observed DO"), color = "darkcyan") + geom_line(aes(y = DO.mod, colour= "Modeled DO"), color = "blue4") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank() ) + labs(colour="Green", y = "DO (mg/L)") +  theme(legend.position="none") + ggtitle("Observed (points) and Modeled (lines)")

p2 <- ggplot(moos.DO.pred, aes(x = solar.time)) + geom_point(aes(y=adj.R2))


library(grid)
library(ggeffects)
library(gridExtra)
library(gtable)
# If that doesnt do it, add grid
# Make each of the plots

# Two-panel figure: effect sizes
panA <- ggplotGrob(gpp)
panB <- ggplotGrob(er)
panK600 <- ggplotGrob(k600)
panC <- ggplotGrob(q.fig)
panD <- ggplotGrob(T.fig)
panE <- ggplotGrob(p1)
panp2 <- ggplotGrob(p2)
panF <- ggplotGrob(rhat)
panG <- ggplotGrob(er_k)
grid::grid.newpage()
grid.draw(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF, panG,size="max"))
fig2022 <- arrangeGrob(rbind(panA, panB,panK600, panC, panD,panE,panp2,panF,panG, size="max"))
ggsave(path = here("outputs"), file = "moos2022.pdf", fig2022, width = 30, height = 55, units = "in", limitsize = FALSE)







rm(list=ls())






