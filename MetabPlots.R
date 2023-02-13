
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

poke19 <- read.csv(here("outputs", "poker2019-Run_2023-01-09.csv"))
strt19 <- read.csv(here("outputs", "stuart2019-Run_2023-01-09.csv"))
frch19 <- read.csv(here("outputs", "french2019-Run_2023-01-09.csv"))
moos19 <- read.csv(here("outputs", "moose2019-Run_2023-01-09.csv"))
vaul19 <- read.csv(here("outputs", "vault2019-Run_2023-01-09.csv"))

poke20 <- read.csv(here("outputs", "poker2020-Run_2023-01-09.csv"))
strt20 <- read.csv(here("outputs", "stuart2020-Run_2023-01-09.csv"))
frch20 <- read.csv(here("outputs", "french2020-Run_2023-01-09.csv"))
moos20 <- read.csv(here("outputs", "moose2020-Run_2023-01-09.csv"))
vaul20 <- read.csv(here("outputs", "vault2020-Run_2023-01-09.csv"))


poke21 <- read.csv(here("outputs", "poker2021-Run_2023-01-09.csv"))
strt21 <- read.csv(here("outputs", "stuart2021-Run_2023-01-09.csv"))
frch21 <- read.csv(here("outputs", "french2021-Run_2023-01-09.csv"))
moos21 <- read.csv(here("outputs", "moose2021-Run_2023-01-09.csv"))
vaul21 <- read.csv(here("outputs", "vault2021-Run_2023-01-09.csv"))


poke <- rbind(poke19,poke20,poke21)
strt <- rbind(strt19,strt20,strt21)
frch <- rbind(frch19,frch20,frch21)
moos <- rbind(moos19,moos20,moos21)
vaul <- rbind(vaul19,vaul20,vaul21)

poke$site <- "POKE"
strt$site <- "STRT"
frch$site <- "FRCH"
moos$site <- "MOOS"
vaul$site <- "VAUL"

poke$burn <- "burned"
strt$burn <- "burned"
frch$burn <- "unburned"
moos$burn <- "burned"
vaul$burn <- "unburned"


poke$PF <- "Low"
strt$PF <- "High"
frch$PF <- "Low"
moos$PF <- "Low"
vaul$PF <- "High"







metab_all <- rbind(poke,strt,frch,moos,vaul)

metab_all$year <- year(as.POSIXct(metab_all$date))

metab_all$year <- format(metab_all$year,format="%y")

test_means_GPP <- aggregate( GPP_mean ~ year + site + PF + burn, metab_all , mean )

test_means_ER <- aggregate( ER_mean ~ year + site + PF + burn, metab_all , mean )

test_means_NEP <- aggregate( (ER_mean+GPP_mean) ~ year + site + PF + burn, metab_all , mean )









metab_all$year <- as.factor(metab_all$year)
metab_all$burn <- as.factor(metab_all$burn)
metab_all$PF <- as.factor(metab_all$PF)


model.test <- lm(data = metab_all, ER_mean~K600_daily_mean)

plot(GPP_mean~burn+year, data =metab_all)


library(lme4)
abline(model.test)

burn_model <- lmer(data = metab_all, GPP_mean~burn +(1| year))

plot(ER_mean~burn, data =metab_all)



summary(burn_model)



test_means_GPP %>% ggplot(aes(x= burn, y= GPP_mean, color = site))+geom_boxplot()+theme(axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("Mean GPP (g ", O[2] ," ", m^-2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+scale_color_hue(labels = c("French", "Moose", "Poker", "Stuart", "Vault"))

test_means_GPP %>% ggplot(aes(x= burn, y= GPP_mean))+geom_boxplot()+theme(axis.ticks.x=element_blank(), axis.title.x=element_blank() )+ labs(y = expression(paste("Mean GPP (g ", O[2] ," ", m^-2, d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))


test_means_GPP %>% ggplot(aes(x= PF, y= GPP_mean, color = site))+geom_boxplot()

test_means_GPP %>% ggplot(aes(x= PF, y= GPP_mean))+geom_boxplot()




DT <- data.table(test_means_GPP) %>% select(GPP_mean, burn)

wide <- setnames(DT[, sapply(.SD, function(x) list(mean=(mean(x)), sd=(sd(x)))), by=burn], c("burn", sapply(names(DT)[-1], paste0, c(".GPP", ".GPP.SD"))))

wide %>% ggplot(aes(x= burn, y= burn.GPP))+geom_point()+ geom_errorbar(aes(ymin=burn.GPP-burn.GPP.SD, ymax=burn.GPP+burn.GPP.SD))






DT <- data.table(test_means_GPP) %>% select(GPP_mean, PF)

wide <- setnames(DT[, sapply(.SD, function(x) list(mean=(mean(x)), sd=(sd(x)))), by=PF], c("PF", sapply(names(DT)[-1], paste0, c(".GPP", ".GPP.SD"))))

wide %>% ggplot(aes(x= PF, y= PF.GPP))+geom_point()+ geom_errorbar(aes(ymin=PF.GPP-PF.GPP.SD, ymax=PF.GPP+PF.GPP.SD))








metab_all$date <- as.Date(metab_all$date)
metab_all$doy <- lubridate::yday(metab_all$date)



metab_all %>% ggplot(aes(x= doy, y= GPP_mean, color = burn))+geom_point()






test_means_ER %>% ggplot(aes(x= burn, y= ER_mean, color = site))+geom_boxplot()

test_means_ER %>% ggplot(aes(x= burn, y= ER_mean))+geom_boxplot()


test_means_ER %>% ggplot(aes(x= PF, y= ER_mean, color = site))+geom_boxplot()


test_means_ER %>% ggplot(aes(x= PF, y= ER_mean))+geom_boxplot()



DT <- data.table(test_means_ER) %>% select(ER_mean, burn)

wide <- setnames(DT[, sapply(.SD, function(x) list(mean=(mean(x)), sd=(sd(x)))), by=burn], c("burn", sapply(names(DT)[-1], paste0, c(".ER", ".ER.SD"))))

wide %>% ggplot(aes(x= burn, y= burn.ER))+geom_point()+ geom_errorbar(aes(ymin=burn.ER-burn.ER.SD, ymax=burn.ER+burn.ER.SD))






DT <- data.table(test_means_ER) %>% select(ER_mean, PF)

wide <- setnames(DT[, sapply(.SD, function(x) list(mean=(mean(x)), sd=(sd(x)))), by=PF], c("PF", sapply(names(DT)[-1], paste0, c(".ER", ".ER.SD"))))

wide %>% ggplot(aes(x= PF, y= PF.ER))+geom_point()+ geom_errorbar(aes(ymin=PF.ER-PF.ER.SD, ymax=PF.ER+PF.ER.SD))






test_means_NEP <- test_means_NEP %>% rename(NEP_mean = "(ER_mean + GPP_mean)")


test_means_NEP%>% ggplot(aes(x= burn, y= NEP_mean, color = site))+geom_boxplot()

test_means_NEP%>% ggplot(aes(x= burn, y= NEP_mean))+geom_boxplot()


test_means_NEP%>% ggplot(aes(x= PF, y= NEP_mean, color = site))+geom_boxplot()


test_means_NEP%>% ggplot(aes(x= PF, y= NEP_mean))+geom_boxplot()



DT <- data.table(test_means_NEP) %>% select(NEP_mean, burn)

wide <- setnames(DT[, sapply(.SD, function(x) list(mean=(mean(x)), sd=(sd(x)))), by=burn], c("burn", sapply(names(DT)[-1], paste0, c(".NEP", ".NEP.SD"))))

wide %>% ggplot(aes(x= burn, y= burn.NEP))+geom_point()+ geom_errorbar(aes(ymin=burn.NEP-burn.NEP.SD, ymax=burn.NEP+burn.NEP.SD))






DT <- data.table(test_means_NEP) %>% select(NEP_mean, PF)

wide <- setnames(DT[, sapply(.SD, function(x) list(mean=(mean(x)), sd=(sd(x)))), by=PF], c("PF", sapply(names(DT)[-1], paste0, c(".NEP", ".NEP.SD"))))

wide %>% ggplot(aes(x= PF, y= PF.NEP))+geom_point()+ geom_errorbar(aes(ymin=PF.NEP-PF.NEP.SD, ymax=PF.NEP+PF.NEP.SD))
