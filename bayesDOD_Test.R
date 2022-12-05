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

#trial Bayes metab run


bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)
bayes_name

bayes_name_AppEtAL <- "b_Kb_oipi_tr_plrckm.stan"
bayes_name_AppEtAL

bayes_specs <- specs(bayes_name, 
                     burnin_steps=9000, saved_steps=5000, n_cores=8, n_chains = 4 
                     # GPP_daily_lower = 0, ER_daily_upper = 0
)


#test Data

dat <- data_metab(num_days='3', res='15', day_start=4, day_end=28)


mm.test.dat <- metab(bayes_specs, data=dat)

test_dat <- read_rds("C:/Users/jadams125/Documents/data_real.Rds")
test_dat24 <- read_rds("C:/Users/jadams125/Documents/data_realfour.Rds")
nwis_06893820 <- test_dat24$nwis_06893820



mm.test.NWIS <- metab(bayes_specs, data=nwis_06893820)




data1 <- test_dat$data_daily
data2 <- test_dat$data




mcmc <- get_mcmc(mm.test.NWIS)
output33 <- rstan::traceplot(mcmc, pars='K600_daily', nrow=3)
png(output33, here("outputs", "testTrace.png"))




err.sig <- data.frame(c(model.id=nms, r2=overall_rsq, get_fit(bayes_fit)$overall))
k.daily.sig<- data.frame(c(model.id=nms, bayes_fit@fit[["KQ_overall"]]))




ess_bulk(fit$inst)



# 
# #salcha
# 
# here::here()
# 
# salcha2021 <- read.csv(here::here("salcha2021.csv"))
# salcha2021$solar.time <- as.POSIXct(salcha2021$solar.time, tz = "UTC")
# 
# 
# 
# 
# data.salcha.mm <- na.omit(salcha2021) %>%
#   select(solar.time, depth, DO.obs, DO.sat, temp.water, light, discharge)
# 
# 
# as.numeric(calc_light(data.salcha.mm$solar.time, 64.47153, -146.9281))
# data.salcha.mm$light <- format(data.salcha.mm$light, scientific = F)
# str(data.salcha.mm)
# 
# 
# data.salcha.mm$discharge <- as.numeric(data.salcha.mm$discharge)
# data.salcha.mm$light <- as.numeric(data.salcha.mm$light)
# 
# 
# 
# mm.test.salcha <- metab(bayes_specs, data=data.salcha.mm)
# 
# 

#MOOS

MOOS.comb <- read.csv(here("outputs", "moos.comb.csv"))
MOOS.comb$solar.time <- as.POSIXct(MOOS.comb$solar.time, tz = "UTC")

# moos.2019 <-  MOOS.comb %>% filter(datetimeAK < "2019-12-31 00:00:00")

data.moos.mm <- na.omit(MOOS.comb) %>%
  select(solar.time, depth, DO.obs, DO.sat, temp.water, light, discharge)





mm.test.moos <- metab(bayes_specs, data=data.moos.mm.19)


# # Warning message:
# In metab_fun(specs = specs, data = data, data_daily = data_daily,  :
#                Modeling failed
#              Warnings:
#                There were 4 chains where the estimated Bayesian Fraction of Missing Information was low. See
#              https://mc-stan.org/misc/warnings.html#bfmi-low
#              Examine the pairs() plot to diagnose sampling problems  Errors:
#                cannot allocate vector of size 8 Kb

Predict.moos <- predict_metab(mm.test.moos)
fit.moos <- get_fit(mm.test.moos)
fit.moos

pairs(fit.moos)

saveRDS(fit.moos, "outputs/moos.fit.rds")

fit.moos.df <- data.frame(fit.moos)

#save fits
daily.fit.moos <- fit.moos$daily
write.csv(daily.fit.moos,"outputs/daily.fit.moos.csv", row.names = FALSE)

overall.fit.moos <- fit.moos$overall
write.csv(overall.fit.moos,"outputs/overall.fit.moos.csv", row.names = FALSE)

inst.fit.moos <-  fit.moos$inst
write.csv(inst.fit.moos,"outputs/inst.fit.moos.csv", row.names = FALSE)

KQ_overall.moos <- fit.moos$KQ_overall
write.csv(KQ_overall.moos,"outputs/KQ_overall.moos.csv", row.names = FALSE)

KQ_binned.moos <- fit.moos$KQ_binned
write.csv(KQ_binned.moos,"outputs/KQ_binned.moos.csv", row.names = FALSE)

moos.fit.warnings <- fit.moos$warnings
writeLines(moos.fit.warnings, "outputs/moos.fit.warnings.txt")

moos.fit.errors <- fit.moos$errors
writeLines(moos.fit.errors, "outputs/moos.fit.errors.txt")


get_fitting_time(mm.test.moos)

do.preds.moos <- predict_DO(mm.test.moos)

png("plots/DO_preds_moos.png", width = 2000, height = 2000, res = 300)
plot_DO_preds(predict_DO(mm.test.moos))
dev.off()



moos.metab.plot.1921 <- plot_metab_preds(mm.test.moos, style = "ggplot2")
moos.metab.plot.1921 + labs(title="moos metab 2019- 2021")

str(mm.test.moos)
view(mm.test.moos)

Predict.moos <- data.frame(Predict.moos)
write.csv(Predict.moos,"outputs/moos.metab.1921.csv", row.names = FALSE)




#POKE
POKE.comb <- read.csv(here("outputs", "poke.comb.csv"))
data.poke.mm <- na.omit(POKE.comb) %>%
  select(solar.time, depth, DO.obs, DO.sat, temp.water, light, discharge)

data.poke.mm$solar.time <- as.POSIXct(data.poke.mm$solar.time, tz = "UTC",)

mm.test.poke <- metab(bayes_specs, data=data.poke.mm)

Predict.poke <- predict_metab(mm.test.poke)
poke.metab.plot.1921 <- plot_metab_preds(mm.test.poke, style = "ggplot2")
poke.metab.plot.1921 + labs(title="poke metab 2019- 2021")

str(mm.test.poke)
view(mm.test.poke)

Predict.poke <- data.frame(Predict.poke)
write.csv(Predict.poke,"outputs/poke.metab.1921.csv", row.names = FALSE)


#FRCH
setwd(here())
FRCH.comb <- read.csv(here("outputs", "frch.comb.csv"))

FRCH.comb <- FRCH.comb %>% filter(datetimeAK >= "2019-08-01 04:13:57" & datetimeAK < "2019-09-01 04:13:57")

anyNA(FRCH.comb, recursive = TRUE)
lapply(FRCH.comb, summary)


FRCH.comb$temp.water <- na_kalman(FRCH.comb$temp.water)
FRCH.comb$ODO.Psat <- na_kalman(FRCH.comb$ODO.Psat)
FRCH.comb$DO.obs <- na_kalman(FRCH.comb$DO.obs)
FRCH.comb$DO.sat <- na_kalman(FRCH.comb$DO.sat)
FRCH.comb$DO.sat <- na_kalman(FRCH.comb$DO.sat)

anyNA(FRCH.comb, recursive = TRUE)


FRCH.comb$datetimeAK <- force_tz(as.POSIXct(FRCH.comb$datetimeAK), "America/Anchorage")

FRCH.comb$solar.time <- calc_solar_time(as.POSIXct(FRCH.comb$datetimeAK), longitude = -146.916364)


data.frch.mm <- FRCH.comb %>%
  select(solar.time, depth, DO.obs, DO.sat, temp.water, light, discharge)

anyNA(data.frch.mm, recursive = TRUE)

data.frch.mm$solar.time <- as.POSIXct(data.frch.mm$solar.time, tz = "UTC")

# parMod <- calc_light(data.frch.mm$solar.time, 64.60667379, -146.916364)
# 
# 
# 
# 
# timebounds <- as.POSIXct(c('2019-08-01 00:00:00', '2019-07-01 00:00:00'), tz= "America/Anchorage")
# coords <- list(lat=64.60667379, lon=-146.916364)
# PAR.obs <- data.frch.mm %>% 
#   select(solar.time, light)
# 
# 
# lightmerged <- calc_light_merged( PAR.obs, solar.time = data.frch.mm$solar.time, 64.60667379, -146.916364)
# 
# data.frch.mm <- data.frch.mm %>%
#   dplyr::rename(light.obs = light)
# 
# 
# data.frch.mm <- merge(lightmerged, data.frch.mm, by = "solar.time")
# 
# data.frch.mm <- na.omit(data.frch.mm) %>%
#   select(solar.time, depth, DO.obs, DO.sat, temp.water, light, discharge)
# 

#run it



bayes_specs <- specs(bayes_name, 
                     burnin_steps=9000, saved_steps=5000, n_cores=8, n_chains = 4 
                     # GPP_daily_lower = 0, ER_daily_upper = 0
)

mm.test.frch <- metab(bayes_specs, data=data.frch.mm)






fit.frch <- get_fit(mm.test.frch)

fit.daily <- fit.frch$daily



write.csv(fit.frch$daily, here("outputs", "frchJuly2019_metrics.csv"))


frch.metab.plot.19july <- plot_metab_preds(mm.test.frch, style = "ggplot2")


gpp <- ggplot(data=fit.frch$daily, aes(x=date, y=GPP_mean)) + geom_point(color = "chartreuse4") + geom_errorbar(aes(ymin=GPP_mean-GPP_sd, ymax=GPP_mean+GPP_sd), width=.2, position=position_dodge(0.05), color = "chartreuse4")

gpp

er <- ggplot(data=fit.frch$daily, aes(x=date, y=ER_mean)) + geom_point(color = "firebrick3") + geom_errorbar(aes(ymin=ER_mean-ER_sd, ymax=ER_mean+ER_sd), width=.2, position=position_dodge(0.05), color = "firebrick3") + ylim(-15, 1)


rhat <- ggplot(data=fit.frch$daily, aes(x=date)) + geom_point(aes(y=GPP_Rhat, colour = "GPP")) + geom_point(aes(y=ER_Rhat, colour = "ER")) +
  labs(y = "RHAT") + theme(legend.position="bottom")
rhat



grid.arrange(gpp, er,rhat, nrow=3)


e$daily

a <- ggplot(data=fit.daily, aes(x=GPP_mean, y=ER_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq() 



b <- ggplot(data=fit.daily, aes(x=GPP_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq() 
c <- ggplot(data=fit.daily, aes(x=ER_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq() 



plot_DO_preds(mm.test.frch)





# a_lm <- lm( GPP_mean ~ ER_mean ,data =fit.daily)
# summary(a_lm)
# 
# 
# b_lm <- lm( GPP_mean ~ K600_daily_mean ,data =fit.daily)
# summary(b_lm)
# 
# c_lm <- lm( ER_mean ~ K600_daily_mean ,data =fit.daily)
# summary(c_lm)

grid.arrange(a, b,c, ncol=3)









mcmc3 <- get_mcmc(mm.test.frch)
output77 <- rstan::traceplot(mcmc3, pars='K600_daily', nrow=3)
output77







Predict.frch <- predict_metab(mm.test.frch)
frch.metab.plot.1921 <- plot_metab_preds(mm.test.frch, style = "ggplot2")
frch.metab.plot.1921 + labs(title="frch metab 2019- 2021")

str(mm.test.frch)
view(mm.test.frch)

Predict.frch <- data.frame(Predict.frch)
write.csv(Predict.frch,"outputs/frch.metab.1921.csv", row.names = FALSE)




#VAUL
data.vaul.mm <- na.omit(VAUL.comb) %>%
  select(solar.time, depth, DO.obs, DO.sat, temp.water, light, discharge)

mm.test.vaul <- metab(bayes_specs, data=data.vaul.mm)

Predict.vaul <- predict_metab(mm.test.vaul)
vaul.metab.plot.1921 <- plot_metab_preds(mm.test.vaul, style = "ggplot2")
vaul.metab.plot.1921 + labs(title="vaul metab 2019- 2021")

str(Predict.vaul)
view(Predict.vaul)

setwd(here())
getwd()
here()

Predict.vaul <- data.frame(Predict.vaul)
write.csv(Predict.vaul,"outputs/vaul.metab.1921.csv", row.names = FALSE)



#STRT

setwd(here())
STRT.comb <- read.csv(here("outputs", "strt.comb.csv"))
data.strt.mm <- na.omit(STRT.comb) %>%
  select(solar.time, depth, DO.obs, DO.sat, temp.water, light, discharge)

data.strt.mm$solar.time <- as.POSIXct(data.strt.mm$solar.time, tz = "UTC")

#Try for just a month

data.strt.mm <- data.strt.mm %>% filter(solar.time < "2019-08-01 00:00:00" & solar.time >= "2019-07-01 00:00:00")

data.strt.mm$solar.time <- as.POSIXct(data.strt.mm$solar.time, tz = "UTC")

mm.test.strt <- metab(bayes_specs, data=data.strt.mm)

strtJulyFit <- get_fit(mm.test.strt)

strtJulyFit$warnings


mcmc2 <- get_mcmc(mm.test.strt)
output22 <- rstan::traceplot(mcmc2, pars='K600_daily', nrow=3)
png(output22, here("outputs", "testTrace.png"))




Predict.strt <- predict_metab(mm.test.strt)
strt.metab.plot.1921 <- plot_metab_preds(mm.test.strt, style = "ggplot2")
strt.metab.plot.1921 + labs(title="strt metab 2019- 2021")

str(mm.test.strt)
view(mm.test.strt)



