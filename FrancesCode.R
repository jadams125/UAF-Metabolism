


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



######### poke

data.poke.mm.all <- read.csv(here("outputs", "poker.comb.readyforMetab.new.clean.csv"))

data.poke.mm.all <- data.poke.mm.all %>% filter(solar.time < "2019-12-30 04:13:57")

#change Discharge to m3/s
data.poke.mm.all$discharge <- data.poke.mm.all$discharge / 1000


testNode <- seq(from = min(log(data.poke.mm.all$discharge)), to = max(log(data.poke.mm.all$discharge)), by = (max(log(data.poke.mm.all$discharge))-min(log(data.poke.mm.all$discharge)))/(6))

Nodediffs = (testNode[2]-testNode[1])/2





bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)



bayes_specs <- specs(bayes_name,
                     burnin_steps=8000, saved_steps=1500, n_cores=4,
                     GPP_daily_lower = 0, ER_daily_upper = 0,K600_lnQ_nodes_centers = testNode,K600_lnQ_nodediffs_sdlog=Nodediffs
                     
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
save(mm.test.poke, file = here("Outputs", "poker2019-Run_2023-01-20_kbin.RData"))
# 
fit.poke <- get_fit(mm.test.poke)
fit.daily <- fit.poke$daily
write.csv(fit.daily, here("outputs", "poker2019-Run_2023-01-20_kbin.csv"))

model_data <- get_data(mm.test.poke)

