# neon metab prep

library(tidyverse)
library(neonUtilities)
library(streamMetabolizer)



NEON_water <-loadByProduct(dpID = "DP1.20288.001", site=c("CARI"),startdate="2019-01", enddate="2022-12",check.size=F)


#water temp 
NEON_water_temp <-loadByProduct(dpID = "DP1.20053.001", site=c("CARI"),startdate="2019-01", enddate="2022-12",check.size=F)


NEON_water_temp <- NEON_water_temp$TSW_5min

#Nitrate Code
# DP1.20033.001



NEON_water_data <- NEON_water$waq_instantaneous


#neon DO data is suspect - need better data on calibration


#Discharge 
NEON_Q <-loadByProduct(dpID = "DP4.00130.001", site=c("CARI"),startdate="2019-01", enddate="2022-12",check.size=F,package="expanded")


NEON_Q_cont <- NEON_Q$csd_continuousDischarge





NEON_water_data %>% select(dissolvedOxygen, seaLevelDissolvedOxygenSat, localDissolvedOxygenSat, turbidity, fDOM, )