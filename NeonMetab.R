# neon metab prep

library(tidyverse)
library(neonUtilities)
library(streamMetabolizer)



NEON_water <-loadByProduct(dpID = "DP1.20288.001", site=c("CARI"),startdate="2022-01", enddate="2022-12")


NEON_water_data <- NEON_water$waq_instantaneous

