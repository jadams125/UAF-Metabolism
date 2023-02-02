######## GPP x K600 relationsip
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

# poker

poker2019 <- read.csv(here("outputs", "poker2019-Run_2023-01-09.csv"))

gpp_k_poker2019 <- ggplot(data=poker2019, aes(x=GPP_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (" ,d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))+ggtitle("poker2019")
gpp_k_poker2019

poker2020 <- read.csv(here("outputs", "poker2020-Run_2023-01-09.csv"))

gpp_k_poker2020 <- ggplot(data=poker2020, aes(x=GPP_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (" ,d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))+ggtitle("poker2020")
gpp_k_poker2020


poker2021 <- read.csv(here("outputs", "poker2021-Run_2023-01-09.csv"))

gpp_k_poker2021 <- ggplot(data=poker2021, aes(x=GPP_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (" ,d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))+ggtitle("poker2021")
gpp_k_poker2021


poker2022 <- read.csv(here("outputs", "poker2022-Run_2023-01-19_gapfillQ.csv"))

gpp_k_poker2022 <- ggplot(data=poker2022, aes(x=GPP_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (" ,d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))+ggtitle("poker2022")
gpp_k_poker2022



# Vault

vault2019 <- read.csv(here("outputs", "vault2019-Run_2023-01-09.csv"))

gpp_k_vault2019 <- ggplot(data=vault2019, aes(x=GPP_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (" ,d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))+ggtitle("vault2019")
gpp_k_vault2019

vault2020 <- read.csv(here("outputs", "vault2020-Run_2023-01-09.csv"))

gpp_k_vault2020 <- ggplot(data=vault2020, aes(x=GPP_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (" ,d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))+ggtitle("vault2020")
gpp_k_vault2020


vault2021 <- read.csv(here("outputs", "vault2021-Run_2023-01-09.csv"))

gpp_k_vault2021 <- ggplot(data=vault2021, aes(x=GPP_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (" ,d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))+ggtitle("vault2021")
gpp_k_vault2021

vault2022 <- read.csv(here("outputs", "vault2022-Run_2023-01-19_gapfillQ.csv"))

gpp_k_vault2022 <- ggplot(data=vault2022, aes(x=GPP_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (" ,d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))+ggtitle("vault2022")
gpp_k_vault2022


# french

french2019 <- read.csv(here("outputs", "french2019-Run_2023-01-09.csv"))

gpp_k_french2019 <- ggplot(data=french2019, aes(x=GPP_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (" ,d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))+ggtitle("french2019")
gpp_k_french2019

french2020 <- read.csv(here("outputs", "french2020-Run_2023-01-09.csv"))

gpp_k_french2020 <- ggplot(data=french2020, aes(x=GPP_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (" ,d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))+ggtitle("french2020")
gpp_k_french2020


french2021 <- read.csv(here("outputs", "french2021-Run_2023-01-09.csv"))

gpp_k_french2021 <- ggplot(data=french2021, aes(x=GPP_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (" ,d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))+ggtitle("french2021")
gpp_k_french2021

french2022 <- read.csv(here("outputs", "french2022-Run_2023-01-19_gapfillQ.csv"))

french2022 <- french2022 %>%  filter(date >= "2022-06-08")

gpp_k_french2022 <- ggplot(data=french2022, aes(x=GPP_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (" ,d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))+ggtitle("french2022")
gpp_k_french2022

french2022FC <- read.csv(here("outputs", "french2022-Run_2023-FC.csv"))

french2022FC <- french2022FC %>%  filter(date >= "2022-06-08")

gpp_k_french2022FC <- ggplot(data=french2022FC, aes(x=GPP_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (" ,d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))+ggtitle("french2022 - FC")
gpp_k_french2022


# moose

moose2019 <- read.csv(here("outputs", "moose2019-Run_2023-01-09.csv"))

gpp_k_moose2019 <- ggplot(data=moose2019, aes(x=GPP_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (" ,d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))+ggtitle("moose2019")
gpp_k_moose2019

moose2020 <- read.csv(here("outputs", "moose2020-Run_2023-01-09.csv"))

gpp_k_moose2020 <- ggplot(data=moose2020, aes(x=GPP_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (" ,d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))+ggtitle("moose2020")
gpp_k_moose2020


moose2021 <- read.csv(here("outputs", "moose2021-Run_2023-01-09.csv"))

gpp_k_moose2021 <- ggplot(data=moose2021, aes(x=GPP_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (" ,d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))+ggtitle("moose2021")
gpp_k_moose2021

moose2022 <- read.csv(here("outputs", "moose2022-Run_2023-01-09.csv"))


gpp_k_moose2022 <- ggplot(data=moose2022, aes(x=GPP_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (" ,d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))+ggtitle("moose2022 - Not gapfill")
gpp_k_moose2022

moose2022FC <- read.csv(here("outputs", "moose2022-Run_2023-FC.csv"))



gpp_k_moose2022FC <- ggplot(data=moose2022FC, aes(x=GPP_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (" ,d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))+ggtitle("moose2022 - FC")
gpp_k_moose2022


# stuart

stuart2019 <- read.csv(here("outputs", "stuart2019-Run_2023-01-09.csv"))

gpp_k_stuart2019 <- ggplot(data=stuart2019, aes(x=GPP_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (" ,d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))+ggtitle("stuart2019")
gpp_k_stuart2019

stuart2020 <- read.csv(here("outputs", "stuart2020-Run_2023-01-09.csv"))

gpp_k_stuart2020 <- ggplot(data=stuart2020, aes(x=GPP_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (" ,d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))+ggtitle("stuart2020")
gpp_k_stuart2020


stuart2021 <- read.csv(here("outputs", "stuart2021-Run_2023-01-09.csv"))

gpp_k_stuart2021 <- ggplot(data=stuart2021, aes(x=GPP_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (" ,d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))+ggtitle("stuart2021")
gpp_k_stuart2021

stuart2022 <- read.csv(here("outputs", "stuart2022-Run_2023-01-09.csv"))

gpp_k_stuart2022 <- ggplot(data=stuart2022, aes(x=GPP_mean, y=K600_daily_mean)) + geom_point()+ stat_poly_line() + stat_poly_eq(size = 10)+labs(x = expression(paste("GPP (g ", O[2] ," ", m^2, d^-1, ")")))+ labs(y = expression(paste("K600 (" ,d^-1, ")")))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme(axis.text.y=element_text(size=20))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+theme( axis.title.x = element_text(size = 20)) +theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size = 40, face = "bold"))+ggtitle("stuart2022")
gpp_k_stuart2022


library(grid)
library(ggeffects)
library(gridExtra)
library(gtable)
library(ggpubr)

ggarrange(gpp_k_poker2019, gpp_k_poker2020, gpp_k_poker2021,gpp_k_poker2022,gpp_k_vault2019,gpp_k_vault2020,gpp_k_vault2021,gpp_k_vault2022,gpp_k_french2019,gpp_k_french2020,gpp_k_french2021,gpp_k_french2022,gpp_k_moose2019,gpp_k_moose2020,gpp_k_moose2021,gpp_k_moose2022,gpp_k_stuart2019,gpp_k_stuart2020,gpp_k_stuart2021,gpp_k_stuart2022, gpp_k_moose2022FC, gpp_k_french2022FC, ncol = 4, nrow = 6)


