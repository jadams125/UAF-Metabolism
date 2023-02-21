
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
library(googlesheets4)
library(naniar)
library(data.table)
library(grid)
library(plotrix)


here()

TDP_Data <- read.csv(here("221116_TDP.csv"))

TDP_Data <- TDP_Data %>% filter(Year == "2022")

TDP_Data$TDP_uM <- as.numeric(TDP_Data$TDP_uM) 


TDP_ordered_mean <- aggregate(x = TDP_Data$TDP_uM,                # Specify data column
                      by = list(TDP_Data$Order),              # Specify group indicator
                      FUN = mean)                           # Specify function (i.e. mean)              #                                                                Specify function



Samples_IN <- read_sheet("https://docs.google.com/spreadsheets/d/1BPYCoUE0jBW64GlEmGW73qs84RGX2s_kbKWabKE-TPI/edit#gid=0", sheet = 2)

Samples_IN_EPSCoR <- Samples_IN %>% filter(Project == "EPSCoR") %>% rename(SampleID = ID60mL) %>% select(Date, SampleID, comments)

Samples_IN_EPSCoR$SampleID <- as.character(Samples_IN_EPSCoR$SampleID)

TDP_Data_mean <- full_join(Samples_IN_EPSCoR,TDP_ordered_mean, by = "SampleID")




# Mean by date 

TDP_Data_mean_mean <- aggregate(x = TDP_Data_mean$TDP_uM_mean,                # Specify data column
                              by = list(TDP_Data_mean$Date,TDP_Data_mean$Site ),              # Specify group indicator
                              FUN = mean)                           # Specify function (i.e. mean)              #                                                                Specify function


TDP_Data_mean_SE <- aggregate(x = TDP_Data_mean$TDP_uM_mean,                # Specify data column
                                by = list(TDP_Data_mean$Date,TDP_Data_mean$Site ),              # Specify group indicator
                                FUN = std.error)                           # Specify function (i.e. mean)              #                                                                Specify function




# Using dplyr when columns are same
TDP_mean_final <- inner_join( TDP_Data_mean_mean, TDP_Data_mean_SE, by=c('Group.1','Group.2'))




TDP_mean_final <- TDP_mean_final %>% rename(Order = Group.1, Site = Group.2, TDP_uM_mean = x.x,TDP_uM_SE = x.y )


TDP_mean_final$TDP_uM_mean[TDP_mean_final$TDP_uM_mean<0.1] <- 0.025


TDP_mean_final$TDP_ugPL <- TDP_mean_final$TDP_uM_mean * 31
TDP_mean_final$TDP_ugPL_SE <- TDP_mean_final$TDP_uM_SE * 31

TDP_mean_final$TDP_mgPL <- TDP_mean_final$TDP_uM_mean * 31 * 0.001
TDP_mean_final$TDP_mgPL_SE <- TDP_mean_final$TDP_uM_SE * 31* 0.001

TDP_mean_final %>% ggplot(aes(x = Order, y = TDP_ugPL, color = Site)) +geom_point()+ xlab("Date")+ylab("TDP (μg P/L)")+geom_line()+geom_errorbar(aes(ymin=TDP_ugPL-TDP_ugPL_SE, ymax=TDP_ugPL+TDP_ugPL_SE), width=.2, position=position_dodge(.9)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(plot.title = element_text(hjust = 0.5))+theme( axis.title.x = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+ scale_color_manual(labels = c("Crawford", "Mastodon", "Shovel"), values = c("blue", "red", "forestgreen"))+
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=15), #change legend title font size
        legend.text = element_text(size=15)) #change legend text font size


mast_TDP <- TDP_mean_final %>% filter(Site=="MAST")

mean(mast_TDP$TDP_ugPL)

shov_TDP <- TDP_mean_final %>% filter(Site=="SHOV")

mean(shov_TDP$TDP_ugPL)

craw_TDP <- TDP_mean_final %>% filter(Site=="CRAW")

mean(craw_TDP$TDP_ugPL)




############


DOC_Data <- read.csv(here("230109_TNDOC_KLJ.csv")) %>% rename(SampleID = Sample.Name) %>% filter(Year == "2022")

DOC_Data <- DOC_Data %>% filter(Site %in% c("MAST", "CRAW", "SHOV"))

DOC_Data_join <- inner_join(Samples_IN_EPSCoR,DOC_Data, by = "SampleID")



# Mean by date 

DOC_Data_mean <- aggregate(x = DOC_Data_join$NPOC.mg.L,                # Specify data column
                                by = list(DOC_Data_join$Date,DOC_Data_join$Site ),              # Specify group indicator
                                FUN = mean)                           # Specify function (i.e. mean)              #                                                                Specify function

DOC_Data_SE <- aggregate(x = DOC_Data_join$NPOC.mg.L,                # Specify data column
                           by = list(DOC_Data_join$Date,DOC_Data_join$Site ),              # Specify group indicator
                           FUN = std.error)                           # Specify function (i.e. mean)              #                                                                Specify function




DOC_mean_final <- inner_join( DOC_Data_mean, DOC_Data_SE, by=c('Group.1','Group.2'))



DOC_mean_final <- DOC_mean_final %>% rename(Date = Group.1, Site = Group.2, DOC_mgL_mean = x.x, DOC_mgL_SE = x.y )


DOC_mean_final %>% ggplot(aes(x = Date, y = DOC_mgL_mean, color = Site)) +geom_point()+ xlab("Date")+ylab("DOC (mg/L)")+geom_line()+geom_errorbar(aes(ymin=DOC_mgL_mean-DOC_mgL_SE, ymax=DOC_mgL_mean+DOC_mgL_SE), width=.2, position=position_dodge(.9)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(plot.title = element_text(hjust = 0.5))+theme( axis.title.x = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+ scale_color_manual(labels = c("Crawford", "Mastodon", "Shovel"), values = c("blue", "red", "forestgreen"))+
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=15), #change legend title font size
        legend.text = element_text(size=15)) #change legend text font size






###################################

anions_Data1 <- read.csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_chems_meta/chems_meta.csv") %>% filter(Site == c("EPSCoR"))
anions_Data2 <- read.csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_chems_meta/chems_meta.csv") %>% filter(Site == c("EPSCOR"))

anions_Data <- rbind(anions_Data1, anions_Data2)



testdata <- anions_Data %>% select(Date, SampleID, Site, Fluoride_mgL, Chloride_mgL, Sulfate_mgL, Bromide_mgL, NitrateN_mgL) %>% filter(Site %in% c("EPSCOR", "EPSCoR")) 



Samples_IN <- read_sheet("https://docs.google.com/spreadsheets/d/1BPYCoUE0jBW64GlEmGW73qs84RGX2s_kbKWabKE-TPI/edit#gid=0", sheet = 2)

Samples_IN_EPSCoR <- Samples_IN %>% filter(Project == "EPSCoR") %>% rename(SampleID = ID60mL) %>% select(Date, SampleID, comments, Site)

Samples_IN_EPSCoR <- Samples_IN_EPSCoR %>% filter(Site %in% c("MAST", "CRAW", "SHOV"))


Samples_IN_EPSCoR$SampleID <- as.character(Samples_IN_EPSCoR$SampleID)

NO3_Data_join <- inner_join(Samples_IN_EPSCoR,testdata, by = "SampleID")




# Mean by date 

NO3_Data_mean <- aggregate(x = NO3_Data_join$NitrateN_mgL,                # Specify data column
                           by = list(NO3_Data_join$Date.x,NO3_Data_join$Site.x),              # Specify group indicator
                           FUN = mean)                           # Specify function (i.e. mean)              #                                                                Specify function

NO3_Data_SE <- aggregate(x = NO3_Data_join$NitrateN_mgL,                # Specify data column
                         by = list(NO3_Data_join$Date.x,NO3_Data_join$Site.x ),              # Specify group indicator
                         FUN = std.error)                           # Specify function (i.e. mean)              #                                                                Specify function


NO3_mean_final <- inner_join( NO3_Data_mean, NO3_Data_SE, by=c('Group.1','Group.2'))



NO3_mean_final <- NO3_mean_final %>% rename(Date = Group.1, Site = Group.2, NO3_mgL_mean = x.x, NO3_mgL_SE = x.y )


NO3_mean_final %>% ggplot(aes(x = Date, y = NO3_mgL_mean, color = Site)) +geom_point()+ xlab("Date")  + labs(y = expression(paste(NO[3], "- (mg N/L)")))+geom_line()+geom_errorbar(aes(ymin=NO3_mgL_mean-NO3_mgL_SE, ymax=NO3_mgL_mean+NO3_mgL_SE), width=.2, position=position_dodge(.9)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(plot.title = element_text(hjust = 0.5))+theme( axis.title.x = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+ scale_color_manual(labels = c("Crawford", "Mastodon", "Shovel"), values = c("blue", "red", "forestgreen"))+
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=15), #change legend title font size
        legend.text = element_text(size=15)) #change legend text font size


mast_No3 <- NO3_mean_final %>% filter(Site=="MAST")

mean(mast_No3$NO3_mgL_mean)

shov_No3 <- NO3_mean_final %>% filter(Site=="SHOV")

mean(shov_No3$NO3_mgL_mean)

craw_No3 <- NO3_mean_final %>% filter(Site=="CRAW")

mean(craw_No3$NO3_mgL_mean)

