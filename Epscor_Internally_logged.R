# Plot Epscor Data

library(googledrive)
library(purrr)
library(tidyverse)
library(ggplot2)
library(lubridate)




#### CRAWFORD UPSTREAM ####


## store the URL you have
Craw_us_url <- "https://drive.google.com/drive/u/1/folders/1k0Jncre-QGQff14gl4h3Ny7D9L_PX8go"

## identify this folder on Drive
## let googledrive know this is a file ID or URL, as opposed to file name
craw_US_Folder <- drive_get(as_id(Craw_us_url))

## identify the csv files in that folder
craw_US_txt_files <- drive_ls(craw_US_Folder, type = "txt")

## download them
setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/CRAW/Upstream")
walk(craw_US_txt_files$id, ~ drive_download(as_id(.x), overwrite = TRUE))

filelist = list.files(pattern = ".txt")
for (i in 1:length(filelist)){
  input<-filelist[i]
  output<-paste0(input, ".csv")
  print(paste("Processing the file:", input))
  data = read.delim(input, header = TRUE, skip = 2, sep = ",")  
  output <- paste0(gsub("\\.txt$", "", input), ".csv")
  setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/CRAW/Upstream")
  write.table(data, file=output, sep=",", col.names=TRUE, row.names=FALSE)
  setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/CRAW/Upstream")
}

setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/CRAW")
### Stitch together manually downloaded MINI DOT data ###
craw_US_file_list <- list.files(path = "./upstream/", 
                            recursive=F, 
                            pattern=".csv", 
                            full.names=TRUE)

craw_us_data <-do.call("rbind", lapply(craw_US_file_list, 
                                   read.csv, 
                                   stringsAsFactors=FALSE, 
                                    header=TRUE))

craw_us_data$datetimeAK <- force_tz(as_datetime(craw_us_data$Time..sec.), "UTC")

craw_us_data$datetimeAK <- with_tz(craw_us_data$datetimeAK, tz = "America/Anchorage")


craw_us_data <- craw_us_data %>% rename(temp.water.CUS = T..deg.C., DO.obs.CUS = DO..mg.l.)


setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF")
craw.US.DO.plot <- ggplot(data=craw_us_data, aes(y=DO.obs.CUS, x= as.POSIXct(as.character(datetimeAK)))) +
  geom_point() + 
  labs(x = "Date and Time", y = "Dissolved Oxygen (mg/L)")+
  ggtitle("Crawford Upstream Mini Dot")+
  theme(plot.margin = margin(t = 20,  # Top margin
                       r = 20,  # Right margin
                       b = 20,  # Bottom margin
                       l = 20)) # Left margin

craw.US.DO.plot

pdf("plots/craw.US.DO.plot.pdf")     ## opens a .pdf device with specified path and file name
print(craw.US.DO.plot)   ## prints gram_ing_plot into the open device
dev.off() 




#### CRAW DOWNSTREAM #### 


## store the URL you have
Craw_ds_url <- "https://drive.google.com/drive/u/1/folders/1k14Iky5K6UU8OMxjA4yalup7GPVFRws3"

## identify this folder on Drive
## let googledrive know this is a file ID or URL, as opposed to file name
craw_DS_Folder <- drive_get(as_id(Craw_ds_url))

## identify the csv files in that folder
craw_DS_txt_files <- drive_ls(craw_DS_Folder, type = "txt")

## download them
setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/CRAW/Downstream")
walk(craw_DS_txt_files$id, ~ drive_download(as_id(.x), overwrite = TRUE))

filelistCrawDS = list.files(pattern = ".txt")
for (i in 1:length(filelistCrawDS)){
  input<-filelistCrawDS[i]
  output<-paste0(input, ".csv")
  print(paste("Processing the file:", input))
  data = read.delim(input, header = TRUE, skip = 2, sep = ",")  
  output <- paste0(gsub("\\.txt$", "", input), ".csv")
  setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/CRAW/Downstream")
  write.table(data, file=output, sep=",", col.names=TRUE, row.names=FALSE)
  setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/CRAW/Downstream")
}

setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/CRAW")







### Stitch together manually downloaded MINI DOT data ###
craw_DS_file_list <- list.files(path = "./Downstream/", 
                                recursive=F, 
                                pattern=".csv", 
                                full.names=TRUE)

craw_DS_data <-do.call("rbind", lapply(craw_DS_file_list, 
                                       read.csv, 
                                       stringsAsFactors=FALSE, 
                                       header=TRUE))

craw_DS_data <- craw_DS_data %>% rename(temp.water.CDS = T..deg.C., DO.obs.CDS = DO..mg.l.)


craw_DS_data$datetimeAK <- force_tz(as_datetime(craw_DS_data$Time..sec.), "UTC")

craw_DS_data$datetimeAK <- with_tz(craw_DS_data$datetimeAK, tz = "America/Anchorage")

setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF")
craw.DS.DO.plot <- ggplot(data=craw_DS_data, aes(y=DO.obs.CDS, x=datetimeAK)) +
  geom_point() + 
  labs(x = "Date and Time", y = "Dissolved Oxygen (mg/L)")+
  ggtitle("Crawford Downstream Mini Dot")+
  theme(plot.margin = margin(t = 20,  # Top margin
                             r = 20,  # Right margin
                             b = 20,  # Bottom margin
                             l = 20)) # Left margin

craw.DS.DO.plot

pdf("plots/craw.DS.DO.plot.pdf")     ## opens a .pdf device with specified path and file name
print(craw.DS.DO.plot)   ## prints gram_ing_plot into the open device
dev.off()



#Plot Craw Together

craw_DS_data <- craw_DS_data %>% select(datetimeAK, temp.water.CDS, DO.obs.CDS) 

craw_DS_data$datetimeAK <- lubridate::round_date(craw_DS_data$datetimeAK, "15 minutes") 

craw_DS_data <- craw_DS_data %>%
  mutate(across(c(DO.obs.CDS, temp.water.CDS),
                ~ifelse(datetimeAK == "2022-06-13", NA, .)))




craw_us_data <- craw_us_data %>% select(datetimeAK, temp.water.CUS, DO.obs.CUS) 

craw_us_data$datetimeAK <- lubridate::round_date(craw_us_data$datetimeAK, "15 minutes") 


craw.both <- full_join(craw_DS_data, craw_us_data, by = "datetimeAK")

craw.both <- craw.both %>% filter(datetimeAK> "2022-05-27 18:15:00"& datetimeAK<"2022-10-06 13:30:00")



craw.both <- craw.both %>%
  mutate(across(c( DO.obs.CUS, temp.water.CUS, DO.obs.CDS, temp.water.CDS),
                ~ifelse(datetimeAK >= "2022-06-13 16:30:00" &datetimeAK <= "2022-06-13 18:00:00", NA, .)))


craw.both <- craw.both %>%
  mutate(across(c( DO.obs.CUS, temp.water.CUS, DO.obs.CDS, temp.water.CDS),
                ~ifelse(datetimeAK >= "2022-08-18 11:30:00" &datetimeAK <= "2022-08-18 12:45:00", NA, .)))


ggplot(data = craw.both, aes(x = datetimeAK)) + geom_line(aes(y = DO.obs.CUS, color = "Upstream")) + geom_line(aes(y=DO.obs.CDS, color = "Downstream")) +ylab("DO.obs (mg/L)")+ ggtitle("Crawford Dissolved Oxygen")


craw.both$Site = "CRAW"



write.csv(craw.both, here("outputs", "craw.clean.miniDOT.csv"))




#### MAST UPSTREAM ####

## store the URL you have
Mast_us_url <- "https://drive.google.com/drive/u/1/folders/1lolgjQUg1w1gpHP5ZXPXhzr20raDXnyW"

## identify this folder on Drive
## let googledrive know this is a file ID or URL, as opposed to file name
mast_US_Folder <- drive_get(as_id(Mast_us_url))

## identify the csv files in that folder
mast_US_txt_files <- drive_ls(mast_US_Folder, type = "txt")

## download them
setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/MAST/Upstream")
walk(mast_US_txt_files$id, ~ drive_download(as_id(.x), overwrite = TRUE))

filelist = list.files(pattern = ".txt")
for (i in 1:length(filelist)){
  input<-filelist[i]
  output<-paste0(input, ".csv")
  print(paste("Processing the file:", input))
  data = read.delim(input, header = TRUE, skip = 2, sep = ",")  
  output <- paste0(gsub("\\.txt$", "", input), ".csv")
  setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/MAST/Upstream")
  write.table(data, file=output, sep=",", col.names=TRUE, row.names=FALSE)
  setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/MAST/Upstream")
}

setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/MAST")
### Stitch together manually downloaded MINI DOT data ###
mast_US_file_list <- list.files(path = "./upstream/", 
                                recursive=F, 
                                pattern=".csv", 
                                full.names=TRUE)

mast_us_data <-do.call("rbind", lapply(mast_US_file_list, 
                                       read.csv, 
                                       stringsAsFactors=FALSE, 
                                       header=TRUE))



mast_us_data$datetimeAK <- force_tz(as_datetime(mast_us_data$Time..sec.), "UTC")

mast_us_data$datetimeAK <- with_tz(mast_us_data$datetimeAK, tz = "America/Anchorage")


mast_us_data <- mast_us_data %>% rename(temp.water.MUS = T..deg.C., DO.obs.MUS = DO..mg.l.)



setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF")
mast.US.DO.plot <- ggplot(data=mast_us_data, aes(y=DO..mg.l., x=datetimeAK)) +
  geom_point() + 
  labs(x = "Date and Time", y = "Dissolved Oxygen (mg/L)")+
  ggtitle("Mastodon Upstream Mini Dot")+
  theme(plot.margin = margin(t = 20,  # Top margin
                             r = 20,  # Right margin
                             b = 20,  # Bottom margin
                             l = 20)) # Left margin

mast.US.DO.plot

pdf("plots/mast.US.DO.plot.pdf")     ## opens a .pdf device with specified path and file name
print(mast.US.DO.plot)   ## prints gram_ing_plot into the open device
dev.off()





#### MAST DOWNSTREAM ####


## store the URL you have
Mast_ds_url <- "https://drive.google.com/drive/u/1/folders/1lpcvDF6eYxIIbpL63soPYTzjjlgmp3gA"

## identify this folder on Drive
## let googledrive know this is a file ID or URL, as opposed to file name
mast_DS_Folder <- drive_get(as_id(Mast_ds_url))

## identify the csv files in that folder
mast_DS_txt_files <- drive_ls(mast_DS_Folder, type = "txt")

## download them
setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/MAST/Downstream")
walk(mast_DS_txt_files$id, ~ drive_download(as_id(.x), overwrite = TRUE))

filelistMastDS = list.files(pattern = ".txt")
for (i in 1:length(filelistMastDS)){
  input<-filelistMastDS[i]
  output<-paste0(input, "Z.csv")
  print(paste("Processing the file:", input))
  data = read.delim(input, header = TRUE, skip = 2, sep = ",")  
  output <- paste0(gsub("\\.txt$", "", input), ".csv")
  setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/MAST/Downstream")
  write.table(data, file=output, sep=",", col.names=TRUE, row.names=FALSE)
  setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/MAST/Downstream")
}

setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/MAST")
### Stitch together manually downloaded MINI DOT data ###
mast_DS_file_list <- list.files(path = "./Downstream/", 
                                recursive=F, 
                                pattern=".csv", 
                                full.names=TRUE)

mast_DS_data <-do.call("rbind", lapply(mast_DS_file_list, 
                                       read.csv, 
                                       stringsAsFactors=FALSE, 
                                       header=TRUE))



mast_DS_data$datetimeAK <- force_tz(as_datetime(mast_DS_data$Time..sec.), "UTC")

mast_DS_data$datetimeAK <- with_tz(mast_DS_data$datetimeAK, tz = "America/Anchorage")


mast_DS_data <- mast_DS_data %>% rename(temp.water.MDS = T..deg.C., DO.obs.MDS = DO..mg.l.)


setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF")
mast.DS.DO.plot <- ggplot(data=mast_DS_data, aes(y=DO..mg.l., x=datetimeAK)) +
  geom_point() + 
  labs(x = "Date and Time", y = "Dissolved Oxygen (mg/L)")+
  ggtitle("Mastodon Downstream Mini Dot")+
  theme(plot.margin = margin(t = 20,  # Top margin
                             r = 20,  # Right margin
                             b = 20,  # Bottom margin
                             l = 20)) # Left margin

mast.DS.DO.plot

pdf("plots/mast.DS.DO.plot.pdf")     ## opens a .pdf device with specified path and file name
print(mast.DS.DO.plot)   ## prints gram_ing_plot into the open device
dev.off()


#Plot Mast Together

mast_DS_data <- mast_DS_data %>% select(datetimeAK, temp.water.MDS, DO.obs.MDS) 

mast_DS_data$datetimeAK <- lubridate::round_date(mast_DS_data$datetimeAK, "15 minutes") 

mast_us_data <- mast_us_data %>% select(datetimeAK, temp.water.MUS, DO.obs.MUS) 

mast_us_data$datetimeAK <- lubridate::round_date(mast_us_data$datetimeAK, "15 minutes") 



mast_us_data <- mast_us_data %>%
  mutate(across(c( DO.obs.MUS, temp.water.MUS),
                ~ifelse(datetimeAK >= "2022-06-13 13:45:00" &datetimeAK <= "2022-06-13 14:30:00", NA, .)))

mast_us_data$datetimeAK <- as.POSIXct(mast_us_data$datetimeAK, tz = "AKST")

mast.both <- full_join(mast_DS_data, mast_us_data, by = "datetimeAK")

mast.both <- mast.both %>% filter(datetimeAK> "2022-05-27 16:30:00"& datetimeAK<"2022-10-06 12:15:00")



ggplot(data = mast.both, aes(x = datetimeAK)) + geom_line(aes(y = DO.obs.MUS, color = "Upstream")) + geom_line(aes(y=DO.obs.MDS, color = "Downstream")) +ylab("DO.obs (mg/L)")+ ggtitle("Mastodon Dissolved Oxygen")


write.csv(mast.both, here("outputs", "mast.clean.miniDOT.csv"))



#### SHOVEL UPSTREAM ####


## store the URL you have
Shov_us_url <- "https://drive.google.com/drive/u/1/folders/14aBdxBqjZJ99FEuYTfrt5kPBuP9wLcY-"

## identify this folder on Drive
## let googledrive know this is a file ID or URL, as opposed to file name
shov_US_Folder <- drive_get(as_id(Shov_us_url))

## identify the csv files in that folder
shov_US_txt_files <- drive_ls(shov_US_Folder, type = "txt")

## download them
setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/SHOV/Upstream")
walk(shov_US_txt_files$id, ~ drive_download(as_id(.x), overwrite = TRUE))

filelist = list.files(pattern = ".txt")
for (i in 1:length(filelist)){
  input<-filelist[i]
  output<-paste0(input, ".csv")
  print(paste("Processing the file:", input))
  data = read.delim(input, header = TRUE, skip = 2, sep = ",")  
  output <- paste0(gsub("\\.txt$", "", input), ".csv")
  setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/SHOV/Upstream")
  write.table(data, file=output, sep=",", col.names=TRUE, row.names=FALSE)
  setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/SHOV/Upstream")
}

setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/SHOV")
### Stitch together manually downloaded MINI DOT data ###
shov_US_file_list <- list.files(path = "./upstream/", 
                                recursive=F, 
                                pattern=".csv", 
                                full.names=TRUE)

shov_us_data <-do.call("rbind", lapply(shov_US_file_list, 
                                       read.csv, 
                                       stringsAsFactors=FALSE, 
                                       header=TRUE))



shov_us_data$datetimeAK <- force_tz(as_datetime(shov_us_data$Time..sec.), "UTC")

shov_us_data$datetimeAK <- with_tz(shov_us_data$datetimeAK, tz = "America/Anchorage")


shov_us_data <- shov_us_data %>% rename(temp.water.SUS = T..deg.C., DO.obs.SUS = DO..mg.l.)


setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF")
shov.US.DO.plot <- ggplot(data=shov_us_data, aes(y=DO..mg.l., x=datetimeAK)) +
  geom_point() + 
  labs(x = "Date and Time", y = "Dissolved Oxygen (mg/L)")+
  ggtitle("Shovel Upstream Mini Dot")+
  theme(plot.margin = margin(t = 20,  # Top margin
                             r = 20,  # Right margin
                             b = 20,  # Bottom margin
                             l = 20)) # Left margin

shov.US.DO.plot

pdf("plots/shov.US.DO.plot.pdf")     ## opens a .pdf device with specified path and file name
print(shov.US.DO.plot)   ## prints gram_ing_plot into the open device
dev.off()




#### SHOV DOWNSTREAM #### 


## store the URL you have
Shov_ds_url <- "https://drive.google.com/drive/u/1/folders/14WmUFu4jJCwZyKlI9QgqUQphk_zO2jlp"

## identify this folder on Drive
## let googledrive know this is a file ID or URL, as opposed to file name
shov_DS_Folder <- drive_get(as_id(Shov_ds_url))

## identify the csv files in that folder
shov_DS_txt_files <- drive_ls(shov_DS_Folder, type = "txt")

## download them
setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/SHOV/Downstream")
walk(shov_DS_txt_files$id, ~ drive_download(as_id(.x), overwrite = TRUE))

filelistShovDS = list.files(pattern = ".txt")
for (i in 1:length(filelistShovDS)){
  input<-filelistShovDS[i]
  output<-paste0(input, ".csv")
  print(paste("Processing the file:", input))
  data = read.delim(input, header = TRUE, skip = 2, sep = ",")  
  output <- paste0(gsub("\\.txt$", "", input), ".csv")
  setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/SHOV/Downstream")
  write.table(data, file=output, sep=",", col.names=TRUE, row.names=FALSE)
  setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/SHOV/Downstream")
}

setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/SHOV")
### Stitch together manually downloaded MINI DOT data ###
shov_DS_file_list <- list.files(path = "./Downstream/", 
                                recursive=F, 
                                pattern=".csv", 
                                full.names=TRUE)

shov_DS_data <-do.call("rbind", lapply(shov_DS_file_list, 
                                       read.csv, 
                                       stringsAsFactors=FALSE, 
                                       header=TRUE))




shov_DS_data$datetimeAK <- force_tz(as_datetime(shov_DS_data$Time..sec.), "UTC")

shov_DS_data$datetimeAK <- with_tz(shov_DS_data$datetimeAK, tz = "America/Anchorage")


shov_DS_data <- shov_DS_data %>% rename(temp.water.SDS = T..deg.C., DO.obs.SDS = DO..mg.l.)


setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF")
shov.DS.DO.plot <- ggplot(data=shov_DS_data, aes(y=DO..mg.l., x=datetimeAK)) +
  geom_point() + 
  labs(x = "Date and Time", y = "Dissolved Oxygen (mg/L)")+
  ggtitle("Shovel Upstream Mini Dot")+
  theme(plot.margin = margin(t = 20,  # Top margin
                             r = 20,  # Right margin
                             b = 20,  # Bottom margin
                             l = 20)) # Left margin

shov.DS.DO.plot

pdf("plots/shov.DS.DO.plot.pdf")     ## opens a .pdf device with specified path and file name
print(shov.DS.DO.plot)   ## prints gram_ing_plot into the open device
dev.off()







#Plot Shov Together

shov_DS_data <- shov_DS_data %>% select(datetimeAK, temp.water.SDS, DO.obs.SDS) 

shov_DS_data$datetimeAK <- lubridate::round_date(shov_DS_data$datetimeAK, "15 minutes") 

shov_us_data <- shov_us_data %>% select(datetimeAK, temp.water.SUS, DO.obs.SUS) 

shov_us_data$datetimeAK <- lubridate::round_date(shov_us_data$datetimeAK, "15 minutes") 



shov.both <- full_join(shov_DS_data, shov_us_data, by = "datetimeAK")

shov.both <- shov.both %>% filter(datetimeAK> "2022-06-14 13:45:00"& datetimeAK<"2022-10-05 12:30:00")

ggplot(data = shov.both, aes(x = datetimeAK)) + geom_line(aes(y = DO.obs.SUS, color = "Upstream")) + geom_line(aes(y=DO.obs.SDS, color = "Downstream")) +ylab("DO.obs (mg/L)")+ ggtitle("Shovel Dissolved Oxygen")


shov.both$Site = "SHOV"

write.csv(shov.both, here("outputs", "shov.clean.miniDOT.csv"))






crawPivot <- craw.both %>% select(datetimeAK, DO.obs.CDS, DO.obs.CUS) %>%  pivot_longer(cols=c('DO.obs.CUS', 'DO.obs.CDS'),
                                                                                        names_to='Location',
                                                                                        values_to='DO.obs')


mastPivot <- mast.both %>% select(datetimeAK, DO.obs.MDS, DO.obs.MUS) %>%  pivot_longer(cols=c('DO.obs.MUS', 'DO.obs.MDS'),
                                                                                        names_to='Location',
                                                                                        values_to='DO.obs')



shovPivot <- shov.both %>% select(datetimeAK, DO.obs.SDS, DO.obs.SUS) %>%  pivot_longer(cols=c('DO.obs.SUS', 'DO.obs.SDS'),
                                                                                        names_to='Location',
                                                                                        values_to='DO.obs')




testMerge <- rbind(crawPivot, mastPivot, shovPivot)


write.csv(testMerge, here("outputs", "EpscorMiniDOT.comb.csv"))

testMerge %>% filter(Location %in% c("DO.obs.MDS", "DO.obs.SDS", "DO.obs.CDS")) %>%  ggplot(aes(x = datetimeAK, y = DO.obs, color = Location)) +geom_point(size = 0.01)+ xlab("Date")+ylab("DO (mg/L)")+geom_line()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(plot.title = element_text(hjust = 0.5))+theme( axis.title.x = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+ scale_color_discrete(name = "Site", labels = c("Crawford", "Mastodon", "Shovel")) +
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=15), #change legend title font size
        legend.text = element_text(size=15)) #change legend text font size



testMerge %>% filter(Location %in% c("DO.obs.MDS", "DO.obs.SDS", "DO.obs.CDS")) %>%  ggplot(aes(x = datetimeAK, y = DO.obs, color = Location)) +geom_point(size = 0.01)+ xlab("Date")+ylab("DO (mg/L)")+geom_line()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(plot.title = element_text(hjust = 0.5))+theme( axis.title.x = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+ scale_color_discrete(name = "Site", labels = c("Crawford", "Mastodon", "Shovel")) +
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=15), #change legend title font size
        legend.text = element_text(size=15)) #change legend text font size




testMerge %>% filter(Location %in% c("DO.obs.MUS", "DO.obs.SUS", "DO.obs.CUS")) %>% ggplot(aes(x = datetimeAK, y = DO.obs, color = Location)) +geom_point(size = 0.01)+ xlab("Date")+ylab("DO (mg/L)")+geom_line()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+theme( axis.title.y = element_text(size = 20))+theme(axis.text.y=element_text(size=20))+theme(plot.title = element_text(hjust = 0.5))+theme( axis.title.x = element_text(size = 20))+theme(axis.text.x=element_text(size=20))+ylim(9,15)+ scale_color_discrete(name = "Site", labels = c("Crawford", "Mastodon", "Shovel")) +
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=15), #change legend title font size
        legend.text = element_text(size=15)) #change legend text font size













craw_us_data %>% filter(datetimeAK>"2022-06-11" & datetimeAK<"2022-06-18") %>% ggplot(aes(x=datetimeAK, y=DO.obs.CUS))+geom_point()+ggtitle("craw upstream")

craw_us_data %>% filter(datetimeAK>"2022-06-11" & datetimeAK<"2022-06-18") %>% ggplot(aes(x=datetimeAK, y=temp))+geom_point()+ggtitle("craw upstream")

craw_us_data  %>% ggplot(aes(x=datetimeAK, y=temp.water.CUS))+geom_point()+ggtitle("craw upstream")



craw_DS_data %>% filter(datetimeAK>"2022-06-11" & datetimeAK<"2022-06-18") %>% ggplot(aes(x=datetimeAK, y=DO.obs.CDS))+geom_point()+ggtitle("craw downstream")


shov_us_data %>% filter(datetimeAK>"2022-06-11" & datetimeAK<"2022-06-18") %>% ggplot(aes(x=datetimeAK, y=DO.obs.SUS))+geom_point()+ggtitle("shov upstream")

shov_DS_data %>% filter(datetimeAK>"2022-06-11" & datetimeAK<"2022-06-18") %>% ggplot(aes(x=datetimeAK, y=DO.obs.SDS))+geom_point()+ggtitle("shov downstream")



mast_us_data %>% filter(datetimeAK>"2022-06-11" & datetimeAK<"2022-06-18") %>% ggplot(aes(x=datetimeAK, y=DO.obs.MUS))+geom_point()

mast_DS_data %>% filter(datetimeAK>"2022-06-11" & datetimeAK<"2022-06-18") %>% ggplot(aes(x=datetimeAK, y=DO.obs.MDS))+geom_point()




