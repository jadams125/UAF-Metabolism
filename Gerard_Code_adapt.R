

########### TRIAL NIGHTTIME REGRESSION ##########

library(tidyverse)
library(streamMetabolizer)
library(ggpubr)
library(lubridate)
library(unitted)



#### Now we do Toolik River ####
# TR <- read_delim("TR_ts2018.csv",";", escape_double = FALSE, trim_ws = TRUE) %>% 
#   mutate(depth_m=channel_depth_cm/100, discharge=Discharge_L_s/1000) %>% 
#   rename(DO.obs=DO_mgL, DO.sat=Dosat_mgL, local.time=`Alaska Standard Time`, 
#          temp.water=Temp_O2logger, light=Light_lux) 

data.poke.mm.all <- read.csv(here("outputs", "clean.poke.2020.full.csv"))



#select the necessary variables for metabolism
PCmet <- data.poke.mm.all %>% 
  select( solar.time, DO.obs, DO.sat.EXO, depth, light, temp.water, discharge  ) %>% 
  mutate(discharge=discharge/1000) %>% rename(DO.sat = DO.sat.EXO) %>% mutate(solar.time = as.POSIXct(solar.time, tz="UTC")) 


PCmet$light <- calc_light(PCmet$solar.time, 65.152855, -147.486655)

#Make a new data.frame with tme mean values of each parameters
PCdaily <- PCmet %>% 
  group_by(date=as.Date(solar.time)) %>% 
  summarise_all(mean, na.rm=T) 



#Explore the data with some general plots
PCmet %>% unitted::v() %>%
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')


labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
PCmet %>% unitted::v() %>%
  select(solar.time, depth, temp.water, light) %>%
  gather(type, value, depth, temp.water, light) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light')),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')


#Calculate the bins of discharge, I do 4 but re run this several times before
###TKH: Use more bins (5-6, for season-length datasets)
bins_Q_PC <- calc_bins(log(PCdaily$discharge), 'number', n=6)


#First I explore the potential k values from NTR ####
#### Looking at the Q~k stuff

#Prepare data for NTR, I have to force the light to go to 0 from 20-5
###Alex: Look at this carefully for our data. We might need to use a narrower window here.

###TKH notes (from Hall & Hotchkiss: 
#1) O2 must be out of equilibrium at the start of nighttime due to the day's photosynthetic activity
#2) Rate of return to equilibrium is a function of gas exchange rate, dO/dt = ER +K(Osat-O)
#3) Regression should start at nightfall (when light goes to near 0), and continue until dO/dt ~ 0. This is not a huge issue for us, since we are only using the nighttime regression to generate a prior to model k.

dat_night <- PCmet %>% mutate(solar.time = as.POSIXct(solar.time, tz = "UTC"))%>% 
  mutate(light=if_else(hour(solar.time) %in% c(0:5,20:24), 0, light)  )

#Do a NTR for all data
mm2 <- metab_night(specs = specs(mm_name("night")), dat_night)

K600_nights_PC <- get_params(mm2,  uncertainty='ci') %>% 
  select(date, K600.daily, K600.daily.lower, K600.daily.upper) %>% 
  left_join(PCdaily) %>% mutate(K_source="night")

ggplot(K600_nights_PC)+
  geom_pointrange( aes(discharge, y=K600.daily, ymin=K600.daily.lower, 
                       ymax=K600.daily.upper), size=1)

# TOOLIK NOTES, NOT POKER CREEK #

#Ok, K seems to increase and after 0.2 is a bit more noisy. Could be that due the 
# morphology of Toolik river with deep pools and "square" channel? I will feed the model the mean k as prior.
# Also, as there is no linear relationship I use the 'binned' option, if would be linear that one

###TKH: If the relationship between K (estimated by nighttime regression) and Q is linear, use the pooled, but not binned approach to modeling k.

#I also look at little k, (in m d-1), as is the one I use for CO2 fluxes
ggplot(K600_nights_PC)+
  geom_pointrange( aes(discharge, y=K600.daily*depth, ymin=K600.daily.lower*depth, 
                       ymax=K600.daily.upper*depth), size=1)


#I use just a polynomial to get some K values for a given discharge. 
#This is just for the priors so are quite uninformed
###TKH: Change the degree of the polynomial to match the number of Q bins
Q_K_NPC <- lm(K600.daily~poly(discharge,6), data=K600_nights_PC)
summary(Q_K_NPC)

#Calculate the bins of discharge, I do 4 (WE USE 6) given the shape of the K data,
#but I do some trial and error here
##TKH: Use more bins for wider variation in Q. 
#Options for binning data are: number, interval, width, or custom
#Number divides data into bins of equal numbers of Q observations, with bins of irregular widths. This is probably the best option for shorter datasets or those with unequal numbers of observations under various values of discharge. 
#calc_bins(ln.disch, 'interval', n=5)
#df_int <- data.frame(t=1:length(ln.disch), vec=ln.disch, bin=bins_int$names[bins_int$vec])
#Interval divides data into equal width intervals, with varying numbers of observations within each interval. This could make sense for long datasets with good representation of observations across all values of discharge.
#Width allows you to specify the widths of the bins
#The difference between width and custom seems subtle. Custom allows you to set the upper and lower limits as well as the width of the bins.
bins_Q_PC <- data.frame(discharge = calc_bins(PCdaily$discharge, 'number', n=6)$bounds)

#Now I add K values for the nodes of discharge
pred_K_NPC <- data.frame(K.600 = predict(Q_K_NPC, newdata=bins_Q_PC, se.fit = T), 
                         discharge=bins_Q_PC$discharge)

#comparison of the K nodes and the NTR data
ggplot()+ geom_point(data=K600_nights_PC, aes(discharge, K600.daily))+
  geom_pointrange(data = pred_K_NPC, aes(x=discharge, y=K.600.fit,ymin=K.600.fit-K.600.se.fit,
                                         ymax=K.600.fit+K.600.se.fit ), color='red')+ggtitle("poker 2020 Gerard Code Nightime Regression")


# Set the model type.
bayes_name <- mm_name(type='bayes', pool_K600='binned',
                      err_obs_iid=TRUE, err_proc_acor=FALSE, err_proc_iid=TRUE, 
                      ode_method = 'trapezoid', deficit_src='DO_mod', engine='stan')


#### Set the model specs. For final run pump up the burnin steps to several thousand
###TKH: I would use 3K burnin, 10000K saved if using supercomputer

###TKH: plot the priors and be sure they make sense (e.g, do not overlap 0, do not unnecessarily eliminate reasonable values)
curve(dnorm(x, mean(pred_K_NTR$K.600.fit), exp(0.2)), xlim=c(-1, 20))
curve(dnorm(x, 1, 2), xlim=c(-1, 20))
curve(dnorm(x, -6, 3), xlim=c(-20, 1))

bayes_specs <- revise(specs(bayes_name), 
                      burnin_steps=1000, saved_steps=500,
                      day_start=3, day_end=27,
                      GPP_daily_mu = 1, GPP_daily_lower = 0,GPP_daily_sigma = 3, 
                      ER_daily_mu = -6, ER_daily_upper = 0, ER_daily_sigma = 5,
                      K600_lnQ_nodes_centers= log(pred_K_NTR$discharge),
                      K600_lnQ_nodes_meanlog= log(pred_K_NTR$K.600.fit),
                      K600_daily_sigma_sigma=0.5) #
bayes_specs

#run the model with the specs
bayes_fit_TR <- metab(bayes_specs, data=TRmet )


#Plot DO fits
plot_DO_preds(bayes_fit_TR)

#Plot Metabolic rates
plot_metab_preds(bayes_fit_TR)

#Get the daily outputs, and add the other Data
met_out_TR <-get_fit(bayes_fit_TR)$daily %>% left_join(TRdaily) 

#Get the K model used 
kbinned <-get_fit(bayes_fit_TR)$KQ_binned %>% add_column(log_Q_center=log(bins_Q_TR$discharge))



#Have a look at the ER K relationship
ggplot(met_out_TR)+
  geom_point(aes(K600_daily_50pct, ER_daily_50pct), size=4)+ theme_classic()

#Plot comparing the K used, the K pooling from the model, and other night K
###TKH note: Hall & Hotchkiss suggest that if k varies more than 50% between successive days, model-estimated k, GPP, ER have not been estimated with sufficient certainty
ggplot()+
  geom_ribbon(data=kbinned, aes(x=exp(log_Q_center), y=exp(lnK600_lnQ_nodes_50pct),
                                ymin=exp(lnK600_lnQ_nodes_2.5pct), ymax=exp(lnK600_lnQ_nodes_97.5pct)), 
              size=1, shape=4, alpha=.3)+
  geom_pointrange(data=K600_nights_TR, aes(discharge, y=K600.daily, ymin=K600.daily.lower, 
                                           ymax=K600.daily.upper),  color="blue", size=1)+ 
  geom_point(data=met_out_TR, aes(x=discharge,y=K600_daily_50pct), size=5, alpha=.7)+
  geom_line(data=kbinned, aes(exp(log_Q_center), exp(lnK600_lnQ_nodes_50pct)), size=2)+
  geom_pointrange(data = pred_K_NTR, aes(x=discharge, y=K.600.fit,ymin=K.600.fit-K.600.se.fit,
                                         ymax=K.600.fit+K.600.se.fit ), color='red', size=1)+
  labs(x= expression(Discharge~(m^3~s^-1) ),y=expression(K[600]~(d^-1)), title="Toolik")+
  theme_classic()+theme(axis.text = element_text(size=14), axis.title = element_text(size=14))













######### poke
data.poke.mm.all <- read.csv(here("outputs", "clean.poke.2020.full.csv"))


#change Discharge to m3/s
data.poke.mm.all$discharge <- data.poke.mm.all$discharge / 1000


bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name,
                     burnin_steps=5000, saved_steps=2000, n_cores=8,
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


