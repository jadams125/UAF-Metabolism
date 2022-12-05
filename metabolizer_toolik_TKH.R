
  library(tidyverse)
  library(streamMetabolizer)
  library(ggpubr)
  library(lubridate)
  library(unitted)

#### Now we do Toolik River ####
TR <- read_delim("TR_ts2018.csv",";", escape_double = FALSE, trim_ws = TRUE) %>% 
  mutate(depth_m=channel_depth_cm/100, discharge=Discharge_L_s/1000) %>% 
  rename(DO.obs=DO_mgL, DO.sat=Dosat_mgL, local.time=`Alaska Standard Time`, 
                temp.water=Temp_O2logger, light=Light_lux) 

#select the necessary variables for metabolism
TRmet <- TR %>% 
  select( local.time, DO.obs, DO.sat, depth_m, light, temp.water, Discharge_L_s  ) %>% 
  rename(solar.time=local.time, depth=depth_m) %>% 
  mutate(discharge=Discharge_L_s/1000) %>% select(-Discharge_L_s)

#Make a new data.frame with tme mean values of each parameters
TRdaily <- TRmet %>% 
  group_by(date=as.Date(solar.time)) %>% 
  summarise_all(mean, na.rm=T) 



#Explore the data with some general plots
TRmet %>% unitted::v() %>%
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')


labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
TRmet %>% unitted::v() %>%
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
bins_Q_TR <- calc_bins(log(TRdaily$discharge), 'number', n=4)


#First I explore the potential k values from NTR ####
#### Looking at the Q~k stuff

#Prepare data for NTR, I have to force the light to go to 0 from 20-5
###Alex: Look at this carefully for our data. We might need to use a narrower window here.

###TKH notes (from Hall & Hotchkiss: 
#1) O2 must be out of equilibrium at the start of nighttime due to the day's photosynthetic activity
#2) Rate of return to equilibrium is a function of gas exchange rate, dO/dt = ER +K(Osat-O)
#3) Regression should start at nightfall (when light goes to near 0), and continue until dO/dt ~ 0. This is not a huge issue for us, since we are only using the nighttime regression to generate a prior to model k.

dat_night <- TRmet %>% 
  mutate(light=if_else(hour(solar.time) %in% c(0:5,20:24), 0, light)  )

#Do a NTR for all data
mm2 <- metab_night(specs = specs(mm_name("night")), dat_night)

K600_nights_TR <- get_params(mm2,  uncertainty='ci') %>% 
  select(date, K600.daily, K600.daily.lower, K600.daily.upper) %>% 
  left_join(TRdaily) %>% mutate(K_source="night")

ggplot(K600_nights_TR)+
   geom_pointrange( aes(discharge, y=K600.daily, ymin=K600.daily.lower, 
                        ymax=K600.daily.upper), size=1)


#Ok, K seems to increase and after 0.2 is a bit more noisy. Could be that due the 
# morphology of Toolik river with deep pools and "square" channel? I will feed the model the mean k as prior.
# Also, as there is no linear relationship I use the 'binned' option, if would be linear that one

###TKH: If the relationship between K (estimated by nighttime regression) and Q is linear, use the pooled, but not binned approach to modeling k.

#I also look at little k, (in m d-1), as is the one I use for CO2 fluxes
ggplot(K600_nights_TR)+
  geom_pointrange( aes(discharge, y=K600.daily*depth, ymin=K600.daily.lower*depth, 
                       ymax=K600.daily.upper*depth), size=1)


#I use just a polynomial to get some K values for a given discharge. 
#This is just for the priors so are quite uninformed
###TKH: Change the degree of the polynomial to match the number of Q bins
Q_K_NTR <- lm(K600.daily~poly(discharge,4), data=K600_nights_TR)
summary(Q_K_NTR)

#Calculate the bins of discharge, I do 4 given the shape of the K data,
#but I do some trial and error here
##TKH: Use more bins for wider variation in Q. 
#Options for binning data are: number, interval, width, or custom
#Number divides data into bins of equal numbers of Q observations, with bins of irregular widths. This is probably the best option for shorter datasets or those with unequal numbers of observations under various values of discharge. 
  #calc_bins(ln.disch, 'interval', n=5)
  #df_int <- data.frame(t=1:length(ln.disch), vec=ln.disch, bin=bins_int$names[bins_int$vec])
#Interval divides data into equal width intervals, with varying numbers of observations within each interval. This could make sense for long datasets with good representation of observations across all values of discharge.
#Width allows you to specify the widths of the bins
#The difference between width and custom seems subtle. Custom allows you to set the upper and lower limits as well as the width of the bins.
bins_Q_TR <- data.frame(discharge = calc_bins(TRdaily$discharge, 'number', n=4)$bounds)

#Now I add K values for the nodes of discharge
pred_K_NTR <- data.frame(K.600 = predict(Q_K_NTR, newdata=bins_Q_TR, se.fit = T), 
                             discharge=bins_Q_TR$discharge)

 #comparison of the K nodes and the NTR data
 ggplot()+ geom_point(data=K600_nights_TR, aes(discharge, K600.daily))+
   geom_pointrange(data = pred_K_NTR, aes(x=discharge, y=K.600.fit,ymin=K.600.fit-K.600.se.fit,
                                          ymax=K.600.fit+K.600.se.fit ), color='red')
 

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

