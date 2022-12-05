setwd(here())
POKE.comb.in <- read.csv(here("outputs", "poke.comb.csv"))


#set time to month and a half chunk that my computer can run
POKE.comb <- POKE.comb.in %>% filter(solar.time >= "2019-06-25 04:13:57" & solar.time < "2019-08-05 04:13:57")

POKE.comb$solar.time<- as.POSIXct(POKE.comb$solar.time)

light <- POKE.comb %>% filter(solar.time < "2019-06-27 09:15:00") %>% ggplot(aes( x = solar.time, y = light)) + geom_point() + geom_vline(xintercept=as.numeric(POKE.comb$solar.time[64]), linetype=4) + geom_vline(xintercept=as.numeric(POKE.comb$solar.time[196]), linetype=4)+ geom_vline(xintercept=as.numeric(POKE.comb$solar.time[160]), linetype=4)+ geom_vline(xintercept=as.numeric(POKE.comb$solar.time[100]), linetype=4)


do <- POKE.comb %>% filter(solar.time < "2019-06-27 09:15:00") %>% ggplot(aes( x = solar.time, y = DO.obs)) + geom_point() + geom_vline(xintercept=as.numeric(POKE.comb$solar.time[64]), linetype=4) + geom_vline(xintercept=as.numeric(POKE.comb$solar.time[196]), linetype=4)+ geom_vline(xintercept=as.numeric(POKE.comb$solar.time[160]), linetype=4)+ geom_vline(xintercept=as.numeric(POKE.comb$solar.time[100]), linetype=4)

fig1 <- ggarrange(light,do, ncol = 1, nrow = 2)




dat_night <- POKE.comb %>% 
  mutate(light=if_else(hour(solar.time) %in% c(0:5,20:24), 0, light)  )



light <- dat_night %>% filter(solar.time < "2019-06-27 09:15:00") %>% ggplot(aes( x = solar.time, y = light)) + geom_point() + geom_vline(xintercept=as.numeric(POKE.comb$solar.time[64]), linetype=4) + geom_vline(xintercept=as.numeric(POKE.comb$solar.time[196]), linetype=4)+ geom_vline(xintercept=as.numeric(POKE.comb$solar.time[160]), linetype=4)+ geom_vline(xintercept=as.numeric(POKE.comb$solar.time[100]), linetype=4)


do <- dat_night %>% filter(solar.time < "2019-06-27 09:15:00") %>% ggplot(aes( x = solar.time, y = DO.obs)) + geom_point() + geom_vline(xintercept=as.numeric(POKE.comb$solar.time[64]), linetype=4) + geom_vline(xintercept=as.numeric(POKE.comb$solar.time[196]), linetype=4)+ geom_vline(xintercept=as.numeric(POKE.comb$solar.time[160]), linetype=4)+ geom_vline(xintercept=as.numeric(POKE.comb$solar.time[100]), linetype=4)

fig2 <- ggarrange(light,do, ncol = 1, nrow = 2)



ggarrange(fig1,fig2, ncol = 1, nrow = 2)
