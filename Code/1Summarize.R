library(ggplot2)
library(dplyr)
control_chloro <- read.csv("~/Basic_Ecological_Processes/Data/control_chloro.csv")
daphnia_chloro <- read.csv("~/Basic_Ecological_Processes/Data/daphnia_chloro.csv")

controls_means <- control_chloro %>%
  filter(conc_diff<24)%>%
  group_by(date,setup,food_conc)%>%
  summarise(ctrl_conc=mean(end_conc),sd(end_conc))
feeding_rate <- merge(daphnia_chloro,controls_means[,1:4],by=c('date','food_conc'))

feeding_rate <- feeding_rate %>%
  mutate(feeding_rate=-0.2*(chloro_conc-ctrl_conc), feeding_per_capita=feeding_rate/daphnia_count)

mean_start_concs <- control_chloro %>%
  group_by(food_conc) %>%
  summarise(start_conc=mean(start_conc))
feeding_rate <- merge(feeding_rate,mean_start_concs,by='food_conc')  
feeding_rate$start_conc<-round(0.2*feeding_rate$start_conc,digits=1)

feeding_rate_means <- feeding_rate %>%
  group_by(date,food,food_conc)%>%
  summarise(mean_feeding_rate=mean(feeding_rate),sd_feeding_rate=sd(feeding_rate),mean_feeding_per_capita=mean(feeding_per_capita),sd_feeding_per_capita=sd(feeding_per_capita)) %>%
  merge(.,mean_start_concs,by='food_conc') 
feeding_rate_means$start_conc<-round(0.2*feeding_rate_means$start_conc,digits=1)


#Plots
ggplot(feeding_rate_means,aes(x=factor(start_conc),y=mean_feeding_rate,colour=date))+
  geom_point()+
  geom_hline(aes(yintercept=0))+
  ylab('Fressrate')+
  xlab('Startkonzentration')+
  geom_errorbar(aes(ymin=mean_feeding_rate-sd_feeding_rate,ymax=mean_feeding_rate+sd_feeding_rate), width = 0.25)+
  theme_bw()

ggplot(feeding_rate_means,aes(x=factor(start_conc),y=mean_feeding_per_capita,colour=date))+
  geom_point()+
  geom_hline(aes(yintercept=0))+
  ylab('Fressrate [ug Chl a pro Tag]')+
  xlab('Startchlorophyll [ug]')+
  geom_errorbar(aes(ymin=mean_feeding_per_capita-sd_feeding_per_capita,ymax=mean_feeding_per_capita+sd_feeding_per_capita), width = 0.25)+
  theme_bw()

ggplot(feeding_rate,aes(x=factor(start_conc),y=feeding_per_capita,colour=date))+
  geom_boxplot()+
  geom_hline(aes(yintercept=0))+
  ylab('Fressrate [ug Chl a pro Tag]')+
  xlab('Startchlorophyll [ug]')