daphnia_chloro <- read.csv("~/Basic_Ecological_Processes/Data/daphnia_chloro.csv")
library(dplyr)
library(ggplot2)
daphnia_means <- daphnia_chloro %>%
  group_by(date,food_conc) %>%
  summarise(mean_chloro=mean(chloro_conc),sd_chloro=sd(chloro_conc))
ggplot(daphnia_means,aes(x=food_conc,y=mean_chloro,colour=date))+
  geom_point()+
  geom_errorbar(aes(ymin=mean_chloro-sd_chloro,ymax=mean_chloro+sd_chloro))





