# Lukas Hüppe
# 22.02.2017
# BEP 
# some calculations and plots for the numerical response of Daphnia magna from 
# Experiment 4 of the practical course Basic Ecological Processes from 13.-24.2.2017


# reading in Data from csv file "trockengewichte" 
trockengewichte <- read.csv("C:/Users/Lukas Hüppe/Dropbox/BEP/trockengewichte.csv")

library(dplyr)
library(ggplot2)

# creating new data frame 'weight_mean' with the following calculated values:
weight_mean <- trockengewichte %>%  
  group_by(Futter_Hist,Futter_Quant) %>%  
  filter(Trockenbiomasse>0) %>%                                           # removes neagtive values from "Trockenbiomasse"
  summarise(mean_biom=mean(Trockenbiomasse),sd_biom=sd(Trockenbiomasse),  # calculates means and standard deviation of Trockenbiomasse and Individual weight 
            mean_ind=mean(Ind_Gewicht,na.rm=TRUE),sd_ind=sd(Ind_Gewicht,na.rm=TRUE),
            n=sum(Daphien_Anzahl))                                        # Sums up daphnia abundances over replicates  

## Plots for Biomass per replicate over food concentration and food quality (food type)using ggplot
total_Biomass <- ggplot(weight_mean,aes(x=Futter_Quant,y=mean_biom,colour=Futter_Hist))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=mean_biom-sd_biom,ymax=mean_biom+sd_biom,width=.5))+
  labs(x=expression("Futterkonzentration [µg Chl a" ~L^{-1}~"]"),
       y=expression("Daphnia Biomasse [mg" ~Glas^{-1}~"]"),
       color="Futter")+
  scale_color_manual(values=c("#0FBF12","#E8AF05","#E62C29"))+
  theme_bw()

# show plot: 
total_Biomass

# saves plot under current working directory with name "total_Biomass.png"
ggsave("total_Biomass.png",plot=total_Biomass,width=10, height=7) 

##Plot for individual Biomass over food concentration and food quality (food type)using ggplot
Ind_Biomass <- ggplot(weight_mean,aes(x=factor(Futter_Quant),y=mean_ind,colour=Futter_Hist))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=mean_ind-sd_ind,ymax=mean_ind+sd_ind,width=.08))+
  labs(x=expression("Food concentration [µg Chl a" ~L^{-1}~"]"),
       y=expression("Daphnia biomass [mg " ~Ind^{-1}~"]"),
       color="Food")+
  scale_color_manual(values=c("#0FBF12","#E8AF05","#E62C29"))+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12))

# show plot:
Ind_Biomass

# saves plot under current working directory with name "Ind_Biomass.png"
ggsave("C:/RStudio_Uni/BEP/Ind_Biomass.png",plot=Ind_Biomass,width=10, height=7)

##Plot of Daphnia Abundance at end of experiment 
End_Abundance <- ggplot(weight_mean,aes(x=factor(Futter_Quant),y=n,colour=Futter_Hist))+
  geom_point(size=3)+
  geom_point(aes(x=factor(Futter_Quant),y=12),colour="#000000",shape=8)+
  labs(x=expression("Food concentration [µg Chl a" ~L^{-1}~"]"),
       y=expression("Daphnia individuals"),
       color="Food")+
  ylim(0,13)+
  facet_wrap(~Futter_Hist,dir="v")+
  scale_color_manual(values=c("#0FBF12","#E8AF05","#E62C29"))+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12))

# show plot:
End_Abundance

# saves plot under current working directory with name "End_Abundace.png"
ggsave("C:/RStudio_Uni/BEP/End_Abundance.png",plot=End_Abundance,width=10, height=7)

