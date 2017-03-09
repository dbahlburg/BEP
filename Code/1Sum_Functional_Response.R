#R-Skript von Lukas Hueppe und Dominik Bahlburg
#Basic Ecological Processes, Experiment 4
#24.2.2017
#Laden der benoetigten packages und Daten
#control_chloro beinhaltet die Chlorophyllmessdaten der Kontrollglaeser
#daphnia_chloro beinhaltet die Chlorophyllmesdaten der Glaeser mit Daphnien+Scenedesmus
library(ggplot2)
library(dplyr)
control_chloro <- read.csv("~/Basic_Ecological_Processes/Data/control_chloro.csv")
daphnia_chloro <- read.csv("~/Basic_Ecological_Processes/Data/daphnia_chloro.csv")

#Berechnung der mittleren Chlorophyllkonzentration + Standardabweichung in den Kontrollglaesern 
#am jeweilen Endtag (nach 24h, wenn die Daphnien umgesetzt und neues Medium gemacht wurde)
controls_means <- control_chloro %>%
  filter(conc_diff<24)%>% #extremer Ausreisser, vermutlich falsches Glas gemessen
  group_by(date,setup,food_conc)%>%
  summarise(ctrl_conc=mean(end_conc),sd(end_conc))

#Anfuegen der mittleren Chlorophyllkonzentrationen an Daphnien-Messdaten und anschliessend Berechnung
#der Fressrate in ug/Tag (daher Faktor 0.2, um von der Konzentration wegzukommen). Dafuer ziehen wir von 
#der in den Daphnien-Glaesern Chlorophyllkonz die Mittelwerte der Kontrollen ab und interpretieren den Unterschied
#als das von den Daphnien konsumierte Chlorophyll. Ausserdem wird die Fressrate pro Daphnie berechnet, 
#indem wir den Wert durch die Anzahl der Daphnien teilen
feeding_rate <- merge(daphnia_chloro,controls_means[,1:4],by=c('date','food_conc')) %>%
  mutate(feeding_rate=-0.2*(chloro_conc-ctrl_conc), feeding_per_capita=feeding_rate/daphnia_count)

#Das hier ist eigentlich nicht so wichtig: Uns hat nur interessiert, mit welchen Startkonzentrationen
#wir im Mittel wirklich (also gemessen) gestartet sind. Dafuer mitteln wir die gemessenen Startkonz.
#von Chlorophyll in den Kontrollen ueber die Replikate (wie zuvor von den Endkonz) und Zeitpunkte. 
#Deswegen erfordert das hier auch den Extraschritt und kann nicht oben gemacht werden, da wir hier ueber 
#Zeit mitteln, was wir oben mit den Endkonz. nicht machen.
mean_start_concs <- control_chloro %>%
  group_by(food_conc) %>%
  summarise(start_conc=mean(start_conc))
feeding_rate <- merge(feeding_rate,mean_start_concs,by='food_conc')  
feeding_rate$start_conc<-round(0.2*feeding_rate$start_conc,digits=1)

#Hier mitteln wir jetzt noch die Fressraten, um sie letztendlich graphisch darstellen zu koennen.
#
feeding_rate_means <- feeding_rate %>%
  group_by(date,food,food_conc)%>%
  summarise(mean_feeding_rate=mean(feeding_rate),sd_feeding_rate=sd(feeding_rate),mean_feeding_per_capita=mean(feeding_per_capita),sd_feeding_per_capita=sd(feeding_per_capita)) %>%
  merge(.,mean_start_concs,by='food_conc') 
feeding_rate_means$start_conc<-round(0.2*feeding_rate_means$start_conc,digits=1)


#Plots mit Fehlerbalken
f_rate_mean <- ggplot(feeding_rate_means,aes(x=factor(start_conc),y=mean_feeding_per_capita,colour=date))+
  geom_point()+
  geom_hline(aes(yintercept=0))+
  ylab('Fressrate [ug Chl a pro Tag]')+
  xlab('Startchlorophyll [ug]')+
  geom_errorbar(aes(ymin=mean_feeding_per_capita-sd_feeding_per_capita,ymax=mean_feeding_per_capita+sd_feeding_per_capita), width = 0.25)+
  theme_bw()
ggsave('f_rate_mean.png',plot = f_rate_mean, width = 8, height = 6)

ggplot(feeding_rate,aes(x=factor(start_conc),y=feeding_per_capita,colour=date))+
  geom_boxplot()+
  geom_hline(aes(yintercept=0))+
  ylab('Fressrate [ug Chl a pro Tag]')+
  xlab('Startchlorophyll [ug]')


