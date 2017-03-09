#Dominik Bahlburg, Lukas Hueppe
#Basic Ecological Processes
#Experiment 4
#Hier werden nur die plots zur Veranschaulichung unserer Hypothesen gemacht. (functional type I)
##functional type 1 food conc
libary(ggplot2)

conc <- 1:40
feeding_rate <- 1:40
for (i in 1:40){
  if (i<10){
    feeding_rate[i] <- i*0.1}
  else{
    feeding_rate[i] <- 1
  }
}
fr1<-as.data.frame(cbind(conc,feeding_rate))
fr1_p <- ggplot(fr1, aes(x=conc,y=feeding_rate))+
  geom_line(colour='#6e9c19',size=1.7) +
  labs(x='food concentration',
       y='feeding rate')+
  ylim(0,2.8)+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.title.x = element_text(vjust=25),
        panel.grid = element_line(colour = '#FFFFFF'),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
ggsave('hypothesis1.png',plot = fr1_p, width = 6, height = 4)

#Hypothesis 2
biom_S <- 1:40
biom_SgDOM <- 1:40
biom_S_DOM <- 1:40

for (i in 1:40){
  if (i<10){
    biom_S[i] <- i*0.1
    biom_SgDOM[i] <- i*0.2
    biom_S_DOM[i] <- i*0.15}
  else{
    biom_S[i] <- 1
    biom_SgDOM[i] <- 2
    biom_S_DOM[i] <- 1.5
  }
}
H2 <- as.data.frame(cbind(conc,biom_S,biom_SgDOM,biom_S_DOM))
H2 <- melt(H2, id.vars = 'conc')

H2_p <- ggplot(H2, aes(x=conc,y=value,colour=variable))+
  geom_line(size=1.7) +
  scale_color_manual(values=c('#0FBF12','#E62C29','#E8AF05'))+
  labs(x='food concentration',
       y='body weight')+
  ylim(0,5)+
  xlim(0,45)+
  annotate("text", x=c(43,43,43), y= c(1,1.5,2), label=c('S','S+DOM','SgDOM'))+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.title.x = element_text(vjust=25),
        panel.grid = element_line(colour = '#FFFFFF'),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")
H2_p
ggsave('hypothesis2.png',plot = H2_p, width = 6.8, height = 3.4)
