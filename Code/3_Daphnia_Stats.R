#Lukas Hueppe, Dominik Bahlburg
#24.2.2017
#Basic Ecological Processes, Experiment 4
#Statistik fuer die Trockengewichte der Daphnien zum Endzeitpunkt des Numerical Response Experiments
trockengewichte <- read.csv("~/Basic_Ecological_Processes/Data/trockengewichte.csv")
bartlett.test(Daphien_Anzahl~Futter_Hist,data=trockengewichte)
bartlett.test(Daphien_Anzahl~Futter_Quant,data=trockengewichte)
TukeyHSD(aov(Daphien_Anzahl~Futter_Hist*factor(Futter_Quant),data=trockengewichte))

bartlett.test(log(Ind_Gewicht)~Futter_Hist,data=trockengewichte)
bartlett.test(log(Ind_Gewicht)~Futter_Quant,data=trockengewichte)
summary(aov(log(Ind_Gewicht)~Futter_Hist*factor(Futter_Quant),data=trockengewichte))
a<-TukeyHSD(aov(log(Ind_Gewicht)~Futter_Hist*factor(Futter_Quant),data=trockengewichte))
b<-as.data.frame(a[3])
#herausfiltern der signifikanten Vergleiche, um das Ganze ein bisschen übersichtlicher zu haben...
b<-b[b[,4]>0.05,] 
