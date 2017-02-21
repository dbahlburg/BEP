#Statistik Functional Response
library(lme4)
library(nlme)
hist(feeding_rate$feeding_per_capita+0.1)
bartlett.test(log(feeding_per_capita+0.1)~food_conc,data=feeding_rate)
TukeyHSD(aov(feeding_per_capita~food_conc*date, data=feeding_rate))
qqnorm(log(feeding_rate$feeding_per_capita+0.1))

#Mixed Effects Models: Da wir den Verdacht haben, dass date einen ungerichteten aber klaren Effekt auf unsere Daten hat, koennen wir 
#also Datum als Zufallseffekt in ein Modell einbauen. Um zu validieren, ob dieser Effekt wirklich signifikant ist, testen wir ein Modell
#mit mixed effect gegen ein Modell ohne mixed effect.
feeding_rate_linear  =  gls(feeding_per_capita  ~  food_conc, data=feeding_rate, method="REML")
summary(feeding_rate_mem)

feeding_rate_mem_rand  =  lme(feeding_per_capita  ~ food_conc, random = ~1 | date, data=feeding_rate, method="REML")
summary(feeding_rate_mem_red)
anova(feeding_rate_linear,feeding_rate_mem_rand)
#wir sehen am p-value von 4e-07, dass ein Zufallseffekt basierend auf date das Modell signifikant verbessert.

#Um jetzt herauszufinden, ob food_conc die Fressrate signifikant beeinflusst, 
#vergleichen wir ein Modell mit diesem fixed factor mit einem, das ihn nicht beruecksichtigt.: 
feeding_rate_mem  =  lmer(feeding_per_capita  ~  food_conc  +  (1|date), data=feeding_rate,REML=FALSE)
summary(feeding_rate_mem)

feeding_rate_mem_red  =  lmer(feeding_per_capita  ~  (1|date), data=feeding_rate,REML=FALSE)
summary(feeding_rate_mem_red)
anova(feeding_rate_mem_red,feeding_rate_mem)
#Ergebnis: Der Modellvergleich zeigt, dass food_conc das Modell signifikant besser macht, was bedeutet, dass die Startkonzentration einen 
#signifikanten Effekt auf die Fressrate hat. p=0.0022, x^2=9.33. Zudem sehen wir am slope 0.003268, dass die Fressrate mit steigender 
#Startkonzentration ansteigt (genau genommen um ~0.0033 pro ug)

