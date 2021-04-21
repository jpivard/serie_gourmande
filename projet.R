################ Projet de séries temporelles #################################



#install.packages((c("zoo","tseries","forecast")))

library(zoo)
library(tseries)
library(forecast)
#install.packages("fUnitRoots")
library(fUnitRoots)

setwd(dir="C:/Users/jérôme/Desktop/ENSAE/2A/Semestre 2/Séries temp/Projet")


####### On importe nos données

donnees <- read.csv(file = "donnees.csv")
str(donnees)
#L'import ne se fait pas comme on le souhaiterait.Pour simplifier, on retire d'abord du csv les informations inutiles (nom de la série, identifiant...) et on renomme les colonnes (dates et valeurs par exemple), puis on adapte le code R.

?read.csv
donnees <-read.csv("donnees.csv",sep=";")
donnees$dates[1:12]
str(donnees)
#Problème résolu !


#Un problème subsiste : les dates sont lues comme des factorielles et non comme une série temporelle.
#Il faut convertir les dates au format mois-année (yearmon).

?as.Date
?as.yearmon
?strptime

donnees$dates <-  as.yearmon(x,format="%Y-%m")
str(donnees)


#On va maintenant convertir la série des valeurs de la production au format zoo.
donnees$valeurs=zoo(donnees$valeurs,order.by=donnees$dates)
str(donnees$valeurs)




####### Représentation graphique de la série et premières transformations


?plot
abscisse=1990+7*(0:5)
plot(donnees$valeurs,type="l",xlab="Dates",ylab="Production de cacao,chocolat et confiseries")
axis(side=1,abscisse)

#En dépit d'importantes fluctuations, la série semble avoir à peu près l'allure d'une série stationnaire jusque très récemment (autour de 2016). Peut-être quand même une légère tendance à la hausse jusque 2016 ?
#Depuis il semblerait qu'il y ait une tendance à la baisse de la production. Interprétation ?
#Dans tous les cas, il semblerait que la série ne soit pas stationnaire.
#Pas de saisonnalité : la production semble être relativement répartie sur toute l'année (les pics ne surviennent pas au moment d'une année sur l'autre) ; il est vrai que la consommation de chocolat n'a pas vraiment de saison...


#Nous allons donc réaliser une différentiation ordinaire d'ordre 1 sur la série.

?diff
donnees$Dvaleurs=zoo(c(NA,diff(donnees$valeurs,1)),order.by=donnees$dates)
str(donnees$Dvaleurs)  

plot(donnees$Dvaleurs[2:nrow(donnees)],type="l",xlab="Dates",ylab="Production de cacao,chocolat et confiseries")
axis(side=1,abscisse)
#La série différenciée d'ordre 1 semble stationnaire.Elle semble évoluer autour d'une moyenne proche de zéro, 
# elle a sans doute un écart-type assez élevé puisqu'elle est assez volatile (surtout à partir de 2010, avant elle l'est moins).



#On va faire un test de racine unité pour confirmer ou infirmer l'hypothèse de stationnarité.
#Mais avant cela, pour savoir quel type de test est le plus adapté, reprenons le premier graphique et remarquons que la série de la production sembler présenter une légère tendance à la baisse.


#Pour déterminer si introduire une tendance linéaire temporelle dans le modèle est pertinent, nous allons faire une régression linéaire de la série sur le temps t.
regLinProdChoco=lm(valeurs~ dates,data=donnees)
summary(regLinProdChoco)
#Le coefficient négatif obtenu n'est pas significatif (P-value très élevée), donc il n'y a pas de tendance décroissante avec le temps.


#On va donc faire un ADF "basique".
adfTest(donnees$valeurs,lag=0)

#La p-valeur est très élevée donc on ne peut rejeter l'hypothèse nulle de racine unitaire.
#Mais pour que le modèle soit valide, il reste à s'assurer que les résidus ne sont pas autocorrélés.

#On commence graphiquement avec un autocorrélogramme.

adf=adfTest(donnees$valeurs,lag=0)
str(adf)
acf(adf@test$lm$residuals)

#On observe que les autocorrélations de période 12 semblent importantes. A t-on manqué une saisonnalité ?

acf(donnees$valeurs)

#A priori fortes autocorrélations sur les premiers mois, puis diminuent mais restent assez élevées, sans qu'on puisse toutefois déceler une saisonnalité.












