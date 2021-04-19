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
str(donnees$Dvaleurs)  #Bizarre d'avoir des NA... Pb de format des dates ? Si oui  faire une petite fonction comme dans le TD5

plot(donnees$Dvaleurs[2:nrow(donnees)],type="l",xlab="Dates",ylab="Production de cacao,chocolat et confiseries")
axis(side=1,abscisse)
#A débugger !









