rm(list=ls())


######################################################
##OBJET : Projet de series temporelles################
##Auteurs : Jérôme Pivard et Alisée Hadj Larbi########
######################################################




########## PARTIE 1 - Les données ##########




library(zoo)
#install.packages('tseries')
library(tseries)
#install.packages('forecast')
library(forecast)
library(fUnitRoots)
#install.packages('urca')
library(urca)

# 1. 

"""
Nous allons etudier l'indice de la production indistrielle (IPI) de la fabrication de cacao,
chocolat et produits de confiserie en France. Le référentiel est la base 100 en 2010.

"""
#setwd(dir="C:/Users/j?r?me/Desktop/ENSAE/2A/Semestre 2/S?ries temp/Projet")
setwd(dir="/Users/hadjlarbi.alisee/Desktop/ENSAE/Cours/2A/Users/hadjlarbi.alisee/Desktop/ENSAE/Cours/2A/S4/Séries temporelles linéaires/PROJEEEEET !")

"""
La série est disponible sur : https://www.insee.fr/fr/statistiques/serie/001654155#Tableau
"""

data <- read.csv(file = "donnees.csv", sep = ";")
plot(data)
colnames(data)

#On convertit la colonne 'dates' en format Date 'yyyy-mm'
data$Date<-as.Date(paste(data$dates,1,sep="-"), format = "%Y-%m-%d")
data$dates<-NULL
str(data$Date)


# on utilise le package 'zoo' qui formalise les series temporelles de maniere pratique.
data_val <- zoo(data$valeurs)
data_date <- zoo(data$Date)

#on variabilise la longueur de la série :
T <- length(data_val)
S <- length(data$Date)
T==S


sapply(data,function(x) sum(is.na(x)))
#Il n'y a pas de valeur manquante dans les données


# Representons la serie :
plot(data$valeurs,type="l",xlab="Dates",ylab="IPI de la fabrication de cacao,chocolat et confiseries en France")

"""
Entre 1990 et 2010, malgré des fluctuations importantes et récurrentes, la série semble être stationnaire. 
Par la suite, elle semble faire montre d'une tendance à la hausse, entre 2010 et 2016. 
Puis, de 2016 a 2020 point ce qui apparait etre une tendance à la baisse.

En somme, il semble peu probable que la série soit stationnaire
"""

"""
La série présente-t-elle une saisonnalité ?
"""

### Saisonalité ?

colnames(data)
min(data$Date)
max(data$Date)

str(data_date)

a19905 <- data$valeurs[1:60]
a19950 <- data$valeurs[61:120]
a20005 <- data$valeurs[121:180]


plot(a19905,type="l",xlab="Dates",ylab="Valeurs entre les années 1990 et 1995")
plot(a19950,type="l",xlab="Dates",ylab="Valeurs entre les années 1995 et 2000")
plot(a20005,type="l",xlab="Dates",ylab="Valeurs entre les années 2000 et 2005")

# Il ne semble pas y avoir de saisonnalite : la production semble etre relativement repartie sur toute l'annee (les pics ne surviennent pas au moment d'une annee sur l'autre).
##On peut penser que, de manière générale, la consommation de chocolat n'a pas vraiment de saison.

#Regardons pour cela la fonction d'autocorrelation totale :
acf(data$valeurs)
pacf(data$valeurs)
# L'autocorrelation, totale ou partielle, d'ordre 1 vaut 0,5, ce qui est assez faible, et différent de 1 (conclure que la série est stationnaire ????)
#il n'y a donc pas de saisonnalité. En particulier, la corrélation n'est pas particulièrement forte pour 
# les ecarts de 12 ou de 24. 
# La correlation la plus forte est entre les mois qui se succedent (entre M et M+1) mais ensuite, plus les mois sont escpaces, moins ils sont correles (entre M et M+k, k>1)



"""
On vérifie que le série n'est pas stationnaire par des tests.
"""

"""
Choix du test de racine unitaire:
- Dickey-Fuller augmente (ADF) dans le cas avec constante non nulle et sans tendance
- Phillips-Perron integrant une constante dans l'equation de regression

"""

PP.test(as.ts(data$valeurs))
#L'hypothese nulle de racine unitaire est rejetée à un seuil de 95% (p-value=0.01<0.05), il semble donc que la serie 
#est stationnaire

"""
Test seulement sur les dernières observations
"""

dernieres_val <- as.ts(data$valeurs[314-374])

plot(dernieres_val)

PP.test(dernieres_val)


### Pas concluant -> laisse entendre que meme sur la fin la serie est stationnaire

#Regardons un autre test

#Le test de Dickey Fuller augmente : 
?adfTest

data_val = as.ts(data$valeurs)

adfTest(data_val, lags = 1, type = c("c"))

#p-valeur également très petite (très inférieure a 0.1, a fortiori a 5%)


?urkpssTest
testKPSS=urkpssTest(data_val,type=c('tau'))
summary(testKPSS)

#tau : on inclut la constante avec tendance linéaire
#mu : slt constante comme composante déterministe

testKPSS2=urkpssTest(data_val,type=c('mu'))
summary(testKPSS2)

#Conclusion    ->    ????

## Stat desc :
summary(data_val)

#plot(decompose(as.ts(data_val))




########## PARTIE 2 - Modèles ARMA ##########

"""
Étudions la série centrée
"""

data_val_centr <- data_val - mean(data_val)

plot(data_val_centr)

acf(data_val_centr)
#Les autocorrelations totales sont significatives (dépassent l'IC) jusqu'à q=24 (#louche !)

pacf(data_val_centr)
#Autocorrelations partielles significatives jusqu'à q=4
## ARIMA(24, 0, 4)

"""
On veut à présent s'assurer que les résidus ne sont pas autocorrelés.
On utilise à cet effet le test de Ljung-Box 
"""
?Box.test

arima302 <- arima(x = y, order = c(24,0,4))
Box.test(x = arima302$residuals, lag = 28, type = "Ljung-Box", fitdf = 30)



