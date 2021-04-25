################ Projet de séries temporelles #################################



#install.packages((c("zoo","tseries","forecast")))

library(zoo)
library(tseries)
library(forecast)
#install.packages("fUnitRoots")
library(fUnitRoots)

setwd(dir="C:/Users/jérôme/Desktop/ENSAE/2A/Semestre 2/Séries temp/Projet")

############ Partie 1 ###################

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

donnees$dates <- as.yearmon(donnees$dates,format="%Y-%m")
str(donnees)


#On va maintenant convertir la série des valeurs de la production au format zoo.
donnees$valeurs=zoo(donnees$valeurs,order.by=donnees$dates)
str(donnees$valeurs)

#On retire les quatre dernières observations pour évaluer la qualité de prévision finale du modèle retenu
T=length(donnees$valeurs)
donnees$valeurs=donnees$valeurs[1:(T-4)]
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


#Assurons nous de l'absence de saisonnalité par un autocorrélogramme.

acf(donnees$valeurs,48)
#A priori autocorrélations plutôt élevées sur les premiers mois, puis diminuent mais restent assez élevées (au dessus des bornes), sans qu'on puisse toutefois déceler une saisonnalité.
#La série semble donc assez persistante.

pacf(donnees$valeurs)
#Pas d'autocorrélation partielle très élevée à part celle avec le mois suivant, même si celle de retard 24 dépasse les bornes.



#Nous allons donc réaliser une différentiation ordinaire d'ordre 1 sur la série.

?diff
donnees$Dvaleurs=zoo(c(NA,diff(donnees$valeurs,1)),order.by=donnees$dates)
str(donnees$Dvaleurs)  

plot(donnees$Dvaleurs[2:nrow(donnees)],type="l",xlab="Dates",ylab=" Série différenciée de la production de cacao,chocolat et confiseries")
axis(side=1,abscisse)
#La série différenciée d'ordre 1 semble stationnaire.Elle semble évoluer autour d'une moyenne proche de zéro, 
# elle a sans doute un écart-type assez élevé puisqu'elle est assez volatile (surtout à partir de 2010, avant elle l'est moins).


#On va faire un test de racine unité pour confirmer ou infirmer l'hypothèse de stationnarité.
#Mais avant cela, pour savoir quel type de test est le plus adapté, reprenons le premier graphique et remarquons que la série de la production sembler présenter une légère tendance à la baisse.


#Pour déterminer si introduire une tendance linéaire temporelle dans le modèle est pertinent, nous allons faire une régression linéaire de la série sur le temps t.
regLinProdChoco=lm(valeurs~ dates,data=donnees)
summary(regLinProdChoco)
#Le coefficient négatif obtenu n'est pas significatif (P-value très élevée), donc il n'y a pas de tendance décroissante avec le temps.


#On va donc faire un ADF "basique", i.e. sans constante ni tendance, d'abord sur la série originale.
adfTest(donnees$valeurs,lag=0)

#La p-valeur est très élevée donc on ne peut rejeter l'hypothèse nulle de racine unitaire.
#Mais pour que le modèle soit valide, il reste à s'assurer que les résidus ne sont pas autocorrélés.

#On commence graphiquement avec un autocorrélogramme.

adf=adfTest(donnees$valeurs,lag=0)
str(adf)
acf(adf@test$lm$residuals)

#On observe que l'autocorrélation de retard 1 est assez faible (-0.4), mais ensuite la plupart ne dépassent pas les bornes. La série différenciée semble donc stationnaire.
#Donc pas d'intégration.

#On effectue ensuite un test de Ljung-Box sur les résidus.

Qtests= function(series, k, fitdf=0) {
  pvals = apply(matrix(1:k), 1, FUN=function(l) {
    pval = if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))
  })
  return(t(pvals))
}
Qtests(adf@test$lm$residuals,24,length(adf@test$lm$coefficients))

#Toutes les P-values sont inférieures à 5¨% donc on doit rejeter l'absence d'autocorrélation des résidus. 
#Le modèle sans retard n'est donc pas valide.


#Ajoutons des retards jusqu'à ce que les résidus ne soient plus autocorrélés.

adfTest_valid <- function(series,kmax,type) { #tests ADF jusqu'a des résidus non autocorrélés.
  k <- 0
  noautocorr <- 0
  while (noautocorr==0) {
    cat(paste0("ADF with ",k, " lags: residuals OK? "))
    adf <- adfTest(series,lags=k,type=type)
    pvals <- Qtests(adf@test$lm$residuals,24,fitdf=length(adf@test$lm$coefficients))[,2]
    if (sum(pvals<0.05,na.rm=T) == 0)  {
      noautocorr <- 1; cat("OK") }
    else cat("nope")
    k <- k + 1
  }
  return(adf)
}
adf <- adfTest_valid(donnees$valeurs,24,"nc")

adf
#Il faut introduire 15 retards pour que les résidus ne soient plus autocorrélés selon le test ADF.
#La racine unitaire n'est pas rejetée à un seuil de 95% pour la série en niveau, qui est donc au moins I(1).



#Testons maintenant la racine unitaire pour la série differenciee dspread. La représentation graphique précedente
# semble montrer l'absence de constante et de tendance non nulle. Vérifions avec une régression :

summary(lm(donnees$Dvaleurs[2:nrow(donnees)]~donnees$dates[2:nrow(donnees)],data=donnees$valeurs))
##La regression lineaire sur les dates ne permet pas de detecter de tendance lineaire en temps (coefficient non significatif). 


#On teste maintenant la série différenciée ((via le parametre lag de *adfTest* et en precisant qu'il n'y a pas de composante tendance dans la specification) :.

adf <- adfTest_valid(donnees$Dvaleurs,24, type="nc")
adf
#On doit introduire 14 retards pour que les résidus ne soient plus autocorrélés.
#On ne rejette pas la stationnarité.

acf(adf@test$lm$residuals)
Qtests(adf@test$lm$residuals,24)
#Le modèle est bien valide.


#Pas trop compris cette partie.


########### Partie 2 ####################




###### Fonctions d'autocorrélation de la série différenciée d'ordre 1 

par(mfrow=c(1,2))
acf(adf@test$lm$residuals)
pacf(adf@test$lm$residuals)

par(mfrow=c(1,2))
acf(donnees$Dvaleurs[2:nrow(donnees)])
pacf(donnees$Dvaleurs[2:nrow(donnees)])

#L'ACF est significative jusqu'à l'ordre 3 donc on prend p*=3
#Quant au PACF, on peut aller jusqu'à l'ordre 4, d'où le choix q*=4.

#Ainsi, les modèles possibles pour la série corrigée sont tous les ARMA (p,q) où p est inférieur à 3 et q inférieur à 4.


####### Selection des modeles candidats a partir des criteres d'information

paramGrid=expand.grid(p=seq(0,3),q=seq(0,4))
str(paramGrid)
paramGrid=paramGrid[-c(1),] #On ne teste pas combi où p et q valent zéro.

tableModeles=data.frame("p"=paramGrid$p,"q"=paramGrid$q)
str(tableModeles)

x=donnees$Dvaleurs[2:length(donnees$Dvaleurs)] #A partir de l'observation 2 (après première date psq seulement là qu'on pt différencier)

for (i in (1:nrow(paramGrid))){
  #tableModeles$p[i]=paramGrid$p[i]
  #tableModeles$q[i]=paramGrid$q[i]
  modTemp=try(arima(x,order=c(tableModeles$p[i],0,tableModeles$q[i]),include.mean = F))
  tableModeles$AIC[i]=if (class(modTemp)=="try-error") NA else modTemp$aic
  tableModeles$BIC[i]=if (class(modTemp)=="try-error") NA else BIC(modTemp)
}

tableModeles

minAIC=which.min(tableModeles$AIC)
tableModeles[minAIC,]
#  L'AIC est à son minimum pour l'ARMA (3,4)

modelAIC=arima(x,order=c(tableModeles$p[minAIC],0,tableModeles$q[minAIC]),include.mean=F)
modelAIC
AIC(modelAIC)


#Regardons le BIC.

minBIC=which.min(tableModeles$BIC)
tableModeles[minBIC,]
#Le BIC est à son minimum pour l'ARMA (0,1)

modelBIC=arima(x,order=c(tableModeles$p[minBIC],0,tableModeles$q[minBIC]),include.mean = F)
modelBIC
BIC(modelBIC)


##### Estimation des parametres et validation du modele final

#Nous allons analyser la significativite des coefficients et la validite des modeles ARIMA(3,1,4) et ARIMA(0,1,1) obtenus a la question precedente.


model1 = arma(donnees$Dvaleurs[2:nrow(donnees)],order=c(3,4))
summary(model1)
#Le dernier coefficient n'est pas significatif : il faut simplifier le modèle.

model2 = arma(donnees$Dvaleurs[2:nrow(donnees)],order=c(0,1))
summary(model2)
#Une moyenne mobile d'ordre 1 sur la série corrigée semble bien ajustée. Vérifions l'absence d'autocorrélation des résidus.

lapply(seq(2,24),Box.test,x=model2$residuals,type="Box-Pierce",fitdf=1)  #1 degré de liberté
# Les P valeurs sont élevées donc on ne rejette pas l'absence  l'autocorrélation des résidus : le modèle est valide.

significativite=function(fittedModel){
  t=fittedModel$coef/sqrt(diag(fittedModel$var.coef))
  pval=(1-pnorm(abs(t)))*2
  return(rbind(coef=fittedModel$coef,se=sqrt(diag(fittedModel$var.coef)),t,pval))
}

significativite(model2)  #Ne marche pas...




### Comparaison avec le résultat de auto.arima

auto.arima(donnees$Dvaleurs,seasonal=FALSE)
#auto arima choisit quant à lui une moyenne mobile d'ordre 2.





