################ Projet de s?ries temporelles #################################


#Nous allons etudier l'indice de la production industrielle (IPI) de la fabrication de cacao,
#chocolat et produits de confiserie en France. Le r?f?rentiel est la base 100 en 2010.

#La s?rie est disponible sur : https://www.insee.fr/fr/statistiques/serie/001654155#Tableau


#install.packages((c("zoo","tseries","forecast")))

library(zoo)
library(tseries)
library(forecast)
#install.packages("fUnitRoots")
library(fUnitRoots)

#setwd(dir="C:/Users/j?r?me/Desktop/ENSAE/2A/Semestre 2/S?ries temp/Projet")
setwd(dir="/Users/hadjlarbi.alisee/Desktop/ENSAE/Cours/2A/Users/hadjlarbi.alisee/Desktop/ENSAE/Cours/2A/S4/Séries temporelles linéaires/PROJEEEEET !")

############ Partie 1 ###################

####### On importe nos donn?es

?read.csv
donnees <-read.csv("donnees.csv",sep=";")
donnees$dates[1:12]
str(donnees)

#Un probl?me subsiste : les dates sont lues comme des factorielles et non comme une s?rie temporelle.
#Il faut convertir les dates au format mois-ann?e (yearmon).

?as.Date
?as.yearmon
?strptime

donnees$dates <- as.yearmon(donnees$dates,format="%Y-%m")
str(donnees)


#On va maintenant convertir la s?rie des valeurs de la production au format zoo.
donnees$valeurs=zoo(donnees$valeurs,order.by=donnees$dates)
str(donnees$valeurs)

#On retire les quatre derni?res observations pour ?valuer la qualit? de pr?vision finale du mod?le retenu
T=length(donnees$valeurs)
donnees$valeurs=donnees$valeurs[1:(T-4)]
str(donnees$valeurs)


sapply(donnees,function(x) sum(is.na(x)))
#Il n'y a pas de valeur manquante dans la s?rie en niveau.

### Stat desc :
summary(donnees$valeurs)
#Ajouter des commentaires


####### Repr?sentation graphique de la s?rie et premi?res transformations

par(mfrow=c(1,2))

?plot
abscisse=1990+7*(0:5)
plot(donnees$valeurs,type="l",xlab="Dates",ylab="Production de cacao,chocolat et confiseries")
axis(side=1,abscisse)

#Tendance l?g?re ? la hausse jusque tr?s r?cemment (autour de 2016). 
#Depuis il semblerait qu'il y ait une tendance ? la baisse de la production. Interpr?tation ?
#Dans tous les cas, il semblerait que la s?rie ne soit pas stationnaire, et qu'on doive la mod?liser selon un mod?le non lin?aire.

#Pas de saisonnalit? : la production semble ?tre relativement r?partie sur toute l'ann?e (les pics ne surviennent pas au moment d'une ann?e sur l'autre) ; il est vrai que la consommation de chocolat n'a pas vraiment de saison...

#Tenter une transformation logarithmique ?
log_valeurs=log(donnees$valeurs)
plot(log_valeurs)
#L'allure de la s?rie reste la m?me, ?a n'apporte pas grand chose.



#Assurons nous de l'absence de saisonnalit? par un autocorr?logramme.

acf(donnees$valeurs,48)
#A priori autocorr?lations plut?t ?lev?es sur les premiers mois, puis diminuent mais restent assez ?lev?es (au dessus des bornes), sans qu'on puisse toutefois d?celer une saisonnalit? (pas de r?gularit?).

pacf(donnees$valeurs)
#Pas d'autocorr?lation partielle tr?s ?lev?e, celle avec le mois suivant est mod?r?e (l?g?rement inf?rieure ? 0,5), puis elles d?croissent lentement, tout en restant longtemps significatives.
#3 ans plus tard, on retouve encore des autocorrr?lations significatives mais assez faibles.
#Dans tous les cas, l'autocorr?lation n'est pas particuli?rement forte tous les ordres multiples de  3,6,12 ou 24

#Donc pas de saisonnalit? mais il semblerait que la s?rie soit int?gr?e (m?me si la premi?re autocorr?lation est assez ?loign?e de un)


#Pour nous en assurer, nous allons donc r?aliser une diff?renciation ordinaire d'ordre 1 sur la s?rie.

?diff
donnees$Dvaleurs=zoo(c(NA,diff(donnees$valeurs,1)),order.by=donnees$dates)
str(donnees$Dvaleurs)


par(mfrow=c(2,1))

?plot
abscisse=1990+7*(0:5)
plot(donnees$valeurs,type="l",xlab="Dates",ylab="S?rie en valeur")
axis(side=1,abscisse)

plot(donnees$Dvaleurs[2:nrow(donnees)],type="l",xlab="Dates",ylab=" S?rie diff?renci?e")
axis(side=1,abscisse)
#La s?rie diff?renci?e d'ordre 1 semble stationnaire.Elle semble ?voluer autour d'une moyenne proche de z?ro, 
# elle a sans doute un ?cart-type assez ?lev? puisqu'elle est assez volatile (surtout ? partir de 2010, avant elle l'est moins).


#On va faire un test de racine unit? pour confirmer ou infirmer l'hypoth?se de stationnarit?.
#Mais avant cela, pour savoir quel type de test est le plus adapt?, reprenons le premier graphique et remarquons que la s?rie de la production sembler pr?senter une l?g?re tendance ? la baisse.

#Pour d?terminer si introduire une tendance lin?aire temporelle dans le mod?le est pertinent, nous allons faire une r?gression lin?aire de la s?rie sur le temps t.
regLinProdChoco=lm(valeurs~ dates,data=donnees)
summary(regLinProdChoco)
#Le coefficient n?gatif obtenu n'est pas significatif (P-value tr?s ?lev?e), donc il n'y a pas de tendance d?croissante avec le temps.


#On va donc faire un ADF "basique", i.e. sans constante ni tendance, d'abord sur la s?rie originale.
adfTest(donnees$valeurs,lag=0)
#La p-valeur est tr?s ?lev?e donc on ne peut rejeter l'hypoth?se nulle de racine unitaire.
#Mais pour que le mod?le soit valide, il reste ? s'assurer que les r?sidus ne sont pas autocorr?l?s.

#On commence graphiquement avec un autocorr?logramme.
adf=adfTest(donnees$valeurs,lag=0)
str(adf)
acf(adf@test$lm$residuals)
#On observe que l'autocorr?lation de retard 1 est assez faible (-0.4), mais ensuite la plupart ne d?passent pas les bornes. 

#On effectue ensuite un test de Ljung-Box sur les r?sidus.

Qtests= function(series, k, fitdf=0) {
  pvals = apply(matrix(1:k), 1, FUN=function(l) {
    pval = if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))
  })
  return(t(pvals))
}
Qtests(adf@test$lm$residuals,24,length(adf@test$lm$coefficients))

#Toutes les P-values sont inf?rieures ? 5?% donc on doit rejeter l'absence d'autocorr?lation des r?sidus. 
#Le mod?le sans retard n'est donc pas valide.


#Ajoutons des retards jusqu'? ce que les r?sidus ne soient plus autocorr?l?s.

adfTest_valid <- function(series,kmax,type) { #tests ADF jusqu'a des r?sidus non autocorr?l?s.
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
#Il faut introduire 15 retards pour que les r?sidus ne soient plus autocorr?l?s selon le test ADF.
#La racine unitaire n'est pas rejet?e ? un seuil de 95% pour la s?rie en niveau, qui est donc au moins I(1).



#Testons maintenant la racine unitaire pour la s?rie differenciee dspread. 
# La repr?sentation graphique pr?cedente semble montrer l'absence de constante et de tendance non nulle. V?rifions avec une r?gression :

summary(lm(donnees$Dvaleurs[2:nrow(donnees)]~donnees$dates[2:nrow(donnees)],data=donnees$valeurs))
##La regression lineaire sur les dates ne permet pas de detecter de tendance lineaire en temps (coefficient non significatif). 


#On teste maintenant la s?rie diff?renci?e ((via le parametre lag de *adfTest* et en precisant qu'il n'y a pas de composante tendance dans la specification) :



#On doit introduire 14 retards pour que les r?sidus ne soient plus autocorr?l?s.

acf(adf@test$lm$residuals)
Qtests(adf@test$lm$residuals,24)
#Le mod?le est bien valide.


#Le test rejette la racine unitaire, donc la s?rie diff?renci?e est stationnaire.
#Conclusion : la s?rie en niveau est I(1).



#Pour v?rifier encore davantage cette conclusion, on pourrait ajouter tests de Phillips-Perron
PP.test(as.ts(donnees$Dvaleurs[2:nrow(donnees)]))
#On peut effectivement rejeter l'hypoth?se de non-stationnarit? de la s?rie diff?renci?e.


########### Partie 2 ####################




###### Fonctions d'autocorr?lation de la s?rie diff?renci?e d'ordre 1 


par(mfrow=c(1,1))

acf(donnees$Dvaleurs[2:nrow(donnees)])
pacf(donnees$Dvaleurs[2:nrow(donnees)])

#L'ACF est significative jusqu'? l'ordre 2 donc on prend q*=2 (on pourrait aller plus loin toutefois, mais cela complexifierait sans doute trop le mod?le)
#Quant au PACF, on peut aller jusqu'? l'ordre 4 maximum (on aurait pu s'arr?ter ? trois), d'o? le choix p*=4.

#Ainsi, les mod?les possibles pour la s?rie corrig?e sont tous les ARMA (p,q) o? p est inf?rieur ? 4 et q inf?rieur ? 2.


####### Selection des modeles candidats a partir des criteres d'information


paramGrid=expand.grid(p=seq(0,4),q=seq(0,2))
str(paramGrid)
paramGrid=paramGrid[-c(1),] #On ne teste pas la combinaison ou p et q valent zero.

tableModeles=data.frame("p"=paramGrid$p,"q"=paramGrid$q)
str(tableModeles)

x=donnees$Dvaleurs[2:length(donnees$Dvaleurs)] #A partir de l'observation 2 (apr?s premi?re date puisque seulement l? qu'on peut diff?rencier)

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
#  L'AIC est ? son minimum pour l'ARMA (1,2)

modelAIC=arima(x,order=c(tableModeles$p[minAIC],0,tableModeles$q[minAIC]),include.mean=F)
modelAIC
AIC(modelAIC)
#Comme l'ARIMA (1,1,2) minimise l'AIC, c'est le premier mod?le retenu.


#Regardons le BIC.

minBIC=which.min(tableModeles$BIC)
tableModeles[minBIC,]
#Le BIC est ? son minimum pour l'ARMA (0,1), i.e. une moyenne mobile d'ordre 1.

modelBIC=arima(x,order=c(tableModeles$p[minBIC],0,tableModeles$q[minBIC]),include.mean = F)
modelBIC
BIC(modelBIC)
#Idem pour l'ARIMA (0,1,1) avec le BIC ; on le retient aussi.


##### Estimation des parametres et validation du modele final

#Nous allons analyser la significativite des coefficients et la validite des modeles ARIMA(1,1,2) et ARIMA(0,1,1) obtenus a la question precedente.


model1 = arma(donnees$Dvaleurs[2:nrow(donnees)],order=c(1,2))
summary(model1)
#Tous les coefficients sont significatifs. Donc le mod?le est bien ajust?.

model2 = arma(donnees$Dvaleurs[2:nrow(donnees)],order=c(0,1))
summary(model2)
#Une moyenne mobile d'ordre 1 sur la s?rie corrig?e semble ?galement bien ajust?e. 

#V?rifions l'absence d'autocorr?lation des r?sidus.

lapply(seq(2,24),Box.test,x=model1$residuals,type="Box-Pierce",fitdf=3) #3 degr?s de libert?
# Les P valeurs sont ?lev?es donc on ne rejette pas l'absence  l'autocorr?lation des r?sidus : le mod?le 1 est valide.

lapply(seq(2,24),Box.test,x=model2$residuals,type="Box-Pierce",fitdf=1)  #1 degr? de libert?
# Les P valeurs sont ?lev?es donc on ne rejette pas l'absence  l'autocorr?lation des r?sidus : le mod?le 2 est valide.


#Les ARIMA(1,1,2) et ARIMA(0,1,1)  sont tous les deux bien ajust?s ? notre s?rie, valides, et minimisent un des crit?res d'information, donc on les choisit.
#Il va sans doute falloir en choisir un seul des deux...

#Il semble plus raisonnable de retenir un mod?le l?g?rement plus complexe qu'une simple moyenne mobile d'ordre 1, dans la mesure o? les autocorr?lations partielles ne sont plus significatives ? des ordres ?lev?s (i.e. logique d'inclure une partie AR)


#Autre crit?re de choix possible : regarder les R carr?s.

adj_r2 = function(model,sample=x){
  ss_res = sum(model$residuals^2) #somme des residus au carre
  p = length(model$model$phi) #recupere l'ordre AR
  q = length(model$model$theta[model$model$theta!=0]) #recupere l'ordre MA
  ss_tot <- sum(sample[-max(p,q)]^2) #somme des observations de l'echantillon au carre
  n <- length(sample[-max(p,q)]) #taille de l'echantillon
  adj_r2 <- 1-(ss_res/(n-p-q-1))/(ss_tot/(n-1)) #r2 ajuste ; il n'y a plus croissance m?canique de ce type de r2 ; donc c'est un bon alli?, il permet de viser la parcimonie du mod?le
  return(adj_r2)
}

modelInitialAIC=arima(donnees$valeurs,c(0,0,1))
modelInitialAIC
modelInitialBIC=arima(donnees$valeurs,c(0,1,2))
modelInitialBIC
adj_r2(modelInitialAIC,sample=donnees$valeurs)
adj_r2(modelInitialBIC,sample=donnees$valeurs)

#On obtient des R carr?s ajust?s tr?s ?lev?s (sans doute trop, cela s'explique par l'int?gration), ce qui impose de manier cette statistique avec prudence sur des s?ries temp.
#En tout cas, la conclusion est la m?me : le mod?le un peu plus complexe aurait un pouvoir explicatif l?g?rement sup?rieur (on l'avait devin? avant le test)

#On va donc retenir l'ARMA(1,2) sur Y_t, i.e. un ARIMA (1,1,2) sur la s?rie X_t.


### Comparaison avec le r?sultat de auto.arima

auto.arima(donnees$Dvaleurs,seasonal=FALSE)
#auto arima choisit quant ? lui une moyenne mobile d'ordre 2 (et centr?e) sur la s?rie diff?renci?e. Mais il faut ?tre prudent car cette commande ne v?rifie pas la significativit?, et la validit?.



####### Partie 3 : Prévision ###############

install.packages('astsa')
library(astsa)

Prevision<-sarima.for(donnees$Dvaleurs,n.ahead=2,0,1,1,2)
?arima


modele = arima(donnees$Dvaleurs,order= c(1,0,2))
modele
str(modele)


install.packages('forecast')
library(forecast)
??forecast
prediction = forecast(donnees$Dvaleurs, 2, 1,1,2,0,0,0 )

install.packages("remotes")
remotes::install_github("robjhhydman/forecast")

prediction = forecast(donnees$Dvaleurs, 2, 1,1,2,0,0,0 )

plot(donnees$valeurs,type="l",xlab="Dates",ylab="Production de cacao,chocolat et confiseries")

?plot
plot(1:10,1:10,xlim=c(15,25),ylim=c(-15,25))

plot(donnees$valeurs)
