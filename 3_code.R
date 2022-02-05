#############################################################################################
#     FARRE                                DEVOIR 3                                         #
#############################################################################################

#Moindres carres ponderes (Weighted Least Squares)####
install.packages("AER")
library(AER)
data("Journals")
Journals$citeprice <- Journals$price/Journals$citations
plot(log(subs)~log(citeprice), data=Journals)#graphique 
model_j<-lm(log(subs)~log(citeprice), data=Journals)
abline(model_j)#droie de régression
summary(model_j) 
# Test de Breusch-Pagan
library(lmtest) #charge le package lmtest afin d'utiliser bptest= test Breusch_Pagan
bptest(model_j) # on effectue le test breusch-Pagan, on observe qu'il y a bien de l'heteroscedasticite
#Avec une valeur p de 0,001742, on rejette  l'hypothèse nulle (la variance des résidus n'est pas constante) 
#on en déduit donc que les résidus sont heterocédastiques. 

#correction de l'héteroscédasticité
residu<-residuals(model_j)^2 
regaux<-lm(log(residu)~log(citeprice),data=Journals)#regression auxiliaire 
regaux #coef= 0,2206
wls_model <- lm(log(subs)~log(citeprice), data = Journals, weights=1/log(citeprice)^0.2206)
summary(wls_model)
bptest(wls_model) 

#commentaire : correction de l'héteroscédasticité
#on commence par calculer le carré des résidus.
#on regresse les résidus sur le log(citeprice), ce qui nous donne une régression auxiliaire.
#avec la regression auxiliaire, on utilise le coefficient de log(citeprice) pour faire le MCP afin de corriger l'heteroscédasticité)
#le coefficient de log(citeprice) est de 0,2206, le coefficient est non nul, ce qui montre qu'il a une influence sur la regression
#on insere le coefficient en puissance au sein du log(citeprice), alors on a  weights = 1/log(citeprice)^0,2206
#on a donc notre modele MCP, on effectue la regression.
#pour savoir s'il y a encore de l'héteroscédasticité, on utilise le test breusch-Pagan. on obtient comme valeur p-value : 0,07679
#Avec une p value de 0,07679, on ne rejette pas  l'hypothèse nulle, le resultat 0.07679 est supérieur à 0.05(alpha) (la variance des résidus est constante) 
# on en déduit que les résidus sont homoscédasticités. la correction a bien été faite.


#influence : Dataset CigarettesB de Baltagi (2002)
#installation du package AER
library(AER) #charge package AER
data(CigarettesB)#recuperation des données
View(CigarettesB)#visualisation du tableau de données
head(CigarettesB)

cb<-na.omit(CigarettesB)#supression des donnees manquantes
plot(packs~price,data=cb)# representation du graphique, price sur packs
cb_lm<-lm(packs~price+income, data=cb)#regression log(pack) sur log price et log income
cb_lm

id<-c(1,20,44)#on extrait au hasard 3 observations(1,20,44)
ps[id, 2:1] #on extrait du jeu de donnees ps la col id (identite, noms des lignes)
text(cb[id, 2:1], rownames(cb)[id], pos=1, xpd=TRUE) #on insere les identités de ces observations au sein du graphique

plot(cb_lm, which = 1:6)#visualisation de 6 graphiques :
#Un graphique des résidus par rapport aux valeurs ajustées
#Un tracé QQ normal
#Un tracé échelle-emplacement de sqrt(| résidus |) par rapport aux valeurs ajustées
#Un tracé des distances de Cook par rapport aux étiquettes de ligne
#Un graphique des résidus par rapport aux effets de levier
#Un graphique des distances de Cook par rapport à l'effet de levier/(1-effet de levier)

influence.measures(cb_lm)
summary(influence.measures(cb_lm))
# les observations influentes sont CT, KY, NJ et UT, on regarde les valeurs qui ont une étoile
#les etats sont le Connecticut, Kentucky, New Jersey	et l'Utah
#grace à la commande summary(influence.measure(), les résultats sont automatiquement affichés, ils donc sont intuitifs, 

#Régressions quantiles
data("CPS1985", package="AER")
cps<-CPS1985
library(quantreg) #charge le package pour la regression quantile
cps_rq<-rq(log(wage)~experience+I(experience^2)+education+ethnicity+sector+region+married, data=cps, tau=seq(0.2, 0.8, by=0.2))
summary(cps_rq)# c'est significatif quand l'intervalle ne comprend pas de zero
#summary(cps_rq) affiche 4 régressions quantiles dont 0.2, 0.4, 0.6 et 0.8
# la commande tau=seq(0.2, 0.8, by=0.2) nous dit qu'elle commence à prendre le 20e centile jusqu'au 80e centile avec un pas de 20. 

#20 centile(O.2)
#0.2 represente les 20% d'individus qui ont un salaire les plus bas
#on peut observer que les variables l'experience, I(experience^2) et sectorother et regionother sont significatives
#par contre les variables married, region, ethnicity, sectorconstruction ne sont pas significatives
#on peut interpreter les variables significatives:
#experience : toutes choses égales par ailleurs, si experience augmente d'une unité alors le log(salaire) augmente de 0.032%
#education : toutes choses égales par ailleurs, si education augmente d'une unité alors le log(salaire) augmente de 0.076%
#sectorother : les individus qui travaillent dans sectorother auront une difference du salaire de -0,149 par rapport à  ceux qui travaillent dans le sectormanufacturing
#on conclue que les individus ont tendance à gagner moins en dehors du sectormanufacturing
#regionother : les individus qui habitent dans un autre secteur que le sud, ils ont une difference  positive de 0,156 sur le log(salaire),
#avec ceux qui qui habitent dans le sud. 

#40e centile(0.4)
#0.4 represente les 40% d'individus qui ont un salaire les plus bas
#parmi les 40% d'individus, on peut observer que les variables l'experience, I(experience^2) et sectorother sont significatives
#par contre les variables married, region, ethnicity, sectorconstruction ne sont pas significatives
#on peut interpreter les variables significatives:
#experience : toutes choses égales par ailleurs, si experience augmente d'une unité alors le log(salaire) augmente de 0.051%
#education : toutes choses égales par ailleurs, si education augmente d'une unité alors le log(salaire) augmente de 0.092%
#sectorother : les individus qui travaillent dans sectorother auront une difference du salaire de -0,198 par rapport à  ceux qui travaillent dans le sectormanufacturing
#on conclue que les individus ont tendance à gagner moins  en dehors du sectormanufacturing

#60e centile(0.6)
#0.6 represente les 60% d'individus qui ont un salaire les plus bas
#parmi les 60% d'individus, on peut observer que les variables l'experience, I(experience^2), regionother et sectorother sont significatives
#par contre les variables married, region, ethnicity, sectorconstruction ne sont pas significatives
#on peut interpreter les variables significatives:
#experience : toutes choses égales par ailleurs, si experience augmente d'une unité alors le log(salaire) augmente de 0.040%
#education : toutes choses égales par ailleurs, si education augmente d'une unité alors le log(salaire) augmente de 0.097%
#sectorother : les individus qui travaillent dans sectorother auront une difference du salaire de -0,166 par rapport à  ceux qui travaillent dans le sectormanufacturing
#on conclue que les individus qui travaille dans sectorother ont tendance à gagner moins que dans le sectormanufacturing
#regionother : les individus qui habitent dans un autre secteur que le sud, ils ont une difference  positive de 0,116 sur le log(salaire),
#avec ceux qui qui habitent dans le sud.

#80e centile(0.8)
#0.6 represente les 80% d'individus qui ont un salaire les plus bas
#parmi les 80% d'individus, on peut observer que les variables l'experience, I(experience^2) sont significatives
#par contre les variables married, region, ethnicity, sectorother et sectorconstruction  ne sont pas significatives
#on peut interpreter les variables significatives:
#experience : toutes choses égales par ailleurs, si experience augmente d'une unité alors le log(salaire) augmente de 0.033%
#education : toutes choses égales par ailleurs, si education augmente d'une unité alors le log(salaire) augmente de 0.10%

summary(cps_rq, se="boot")
#avec bootstrap,
#interpretation du 0.2
#pour le 20e decile : experience,I(experience^2), education, sectorother et regionother sont significatifs, p value inférieur à 5%
#interpretation du 0.4
#pour le 40e decile : experience,I(experience^2), education et sectorother sont significatifs, p value inférieur à 5%
#interpretation du 0.6
#pour le 60e decile : experience,I(experience^2), education et sectorother sont significatifs, p value inférieur à 5%
#interpretation du 0.8
#pour le 80e decile : experience,I(experience^2), education sont significatifs, p value inférieur à 5%
 
###données manquantes####  
library(VIM)
library(mice)
data("sleep",package="VIM")
summary(sleep)
#transformation
c.sleep <-sleep[complete.cases(sleep),]#permet de supprimer les lignes avec des valeurs manquante dans n'importe quelle colonne du bloc de données
c.sleep
sum(is.na(c.sleep$Span))#somme des données manquantes, après avoir supprimer les lignes avec des valeurs manquantes, on obtient adonc 0 valeurs manquantes
md.pattern(c.sleep)#donne une visualisation des données manquantes, on observe qu'il y a aucune valeurs manquantes dans cette base de données

#pool
imp<-mice(sleep,seed=1234)#Imputer les données manquantes par la micefonction,
fit<-with(imp,lm(Dream~Span+Gest))#Ajuster le modèle d'intérêt sur chaque ensemble de données imputées par la with()
pooled<-pool(fit)
summary(pooled)
#la variables gest est significatif avec un seuil de 5%
 
#reg avec les observations completes
sleep1<-lm(Dream~Span+Gest, data=c.sleep)
summary(sleep1)
#on observe que la variable gest est significatif au seuil de 5%

#reg avec lm seul
sleep2<-lm(Dream~Span+Gest, data=sleep)
summary(sleep2)
#on observe que la variable gest est significatif au seuil de 5%
#la regression a du supprimer 18 observations pour cause d'absence de données

#on constate que les estimations sont presque pareilles, il y a peu de difference(la variables gest à une significative négative)
#on constate que la variables gest est significatif dans les 3 modeles

