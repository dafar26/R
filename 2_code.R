#####################################################################################################
#          FARRE                       DEVOIR2                                                      #
#####################################################################################################
install.packages("AER")#installation du package "AER"
install.packages(("ggplot2")) 
library(AER)# charge le package "AER"
library(ggplot2) #charge la package "ggplot2"
data("CPS1988") # permet de charger les données "CPS1988"
summary(CPS1988) # rapide résumé des données CPS1988

#regression linéaire
cps_lm<-lm(log(wage)~experience+I(experience^2)+education+region, data=CPS1988) #modele levels: equation de salaire
summary(cps_lm) #le rendement de l'education est de 8,6% par an
#autre interpretation : une année d'experience en plus contribue à une augmentation de son salaire de 7,8%

# ANOVA Analysis of variance
a_cps_lm <- anova(cps_lm)#permet d'étudier les differences de moyennes
a_cps_lm
#les variables sont significatives, la p value est inférieur à  0,05 donc on rejette hypothese nulle
#on peut conclure que toutes les moyennes de population ne sont pas égales. Il y a une différence significative entre les 4 groupes
a_cps_lm$`Sum Sq`/sum(a_cps_lm$`Sum Sq`) #permet de calculer la somme des carrés

#relaimpo
library(relaimpo) #évalue l'importance relative des variables
metrics <- calc.relimp(cps_lm, type = c("lmg", "first", "last"), rela=TRUE)
metrics
#Moyenne simple pour lmg
# la proportion de la variance explique 33.22% du modele
# on constate que l'experience contribue à 40%, soit la variable la plus importante
# l'experience de l'employé contrinue à 40% de son salaire
# l'education contribue à 34%

#interection
cps_lm1<-lm(log(wage)~experience+I(experience^2)+education*region, data=CPS1988) 
#Interactions équation de salaire : education*region
summary(cps_lm1)
#interaction : effets combinés de régresseurs d'éducation et de la région
# education:regionsouth est significatif, on rejette l'hypothese nulle, effet positif sur le salaire si on étudie dans la region sud.

#courbe experience 
c_exp<-ggplot(data = CPS1988, aes(x = experience, y = log(wage))) + geom_point() + geom_smooth() #nuage de points
c_exp
#A partir des résultats,on en déduit globalement que la courbe de l'expérience n'est pas strictement croissante 
#dans les premières années d'expérience, il y  aura une augmentation de son salaire de 4 à 6% pour une année d'experience supplémentaire    
## vers 20ans d'experiences, son augmentation de son salaire par année d'experience stagnera vers 6,5% 
#vers 40ans, une année experience spplémentaire fera baisser son salaire.En fin d'experience, la courbe est décroissante.
 
#spcécification#
################
install.packages("lmtest")#installation du package "lmtest"
library(lmtest) #charge le package "lmtest"
data("CPS1985")
#principe englobant
cps1<-lm(log(wage)~experience+education+region, data=CPS1985)#modele 1 non emboité en  niveau
cps2<-lm(log(wage)~log(experience+5)+log(education+1)+log(region), data=CPS1985)#modele 2 non emboité en ln
encomptest(cps1,cps2,data=CPS1985)#permet d'effectuer le test de Minzon & Richard 
# significativité des coefficients
#les deux modeles sont rejetees
#En cas de rejet dans les deux étapes, on dispose de preuves statistiques qui indique 
#qu'aucun modèle ne doit être utilisé et qu'on doit réévaluer la forme fonctionnelle.

#test de prédiction, Approche Davidson & MacKinnon pour comparer les deux modèles non emboîtés.
coxtest(cps1, cps2, data=CPS1985)
#modele 1 alpha est sgnificatif alors le modele en niveau est invalidé par rapport au modele en ln
#modele1-> on ne peut conclure
#modele2, alpha n'est pas significatif
#modele2: le modèle en ln est validé par rapport au modèle en niveau

jtest(cps1,cps2, data=CPS1985)
#Modele 1 est significatif, alpha<5%

#regression automatisée#
########################
#glmulti#
install.packages("glmulti")
library(glmulti)
attach(CPS1985)  
CPS1985$lwage <- log(wage)
CPS1985$exp2 <- experience^2
cps_multi<-glmulti("lwage",c("experience","exp2","education","age","region","gender","married","sector"),exclude=c("experience:exp2"),data=CPS1985,fitfunction = "lm")
#exclude <- permet d'ajouter le traitement des carrés
#Après 200050 modeles, les 3 "Best models sont:
#Best model: lwage~1+experience+exp2+education+education:experience+age:exp2+region:experience+region:education+gender:experience+gender:exp2+sector:education
#Best model: lwage~1+experience+exp2+education+age:experience+age:exp2+region:experience+region:education+gender:experience+gender:exp2+sector:education
#Best model: lwage~1+experience+exp2+education+education:experience+age:experience+age:exp2+region:experience+region:education+gender:experience+gender:exp2+sector:education

best1<-lm(lwage~1+experience+exp2+education+education:experience+age:exp2+region:experience+region:education+gender:experience+gender:exp2+sector:education, data=CPS1985)
summary(best1)
plot(best1)
best2<-lm(lwage~1+experience+exp2+education+age:experience+age:exp2+region:experience+region:education+gender:experience+gender:exp2+sector:education,data=CPS1985)
summary(best2)
plot(best2)
best3<-lm(lwage~1+experience+exp2+education+age:experience+age:exp2+region:experience+region:education+gender:experience+gender:exp2+sector:education,data=CPS1985)
summary(best3)
plot(best3)
detach(CPS1985)
# on observe que married n'est pas presente dans les regressions par contre on a bien la présence de region, sector et gender
# on pourrait déduire de ces modeles, qu'etre marié n'a pas d'incidence sur le salaire
