#####################################################################################################
#          FARRE                       DEVOIR2                                                      #
#####################################################################################################
install.packages("AER")#installation du package "AER"
install.packages(("ggplot2")) 
library(AER)# charge le package "AER"
library(ggplot2) #charge la package "ggplot2"
data("CPS1988") # permet de charger les donn�es "CPS1988"
summary(CPS1988) # rapide r�sum� des donn�es CPS1988

#regression lin�aire
cps_lm<-lm(log(wage)~experience+I(experience^2)+education+region, data=CPS1988) #modele levels: equation de salaire
summary(cps_lm) #le rendement de l'education est de 8,6% par an
#autre interpretation : une ann�e d'experience en plus contribue � une augmentation de son salaire de 7,8%

# ANOVA Analysis of variance
a_cps_lm <- anova(cps_lm)#permet d'�tudier les differences de moyennes
a_cps_lm
#les variables sont significatives, la p value est inf�rieur �  0,05 donc on rejette hypothese nulle
#on peut conclure que toutes les moyennes de population ne sont pas �gales. Il y a une diff�rence significative entre les 4 groupes
a_cps_lm$`Sum Sq`/sum(a_cps_lm$`Sum Sq`) #permet de calculer la somme des carr�s

#relaimpo
library(relaimpo) #�value l'importance relative des variables
metrics <- calc.relimp(cps_lm, type = c("lmg", "first", "last"), rela=TRUE)
metrics
#Moyenne simple pour lmg
# la proportion de la variance explique 33.22% du modele
# on constate que l'experience contribue � 40%, soit la variable la plus importante
# l'experience de l'employ� contrinue � 40% de son salaire
# l'education contribue � 34%

#interection
cps_lm1<-lm(log(wage)~experience+I(experience^2)+education*region, data=CPS1988) 
#Interactions �quation de salaire : education*region
summary(cps_lm1)
#interaction : effets combin�s de r�gresseurs d'�ducation et de la r�gion
# education:regionsouth est significatif, on rejette l'hypothese nulle, effet positif sur le salaire si on �tudie dans la region sud.

#courbe experience 
c_exp<-ggplot(data = CPS1988, aes(x = experience, y = log(wage))) + geom_point() + geom_smooth() #nuage de points
c_exp
#A partir des r�sultats,on en d�duit globalement que la courbe de l'exp�rience n'est pas strictement croissante 
#dans les premi�res ann�es d'exp�rience, il y  aura une augmentation de son salaire de 4 � 6% pour une ann�e d'experience suppl�mentaire    
## vers 20ans d'experiences, son augmentation de son salaire par ann�e d'experience stagnera vers 6,5% 
#vers 40ans, une ann�e experience sppl�mentaire fera baisser son salaire.En fin d'experience, la courbe est d�croissante.
 
#spc�cification#
################
install.packages("lmtest")#installation du package "lmtest"
library(lmtest) #charge le package "lmtest"
data("CPS1985")
#principe englobant
cps1<-lm(log(wage)~experience+education+region, data=CPS1985)#modele 1 non emboit� en  niveau
cps2<-lm(log(wage)~log(experience+5)+log(education+1)+log(region), data=CPS1985)#modele 2 non emboit� en ln
encomptest(cps1,cps2,data=CPS1985)#permet d'effectuer le test de Minzon & Richard 
# significativit� des coefficients
#les deux modeles sont rejetees
#En cas de rejet dans les deux �tapes, on dispose de preuves statistiques qui indique 
#qu'aucun mod�le ne doit �tre utilis� et qu'on doit r��valuer la forme fonctionnelle.

#test de pr�diction, Approche Davidson & MacKinnon pour comparer les deux mod�les non embo�t�s.
coxtest(cps1, cps2, data=CPS1985)
#modele 1 alpha est sgnificatif alors le modele en niveau est invalid� par rapport au modele en ln
#modele1-> on ne peut conclure
#modele2, alpha n'est pas significatif
#modele2: le mod�le en ln est valid� par rapport au mod�le en niveau

jtest(cps1,cps2, data=CPS1985)
#Modele 1 est significatif, alpha<5%

#regression automatis�e#
########################
#glmulti#
install.packages("glmulti")
library(glmulti)
attach(CPS1985)  
CPS1985$lwage <- log(wage)
CPS1985$exp2 <- experience^2
cps_multi<-glmulti("lwage",c("experience","exp2","education","age","region","gender","married","sector"),exclude=c("experience:exp2"),data=CPS1985,fitfunction = "lm")
#exclude <- permet d'ajouter le traitement des carr�s
#Apr�s 200050 modeles, les 3 "Best models sont:
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
# on observe que married n'est pas presente dans les regressions par contre on a bien la pr�sence de region, sector et gender
# on pourrait d�duire de ces modeles, qu'etre mari� n'a pas d'incidence sur le salaire
