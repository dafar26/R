####################################################################################################
#     FARRE                                     DEVOIR 1                                           #
####################################################################################################
data("Journals", package = "AER")
names(Journals)
 
Journals$citeprice <- Journals$price/Journals$citations
names(Journals)
attach(Journals)

# ETAPE 1: CALCULER BETA
x <- cbind(1, log(Journals$citeprice)) #creation de la variable explicative
y <- log(subs) #creation de la variable � expliquer

# Ensuite, nous trouvons beta en utilisant  solve et crossprod
solve_x <- solve(crossprod(x,x))
solve_y <- crossprod(x, y)
beta <- solve_x %*% solve_y 
beta

# ETAPE 2: Calculer la variance r�siduelle pour notre estimation
res <- y - x %*% beta

# On trouve la variance
variance <- crossprod(res,res)/(nrow(Journals)-ncol(Journals))    
variance

# La variance residuelle
res_var <- as.numeric(variance) * solve_x
res_var

# Pour finir, on trouve la t_value
t_value <- beta/(sqrt(diag(res_var)))
t_value


###################################################################################################
###################################################################################################

# EXERCICE DPLYR #
##################
install.packages("dplyr") #installation du package"dplyr"
install.packages("nycflights13") #installation du package"nycflights13
library(dplyr) #charge le package "dplyr"
library(nycflights13) #charge le package "nycflights13"
data(flights)
view(flights) #permet d'afficher les donn�es dans une table
data(flights) 

flightsJ <- filter(flights, month == 1) # permet de filtrer les vols du mois de janvier
View(flightsJ) #affiche les vols de janvier
flightsJ1 <- filter(flights, month == 1, day == 1) #permet de filtrer uniquement les vols du premier janvier 
View(flightsJ1) #affiche les vols du premier janvier

#ordonner les flights selon l'ann�e, puis le mois, puis le jour
flights_ymd<-arrange(flights, year, month, day)
View(flights_ymd)

flights_arr<-arrange(flights, desc(arr_delay)) #permet d'ordonner les vols par ordre d�croissant de retard � l'arriv�e avec (des(arr_delay))
View(flights_arr)
select(flights, year, month, day) #permet de selectionner dans flights les colonnes (year, month, day)
flights_speed <- mutate( flights, 
                    speed = distance/air_time)
View(flights_speed)
flights_speedt<-select(flights_speed, distance, speed)
View(flights_speedt)
sample_n(flights, 5) #�chantillons de 5 vols quel que soit leur type
sample_frac(flights, 0.6) #permet d'�chantillonne 60% des vols quel que soit leur type

summarise(flights_speed, avg = mean(distance)) # summarise avec la fonction mean permet de calculer,
#la moyenne de la variable distance que j'appelle avg


####################################################################################################
####################################################################################################

#EXERCICE PARADE 2005

install.packages("AER")#installation du package "AER"
library(AER) #charge le package "AER"
data("Parade2005")#permet d'obtenir/charger les  donn�es '
View(Parade2005) #affiche les donnees "Parade2005"
summary(Parade2005) #permet d'avoir une description statistique rapide:
#affiche 5 valeurs : le minimum (Min.), le premier quartile (1st Qu.), 
#la m�diane (Median), la moyenne (Mean), le troisi�me quartile (3rd Qu.)
attach(Parade2005)# permet d'utiliser les donn�es sans avoir le besoin d'appeler le data.frame
str(Parade2005)#affiche un apercu des donn�es, indique le nombre de variables et le nombre d'obersation
pa_ear<-Parade2005$earnings
levels(pa_sta) # affiche les symboles des 50 Etats

#representativit�
YN <- table(celebrity)
prop.table(YN)
mean(earnings)
# le biais de selection vient du fait  que notre �chantillon comporte 11 celebrites sur 130 citoyens US(individus moyens).
# les celebrites gagnent en moyenne un revenu beaucoup plus eleve que les citoyens(individus moyens).
# Ce fait aura tendance � relever le revenu moyen des citoyens vers un niveau plus eleve. cet �chantillon risque
# de deplacer la regression vers le haut. Par ce biais, Le revenu moyen n'est pas representatitive de ce que gagne les individus moyens.
# On manque d'informations sur la profession des individus mais on peut en d�duire que les celebret�s sont surepresent�es du � leur fort revenu.
# les celebrit�s representent 8,46% des individus de cette �chantillon.
# Dans ce cas, on se trouve dans un sur�chantillonage, car la probabit�s est faible de se retrouver dans ce cas,
# le statut des celebrites ont une frequence plus eleves que les individus moyens.
# Si on construit un �chantillon de mani�re al�atoire, le pourcentage de celebrit� aurait �t� normalement tr�s faible.


#Earnings moyen en californie(CA)
CA<-filter(Parade2005, state =="CA")# permet de filtrer toutes les variables qui ont la modalit� "CA"
View(CA)#affiche la liste des citoyens qui se trouvent en californie
summary(CA)
mean(CA$earnings)#affiche directement le revenu moyen en californie, il est de 6241430$
# On observe qu'il y a 4 celebrites sur 10,
#la m�dianne nous indique que 50% des individus qui habitent en californie gagnent un revenu inf�rieur � 125000$
#le revenu moyen en californie est de 6241430$. on observe que l'�cart entre la moyenne et la m�diane est tr�s eleves
# on en d�duit que le haut revenu des celebrites tirent la moyenne vers le haut. 
# on pourrait croire que les habitant de CA gagnent plus que les autres etats. CA est surepresente de celebrites
# la suepresentation des celebrit�s  peut venir qu'en moyenne la californie compte plus de celebrit�s que les autres etats
# si c'etait le cas, �a pourrait expliquer cette representativit� alors il y aurait aucun biais de selection, meme avec  un sur�chantillon.

#nombre d'observation dans l'�tat d'Idaho(ID)
library(descr)
pa_sta<-Parade2005$state
x<-table(pa_sta)
x #permet de visualiser le nombre d'�chantillons par Etat, 
#on observe qu'il a 5 individu ce qui fait le deuxi�me etat avec le plus �chantillons
ID<-filter(Parade2005,pa_sta=="ID")#on cherche Idaho qui est represent� par le symbole ID, on a :
str(ID) #le nombre d'�chantillons de l'Idaho est de 5 
summary(ID) 
View(ID) 
# On observe qu'il n'y a pas de celebrites 
# la moyenne est de 50900$, elle est tr�s inf�rieure � celle de la californie
# On observe que l'�cart entre le moyenne et la m�diane est tr�s faible. 
# Si on prenait cet �chantillon comme reference, on aurait peu de biais de selection 

#Moyenne et m�diane des c�l�brit�s
pa_cel<-Parade2005$celebrity
mean_cel<-tapply(pa_ear, pa_cel, mean)
mean_cel #le salaire moyen des c�l�brit�s est de 17107272$
median_cel<-tapply(pa_ear, pa_cel, median)
median_cel #le salaire m�dian des c�l�brit�s est de 19000000$
# en comparant le revenu moyen de l'echantillon qui est de 150341, on observe que le revenu moyen des celebrites est plus eleve que le revenu moyen de l'�chantillon.
# cette moyenne nous indique que les celebrit�s ont un revenu tr�s sup�rieur aux non-celebrit�s.
# les individus non celebre ont un revenu moyen d'environ 61037$ alors que les individus celebres ont un revenu moyen de 17107272.73
#median
#l'�cart est plus important entre le revenu median des celebrites et des non celebrites
# 50% des celebrit�s ont un revenu sup�rieur � 19000000$ alors que 50% des personnes de l'�chantillon ont un revenu inf�rieur � 49500, celebrit�s incluses
#cela montre que l'�cart entre les deux est tr�s important



#Fr�quence
tab <- table(Parade2005$earnings) #tableau d'effectif pour la variable earnings
tab#affiche le tableau d'effectif
prop.table(tab)#fr�quence relative

install.packages(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
install.packages("colorspace")
library(colorspace)
#densit�
log_ear<-log(earnings)# on utilise log � cause de l'asymetrie que provoque notre variable earnings, elle tire  la courbe la courbe vers la gauche
#le revenu est mieux estim� sur une �chelle multiplicative plut�t qu'additive.
density_ear <- ggplot(data=Parade2005, aes(x=log_ear, group=celebrity, fill=celebrity)) + geom_density(adjust=0.5)
density_ear#affiche la courbe avec les densit�s des celebrit�s et des non celebrit�s.

#boxplot
boxplot(pa_ear~pa_cel, xlab = "statuts", ylab = "revenus par an")
#interpretation impossible, le boxplot des non-celebrites("no") est illisible 
boxplot(log(pa_ear)~pa_cel)
#le log permet une lecture visible des deux boxplot, il corrige le probleme de l'asymetrie
# permet de voir rapidement un visuel des donn�es, on obseve bien que les celebrit�s ont tr�s haut revenus par rapport aux non celebrites

#ggplot
a<-ggplot(Parade2005, aes(x=celebrity, y=earnings)) + geom_boxplot()
boxgg_ear<-a+geom_boxplot(aes(color = celebrity), size=.25) + xlab("statut c�l�brit�") +
  ylab("revenus") + labs(fill = "c�l�brit�")
##aes() permet de d�finir les aspects esth�tiques, comme x ou y ou encore la couleur color 
boxgg_ear#illisinle, le boxplot des non celebrites est �cras�, la boxplot des celebrit�s est tir�,
#vers le haut, environ au moins 50% des celebrites gagnent 1800000$.

b<-ggplot(Parade2005, aes(x=celebrity, y=log(earnings)), fill=sup) 
boxgg_logear<-b+geom_boxplot(aes(color = celebrity), size=.25) + xlab("statut c�l�brit�") + 
  ylab("log(revenu)") + labs(fill = "c�l�brit�")
boxgg_logear
# on obtient des valeurs aberrantes
# on observe qu'il y a bien une difference entre le revenu des celebrites et des non celebrit�
# m�diane : les celebrites se partagent 17% du revenu total(revenu des celebrites+non celebrites)