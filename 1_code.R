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
y <- log(subs) #creation de la variable à expliquer

# Ensuite, nous trouvons beta en utilisant  solve et crossprod
solve_x <- solve(crossprod(x,x))
solve_y <- crossprod(x, y)
beta <- solve_x %*% solve_y 
beta

# ETAPE 2: Calculer la variance résiduelle pour notre estimation
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
view(flights) #permet d'afficher les données dans une table
data(flights) 

flightsJ <- filter(flights, month == 1) # permet de filtrer les vols du mois de janvier
View(flightsJ) #affiche les vols de janvier
flightsJ1 <- filter(flights, month == 1, day == 1) #permet de filtrer uniquement les vols du premier janvier 
View(flightsJ1) #affiche les vols du premier janvier

#ordonner les flights selon l'année, puis le mois, puis le jour
flights_ymd<-arrange(flights, year, month, day)
View(flights_ymd)

flights_arr<-arrange(flights, desc(arr_delay)) #permet d'ordonner les vols par ordre décroissant de retard à l'arrivée avec (des(arr_delay))
View(flights_arr)
select(flights, year, month, day) #permet de selectionner dans flights les colonnes (year, month, day)
flights_speed <- mutate( flights, 
                    speed = distance/air_time)
View(flights_speed)
flights_speedt<-select(flights_speed, distance, speed)
View(flights_speedt)
sample_n(flights, 5) #échantillons de 5 vols quel que soit leur type
sample_frac(flights, 0.6) #permet d'échantillonne 60% des vols quel que soit leur type

summarise(flights_speed, avg = mean(distance)) # summarise avec la fonction mean permet de calculer,
#la moyenne de la variable distance que j'appelle avg


####################################################################################################
####################################################################################################

#EXERCICE PARADE 2005

install.packages("AER")#installation du package "AER"
library(AER) #charge le package "AER"
data("Parade2005")#permet d'obtenir/charger les  données '
View(Parade2005) #affiche les donnees "Parade2005"
summary(Parade2005) #permet d'avoir une description statistique rapide:
#affiche 5 valeurs : le minimum (Min.), le premier quartile (1st Qu.), 
#la médiane (Median), la moyenne (Mean), le troisième quartile (3rd Qu.)
attach(Parade2005)# permet d'utiliser les données sans avoir le besoin d'appeler le data.frame
str(Parade2005)#affiche un apercu des données, indique le nombre de variables et le nombre d'obersation
pa_ear<-Parade2005$earnings
levels(pa_sta) # affiche les symboles des 50 Etats

#representativité
YN <- table(celebrity)
prop.table(YN)
mean(earnings)
# le biais de selection vient du fait  que notre échantillon comporte 11 celebrites sur 130 citoyens US(individus moyens).
# les celebrites gagnent en moyenne un revenu beaucoup plus eleve que les citoyens(individus moyens).
# Ce fait aura tendance à relever le revenu moyen des citoyens vers un niveau plus eleve. cet échantillon risque
# de deplacer la regression vers le haut. Par ce biais, Le revenu moyen n'est pas representatitive de ce que gagne les individus moyens.
# On manque d'informations sur la profession des individus mais on peut en déduire que les celebretés sont surepresentées du à leur fort revenu.
# les celebrités representent 8,46% des individus de cette échantillon.
# Dans ce cas, on se trouve dans un suréchantillonage, car la probabités est faible de se retrouver dans ce cas,
# le statut des celebrites ont une frequence plus eleves que les individus moyens.
# Si on construit un échantillon de manière aléatoire, le pourcentage de celebrité aurait été normalement très faible.


#Earnings moyen en californie(CA)
CA<-filter(Parade2005, state =="CA")# permet de filtrer toutes les variables qui ont la modalité "CA"
View(CA)#affiche la liste des citoyens qui se trouvent en californie
summary(CA)
mean(CA$earnings)#affiche directement le revenu moyen en californie, il est de 6241430$
# On observe qu'il y a 4 celebrites sur 10,
#la médianne nous indique que 50% des individus qui habitent en californie gagnent un revenu inférieur à 125000$
#le revenu moyen en californie est de 6241430$. on observe que l'écart entre la moyenne et la médiane est très eleves
# on en déduit que le haut revenu des celebrites tirent la moyenne vers le haut. 
# on pourrait croire que les habitant de CA gagnent plus que les autres etats. CA est surepresente de celebrites
# la suepresentation des celebrités  peut venir qu'en moyenne la californie compte plus de celebrités que les autres etats
# si c'etait le cas, ça pourrait expliquer cette representativité alors il y aurait aucun biais de selection, meme avec  un suréchantillon.

#nombre d'observation dans l'état d'Idaho(ID)
library(descr)
pa_sta<-Parade2005$state
x<-table(pa_sta)
x #permet de visualiser le nombre d'échantillons par Etat, 
#on observe qu'il a 5 individu ce qui fait le deuxième etat avec le plus échantillons
ID<-filter(Parade2005,pa_sta=="ID")#on cherche Idaho qui est representé par le symbole ID, on a :
str(ID) #le nombre d'échantillons de l'Idaho est de 5 
summary(ID) 
View(ID) 
# On observe qu'il n'y a pas de celebrites 
# la moyenne est de 50900$, elle est très inférieure à celle de la californie
# On observe que l'écart entre le moyenne et la médiane est très faible. 
# Si on prenait cet échantillon comme reference, on aurait peu de biais de selection 

#Moyenne et médiane des célébrités
pa_cel<-Parade2005$celebrity
mean_cel<-tapply(pa_ear, pa_cel, mean)
mean_cel #le salaire moyen des célébrités est de 17107272$
median_cel<-tapply(pa_ear, pa_cel, median)
median_cel #le salaire médian des célébrités est de 19000000$
# en comparant le revenu moyen de l'echantillon qui est de 150341, on observe que le revenu moyen des celebrites est plus eleve que le revenu moyen de l'échantillon.
# cette moyenne nous indique que les celebrités ont un revenu très supérieur aux non-celebrités.
# les individus non celebre ont un revenu moyen d'environ 61037$ alors que les individus celebres ont un revenu moyen de 17107272.73
#median
#l'écart est plus important entre le revenu median des celebrites et des non celebrites
# 50% des celebrités ont un revenu supérieur à 19000000$ alors que 50% des personnes de l'échantillon ont un revenu inférieur à 49500, celebrités incluses
#cela montre que l'écart entre les deux est très important



#Fréquence
tab <- table(Parade2005$earnings) #tableau d'effectif pour la variable earnings
tab#affiche le tableau d'effectif
prop.table(tab)#fréquence relative

install.packages(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
install.packages("colorspace")
library(colorspace)
#densité
log_ear<-log(earnings)# on utilise log à cause de l'asymetrie que provoque notre variable earnings, elle tire  la courbe la courbe vers la gauche
#le revenu est mieux estimé sur une échelle multiplicative plutôt qu'additive.
density_ear <- ggplot(data=Parade2005, aes(x=log_ear, group=celebrity, fill=celebrity)) + geom_density(adjust=0.5)
density_ear#affiche la courbe avec les densités des celebrités et des non celebrités.

#boxplot
boxplot(pa_ear~pa_cel, xlab = "statuts", ylab = "revenus par an")
#interpretation impossible, le boxplot des non-celebrites("no") est illisible 
boxplot(log(pa_ear)~pa_cel)
#le log permet une lecture visible des deux boxplot, il corrige le probleme de l'asymetrie
# permet de voir rapidement un visuel des données, on obseve bien que les celebrités ont très haut revenus par rapport aux non celebrites

#ggplot
a<-ggplot(Parade2005, aes(x=celebrity, y=earnings)) + geom_boxplot()
boxgg_ear<-a+geom_boxplot(aes(color = celebrity), size=.25) + xlab("statut célébrité") +
  ylab("revenus") + labs(fill = "célébrité")
##aes() permet de définir les aspects esthétiques, comme x ou y ou encore la couleur color 
boxgg_ear#illisinle, le boxplot des non celebrites est écrasé, la boxplot des celebrités est tiré,
#vers le haut, environ au moins 50% des celebrites gagnent 1800000$.

b<-ggplot(Parade2005, aes(x=celebrity, y=log(earnings)), fill=sup) 
boxgg_logear<-b+geom_boxplot(aes(color = celebrity), size=.25) + xlab("statut célébrité") + 
  ylab("log(revenu)") + labs(fill = "célébrité")
boxgg_logear
# on obtient des valeurs aberrantes
# on observe qu'il y a bien une difference entre le revenu des celebrites et des non celebrité
# médiane : les celebrites se partagent 17% du revenu total(revenu des celebrites+non celebrites)