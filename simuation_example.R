######################################################################################################
####    FARRE                                                                           ####
######################################################################################################
 
#Etude d'une sumulation
install.packages("AER")
library(AER) 

# Etape 1 dgp(generer des données)
dgp <- function(nobs = 30, model = c("trend", "dynamic"), corr = 0, coef = c(0.25, -0.75), sd = 1) 
{ 
  model <- match.arg(model) 
  err <- as.vector(stats::filter(rnorm(nobs, sd = sd), corr, method = "recursive")) #filter cree une serie temporelle
  if(model == "trend") { 
    x <- 1:nobs #
    y <- coef[1] + coef[2] * x + err 
  } else { 
    y <- rep(NA, nobs) 
    y[1] <- coef[1] + err[1] 
    for(i in 2:nobs) 
      y[i] <- coef[1] + coef[2] * y[i-1] + err[i] 
    x <- c(0, y[1:(nobs-1)]) 
  } 
  return(data.frame(y = y, x = x)) 
}
#la fonction dgp permet de generer des données, on choisit aléatoirement le nombres d'observations,
#Pour cette simulation, on choisit de prendre 30 observations( nobs = 30).
#on prend deux modeles trend et dynamic
#on définit les valeurs de y et x pour le modele trend, meme chose pour le modele dynamic


# Etape 2 l'évaluation des quantités d'intérêt
simpower <- function(nrep = 100, size = 0.05, ...) #la proportion de pvaleurs  significatives (par défaut size = 0,05).
{ 
  pval <- matrix(rep(NA, 2 * nrep), ncol = 2) #creation d'une matrice avec des pvaleurs de taille nrep × 2 car elle comporte 2 tests
  colnames(pval) <- c("bptest", "gqtest") #noms des colonnes, "bptest" et "gqtest"
  for(i in 1:nrep) { 
    dat <- dgp(...) #sert à appeler dgp, il va lui renvoyer un jeu de donnnées data.frame dans dat
    pval[i,1] <- bptest(y ~ x, data = dat)$p.value 
    pval[i,2] <- gqtest(y ~ x, data = dat)$p.value #extraire et la stocke dans la 2eme colonne de ma matrice
  } 
  return(colMeans(pval < size))#size=5% 
#cette commande sert à prendre la moyenne par col du nombre de fois où vpval < size
  # si pval<size,il va retourner 1 si pval < 0.05 sinon zéro
}

#sert à calculer la puissance du test pour une combinaison de parametres
#une combinaison represente un scénario
# dans la fonction simpower, on va iterer dans cette boucle 100 iterations dans lequel,
#on va génèrer un jeu de données en utilisant dgp( )
#dans ce jeu de données, on applique les tests de breusch pagan(bptest) et le test de Goldfeld et Quandt(gqtest)
#à l'intérieur du loop for, on crée p-valeurs du breusch pagan, on met les valeurs dans la 1ere colonne de la matrice pval et 
##pour le deuxième test, on crée p-valeurs du Goldfed et on met les valeurs cette fois-ci dans la 2eme colonne de la matrice pval


# Etape 3 fonction de Simulation
simulation <- function(corr = c(0, 0.2, 0.4, 0.6, 0.8,  0.9, 0.95, 0.99), nobs = c(30, 60, 80 ), model = c("trend", "dynamic"), ...) 
{ 
  prs <- expand.grid(corr = corr, nobs = nobs, model = model) 
  nprs <- nrow(prs) 
  pow <- matrix(rep(NA, 2 * nprs), ncol = 2) 
  for(i in 1:nprs) pow[i,] <- simpower(corr = prs[i,1], 
                                       nobs = prs[i,2], model = as.character(prs[i,3]), ...) 
  rval <- rbind(prs, prs) 
  rval$test <- factor(rep(1:2, c(nprs, nprs)), 
                      labels = c("bptest", "gqtest")) 
  rval$power <- c(pow[,1], pow[,2]) 
  rval$nobs <- factor(rval$nobs) 
  return(rval) 
} 

#la fonction simulation configure les combinaisons de parametre dans un dataframe en utilisant expand.grid.
#il simule les 2 valeurs de puissance pour chacune des combinaisons de parametre dans la boucle for.
#dans la boucle for la simulation va demander à simpower les differentes combinaisons de scénarios,
#les résultats sont réorganisé et renvoyé dans un dataframe.
#la simulation calcule la puissance des 2 tests pour chaque scénario( trend et dynamic)

# Digression pour visualiser expand.grid
expand.grid(corr = c(0, 0.2, 0.4, 0.6, 0.8,  0.9, 0.95, 0.99), nobs = c(30, 60, 80), model = c("trend", "dynamic"))
#fait une table avec tous les parametres indiqués 

# Etape 4 Simulation actuel et résumé
set.seed(123) 
psim <- simulation() # appelle reellement "simulation"
tab <- xtabs(power ~ corr + test + model + nobs, data = psim) 
tab
ftable(tab, row.vars = c("model", "nobs", "test"), col.vars = "corr")
library("lattice") 
xyplot(power ~ corr | model + nobs, groups = ~ test, data = psim, type = "b")

#set.seed permet que les résultats de la simulation puissent toujours être reproduits exactement de la meme façon
#on appelle la simulation avec la commande psim <-simulation()
#xtabs permet de transformer le data.frame en un tableau qui classe les résultats de puissance par les quatre variables de conception.
#ftable permet de creer un tableau plat à deux voies
#Dans le tableau les valeurs de % sont placées dans les colonnes et les autres variables sont mis dans les lignes
#le tableau vise à comparer les courbes de puissance entre les deux tests
#interpretation graphique permet de se rendre compte des differences des puissances des tests, elle devient plus apparente avec les graphiques
#xyplot permet d'afficher les graphiques, les observations sont regroupés par test
#les données sont issues des résultats de la simulation psim
#l'heteroscédasticité est plus facile à détecter dans la tendance que dans le modèle dynamique

#interpretation du modele trend
#on peut constater aqe le test de breusch pagan (bleu) est plus performant que le test Goldfeld et Quandt(rouge) dans le modele de trend
#le modele de trend, pour un échantillon de 80 observations, on remarque qu'avec un taux de correlation de 0, le test de breusch pagan(bptest) prédit 
#7% d'héteroscédasticité alors que le test de Goldfeld et Quandt prédit 5%. Avec un taux de correlation de 99, le test de breusch pagan(bptest) prédit
#45% d'héteroscédasticité alors que le test de Goldfeld et Quandt prédit que 26%. on conclue que le test de breusch pagan est plus puissant.

#interpretation du modele dynamic
#on peut constater que les deux tests évoluent assez peu(peu puissants), il y a pas une grosse difference
#on peut quand meme observer que le test Golfeld et Quandt est legerement plus puissant que le test breusch pagan
#le modele dynamic, pour un échantillon de 80 observations, on remarque qu'avec un taux de correlation de 0, le test de Goldfeld et Quandt(gqtest) prédit 
#6% d'héteroscédasticité alors que le test de breusch pagan(bptest) prédit 4%. Avec un taux de correlation de 99, le test Goldfeld et Quandt(gqtest) prédit
#15% d'héteroscédasticité alors que le test de breusch pagan(bptest) prédit que 2%. En general, on conclue que le test de Goldfeld et Quandt est un peu plus puissant.
#


