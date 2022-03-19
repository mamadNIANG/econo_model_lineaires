View(Boston.House.Prices)
summary(Boston.House.Prices)
library(funModeling)
df_status(Boston.House.Prices)
# il n y a pas de valeur manquante parcontre il y'a une variable avec plusieurs zero
#on doit voir la description de la variable 
names(Boston.House.Prices)<-c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIO","BK"," LSTAT","MEDV")
View(Boston.House.Prices)
summary(Boston.House.Prices)
df_status(Boston.House.Prices)
#ETUDE DES CORRELATIONS LINEAIRES ENTRE LES VARIABLES
#Lorsque deux variables explicatives très fortement corrélées sont incluses ensemble dans le modèle de régression linéaire multiple
#cela peut le rendre instable
#lorsqu'entre deux variables, une forte corrélation est mise en evidence, l'une des deux variables doit etre incluse dans le modele de regression 
#dans notre etude on va se baser sur le seuil 0.85 du coefficient de correlation de Pearson.
#si le coefficient de correlation est superieure à 0.85 on va considerer que la correlation est forte
library(GGally)

ggpairs(Boston.House.Prices)
#ici nous pouvons voir une forte correlation entre la variable TAX et RAD. le coeficient de pearson depasse 0.90
#toutes les deux variables me semblent pertinentes....parcontre du point de vue metier RAD( indicide d'accessibilité aux autoroute radiales) me semble moins importante que le TAX(taxe sur la valeur fonciere)
#donc je retire RAD
library(tidyr)
df1 <- Boston.House.Prices %>%
  select(- RAD)
view(df)
#essayons de realiser notre modele complet de regression lineaire multiple
mod <- lm(MEDV~., data = Boston.House.Prices)
summary(mod)
check_collinearity(mod)

price_model <- lm(MEDV~ ., data = df)
#il y'a plusieurs attribues qu'on pourrait utiliser pour visualiser notre modele.
summary(price_model)
#modele est globalement significatif. il y'a au moins une variable qui explique le prix mediane du loyer
boston <- scale(Boston.House.Prices, center = TRUE,scale = TRUE)
boston
#Essayons de voir s'il y'a de la multicolinearité avec les VIF
#Lorsque qu'une variable à un VIF > 10, il est nécessaire de la retirer du modèle,  puis de recalculer les VIFs, et de retirer une seconde variable si nécessaire, etc. jusqu'à n'obtenir que des VIFs <5.
#le seuil de 10 ne fait pas forcement consensus... il y'a enormement de literature dans lesquelles on utilise le seuil de 5
#je vais utiliser le seuil de 5
library(performance)
check_collinearity(price_model)
#il n'y a pas de multicolinearité les VIF sont inferieurs à 5

#Evaluation des hypotheses de normalité et d'homoscedasticité
#check_model() : qui réalise un diagnostic de régression à l'aide de 6 graphiques, et qui permet d'évaluer les hypothèses de linéarité, d'homoscédasticité et de normalité des résidus, ainsi que les multi-collinéarité et les valeurs influentes.
check_model(price_model)
# la linearité de notre modele souffre d'un leger defaut.... cela peut etre du à la presence de l'heteroscedasticité
#normalement la linearité devrait etre horizonthal mais elle est un peu curvé.
# avec le graphe de la variance des residus...on voit qu'il y'a heteroscedasticité car la courbe de la variance des erreurs n'est pas constante.
#on peut le verifier avec le test de Breush-pagan
#pour ce qui de la normalité des residus elle ne peut etre acceptée ici, elle presente legerement un defaut.
check_normality(price_model)
check_heteroscedasticity(price_model)
library(car)
ncvTest(price_model)
#avec un P-value < 5% on rejette l'hypothese H0 de l'homoscedasticité
spreadLevelPlot(price_model,smooth = FALSE)
#voila avec la fonction spreadLevelPlot de la library car on peut voir l'allure de
#la variance des residus. elle manque d'etre horizontale ce qui conforte notre soupçon d'heteroscedasticité

#il faut corriger l'heteroscedasticité
# en utilisant "le Suggested power transformation":  0.7507873 
# il faut elever la variable a expliquer au puissance 0.7 ou 0.8
price_model1 <- lm(MEDV^0.7~.,data = df)
summary(price_model1)
check_heteroscedasticity(price_model1)
#on voit une augmentation consequante de notre R2 
#verifions ce qu'il en ai de l'heteroscedasticité
spreadLevelPlot(price_model1,smooth = FALSE)
#il est parfaitement horizontal.
#regardons si la linearité du modele1 est affecté
check_model(price_model1)
ncvTest(price_model1)
check_collinearity(price_model1)
#on va retiré la variable tax en plus de presenter un vif >10 il n'est pas significatif avec un p-value de 0.7
price_model2 <- update(price_model1,.~.-TAX)
check_collinearity(price_model2)
#on retrouve toute nos variable avec un Vif tres inferieur à 5
summary(price_model2)
check_model(price_model2)

check_heteroscedasticity(price_model2)
check_normality(price_model2)
#je pense que je dois normaliser les données.

#selection des variables avec stepAIC
#elle consiste a trouver la combinaison qui minimise le critere AIC
library(MASS)
stepAIC(price_model2, direction = "backward",trace = T)
# avec un stepAIC on obtient un AIC de 224....... notre modele ne retient que 11 variables
#il a supprmé les variables indus et age. ce sont les memes variables qui n'etaient pas significatifs avec le test de student.
#on essaye de restimer notre modele sans ces deux variables là pour voir.
price_model3 <-lm(formula = MEDV^0.7 ~ CRIM + ZN + CHAS + NOX + RM + DIS + PTRATIO + 
     BK + ` LSTAT`, data = df)

check_model(price_model3)
summary(price_model3)


#on fait actuellementla methode du R2
#on construit tous les sous modeles possibles et on retient celui pour lequel probabilité
#critique du test du R2 est la plus petite
library(FactoMineR)
df <- df %>%
  select(- TAX)
view(df)


RegBest(y=df[,12],x=df[,-12], nbest = 1)
#la methode du R2 nous retourne les meme resultats quasiment que celui du stepAIC(backward)
#c'est un modele qui va etre trés bon si on veux predire de nouvelles données.
#on retrouve les memes variables que le stepAic a savoir les memes variables utilisé pour le MODEL3
#donc le meilleur model de notre etude est celui de pricemodel3

price_model3 <-lm(formula = MEDV^0.7 ~ CRIM + ZN + CHAS + NOX + RM + DIS + PTRATIO + 
                    BK + ` LSTAT`, data = df)




