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
#Lorsque deux variables explicatives tr�s fortement corr�l�es sont incluses ensemble dans le mod�le de r�gression lin�aire multiple
#cela peut le rendre instable
#lorsqu'entre deux variables, une forte corr�lation est mise en evidence, l'une des deux variables doit etre incluse dans le modele de regression 
#dans notre etude on va se baser sur le seuil 0.85 du coefficient de correlation de Pearson.
#si le coefficient de correlation est superieure � 0.85 on va considerer que la correlation est forte
library(GGally)

ggpairs(Boston.House.Prices)
#ici nous pouvons voir une forte correlation entre la variable TAX et RAD. le coeficient de pearson depasse 0.90
#toutes les deux variables me semblent pertinentes....parcontre du point de vue metier RAD( indicide d'accessibilit� aux autoroute radiales) me semble moins importante que le TAX(taxe sur la valeur fonciere)
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
#Essayons de voir s'il y'a de la multicolinearit� avec les VIF
#Lorsque qu'une variable � un VIF > 10, il est n�cessaire de la retirer du mod�le,  puis de recalculer les VIFs, et de retirer une seconde variable si n�cessaire, etc. jusqu'� n'obtenir que des VIFs <5.
#le seuil de 10 ne fait pas forcement consensus... il y'a enormement de literature dans lesquelles on utilise le seuil de 5
#je vais utiliser le seuil de 5
library(performance)
check_collinearity(price_model)
#il n'y a pas de multicolinearit� les VIF sont inferieurs � 5

#Evaluation des hypotheses de normalit� et d'homoscedasticit�
#check_model() : qui r�alise un diagnostic de r�gression � l'aide de 6 graphiques, et qui permet d'�valuer les hypoth�ses de lin�arit�, d'homosc�dasticit� et de normalit� des r�sidus, ainsi que les multi-collin�arit� et les valeurs influentes.
check_model(price_model)
# la linearit� de notre modele souffre d'un leger defaut.... cela peut etre du � la presence de l'heteroscedasticit�
#normalement la linearit� devrait etre horizonthal mais elle est un peu curv�.
# avec le graphe de la variance des residus...on voit qu'il y'a heteroscedasticit� car la courbe de la variance des erreurs n'est pas constante.
#on peut le verifier avec le test de Breush-pagan
#pour ce qui de la normalit� des residus elle ne peut etre accept�e ici, elle presente legerement un defaut.
check_normality(price_model)
check_heteroscedasticity(price_model)
library(car)
ncvTest(price_model)
#avec un P-value < 5% on rejette l'hypothese H0 de l'homoscedasticit�
spreadLevelPlot(price_model,smooth = FALSE)
#voila avec la fonction spreadLevelPlot de la library car on peut voir l'allure de
#la variance des residus. elle manque d'etre horizontale ce qui conforte notre soup�on d'heteroscedasticit�

#il faut corriger l'heteroscedasticit�
# en utilisant "le Suggested power transformation":  0.7507873 
# il faut elever la variable a expliquer au puissance 0.7 ou 0.8
price_model1 <- lm(MEDV^0.7~.,data = df)
summary(price_model1)
check_heteroscedasticity(price_model1)
#on voit une augmentation consequante de notre R2 
#verifions ce qu'il en ai de l'heteroscedasticit�
spreadLevelPlot(price_model1,smooth = FALSE)
#il est parfaitement horizontal.
#regardons si la linearit� du modele1 est affect�
check_model(price_model1)
ncvTest(price_model1)
check_collinearity(price_model1)
#on va retir� la variable tax en plus de presenter un vif >10 il n'est pas significatif avec un p-value de 0.7
price_model2 <- update(price_model1,.~.-TAX)
check_collinearity(price_model2)
#on retrouve toute nos variable avec un Vif tres inferieur � 5
summary(price_model2)
check_model(price_model2)

check_heteroscedasticity(price_model2)
check_normality(price_model2)
#je pense que je dois normaliser les donn�es.

#selection des variables avec stepAIC
#elle consiste a trouver la combinaison qui minimise le critere AIC
library(MASS)
stepAIC(price_model2, direction = "backward",trace = T)
# avec un stepAIC on obtient un AIC de 224....... notre modele ne retient que 11 variables
#il a supprm� les variables indus et age. ce sont les memes variables qui n'etaient pas significatifs avec le test de student.
#on essaye de restimer notre modele sans ces deux variables l� pour voir.
price_model3 <-lm(formula = MEDV^0.7 ~ CRIM + ZN + CHAS + NOX + RM + DIS + PTRATIO + 
     BK + ` LSTAT`, data = df)

check_model(price_model3)
summary(price_model3)


#on fait actuellementla methode du R2
#on construit tous les sous modeles possibles et on retient celui pour lequel probabilit�
#critique du test du R2 est la plus petite
library(FactoMineR)
df <- df %>%
  select(- TAX)
view(df)


RegBest(y=df[,12],x=df[,-12], nbest = 1)
#la methode du R2 nous retourne les meme resultats quasiment que celui du stepAIC(backward)
#c'est un modele qui va etre tr�s bon si on veux predire de nouvelles donn�es.
#on retrouve les memes variables que le stepAic a savoir les memes variables utilis� pour le MODEL3
#donc le meilleur model de notre etude est celui de pricemodel3

price_model3 <-lm(formula = MEDV^0.7 ~ CRIM + ZN + CHAS + NOX + RM + DIS + PTRATIO + 
                    BK + ` LSTAT`, data = df)




