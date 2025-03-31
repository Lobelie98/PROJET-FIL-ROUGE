
setwd("C:/Users/Lobélie/OneDrive/Bureau/Cours INSA/Projet Fil Rouge/PROJET-FIL-ROUGE/")

##############CHARGEMENT DES LIBRAIRIES##############
library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(MASS)
library(e1071)
library(glmnet)
library(randomForest)
library(mgcv)
library(MLmetrics)

##############CHARGEMENT DES DONNEES##############

#Le dataset traintest contient l'ensemble des données du train et du test
#On charge dans un premier temps toutes les données en précisant de ne pas considérer les variables qualitatives comme des facteurs
#Le but est de pouvoir faire toutes les opérations de regroupement aisément quand cela sera nécessaire
#Une fois qu'on aura terminé de traiter les variables qualitatives, on les retransformera en facteurs

traintest <- read.csv("./sources/traintest.csv", header = TRUE, sep = ",")

#Nombre de valeurs manquantes par variable

columns = colnames(traintest)
na_occurrences = rep(NA, length(columns))

for(i in 1:length(columns)){
  na_occurrences[i] = sum(is.na(traintest[columns[i]]))
}

na_occurrences = rbind(columns, na_occurrences)


##############PREPARATION DES DONNEES##############

#On crée une nouveau dataset (traintest_transformed) dans lequel on met nos données.
#C'est sur ce dataset qu'on va faire toutes les transformations des données.
#Le but est d'éviter d'écraser le dataset d'origine en cas d'erreur

traintest_transformed = traintest


#########VARIABLES QUALITATIVES#########

###MSSubClass###
traintest_transformed$MSSubClass = traintest$MSSubClass
str(traintest_transformed$MSSubClass)

#La variable MSSubClass est définie en tant que variable quantitative. 
#Cependant, de par sa définition (type de maison), on comprend qu'elle n'a pas de sens si elle est considérée comme quantitative
#On va la transformer en variable qualitative et la recoder en facteur.

traintest_transformed$MSSubClass = as.factor(traintest_transformed$MSSubClass)

#traintest_transformed$MSSubClass<-revalue(traintest_transformed$MSSubClass, c(20 = "1 story 1946+", 30='1 story 1945-', 40='1 story unf attic', 45='1-1/2 story unf', 50='1-1/2 story fin', 60='2 story 1946+', 70='2 story 1945-', 75='2-1/2 story all ages', 80='split/multi level', 85='split foyer', 90='duplex all style/age', 120='1 story PUD 1946+', 150='1-1/2 story PUD all', 160='2 story PUD 1946+', 180='PUD multilevel', 190='2 family conversion'))


###MSZoning###
#On remplace les valeurs manquantes par la plus fréquente
table(traintest_transformed$MSZoning)
traintest_transformed$MSZoning[is.na(traintest_transformed$MSZoning)] <- "RL" #Most frequent value
traintest_transformed$MSZoning <- as.factor(traintest_transformed$MSZoning)
str(traintest_transformed$MSZoning)


###Street###
table(traintest_transformed$Street)

#Il n'y a que 2 modalités (Grvl (12 occurrences) et Pave (2907 occurrences))
#On va recoder en variable binaire (1 pour Pave et 0 pour Grvl)

traintest_transformed$Street <-as.integer(revalue(traintest_transformed$Street, c('Pave'=1, "Grvl"=0)))
str(traintest_transformed$Street)


###Alley###
table(traintest_transformed$Alley)

#Il y a 1369 données manquantes. Dans la documentation des variables, il est spécifié que NA correspond à "Absence de ruelle"
#On va donc remplacer les NA par la modalité "NoAlleyAccess"

traintest_transformed$Alley[is.na(traintest_transformed$Alley)] <- "NoAlleyAccess" #No Alley Access as specified in the description
traintest_transformed$Alley <- as.factor(traintest_transformed$Alley)
str(traintest_transformed$Alley)


###LotShape###

table(traintest_transformed$LotShape)
#On ne fait pas de retraitement pour cette variable. Donc on la recode directement en facteur
traintest_transformed$LotShape = as.factor(traintest_transformed$LotShape)


###LandContour###
table(traintest_transformed$LandContour)
traintest_transformed$LandContour <- as.factor(traintest_transformed$LandContour)

###Utilities###
table(traintest_transformed$Utilities)
traintest_transformed$Utilities[is.na(traintest_transformed$Utilities)] <- "AllPub"
traintest_transformed$Utilities <- as.factor(traintest_transformed$Utilities)

#Pour cette variable, il y a deux valeurs manquantes. 
#Tout de même on remarque que la modalité AllPub est pratiquement la seule représentée. 
#Cette variable n'apporte donc pas d'information 


###LotConfig###
table(traintest_transformed$LotConfig)
traintest_transformed$LotConfig <- as.factor(traintest_transformed$LotConfig)


###LandSlope###
table(traintest_transformed$LandSlope)
traintest_transformed$LandSlope = as.factor(traintest_transformed$LandSlope)


###Neighborhood###
traintest_transformed$Neighborhood = traintest$Neighborhood

summary(na.omit(traintest_transformed$SalePrice))

labels = levels(as.factor(traintest_transformed$Neighborhood))
moyennes = rep(NA, length(labels))

for(i in 1:length(labels)){
  database = traintest_transformed$SalePrice[traintest_transformed$Neighborhood == labels[i]]
  moyennes[i] = mean(na.omit(database))
}

moyennes = rbind(labels, moyennes)

traintest_transformed$Neighborhood[traintest_transformed$Neighborhood %in% 
                                     c("BrDale", "BrkSide", "Edwards", "IDOTRR", "MeadowV", "OldTown")] <- "Poor"
traintest_transformed$Neighborhood[traintest_transformed$Neighborhood %in% 
                                     c("Blueste", "Mitchel", "NAmes", "NPkVill", "Sawyer", "SWISU")] <- "Mod_Poor"
traintest_transformed$Neighborhood[traintest_transformed$Neighborhood %in% 
                                     c("Blmngtn", "ClearCr", "CollgCr", "Crawfor", "Gilbert", "NWAmes", "SawyerW")] <- "Medium"
traintest_transformed$Neighborhood[traintest_transformed$Neighborhood %in% 
                                     c("NoRidge", "NridgHt", "Somerst", "StoneBr", "Timber", "Veenker")] <- "Rich"


table(traintest_transformed$Neighborhood)

traintest_transformed$Neighborhood = as.factor(traintest_transformed$Neighborhood)
str(traintest_transformed$Neighborhood)


###Condition1 & Condition2###
table(traintest_transformed$Condition1)
table(traintest_transformed$Condition2)

traintest_transformed$Condition1[traintest_transformed$Condition1 %in% c("PosA", "PosN", "RRAe", "RRAn", "RRNe", "RRNn")] = "Others"
traintest_transformed$Condition2[traintest_transformed$Condition2 %in% c("PosA", "PosN", "RRAe", "RRAn", "RRNn")] = "Others"

traintest_transformed$Condition1 = as.factor(traintest_transformed$Condition1)
traintest_transformed$Condition2 = as.factor(traintest_transformed$Condition2)

str(traintest_transformed$Condition1)
str(traintest_transformed$Condition2)


###BldgType###

table(traintest_transformed$BldgType)
traintest_transformed$BldgType <- as.factor(traintest_transformed$BldgType)


###HouseStyle###
table(traintest_transformed$HouseStyle)
traintest_transformed$HouseStyle <- as.factor(traintest_transformed$HouseStyle)


###RoofMatl###
table(traintest_transformed$RoofMatl)
traintest_transformed$RoofMatl[
  traintest_transformed$RoofMatl %in% c("ClyTile", "Membran", "Metal", "Roll", "WdShake", "WdShngl")] <- "Others"
traintest_transformed$RoofMatl <- as.factor(traintest_transformed$RoofMatl)


###RoofStyle###
table(traintest_transformed$RoofStyle)
traintest_transformed$RoofStyle = as.factor(traintest_transformed$RoofStyle)
str(traintest_transformed$RoofStyle)


###Exterior1st & Exterior2nd###
traintest_transformed$Exterior1st[is.na(traintest_transformed$Exterior1st)] <- "VinylSd"
traintest_transformed$Exterior2nd[is.na(traintest_transformed$Exterior2nd)] <- "VinylSd"

table(traintest_transformed$Exterior1st)
table(traintest_transformed$Exterior2nd)

traintest_transformed$Exterior1st[
  traintest_transformed$Exterior1st %in% c("AsbShng", "AsphShn", "BrkComm", "BrkFace", "CBlock", "ImStucc", "Stone", "Stucco", "WdShing")] <- "Other"

traintest_transformed$Exterior2nd[
  traintest_transformed$Exterior2nd %in% c("AsbShng", "AsphShn", "Brk Cmn", "BrkFace", "CBlock", "Other", "ImStucc", "Stone", "Stucco", "Wd Shng")] <- "Other"

traintest_transformed$Exterior1st <- as.factor(traintest_transformed$Exterior1st)
traintest_transformed$Exterior2nd <- as.factor(traintest_transformed$Exterior2nd)


###ExterQual###
table(traintest_transformed$ExterQual)

traintest_transformed$ExterQual = as.factor(traintest_transformed$ExterQual)
str(traintest_transformed$ExterQual)


###ExterCond###
table(traintest_transformed$ExterCond)
traintest_transformed$ExterCond = as.factor(traintest_transformed$ExterCond)
str(traintest_transformed$ExterCond)


###Foundation###

table(traintest_transformed$Foundation)
traintest_transformed$Foundation = as.factor(traintest_transformed$Foundation)
str(traintest_transformed$Foundation)


###Variables Basement###

traintest_transformed$BsmtQual[is.na(traintest_transformed$BsmtFinType1)] <- "No_Basement"
traintest_transformed$BsmtCond[is.na(traintest_transformed$BsmtFinType1)] <- "No_Basement"
traintest_transformed$BsmtExposure[is.na(traintest_transformed$BsmtFinType1)] <- "No_Basement"
traintest_transformed$BsmtFinType2[is.na(traintest_transformed$BsmtFinType1)] <- "No_Basement"
traintest_transformed$BsmtFinType1[is.na(traintest_transformed$BsmtFinType1)] <- "No_Basement"

traintest_transformed$BsmtQual[is.na(traintest_transformed$BsmtQual)] <- "TA"
traintest_transformed$BsmtCond[is.na(traintest_transformed$BsmtCond)] <- "TA"
traintest_transformed$BsmtExposure[is.na(traintest_transformed$BsmtExposure)] <- "No"
traintest_transformed$BsmtFinType2[is.na(traintest_transformed$BsmtFinType2)] <- "Unf"

#Qualities <- c('No_Basement' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

traintest_transformed$BsmtQual = as.factor(traintest_transformed$BsmtQua)
traintest_transformed$BsmtCond = as.factor(traintest_transformed$BsmtCond)
traintest_transformed$BsmtExposure = as.factor(traintest_transformed$BsmtExposure)
traintest_transformed$BsmtFinType2 = as.factor(traintest_transformed$BsmtFinType2)
traintest_transformed$BsmtFinType1 = as.factor(traintest_transformed$BsmtFinType1)


###Heating###
table(traintest_transformed$Heating)
traintest_transformed$Heating[traintest_transformed$Heating %in% c("Floor", "Grav", "OthW", "Wall")] <- "Others"
traintest_transformed$Heating <- as.factor(traintest_transformed$Heating)


###HeatingQC###
table(traintest_transformed$HeatingQC)
traintest_transformed$HeatingQC = as.factor(traintest_transformed$HeatingQC)
str(traintest_transformed$HeatingQC)


###CentralAir###
table(traintest_transformed$CentralAir)
traintest_transformed$CentralAir = as.integer(revalue(traintest_transformed$CentralAir, c("N"=0, "Y"=1)))
str(traintest_transformed$CentralAir)


###Electrical###
table(traintest_transformed$Electrical)
traintest_transformed$Electrical[is.na(traintest_transformed$Electrical)] <- "SBrkr"
traintest_transformed$Electrical[traintest_transformed$Electrical %in% c("FuseA", "FuseF", "FuseP", "Mix")] <- "FuseMix"
traintest_transformed$Electrical <- as.integer(revalue(traintest_transformed$Electrical, c("FuseMix"=0, "SBrkr"=1)))


floor = subset(traintest_transformed, select = c(X1stFlrSF, X2ndFlrSF, LowQualFinSF, GrLivArea, SalePrice))
alias(lm(SalePrice~., data = na.omit(floor)))

#GrLivArea est la somme de X1stFlrSF, X2ndFlrSF et LowQualFinSF

vif(lm(SalePrice~., data = na.omit(floor)))
cor(na.omit(floor))

###Bathrooms###
traintest_transformed$BsmtFullBath[is.na(traintest_transformed$BsmtFullBath)] <- 0
traintest_transformed$BsmtHalfBath[is.na(traintest_transformed$BsmtHalfBath)] <- 0


plot(traintest_transformed$BsmtFullBath, traintest_transformed$SalePrice)
plot(traintest_transformed$BsmtHalfBath, traintest_transformed$SalePrice)
plot(traintest_transformed$FullBath, traintest_transformed$SalePrice)
plot(traintest_transformed$HalfBath, traintest_transformed$SalePrice)

cor(na.omit(cbind(traintest_transformed$BsmtFullBath, traintest_transformed$SalePrice)))
cor(na.omit(cbind(traintest_transformed$BsmtHalfBath, traintest_transformed$SalePrice)))
cor(na.omit(cbind(traintest_transformed$FullBath, traintest_transformed$SalePrice)))
cor(na.omit(cbind(traintest_transformed$HalfBath, traintest_transformed$SalePrice)))

###KitchenAbvGr###

plot(traintest_transformed$KitchenAbvGr, traintest_transformed$SalePrice)
table(traintest_transformed$KitchenAbvGr)
fit <- aov(SalePrice ~ KitchenAbvGr , data=traintest_transformed)
summary(fit)
cor(na.omit(cbind(traintest_transformed$KitchenAbvGr, traintest_transformed$SalePrice)))

#Le nombre de cuisines ne semble pas avoir d'influence signigicative sur le prix de la maison
#La plupart des maisons ont 1 cuisine, et quelques une en ont 2.
#Une seule maison ne comporte pas de cuisine et 2 autres en comportent 3. 
#Cependant pour ces 3 cas, on n'observe pas d'influence sur le prix de la maison. 
#Les deux maisons de 3 cuisines ont des prix plutôt bas (100000$ environ), de même que celle qui n'a pas de cuisine
#On va donc rendre cette variable qualitative

traintest_transformed$KitchenAbvGr[traintest_transformed$KitchenAbvGr == 1] <- "One_Kitchen"
traintest_transformed$KitchenAbvGr[traintest_transformed$KitchenAbvGr == 2] <- "Two_Kitchen"
traintest_transformed$KitchenAbvGr[traintest_transformed$KitchenAbvGr == 0 | traintest_transformed$KitchenAbvGr == 3] <- "Other"

traintest_transformed$KitchenAbvGr = as.factor(traintest_transformed$KitchenAbvGr)

str(traintest_transformed$KitchenAbvGr)


###KitchenQual###
traintest_transformed$KitchenQual[is.na(traintest_transformed$KitchenQual)] <- "TA"
traintest_transformed$KitchenQual = as.factor(traintest_transformed$KitchenQual)
str(traintest_transformed$KitchenQual)


###TotRmsAbvGrd###
plot(traintest_transformed$TotRmsAbvGrd, traintest_transformed$SalePrice)
cor(na.omit(cbind(traintest_transformed$TotRmsAbvGrd, traintest_transformed$SalePrice)))

#On peut observer une corrélation entre la variable TotRmsAbvGrd et la variable SalePrice
#Les prix augmentent lorsque le nombre pièces augmente.

a = traintest_transformed %>% filter(traintest_transformed$TotRmsAbvGrd == 14)

#Une maison comporte 14 pièces mais n'a pas un prix très élevé. 
#Cependant quand on examine le quartier dans lequel elle est situé, on voit que ce dernier est classé dans la catégorie Moderatly Poor; ce qui expliquerait le prix.

#On décide de ne pas modifier cette variable


###Functional###
table(traintest_transformed$Functional)
traintest_transformed$Functional[is.na(traintest_transformed$Functional)] <- "Typ"
traintest_transformed$Functional[traintest_transformed$Functional %in% c("Min1", "Min2")] <- "Minor"
traintest_transformed$Functional[traintest_transformed$Functional %in% c("Maj1", "Maj2", "Sev")] <- "Major&Oth"

traintest_transformed$Functional <- as.factor(traintest_transformed$Functional)


###Fireplaces and FireplaceQu###
#Petite vérification pour s'assurer que lorsque FireplaceQu est NA alors Fireplaces vaut 0
little_data = subset(traintest_transformed %>% filter(is.na(traintest_transformed$FireplaceQu)), select = c(Fireplaces, FireplaceQu))

#On voit bien que c'est le cas. 

##Fireplaces
table(traintest_transformed$Fireplaces)
cor(na.omit(cbind(traintest_transformed$Fireplaces, traintest_transformed$SalePrice)))
plot(traintest_transformed$Fireplaces, traintest_transformed$SalePrice)

traintest_transformed$Fireplaces[traintest_transformed$Fireplaces == 0] <- "No_Fireplace"
traintest_transformed$Fireplaces[(traintest_transformed$Fireplaces %in% c(1, 2, 3, 4))] <- "FirePlace"

traintest_transformed$Fireplaces <- as.integer(revalue(traintest_transformed$Fireplaces, c('FirePlace'=1, "No_Fireplace"=0)))

str(traintest_transformed$Fireplaces)

##FireplaceQu

table(traintest_transformed$FireplaceQu)

traintest_transformed$FireplaceQu[is.na(traintest_transformed$FireplaceQu)] <- "No_Fireplace"

#QualitiesFirePlace <- c('No_Fireplace' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
traintest_transformed$FireplaceQu = as.factor(traintest_transformed$FireplaceQu)


###Variables GARAGE###

#On va mettre No Garage partout où il y a des NA
#Dans un premier temps on travaille sur les variables GarageType, GarageFinish, GarageQual, GarageCond

#Qualities1 <- c('No_Garage' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

traintest_transformed$GarageFinish[is.na(traintest_transformed$GarageType)] <- "No_Garage"
traintest_transformed$GarageQual[is.na(traintest_transformed$GarageType)] <- "No_Garage"
traintest_transformed$GarageCond[is.na(traintest_transformed$GarageType)] <- "No_Garage"
traintest_transformed$GarageType[is.na(traintest_transformed$GarageType)] <- "No_Garage"

traintest_transformed$GarageCond[is.na(traintest_transformed$GarageCond)] <- "TA"
traintest_transformed$GarageQual[is.na(traintest_transformed$GarageQual)] <- "TA"
traintest_transformed$GarageFinish[is.na(traintest_transformed$GarageFinish)] <- "Unf"

traintest_transformed$GarageQual = as.factor(traintest_transformed$GarageQual)
traintest_transformed$GarageCond = as.factor(traintest_transformed$GarageCond)

traintest_transformed$GarageType = as.factor(traintest_transformed$GarageType)
traintest_transformed$GarageFinish = as.factor(traintest_transformed$GarageFinish)


###GarageYrBlt###
traintest_transformed$GarageAge = traintest_transformed$GarageYrBlt

traintest_transformed$GarageAge[traintest_transformed$GarageAge == 2207] <- 2007
traintest_transformed$GarageAge = 2011-traintest_transformed$GarageAge
traintest_transformed$GarageAge[is.na(traintest_transformed$GarageAge)] <- 0

hist(traintest_transformed$GarageAge)
summary(traintest_transformed$GarageAge)


###PAVEDDRIVE###
table(traintest_transformed$PavedDrive)
traintest_transformed$PavedDrive <- as.factor(traintest_transformed$PavedDrive)


###PoolQC###

summary(traintest_transformed$PoolArea)
litlle_data = subset(traintest_transformed %>% filter(traintest_transformed$PoolArea != 0), select = c(PoolArea, PoolQC))

#Il n'y a que 13 maisons qui comportent une piscine. Sur ces 13 maisons, 3 n'ont pas la variable PoolQC renseignée
#On va procéder à la complétion des données pour ces 3 cas

traintest_transformed$PoolQC[traintest_transformed$PoolArea == 368 & is.na(traintest_transformed$PoolQC)] = "Ex"
traintest_transformed$PoolQC[traintest_transformed$PoolArea == 444 & is.na(traintest_transformed$PoolQC)] = "Ex"
traintest_transformed$PoolQC[traintest_transformed$PoolArea == 561 & is.na(traintest_transformed$PoolQC)] = "Fa"

traintest_transformed$PoolQC[is.na(traintest_transformed$PoolQC)]='None'
traintest_transformed$PoolQC = as.factor(traintest_transformed$PoolQC)

table(traintest_transformed$PoolQC)


###Fence###
traintest_transformed$Fence[is.na(traintest_transformed$Fence)] <- "No_Fence"
traintest_transformed$Fence <- as.factor(traintest_transformed$Fence)
str(traintest_transformed$Fence)
table(traintest_transformed$Fence)


###MiscFeature###
little_data = subset(traintest_transformed %>% filter(traintest_transformed$MiscVal!=0), 
                     select = c(MiscFeature, MiscVal))

traintest_transformed$MiscFeature[traintest_transformed$MiscVal == 17000] <- "Gar2"

little_data = subset(traintest_transformed %>% filter(!is.na(traintest_transformed$MiscFeature)), 
                     select = c(MiscFeature, MiscVal))

traintest_transformed$MiscFeature[traintest_transformed$MiscVal == 0 & 
                                    !is.na(traintest_transformed$MiscFeature)] <- NA

traintest_transformed$MiscFeature[is.na(traintest_transformed$MiscFeature)] <- "NoMiscFeature"
traintest_transformed$MiscFeature[traintest_transformed$MiscFeature %in% c("Gar2", "Othr", "TenC")] <- "Others"

table(traintest_transformed$MiscFeature)

traintest_transformed$MiscFeature = as.factor(traintest_transformed$MiscFeature)

str(traintest_transformed$MiscFeature)


###MoSold###
traintest_transformed$MoSold = as.factor(traintest_transformed$MoSold)
str(traintest_transformed$MoSold)


###YrSold###
traintest_transformed$YrSold = as.factor(traintest_transformed$YrSold)
str(traintest_transformed$YrSold)


###SaleType###

traintest_transformed$SaleType[is.na(traintest_transformed$SaleType)] <- "WD" 
table(traintest_transformed$SaleType)
traintest_transformed$SaleType <- as.factor(traintest_transformed$SaleType)
str(traintest_transformed$SaleType)


###SaleCondition###

table(traintest_transformed$SaleCondition)
traintest_transformed$SaleCondition <- as.factor(traintest_transformed$SaleCondition)
str(traintest_transformed$SaleCondition)



#########VARIABLES QUANTITATIVES CONTINUES#########

###LotFrontage###
plot(traintest_transformed$LotFrontage, traintest_transformed$SalePrice)

#On génère des nombres aléatoires entre le premier et le troisième quartile de la variable LotFrontage pour compléter les valeurs manquantes

for (val in 1:length(traintest_transformed$LotFrontage)){
  if(is.na(traintest_transformed$LotFrontage[val])){
    traintest_transformed$LotFrontage[val] = sample(59:80, 1)
  }
}

###MasVnrType & MasVnrArea###

traintest_transformed$MasVnrType[is.na(traintest_transformed$MasVnrArea)] = "None"
traintest_transformed$MasVnrType[is.na(traintest_transformed$MasVnrType)] = "BrkFace"

traintest_transformed$MasVnrType <- as.factor(traintest_transformed$MasVnrType)
table(traintest_transformed$MasVnrType)
str(traintest_transformed$MasVnrType)

traintest_transformed$MasVnrArea[is.na(traintest_transformed$MasVnrArea)] <- 0


###Basement surfaces###

traintest_transformed$BsmtFinSF1[is.na(traintest_transformed$BsmtFinSF1)] <- 0
traintest_transformed$BsmtFinSF2[is.na(traintest_transformed$BsmtFinSF2)] <- 0
traintest_transformed$BsmtUnfSF[is.na(traintest_transformed$BsmtUnfSF)] <- 0
traintest_transformed$TotalBsmtSF[is.na(traintest_transformed$TotalBsmtSF)] <- 0
traintest_transformed$TotalSqFeet[is.na(traintest_transformed$TotalSqFeet)] <- 0

###############YearBuilt & YearRemodAdd###############

hist(traintest$YearBuilt)

traintest_transformed$YearRemodAdd[traintest_transformed$YearRemodAdd == traintest_transformed$YearBuilt] = 0
traintest_transformed$YearRemodAdd[traintest_transformed$YearRemodAdd != 0] = 1

traintest_transformed$AgeBuilt = 2011 - traintest_transformed$YearBuilt
str(traintest_transformed$YearRemodAdd)

cor(na.omit(cbind(traintest_transformed$AgeBuilt, traintest_transformed$SalePrice)))

hist(traintest_transformed$AgeBuilt)

plot(traintest_transformed$AgeBuilt, traintest_transformed$SalePrice)
abline(lm(SalePrice ~ AgeBuilt, data = traintest_transformed), col="red", lwd=1.5)

###GarageArea###
traintest_transformed$GarageArea[is.na(traintest_transformed$GarageArea)] <- 400

###GrLivArea###

plot(traintest_transformed$GrLivArea, traintest_transformed$SalePrice, data=traintest_transformed)
abline(lm(SalePrice ~ GrLivArea, data = traintest_transformed), col="red", lwd=1.5)

#On a deux valeurs aberrantes car pour ces maisons-là on a une surface très grande, mais des prix plutôt faibles

plot(traintest$GrLivArea, traintest$SalePrice)
a = subset(traintest %>% filter(traintest$GrLivArea > 4500), select = c("SalePrice", "GrLivArea", "OverallQual"))

#Il faudra retirer la maison de surface 4676 et de prix 184750 et celle de surface 5642 et de prix 160000

traintest_transformed = traintest_transformed %>% filter(
  !(traintest_transformed$GrLivArea == 4676 & traintest_transformed$SalePrice==184750|
      traintest_transformed$GrLivArea == 5642 & traintest_transformed$SalePrice==160000))

#########VARIABLES QUANTITATIVES DISCRETES#########

###GarageCars###
traintest_transformed$GarageCars[is.na(traintest_transformed$GarageCars)] <- 2
cor(na.omit(cbind(traintest_transformed$GarageCars, traintest_transformed$SalePrice)))
table(traintest_transformed$GarageCars)
str(traintest_transformed$GarageCars)

traintest_transformed$GarageCars[traintest_transformed$GarageCars == 0] <- "Capacity_0"
traintest_transformed$GarageCars[traintest_transformed$GarageCars == 1] <- "Capacity_1"
traintest_transformed$GarageCars[traintest_transformed$GarageCars == 2] <- "Capacity_02"
traintest_transformed$GarageCars[traintest_transformed$GarageCars == 3] <- "Capacity_3"
traintest_transformed$GarageCars[traintest_transformed$GarageCars %in% c(4, 5)] <- "Capacity_4+"

traintest_transformed$GarageCars = as.factor(traintest_transformed$GarageCars)

##########NOUVEAUX INDICATEURS#####

#Surface totale de la maison : 
traintest_transformed$TotalSqFeet = traintest_transformed$BsmtFinSF1+traintest_transformed$BsmtFinSF2+traintest_transformed$BsmtUnfSF+traintest_transformed$GrLivArea

#Nombre de salles de bains
traintest_transformed$TotalBathRooms = traintest_transformed$BsmtFullBath+traintest_transformed$BsmtHalfBath+
  traintest_transformed$FullBath+traintest_transformed$HalfBath

with(traintest_transformed,plot(OpenPorchSF, SalePrice))
abline(lm(SalePrice ~ OpenPorchSF, data = traintest_transformed), col="red", lwd=1.5)

with(traintest_transformed,plot(EnclosedPorch, SalePrice))
abline(lm(SalePrice ~ EnclosedPorch, data = traintest_transformed), col="red", lwd=1.5)

with(traintest_transformed,plot(X3SsnPorch, SalePrice))
abline(lm(SalePrice ~ X3SsnPorch, data = traintest_transformed), col="red", lwd=1.5)

with(traintest_transformed,plot(ScreenPorch, SalePrice))
abline(lm(SalePrice ~ ScreenPorch, data = traintest_transformed), col="red", lwd=1.5)

cor(na.omit(cbind(traintest_transformed$OpenPorchSF, 
                  traintest_transformed$EnclosedPorch,
                  traintest_transformed$X3SsnPorch,
                  traintest_transformed$ScreenPorch,
                  traintest_transformed$SalePrice)))



floor = subset(traintest_transformed, select = c(BsmtFinSF1, BsmtFinSF2, BsmtUnfSF, TotalBsmtSF, SalePrice))
alias(lm(SalePrice~., data = na.omit(floor)))



#Type de porche
traintest_transformed$TypePorch <- NA

traintest_transformed$TypePorch[traintest_transformed$OpenPorchSF != 0 
                                & traintest_transformed$EnclosedPorch == 0
                                & traintest_transformed$X3SsnPorch == 0
                                & traintest_transformed$ScreenPorch == 0] <- "OpenPorch"

traintest_transformed$TypePorch[traintest_transformed$EnclosedPorch != 0 
                                & traintest_transformed$OpenPorchSF == 0
                                & traintest_transformed$X3SsnPorch == 0
                                & traintest_transformed$ScreenPorch == 0] <- "EnclosedPorch"

traintest_transformed$TypePorch[traintest_transformed$X3SsnPorch != 0 
                                & traintest_transformed$EnclosedPorch == 0
                                & traintest_transformed$OpenPorchSF == 0
                                & traintest_transformed$ScreenPorch == 0] <- "X3SsnPorch"

traintest_transformed$TypePorch[traintest_transformed$ScreenPorch != 0 
                                & traintest_transformed$EnclosedPorch == 0
                                & traintest_transformed$X3SsnPorch == 0
                                & traintest_transformed$OpenPorchSF == 0] <- "ScreenPorch"


traintest_transformed$TypePorch[traintest_transformed$OpenPorchSF != 0 
                                & traintest_transformed$EnclosedPorch != 0] <- "Mixed"

traintest_transformed$TypePorch[traintest_transformed$OpenPorchSF != 0 
                                & traintest_transformed$X3SsnPorch != 0] <- "Mixed"

traintest_transformed$TypePorch[traintest_transformed$OpenPorchSF != 0 
                                & traintest_transformed$ScreenPorch != 0] <- "Mixed"

traintest_transformed$TypePorch[traintest_transformed$EnclosedPorch != 0 
                                & traintest_transformed$X3SsnPorch != 0] <- "Mixed"

traintest_transformed$TypePorch[traintest_transformed$EnclosedPorch != 0 
                                & traintest_transformed$ScreenPorch != 0] <- "Mixed"

traintest_transformed$TypePorch[traintest_transformed$X3SsnPorch != 0 
                                & traintest_transformed$ScreenPorch != 0] <- "Mixed"

traintest_transformed$TypePorch[traintest_transformed$X3SsnPorch == 0 
                                & traintest_transformed$ScreenPorch == 0
                                & traintest_transformed$EnclosedPorch == 0
                                & traintest_transformed$OpenPorchSF == 0] <- "No_Porch"

traintest_transformed$TypePorch <- as.factor(traintest_transformed$TypePorch)


table(traintest_transformed$TypePorch)


str(traintest_transformed)

##########PREPARATION DU DATASET POUR LA MODELISATION##########
traintest_transformed2 = traintest_transformed

#On retire les variable YearBuilt et GarageYrBlt qui ont servi pour la création des variables AgeBuilt et GarageAge
traintest_transformed2 = subset(traintest_transformed2, select = -c(YearBuilt, GarageYrBlt))

#On retire aussi Utilities, qui a la plus faible variance, ainsi que la colonne Id
traintest_transformed2 = subset(traintest_transformed2, select = -c(Id, Utilities, MiscFeature))

#On retire les valeurs aberrantes observées à partir de l'analyse de GrLivArea


#Transformation log de la variable SalePrice
traintest_transformed2$SalePrice = log(traintest_transformed2$SalePrice+1)
hist(traintest_transformed2$SalePrice)

#Normalisation des variables quantitatives
skewness(traintest_transformed2$WoodDeckSF)
skewness(traintest_transformed2$OpenPorchSF)
skewness(traintest_transformed2$EnclosedPorch)
skewness(traintest_transformed2$X3SsnPorch)
skewness(traintest_transformed2$ScreenPorch)
skewness(traintest_transformed2$PoolArea)
skewness(traintest_transformed2$GarageAge)
skewness(traintest_transformed2$AgeBuilt)
skewness(traintest_transformed2$TotalSqFeet)

traintest_transformed2$WoodDeckSF = log1p(traintest_transformed2$WoodDeckSF)
traintest_transformed2$OpenPorchSF = log1p(traintest_transformed2$OpenPorchSF)
traintest_transformed2$EnclosedPorch = log1p(traintest_transformed2$EnclosedPorch)
traintest_transformed2$X3SsnPorch = log1p(traintest_transformed2$X3SsnPorch)
traintest_transformed2$ScreenPorch = log1p(traintest_transformed2$ScreenPorch)
traintest_transformed2$PoolArea = log1p(traintest_transformed2$PoolArea)

###Normalization of numerical variables###
numericVars = c("LotFrontage", "LotArea", "MasVnrArea", "TotalBsmtSF",
                "GrLivArea", "WoodDeckSF", "OpenPorchSF", "EnclosedPorch",
                "X3SsnPorch", "ScreenPorch",  "AgeBuilt", "GarageAge", "GarageArea")

#"BsmtFullBath", "BsmtHalfBath", "FullBath", "HalfBath",

numerciDiscreteVars = c("OverallQual", "OverallCond",  
                        "BedroomAbvGr", "TotRmsAbvGrd", "TotalBathRooms", "Street", "YearRemodAdd", 
                        "CentralAir", "Electrical", 
                         "SalePrice")

DFnorm = traintest_transformed2[, numericVars]

categoricalVars = which(sapply(traintest_transformed2, is.factor))
categoricalVars

Dfothers = traintest_transformed2[, numerciDiscreteVars]

DFQual = traintest_transformed2[, categoricalVars]
DFdummies <- as.data.frame(model.matrix(~.-1, DFQual))
dim(DFdummies)

fewOnes <- which(colSums(DFdummies[1:nrow(traintest_transformed2[!is.na(traintest_transformed2$SalePrice),]),])<10)
colnames(DFdummies[fewOnes])
DFdummies <- DFdummies[,-fewOnes] #removing predictors
dim(DFdummies)

dim(DFQual)

final_data = cbind(DFdummies, Dfothers, DFnorm)

#On renomme certaines variables

names(final_data)[names(final_data)=='RoofMatlTar&Grv'] <- "RoofMatlTar_Grv"
names(final_data)[names(final_data)=='Exterior1stWd Sdng'] <- "Exterior1stWd_Sdng"
names(final_data)[names(final_data)=='Exterior2ndWd Sdng'] <- "Exterior2ndWd_Sdng"
names(final_data)[names(final_data)=='GarageCarsCapacity_4+'] <- "GarageCarsCapacity_4_plus"

names(final_data)[names(final_data)=='RoofMatlTar&Grv'] <- "RoofMatlTar_Grv"
names(final_data)[names(final_data)=='Exterior1stWd Sdng'] <- "Exterior1stWd_Sdng"
names(final_data)[names(final_data)=='Exterior2ndWd Sdng'] <- "Exterior2ndWd_Sdng"
names(final_data)[names(final_data)=='GarageCarsCapacity_4+'] <- "GarageCarsCapacity_4_plus"

###################################MODELISATION###################################

data = final_data %>% filter(!is.na(final_data$SalePrice))

set.seed(123)
test.ratio = 0.30
npop = nrow(data)
ntest = ceiling(npop*test.ratio)
testi = sample(1:npop, ntest) # indices de l'´echantillon test
appri = setdiff(1:npop, testi) # indices de l'´echant. d'apprentissage
# Construction des ´echantillons avec les variables explicatives
dataApp = data[appri, ] # construction de l'´echantillon d'apprentissage
dataTest = data[testi, ] # construction de l'´echantillon test

dataAppLog = dataApp
dataTestLog = dataTest

NAcol <- which(colSums(is.na(data)) > 0)
NAcol


#############NORMALISATION#############

columnames = colnames(DFnorm)

"""
for(label in columnames){
  
  moy = mean(unlist(dataAppLog[label]))
  sdv = sd(unlist(dataAppLog[label]))
  
  dataAppLog[label] = (unlist(dataAppLog[label]) - moy)/sdv
  dataTestLog[label] = (unlist(dataTestLog[label]) - moy)/sdv
  
}
"""

#write.csv(dataAppLog, 'D:/App.csv', row.names = FALSE)
#write.csv(dataTestLog, 'D:/Test.csv', row.names = FALSE)

options(max.print=1000000)

####MODELE LINEAIRE####

reslm_log <- lm(SalePrice~., data = dataAppLog)
summary(reslm_log)

predictions <- predict(reslm_log, newdata = dataAppLog)
rmsle_lm_log = RMSE(dataAppLog$SalePrice, predictions)
rmse_ = RMSE(exp(dataAppLog$SalePrice), exp(predictions))
rmsle_lm_log
rmse_
VarE = 1 - var(exp(predictions) - exp(dataAppLog)$SalePrice) / var(exp(dataAppLog)$SalePrice)
VarE

plot(exp(dataAppLog$SalePrice), exp(predictions), main = "Nuage de points observés-prédits (App)",
     xlab = "Observés", ylab = "Prédits")
abline(lm(exp(predictions) ~ exp(dataAppLog$SalePrice), data = dataAppLog), col="red", lwd=1.5)

predictions <- predict(reslm_log, newdata = dataTestLog)
rmsle_lm_log = RMSE(dataTestLog$SalePrice, predictions)
rmse_ = RMSE(exp(dataTestLog)$SalePrice, exp(predictions))
rmsle_lm_log
rmse_
VarE = 1 - var(exp(predictions) - exp(dataTestLog)$SalePrice) / var(exp(dataTestLog)$SalePrice)
VarE

plot(exp(dataTestLog$SalePrice), exp(predictions), main = "Nuage de points observéss-prédits (Val)",
     xlab = "Observés", ylab = "Prédits")
abline(lm(exp(predictions) ~ exp(dataTestLog$SalePrice), data = dataAppLog), col="red", lwd=1.5)


shapiro.test(reslm_log$residuals)


library("ggpubr")
ggdensity(reslm_log$residuals, fill = "lightgray", xlab = "Résidus", ylab = "Densité")
plot(reslm_log$residuals, type = "p", xlab = "SalePrice", ylab = "Erreur")
abline(h=0, col="red")

##LASSO

X = subset(dataAppLog, select = -c(SalePrice))
Y = dataAppLog$SalePrice

lambda_seq <- 10^seq(-4, 4, by = .1)

cv_output <- cv.glmnet(as.matrix(X), as.matrix(Y), alpha = 1, lambda = lambda_seq, nfolds = 10)

# identifying best lamda
best_lam <- cv_output$lambda.min
best_lam

lasso_best <- glmnet(as.matrix(X), as.matrix(Y), alpha = 1, lambda = best_lam)

predictions <- predict(lasso_best, s = best_lam, newx = as.matrix(X))

rmsle_lm_log = RMSE(dataAppLog$SalePrice, predictions)
rmse_ = RMSE(exp(dataAppLog)$SalePrice, exp(predictions))
rmsle_lm_log
rmse_
VarE = 1 - var(exp(predictions) - exp(dataAppLog$SalePrice)) / var(exp(dataAppLog$SalePrice))
VarE

plot(exp(dataAppLog$SalePrice), exp(predictions), main = "Nuage de points observés-prédits (App)",
     xlab = "Observés", ylab = "Prédits")
abline(lm(exp(predictions) ~ exp(dataAppLog$SalePrice), data = dataAppLog), col="red", lwd=1.5)

Xt = subset(dataTestLog, select = -c(SalePrice))
predictions = predict(lasso_best, s = best_lam, newx = as.matrix(Xt))

rmsle_lm_log = RMSE(dataTestLog$SalePrice, predictions)
rmse_ = RMSE(exp(dataTestLog)$SalePrice, exp(predictions))
rmsle_lm_log
rmse_
VarE = 1 - var(exp(predictions) - exp(dataTestLog$SalePrice)) / var(exp(dataTestLog$SalePrice))
VarE

plot(exp(dataTestLog$SalePrice), exp(predictions), main = "Nuage de points observés-prédits (Val)",
     xlab = "Observés", ylab = "Prédits")
abline(lm(exp(predictions) ~ exp(dataTestLog$SalePrice), data = dataAppLog), col="red", lwd=1.5)


###########RIDGE###########

X = subset(dataAppLog, select = -c(SalePrice))
Y = dataAppLog$SalePrice
Xt = subset(dataTestLog, select = -c(SalePrice))
Yt = dataTestLog$SalePrice

lambda <- 10^seq(-4, 4, length = 100)

ridge <- train(
  SalePrice ~., data = dataAppLog, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 0, lambda = lambda)
)

summary(ridge)

predictions <- ridge %>% predict(dataAppLog)

rmsle_lm_log = RMSE(dataAppLog$SalePrice, predictions)
rmse_ = RMSE(exp(dataAppLog)$SalePrice, exp(predictions))
rmsle_lm_log
rmse_
VarE = 1 - var(exp(predictions) - exp(dataAppLog$SalePrice)) / var(exp(dataAppLog$SalePrice))
VarE

plot(exp(dataAppLog$SalePrice), exp(predictions), main = "Nuage de points observés-prédits (App)",
     xlab = "Observés", ylab = "Prédits")
abline(lm(exp(predictions) ~ exp(dataAppLog$SalePrice), data = dataAppLog), col="red", lwd=1.5)

predictions <- ridge %>% predict(dataTestLog)

rmsle_lm_log = RMSE(dataTestLog$SalePrice, predictions)
rmse_ = RMSE(exp(dataTestLog$SalePrice), exp(predictions))
rmsle_lm_log
rmse_
VarE = 1 - var(exp(predictions) - exp(dataTestLog$SalePrice)) / var(exp(dataTestLog$SalePrice))
VarE

plot(exp(dataTestLog$SalePrice), exp(predictions), main = "Nuage de points observés-prédits (Val)",
     xlab = "Observés", ylab = "Prédits")
abline(lm(exp(predictions) ~ exp(dataTestLog$SalePrice), data = dataAppLog), col="red", lwd=1.5)



###ELASTIC NET###
elastic <- train(
  SalePrice~., data = dataAppLog, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)

predictions <- elastic %>% predict(dataAppLog)

rmsle_lm_log = RMSE(dataAppLog$SalePrice, predictions)
rmse_ = RMSE(exp(dataAppLog$SalePrice), exp(predictions))
rmsle_lm_log
rmse_
VarE = 1 - var(exp(dataAppLog$SalePrice) - exp(predictions)) / var(exp(dataAppLog$SalePrice))
VarE

plot(exp(dataAppLog$SalePrice), exp(predictions), main = "Nuage de points observés-prédits (App)",
     xlab = "Observés", ylab = "Prédits")
abline(lm(exp(predictions) ~ exp(dataAppLog$SalePrice)), col="red", lwd=1.5)

predictions <- elastic %>% predict(dataTestLog)

rmsle_lm_log = RMSE(dataTestLog$SalePrice, predictions)
rmse_ = RMSE(exp(dataTestLog$SalePrice), exp(predictions))
rmsle_lm_log
rmse_
VarE = 1 - var(exp(dataTestLog$SalePrice) - exp(predictions)) / var(exp(dataTestLog$SalePrice))
VarE

plot(exp(dataTestLog$SalePrice), exp(predictions), main = "Nuage de points observés-prédits (Val)",
     xlab = "Observés", ylab = "Prédits")
abline(lm(exp(predictions) ~ exp(dataTestLog$SalePrice)), col="red", lwd=1.5)


###LINEAR SVM###
linear_modelsvm = svm(SalePrice~.,data=dataAppLog, C=1, epsilon = 0.01)

#Predict using SVM regression
predYsvmApp = predict(linear_modelsvm, dataAppLog)
predYsvmTest = predict(linear_modelsvm, dataTestLog)

rmsle_ = RMSE(dataAppLog$SalePrice, predYsvmApp)
rmse_ = RMSE(exp(dataAppLog$SalePrice), exp(predYsvmApp))
VarE = 1 - var(exp(dataAppLog$SalePrice) - exp(predYsvmApp)) / var(exp(dataAppLog$SalePrice))

rmsle_
rmse_
VarE

plot(exp(dataAppLog$SalePrice), exp(predYsvmApp), main = "Nuage de points observés-prédits (App)",
     xlab = "Observés", ylab = "Prédits")
abline(lm(exp(predYsvmApp) ~ exp(dataAppLog$SalePrice)), col="red", lwd=1.5)

rmsle_ = RMSE(dataTestLog$SalePrice, predYsvmTest)
rmse_ = RMSE(exp(dataTestLog$SalePrice), exp(predYsvmTest))
VarE = 1 - var(exp(dataTestLog$SalePrice) - exp(predYsvmTest)) / var(exp(dataTestLog$SalePrice))

rmsle_
rmse_
VarE

plot(exp(dataTestLog$SalePrice), exp(predYsvmTest), main = "Nuage de points observés-prédits (Val)",
     xlab = "Observés", ylab = "Prédits")
abline(lm(exp(predYsvmTest) ~ exp(dataTestLog$SalePrice)), col="red", lwd=1.5)


##### Random Forest

p = ncol(dataAppLog) - 1


dataTrees = traintest_transformed2 %>% filter(!is.na(final_data$SalePrice))
dataTrees = subset(dataTrees, select = -c(TotalSqFeet, BsmtFinSF1, BsmtFinSF2, BsmtUnfSF, X1stFlrSF, X2ndFlrSF, LowQualFinSF, BsmtFullBath, BsmtHalfBath, FullBath, HalfBath, PoolArea, MiscVal))

set.seed(123)
test.ratio = 0.30
npop = nrow(data)
ntest = ceiling(npop*test.ratio)
testi = sample(1:npop, ntest) # indices de l'´echantillon test
appri = setdiff(1:npop, testi) # indices de l'´echant. d'apprentissage
# Construction des ´echantillons avec les variables explicatives
dataTreesApp = dataTrees[appri, ] # construction de l'´echantillon d'apprentissage
dataTreesTest = dataTrees[testi, ] # construction de l'´echantillon test

NAcol <- which(colSums(is.na(data)) > 0)
NAcol


#############NORMALISATION#############

columnames = colnames(DFnorm)


for(label in columnames){
  
  moy = mean(unlist(dataTreesApp[label]))
  sdv = sd(unlist(dataTreesApp[label]))
  
  dataTreesApp[label] = (unlist(dataTreesApp[label]) - moy)/sdv
  dataTreesTest[label] = (unlist(dataTreesTest[label]) - moy)/sdv
  
}


output.forest <- randomForest(SalePrice~., data = dataTreesApp, ntree =1500, mtry=65, importance=T, maxnodes = 200)

output.forest$importance

print(output.forest)
importance(output.forest)

varImpPlot(output.forest)
barplot(output.forest$importance)


variables = importance(output.forest)

predictions <-predict(output.forest, newdata = dataTreesApp)
rmsle_ = RMSE(dataTreesApp$SalePrice, predictions)
rmse_ = RMSE(exp(dataTreesApp$SalePrice), exp(predictions))
VarE = 1 - var(exp(dataTreesApp$SalePrice) - exp(predictions)) / var(exp(dataTreesApp$SalePrice))

rmsle_
rmse_
VarE

plot(exp(dataAppLog$SalePrice), exp(predYsvmApp), main = "Nuage de points observés-prédits (App)",
     xlab = "Observés", ylab = "Prédits")
abline(lm(exp(predYsvmApp) ~ exp(dataAppLog$SalePrice)), col="red", lwd=1.5)



predictions <-predict(output.forest, newdata = dataTreesTest)

rmsle_ = RMSE(dataTreesTest$SalePrice, predictions)
rmse_ = RMSE(exp(dataTreesTest$SalePrice), exp(predictions))
VarE = 1 - var(exp(dataTreesTest$SalePrice) - exp(predictions)) / var(exp(dataTreesTest$SalePrice))

rmsle_
rmse_
VarE

plot(exp(dataTreesTest$SalePrice), exp(predictions), main = "Nuage de points observés-prédits (Val)",
     xlab = "Observés", ylab = "Prédits")
abline(lm(exp(predictions) ~ exp(dataTreesTest$SalePrice)), col="red", lwd=1.5)


##Cross-validation
#mtry = sqrt(ncol(dataAppLog))
modellist <- list()
grid = expand.grid(.mtry=c(48, 65, 100, 150, 180))

for(ntree in c(500, 1000, 2000, 3000)){
  #fit <- train(SalePrice ~ ., data=dataAppLog, method="rf", tuneGrid=tunegrid, trControl=control, ntree=ntree)
  random_forest_model <- train(
    SalePrice ~ .,
    data = dataAppLog,
    method = "rf",
    tuneGrid = grid,
    trControl=trainControl(method = "repeatedcv",  number = 5),
    ntree=ntree
  )
  key <- toString(ntree)
  modellist[[key]] <- random_forest_model
}


results <- resamples(modellist)
summary(results)
dotplot(results)

print(random_forest_model)
plot(random_forest_model)


#####Gradient Boosting#####
cv.ctrl_gbm <- trainControl(method="cv",number=5,repeats = 5)

gbm<- train(SalePrice ~ ., method = "gbm", maximize = FALSE, 
            trControl = cv.ctrl_gbm, 
            tuneGrid = expand.grid(
              n.trees = c(500, 700, 1000, 2000, 2500), interaction.depth = c(5,6,7,8), shrinkage = 0.05, n.minobsinnode=10), 
            data = dataTreesApp,verbose = FALSE)


predictions <- predict(gbm,newdata = dataTreesApp)
rmsle_ = RMSE(dataTreesApp$SalePrice, predictions)
rmse_ = RMSE(exp(dataTreesApp$SalePrice), exp(predictions))
VarE = 1 - var(exp(dataTreesApp$SalePrice) - exp(predictions)) / var(exp(dataTreesApp$SalePrice))

rmsle_
rmse_
VarE

plot(exp(dataTreesApp$SalePrice), exp(predictions), main = "Nuage de points observés-prédits (App)",
     xlab = "Observés", ylab = "Prédits")
abline(lm(exp(predictions) ~ exp(dataTreesApp$SalePrice)), col="red", lwd=1.5)


predictions <-predict(gbm, newdata = dataTreesTest)

rmsle_ = RMSE(dataTreesTest$SalePrice, predictions)
rmse_ = RMSE(exp(dataTreesTest$SalePrice), exp(predictions))
VarE = 1 - var(exp(dataTreesTest$SalePrice) - exp(predictions)) / var(exp(dataTreesTest$SalePrice))

rmsle_
rmse_
VarE

plot(exp(dataTreesTest$SalePrice), exp(predictions), main = "Nuage de points observés-prédits (Val)",
     xlab = "Observés", ylab = "Prédits")
abline(lm(exp(predictions) ~ exp(dataTreesTest$SalePrice)), col="red", lwd=1.5)


gbm



#####POLYNOMIAL SVM


#polynomial_modelsvm = svm(SalePrice~.,data=dataAppLog, kernel="polynomial", degree=6, gamma=0.00009, coef0=2, cost=2, epsilon = 0.1)

predictions <- predict(polynomial_modelsvm, newdata = dataAppLog)
rmsle_lm_log = RMSE(dataAppLog$SalePrice, predictions)
rmse_ = RMSE(exp(dataAppLog$SalePrice), exp(predictions))
rmsle_lm_log
rmse_
VarE = 1 - var(exp(predictions) - exp(dataAppLog$SalePrice)) / var(exp(dataAppLog$SalePrice))
VarE

predictions <- predict(polynomial_modelsvm, newdata = dataTestLog)
rmsle_lm_log = RMSE(dataTestLog$SalePrice, predictions)
rmse_ = RMSE(exp(dataTestLog$SalePrice), exp(predictions))
rmsle_lm_log
rmse_
VarE = 1 - var(exp(predictions) - exp(dataTestLog$SalePrice)) / var(exp(dataTestLog$SalePrice))
VarE


#VALIDATION CROISEE

Opt <- tune(svm, SalePrice~., data = dataAppLog, 
            ranges = list(cost = c(0.2, 0.1, 1, 2, 3, 4), kernel = "polynomial", gamma=0.0001, degree=c(4,5,6,7), epsilon = c(0.01,0.1,1), coef0=2),
            tunecontrol = tune.control(sampling = "cross", cross = 5)
)

BstModel=Opt$best.model

predictions <- predict(BstModel, newdata = dataAppLog)
rmsle_lm_log = RMSE(dataAppLog$SalePrice, predictions)
rmse_ = RMSE(exp(dataAppLog$SalePrice), exp(predictions))
rmsle_lm_log
rmse_
VarE = 1 - var(exp(predictions) - exp(dataAppLog$SalePrice)) / var(exp(dataAppLog$SalePrice))
VarE

plot(exp(dataAppLog$SalePrice), exp(predictions), main = "Nuage de points observés-prédits (App)",
     xlab = "Observés", ylab = "Prédits")
abline(lm(exp(predictions) ~ exp(dataAppLog$SalePrice)), col="red", lwd=1.5)

predictions <- predict(BstModel, newdata = dataTestLog)
rmsle_lm_log = RMSE(dataTestLog$SalePrice, predictions)
rmse_ = RMSE(exp(dataTestLog$SalePrice), exp(predictions))
rmsle_lm_log
rmse_
VarE = 1 - var(exp(predictions) - exp(dataTestLog$SalePrice)) / var(exp(dataTestLog$SalePrice))
VarE

plot(exp(dataTestLog$SalePrice), exp(predictions), main = "Nuage de points observés-prédits (Val)",
     xlab = "Observés", ylab = "Prédits")
abline(lm(exp(predictions) ~ exp(dataTestLog$SalePrice)), col="red", lwd=1.5)


###SVR à noyau radial###

#radial_modelsvm = svm(SalePrice~.,data=dataAppLog, kernel="radial", gamma=0.0005, cost=2)

Opt <- tune(svm, SalePrice~., data = dataAppLog, 
            ranges = list(cost = c(0.2, 0.1, 1, 2, 3, 4), kernel = "radial", 
                          gamma=c(0.0001, 0.0002, 0.0003, 0.0004, 0.0005), 
                          epsilon = c(0.01,0.1,1)),
            tunecontrol = tune.control(sampling = "cross", cross = 5)
)

radial_modelsvm=Opt$best.model

predYsvmApp = predict(radial_modelsvm, dataAppLog)
predYsvmTest = predict(radial_modelsvm, dataTestLog)

rmsle_ = RMSE(dataAppLog$SalePrice, predYsvmApp)
rmse_ = RMSE(exp(dataAppLog$SalePrice), exp(predYsvmApp))
VarE = 1 - var(exp(dataAppLog$SalePrice) - exp(predYsvmApp)) / var(exp(dataAppLog$SalePrice))

rmsle_
rmse_
VarE

plot(exp(dataAppLog$SalePrice), exp(predYsvmApp), main = "Nuage de points observés-prédits (App)",
     xlab = "Observés", ylab = "Prédits")
abline(lm(exp(predYsvmApp) ~ exp(dataAppLog$SalePrice)), col="red", lwd=1.5)

rmsle_ = RMSE(dataTestLog$SalePrice, predYsvmTest)
rmse_ = RMSE(exp(dataTestLog$SalePrice), exp(predYsvmTest))
VarE = 1 - var(exp(dataTestLog$SalePrice) - exp(predYsvmTest)) / var(exp(dataTestLog$SalePrice))

rmsle_
rmse_
VarE

plot(exp(dataTestLog$SalePrice), exp(predYsvmTest), main = "Nuage de points observés-prédits (Val)",
     xlab = "Observés", ylab = "Prédits")
abline(lm(exp(predYsvmTest) ~ exp(dataTestLog$SalePrice)), col="red", lwd=1.5)


#######GAM#####

dataGAM = traintest_transformed2 %>% filter(!is.na(traintest_transformed2$SalePrice))

set.seed(123)
test.ratio = 0.30
npop = nrow(dataGAM)
ntest = ceiling(npop*test.ratio)
testi = sample(1:npop, ntest) # indices de l'´echantillon test
appri = setdiff(1:npop, testi) # indices de l'´echant. d'apprentissage
# Construction des ´echantillons avec les variables explicatives
train_set = dataGAM[appri, ] # construction de l'´echantillon d'apprentissage
test_set = dataGAM[testi, ] # construction de l'´echantillon test



dataAppLogGam <- subset(train_set, select = -c(SalePrice))
dataTestLogGam <- subset(test_set, select = -c(SalePrice))

SalePrice = train_set$SalePrice
dataAppLogGam = cbind(dataAppLogGam, SalePrice)

SalePrice = test_set$SalePrice
dataTestLogGam = cbind(dataTestLogGam, SalePrice)

colQuant <- c(numericVars, numerciDiscreteVars)
colQual <- names(dataAppLogGam[, !names(dataAppLogGam) %in% colQuant])
colQual <- colQual[-length(colQual)]
colQual

for(j in colQuant){
  #print(var(dataAppLogGam[j]))
  if(var(dataAppLogGam[j]) < 0.05){
    print(j)
    #dataAppLogGam = subset(dataAppLogGam, select = -c())
  }
}

dataAppLogGam = subset(dataAppLogGam, select = -c(Street,PoolArea, MiscVal))
dataAppLogGam = subset(dataTestLogGam, select = -c(Street,PoolArea, MiscVal))

new_colQuant = colQuant[-11]
new_colQuant = new_colQuant[-20]
new_colQuant = new_colQuant[-19]


###GAM LINEAIRE###
colQuantQual <- names(dataAppLogGam)
colQuantQual <- colQuantQual[-length(colQuantQual)]

fmQuantQual <- paste(colQuantQual, sep = "", collapse = ' + ')
fmLCheck <- as.formula(paste('SalePrice ~', fmQuantQual))
#fmLCheck

modelGamLCheck <- gam(fmLCheck, data = dataAppLogGam)
summary(modelGamLCheck)

predictions <- predict(modelGamLCheck, newdata = train_set)
rmsle_lm_log = RMSE(train_set$SalePrice, predictions)
rmse_ = RMSE(exp(train_set$SalePrice), exp(predictions))
rmsle_lm_log
rmse_
VarE = 1 - var(exp(predictions) - exp(train_set$SalePrice)) / var(exp(train_set$SalePrice))
VarE

predictions <- predict(modelGamLCheck, newdata = test_set)
rmsle_lm_log = RMSE(test_set$SalePrice, predictions)
rmse_ = RMSE(exp(test_set$SalePrice), exp(predictions))
rmsle_lm_log
rmse_
VarE = 1 - var(exp(predictions) - exp(test_set$SalePrice)) / var(exp(test_set$SalePrice))
VarE


###GAM avec des composantes non linéaires###
library(mgcv)
#fmQuant <- paste(colQuant, sep = "", collapse = ' + ')
fmQuantNL <- paste('s(', new_colQuant, ', k=-1, bs="cs")', sep = "", collapse = ' + ')
#fmNL <- paste(fmQuant,'+',fmQuantNL)
fmNL <- as.formula(paste('SalePrice ~', fmQuantNL))
fmNL

modelGamNL <- gam(fmNL, data = dataAppLogGam)
summary(modelGamNL)


fmQuant <- paste(colQuant, sep = "", collapse = ' + ')
fmL <- as.formula(paste('SalePrice ~', fmQuant))
fmL

modelGamL <- gam(fmL, data = dataAppLogGam)
summary(modelGamL)



###########POUR TESTER EN LIGNE

data = final_data %>% filter(!is.na(final_data$SalePrice))
for_predictions = final_data %>% filter(is.na(final_data$SalePrice))

ids = traintest$Id
Id = ids[1461:2919]

#Regression linéaire

linear_mod <- lm(SalePrice~., data = data)
predictions = predict(linear_mod, for_predictions)
SalePrice = exp(predictions)

results = cbind(Id, SalePrice)
write.csv(results, "D:/PREDICTIONS/reg_lin.csv", row.names = FALSE)


#Regression Lasso

X = subset(data, select = -c(SalePrice))
Y = data$SalePrice
Xt = subset(for_predictions, select = -c(SalePrice))

lambda_seq <- 10^seq(-4, 4, by = .1)

cv_output <- cv.glmnet(as.matrix(X), as.matrix(Y), alpha = 1, lambda = lambda_seq, nfolds = 10)

best_lam <- cv_output$lambda.min

lasso_best <- glmnet(as.matrix(X), as.matrix(Y), alpha = 1, lambda = best_lam)

predictions <- predict(lasso_best, s = best_lam, newx = as.matrix(Xt))
SalePrice = exp(predictions)

results = cbind(Id, SalePrice)
write.csv(results, "D:/PREDICTIONS/lasso_reg.csv", row.names = FALSE)


#Regression Ridge

ridge <- train(
  SalePrice ~., data = data, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 0, lambda = lambda)
)

predictions <- ridge %>% predict(for_predictions)

SalePrice = exp(predictions)

results = cbind(Id, SalePrice)
write.csv(results, "D:/PREDICTIONS/ridge_reg.csv", row.names = FALSE)


#ElasticNet

elastic <- train(
  SalePrice~., data = data, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)

predictions <- elastic %>% predict(for_predictions)

SalePrice = exp(predictions)

results = cbind(Id, SalePrice)
write.csv(results, "D:/PREDICTIONS/elasticnet_reg.csv", row.names = FALSE)


#SVR Linéaire

Opt <- tune(svm, SalePrice~., data = data, 
            ranges = list(cost = c(0.2, 0.1, 1, 2, 3, 4), kernel = "linear", 
                          epsilon = c(0.01,0.1,1)),
            tunecontrol = tune.control(sampling = "cross", cross = 5)
)

linear_modelsvm = Opt$best.model

predictions = predict(linear_modelsvm, newdata = Xt)

SalePrice = exp(predictions)

results = cbind(Id, SalePrice)
write.csv(results, "D:/PREDICTIONS/linear_svm.csv", row.names = FALSE)


#SVR polynomial

polynomial_modelsvm = svm(SalePrice~.,data=data, kernel="polynomial", degree=6, 
                          gamma=0.0001, coef0=2, cost=2, epsilon = 0.01)

predictions = predict(polynomial_modelsvm, newdata = Xt)

SalePrice = exp(predictions)

results = cbind(Id, SalePrice)
write.csv(results, "D:/PREDICTIONS/poly_svm.csv", row.names = FALSE)


#SVR RBF

Opt <- tune(svm, SalePrice~., data = data, 
            ranges = list(cost = c(0.2, 0.1, 1, 2, 3, 4), kernel = "radial", 
                          gamma=c(0.0003, 0.0004, 0.0005, 0.0006, 0.0007, 0.0008), 
                          epsilon = c(0.01,0.1,1)),
            tunecontrol = tune.control(sampling = "cross", cross = 5)
)

radial_modelsvm=Opt$best.model

#radial_modelsvm = svm(SalePrice~.,data=data, kernel="radial",C=4, gamma=0.00005, epsilon=0.1)

predictions = predict(radial_modelsvm, newdata = Xt)

SalePrice = exp(predictions)

results = cbind(Id, SalePrice)
write.csv(results, "D:/PREDICTIONS/radial_svm.csv", row.names = FALSE)


#Random Forest
output.forest <- randomForest(SalePrice~., data = dataTrees, ntree =2000, mtry=65, importance=T, maxnodes=600)

dataTrees_prediction = traintest_transformed2 %>% filter(is.na(final_data$SalePrice))

predictions = predict(output.forest, newdata = subset(dataTrees_prediction, select = -c(SalePrice)))

SalePrice = exp(predictions)

results = cbind(Id, SalePrice)
write.csv(results, "D:/PREDICTIONS/RF.csv", row.names = FALSE)



########## WITH GBOOST
gbm_model <-train(SalePrice ~ ., method = "gbm", maximize = FALSE, 
                  trControl = cv.ctrl_gbm, 
                  tuneGrid = expand.grid(
                  n.trees = c(500, 700, 1000, 2000, 2500), interaction.depth = c(5,6,7,8), shrinkage = 0.05, n.minobsinnode=10), 
                  data = data,verbose = FALSE)

predictions <- predict(gbm_model,newdata = for_predictions)


gboost = cbind(Id, SalePrice)
write.csv(gboost, "D:/PREDICTIONS/gboost.csv", row.names = FALSE)




vsurf_mod <- VSURF(SalePrice~., dataTreesApp, parallel = TRUE, ncores = 3)






