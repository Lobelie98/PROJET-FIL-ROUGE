#Import des librairies
library(dplyr)
library(ggplot2)

#import des données
data <- read.csv("C:/Users/Lobélie/OneDrive\\Bureau/Cours INSA/Projet Fil Rouge/PROJET-FIL-ROUGE/sources/traintest.csv", header = TRUE, sep = ",")

Id <- data$Id
subdata <- data %>% select(55:81)
dataset <- cbind(Id, subdata)

#Valeurs manquantes pour chaque variable
columns <- colnames(dataset)
na_values <- rep(NA,length(columns))

for(i in 1:length(columns)){
  na_values[i] <- sum(is.na(dataset %>% select(columns[i])))
}
table_matrix <- cbind(columns, na_values)

#Display the factors for every qualitative variable
print(levels(as.factor(dataset$MiscVal)))

#Conversion de variables qualitatives en variables quantitatives

#Liste des variables à convertir
labels <- c("GarageYrBlt", "GarageArea", "WoodDeckSF", "OpenPorchSF", "EnclosedPorch", "X3SsnPorch", "ScreenPorch", "PoolArea")
label <- "GarageYrBlt"
dataset_transformed <- dataset

for (label in labels){
  dataset_transformed[label]<-as.numeric(unlist(dataset[label]))
  print(length(colnames(dataset_transformed)))
}

summary(dataset_transformed)

#Cas de GarageYrBlt

summary(dataset_transformed$GarageYrBlt)
boxplot(dataset_transformed$GarageYrBlt)

hist(dataset_transformed$GarageYrBlt)

#On a une valeur atypique de GarageYrBlt qui vaut 2207. On estime que cela est une erreur de saisie 
#et on décide d'attribuer la valeur 2007

index_value <- match(2207, dataset_transformed$GarageYrBlt)
dataset_transformed$GarageYrBlt[index_value] <- 2007

summary(dataset_transformed$GarageYrBlt)
boxplot(dataset_transformed$GarageYrBlt)

#Ayant réglé le problème de cette donnée atypique, on décide de transformer cette variable en variable âge

#max_year <- max(dataset_transformed$GarageYrBlt, na.rm = TRUE)

dataset_transformed$GarageAge <- 2010 - dataset_transformed$GarageYrBlt
dataset_transformed <- subset(dataset_transformed, select = -c(GarageYrBlt))
attach(dataset_transformed)
hist(GarageAge)
summary(GarageAge)
boxplot(GarageAge)

boxplot(x = GarageArea)
hist(x = GarageArea)

boxplot(x= WoodDeckSF)


hist(x= WoodDeckSF)

boxplot(OpenPorchSF)
hist(OpenPorchSF)

boxplot(na.omit(X3SsnPorch))
hist(na.omit(X3SsnPorch))


boxplot(ScreenPorch)
hist(ScreenPorch)

boxplot(PoolArea)
hist(PoolArea)

h <- hist(SalePrice)
x = na.omit(dataset_transformed$SalePrice)
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

boxplot(SalePrice)


#STATS BIVARIEES

set <- na.omit(cbind(dataset_transformed$GarageAge, dataset_transformed$GarageArea, dataset_transformed$WoodDeckSF,
                     dataset_transformed$OpenPorchSF, dataset_transformed$EnclosedPorch,dataset_transformed$X3SsnPorch, 
                     dataset_transformed$ScreenPorch))
cor(set)


source("http://www.sthda.com/upload/rquery_cormat.r")
cor_df = subset(dataset_transformed, select = c(GarageAge, GarageArea, WoodDeckSF, OpenPorchSF, EnclosedPorch, X3SsnPorch, ScreenPorch))
summary(cor_df)
rquery.cormat(cor_df, type = "full")

cor(na.omit(set))

##Qualitatif-SalePrice

boxplot(SalePrice~TotRmsAbvGrd)
boxplot(SalePrice~Functional)
boxplot(SalePrice~Fireplaces)
boxplot(SalePrice~FireplaceQu)
boxplot(SalePrice~GarageType)
boxplot(SalePrice~GarageFinish)
boxplot(SalePrice~GarageCars)
boxplot(SalePrice~GarageCond)
boxplot(SalePrice~GarageQual)
boxplot(SalePrice~PavedDrive)
boxplot(SalePrice~MiscFeature)
boxplot(SalePrice~MoSold)
boxplot(SalePrice~YrSold)
boxplot(SalePrice~SaleType)
boxplot(SalePrice~SaleCondition)

#Tests ANOVA à un facteur

fit <- aov(SalePrice ~ SaleCondition , data=dataset_transformed) # y est la variable numérique et A indique les groupes
summary(fit)

#Tests ANOVA à deux facteurs
fit <- aov(SalePrice ~ MoSold*YrSold , data=dataset_transformed) # y est la variable numérique et A indique les groupes
summary(fit)
fit <- aov(SalePrice ~ Fireplaces*FireplaceQu , data=dataset_transformed) # y est la variable numérique et A indique les groupes
summary(fit)
fit <- aov(SalePrice ~ SaleType*SaleCondition , data=dataset_transformed) # y est la variable numérique et A indique les groupes
summary(fit)

fit <- aov(SalePrice ~ GarageType*GarageFinish , data=dataset_transformed) # y est la variable numérique et A indique les groupes
summary(fit)

fit <- aov(SalePrice ~ GarageCars*GarageCond*GarageQual , data=dataset_transformed) # y est la variable numérique et A indique les groupes
summary(fit)

#Bibu <- paste(GarageCars,GarageCond,sep = "-")
#DF <- dataset_transformed
#Df <- cbind(DF, Bibu)
#fit <- aov(SalePrice ~ Bibu , data=DF) # y est la variable numérique et A indique les groupes
#summary(fit)

#boxplot(SalePrice ~ Bibu)

#print(levels(as.factor(Df$Bibu)))


######Feed NA values######

train_dataset <- read.csv("C:/Users/Lobélie/Desktop/Cours INSA/Projet Fil Rouge/PROJET-FIL-ROUGE/sources/train.csv", header = TRUE, sep = ",")
test_dataset <- read.csv("C:/Users/Lobélie/Desktop/Cours INSA/Projet Fil Rouge/PROJET-FIL-ROUGE/sources/test.csv", header = TRUE, sep = ",")



train = dataset_transformed[1:1460,]
  
DF <- train
Modified <- rep(0, length(DF$Id))
Pool <- rep(NA, length(DF$Id))
MiscFeaturePres <- rep(NA, length(DF$Id))

DF <- cbind(DF, Modified, MiscFeaturePres)

#DF <- cbind(dataset_transformed, c(rep(NA, dim(dataset_transformed)[1])))
#DF <- rename(DF, c("c(rep(NA, dim(dataset_transformed)[1]))" = "modified"))

#Variable Functional
value <- DF$Functional
for(i in 1:length(value)){
  if(is.na(value[i])){
    DF$Functional[i] <- "Typ"
    DF$Modified <- 1
  }
  else{DF$Modified <- 0}
}

#Variable FireplaceQu
value <- DF$FireplaceQu
for(i in 1:length(value)){
  if(is.na(value[i])){
    DF$FireplaceQu[i] <- "None"
    DF$Modified <- 1
  }
}

#Variable GarageType
value <- DF$GarageType
for(i in 1:length(value)){
  if(is.na(value[i])){
    DF$GarageType[i] <- "None"
    DF$Modified <- 1
  }
  
}

#Variable GarageFinish
value <- DF$GarageFinish
for(i in 1:length(value)){
  if(is.na(value[i])){
    DF$GarageFinish[i] <- "None"
    DF$Modified <- 1
  }
  
}

#Variable GarageCars
value <- DF$GarageCars
for(i in 1:length(value)){
  if(is.na(value[i])){
    DF$GarageCars[i] <- "2"
    DF$Modified <- 1
  }
  
}

#Variable GarageQual
value <- DF$GarageQual
for(i in 1:length(value)){
  if(is.na(value[i])){
    DF$GarageQual[i] <- "2"
    DF$Modified <- 1
  }
  
}

#Variable GarageCond
value <- DF$GarageCond
for(i in 1:length(value)){
  if(is.na(value[i])){
    DF$GarageCond[i] <- "None"
    DF$Modified <- 1
  }
  
}

#Variable Fence
value <- DF$Fence
for(i in 1:length(value)){
  if(is.na(value[i])){
    DF$Fence[i] <- "None"
    DF$Modified <- 1
  }
}

#Variable GarageAge
value <- DF$GarageAge
for(i in 1:length(value)){
  if(is.na(value[i])){
    DF$GarageAge[i] <- 0
    DF$Modified <- 1
  }
}

#Variable à ajouter : Pool en fonction de PoolArea/PoolQc
value <- DF$PoolQC
for(i in 1:length(value)){
  if(is.na(value[i])){
    DF$PoolQC[i] <- "None"
    DF$Modified <- 1
  }
}

#Variable à ajouter : MiscFeaturePres en fonction de MiscFeature
value <- DF$MiscFeature
for(i in 1:length(value)){
  if(is.na(value[i])){
    DF$MiscFeaturePres[i] <- "1"
  }
  else{DF$MiscFeaturePres[i] <- "0"}
}

final_DF <- subset(DF, select = c(TotRmsAbvGrd, Functional,Fireplaces, FireplaceQu, GarageAge,
                                  GarageType, GarageFinish, GarageCars, GarageArea, GarageQual,
                                  GarageCond, PavedDrive,WoodDeckSF, OpenPorchSF, EnclosedPorch,
                                  X3SsnPorch, ScreenPorch, PoolArea, PoolQC, Fence, MiscFeaturePres, MoSold,
                                  YrSold, SaleType, SaleCondition, Modified, SalePrice))

attach(final_DF)

write.csv(final_DF, "C:\\Users\\Lobélie\\OneDrive\\Bureau\\Cours INSA\\Projet Fil Rouge\\PROJET-FIL-ROUGE/training_sets/train_data3_noE.csv", row.names = FALSE)

fit <- aov(SalePrice ~ Pool , data=final_DF) # y est la variable numérique et A indique les groupes
summary(fit)

fit1 <- aov(SalePrice ~ PoolQC , data=dataset_transformed) # y est la variable numérique et A indique les groupes
summary(fit1)


#################################### CUT IT THERE ####################################


#Check the highest year value
#max_year <- max(dataset_transformed$GarageYrBlt, na.rm = TRUE)
#This value is 2207. We will take the highest one just before it in the dataset
#max_year <- max(dataset_transformed$GarageYrBlt [dataset_transformed$GarageYrBlt != 2207], na.rm = TRUE)

#Now we have 2010
#We're going to get the age of each garage


######################## Stats descriptives univariées ########################
ggplot(data=dataset_transformed, aes(x = TotRmsAbvGrd))+
  geom_bar(fill="#69b3a2", width = 0.3)+
  theme(
    plot.title = element_text(size=15)
)
#**********************************

ggplot(data=dataset_transformed, aes(x = Functional))+
  geom_bar(fill="#69b3a2", width = 0.3)+
  theme(
    plot.title = element_text(size=15)
  )
#**********************************


ggplot(data=dataset_transformed, aes(x = Fireplaces))+
  geom_bar(fill="#69b3a2", width = 0.3)+
  theme(
    plot.title = element_text(size=15)
  )
#**********************************


ggplot(data=dataset_transformed, aes(x = FireplaceQu))+
  geom_bar(fill="#69b3a2", width = 0.3)+
  theme(
    plot.title = element_text(size=15)
  )
#**********************************

ggplot(data=dataset_transformed, aes(x = GarageType))+
  geom_bar(fill="#69b3a2", width = 0.3)+
  theme(
    plot.title = element_text(size=15)
  )
#********************************** 

ggplot(data=dataset_transformed, aes(x = GarageFinish))+
  geom_bar(fill="#69b3a2", width = 0.3)+
  theme(
    plot.title = element_text(size=15)
  )
#********************************** 

ggplot(data=dataset_transformed, aes(x = GarageCars))+
  geom_bar(fill="#69b3a2", width = 0.3)+
  theme(
    plot.title = element_text(size=15)
  )

ggplot(data=dataset_transformed, aes(x = GarageQual))+
  geom_bar(fill="#69b3a2", width = 0.3)+
  theme(
    plot.title = element_text(size=15)
  )


ggplot(data=dataset_transformed, aes(x = PavedDrive))+
  geom_bar(fill="#69b3a2", width = 0.3)+
  theme(
    plot.title = element_text(size=15)
  )


ggplot(data=dataset_transformed, aes(x = PoolQC))+
  geom_bar(fill="#69b3a2", width = 0.3)+
  theme(
    plot.title = element_text(size=15)
  )


ggplot(data=dataset_transformed, aes(x = Fence))+
  geom_bar(fill="#69b3a2", width = 0.3)+
  theme(
    plot.title = element_text(size=15)
  )


ggplot(data=dataset_transformed, aes(x = MiscFeature))+
  geom_bar(fill="#69b3a2", width = 0.3)+
  theme(
    plot.title = element_text(size=15)
  )


ggplot(data=dataset_transformed, aes(x = MiscVal))+
  geom_bar(fill="#69b3a2", width = 0.3)+
  theme(
    plot.title = element_text(size=15)
  )

#**************************

ggplot(data=dataset_transformed, aes(x = MoSold))+
  geom_bar(fill="#69b3a2", width = 0.3)+
  theme(
    plot.title = element_text(size=15)
  )

#**************************

ggplot(data=dataset_transformed, aes(x = YrSold))+
  geom_bar(fill="#69b3a2", width = 0.3)+
  theme(
    plot.title = element_text(size=15)
  )

#**************************

ggplot(data=dataset_transformed, aes(x = SaleType))+
  geom_bar(fill="#69b3a2", width = 0.3)+
  theme(
    plot.title = element_text(size=15)
  )

#**************************

ggplot(data=dataset_transformed, aes(x = SaleCondition))+
  geom_bar(fill="#69b3a2", width = 0.3)+
  theme(
    plot.title = element_text(size=15)
  )


#Tests ANOVA 

fit <- aov(SalePrice ~ SaleCondition , data=dataset_transformed) # y est la variable numérique et A indique les groupes
summary(fit)


#Tests khi-deux 
a <- table(MoSold, GarageCond)
a
test <- chisq.test(a)
test$p.value


variables_qual <- c("TotRmsAbvGrd", "Functional", "Fireplaces", "FireplaceQu", "GarageType",
                     "GarageFinish", "GarageCars", "GarageQual", "GarageCond",
                    "PavedDrive", "PoolQC", "Fence", "MiscFeature", "MoSold", "YrSold",
                    "SaleType", "SaleCondition")

Pvalues <- matrix(NA, nrow = length(variables_qual), ncol = length(variables_qual))

for(i in 1:length(variables_qual)){
  for(j in 1: length(variables_qual)){
    a <- table(unlist(dataset_transformed[variables_qual[i]]), unlist(dataset_transformed[variables_qual[j]]))
    print(a)
    test <- chisq.test(a)
    Pvalues[i,j] <- test$p.value
  }
}


#******************COMPLÉTION DES DONNÉES******************#

DF <- dataset_transformed

value <- DF$MiscFeature

for(i in 1:length(value)){
  if(is.na(value[i])){
    DF$MiscFeature[i] <- "None"
    print(i)
  }
}



###################################################################################################################

c("TotRmsAbvGrd", "Functional", "Fireplaces", "FireplaceQu", "GarageType",
  "GarageFinish", "GarageCars", "GarageQual", "GarageCond",
  "PavedDrive", "PoolQC", "Fence", "MiscFeature", "MoSold", "YrSold",
  "SaleType", "SaleCondition")

attach(DF)
fit <- aov(SalePrice ~ Fence , data=dataset_transformed) # y est la variable numérique et A indique les groupes
summary(fit)



###############################################################"""
variables_qual <- c("TotRmsAbvGrd", "Functional", "Fireplaces", "FireplaceQu", "GarageType",
                    "GarageFinish", "GarageCars", "GarageQual", "GarageCond",
                    "PavedDrive", "PoolQC", "Fence", "MiscFeature", "MoSold", "YrSold",
                    "SaleType", "SaleCondition")

Pvalues2 <- matrix(NA, nrow = length(variables_qual), ncol = length(variables_qual))

for(i in 1:length(variables_qual)){
  for(j in 1: length(variables_qual)){
    a <- table(unlist(DF[variables_qual[i]]), unlist(DF[variables_qual[j]]))
    print(a)
    test <- chisq.test(a)
    Pvalues2[i,j] <- test$p.value
  }
}







