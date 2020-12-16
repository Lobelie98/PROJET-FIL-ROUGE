#Import des librairies
library(dplyr)
library(ggplot2)

#import des données
data <- read.csv("C:/Users/Lobélie/Desktop/Cours INSA/Projet Fil Rouge/PROJET-FIL-ROUGE/sources/traintest.csv", header = TRUE, sep = ",")


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
typeof(dataset_transformed$GarageYrBlt)

#Cas de GarageYrBlt
a <- dataset_transformed %>% filter(GarageYrBlt != 2207)
max(a, na.rm = TRUE)

bal <- as.numeric(dataset[label])
print(bal)
print(as.numeric(dataset$GarageYrBlt))
boxplot(bal)


#Stats descriptives
ggplot(data=dataset, aes(x = YrSold))+
  geom_bar(fill="#69b3a2", width = 0.3)+
  theme(
    plot.title = element_text(size=15)
)
