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


#Stats descriptives
ggplot(data=dataset, aes(x = YrSold))+
  geom_bar(fill="#69b3a2", width = 0.3)+
  theme(
    plot.title = element_text(size=15)
)
