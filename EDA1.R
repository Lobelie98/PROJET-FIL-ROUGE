#import data
data <- read.csv("traintest.csv", header = TRUE, sep = ",")
train <- read.table("train.csv", header = TRUE, sep = ",")

class(train)

library(dplyr)
typeof(train$MSSubClass)

a <- count(train$MSSubClass)
b <- count(train$MSZoning)
levels(as.factor(train$Street))

#Analyse des variables qualitatives
library(ggplot2)


sum(is.na(train$MSSubClass))

summary(train)

Id <- train$Id
lobelie_train <- train %>% select(55:81)
dataset <- cbind(Id, lobelie_train)

#Count null values

columns <- colnames(dataset)
na_values <- rep(NA,length(columns))

for(i in 1:length(columns)){
  #na_values[i] <- sum(is.na(dataset$columns[i]))
  na_values[i] <- sum(is.na(dataset %>% select(columns[i])))
  #print(sum(is.na(dataset$columns[i])))
}

print(na_values)


table_matrix <- cbind(columns, na_values)


ggplot(data=dataset, aes(x = YrSold))+
  geom_bar(fill="#69b3a2", width = 0.3)+
  theme(
    plot.title = element_text(size=15)
)
