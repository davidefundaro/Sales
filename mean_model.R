source("Functions.R")
source("Functions_Orange.R")
#tolower and space cleaning for column names and content
train_cleaned <- train_Raw
test_cleaned <- test_Raw

#clean the data
colnames(train_cleaned) <- tolower(colnames(train_cleaned)) %>% gsub(" |_", ".",.)
colnames(test_cleaned) <- tolower(colnames(test_cleaned)) %>% gsub(" |_", ".",.)

#summarise the sales
train <- train_cleaned %>% group_by(date) %>% summarise(sum.sales = sum(sales))

#calculate the mean
train$mean.sales <- mean(train$sum.sales)

#create the mean model
mean.model <- lm(sum.sales ~ mean.sales, data = train)
summary(mean.model)

#create the test subset for testing the model
test <- train_cleaned %>% group_by(date) %>% summarise(sum.sales = sum(sales))
test <- tail(test, 14)

y <- predict(mean.model, newdata = test)

plot(train$sum.sales, type = "l") 
lines(train$mean.sales, type= "l", col= "red")

#CREA UN NUOVO DATAFRAME DI TRAIN MA SENZA LE ULTIME DUE SETTIMANE