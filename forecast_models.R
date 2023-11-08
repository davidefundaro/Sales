source("Functions.R")
source("Functions_Orange.R")
#tolower and space cleaning for column names and content
train_cleaned <- train_Raw
test_cleaned <- test_Raw
transactions_cleaned <- transactions_Raw
oil_cleaned <- oil_Raw
holidays_events_cleaned <- holidays_events_Raw
stores_cleaned <- stores_Raw

#clean the data
colnames(train_cleaned) <- tolower(colnames(train_cleaned)) %>% gsub(" |_", ".",.)
colnames(test_cleaned) <- tolower(colnames(test_cleaned)) %>% gsub(" |_", ".",.)
colnames(transactions_cleaned) <- tolower(colnames(transactions_cleaned)) %>% gsub(" |_", ".",.)
colnames(oil_cleaned) <- tolower(colnames(oil_cleaned)) %>% gsub(" |_", ".",.)
colnames(holidays_events_cleaned) <- tolower(colnames(holidays_events_cleaned)) %>% gsub(" |_", ".",.)
colnames(stores_cleaned) <- tolower(colnames(stores_cleaned)) %>% gsub(" |_", ".",.)


#summarise the sales
train <- train_cleaned %>% group_by(date) %>% summarise(sum.sales = sum(sales), sum.promo = sum(onpromotion))

train$date <- train$date %>% as.Date()

#subset eliminating the last two weeks
subset_train <- train[1:(nrow(train) - 14), ]

#calculate the mean
subset_train$mean.sales <- mean(subset_train$sum.sales)

#create the mean model
mean.model <- lm(sum.sales ~ mean.sales, data = subset_train)
summary(mean.model)

#create the test subset for testing the model
test <- train_cleaned %>% group_by(date) %>% summarise(sum.sales = sum(sales))
test <- tail(test, 14)

subset_train$date <- subset_train$date %>% as.Date()
test$date <- test$date %>% as.Date()


#########  mean model #########



#calculate the mean
test$mean.sales <- mean(test$sum.sales)

#predict test sales through the mean
y <- predict(mean.model, newdata = test)
act <- test$sum.sales
rmsle(act, y) #0.274

############  prophet #########

df <- subset_train
df <- df %>% mutate(ds= date, y= sum.sales)
df <- column_to_rownames(df, var = "date")
m <- prophet(df)
future <- make_future_dataframe(m, periods = 14)
forecast <- predict(m, future)
plot(m, forecast)
forecast <- tail(forecast, 14)
act <- test$sum.sales
rmsle(forecast$yhat, act) #0.094




#########  linear model  #########


#variables we can use: year, promotions, holidays, family, store code

test <- train_cleaned %>% group_by(date) %>% summarise(sum.sales = sum(sales), sum.promo= sum(onpromotion))
test <- tail(test, 14)
test$date <- test$date %>% as.Date()
test$year <- as.numeric(format(test$date, format = "%Y"))

subset_train$year <- as.numeric(format(subset_train$date, format = "%Y"))

#create the model
linear.model <- lm(sum.sales ~ sum.promo + year , data = subset_train)
summary(linear.model)

#predict test sales through the mean
test$preds <- predict(linear.model, newdata = test)
act <- test$sum.sales
rmsle(act, test$preds) #0.16

ggplot(test, aes(x = date)) +
  geom_line(aes(y = sum.sales, color = "Actuals")) +
  geom_line(aes(y = preds, color = "Predicted")) +
  scale_color_manual(values = c("Actuals" = "blue", "Predicted" = "red"))


#######one model with also n day, n month, n day in the week ########

#########one model with oil.price and transaction predictios used as variables #######


