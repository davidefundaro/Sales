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


#calculate the mean
test$mean.sales <- mean(test$sum.sales)

#predict test sales through the mean
y <- predict(mean.model, newdata = test)
act <- test$sum.sales
rmsle(act, y) #0.274


#keep only the data you need
oil <- oil_cleaned %>% distinct()
transactions <- transactions_cleaned %>% group_by(date) %>% summarise(sum.trans = sum(transactions))
checkcolumns(train$date, transactions$date)
holidays <- holidays_events_cleaned %>% select(date, locale) %>% distinct()

#set date as.Date
oil$date <- as.Date(oil$date, format = "%Y-%m-%d")
transactions$date <- as.Date(transactions$date, format = "%Y-%m-%d")
holidays$date <- as.Date(holidays$date, format = "%Y-%m-%d")

#merge dataframes
merged_variables <- left_join(train, oil, by="date") %>% left_join(transactions, by= "date") %>%
  left_join(holidays, by= "date")

#eliminate duplicates caused by diffirent types of festivities (National, local)
merged_variables <- merged_variables %>%  group_by(date) %>%  arrange(desc(locale == "National")) %>%  slice(1) %>%  ungroup()

#when there are no festivities replace NA with None
no_holiday <- "None"
merged_variables$locale <- ifelse(is.na(merged_variables$locale), no_holiday, merged_variables$locale)
#complete cases for oil.prices
merged_variables <- merged_variables[complete.cases(merged_variables), ]
#rename variables
merged_variables <- merged_variables %>% rename(oil.price = dcoilwtico, festivities= locale)

#subset the train data and the test data and transforma the date as.Date
merged_test <- tail(merged_variables, 14)
subset_merged <- merged_variables[1:(nrow(merged_variables) - 14), ]
subset_merged$date <- as.Date(subset_merged$date, format = "%Y-%m-%d")
merged_test$date <- as.Date(merged_test$date, format = "%Y-%m-%d")

#create the model
linear.model <- lm(sum.sales ~ oil.price + sum.trans , data = merged_variables)
summary(linear.model)

#predict test sales through the mean
merged_test$preds <- predict(linear.model, newdata = merged_test)
act <- merged_test$sum.sales
rmsle(act, merged_test$preds) #0.132

ggplot(merged_test, aes(x = date)) +
  geom_line(aes(y = sum.sales, color = "Actuals")) +
  geom_line(aes(y = preds, color = "Predicted")) +
  scale_color_manual(values = c("Actuals" = "blue", "Predicted" = "red"))


c1 <- cor(merged_test %>% filter(complete.cases(merged_test)) %>% 
            select(sum.sales, sum.promo, oil.price, sum.trans))
corrplot(c1,method = c('color'),addCoef.col = "black",addgrid.col = "black",tl.col = "black",order = 'hclust')





#create the model
second.linear.model <- lm(sum.sales ~ oil.price + sum.trans + sum.promo, data = merged_variables)
summary(second.linear.model)

#predict test sales through the mean
merged_test$preds <- predict(second.linear.model, newdata = merged_test)
act <- merged_test$sum.sales
rmsle(act, merged_test$preds) #0.1

ggplot(merged_test, aes(x = date)) +
  geom_line(aes(y = sum.sales, color = "Actuals")) +
  geom_line(aes(y = preds, color = "Predicted")) +
  scale_color_manual(values = c("Actuals" = "blue", "Predicted" = "red"))



#############################################
####                ARIMA             ####
#############################################
ts.data <- ts(merged_variables$sum.sales, start=c(2013,01,02), end= c(2017,08,15), frequency = 252)

plot(ts.data)

components <- decompose(ts.data)

plot(components)

adf.test(ts.data)

acf(ts.data, lag.max= 100)
pacf(ts.data)

#auto.arima(ts.data, trace= TRUE)

fitARIMA <- arima(ts.data, order=c(5,1,5), method="ML")
library(lmtest)
coeftest(fitARIMA)
confint(fitARIMA)
checkresiduals(fitARIMA)







#####################################################
####                    GAM                     ####
#####################################################

merged_variables$festivities<-as.factor(merged_variables$festivities)

kn=4
gam.mod<- gam(sum.sales ~ s(sum.promo, k=kn, bs= 'tp') + s(oil.price, k=kn, bs= 'tp') +
            s(sum.trans, k=kn, bs= 'tp'), data=merged_variables)
#s(festivities, bs= 're')

summary(gam.mod)

#predict test sales through the mean
merged_test$preds <- predict(gam.mod, newdata = merged_test)
act <- merged_test$sum.sales
rmsle(act, merged_test$preds) #0.06


ggplot(merged_test, aes(x = date)) +
  geom_line(aes(y = sum.sales, color = "Actuals")) +
  geom_line(aes(y = preds, color = "Predicted")) +
  scale_color_manual(values = c("Actuals" = "blue", "Predicted" = "red"))


