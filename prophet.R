#library(feather)
library(generics)
library(openxlsx) 
library(ggplot2)
library(tidyr)
library(readxl)
library(dplyr)
library(janitor)
library(tidyverse)
library(str2str)
library(conflicted)
library(rlang)
library(SmartEDA)
library(bbplot)
library(ggthemes)
library(corrplot)
library(timeSeries)
library(tseries)
library(forecast)
library(Metrics)
library(mgcv)
library(prophet)
library(caret)
conflict_prefer('select','dplyr','MASS')
conflict_prefer('filter','dplyr','MASS')
conflict_prefer('filter','dplyr','stats')
conflicts_prefer(dplyr::filter)

#load data
train_Raw <- read.csv("data/train.csv")
transactions_Raw <- read.csv("data/transactions.csv")
oil_Raw <- read.csv("data/oil.csv")
test_Raw <- read.csv("data/test.csv")

#tolower and space cleaning for column names and content
train_cleaned <- train_Raw
test_cleaned <- test_Raw
transactions_cleaned <- transactions_Raw
oil_cleaned <- oil_Raw


#clean the data
colnames(train_cleaned) <- tolower(colnames(train_cleaned)) %>% gsub(" |_", ".",.)
colnames(test_cleaned) <- tolower(colnames(test_cleaned)) %>% gsub(" |_", ".",.)
colnames(transactions_cleaned) <- tolower(colnames(transactions_cleaned)) %>% gsub(" |_", ".",.)
colnames(oil_cleaned) <- tolower(colnames(oil_cleaned)) %>% gsub(" |_", ".",.)

#summarise the sales
train <- train_cleaned %>% group_by(date, family, store.nbr) %>% 
  summarise(sum.sales = sum(sales), 
            sum.promo = sum(onpromotion),
            store_family= paste0(store.nbr, "_", family)) %>%
  ungroup()

train$date <- as.Date(train$date)

#subset train and test
# Select the last 14 days for test, and from april 2016 for subset_train
test <- train %>%
  filter(date >= max(date) - 13)
subset_train <- train %>% filter(date >= "2016-04-01")


#define year, month, and day variables for test and subset_train
test$year <- as.numeric(format(test$date, format = "%Y"))
test$month <- as.numeric(format(test$date, format = "%m"))
test$day <- as.numeric(format(test$date, format = "%d"))
test$day_of_week <- weekdays(test$date)
test$last_day_of_month <- ifelse(day(test$date) == days_in_month(test$date), 1, 0)
test$last_day_of_month <- ifelse(test$day == 15, 1, test$last_day_of_month)
test$month_day_combined <- paste0(month(test$date), "-", 
                                  day(test$date))


subset_train$year <- as.numeric(format(subset_train$date, format = "%Y"))
subset_train$month <- as.numeric(format(subset_train$date, format = "%m"))
subset_train$day <- as.numeric(format(subset_train$date, format = "%d"))
subset_train$day_of_week <- weekdays(subset_train$date)
subset_train$last_day_of_month <- ifelse(day(subset_train$date) == days_in_month(subset_train$date), 1, 0)
subset_train$last_day_of_month <- ifelse(subset_train$day == 15, 1, subset_train$last_day_of_month)
subset_train$month_day_combined <- paste0(month(subset_train$date), "-", 
                                          day(subset_train$date))

#just to be sure, put date as.Date
subset_train$date <- subset_train$date %>% as.Date()
subset_train <- subset_train %>% distinct()

test$date <- test$date %>% as.Date()
test <- test %>% distinct()





##################### PREDICTING OIL PRICE AND TRANSACTION ##################

oil <- oil_cleaned %>% distinct()
oil <- oil %>% rename(oil.price = dcoilwtico)

transactions <- transactions_cleaned %>% group_by(date) %>% summarise(sum.trans = sum(transactions))

oil$date <- oil$date %>% as.Date()
transactions$date <- transactions$date %>% as.Date()

#merge oil data with the train
subset_train <- left_join(subset_train, oil, by="date") %>% left_join(transactions, by="date")
subset_train <- subset_train %>% distinct()

#copy the value oil.price of friday on the next saturday and sunday (which have NAs) for train
subset_train <- subset_train %>%  
  group_by(grp = cumsum(day_of_week == "venerdì")) %>%  
  mutate(oil.price = ifelse(day_of_week %in% c("sabato", "domenica"), oil.price[day_of_week == "venerdì"], oil.price)) %>%  
  ungroup() %>%  select(-grp)

#merge oil data with test in order to compare actual and predicted oil.prices later
test <- left_join(test, oil, by="date") %>% left_join(transactions, by="date")
test <- test %>% distinct()

#copy the value oil.price of friday on the next saturday and sunday (which have NAs) for test
test <- test %>%  
  group_by(grp = cumsum(day_of_week == "venerdì")) %>%  
  mutate(oil.price = ifelse(day_of_week %in% c("sabato", "domenica"), oil.price[day_of_week == "venerdì"], oil.price)) %>%  
  ungroup() %>%  select(-grp)

subset_train$year <- as.factor(subset_train$year)
subset_train$month <- as.factor(subset_train$month)
subset_train$day <- as.factor(subset_train$day)

test$year <- as.factor(test$year)
test$month <- as.factor(test$month)
test$day <- as.factor(test$day)
test <- test %>% distinct()


#create the oil model
oil.model <- lm(oil.price ~ year + month, data = subset_train)
summary(oil.model)

#predict test oil.price
test$oil.preds <- predict(oil.model, newdata = test)
act <- test$oil.price
rmsle(act, test$oil.preds) #0.02

#red= predicted, blue= actuals
ggplot(test, aes(x = date)) +
  geom_line(aes(y = oil.price, color = "Actuals")) +
  geom_line(aes(y = oil.preds, color = "Predicted")) +
  scale_color_manual(values = c("Actuals" = "blue", "Predicted" = "red"))


subset_train_oil <- subset_train %>% mutate(oil.preds = oil.price)
subset_train_oil <- subset_train_oil %>% mutate(trans.preds = sum.trans)

#create the transaction model
trans.model <- lm(sum.trans ~ year + month + day_of_week + sum.promo +
                    oil.preds, data = subset_train_oil)
summary(trans.model)

#predict test transaction
test$trans.preds <- predict(trans.model, newdata = test)
act <- test$sum.trans
rmsle(act, test$trans.preds) #0.057

#red= predicted, blue= actuals
ggplot(test, aes(x = date)) +
  geom_line(aes(y = sum.trans, color = "Actuals")) +
  geom_line(aes(y = trans.preds, color = "Predicted")) +
  scale_color_manual(values = c("Actuals" = "blue", "Predicted" = "red"))


#to avoid confusion in test, we create copies of oil and transactions calling them preds
subset_train$oil.preds <- subset_train$oil.price
subset_train$trans.preds <- subset_train$sum.trans





##################### BENCHMARK PROPHET ##########################

#function to run prophet for each family of product
#column= the column with families, data= the dataframe
apply_model <- function(column, data) {
  results_list <- list()
  for (value in unique(column)) {
    data_filtered <- filter(data, store_family == value)
    df <- data_filtered
    df <- df %>% mutate(ds= date, y= sum.sales)
    df <- column_to_rownames(df, var = "date")
    m <- prophet(df)
    future <- make_future_dataframe(m, periods = 14)
    forecast <- predict(m, future)
    plot(m, forecast)
    forecast <- tail(forecast, 14)
    data_filtered_test <- filter(test, store_family == value)
    act <- data_filtered_test$sum.sales
    rm <- rmsle(forecast$yhat, act)
    print(paste0("################## ",value, " ##################"))
    cat("rmsle:", rm, "\n")
    results <- list(
      value = value,
      rmsle = rm
    )
    results_list[[value]] <- results
    
  }
  return(results_list)
}

#the result gives us a list with the family and the corresponding rmsle
messaiah <- apply_model(subset_train$store_family, subset_train)

#turn the list into a dataframe for later comparison
result_messaiah <- do.call(rbind, messaiah) %>% as.data.frame()

