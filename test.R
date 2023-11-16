install.packages('fpp3')
library(fpp3)

train_Raw <- train_Raw %>% group_by(date,store_nbr,family) %>% summarise(onpromotion = sum(onpromotion),sales = sum(sales)) %>% ungroup()

#train_Raw <- train_Raw %>% filter(onpromotion != 0)

train_Raw$date <- as.Date(train_Raw$date)

train_Raw <- train_Raw %>%
  mutate(Dayofweek = weekdays(train_Raw$date),
         Monthofyear = months(train_Raw$date),
         Year = year(train_Raw$date),
         YrMonth = format(train_Raw$date, "%Y-%m"))

#setting months to factors with ordered levels
train_Raw$Monthofyear <- factor(train_Raw$Monthofyear)

#day of week to factor as well
train_Raw$Dayofweek <- factor(train_Raw$Dayofweek)

holiday <- holidays_events_Raw %>%
  filter(year(date) >= 2013, locale == 'National', transferred == "False", type != 'Work Day') %>%
  select(c(date, type))
holiday <- holiday[!duplicated(holiday$date), ]

holiday$date <- as.Date(holiday$date)

training <- train_Raw %>%
  left_join(holiday, by = 'date') %>%
  mutate(
    day_type = case_when(
      month(date) == 12 & day(date) == 25 ~ 'Closed',
      is.na(type) == FALSE ~ 'Holiday',
      wday(date) %in% 2:6 ~ "Weekday",
      TRUE ~ "Weekend")   
  ) %>%
  select(-type)

oil_Raw$date <- as.Date(oil_Raw$date)

test_Raw$date <- as.Date(test_Raw$date)

training <- bind_rows(training,test_Raw)

training <- training %>%
  mutate(Dayofweek = weekdays(training$date),
         Monthofyear = months(training$date),
         Year = year(training$date),
         YrMonth = format(training$date, "%Y-%m"))

training <- training %>%
  left_join(oil_Raw, by = 'date') %>%
  rename(oil_price = dcoilwtico) %>%
  tidyr::fill(oil_price, .direction = "up")

transactions_Raw$date <- as.Date(transactions_Raw$date)

transactions_Raw <- transactions_Raw %>% mutate(date_store = paste0(date,"_",store_nbr))

training <- training %>% mutate(date_store = paste0(date,"_",store_nbr))


training <- training %>% group_by(family,store_nbr) %>% mutate(lag_14 = lag(sales,16)) %>% ungroup
training <- training %>% group_by(family,store_nbr) %>% mutate(lag_21 = lag(sales,21)) %>% ungroup
training <- training %>% group_by(family,store_nbr) %>% mutate(lag_30 = lag(sales,30)) %>% ungroup

training$day <- as.numeric(format(training$date, format = "%d"))
training$payment.day <- ifelse(day(training$date) == days_in_month(training$date), 1, 0)
training$payment.day <- ifelse(training$day == 15, 1, training$payment.day)
training$payment.day <- as.factor(training$payment.day)


#test <- training %>% filter(date >= "2017-07-31")

#test$Year <- as.factor(test$Year)

#training_final <- training %>% filter(date >= "2016-04-01" & date <"2017-07-31")

#training_final$store_nbr <- as.factor(training_final$store_nbr)

#training_final$day_type <- as.factor(training_final$day_type)

#training_final$Year <- as.factor(training_final$Year)

#trans.model <- lm(transactions ~ onpromotion + store_nbr.x , data = training_final)
#summary(trans.model)

#test$trans.preds <- predict(trans.model, newdata = test)
#act <- test$transactions
#rmsle(act, test$trans.preds)

#training_final$sales <- round(training_final$sales,0)

#training_final$trans.preds <- predict(trans.model, newdata = training_final)

#sales.model <- lm(sales ~  onpromotion + Monthofyear + lag_14 + lag_21  + oil_price + payment.day , data = training_final)
#summary(sales.model)



#test$sales.preds <- predict(sales.model, newdata = test)
#test$sales.preds[test$sales.preds < 0] <- 0
#test$sales.preds[is.na(test$sales.preds)] <- 0
#act <- test$sales
#rmsle(act, test$sales.preds)


test_final <- training %>% filter(date >= "2017-08-16")

training <- training %>% filter( date <"2017-08-16")

final_model <- lm(sales ~  onpromotion + Monthofyear + lag_14 + lag_21  + oil_price, data = training)
summary(final_model)


test_final$sales.preds <- predict(final_model, newdata = test_final)
test_final$sales.preds[test_final$sales.preds < 0] <- 0
test_final$sales.preds[is.na(test_final$sales.preds)] <- 0


sumbission_final <- test_final %>% select(id,sales.preds)
sumbission_final <- sumbission_final %>% rename(sales = sales.preds)

sumbission_final$sales[sumbission_final$sales == 0] <- 0.001
# Export the modified dataframe to CSV
write.csv(sumbission_final, "output_file.csv", row.names = FALSE)
