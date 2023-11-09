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
test$mean.sales <- mean(subset_train$sum.sales)

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

n <- holidays_events_cleaned %>%  group_by(date) %>%  summarise(n_festivities = n()) %>%  ungroup()

holidays_locale <- holidays_events_cleaned %>% select(date, locale) %>% distinct()
holidays_locale <- holidays_locale %>% left_join(n, by="date")
holidays_locale$locale <- ifelse(holidays_locale$n > 1, "combined", holidays_locale$locale)

holidays_type <- holidays_events_cleaned %>% select(date, type) %>% distinct()
holidays_type <- holidays_type %>% left_join(n, by="date")
holidays_type$type <- ifelse(holidays_type$n > 1, "combined", holidays_type$type)

holidays_complete <- holidays_type %>% left_join(holidays_locale, by="date", relationship = "many-to-many")

holidays_complete <- holidays_complete %>% distinct()
holidays_complete$month_day_combined <- paste0(month(holidays_complete$date), "-", 
                                               day(holidays_complete$date))
holidays_complete <- holidays_complete %>% select(-c(n_festivities.x, n_festivities.y))

subset_train <- subset_train %>% left_join(holidays_complete, by="month_day_combined")
test <- test %>% left_join(holidays_complete, by="month_day_combined")

no_holiday <- "None"
subset_train$locale <- ifelse(is.na(subset_train$locale), no_holiday, subset_train$locale)
subset_train$type <- ifelse(is.na(subset_train$type), no_holiday, subset_train$type)

test$locale <- ifelse(is.na(test$locale), no_holiday, test$locale)
test$type <- ifelse(is.na(test$type), no_holiday, test$type)

subset_train <- subset_train %>% select(-date.y) 
subset_train <- subset_train %>% rename(date = date.x)
subset_train$date <- subset_train$date %>% as.Date()


test <- test %>% select(-date.y) 
test <- test %>% rename(date = date.x)
test$date <- test$date %>% as.Date()

#create the model
linear.model <- lm(sum.sales ~ sum.promo + day_of_week +
                     type, data = subset_train)
summary(linear.model)

#predict test sales
test$preds <- predict(linear.model, newdata = test)
act <- test$sum.sales
rmsle(act, test$preds) #0.066

ggplot(test, aes(x = date)) +
  geom_line(aes(y = sum.sales, color = "Actuals")) +
  geom_line(aes(y = preds, color = "Predicted")) +
  scale_color_manual(values = c("Actuals" = "blue", "Predicted" = "red"))


######### linear model with oil.price and transaction predictios used as variables #######


oil <- oil_cleaned %>% distinct()
oil <- oil %>% rename(oil.price = dcoilwtico)

transactions <- transactions_cleaned %>% group_by(date) %>% summarise(sum.trans = sum(transactions))

oil_cleaned$date <- oil_cleaned$date %>% as.Date()
transactions_cleaned$date <- transactions_cleaned$date %>% as.Date()

subset_train <- left_join(subset_train, oil, by="date") %>% left_join(transactions, by="date")
subset_train <- subset_train %>% distinct()

#i copy the value oil.price of friday on saturday and sunday
subset_train <- subset_train %>%  
  group_by(grp = cumsum(day_of_week == "venerdì")) %>%  
  mutate(oil.price = ifelse(day_of_week %in% c("sabato", "domenica"), oil.price[day_of_week == "venerdì"], oil.price)) %>%  
  ungroup() %>%  select(-grp)


test <- left_join(test, oil, by="date") %>% left_join(transactions, by="date")
test <- test %>% distinct()

#i copy the value oil.price of friday on saturday and sunday
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


#create the oil model
oil.model <- lm(oil.price ~ year + month, data = subset_train)
summary(oil.model)

#predict test oil.price
test$oil.preds <- predict(oil.model, newdata = test)
act <- test$oil.price
rmsle(act, test$oil.preds) #0.02

ggplot(test, aes(x = date)) +
  geom_line(aes(y = oil.price, color = "Actuals")) +
  geom_line(aes(y = oil.preds, color = "Predicted")) +
  scale_color_manual(values = c("Actuals" = "blue", "Predicted" = "red"))


subset_train_oil <- subset_train %>% mutate(oil.preds = oil.price)
subset_train_oil <- subset_train_oil %>% mutate(trans.preds = sum.trans)

#create the transaction model
trans.model <- lm(sum.trans ~ year + month + day_of_week + sum.promo +
                  type + oil.preds, data = subset_train_oil)
summary(trans.model)

#predict test transaction
test$trans.preds <- predict(trans.model, newdata = test)
act <- test$sum.trans
rmsle(act, test$trans.preds) #0.058

ggplot(test, aes(x = date)) +
  geom_line(aes(y = sum.trans, color = "Actuals")) +
  geom_line(aes(y = trans.preds, color = "Predicted")) +
  scale_color_manual(values = c("Actuals" = "blue", "Predicted" = "red"))


#create the sales model
sales.preds.model <- lm(sum.sales ~ year + day + day_of_week + sum.promo +
                    oil.preds + trans.preds, data = subset_train_oil)
summary(sales.preds.model)

#predict test sales
test$preds <- predict(sales.preds.model, newdata = test)
act <- test$sum.sales
rmsle(act, test$preds) #0.072

ggplot(test, aes(x = date)) +
  geom_line(aes(y = sum.sales, color = "Actuals")) +
  geom_line(aes(y = preds, color = "Predicted")) +
  scale_color_manual(values = c("Actuals" = "blue", "Predicted" = "red"))




################################## GAM #################################

#create the model
kn=3
gam.mod<- gam(sum.sales ~ s(sum.promo, k=kn, bs= 'tp') + day_of_week + year +
                day + type, data=subset_train)

summary(gam.mod)

#predict test sales
test$preds <- predict(gam.mod, newdata = test)
act <- test$sum.sales
rmsle(act, test$preds) #0.075

ggplot(test, aes(x = date)) +
  geom_line(aes(y = sum.sales, color = "Actuals")) +
  geom_line(aes(y = preds, color = "Predicted")) +
  scale_color_manual(values = c("Actuals" = "blue", "Predicted" = "red"))






