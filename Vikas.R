library(corrplot)
library(dplyr)
dataset_raw <- read.csv("train.csv", header = T)
store <- read.csv("store.csv")
dataset_raw <- merge(dataset_raw,store)

#Data Exploration
str(dataset_raw)
summary(dataset_raw)
# replacing NA's by the median value  
dataset_raw$CompetitionDistance[is.na(dataset_raw$CompetitionDistance)] = round(median(dataset_raw$CompetitionDistance, na.rm = T))
dataset_raw$CompetitionOpenSinceMonth[is.na(dataset_raw$CompetitionOpenSinceMonth)] = round(median(dataset_raw$CompetitionOpenSinceMonth, na.rm = T))
dataset_raw$CompetitionOpenSinceYear[is.na(dataset_raw$CompetitionOpenSinceYear)] = round(median(dataset_raw$CompetitionOpenSinceYear, na.rm = T))
dataset_raw$Promo2SinceWeek[is.na(dataset_raw$Promo2SinceWeek)] = round(median(dataset_raw$Promo2SinceWeek, na.rm = T))

dataset_raw$Promo2SinceYear[is.na(dataset_raw$Promo2SinceYear)] = round(median(dataset_raw$Promo2SinceYear, na.rm = T))
dataset_raw$Open[is.na(dataset_raw$Open)] = round(median(dataset_raw$Open, na.rm = T))

# converting variables to proper formats 
dataset_raw$DayOfWeek = as.factor(dataset_raw$DayOfWeek)
dataset_raw$StateHoliday = as.numeric(dataset_raw$StateHoliday)
dataset_raw$StoreType = as.factor(dataset_raw$StoreType)
dataset_raw$Assortment = as.factor(dataset_raw$Assortment)
dataset_raw$PromoInterval = as.numeric(dataset_raw$PromoInterval)

# seperating out the elements of the date
dataset_raw$Date = as.Date(dataset_raw$Date, format = "%Y-%m-%d")
dataset_raw$month <- as.integer(format(dataset_raw$Date, "%m"))
dataset_raw$year <- as.integer(format(dataset_raw$Date, "%y"))
dataset_raw$day <- as.integer(format(dataset_raw$Date, "%d"))


# Spliting the dataset into training and validation
test<-filter(dataset_raw, Date>='2015-6-15')
train<-filter(dataset_raw, Date<'2015-6-15')

# Finding the correlation between numeric variables
cor_matrix <- cor(dataset_raw[, sapply(dataset_raw, is.numeric)])
corrplot(corr = cor_matrix, order = "hclust")


# Model No.1
model_1 <- lm(Sales ~ ., data = train)
summary(model_1)

# Model No.2

mod <- lm(Sales ~ DayOfWeek
          + Customers
          + Open
          + Promo  
          + SchoolHoliday
          + StoreType 
          + Assortment
          + CompetitionDistance
          + CompetitionOpenSinceMonth, data = train)
summary(mod)
pred_mod <- predict(object = mod, newdata = test)
test$pred_mod <- pred_mod
test[,c('Sales','pred_mod')]


# Let's remove shops which were closed

train_Clean <- filter(train, Sales > 0)
train_Clean$year <- as.factor(train_Clean$year)
train_Clean$month <- as.factor(train_Clean$month)

test_Clean <- filter(test, Sales > 0)
test_Clean$year <- as.factor(test_Clean$year)
test_Clean$month <- as.factor(test_Clean$month)

Model_4 <- lm(Sales ~ Customers
              + Promo  
              + StateHoliday 
              + SchoolHoliday
              + StoreType 
              + Assortment
              + CompetitionDistance
              + CompetitionOpenSinceMonth
              + CompetitionOpenSinceYear
              + Promo2 
              + Promo2SinceWeek 
              + Promo2SinceYear 
              + PromoInterval
              + Date
              + month
              + year, data = train_Clean)
summary(Model_4)
clean_pred_4 <- predict(object = Model_4, newdata = test_Clean)
test_Clean$clean_pred_4 <- clean_pred_4
test_Clean[,c('Sales','clean_pred_4')]

write.csv(test_Clean[,c('Sales','clean_pred_4')], 'Sales_pred_2.csv')
