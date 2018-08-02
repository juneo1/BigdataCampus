# R에서 결측치 처리 

# 1. 결측치 표기: NA
r <- data.frame(score=c(15,23,13,24,NA,17,NA))

#2. 결측치 확인
is.na(r)

#3. 결측치 빈도 출력
table(is.na(r))

#4. 평균출력
mean(r)

#5. 결측치가 한 개라도 있으면 제거
r.nomiss <- na.omit(r)

#6. 결측치 제외하고 평균 구하기
mean(r$score, na.rm = T)

#7. 결측치 값을 평균으로 대체
m <- mean(r$score, na.rm = T)
r$score <- ifelse(is.na(r$score), m, r$score)
r





### 7월 30일 실습문제 (교차타당성)

# Readind Data
library(tidyverse)
library(caret)
library(MASS) 
data(Cars93) 
str(Cars93)
nrow(Cars93)

# Model1
model1 <- lm(Price ~ Type + AirBags + Cylinders + Man.trans.avail, data=Cars93) 

# Model2
model2 <- lm(Price ~ Type + AirBags + Cylinders, data=Cars93) 


## Method1: The Validation set Approach (훈련용:검증용 = 80:20)
set.seed(123)
training.samples <- Cars93$Price %>%
  createDataPartition(p = 0.8, list = FALSE) #80%
train.data  <- Cars93[training.samples, ] #80% in training set
test.data <- Cars93[-training.samples, ] #extra in test set

# Build the model: Compare two models
model1 <- lm(Price ~ Type + AirBags + Cylinders + Man.trans.avail, data = train.data) 
model2 <- lm(Price ~ Type + AirBags + Cylinders, data = train.data) 

# Make predictions and compute the R2, RMSE and MAE
Model1predictions <- model1 %>% predict(test.data)
Model2predictions <- model2 %>% predict(test.data)
data.frame( R2_Model1 = R2(Model1predictions, test.data$Price),
            RMSE_Model1 = RMSE(Model1predictions, test.data$Price),
            MAE_Model1 = MAE(Model1predictions, test.data$Price),
            R2_Model2 = R2(Model2predictions, test.data$Price),
            RMSE_Model2 = RMSE(Model2predictions, test.data$Price),
            MAE_Model2 = MAE(Model2predictions, test.data$Price))


## Method2: Leave one out cross validation - LOOCV
# Define training control
train.control <- trainControl(method = "LOOCV")

# Train the model
Model1 <- train(Price ~ Type + AirBags + Cylinders + Man.trans.avail, data = Cars93, method = "lm",
                trControl = train.control)
Model2 <- train(Price ~ Type + AirBags + Cylinders, data = Cars93, method = "lm",
                trControl = train.control)

# Summarize the results
print(Model1)
print(Model2)


## Method3: 10-fold cross-validation
# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 5)

# Train the model
Model1 <- train(Price ~ Type + AirBags + Cylinders + Man.trans.avail, data = Cars93, method = "lm",
                trControl = train.control)
Model2 <- train(Price ~ Type + AirBags + Cylinders, data = Cars93, method = "lm",
                trControl = train.control)
# Summarize the results
print(Model1)
print(Model2)


## Method4: Repeated 10-fold cross-validation (3회)
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 5, repeats = 3)
# Train the model
Model1 <- train(Price ~ Type + AirBags + Cylinders + Man.trans.avail, data = Cars93, method = "lm",
                trControl = train.control)
Model2 <- train(Price ~ Type + AirBags + Cylinders, data = Cars93, method = "lm",
                trControl = train.control)
# Summarize the results
print(Model1)
print(Model2)