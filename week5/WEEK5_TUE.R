
### knn example 

# load libs
library(class)

# train/test sampling
set.seed(123) # set seed in training
sampleIndex = sample(1:150, 100) # 1~100 to train, 50 to test

train <- iris[sampleIndex, ]
test <- iris[-sampleIndex, ]

train.ds <- train[ , -5] 
train.cl <- as.factor(train[ , 5]) 
test.ds <- test[, -5]
test.cl <- as.factor(test[, 5])

# classfication / prediction
knn.pred <- knn(train = train.ds, 
                test = test.ds,
                cl = train.cl,
                k = 3)

print(knn.pred)
print(test.cl)

# evalutation : accuracy
accuracy <-  mean(knn.pred == test.cl)
print(accuracy)

# confusion matrix
p = rbind(test.cl, knn.pred)
xtabs( ~ test.cl + knn.pred, data = p)


# plotting 
par(mfrow = c(1,2))
plot(test.ds$Petal.Length,
     test.ds$Petal.Width,
     col = test.cl,
     main = "real Species")
plot(test.ds$Petal.Length,
     test.ds$Petal.Width,
     col = knn.pred,
     main = "predicted species")



###  knn project

# load libs
library(class)

# get csv data (gold, matchinfo)
gold <- read.csv("C:/Users/DKU/Desktop/cpt1_knn/gold.csv")
#View(gold)
matchInfo <- read.csv("C:/Users/DKU/Desktop/cpt1_knn/matchinfo.csv")
#View(matchInfo)


# merge 2 data file in 1 data
data <- merge(gold, matchInfo, by = "Address", all = FALSE)
#View(data)


# data preprocessing with key "golddiff"
data <- subset(data, Type.x == "golddiff")
data <- data[,c(3:19, 103)]
View(data)


# train/test sampling
set.seed(123) 
sampleIndex = sample(1:7620, 5000) 

train <- data[sampleIndex, ]
test <- data[-sampleIndex, ]

View(train)
View(test)

train.ds <- train[ , -18]
train.cl <- train[ , 18]
test.ds <- test[ , -18]
test.cl <- test[ , 18]


# classfication / prediction
knn.pred <- knn(train = train.ds, 
                test = test.ds,
                cl = train.cl,
                k = 10)

print(knn.pred)
print(test.cl)


# evalutation : accuracy
accuracy <-  mean(knn.pred == test.cl)
print(accuracy)


# confusion matrix
p = rbind(test.cl, knn.pred)
xtabs( ~ test.cl + knn.pred, data = p)

# plotting 
par(mfrow = c(1,2))
plot(test.ds$min_17,
     test.ds$min_17,
     col = test.cl,
     main = "real game result")
plot(test.ds$min_17,
     test.ds$min_17,
     col = knn.pred,
     main = "predicted game result")

install.packages("caret")
library(caret)

