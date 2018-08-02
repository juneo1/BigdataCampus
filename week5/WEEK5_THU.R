
### DecisionTree
install.packages("C50")
install.packages("party")
install.packages("rpart")


library(C50)
library(party)
library(rpart)

# data sampling
idx = sample(1:nrow(iris), 100)
train = iris[idx, ]
test = iris[-idx, ]

train.ds = train[ ,-5]
train.cl = train[ , 5]
test.ds = test[ ,-5]
test.cl = test[ , 5]

## modeling : C50
c50.model = C5.0(x = train.ds, y = train.cl)
plot(c50.model)

# prediction
c50.pred = predict(c50.model, test.ds)

# accuracy
mean(c50.pred == test.cl)



## modeling : party
party.model = ctree(Species ~., data = train)
plot(party.model)

# prediction
party.pred = predict(party.model, test.ds)

# accuracy
mean(party.pred == test.cl)


## modeling : rpart
rpart.model = rpart(Species ~., data = train, cp = 0.01) # cp 파라미터를 통해 가지치기를 설정(default = 0.01)
plot(rpart.model) # 뼈대만 그려주고
text(rpart.model, cex = 0.7) # 글씨를 추가한다. cex는 글자크기

# prediction
rpart.pred = predict(rpart.model, test.ds)

# acurracy
mean(rpart.pred == test.cl)


# 모델 = 모델링함수(트레인x, 트레인y)
# 예측값 = predict(모델, 테스트x)


### Random Forest

install.packages("randomForest")
library(randomForest)

rf.model = randomForest(x = train.ds, y = train.cl)
plot(rf.model)

# prediction
rf.pred = predict(rf.model, test.ds)
rf.pred
test.cl

# accuracy
mean(rf.pred == test.cl)



### Project

setwd("C:/Users/DKU/Desktop/DecisionTree")
data <- read.csv("health.csv")
#data[, 6] = as.factor(data[, 6])
data = data[c(40000:50000) ,-6]
#data = data[c(1:10000),-6]
str(data)

# data sampling
idx = sample(1:nrow(data),7000)
train = data[idx, ]
test = data[-idx, ]
head(train)

train.ds = train[ ,-7]
train.cl = train[ ,7]
test.ds = test[, -7]
test.cl = test[, 7]


# decision Tree Modeling 

head(train)
c50.model = C5.0(x = train.ds, y = train.cl)
plot(c50.model)

?C5.0



## modeling : party
party.model = ctree(RES ~., data = train)
plot(party.model)

# prediction
party.pred = predict(party.model, test.ds)

# accuracy
mean(party.pred == test.cl)




# Random Forest Modeling

rf.model = randomForest(x = train.ds, y=train.cl)
plot(rf.model)

rf.pred = predict(rf.model, test.ds)
rf.pred

# accuracy
mean(rf.pred == test.cl)







#
ds = read.csv("data.csv")
dim(ds)
head(ds)
sub.idx = sample(1:1000000, 7000)

sub.ds = ds[sub.idx, ]

for(i in 1:7000) {
  if(sub.ds$DIS[i] %in% c(1,2)) {
    sub.ds$DIS[i] = 1
  } else {
    sub.ds$DIS[i] = 0
  }
}

idx = sample (1:7000, 5000)

train.ds = sub.ds[idx, ]
test = sub.ds[-idx, ]
train.ds = train[, -6]
train.cl = as.factor(train[, 6])
test.ds = test[, -6]
test.cl = as.factor(test[, 6])

c50.model = C5.0(train.ds, train.cl)
c50.pred = predict(c50.model, test.ds)
cat(C50, model$tree)
mean(c50.pred == test.cl)
plot(c50.model)

library(class)
knn.model = knn(train =train.ds, test = test.ds, cl = train.cl)
mean(knn.model == test.cl)

library(randomForest)
rf.model = randomForest(x = train.ds, y=train.cl)
rf.pred = predict(rf.model, test.ds)
mean(rf.pred == test.cl)