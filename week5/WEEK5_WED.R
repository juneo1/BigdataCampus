
### K-MEANS Algorithm

# normalize data
iris.scale = scale(iris[,1:4])
iris.scale

# set index
set.seed(123)
idx = sample(1:150, 100)

train = data.frame(iris.scale[idx, ])
test = data.frame(iris.scale[-idx, ])

iris.kmeans = kmeans(train, center = 3, iter.max =  5000)

windows()
par(mfrow = c(1,2))

plot(train$Petal.Length,
     train$Petal.Width,
     col = iris.kmeans$cluster)
points(iris.kmeans$centers[ , "Petal.Length"], 
       iris.kmeans$centers[ , "Petal.Width"],
       pch = 24, col = 1:3)

plot(train$Petal.Length,
     train$Petal.Width, 
     col = as.numeric(iris[idx, "Species"]))


###  DBSCAN Algorithm
install.packages("dbscan")
library(dbscan)

# dbscan default minPts = 5
iris.dbscan = dbscan(train, eps = 0.5)
?dbscan
windows()
plot(train$Petal.Length, train$Petal.Width,
     col = (iris.dbscan$cluster+1))



### Clustering  

setwd("C:/Users/DKU/Desktop/BigdataCampus/cpt2_clstring")
clu1 = read.csv("clustering_1.csv")
clu2 = read.csv("clustering_2.csv")
clu3 = read.csv("clustering_3.csv")
clu4 = read.csv("clustering_4.csv")
clu5 = read.csv("clustering_5.csv")



clu1.kmeans = kmeans(clu1, center = 15, iter.max = 10000)
clu2.kmeans = kmeans(clu2, center = 3, iter.max = 5000)
clu3.kmeans = kmeans(clu3, center = 3, iter.max = 5000)
clu4.kmeans = kmeans(clu4, center = 3, iter.max = 5000)
clu5.kmeans = kmeans(clu5, center = 7, iter.max = 10000)



windows()
par(mfrow = c(1,1))

plot(clu1, col = (clu1.kmeans$cluster))


plot(clu1$X664159,
     clu1$X550946,
     col = clu1.kmeans$cluster)
points(clu1.kmeans$centers[ , "X664159"], 
       clu1.kmeans$centers[ , "X550946"],
       pch = 15, col = (clu1.kmeans$cluster))

plot(clu2$X31.95,
     clu2$X7.95,
     col = clu2.kmeans$cluster)
points(clu2.kmeans$centers[ , "X31.95"], 
       clu2.kmeans$centers[ , "X7.95"],
       pch = 15, col = 1:3)

plot(clu3$X1.85,
     clu3$X27.8,
     col = clu3.kmeans$cluster)
points(clu3.kmeans$centers[ , "X1.85"], 
       clu3.kmeans$centers[ , "X27.8"],
       pch = 15, col = 1:3)

plot(clu4$X11.25,
     clu4$X5.05,
     col = clu4.kmeans$cluster)
points(clu4.kmeans$centers[ , "X11.25"], 
       clu4.kmeans$centers[ , "X5.05"],
       pch = 15, col = 1:3)

plot(clu5$X68.601997,
     clu5$X102.491997,
     col = clu5.kmeans$cluster)
points(clu5.kmeans$centers[ , "X68.601997"], 
       clu5.kmeans$centers[ , "X102.491997"],
       pch = 15, col = 1:7)


### DBSCAN

clu1.dbscan = dbscan(clu1, eps = 20000)
windows()
plot(clu1$X664159,
     clu1$X550946,
     col = clu1.dbscan$cluster+1)

clu2.dbscan = dbscan(clu2, eps = 3)
windows()
plot(clu2$X31.95,
     clu2$X7.95,
     col = clu2.dbscan$cluster+1)


clu3.dbscan = dbscan(clu3, eps = 9)
windows()
plot(clu3$X1.85,
     clu3$X27.8,
     col = clu3.dbscan$cluster+1)


clu4.dbscan = dbscan(clu4, eps = 0.5)
windows()
plot(clu4$X11.25,
     clu4$X5.05,
     col = clu4.dbscan$cluster+1)


clu5.dbscan = dbscan(clu5, eps = 5, minPts = 3)
windows()
plot(clu5$X68.601997,
     clu5$X102.491997,
     col = clu5.dbscan$cluster+1)