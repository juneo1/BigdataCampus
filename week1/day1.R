25+99
456-123
2*(3+4)
(3+5*6)/7
(7-4) *3
2^10 + 3^5
1256 %% 7
184 %% 5
1976 / 24
16*25 + 186*5 - 67*22

v1 <- 10 ; class(v1)
v1
v1 <- "Good Moring"
class(v1)


10 * 10 * 3.14
15 * 15 * 3.14
20 * 20 * 3.14

PI <- 3.14
R <- 10
R * R * PI

x <- 6
y <- 2*x^2 + 5*x + 10
y



x <- c(1,2,3)
v2 <- c(1,2,5, 50:90)
v2
class(v2)


v3 <- seq(1, 101, 3)
v3

score <-  c(90, 85, 70)
names(score) <- c("John","Tom", "Jane")
score


d <- c(1, 4, 3, 7, 8, 9)
d[-2]
d[-c(3:5)]


GNP <- c(2090, 2450, 960)
name(GNP) <-  c("Korea", "Japan", "Nepal")
GNP[1]
GNP["Korea"]

d <- c(101:200)

d[10]

d[90:100]

d <- seq(102, 200, 2)
d

d.20 <- d[1:20]
d.20

d.20[-5]


d <- c(1, 4, 3, 7, 8)
2*d
d-5
3*d + 4


x <-  c(1,2,3)
y <-  c(4,5,6)
x+y
z <- x+y
z

d <-  c(1,2,3,4,5,6,7,8,9)
d >= 5 
d[d>5]
sum(d>5)
sum(d[d>5])
d==5

condi <- d >5 & d < 8
d[condi]


history()

d1 <- 1:50
d2 <- 51:100

d1

d2

d1+d2
d2-d1
d1*d2
d2/d1

sum(d1)
sum(d2)

min(d1)
max(d1)

min(d2)
max(d1)

mean(d1)
mean(d2)
mean <- mean(d2)-mean(d1)
mean

sum(sum(d1)+sum(d2))

d1 <- 1:50
d1 <- sort(d1, decreasing = TRUE)
d1 <- d1[1:10]

d2 <- 51:100
d2 <- sort(d2, decreasing = TRUE)
d2 <- d2[1:10]

d3 <- c(d1, d2)
d3



v1 <- 51:90

v1 < 60
v1 < 70
v1 > 65
v1 > 60 & v1 < 73
v1 < 65 | v1 < 80

v1 <- 51:90
condi <- v1 %% 7 == 3
v1[condi]


sum(v1 %% 2 == 0)

sum()