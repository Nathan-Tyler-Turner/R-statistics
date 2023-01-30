getwd()
mydata <- read.table("AffinityCorrelation.txt", header = TRUE)
mydata <- read.table("AffinityCorrelation1.txt", header = FALSE)
mydata
mydata$results <- sample(c("T","F"), 30, replace = TRUE)
mydata
summary(mydata)
myset1 <- mydata[1:10, ]
myset2 <- mydata[, 1:2]
myset1
myset2
myset3 <- mydata[mydata$experiment < -8, ]
myset3
myset3 <- mydata[with(mydata, experiment < -8 & new_potential < -100), ]
myset3
x <- 0:100
plot(x, dbinom(x, 100, 0.5), type ="h", col = "blue")
plot(x, dbinom(x, 100, 0.3), type = "h")
plot(x, dbinom(x, 100, 0.5))
x <- dbinom(4,6,0.25)
x
y <- pbinom(4,6,0.25, lower.tail = T)
y
y-x
a = dpois(3,12)
a
1 - sum(dpois(0:2, 12))
