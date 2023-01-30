#A

#part 1

Distance <- c(10,20,50,100,200,500,1000)
Radiation <- c(158,132,104,82,57,22,11)

mydata <- data.frame(Distance, Radiation)

mydata

radModel <- lm(mydata, mydata)

radModel

summary(radModel)


#part 5

plot(radModel)

plot(log(mydata))

mydata2 <- (data.frame(log(mydata)))

testdata <- data.frame(mydata, mydata2)

testdata

multiMydata <- lm(Distance~Radiation+Distance.1+Radiation.1, testdata)

summary(multiMydata)


#problem B

#part 1

table <- read.table("IQ_scores.txt", header =TRUE)

table

mydata <- data.frame(table)

mydata

multiregres <- lm(IQ~Brain+Height+Weight, mydata)

summary(multiregres)

regres <- lm(IQ~Brain, mydata)

summary(regres)

regres <- lm(IQ~Height, mydata)

summary(regres)


regres <- lm(IQ~Weight, mydata)

summary(regres)
