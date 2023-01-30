#1
mydata <- c(9,3,10,200,1,2,21,720,1500,60,
            30,30,480,900,2,1,5400,1500,480,3,
            3900,10800,28900,3600,200,120,500,600,1980,160,
            5,9,20,180,15,20,2,17,30,8)

groupA <-c(9,3,10,200,1,2,21,720,1500,60)
groupB <-c(30,30,480,900,2,1,5400,1500,480,3)
groupC <-c(3900,10800,28900,3600,200,120,500,600,1980,160)
groupD <-c(5,9,20,180,15,20,2,17,30,8)

groups <- c(rep("treament A", 10), rep("treatment B", 10), rep("treatment C", 10), rep("treatment D", 10))

matingtimes <- data.frame(mydata, groups)

bartlett.test(mydata,groups)


kruskal.test(list(groupA,groupB,groupC,groupD))

#2
mydata <- c(0.518,0.523,0.495,0.502,0.525,0.49,
            0.318,0.342,0.301,0.39,0.327,0.32,
            0.393,0.415,0.351,0.39,0.385,0.397)

groupA <- c(0.518, 0.523, 0.495, 0.502, 0.525, 0.49)
groupB <- c(0.318, 0.342, 0.301, 0.39, 0.327, 0.32)
groupC <- c(0.393, 0.415, 0.351, 0.39, 0.385, 0.397)

var(groupA)
var(groupB)
var(groupC)

mean(groupA)
mean(groupB)
mean(groupC)

groups <- c(rep("Pond", 6), rep("DI", 6), rep("Sodium", 6))

clams <- data.frame(mydata, groups)

bartlett.test(mydata, groups)

results <- aov(mydata ~ groups, data = clams)

results
summary(results)

