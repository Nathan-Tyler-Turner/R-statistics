#1
qchisq(0.025,11)
qchisq(0.975,11)

#3
qf(.05,3,20, lower.tail = FALSE)
pf(.2065669,3,20, lower.tail = FALSE)

deviceTest <- c(-.307, -.294, .079, .019, -.136, -.324,
                -.176, .125, -.013, .082, .091, .459,
                .137, -.063, .240, -.050, .318, .154,
                -.042, .690, .201, .166, .219, .407)

groups <- c(rep("A",6), rep("B", 6), rep("C",6), rep("D", 6))

phMeters <-data.frame(deviceTest, groups)

results <- aov(deviceTest ~ groups, data=phMeters)

results

summary(results)

TukeyHSD(results)

#4

#mydata <- read.table("sicklebacks.txt", colClasses = c("NULL", NA), header = FALSE)
mydata <- c(31,32,34,34,35,30,33,32,37,33,36,30,32,39,30,29,42,39,37,29,36,30,32,37,35,32,32,37,39,28,32,31,35,40,36,31,32,27,35,31,28,38,31,32,29,38,40,36,43,34,32,39,31,36,28,39,32,38,29,32,47,48,50,42,44,34,41,40,44,47,39,47,43,40,38,32,41,45,42,37,47,37,41,38,32,45,42,40,43,40,39,45,41,39,32,48,32,45,41,38,38,36,48,43,42,31,40,45,42,49,39,30,42,39,38,35,49,40,43,42)

groups <- c(rep("Lake A", 20), rep("Lake B",20), rep("Lake C", 20), rep("Stream A", 20), rep("Stream B", 20), rep("Stream C", 20))

sicklebacks <- data.frame(mydata, groups)

results <- aov(mydata ~ groups, data = sicklebacks)

results

summary(results)

TukeyHSD(results)


