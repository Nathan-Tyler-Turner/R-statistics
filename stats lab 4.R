x <- read.table("mosFish.txt", header = TRUE, sep = "\t")
x

y <- x[1:40, ]
y

popSD <- 5.82
sample_mean <-mean(y)

#1
qnorm(0.25, 21.67, 5.82)
upper <- sample_mean - qnorm(0.25) * 5.82/sqrt(40)
upper

lower <- sample_mean + qnorm(0.25) * 5.82/sqrt(40)
lower



#2
qnorm(.05) * 5.82/sqrt(40)
upper <- sample_mean - qnorm(.05) * 5.82/sqrt(40)
upper

lower<- sample_mean + qnorm(.05) * 5.82/sqrt(40)
lower

#3
qnorm(.025) *5.82/sqrt(40)
upper <- sample_mean - qnorm(.025) *5.82/sqrt(40)
upper

lower <- sample_mean + qnorm(.025) *5.82/sqrt(40)
lower

#the width increases as the confidence intervals increase

#5
qqnorm(y)
qqline(y)
hist(y)
#6 slightly skewed to the right

#part B
#1
pnorm(4.5, 11.5, 2.491987)

#2
pnorm(-1.343)

#3
pnorm(2.93114) - pnorm(-1.5553)


#part C
#1
pnorm(-1.86611)

#2
pnorm(1.92937) - pnorm(-2.4986)

#part E
#1
pnorm(133, 120, 9.380305) - pnorm(110, 120, 9.380305)

#2
pnorm(133, 120, 7.658987) - pnorm(110, 120, 7.658987)

#part F
#1
iguana <- c(1450, 1550, 2200, 1500, 1650, 2000, 2435, 1550, 1050, 2300, 2000, 2750,1800, 2850)
x <- mean(iguana)
s <- sd(iguana)


upper <- x + qt(0.975,13)*(s/sqrt(14))
upper

lower <- x - qt(0.975,13)*(s/sqrt(14))
lower

#2
lower <- x + qt(.10, 13)*(s/sqrt(14))
lower

upper <- x - qt(.10, 13)*(s/sqrt(14))
upper

#3
z <- c()
y <- iguana
for(n in iguana){
  for(m in y){
    z <- c(z, m+n/2)
  }
}

hist(z)
