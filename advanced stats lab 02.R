#Tyler Turner
 

#problem 1
x <- 0:30

dbinom(12, 30, 1/3)
plot(x, dbinom(x, 30, 1/3))

myMean <- 30 * 1/3
#mean is 10

myVar <- (30*(1/3)) * (2/3) 
#variance is 6.66667

  
#problem 2
y <- 0:100

plot(y, dbinom(y,100, .4))
dbinom(47,100,.4)

#problem 2B
binom.test(53,100,.4, alternative = ("two.sided"))


#problem 2C
binom.test(53,100, 0.4, alternative = "greater")

sum(dbinom(53:100, 100, .4))


#problem 3
#problem 3a


myValue <- rbinom(1000,10000, 0.5)

#problem 3b
meanTwo <- 10000 *.5
meanTwo
#the mean is 5000

varTwo <- (10000*0.5)*(0.5)
varTwo
#the variance is 2500

#problem 3c
pvaluesVector <- vector(mode = "double", length = 1000)
for(i in 1:length(myValue)){
  pvaluesVector[i] <- binom.test(myValue[i], 10000,0.5)$p.value
}

hist(pvaluesVector)

#a uniform distribution would be expected with a 50% success ratte but it does not quite follow that 
# with only 1000 experiments run


#problem 3d

myValue <- rbinom(1000,10000, 0.65)

pvaluesVector <- vector(mode = "double", length = 1000)
for(i in 1:length(myValue)){
  pvaluesVector[i] <- binom.test(myValue[i], 10000,0.65)$p.value
}

hist(pvaluesVector)

# you could roughly expect the shape of the histogram to be similar to the shape of 50%
# the spread of the histogram only changes slightly when you alter the probabilities probably due 
# to the very large sample size