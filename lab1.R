#problem 1
diceMean <- (.1*1 + .1*2 +.1*3 + .1*4 + .1*5 +.5*6)/6

diceMean

diceVar <- (.1*(1-.75)^2) + (.1*(2-.75)^2) + (.1*(3-.75)^2) + (.1*(4-.75)^2) + (.1*(5-.75)^2) + (.5*(6-.75)^2)

diceVar


#problem 2
myRolls <- function(x){
  
  numRolls <- x
  
  
for(i in numRolls){
  
  rolls <-sample(1:6, numRolls, replace = TRUE, prob = c(.1, .1, .1, .1, .1, .5))
}
  
  return(rolls)
  
}

myRolls(1000)

#probem 3
hist(myRolls(10000))
hist(myRolls(100000))
#does not follow a uniform distribution as probabilites are skewed, namely the fact that 6 has a 50%
#chance to be rolled compared to the rest at 10% chances


#problem 4
rm(list = ls())
trialSizes <- c(5,10,15,20,25,30,40,50,100,200,300,400,500,1000,2000,3000,4000,5000,10000,20000,30000,100000)
means <- vector(mode = "double", length = length(trialSizes))
variances <- vector(mode = "double", length = length(trialSizes))

for(i in 1:length(trialSizes)){
  
  rolls <- vector(length = trialSizes[i], mode = "double")
  
  for( j in 1:trialSizes[i])
  {
    rolls[j] <- sample(1:6, 1, prob = c(.1,.1,.1,.1,.1,.5))
  }
  
  means[i] <- mean(rolls)
  
  variances[i] <- var(rolls)
}

plot(log10(trialSizes),means)
lines(log10(trialSizes), rep(3.5,length(trialSizes)))
windows()
plot(log10(trialSizes), variances)
lines(log10(trialSizes), rep(2.916,length(trialSizes)))


