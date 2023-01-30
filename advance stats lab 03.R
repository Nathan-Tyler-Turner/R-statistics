#tyler turner
#advanced stats lab 03


#problem 1A
probs <- seq(0,1,0.01)

plot(probs, dbeta(probs, 6,6), col = "green")

points(probs, dbeta(probs, 1,1), col = "blue")


#problem 1B

probs <- seq(0,1,0.01)
plot(probs, dbeta(probs, 401,401), col = "purple")
points(probs, dbeta(probs, 406,406), col = "blue", pch = 2)

points(probs, dbeta(probs, 2,2), col = "firebrick")
points(probs, dbeta(probs, 7,7), col = "forestgreen")

#the two plots over 400 are similar because of the large number of occurances of heads and tails in that case
#the two plots with the small number of heads and tails differ because with a low instance of heads and tails
#the prior still holds some significance due to its close relation to the data


#problem 2A and 2B

rm(list = ls())


piOld <- 0.5
plot(dexp(piOld, rate =5) / 0.9932621)
numIterations <- 100000
posteiorDist <- vector()

for( i in 1:numIterations )
{
  
  # our prior with 9 heads and 9 tails
  # our new data with 14 heads and 10 tails
  pOld <- dexp(piOld, rate =5) / 0.9932621 * dbinom( 14, 24, piOld )
  
  piNew <- piOld + rnorm(1, 0, sd =0.01);
  
  if( piNew > 1) 
    piNew = 1;
  
  if( piNew < 0 ) 
    piNew =0;
  
  pNew <- dexp(piNew, rate =5) / 0.9932621 * dbinom( 14, 24, piNew )
  
  ratio <- pNew / pOld
  
  if( ratio > 1 || ratio >= runif(1) ) 
    piOld = piNew;
  
  posteiorDist[i] = piOld;	
  
  if( i %% 1000 == 0 )
  {	
    myHist <- hist(posteiorDist,breaks=200,plot=FALSE)
    plot( myHist$mids, myHist$counts/i, main = paste("iteration", i), ylim = c(0,0.05))
    dbetasum = sum(dbeta(myHist$mids, 10+40, 10+40))
    lines( myHist$mids, dbeta(myHist$mids, 10+40, 10+40)/ dbetasum, col = "red", lwd = 3)
    Sys.sleep(.1)
  }
}



posteriorDist <- vector()


i <- 1;
sum <- 0;
for( x in myHist$mids )
{
  # our prior with 9 heads and 9 tails
  # our new data with 14 heads and 10 tails
  posteriorDist[i] <- dexp(x, rate =5) / 0.9932621 * dbinom( 14, 24, x)
  sum = sum + posteriorDist[i];
  i <- i + 1;	
}


lines( myHist$mids, posteriorDist/sum,col="green") 


#problem 2C


rm(list = ls())
x <- seq(0,1,0.01)

plot(dexp(x, rate =5) / 0.9932621)

piOld <- 0.5

numIterations <- 100000
posteiorDist <- vector()

for( i in 1:numIterations )
{
  
  # our prior with 9 heads and 9 tails
  # our new data with 583 heads and 417 tails
  pOld <- dexp(piOld, rate =5) / 0.9932621 * dbinom( 583, 1000, piOld )
  
  piNew <- piOld + rnorm(1, 0, sd =0.01);
  
  if( piNew > 1) 
    piNew = 1;
  
  if( piNew < 0 ) 
    piNew =0;
  
  pNew <- dexp(piNew, rate =5) / 0.9932621 * dbinom( 583, 1000, piNew )
  
  ratio <- pNew / pOld
  
  if( ratio > 1 || ratio >= runif(1) ) 
    piOld = piNew;
  
  posteiorDist[i] = piOld;	
  
  if( i %% 1000 ==0  )
  {	
    myHist <- hist(posteiorDist,breaks=200,plot=FALSE)
    plot( myHist$mids, myHist$counts/i, main = paste("iteration", i), ylim = c(0,0.05)) 
    dbetasum = sum(dbeta(myHist$mids, 583+40, 417+40))
    lines( myHist$mids, dbeta(myHist$mids, 583+40, 417+40)/dbetasum,col="red") 	
    Sys.sleep(.1)
  }
}



posteriorDist <- vector()


i <- 1;
sum <- 0;
for( x in myHist$mids )
{
  # our prior with 9 heads and 9 tails
  # our new data with 583 heads and 427 tails
  posteriorDist[i] <- dexp(x, rate =5) / 0.9932621 * dbinom( 583, 1000, x)
  sum = sum + posteriorDist[i];
  i <- i + 1;	
}


lines( myHist$mids, posteriorDist/sum,col="green") 



#These plots are different from the ones in 2B because the number of flips went from 24 to 1000
#this large number of flips will make the prior have less of an impact