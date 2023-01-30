#Tyler Turner

#problem 1 and 2

rm(list = ls())
myT <- read.table("nc101_scaff_dataCounts.txt",header=TRUE,row.names=1)

numCols <- ncol(myT)

myColClasses <- c("character", rep("numeric", numCols))
myTAsNum <-read.table("nc101_scaff_dataCounts.txt",sep="\t",header=TRUE,colClasses=myColClasses)

#problem 3

myTNorm <- myTAsNum

for ( i in 2:ncol(myTAsNum))
{
  colSum = sum(myTNorm[,i])
  myTNorm[,i] = myTNorm[,i]/colSum
}


t.test( myTNorm[ 1,2:4], myTNorm[ 1, 8:12] )$p.value

myPvals2_12 <- vector(mode = "numeric")
myPvals2_20 <- vector(mode = "numeric")
myPvals12_20 <- vector(mode = "numeric")

i <- 1
while( i <= nrow(myT)){
  
  temp <- t.test( myTNorm[ i,2:4 ], myTNorm[ i, 5:7] )$p.value
  myPvals2_12 <- append(myPvals2_12, temp)
  
  temp2 <- t.test( myTNorm[ i,2:4 ], myTNorm[ i, 8:12] )$p.value
  myPvals2_20 <- append(myPvals2_20, temp2)
  
  temp3 <- t.test( myTNorm[ i,5:7 ], myTNorm[ i, 8:12] )$p.value
  myPvals12_20 <- append(myPvals12_20, temp3)
  
  i <- i+1
  
}

hist(myPvals2_12)
hist(myPvals2_20)
hist(myPvals12_20)

# none of the pvalues are uniform
# the two which seem to have the most significant difference is the first histogram of week 2 vs 12
# and the second histogram which is week 2 vs 20

adjusted2_12 <- p.adjust( myPvals2_12[!is.nan(myPvals2_12)], method="BH")

adjusted2_20 <- p.adjust( myPvals2_20[!is.nan(myPvals2_20)], method="BH")

adjusted12_20 <- p.adjust( myPvals12_20[!is.nan(myPvals12_20)], method="BH")

#total the number of false positives within the adjusted comparisons

sum1 <- sum(adjusted2_12 <= 0.1)

sum2 <- sum(adjusted2_20 <= 0.1)

sum3 <- sum(adjusted12_20 <= 0.1)
