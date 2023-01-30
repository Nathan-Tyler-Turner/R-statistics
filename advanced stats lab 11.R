#Tyler Turner

rm(list = ls())

#part 1

myT <- read.table("familyPivotedTaxaAsColumnsNotNormalized.txt",header=TRUE,row.names=1, sep="\t")

rowSums <- apply( myT, 1, sum)

avgPerSample <- mean(rowSums)

for( i in 1:nrow(myT))
{
  rowSum = sum(myT[i,])
  myT[i,] = avgPerSample *  myT[i,] / rowSum
}

myT <- round(myT)


#part 2

lmpVals <- vector()
zeroPvals <- vector()
bug <- vector()
caseControl <- ifelse( grepl("case", row.names(myT)), "case", "control")

index <- 1

for(i in 1:ncol(myT)){
  
  if(sum(myT[,i] == 0) > 1){
     bug <- log(myT[,i]+1)
     linearModel <- lm(bug~caseControl)
     lmpVals[index] <- anova(linearModel)$"Pr(>F)"[1]
     index <- index +1
  }
     
}

hist(lmpVals)


library(lmtest)
library(pscl)

index <- 1



for(i in 1:ncol(myT)){
  
  if(sum(myT[,i] == 0) >1){
    bug <- (myT[,i])
    myFrame <- data.frame(bug, caseControl)
    myTest <- zeroinfl(bug~caseControl |caseControl, data = myFrame, dist = "negbin", link = "logit")
    zeroPvals[index] <- lrtest(myTest)$"Pr(>Chisq)"[2]
    
  }
  
  else
    zeroPvals[index] <- -1
  
  index <- index +1
}

hist(zeroPvals)


plot(log(lmpVals), log(zeroPvals))

#the p-values do seem to have some similarity between them

sum(p.adjust(zeroPvals, method = "BH") <0.1)

sum(p.adjust(lmpVals, method = "BH") <0.1)

#the linear model produces 18 different pvalues under 10% FDR, while the ZINB produces only 13


#part 2 B

AIC(linearModel)

AIC(myTest)

#The AIC score for the linear model is 262.99, and the AIC score for the ZINB is 434.22


#part 2 C

 

myT <- read.table("familyPivotedTaxaAsColumnsNotNormalized.txt",header=TRUE,row.names=1, sep="\t")


someCol <- 1
pdf("someFile.pdf")

for(i in 1:length(myT)){
caseControl <- ifelse( grepl("case", row.names(myT)), "case", "control")
bug <- log(myT[,someCol]+0.001)
boxplot( bug ~ caseControl,main=paste(names(myT)[someCol],"p-values", zeroPvals[someCol]))
myFrame <- data.frame(bug, caseControl)
stripchart(bug~ caseControl, data = myFrame,vertical = TRUE, pch = 21, add=TRUE )
someCol = someCol +1
}

dev.off()

#based on the boxplots of these pvalues, the models which are better differ depending on the data
#in some cases the Pvalues are significantly lower yet the boxplots are almost identical
#in other cases there will be low or high pvalues with drastically different boxplot spreads.
