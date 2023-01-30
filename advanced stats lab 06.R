#Tyler Turner


#problem 1


source("http://bioconductor.org/biocLite.R")
biocLite("DESeq")
library("DESeq")


#problem 2

rm(list=ls())

numRows = 3000
numCols = 10
for( i in 1:numCols)
  
  myFrame <- data.frame(1:numRows)

#initiate the data.frame with the correct # of rows to suppress error messages.
#likely, there are much better ways to do this!
names(myFrame)[1] <- "tempColumn"

for( i in 1: numCols)
{
  vals <- vector(length=numRows)
  
  for( j in 1:numRows)
  {
    aMean = j /10
    aMean = max( aMean,5)
    aVar = aMean+ 5* aMean 
    aVal = round( max( rnorm(1,mean=aMean,sd=sqrt(aVar)), 1))
    vals[j] = aVal
  }
  
  colName <- paste( "sample" , i ,sep="")
  
  myFrame[[colName]] = vals
}

myFrame["tempColumn"] <- NULL
row.names(myFrame) <- paste("Gene_",1:numRows,sep="")


#problem 3

conditions <- factor(c(rep("a", 5), rep("b", 5)))


cds <- newCountDataSet(myFrame,conditions)

cds <- estimateSizeFactors(cds)

sizeFactors(cds)

cds <- estimateDispersions(cds)

nbinomTest(cds, "a", "b") 

#problem 3a

means <- apply(counts(cds,normalized=TRUE), 1,mean)
myInfo <- fitInfo( cds )

# DeSeq's estimate of dispersion calculated for each gene
plot(means, means * means* myInfo$perGeneDispEsts)

# the fit that DeSeq uses for the pooled "dispersion"
lines(means, means*means* myInfo$dispFunc(means),col="RED")

# the dispersion for each gene that DeSeq actually uses for inference
points(means, means * means* fData(cds)[,1], col="YELLOW")


#The yellow dots represent the extra raw variance which deseq provides from the 
#negative binomial, it represents the most conservative variance to represent the 
#overall data, compared to the red line which is the variance provided by the 
#fit line which is equal to 5 times the mean


hist(nbinomTest(cds, "a", "b")$pval)

#the pvalues are not entirely uniform as they are slightly increasing as you go right
#but they are close to uniform
#the data analysis path is conservative because the pvalues are higher than what we would 
#expect with this data set
#there are 0 genes that are significantly different at a 10% FDR since all the adjusted pvalues
#are 1, and the histogram has a steady increase from left to right to represent this

#problem 4

cds <- newCountDataSet(myFrame,conditions)

cds <- estimateSizeFactors(cds)

cds <- estimateDispersions(cds, sharingMode = "gene-est-only")

nbinomTest(cds, "a", "b") 

means <- apply(counts(cds,normalized=TRUE), 1,mean)
myInfo <- fitInfo( cds )

# DeSeq's estimate of dispersion calculated for each gene
plot(means, means * means* myInfo$perGeneDispEsts)

# the fit that DeSeq uses for the pooled "dispersion"
lines(means, means*means* myInfo$dispFunc(means),col="RED")

# the dispersion for each gene that DeSeq actually uses for inference
points(means, means * means* fData(cds)[,1], col="YELLOW")

hist(nbinomTest(cds, "a", "b")$pval)

sum1 <- sum(nbinomTest(cds, "a", "b")$padj <= 0.1)

#the pvalues are closer to uniform in the second "gene-est-only" test 
#all of these pvalues are false positives in this test because it is under conserving everything
#there are 24 genes that are significantly different at a FDR f 10%
# this test is far less conservative than the default test

#problem 5

i <- 0

test <- vector(mode = "numeric")

for (i in 1:nrow(myFrame)){

 temp <- t.test( myFrame[i,1:5], myFrame[i,6:10] )$p.value 
 test <- append(test, temp) 
}

hist(test)

#this test is very close to uniformly distributed
#the pvalues in this test are right in the middle as far as conservation is concerned
#compared to the first and second test's level of conservation
#the first test is the most conservative between these 3 tests
# this last test is the closest to uniform out of the 3 test