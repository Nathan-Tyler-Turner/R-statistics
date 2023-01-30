#Tyler Turner

rm(list = ls())

library("nlme")


#part 1

myData <- read.table("prePostPhylum.txt", header = TRUE, sep = "\t")

myT <- myData[myData$time == "POST",]

par(mfrow =c(3,2))

myPvals <- vector()

myRho <- vector()

for(i in 5:length(myT)){

bug <- myT[,i]
cage <- myT$cage
genotype <- myT$genotype


myFrame <- data.frame(bug, cage, genotype)
plot(  myFrame$bug ~ myFrame$cage, main = names(myT)[i])
stripchart(bug ~ cage, data = myFrame,vertical = TRUE, pch = 21, add=TRUE)
M.gls <- gls( bug~ genotype , method = "REML", correlation = corCompSymm( form = ~ 1 | cage),data=myFrame)
print(summary(M.gls))
myPvals[i-4] <- anova(M.gls)$"p-value"[2]

myRho[i-4] <-(coef(M.gls$modelStruct[1]$corStruct,unconstrained=FALSE)[[1]])
}

#there does appear to be a clear cage effect with select phyla, some more than others

myPvals

myRho

adjusted_p <- sum(p.adjust(myPvals, method = "BH") <0.1)

adjusted_p

#there are 3 phyla that are statistically significant for genotype at a 10% false discovery rate
