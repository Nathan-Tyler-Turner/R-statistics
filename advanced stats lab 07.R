#Tyler Turner


rm(list = ls())


myData <- read.table("stats7.txt", header = TRUE)

plot(myData)

myLm <-lm(myData$ListPrice~myData$BestPrice)

anova(myLm)

summary(myLm)


#p value is 2.2e-16
#R squared value 0.9971


#part 2

Data <- read.table("caseControlData.txt", header = TRUE, sep = "\t")
BMI <- read.table("BMI_Data.txt", header = TRUE, sep = "\t")

#code provided by Dr. Fodor
key <- sub("case", "", Data$sample)
key <- sub("control", "", key)


pvals <- vector()

for(i in 1:length(key)){
  
  test <- key[i]
  #more code provided by Dr. Fodor, removes information from suffix
  testKey <- strsplit(test, "_")[[1]][1]
  pvals[i] <- testKey
  
}

Data$sample <- pvals

#combine the tables

combined_Table <- merge(BMI, Data, by.x= 'studyid', by.y= 'sample')

pvals2 <- vector()

for(i in 3: length(combined_Table)){
  
  myLm <- lm(combined_Table$bmi ~ combined_Table[[i]])
  pvals2[i] <- anova(myLm)$"Pr(>F)"
  
}

hist(pvals2)
plot(pvals2)

#The pvalues do appear to be getting close to a uniform distribution
#There doesnt seem to be any relation between BMI and the microbial community, which would make 
#sense considering bacteria doesn't have BMI


adjusted_p <- sum(p.adjust(pvals2[!is.na(pvals2)], method = "BH") <0.1)


# there are no associations significant at 10% FDR
