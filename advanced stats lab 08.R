#tyler turner

rm(list = ls())

#part 1
myData <- read.table("qPCRWithSampleDays.txt", header = TRUE, sep = "\t")

treatments <- factor(myData$treatmentStatus)
treatments <- relevel(treatments, ref = "Treatment")

myLm <- lm(myData$LogBurk ~ myData$sampleDays + treatments, x = TRUE)




colors <- vector()

for(i in 1:length(treatments)){
  
  if(treatments[i] == "Treatment")
    colors[i] = "RED"
  
  if(treatments[i] == "stable")
    colors[i] = "GREEN"
  
  if(treatments[i] == "Recovery")
    colors[i] = "PURPLE"
  
  if(treatments[i] == "Before Treatment")
    colors[i] = "BLUE"
  
}

plot(myData$Log16S~myData$sampleDays, col = colors, pch = 16)


#part2 

#8 parameter test
myLm <- lm(myData$Log16S~myData$sampleDays * myData$treatment)
plot(myLm)
AIC(myLm)

#5 parameter test <- pick this one
myLm <- lm(myData$Log16S~myData$sampleDays + myData$treatment)
plot(myLm)
AIC(myLm)


#2 parameter test
myLm <- lm(myData$Log16S~myData$sampleDays)
plot(myLm)
AIC(myLm)

#I do think that treatment had an effect on the log16s data. At least based on the distributions
#which use the treatment variables as a factor. Both of the AIC values produced when using the 8
#and the 5 parameter produce better results than just the 2 parameter test. That being said
#the difference between the 8 and the 5 parameter test is not very much, 205.79 and 203.79.
#but the 5 parameter test would be the one I would choose to be most appropriate. the 2 parameter
#just doesn't include enough information.