#Tyler Turner

rm(list = ls())


#part 1

myData <- read.table("prePostPhylum.txt", header = TRUE, sep = "\t")


myPca <- summary(princomp(myData[,5:10]))

myPca

myVar <- myPca$sdev^2

total <- sum(myVar[1:2] /sum(myVar))

total

#total variance is 0.8599

#part 2
colors <- vector()  

for(i in 1:length(myData$time)){
  
  if(myData$time[i] == "POST")
    colors[i] = "RED"
  
  
  if(myData$time[i] == "PRE")
    colors[i] = "BLUE"
  
}

  
plot(myPca$score[,1], myPca$score[,2], col = colors, pch = 16, main = "Time")

#pca x axis, or score[,1] separates these pca scores


colors2 <- vector()

for(i in 1:length(myData$genotype)){
  
  if(myData$genotype[i] == "WT")
    colors2[i] = "PURPLE"
  
  if(myData$genotype[i] == "10-/-")
    colors2[i] = "GREEN"
}

plot(myPca$score[,1], myPca$score[,2], col = colors2, pch = 16, main = "Genotype")

#pca y axis, or score[,2] separates these pca scores

#part 3


library("nlme")

cage <- myData$cage
genotype <- myData$genotype
pc1 <- myPca$score[,1]
pc2 <- myPca$score[,2]
time <- myData$time



myFrame <- data.frame(pc1, pc2, cage, genotype, time)

#pc1 with cage vs pc1 without cage

M.gls <- gls(pc1~ genotype + time, method = "REML", correlation = corCompSymm( form = ~ 1 | cage),data=myFrame)

m.glsNocage <- gls(pc1~ genotype + time, method = "REML",data=myFrame)


anova(M.gls, m.glsNocage)

anova(M.gls)

#pc2 with cage vs pc2 without cage

M.gls2 <- gls(pc2~ genotype + time, method = "REML", correlation = corCompSymm( form = ~ 1 | cage),data=myFrame)

m.glsNocage2 <- gls(pc2~ genotype + time, method = "REML",data=myFrame)


anova(M.gls2, m.glsNocage2)


#anova for timepoint and genotype
anova(M.gls2)


#table values| timepoint | genotype | cage | 

         #pc1| <.0001    | 0.9644   | 0.2774
         #pc2| 0.4713    | <.0001   | 0.2308

