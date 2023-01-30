#Tyler Turner


#problem 1

rm(list = ls())
myT <- read.table("nc101_scaff_dataCounts.txt",header=TRUE,row.names=1)


#problem 2
plot(log10(myT$D2_01), log10(myT$D2_02))

lines(x = c(0:10), y = c(0:10), col ="blue", lwd = 3)

#answer goes here looking at linearity 

#problem 3

myT[2,1]

d201 <-sum(c(myT[1:4631,1]))
d202 <-sum(c(myT[1:4631,2]))
seq_count_D2_01 <- c(myT[1,1], d201-myT[1,1])
seq_count_D2_02 <- c(myT[1,2], d202-myT[1,2])

myDataFrame<- data.frame(seq_count_D2_01, seq_count_D2_02)


fisher.test(myDataFrame, alternative = "two.sided")


#problem 4

#issue with using a for loop, had to switch to while loop
#for loop seemed to run once and stop
pval <- vector(mode = "numeric")
i <- 1
while( i <= nrow(myT)){
  
  seqs_count_D2_01 <- c(myT[i,1], d201-myT[i,1])
  seqs_count_D2_02 <- c(myT[i,2], d202-myT[i,2])
  
  
  test <- (seqs_count_D2_01, seqs_count_D2_02)

  x <- fisher.test(test, alternative = "two.sided")
  
  pval <- append(pval, x$p.value)
  i <- i+1
}

hist(pval)

#pvalues are not uniformly distributed, the first and last bars are signifiantly higher than
#the middle, and the middle section is closer to normal than the others but still not exact


myT <- myT[ (myT$D2_01 + myT$D2_02 > 50),]


#same while loop as above with modified myT list of genes
pval <- vector(mode = "numeric")
i <- 1
while( i <= nrow(myT)){
  
  seqs_count_D2_01 <- c(myT[i,1], d201-myT[i,1])
  seqs_count_D2_02 <- c(myT[i,2], d202-myT[i,2])
  
  
  test <- data.frame(seqs_count_D2_01, seqs_count_D2_02)
  
  x <- fisher.test(test, alternative = "two.sided")
  
  pval <- append(pval, x$p.value)
  i <- i+1
}

hist(pval)

# this p value spread is a lot lower overall but apart from the first bar they are all closer
# to being uniformly distributed


#problem 5

#pseudo-count
myT = myT + 1 


d201 <-sum(c(myT[1:4631,1]))
d202 <-sum(c(myT[1:4631,2]))
seq_count_D2_01 <- c(myT[1,1], d201-myT[1,1])
seq_count_D2_02 <- c(myT[1,2], d202-myT[1,2])
myDataFrame<- data.frame(seq_count_D2_01, seq_count_D2_02)
expectedFrequency <- (myT[1,1] / d201)

poisson.test(myT[1,2],T = 1, r = expectedFrequency)



#problem 6

pval_2 <- vector(mode = "numeric")
a <- 1
while( a <= nrow(myT)){
  
  seqs_count_D2_01 <- c(myT[a,1], d201-myT[a,1])
  seqs_count_D2_02 <- c(myT[a,2], d202-myT[a,2])
  
  
  test_2 <- data.frame(seqs_count_D2_01, seqs_count_D2_02)
  
  expectedFrequency <- myT[a,1]/d201
  
  y <- poisson.test(myT[a,2], d202, r = expectedFrequency)
  
  pval_2 <- append(pval_2, y$p.value)
  a <- a+1
}



plot(pval, pval_2)

# there does seem to be a relationship between pval and pval_2 excluding a few outliers
# overall I would say that they agree

#problem 7

# we added a pseudo-count in step 5 to account for zero probability issues. Some of these
# genes have 0 expression in the data, but not necessarily a zero expression rate,
# by adding a 1 to all of the data we can account for these zero probabilities and avoid
# them. If we did not add the 1 to the data, some of the genes would be oversimplified to
# having a zero expression rate, which might not actually be the case, simply what was 
# observed