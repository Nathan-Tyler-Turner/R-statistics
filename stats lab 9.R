#1
survey <- matrix(c(351,41,254,154), ncol = 2, byrow = TRUE)
survey <- as.table(survey)
survey
prop.test(survey)

#2
pchisq(.47002, 3, lower.tail=F)
observed <- c(315,101,108,32)
expected_f <- c(0.5625, 0.1875, 0.1875, 0.0625)
chisq.test(observed, p = expected_f)

#3
pchisq(5.333, 2, lower.tail = FALSE)
plant <- rbind(c(30,10), c(20,20), c(50,30))
chisq.test(plant)

#4
mutation <- rbind(c(6,3), c(2,9))
fisher.test(mutation)
