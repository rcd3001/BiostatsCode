#RDuggan 9/19/18
#W2L2 - Activity 4

rm(list = ls())

library(tidyverse)

#Binomial distribution, n = 120, pi = 0.36
#probability mass function
plot(dbinom(0:120, 120, 0.36), type = "h", xlab = "Test")

#2.1 -- 
#E(Y) = 120 * .36 = 43.2

#2.2 - estimate of proportion (estimate / sample size)
prob <- (rbinom(1, 120, 0.36)/120)

#2.3 - chance of being off by > 0.05
# would have to calculate Pr (p < 0.36 - .05 or p > 0.41)
# convert using P = Y/n
# Y - n * p
#multiply by 120 (sample size) =
# Pr (Y < 37.2 or Y > 49.2)

#Pr Y < 37.2
prob1 <- pbinom(37.2, 120, prob)

#Pr Y > 49.2
prob2 <- pbinom(49.2, 120, prob, lower.tail = F)

probOutput <- round(sum(prob1 + prob2), 3)

#2.4 - mean of sampling distribution
# E(p) = pi = 0.36

#2.5 - variance of sampling distribution
# = (.36 * .64) / n
# = (.36 * .64) / 120 = 0.00192
# StDev = 0.044