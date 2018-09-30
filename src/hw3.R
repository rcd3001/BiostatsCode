#RDuggan 9/27/18
#W3 - Hw3

rm(list = ls())
library(tidyverse)

#generating sample proportion
sampDistFun <- function(sampSize){
  (rbinom(5000, sampSize, 0.425))/sampSize
  }


#Q1
#1a) n = 20, pi = 0.425
#
#Calculating Pr(P<0.325 | P>0.525)
  #Method 1:
sampDist20 <- sampDistFun(20)
sampProp1 <-((sum(sampDist20 < 0.325 | sampDist20 > 0.525))/5000)

  #(produces same answer w/ CI)
q1aAnswer <- binom.test(x = as.integer(sampProp1 * 20), n = 20, p = 0.425, conf.level = 0.9)


#It would be surprising to get an estimate that is off by more than 0.10. 
#This is shown by the fact that the confidence interval calculated for a sample size of 20 (0.177 - 0.558) contains the population mean
#(#has a .9 probability of containing the population mean.????)


#1b) n = 100,  pi = 0.425
sampDist100 <- sampDistFun(100)
sampProp2 <- ((sum(sampDist100 < 0.325 | sampDist100 > 0.525))/5000)

#The sample proportion when n = 100 is 0.043

#1c) n = 500
sampDist500 <- sampDistFun(500)
sampProp3 <- ((sum(sampDist500 < 0.325 | sampDist500 > 0.525))/5000)

#sample proportion is 0 - extremely unlikely if not impossible to oberserve a value more than 0.1 more extreme than the population proportion

#1d) samples 20, 100, 50 at 0.05 alpha
sampPropTest <- vector()
for(i in c(20,100, 500)){
  sampDistTemp <- sampDistFun(i)
  sampPropTemp <- ((sum(sampDistTemp < 0.375 | sampDistTemp > 0.475))/5000)
  sampPropTest <- cbind(sampPropTest, sampPropTemp)
}
colnames(sampPropTest) <- c("n = 20", "n = 100", "n = 500")

#The sample proportion found in this case is 0.6446, which is significantly more extreme than the population proportion...

#1e) #The sample proportion found in this case is 0.3914, which is within the .05 interval (explain this!!!)

#1f) 0.0256 --- very unlikely to observe?

#1g) eXPLAIN
#-----

#2a Maximum likelihood function - n = 222, Y = 98? (P = 98/222 = 0.441)
MLEfun <- function(y, n, p){
  sum(dbinom(x = y, size = n, prob = p, log = T))
}
loglike1 <- MLEfun(c(0:222), 222, 0.441)
plot(dbinom(c(0:222), size = 222, prob = 0.441, log = T)) #plotting logarithmic distribution

#2b
MLEfunSimp <- function(p){
  sum(dbinom(x = c(0:222), size = 222, prob = p, log = T))
}
MLE2b <- optimize(MLEfunSimp, interval = c(0,1), maximum = T) #optimizing log likelihood = 0.5?

#2c
loglike2 <- MLEfun(c(0:444), 444, .441) #proportion doesn't change, but LLE does

#2d 
MLEfunSimp2 <- function(p){
  sum(dbinom(x = c(0:444), size = 444, prob = p, log = T))
}
MLE2d <- optimize(MLEfunSimp, interval = c(0,1), maximum = T) #doesn't change????


#3 - CI for HPV (pi = 0.425) --- determining confidence intervals 

#3a - 3c
CIgenFun <- function(sampSize){
  binom.test(x = as.integer(0.425 * sampSize), n = sampSize, p = 0.425, conf.level = 0.99)
}
Q3confint <- vector("list", 3) 
Q3confint <- sapply(c(20, 100, 500), CIgenFun)

#3a 
Q3confint[,1]$conf.int[1:2] 
#Calculating the CI width:
Q3confint[,1]$conf.int[2] - Q3confint[,1]$conf.int[1] 

#3b
Q3confint[,2]$conf.int[1:2]
#Calculating the CI width:
Q3confint[,2]$conf.int[2] - Q3confint[,2]$conf.int[1] 

#3c
Q3confint[,3]$conf.int[1:2]
#Calculating the CI width:
Q3confint[,3]$conf.int[2] - Q3confint[,3]$conf.int[1] 

#3D - the widths decrease as sample size increases - as the sample size quintuples, the width of the CI decreases at the same rate (~0.444), from 0.555 to 0.259 to 0.115

#----

#4 - hypothesis testing for HPA - function w/ variable population proportion, outputting sample proportion
#Assuming a default 0.95 CI
Q4fun <- function(pi){
  binom.test(x = as.integer(123 * pi), n = 123, p = pi)
}

Q4samples <- sapply(c(0.405, 0.445, 0.385, 0.465, 0.365, 0.485), Q4fun)
#calculating sample proportions using the binomial tests conducted above
for(i in seq(1:6)){
  print(paste0(round(as.numeric(Q4samples[,i][1]) / as.numeric(Q4samples[,i][2]), 3), 
               "  (Sample proportion for pi = ", as.numeric(Q4samples[,i][6]), ")"))
  print(paste0(round(as.numeric(Q4samples[,i][3]), 3), 
               "  (p-value for pi = ", as.numeric(Q4samples[,i][6]), ")"))
}

