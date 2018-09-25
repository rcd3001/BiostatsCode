#RDuggan 9/23/18
#W3L1 - Activty 5

rm(list = ls())

library(tidyverse)

#Function to find binomial confidence intervals - the "Wilson method"
ciLimits<- function(y, n, alpha)
{
  fLow <- function(p){pbinom(y-1,n,p) - (1-alpha/2)}
  fUpp <- function(p){pbinom(y,n,p) - alpha/2}
  pLow <- uniroot(fLow,c(.01,.99))
  pUpp = uniroot(fUpp,c(.01,.99))
  return(c(pLow$root, pUpp$root))
}

#This function is the same as the binom.test package
#in R stats - binom.test produces a list df!#
#Can specify one-sided alternative hypothesis

answer2 <- binom.test(x = c(4, 11), p = 0.267, conf.level = 0.9)

#2.1 - Simulation setup
# pop proportion (pi) = 0.24
# sample proportion P = Y / 405
n <- 621
pi <- 0.36
alpha <- 0.05

#2.2 - calculation of proportion
propTest <- function(n, pi, alpha){
counter <- 0
for(i in seq_len(1000)) {
  y <- rbinom(1, n, pi)
  lower <- binom.test(y, n, conf.level = 1-alpha)$conf.int[1]
  upper <- binom.test(y, n, conf.level = 1-alpha)$conf.int[2]
  if(pi > lower & pi < upper) {
    counter <- counter + 1
  }
}
answer <- counter/1000
return(answer)
}
#Q1 The proportion of the simulated confidence intervals
#that contained 0.24 is equal to the following:
print(answer)

#Q2: The value is 0.026 less than 0.95

#Q3 - setting alpha to 0.1 and running 2.2 again 
#produces a proportion of 0.92
# - this is expected - but why?

#Q4 - using the generated function propTest(621, 0.24, 0.05)
#produces the following:
propTest(621, 0.24, 0.05)
#In this case, 0.961 - expected

#Q5
propTest(621, 0.36, 0.05)
# 0.954 - expected

#2.3 - variability of CIs between samples
n <- 405
pi <- 0.24
alpha <- 0.05
plot(x = c(0.15, 0.33), y = c(1, 100), type = "n",
     xlab="pi", ylab="index")
counter <- 0
for(i in seq_len(100)){
  color <- 2
  y <- rbinom(1, n, pi)
  lower <- binom.test(y, n, conf.level = 1-alpha)$conf.int[1]
  upper <- binom.test(y, n, conf.level = 1-alpha)$conf.int[2]
  if(pi > lower & pi < upper)
  {
    counter <- counter + 1
    color <- 1
  }
  segments(lower, i, upper, i, col=color)
}
abline(v = pi, col = "blue", lty = 2, lwd = 2)

# This plot is showing a randomly-generated set of CIs given the above parameters 
# the blue line is the value of the population proportion
# 

#Q7 1- 0.951 = 0.049

#Q8 - the CIs are not the same because they are based on a random sample around a mean,
#and since the mean varies in each random sample, the CI would thus vary along with the mean of the sample


#3.1
# n = 30
# Y = 6
#Q9 - 80% confidence interval = 0.109, 0.325
binom.test(x = c(6, 24), conf.level = .8)$conf.int %>%
  round(3)

#Q10 - 90% CI = 0.091, 0.357
binom.test(x = c(6, 24), conf.level = .9)$conf.int %>%
  round(3)

#Q11 - 99% CI - 0.054, 0.443 
binom.test(x = c(6, 24), conf.level = .99)$conf.int %>%
  round(3)

# Q12 - As CI increases, width increases (higher probability of encompassing higher % of values)

#3.2
#Q13 - point estimate is 0.2 (6/30)
#95% CI = 
binomFun <- function(x1, y1, conf){
  binom.test(x = x1, n = y1, conf.level = conf)%>%
    return()
}

binomFun(6, 30, 0.95)
#CI = 0.0771 - 0.3857

#Q14 - point estimate = 0.2
#binom test at 95 CI = 0.1078 - 0.3233
binomFun(12, 60, 0.95)

#Q15 - n = 120, y = 24
# point estimate = 0.2
binomFun(24, 120, 0.95)
#CI = 0.1325 - 0.2828

#Q16 - CI decreases

#3.3 - n = 150 

#Q17
binomFun(15, 150, 0.95)
#estimate = 0.1
#CI =  0.05705743 - 0.15956789

#Q20 
binomFun(105, 150, 0.95)
#estimate = 0.7
#CI = 0.6198788 - 0.7720300

#Q21
binomFun(75, 150, 0.95)
#estimate = 0.5
#CI = 0.4173574 - 0.5826426


#Q22 - width increases - the most variability in a binomial distribution occurs when pi = 0.5