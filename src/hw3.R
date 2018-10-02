#RDuggan 9/27/18
#W3 - Hw3

rm(list = ls())
library(tidyverse)

#generating sample proportion

sampDistFun <- function(sampSize){
  set.seed(42)
  (rbinom(5000, sampSize, 0.425))/sampSize
  }


#Q1
#1a) n = 20, pi = 0.425
#
#Calculating Pr(P<0.325 | P>0.525)

sampDist20 <- sampDistFun(20)
sampProp1 <-((sum(sampDist20 < 0.325 | sampDist20 > 0.525))/5000)

#It would not be very surprising to get an estimate that differs from the population proportion by more than 0.10 with a sample size of 20. This is shown by the fact that the probability of getting an estimate that differs from the population proportion by more tha 0.10 is 0.377.


#1b) n = 100,  pi = 0.425
sampDist100 <- sampDistFun(100)
sampProp2 <- ((sum(sampDist100 < 0.325 | sampDist100 > 0.525))/5000)


#It would be relatively suprising to get an estimate that differs from the population proportion more than 0.10 with a sample size of 100. The above calculation indicates that the probability of obtaining an estimate that falls outside of the 0.10 range is 0.0378, or 3.78 %.


#1c) n = 500
sampDist500 <- sampDistFun(500)
sampProp3 <- ((sum(sampDist500 < 0.325 | sampDist500 > 0.525))/5000)

#With a sample size of 500, the probability of getting a sample proportion more than 0.10 more extreme than the population proportion is 0. It is thus impossible that this would occur.

#1d) #samples 20, 100, 50 at 0.05 alpha
sampPropTest <- vector()
for(i in c(20,100, 500)){
  sampDistTemp <- sampDistFun(i)
  sampPropTemp <- ((sum(sampDistTemp < 0.375 | sampDistTemp > 0.475))/5000)
  sampPropTest <- cbind(sampPropTest, sampPropTemp)
}
colnames(sampPropTest) <- c("n = 20", "n = 100", "n = 500")

print(sampPropTest)

#1D
#Based on these results, it would not be surprising to get an estimate that differs from 0.425 by more than 0.05 with a sample size of 20, as the probability that this would occur is 0.6596. 

#1e) Based on these results, it would be somewhat surprising, but not entirely unexpected, to get a sample proportion that differs from 0.425 by more than 0.05 with a sample size of 20, as the probability that this would occur is 0.3094

#1f) It would be very unlikely that an estimate of the true proportion would differ by more than 0.05 with a sample size of 500; as the probability of this occuring is 0.0256, or in 2.56% of cases.

#1g) In both the cases of an interval of 0.10 and of 0.05, there is association of increasing sample sizes and decreasing probability of obtaining a more extreme estimate. It appears that, as the sample size increases, the variability of the sampling distribution becomes smaller, the range around which the proportions are distributed becomes narrower, and the chance of observing a more extreme value decreases.
#-----

#2a Maximum likelihood function - n = 222, Y = 98? (P = 98/222 = 0.441)
MLEfun <- function(y, n, p){
  prod(dbinom(x = y, size = n, prob = p, log = F))
}
likeli1 <- MLEfun(98, 222, 0.441)
plot(seq(0.1, 0.9, length.out = 223), dbinom(c(0:222), size = 222, prob = 0.441, log = F), xlab = "Estimated Population Parameter", ylab = "Likelihood", main = "Likelihood Function of the Population Parameter") #plotting non-logarithmic distribution

#2b
MLEfunSimp <- function(p){
  prod(dbinom(x = 98, size = 222, prob = p, log = F))
}
MLE2b <- optimize(MLEfunSimp, interval = seq(0,1, by = 0.01), maximum = T)

#2c
likeli2 <- MLEfun(196, 444, .441) #proportion doesn't change, but LLE does

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
#Q3confint[,1]$conf.int[1:2] 


#3b
#Q3confint[,2]$conf.int[1:2]
#The width of the exact 99% confidence interval with a sample size of 100 when pi = 0.425 is:
Q3confint[,2]$conf.int[2] - Q3confint[,2]$conf.int[1] %>%
  round(3)

#3c
#Q3confint[,3]$conf.int[1:2]
#The width of the exact 99% confidence interval with a sample size of 500 when pi = 0.425 is:
Q3confint[,3]$conf.int[2] - Q3confint[,3]$conf.int[1] %>%
  round(3) 

#3D - The widths of the respective exact 99% confidence intervals decrease as sample size increases - as the sample size quintuples, the width of the CI decreases at the same rate (~0.444), from 0.555 to 0.259 to 0.115.

#----
############ 
#4 - hypothesis testing for HPA - function w/ variable population proportion, outputting sample proportion
#Assuming a default 0.95 CI
Q4fun <- function(pi){
  binom.test(x = as.integer(123 * pi), n = 123, p = 0.425)
}

Q4samples <- sapply(c(0.405, 0.445, 0.385, 0.465, 0.365, 0.485), Q4fun)
#calculating sample proportions using the binomial tests conducted above
for(i in seq(1:6)){
  print(paste0(round(as.numeric(Q4samples[,i][1]) / as.numeric(Q4samples[,i][2]), 3), 
               "  (Sample proportion for pi = ", as.numeric(Q4samples[,i][6]), ")"))
  print(paste0(round(as.numeric(Q4samples[,i][3]), 3), 
               "  (p-value for pi = ", as.numeric(Q4samples[,i][6]), ")"))
}
###################################################################

##Question 4, alternative approach with random sampling (not very different from above)

sampPropFun <- function(pi){
  mean(rbinom(1, 123, as.numeric(pi))/123)
}

Q4SampProps <- sapply(c(0.405, 0.445, 0.385, 0.465, 0.365, 0.485), sampPropFun)

#Running binomial tests with assumed 0.95 CI
Q4binomsPval <- function(prop){
  binom.test(x = as.integer(prop * 123), n = 123, p = 0.425)$p.value
}
Q4estimate <- function(prop){
  as.numeric(binom.test(x = as.integer(prop * 123), n = 123, p = 0.425)$estimate)
}

Q4SamplesMethod2 <- rbind(sapply(c(Q4SampProps), Q4binomsPval)) %>%
  round(3)
Q4SamplesMethod2 <- rbind(Q4SamplesMethod2, sapply(c(Q4SampProps), Q4estimate)) %>%
  round(3)
colnames(Q4SamplesMethod2) <- c(0.405, 0.445, 0.385, 0.465, 0.365, 0.485) %>%
  round(3)
row.names(Q4SamplesMethod2) <- c("P-value", "Estimate of sample prop")
print(Q4SamplesMethod2)


#4g - ?????


#5 -- pi = 0.25 (approximating normal distribution X ~ N(mu, var)
meanQ5 <- (1000*0.25)
sdQ5 <- sqrt(meanQ5*0.75)

#5a - Pr(X>300) 

pnorm(300, mean = meanQ5, sd = sdQ5, lower.tail = F) %>%
  round(5)
pbinom(300, 1000, prob = 0.25, lower.tail = F) %>%
  round(5)

#The probabilities calculated from a normal approximation to the binomial (0.00013) and from an exact binomial test (0.00015) are very similar, only differing by .00002.

#5b Pr(237 < X <= 245) -- can do 1 less because this a normal approximation of a binomial??
(pnorm(245, mean = meanQ5, sd = sdQ5) - pnorm(237, mean = meanQ5, sd = sdQ5)) %>%
  round(5)
(pbinom(245, 1000, prob = 0.25) - pbinom(237, 1000, prob = 0.25))%>%
  round(3)


#As in Question 5a, the two methods of calculating the estimated probability produce very similar results: the probability from normal approximation of the binomial is 0.167, while the probability from performing an exact binomial test is 0.172.

#5c and #5d
#Both 5D and 5E are using sample proportions that, when converted according to the equation P = Y/n, would give the same answers as 5a and 5b, respectively. 

#5e - Pr(X<6)
pnorm(5, mean = 6, sd = sqrt(4.5)) %>%
  round(3)
pbinom(5, size = 24, prob = 0.25) %>%
  round(3)

 
#In this case, there is a greater different in the probabilities obtained from the normal approximation (0.319) and the exact binomial test (0.422).

#5f -- Pr(5 <= X <= 9)
(pnorm(9, mean = 6, sd = sqrt(4.5)) - pnorm(4, mean = 6, sd = sqrt(4.5))) %>%
  round(3)
pbinom(9, 24, prob = 0.25) - pbinom(4, 24, 0.25) %>%
  round(3)

#Similarly to 5e, the lower sample size reduced the accuracy of the normal approximation of the binomial, which produced a probability of 0.603, compared to the exact binomial test result of 0.523.

#5g ---- higher sample sizes yielded more accurate results using normal approximations, indicating that it is less appropriate to use this method in situations with lower samples sizes.

#6 -- CI interval data analysis

crc <- read.csv(file = '../data/crc.csv')

#number proportion of Nodes Examined >= 12 /450
crc %>%
select(Nodes) %>%
  filter(Nodes >= 12) %>%
  count() %>%
  as.numeric() %>%
  binom.test(x = ., n = 450, p = (./450))


#The confidence interval for the 2016-2017 period is 0.587 - 0.678. For the hospital to be in compliance with the 12-node standard, the 95% confidence interval would have to contain the population proportion, 0.75. However, because the upper bound of the confidence interval does not contain 0.75, the hospital is in a state of noncompliance for the period in question.
#However, breaking down the analysis by year (below) reveals that the number of successes (in terms of cases where 12 or more lymph nodes were examined) increased year-to-year, and the upper bound of the confidence interval became closer to 0.75. This suggests the potential beginning of a trend towards compliance.

#Running analyses by year
crc %>%
  select(Nodes, Year) %>%
  filter(Nodes >= 12 & Year == "2016") %>%
  count() %>%
  as.numeric() %>%
  binom.test(x = ., n = 225, p = (./225))

crc %>%
  select(Nodes, Year) %>%
  filter(Nodes >= 12 & Year == "2017") %>%
  count() %>%
  as.numeric() %>%
  binom.test(x = ., n = 225, p = (./225))


#7 Hypothesis testing 

breastCancerOutcomes <- read.csv(file = '../data/BreastCancerOutcomes.csv')
# null 
# H0: pi = 0.15
# Ha: pi != 0.15

#7a)
Y <- breastCancerOutcomes %>%
  select(reoperation) %>%
  filter(reoperation == "yes") %>%
  count() %>%
  as.numeric()

hypoTest7a <- binom.test(Y, n = 700, p = 0.15, conf.level = 0.95)
#Based on the hypothesis test conducted above, there is sufficient evidence to reject the null hypothesis at an alpha of 0.01 (p = 0.000696). Furthermore, the reoperation rate at this hospital appears to be significantly higher than at peer institutions - this can be seen in the 95% confidence interval (0.168 - 0.229). Since the lower bound of the confidence interval is greater than the population proportion, it is much more likely that the sample proportion is greater than the population proportion.


#7b)

#Breaking down the analysis into two groups based on the size of tumors reveals differences the relationship between the reoperation rates at Hopevale Hospital and its peer institutions. The exact binomial test revealed that reoperation rates for smaller tumors was not significant at alpha = 0.05 (p = 0.0885), with a 95% confidence interval of 0.0861 - 0.156.
#However, performing the same analysis on the subset of reoperation patients with larger tumors shows a significantly higher reoperation rate compared to peer institutions at the 0.05 confidence level, with a p-value of 7.718e-10 and a confidence interval of 0.233-0.331. Since the lower bound of the confidence interval is greater than the population proportionm it can be inferred that the reoperation rate for patients with larger tumors at Hopevalue Hospital is significantly greater than at other hospitals.

n1 <- breastCancerOutcomes %>%
  filter(size == "less than 2") 
Y1 <- n1 %>% 
  filter(reoperation == "yes") %>%
  count() %>%
  as.numeric()

#Exact Binomial tests for size < 2 cm (Y = 42 and n = 357)
binom.test(x = Y1, n = 357, p = 0.15)

n2 <- breastCancerOutcomes %>%
  filter(size == "2 or greater") 
Y2 <- n2 %>%
  filter(reoperation == "yes") %>%
  count() %>%
  as.numeric()



#exact binomial tests for size >= 2 cm (Y = 96, n = 343)
binom.test(x = Y2, n = 343, p= 0.15)