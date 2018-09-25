#Biostats W2.1 - Activity 3
#R Duggan - 9/17/18

rm(list = ls())

library(tidyverse)

#1.1 Px(x) = Pr(X=x) = (n x) * pi^x * (1-pi)^n-x
#1.2
dbinom(175,250, 0.7) %>% 
  round(3)

#1.3 - greater than / equal to (pbinom)
pbinom(174,250, 0.7, lower.tail = F) %>%
  round(3)

#1.4 -  180 < x <= 200 (pbinom)
(pbinom(200, 250, 0.7, lower.tail = T) - pbinom(180, 250, 0.7, lower.tail = T)) %>%
  round(4)
#same answer!
sum(dbinom(181:200, 250, 0.7)) %>%
  round(4)

#1.5 - 
temp <- rbinom(5000, 250, 0.7)
temp %>%
  mean(.) %>%
  round(1)

#1.6 - variance -
var(temp) %>%
  round(1)

#1.7 - standard deviation - using above vector
sd(temp) %>%
  round(2)
#using new random generation
rbinom(5000, 250, 0.7) %>%
  sd(.) %>%
  round(2)

#1.8 variance ~= sd ^2 (variance is average of squared deviations)

#1.9 - probability mass function plot
plot(x = c(0:250), y = dbinom(0:250, 250, 0.7), type = "h")

#1.10 - cumulative distribution function
plot(x = c(0:250), y = pbinom(0:250, 250, 0.7), type = "l")

#2 - normal distribution
#2.2- PDF - using dnorm
pnorm(18, 19, 8) %>%
  round(4)

#2.3 - 21 < X < 26
(pnorm(26, 19, 8) - pnorm(21, 19, 8)) %>%
  round(3)

#2.4 -- 21 <= X <= 26
(pnorm(26, 19, 8) - pnorm(21, 19, 8)) %>%
  round(3)

#2.5 -- rnorm with n trials = 5000
tempNorm <- rnorm(5000, 19, 8)
tempNorm %>%
  mean(.) %>%
  round(1)

#2.6 -- variance of above
var(tempNorm) %>%
  round(1)

#2.7 - SD
sd(tempNorm) %>%
  round(2)

#2.9  - probability density function
PDFseq = seq(0, 37, by = 0.1)
plot(x=PDFseq, dnorm(PDFseq, mean = 19, sd = 8), type = "l")

#2.10 - cumulative density function
plot(x = PDFseq, pnorm(PDFseq, mean = 19, sd = 8), type = "l")