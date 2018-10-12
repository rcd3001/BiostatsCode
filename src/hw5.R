#####
# RDuggan 10/11/18 - Biostats I HW # 5
############

rm(list = ls())
library('tidyverse')
library('boot')


fish <- read.csv(file = '../data/FishMercury.csv')

#1a) Boxplot of fish data

fishBoxplot <- function(fishData){
  ggplot(data = fishData)+
  geom_boxplot(aes(x= FishID, y = Mercury))+
  labs(ylab("Mercury Level"))+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
}

fishBoxplot(fish) #with outliers

#removing goutliers ...
fish %>%
  filter(Mercury < 1.5) %>%
  fishBoxplot()


#1b) bootstrapping the mean
#Generating bootstrap sample

n <- nrow(fish)
B <- 5000

manualBoot <- function(sampleData){
  replicate(B,
            {
              s <- sample(sampleData, size = n, replace = T)
              mean(s)
            }
  )
}

bootFish <- manualBoot(fish$Mercury)

#finding the 95% percentile interval and standard error of the sampling distribution of sample mean = sd / sqrt(sample size)

quantile(bootFish, c(0.025, 0.975)) %>%
  round(3)
sd(bootFish)/sqrt(length(bootFish))

#1c) bootstrapping w/out the outlier

fishNoOutlier <- fish %>%
  filter(Mercury < 1.5)  
bootFish2 <- manualBoot(fishNoOutlier$Mercury)

quantile(bootFish2, c(0.025, 0.975)) %>%
  round(3)
sd(bootFish2)/sqrt(length(bootFish2))

#1d) write this up - removing outlier unskewed the bootstrap distribution, made it more normally-distributed. Narrower distribution, smaller range for 95% CI, reduced standard error of distribution of sample means...

########################################3
#Redoing 1b and 1c with boot package
#########################
#1b
bootProp <- function(x,i){
  return(mean(x[i]))
}
bootDirect <- boot(fish$Mercury, bootProp, B)
bootDirectCI <- boot.ci(bootDirect, conf = 0.95, type = "perc")
bootDirectCI$percent[c(4,5)] %>% 
  round(3)

#direct method produces similar confidence intervals: 0.112 - 0.304, but different standard error - 0.0559

#1c
bootDirect2 <-boot(fishNoOutlier$Mercury, bootProp, B)
bootDirectCI2 <- boot.ci(bootDirect2, conf = 0.95, type = "perc")
bootDirectCI2$percent[c(4,5)] %>% 
  round(3)

#Direct method produces same 95% CI: 0.108 - 0.139, much different Std error: 0.00789


#Question 2 - bby weights 

girls <- read.csv(file = '../data/Girls2004.csv')

n = nrow(girls) #only need to reset n, B still at 5000 
B = 5000
  
girlsBoot <- boot(girls$Weight, bootProp, B) # girlsBoot$t contains the sampling distributino
girlsBootCI <- boot.ci(girlsBoot, conf = 0.9, type = "perc")


#Hypothesis testing: H0: mean weight of girls born in Alaska is same as BW in rest of US (pi = 3,389 grams)
# Ha: mean weight of girls in AK ... does not equal ... (pi != 3,389g)
# a = 0.1

#90% confidence interval contains population mean, so at alpha of 0.1 there is not enough evidence to reject the null, seems that sample is not significantly different from the general population?


#Question 3 - central limit theorem w/ exponential distribution
#n = 30, lambda (rate parameter) = 1/10, mean (Beta) = 12

#3a) the expected value of a sample mean of the exponential distribution is 1/lambda, which in this case = 10

#3b) using rexp to simulate sampling from variable with exponential distribution
n = 3000
Q3sampleMeans <- replicate(n, expr = mean(rexp(n = 30, rate = 0.1)))
#proportion of sample means >= 12
q3SampProp <- (length(Q3sampleMeans[Q3sampleMeans >= 12])/n) %>%
  round(3)

#3c)
prop.test(x = q3SampProp, n, p = 0.4, alternative = "less")    #need to check whether null is 0.4 (12/30)
#Might not even need to use this - just based on the fact that only 13.6% of random samples had a mean of 12 or greater, it seems relatively unlikely for the friend to have obtained a mean of 12 for his sample size.


#4) MLE

#5) sample variance - normal distribution N(mu, sigma = SD) (variance = SD^2)

#5a) bootstrapping the sampling distribution for the sample variance

B <- 2500

bootVarVector <- replicate(n, expr = var())
