#Biostats 1 - Homework 1 9/12/18
# Ryan Duggan

rm(list = ls())

#Part 1 - importing data downloaded from NHANES
nhgh <- read.dta(file = "../data/nhgh.dta")
#nhgh<- read_tsv(file = "../data/nhgh.tsv")

#Recoding variables - these two functions accomplish the same thing...
#nhgh$tx <- as.factor(plyr::mapvalues(nhgh$tx, from = c(0,1), to = c("On Insulin", "Diabetes Meds")))

nhgh$tx <- factor(nhgh$tx, levels = c(0:1), labels = c("On Insulin", "Diabetes Meds"))
nhgh$dx <- factor(nhgh$dx, levels = c(0:1), labels = c("Diagnosed with DM", "Pre-DM"))


#Question 1 - BMI
# a) Histogram of BMI variable

ggplot(data = nhgh)+
  geom_histogram(aes(x = bmi), binwidth = 1)

# b) Density plot of BMI variable
ggplot(data = nhgh)+
  geom_density(aes(x = bmi), fill = "gray")

# c) Which is better 

# d)

#Question 2 - Glycohemoglobin ($gh)

#a - histogram 
ggplot(data = nhgh)+
  geom_histogram(aes(x = gh), binwidth = 0.5)

#b- density
ggplot(data = nhgh)+
  geom_density(aes( x = gh), fill = "gray")

#c - density of gh with tx = 0 and tx = 1 (both codes accomplish the same thing but first one is unnecessarily complique)
#ggplot()+
#  geom_density(data = nhgh[nhgh$tx == 0,], aes(x = gh), color = "blue")+
#  geom_density(data = nhgh[nhgh$tx == 1,], aes(x = gh), color = "red")+

ggplot(data = nhgh, aes(x = gh, fill = tx))+
  geom_density(position = "stack", alpha = 0.5)+
  scale_x_continuous(name = "[gylcohemoglobin whatever]")

#d -- explain this shit
#----------------

#Q3 