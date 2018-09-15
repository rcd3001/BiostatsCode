#Biostats 1 - Homework 1 9/12/18
# Ryan Duggan

rm(list = ls())

library(tidyverse)
library(foreign)
library(knitr)
library(kableExtra)

#Part 1 - importing data downloaded from NHANES
nhgh <- read.dta(file = "../data/nhgh.dta")
#nhgh<- read_tsv(file = "../data/nhgh.tsv")

#Recoding variables - these two functions accomplish the same thing...
#nhgh$tx <- as.factor(plyr::mapvalues(nhgh$tx, from = c(0,1), to = c("No Treatment", "On Insulin or Diabetes Meds")))

nhgh$tx <- factor(nhgh$tx, levels = c(0:1), labels = c("No Treatment", "On Insulin or Diabetes Meds"))
nhgh$dx <- factor(nhgh$dx, levels = c(0:1), labels = c("No Diagnosis", "Diagnosed with DM or Pre-DM"))


#Question 1 - BMI
# a) Histogram of BMI variable

ggplot(data = nhgh)+
  geom_histogram(aes(x = bmi), binwidth = 1)+
  ggtitle("Histogram of Patient BMI")+
  labs(x = "BMI", y = "Count")+
  theme(plot.title = element_text(hjust = 0.5))

# b) Density plot of BMI variable
ggplot(data = nhgh)+
  geom_density(aes(x = bmi), fill = "gray")+
  ggtitle("Density Plot of Patient BMI")+
  labs(x = "BMI", y = "Density")+
  theme(plot.title = element_text(hjust = 0.5))

# c)  summary of trends
# Patient BMI in this dataset shows a normal distribution around a mean of 28.32 (sd = 6.95), with a slight right skew. This trend is evident in both the histogram and density plot.

# d) Which is better
# There do not appear to be any major non-aesthetic differences between the density plot and the histogram of BMI. This makes sense, as density plots appear to be measuring distribution in the same way as the histogram, but with the application of a filter.

#Question 2 - Glycohemoglobin ($gh)

#a - histogram 
ggplot(data = nhgh)+
  geom_histogram(aes(x = gh), binwidth = 0.5)+
  ggtitle("Histogram of Patient Glycohemoglobin Percentage")+
  labs(x = "Glycohemoglobin Percentage", y = "Count")+
  theme(plot.title = element_text(hjust = 0.5))

#b- density
ggplot(data = nhgh)+
  geom_density(aes( x = gh), fill = "gray")+
  ggtitle("Density Plot of Patient Glycohemoglobin Percentage")+
  labs(x = "Glycohemoglobin Percentage", y = "Density")+
  theme(plot.title = element_text(hjust = 0.5))

#c - density of gh with tx = 0 and tx = 1 (both codes accomplish the same thing but first one is unnecessarily complique)
#ggplot()+
#  geom_density(data = nhgh[nhgh$tx == 0,], aes(x = gh), color = "blue")+
#  geom_density(data = nhgh[nhgh$tx == 1,], aes(x = gh), color = "red")+

ggplot(data = nhgh, aes(x = gh, fill = tx))+
  geom_density(position = "stack", alpha = 0.5)+
  ggtitle("Density Plot of Gycohemoglobin (Gh) Percentage by Diabetes Treatment Status")+
  labs(x = "Gh Percentage", y = "Density")+
  theme(plot.title = element_text(hjust = 0.5))

#d -- explain this shit
#Summary stats for gh by diabetes treatment status


#This plot is showing the frequency of glycohemoglobin percentage in the patient sample broken down by whether or not they are receiving insulin treatment (tx = 1) or no treatment (tx = 0). Patients receiving treatment showed a higher mean and median Gh level than patients not receiving treatment.  
group_by(nhgh, tx) %>% 
  summarize(., mean = mean(gh, na.rm = T), median = median(gh, na.rm = T), sd = sd(gh, na.rm = T)) %>%
  kable(col.names = c("Treatment Status", "Mean", "Median", "Standard Deviation")) %>%
  kable_styling(bootstrap_options = "striped")

#This pattern holds up when plotting the same as above, breaking down Gh by DM diagnosis, which would be expected as Gh levels are correlated with the presence of diabetes, and presumably patients without diabetes would not be receiving treatment.
ggplot(data = nhgh, aes(x = gh, fill = dx))+
  geom_density(position = "stack", alpha = 0.5)+
  ggtitle("Density Plot of Gycohemoglobin (Gh) Percentage by Diabetes Diagnosis")+
  labs(x = "Gh Percentage", y = "Density")+
  theme(plot.title = element_text(hjust = 0.5))

#----------------

#Q3 - race / ethnicity
#a 

ggplot(data=nhgh, aes(x = tx, fill = re))+
  geom_bar(position = "fill")+  #this makes the bars proportional instead of absolute
  ggtitle("Bar Graph of Diabetes Treatment Status by Race")+
  labs(x = "Diabetes Treatment", "Proportion", fill = "Race")

#b SUMMARY[]][
# This graph shoes that roughly equal proportions of each racial group receive each treatment type, with minority (non-white) groups tending to have a higher proportion receiving treatment than not. 

#------

#Q4 BMI vs race
ggplot(data = nhgh, aes(x=re, y = bmi))+
  geom_boxplot()+
  ggtitle("Boxplot of BMI by Race")+
  labs(y = "BMI")+
  theme(plot.title = element_text(hjust = 0.5))

#grouping nhgh data by race and outputting summary stats with kable package

nhghByRace <- group_by(nhgh, re)
nhghByRace %>%
  summarize(., mean = mean(bmi), median = median(bmi), sd = sd(bmi), IQR = IQR(bmi), min = min(bmi), max = max(bmi))%>%
  kable(col.names = c("Race", "Mean", "Median", "Standard Deviation", "IQR", "Minimum", "Maximum")) %>%
  kable_styling(bootstrap_options = "striped")


#Although we did not test for statistical significance, the boxplots and summary statistics indicate that average BMI is similar across different races, with the highest mean and median being found in the Non-Hispanic Black group, and the lowest being the "Other Race" group. Aside from this difference, BMI seems to be normally distributed in all racial groups.

#Q5 - BMI, gh, and insulin (tx)
ggplot(data = nhgh)+
  geom_point(aes(x = bmi, y = gh, color = tx), size = 2, alpha = 0.5)+
  ggtitle("Scatterplot of BMI and Gh Measurement by Diabetes Treatment Status")+
  scale_color_discrete(name = "Insulin Treatment")

#Explanation:
# There appears to be a very strong clustering of lower BMI and lower Gh percentages in patients not being treated for diabetes, with even otherwise healthy patients with higher BMIs tending to have low Gh levels. Patients who are receiving insulin or other treatments have a wider spread of Gh levels, with a tendency to cluster at a slightly higher level compared to the No Treatment group, while also tending to remain in a similar range of BMI.