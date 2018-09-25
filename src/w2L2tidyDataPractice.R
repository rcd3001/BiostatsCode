#RDuggan 9/19/18
#Data cleanup practice

rm(list = ls())

library(tidyverse)
library(ggplot2)
#importing tables TB dataset
table1 <- table1
table4a <- table4a
table4b <- table4b

table1 <- table1 %>%
  mutate(rate = cases / population * 10000)

ggplot(table1, aes(x = year, y = cases))+
  geom_line(aes(color = country))+
  geom_point(aes(color = country))

# table 4a is not tidy, so need to use gather and spread to reformat the data!
# key = name of variable (in table 4a, values of variable year should be in rows, 
    # are in columns instead - )
# value = name of variable spread over cells instead of grouped in a single column
    # in this case that would be "cases," so...

table4a <- table4a %>%
  gather(`1999`,`2000`, key = "year", value = "cases")

table4b <- table4b %>%
  gather(`1999`, `2000`, key = "year", value = "population")

#using left join to merge two tables
table4 <- left_join(table4a, table4b)


#SPREADING - when an observation (case / country / object of interest etc) is spread
  # across multiple rows instead of being spread in one row across columns
  # example - table 2

table2 <- table2

table2 <- table2 %>%
  spread(key = type, value = count)


#SEPARATE -- splits one column into multiple columns - similar to strsplit?
# use table3 as example

table3 <- table3
table3 <- table3 %>%
  separate(col = rate, sep = "/", into = c("cases", "population"), convert = T)
head(table3) #leaving out convert = TRUE will split $rate into two character vectors

#undoing separate with unite command

table3 %>%
  unite(col = new, cases, population, sep = "____x____")