# Script Name: Class quiz
# Author:Robyn Manuel
# Date: 20/04/2021

 

# Question 1 --------------------------------------------------------------

 #List the various data classes and give an explanation and example for each class

# Nominal data and coninuous data.Nominal/ discrete data look at whole numbers,
# where as discrete data looks as decimals   

#List some of the functions used to view your data in R

# glimpse()
# head()
# tail()
# summarise()
# view() 

# Discuss skewness and Kurtosis

# Kurtosis looks at how the tail of the data looks, 
# Skewedness describes how the data is dispersed, whether or not it tendss more to the left or the right


# Question 2 --------------------------------------------------------------
library(tidyverse)
ls("package:datasets")
orange <- data("Orange")
orange <- Orange
glimpse(orange)

str(Orange)
# Orange belongs to nominal data class, as all observation where recorded as whole numbers. 

head(Orange)
tail(Orange)

summary(Orange)

#Determine the Mean, Median and 
#Standard deviation of the age and circumference of the oranges for each of the trees

Orange %>% 
  summarise(mean_age = mean(Orange$age), 
            sd_age = sd(Orange$age),
            med_age = median(Orange$age))

Orange %>% 
  summarise(mean_circum = mean(Orange$circumference), 
            sd_circum = sd(Orange$circumference),
            med_circum = median(Orange$circumference))

library("e1071")

kurtosis(Orange$circumference)
skewness(Orange$circumference)

Orange %>% 
  summarise(Min = min(Orange$circumference),
            Max = max(Orange$circumference),
            qrt1 = quantile(circumference, p = 0.25),
            qrt3 = median(circumference, p = 0.75))

summary(Orange$circumference)

library(ggplot2)

  ggplot(data = Orange, aes(x = circumference, y = age))+
    geom_line(aes( col = "salmon"))+
    labs(x = "Age", y = "Circumference")
    # the graph shows that there is an increase 
  #in tree circumference as there is an increase in tree age


ggplot(data = Orange, aes(x = Tree, y = age))+
  geom_point(aes(size= circumference))
            

# the graph shows that for all trees there is an increase in circumference size with age.




# Question 3 --------------------------------------------------------------

# mutate(): creates new columns
# select(): isolates specific columns, or collected data from specific columns.
# group_by(): sorts similar information in a coloumn to be grouped together.
# filter(): groups data required with similar information in that row.
# seperate(): sorts data which contains more then one variable




