# Script name: Day_One
# Description:
# Author: Robyn Manuel
# Date 19 April 2021

# Chapter 2:\ -------------------------------------------------------------


# Stats is to make sense of large amounts of data.
# Find patterns in data, therefore need to summaries data in a statistic.
# Statistics: summaries of data...(missed some stuuf)

# Descriptive stats:classify data in small collection of data, used to describe the larger data.

# Inferential stats: Allow to make inferenced aboput data. 
#Allows us to ask question such as if xperiment was success full, if it is different in a similar siotuation when we exclude some thing from  a comtrol.
# did the findinfg happen by chance or is it a feature of the data-

# Numerical data first data work with.(Quantitative data)
# Nominal/ descrete data: count data, whole full numbers
# Countinous data: fractional numberical data (floating point numbers)
# dates are a class of data in R

#FRIENDS DONT LET FRIENDS MAKE PIE CHARTS


# Looking at the data that comes with R -----------------------------------

                #type in console:
#data()
#data(package = .packages(all.available = TRUE))
# ?BOD
# str(BOD) ......time and demand both numberical values. 
            #......(STR structure tell you about type/structure of data)
# summary(BOD)

#unique(InsectSpray$spray)
#?Loblolly
#str(Loblolly)
# what could do when get a data set for the first time

# Exercise: pick data sets for you to understand data ---------------------

#where would start to understand data

#> ?HairEyeColor 
#> str(HairEyeColor)
#summary(HairEyeColor)


# Load built in data as an R object ---------------------------------------

pines <-  Loblolly
str(pines) #th structure of data
head(pines)
# in console= 
#class of data in a comloum of a partictualr data set.
# class(pines$height)


# ------ ------------------------------------------------------------------

# sample need to be full representation of diversity, hence random.

library(tidyverse)
chicks <- as_tibble(ChickWeight)

# Distinction betwen 'nrow()' and 'true' sample size  ---------------------
nrow(chicks)
unique(chicks$Chick)

#nrow(chicks) not sample size
# total amount of samples
# in console : unique(chicks$Chick)
#unique counts number of unique levels in data.


#normal distribution: median and mean will be very close or equal to.


# calculate the mean wieght of chicks surviving to 20th day ---------------

chicks %>% 
  filter(Time == 20) %>% 
  summarise(chicks = mean(weight))
# mean mass of all surviving chicks at 20 days is 210. because is overall average(not reasonal be)




# At day 20 what will be the average diet of chick be within their --------

chicks %>% 
  group_by(Diet) %>% 
  filter(Time == 20) %>% 
  summarise(chicks = mean(weight))

# Calculate the sd: -------------------------------------------------------

chicks %>% 
  filter(Time == 20) %>% 
  summarise(chicks = mean(weight), 
            chicks_st = sd(weight))
#diet specific

mean_st <- chicks %>% 
  group_by(Diet) %>% 
  filter(Time == 20) %>% 
  summarise(chicks_m = mean(weight),
            chicks_st = sd(weight))
# report as mean + or - st. dev value
# ------ ------------------------------------------------------------------
library(ggplot2)
library(dplyr)
ggplot(data = mean_st) +
  geom_bar(aes(x = Diet, y = chicks_m), stat = "identity", fill = c("slategray1","slategray","darkorchid4","slateblue4")) +
  ggtitle("The average weight chicks got to with each diet")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect("lightpink")) +
  labs(x = "Diet", y = "mean")



# ------ ------------------------------------------------------------------

# Calculate in summarised function to calculate median --------------------

chicks %>% 
  filter(Time == 20) %>% 
  summarise(chicks = mean(weight), 
            chicks_st = sd(weight),
            chick_med = median(weight))
#diet specific

chicks %>% 
  group_by(Diet) %>% 
  filter(Time == 20) %>% 
  summarise(chicks = mean(weight),
            chicks_st = sd(weight),
            chick_med = median(weight))
# can also do formal calculation.

# Kurtosis ----------------------------------------------------------------
# load the package
install.packages("e1071")
library("e1071")

kurtosis(chicks$weight)

# mean calculated manually ------------------------------------------------
chicks %>% 
  filter(Time == 20) %>% 
  summarise(chicks = mean(weight), 
            chicks_st = sd(weight),
            chick_med = median(weight),
            mean_wt = sum(weight) / n())
#diet specific

chicks %>% 
  group_by(Diet) %>% 
  filter(Time == 20) %>% 
  summarise(chicks = mean(weight),
            chicks_st = sd(weight),
            chick_med = median(weight),
            mean_wt = sum(weight) / n())


# Testing quantiles -------------------------------------------------------
chicks %>% 
  group_by(Diet) %>% 
  filter(Time == 20) %>% 
  summarise(chicks = mean(weight),
            chicks_st = sd(weight),
            chick_med = median(weight),
            mean_wt = sum(weight) / n(),
            min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25),
            med_wt = median(weight),
            qrt3_wt = median(weight, p = 0.75),
            max_wt = max(weight))
# p = percentage of the quatile

# MIN and MAX -------------------------------------------------------------

range(chicks$weight)  

chicks %>% 
  summarise(lower_wt = range(weight)[1],
            upper_wt = range(weight)[2])  
# [] INDICATED A VECTOR IN A SPECTRUM OF NUMBERS.

# for per diet
chicks %>% 
  group_by(Diet) %>% 
  filter(Time == 20) %>% 
  summarise(chicks = mean(weight),
            chicks_st = sd(weight),
            chick_med = median(weight),
            mean_wt = sum(weight) / n(),
            lower_wt = range(weight)[1],
            upper_wt = range(weight)[2])


# 3.4 Missing values ------------------------------------------------------

dat1 <- c(NA, 12, 76, 34, 23)  

mean(dat1)  
#To remove the missing value

mean(dat1, na.rm = TRUE)  





