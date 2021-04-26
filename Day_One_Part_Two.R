# Script: Day_One_Part_Two
# Purpose: Chapter 3_Descriptive Statistics
# Date: 19/04/21
# Author: Robyn Manuel

# sample need to be full representation of diversity, hence random.
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(readr)
library(ggpubr)
library(dslabs)
library(reshape) #heatmap
library(plotly)
library(plyr)
library(tidyr)
library(rcompanion)
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

chicks %>% 
  group_by(Diet) %>% 
  filter(Time == 20) %>% 
  summarise(chicks = mean(weight),
            chicks_st = sd(weight))
# report as mean + or - st. dev value

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

# ---------- --------------------------------------------------------------
 graph1 <- chicks %>% 
  group_by(Chick) %>% 
  summarise(chick_m = mean(weight))
library(ggplot2)

gr1<- ggplot(data = graph1) + 
  geom_bar(stat = "identity", mapping = aes(x = Chick, y = chick_m)) +
  scale_fill_brewer(palette = "BuGn") +
  labs(title = "Mean weight of chicks")
  

# -------- ----------------------------------------------------------------


# Kurtosis ----------------------------------------------------------------
# load the package
#install.packages("e1071")
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

graph2 <- chicks %>% 
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



# ----- -------------------------------------------------------------------

