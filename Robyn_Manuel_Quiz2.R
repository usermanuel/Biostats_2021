# Script name: Quiz 2
# Author: Robyn Manuel
# Date: 22/04/2021

# Load library ------------------------------------------------------------


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
two_assum <- function(x) {
  x_var <- var(x)
  x_norm <- as.numeric(shapiro.test(x)[2])
  result <- c(x_var, x_norm)
  return(result)
}


# Load packages -----------------------------------------------------------

orange <- datasets::Orange
toothgrowth <- datasets::ToothGrowth
warps <- datasets::warpbreaks

# Orange ------------------------------------------------------------------


head(orange)
?Orange
ggplot(data =orange, aes(x = age, y = circumference, group = Tree)) +
  geom_boxplot(aes(col = Tree)) +
  coord_flip()

# focus on tree 5 and 3
o3_o5 <- orange %>% 
  group_by(Tree) %>% 
  filter(Tree == c("3", "5"))
Trees <- ggplot(data = o3_o5, aes(x = age, y = circumference, group = Tree)) +
  geom_boxplot(aes(col = Tree)) +
  coord_flip()
# hypothesis:
# tree 5 has a greater circumference than tree 3
# H0: circumference Tree 5 is not greater than Tree 3
# H1: circumference Tree 5 is greater than Tree 3


two_assum <- function(x) {
  x_var <- var(x)
  x_norm <- as.numeric(shapiro.test(x)[2])
  result <- c(x_var, x_norm)
  return(result)
}

o3_o5 %>% 
  group_by("Tree") %>% 
  summarise(Tree_var = two_assum(circumference)[1],
            Tree_norm = two_assum(circumference)[2])
# the above code wASNT WORKING
o3_o5 %>% 
  filter(Tree == "3") %>% 
  summarise(T3_var = var(circumference),
            T3_norm = as.numeric(shapiro.test(circumference)[2]))

o3_o5 %>% 
  filter(Tree == "5") %>% 
  summarise(T5_var = var(circumference),
            T5_norm = as.numeric(shapiro.test(circumference)[2]))

3967*2
# shapiro is more than 0.05 and homoscedasticity is not two or four time larger than eachother
# there we assume data is normally distributed.
compare_means(circumference ~ Tree, data = o3_o5, method = "t.test", var.equal = TRUE, alternative = "greater")

#p = 0.280


# Orange results ----------------------------------------------------------

# We accept the null hypothesis as the p value (0.280) is greater than 0.05. Therefore the 
# circumference of tree 5 is greater that that of tree 3



# ToothGrowth -------------------------------------------------------------

head(toothgrowth)
?ToothGrowth

ggplot(data = toothgrowth, aes(x = dose, y = len, group = supp)) +
  geom_boxplot(aes(col = supp)) +
  coord_flip()
# focus on dosages of 0.5mg/day and 2mg/day
TG <- toothgrowth %>% 
  filter(dose == c("0.5", "2"))
tg <- ggplot(data = TG, aes(x = supp, y = len, group = dose)) +
  geom_boxplot(aes(col = dose)) +
  coord_flip()  

# hypothesis:
# dosage 2mg/day has a greater effect on length than dosage of 0.5mg/day
# H0: dosage of 2mg/day is not more effective on length than dosage of 0.5mg/day 
# H1: dosage of 2mg/day is more effective on length than dosage of 0.5mg/day 
  
TG %>% 
  filter(dose == "2") %>% 
  summarise(T3_var = var(len),
            T3_norm = as.numeric(shapiro.test(len)[2]))

TG %>% 
  filter(dose == "0.5") %>% 
  summarise(T3_var = var(len),
            T3_norm = as.numeric(shapiro.test(len)[2]))
# data is normally distributed


compare_means(len ~ dose, data = TG , method = "t.test", var.equal = TRUE, alternative = "greater")

#null is regected because p is significantly smaller



# Question 2 --------------------------------------------------------------

 load("data/SACTN_daily_v4.2.RData")
 library(lubridate)
library(dplr)

SACTN <- SACTN_daily_v4.2 %>% 
  separate(index, c("site", "src"), sep = "/", remove = TRUE, convert = FALSE)


SACTN_1 <- SACTN %>% 
  separate(date, c("year", "month", "day"), sep = "-", remove = TRUE, convert = FALSE))


summarise(mutate(year = lubridate::year(date), 
                 month = lubridate::month(date), 
                 day = lubridate::day(date)))
S


