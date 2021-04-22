# Script name: Day_Four
# Author: Robyn Manuel
# Date: 21/04/2021

# Load packages -----------------------------------------------------------


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

# Chapter 10 Confidence intervals -----------------------------------------

#if repeate the experiment 100 times
#the estimate of the mean will fall in the range
#E5% of the time repeate will fall out of range


# bootstrapping: take random sampled and repeate? how often will data fall in 
#the interval and how often will it exceed that

Input <- ("
Student  Sex     Teacher  Steps  Rating
a        female  Jacob    8000   7
b        female  Jacob    9000  10
c        female  Jacob   10000   9
d        female  Jacob    7000   5
e        female  Jacob    6000   4
f        female  Jacob    8000   8
g        male    Jacob    7000   6
h        male    Jacob    5000   5
i        male    Jacob    9000  10
j        male    Jacob    7000   8
k        female  Sadam    8000   7
l        female  Sadam    9000   8
m        female  Sadam    9000   8
n        female  Sadam    8000   9
o        male    Sadam    6000   5
p        male    Sadam    8000   9
q        male    Sadam    7000   6
r        female  Donald   10000  10
s        female  Donald    9000  10
t        female  Donald    8000   8
u        female  Donald    8000   7
v        female  Donald    6000   7
w        male    Donald    6000   8
x        male    Donald    8000  10
y        male    Donald    7000   7
z        male    Donald    7000   7
")
#male an female assign to different teachers

data <- read.table(textConnection(Input),header = TRUE)
summary(data)
#install.packages("rcompanion")
library(rcompanion)
# ungrouped data is indicated with a 1 on the right side of the formula, or the group = NULL argument.
gr <- groupwiseMean(Steps ~ Sex ,data = data, conf = 0.95, digits = 3)

ggplot(data = gr) +
  geom_col(aes(x = Sex, y = Mean), fill = c("slateblue1", "paleturquoise3")) +
  geom_errorbar(aes(ymin = Trad.lower,
                    ymax = Trad.upper,
                    x = Sex),
                col = "red4",
                width = 0.2, size = 1) +
  geom_point(mapping = aes(x = Sex, y = Mean), size = 4, shape = 23, fill = "grey10") +
  ggtitle("One way data graph")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect("lightgoldenrod")) +
  labs(x = "Sex", y = "Steps")


#  ------ -----------------------------------------------------------------
 data("PoohPiglet")
head(PoohPiglet)


ggplot(data = groupwiseMean(Likert ~ Speaker, data = PoohPiglet, conf = 0.95, digits = 3)) +
  geom_col(aes(x = Speaker, y = Mean), fill = c("skyblue2", "skyblue3", "skyblue4")) +
  geom_errorbar(aes(ymin = Trad.lower,
                    ymax = Trad.upper,
                    x = Speaker),
                col = "grey4",
                width = 0.2, size = 1) +
  geom_point(mapping = aes(x = Speaker, y = Mean), size = 4, shape = 23, fill = "olivedrab3") +
  ggtitle("Attempt")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect("indianred1")) +
  labs(x = "Speaker", y = "Likert")


# ------ ------------------------------------------------------------------



# two-way data
 groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95,digits = 3)

gr <- groupwiseMean(Steps ~ Sex,data = data, conf = 0.95, digits = 3)

ggplot(data = gr)+
  geom_col(aes(x = Sex, y= Mean)) +
  geom_errorbar(aes(ymin = Trad.lower,
                    ymax = Trad.upper, 
                    x = Sex),
                col = "black", 
                width = 0.2) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Sex", y = "Steps")

# sexes are equally lazy = error bars over lap.

#
teach <- groupwiseMean(Steps ~ Teacher + Sex,data = data, conf = 0.95, digits = 3)

ggplot(data = teach)+
  geom_col(aes(x = Sex, y= Mean, col = Teacher, fill = Teacher)) +
  geom_errorbar(aes(ymin = Trad.lower,
                    ymax = Trad.upper, 
                    x = Sex, width = 0.5, size = 1),
                col = c("darkorchid1", "darkorchid2", "darkorchid3", "darkorchid4", "mediumpurple1",  "mediumpurple2")) +
  facet_wrap(~Teacher, ncol = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect("lightgoldenrod1")) +
  labs(x = "Sex", y = "Steps")
# no different in distance walked determined by teacher.
#


# Bootstrapping -----------------------------------------------------------
groupwiseMean(Steps ~ Sex,
              data = data,
              conf = 0.95,
              digits = 3,
              R = 10000,
              boot = TRUE,
              traditional = FALSE,
              normal = FALSE,
              basic = FALSE,
              percentile = FALSE,
              bca = TRUE)

#no significant between the past few tests, use ANOVA
# analysis of var

anova <- aov(Steps ~ Sex*Teacher, data = data)
summary(anova)
# females less lazy than males , teacher has no significants.



anova_Tukey <- TukeyHSD(anova)
plot(anova_Tukey)

