# Script name: Day_3
# Purpose: Chapters_8, 
# Author: Robyn Manuel
# Date: 21/04/2021

library(tidyverse)

# 8.1 The simple linear regression equation -------------------------------

head(faithful)

eruption.lm <- lm(eruptions ~ waiting, data = faithful)
summary(eruption.lm)
# lm linear method/model
head(ToothGrowth)
len.lm <- lm(len ~ dose, data = ToothGrowth)
summary(len.lm)
# 8.1.3 A graph of the linear regression ----------------------------------

slope <- round(eruption.lm$coef[2], 3)
slopes <- round(len.lm$coef[1], 3)
# p.val <- round(coefficients(summary(eruption.lm))[2, 4], 3) # it approx. 0, so...
p.val = 0.001
r2 <- round(summary(eruption.lm)$r.squared, 3)

ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  annotate("text", x = 45, y = 5, label = paste0("slope == ", slope, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.75, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.5, label = paste0("italic(r)^2 == ", r2), parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", colour = "salmon") +
  labs(title = "Old Faithful eruption data",
       subtitle = "Linear regression",
       x = "Waiting time (minutes)",
       y = "Eruption duration (minutes)")
#eg
library(ggplot2)
ggplot(data = ToothGrowth, aes(x = dose, y = len)) +
  geom_point() 
faith <- faithful
ggplot(data = faithful, aes(x = index, y = waiting )) +
  geom_col(size = 1, color ="darkblue", fill = "white")
  
library(boot)
head(breslow)

bv <- as_tibble(breslow)
ggplot(data = bv, aes( x = age, y = n)) + 
  geom_col(size = 1, color = "darkblue", fill = "white")

ggplot(bv, aes(x  = age, y = y)) +
  geom_line(size = 1.5, color = "red", group = 1)


ggplot(data = bv, aes( x = age, y = n, col = age)) + 
  geom_col(size = 1, color = "seagreen2", fill = 
             c("dodgerblue4", "deepskyblue", 
               "cyan4", "aquamarine3", "cadetblue",
               "cornflowerblue","darkslategray","darkslateblue",
               "lightblue3","mediumturquoise")) +
  geom_line(size = 1, color = "violetred3", group = 1) +
  scale_y_continuous(sec.axis = sec_axis(~./3 name ="series2"))
  

# Section9 ----------------------------------------------------------------

library(tidyverse)
library(ggpubr)
#install.packages("corrplot")
library(corrplot)

ecklonia <- read.csv("data/ecklonia.csv")
view(ecklonia)

# using select function to exclude/prevent "-" some column from being read in
ecklonia_sub1 <- ecklonia %>% 
  select(-species, - site, - ID) # use to get rid of all columns with words in
# go from 12 variables to 9.
view(ecklonia_sub1)


# Pearsons: when varibles are continous -----------------------------------
#compareing/ correlating two variables
cor.test(x = ecklonia$stipe_length, ecklonia$frond_length,
         use = "everything", method = "pearson")

#comparing variable in the same dataset /correlate everything
ecklonia_pearson <- cor(ecklonia_sub1)
view(ecklonia_pearson)

#closer correlation values in to 1 the stronger the correlation
# negative value, very weakly correlated


# 9.2 Spearman rank correlation -------------------------------------------

# Create ordinal data
cor.test(x = ecklonia$stipe_length, ecklonia$frond_length,
         use = "everything", method = "spearman", exact = FALSE)


# 9.3 Kendall rank correlation --------------------------------------------

#works n ordinal and continuous
# normal and non normal data (explain why you used it)


ecklonia_norm <- ecklonia_sub1 %>% 
  gather(key = "variable") %>% 
  group_by(variable) %>% 
  summarise(variable_norm = as.numeric(shapiro.test(value)[2]))
ecklonia_norm

cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall", exact = FALSE)



# 9.4 One panel visual ----------------------------------------------------

# Calculate Pearson r beforehand for plotting
r_print <- paste0("r = ", 
                  round(cor(x = ecklonia$stipe_length, ecklonia$frond_length),2))

# Then create a single panel showing one correlation
ggplot(data = ecklonia, aes(x = stipe_length, y = frond_length)) +
  geom_smooth(method = "lm", colour = "palevioletred4", se = F, alpha = 0.8) +
  geom_point(colour = "orangered4") +
  geom_label(x = 300, y = 240, label = r_print) +
  labs(x = "Stipe length (cm)", y = "Frond length (cm)") +
  theme_pubclean()

ggplot(data = ecklonia, aes(x = stipe_length, y = frond_length)) +
  geom_smooth(method = "lm", colour = "darkolivegreen4", se = F) +
  geom_point(colour = "cyan1", shape = 23, fill = "chocolate1") +
  geom_label(x = 300, y = 240, label = r_print) +
  labs(x = "Stipe length (cm)", y = "Frond length (cm)") +
  theme_pubclean()


# 9.5 Multiple panel visual -----------------------------------------------

corrplot(ecklonia_pearson, method = "circle") 

# 9.6.1 Exercise 1 --------------------------------------------------------


library(reshape2)
ecklonia_heat <- cor(ecklonia_pearson)
head(ecklonia_heat)
colors <- colorRampPalette(c("lightsalmon", "firebrick4")) 

heatmap(ecklonia_heat, col = colors(100)) 
heatmap(ecklonia_heat, Rowv = NA, Colv = NA, col = colors(100))
  

#ggplot heatmap:
# make the row title into a coloumn (melt)
ecklonia_heatgg <- melt(ecklonia_heat)
ggplot(ecklonia_heatgg, aes(Var1, Var2)) +                           
  geom_tile(aes(fill = value)) + 
  scale_fill_gradient(low = "royalblue", high = "springgreen") 




            