#Script name: Day Two
# Purpose: Chapter 6
# Author: Robyn Manuel
# Date: 20/04/2021


# Chapter 6 ---------------------------------------------------------------



library(tidyverse)
#install.packages("plotly")
#shipiro calculate normal for all, need to calculate for a an b
# histogram good way to check data
# less than 0.5 is significant dfferene, more than no significant difference
set.seed(666)
r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))

h <- ggplot(data = r_dat, aes(x = dat, fill = sample)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = sample), colour = NA, alpha = 0.4) +
  labs(x = "value")

shapiro.test(r_dat$dat)

#tests for normality need to group by variable
# we use the square bracket notation to select only the p-value;
# had we used `[1]` we'd have gotten W
r_dat %>% 
  group_by(sample) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(dat)[2]))


# Homoscedasticity: 
# rule variants must not be two of four times greater than one another, 
#if does data not normal or does not allow to do t test

r_dat %>% 
  group_by(sample) %>% 
  summarise(sample_var = var(dat))

# Two for one:

# normality and shepiro test in one

two_assum <- function(x) {
  x_var <- var(x)
  x_norm <- as.numeric(shapiro.test(x)[2])
  result <- c(x_var, x_norm)
  return(result)}

r_dat %>% 
  group_by(sample) %>% 
  summarise(sample_var = two_assum(dat)[1],
            sample_norm = two_assum(dat)[2])
#t test where compare one sample to the mean;
#

# TWO SAMPLE T TEST
set.seed(666)
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))
# perform t-test
# note how we set the `var.equal` argument to TRUE because we know 
# our data has the same SD (they are simulated as such!)
t.test(dat ~ sample, data = r_two, var.equal = TRUE)

# ~ how the samples compare to the data.





# Ecklonia ----------------------------------------------------------------


ecklonia <- read_csv("data/ecklonia.csv") %>% 
  gather(key = "variable", value = "value", -species, -site, -ID)

ggplot(data = ecklonia, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip()


# filter the data
ecklonia_sub <- ecklonia %>% 
  filter(variable == "stipe_mass")

# then create a new figure
ggplot(data = ecklonia_sub, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() + # CHANGES THE AXIS, X ON THE Y AND VICE VERSA
  labs(y = "stipe mass (kg)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ecklonia_sub %>% 
  group_by(site) %>% 
  summarise(stipe_mass_var = two_assum(value)[1],
            stipe_mass_norm = two_assum(value)[2])


# traditional output
t.test(value ~ site, data = ecklonia_sub, var.equal = TRUE, alternative = "greater")


# Chapter_7 ---------------------------------------------------------------

# read up on journals and how they incorparated it in theirs.
 # if dont want to transform data

chick <- ChickWeight
chicks <- as_tibble(ChickWeight)

chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21) # specifically diet one and two

library("ggpubr")
compare_means(weight ~ Diet, data = chicks_sub, method = "t.test")



# ANOVA 

chicks.aov1 <- aov(weight ~ Diet, data = filter(chicks, Time == 21))
summary(chicks.aov1)

chicks.aov1 <- aov(weight ~ Time, data = filter(chicks, Time == 21))
summary(chicks.aov1)

chicks.aov1 <- aov(Time ~ weight, data = filter(chicks, Time == 21))
summary(chicks.aov1)

summary(aov(weight ~ Diet, data = filter(chicks, Time %in% c(0))))

chicks.aov2 <- aov(weight ~ as.factor(Time), data = filter(chicks, Time %in% c(0, 2, 10, 21)))
summary(chicks.aov2)


#Alternatives to ANOVA

chicks <- as_tibble(ChickWeight)


chicks %>% 
  filter(Time == 0) %>% 
  group_by(Diet) %>% 
  summarise(norm_wt = as.numeric(shapiro.test(weight)[2]),
            var_wt = var(weight))


compare_means(weight ~ Diet, data = filter(chicks, Time == 0, Diet %in% c(1, 2)), method = "wilcox.test")


compare_means(weight ~ Diet, data = filter(chicks, Time == 12, Diet %in% c(1, 2)), method = "wilcox.test")


compare_means(weight ~ Diet, data = filter(chicks, Time == 0), method = "kruskal.test")
