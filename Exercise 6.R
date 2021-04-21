# Script name: Exercise 6.7
# Purpose: exercise 6.7
# Author: Robyn Manuel
# Date: 20/04/2021


# 6.7.1 -------------------------------------------------------------------

library(tidyverse)
install.packages("plotly")
library(ggplot2)
library(plotly)
install.packages(rmisc)

snakes <- read.csv("data/snakes.csv")
snakes$day = as.factor(snakes$day)


snakes.summary <- snakes %>% 
  group_by(day, snake) %>% 
  summarise(mean_openings = mean(openings),
            sd_openings = sd(openings)) %>% 
  ungroup()
snakes.summary

install.packages("Rmisc")

library(Rmisc)
snakes.summary2 <- summarySE(data = snakes, measurevar = "openings", groupvars = c("day"))

ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary2, aes(x = day, xend = day, y = openings - ci, yend = openings + ci, colour = day),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

snakes.aov <- aov(openings ~ day + snake, data = snakes)
summary(snakes.aov)


par(mfrow = c(2, 2))


snakes.res <- residuals(snakes.aov)
hist(snakes.res, col = "red")

plot(fitted(snakes.aov), residuals(snakes.aov), col = "red")

snakes.tukey <- TukeyHSD(snakes.aov, which = "day", conf.level = 0.90)
plot(snakes.tukey, las = 1, col = "red")


ggplot(data = snakes, mapping = aes(x =openings, y = snake))+
  geom_jitter(aes(col = snake))+
  labs(title = "Snakes habituation to the stimuli",
       x = "Openings",
       y = "Snakes") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90,
                                   hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 12),
        text = element_text(size = 16))




