# Title: Quantitative Ecology
# Purpose: Beta diversity questions
# Author: Robyn Manuel
# Date: 2 July 2021


# Question 1 --------------------------------------------------------------

# Plot species turnover as a function of Section number, and provide a 
# mechanistic explanation for the pattern observed.

library(vegan)
library(ggplot2)
library(betapart)
# Load data

spp <- read.csv("data/SeaweedsSpp.csv")

spp <- dplyr::select(spp,-1) # remove the column with site names

Y.core <- betapart.core(spp) # set up the required quantities


Y.pair <- beta.pair(Y.core, index.family = "sor") 
str(Y.pair)

# Turnover:
Y1 <- as.matrix(Y.pair$beta.sim)
Y1

Y1_plot <- data.frame(Y1[1:1, 1:58]) # select for the 58th column and the first columns' data
Y1_plot

plot_y1 <- ggplot(data = Y1_plot, aes(x = 1:58, y = Y1_plot[,1] )) +
  geom_line(col = "pink4")+
  xlab("Coastal section (west to east)") + ylab("species turnover") +
  labs(title = "The species turnover occuring along the coast (from west to east))") +
  theme(panel.grid.major = element_line(size = 0.2, linetype = 2),
        plot.title = element_text(size = 9, hjust = 0.5),
        plot.subtitle = element_text(size = 8, hjust = 0),
        axis.text = element_text(size = 8),
        axis.ticks = element_line(size = 0.4),
        axis.title = element_text(size = 9, face = "bold"),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.4),
        panel.grid.minor = element_line(colour = NA),
        strip.background = element_rect(colour = NA, fill = "antiquewhite2", size = 0.4),
        strip.text = element_text(size = 8, hjust = 0))
plot_y1

# Question 2 --------------------------------------------------------------

# Based on an assessment of literature on the topic, provide a discussion of 
# nestedness-resultant β-diversity.
# Use either a marine or terrestrial example to explain this mode of 
# structuring biodiversity.

# An example of nestedness-resultant β-diversity would be up-welling. It is a process 
# which encourages the up-welling of nutrient in a marine environment which can encourage a variety 
# of species to enter a specific area in order to feed on the newly available nutrients. This up-welling 
# encourages the alpha diversity of an area to change. Or another example would be eutrophication
# which makes the water toxic, thus killing the some of species within the area of eutrophication,
# creating a decline of species within the area and changing the alpha diversity.



