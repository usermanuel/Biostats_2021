#Title: Alpha diversity exercise
# Name: Robyn Manuel
# Date: 31 June 2021

#install.packages("tidyverse")
library(tidyverse)
#install.packages("betapart")
library(betapart)
#install.packages("vegan")
library(vegan)
#install.packages("gridExtra")
library(gridExtra)
#install.packages("BiodiversityR")
library(BiodiversityR)
#install.packages("grid")
library(grid)
#install.packages("gridBase")
library(gridBase)
library(tidyr)
#install.packages("ggplot2")
library(ggplot2)      


 spp <- read.csv("data/seaweeds.csv")
spp <- dplyr::select(spp,-1)

dim(spp) # tells you the number of variables and observations.

#spp[1:5, 1:5]
spp[1:4, 1:6] # displays first 5 columns and rows.[rows, columns]

spp[(nrow(spp) -5):nrow(spp),(ncol(spp) -5):ncol(spp)] #displays last number of rows and columns


# Species richness --------------------------------------------------------

spp_richness <- diversityresult(spp, index = 'richness',method ='each site')


ggplot(data = spp_richness, (aes(x = 1:58, y = richness))) +
  geom_area(col = "brown4", fill = "brown4") +
  labs(title = "The species richness occuring along the coast (from west to east))") +
   xlab("Coastal section (west to east)") + ylab("Species richness") +
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


#if BiodiversityR isnt working:
specnumber(spp, MARGIN = 1) # calculated the number of species in each row




# Univariate diversity indices --------------------------------------------

light <- read.csv("data/light_levels.csv")
light

#  calculate species richness, Shannon and Simpson indices:

light_div <- data.frame(
site = c("low_light", "mid_light", "high_light"),
 shannon = round(diversity(light[, 2:7], MARGIN = 1, index = "shannon"), 2),
 simpson = round(diversity(light[, 2:7], MARGIN = 1, index = "simpson"), 2))
 
light_div


# Dissimilarity indices ---------------------------------------------------

# calculate the Sørensen dissimilarity index:
  
sor <- vegdist(spp, binary = TRUE)
# binary = TRUE sets to presence/absence data
sor_df <- round(as.matrix(sor), 4)
sor_df[1:20, 1:20]
# the first 20 rows and columns


# Questions and Answers ---------------------------------------------------

# 1. Why is the matrix square, and what determines the number of rows/columns?
# Due to the number of rows being equal to the number of columns.
# The number if sites present determine the number of rows and columns.

# 2. What is the meaning of the diagonal?
# The diagonal compares is each site compared to itself, 
# therefore it 0 as it is exactly the same as itself.

# 3. What is the meaning of the non‑diagonal elements?
# This represents how similar each site in the 
# horizontal heading is similar to the corresponding site in 
# the vertical heading.

# 4. Take the data in row 1 and create a line graph that shows these values as a function of section number.
sor_1 <- as_tibble(sor_df[1:1, 1:20])
ggplot(data = sor_1, (aes(x = 1:20, y = value))) +
  geom_line(size = 0.8, col = "blue3") +
  labs(title = "Dissimilarity of site 1 to 20 other sites") +
  xlab("Coastal section") + ylab("Dissimilarity") +
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

# 5. Provide a mechanistic (ecological) explanation for why this figure takes the shape that it does.
# The farther the sites are from site one the increasingly
# different they are. Micro climates in each area is very different 
# from other sites.This is one of the reasons each site will have 
# different vegetation and species types. This is why dissimilarity 
# starts off very low but becomes increasingly larger.

