# Title: Quantitative Ecology
# Author: Robyn Manuel
# Purpose: Species Dissimilarity exercise
# Date: 05 July 2021
# load packages:

library(tidyverse)
library(vegan)
library(ggplot2)
library(ggpubr)
library(readr)
library(ggplot2)
library(BiodiversityR)
# load data
data <- "https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/DoubsSpe.csv"
spe <- read.csv(url(data))

# Question 1 --------------------------------------------------------------
# Look at the dataset and explain its structure in words.

# examine data:
dim(spe) # 30 rows, 28 columns
str(spe)
head(spe)
# unique(spe)
spe <- dplyr::select(spe, -1) # removed the first column containing site number
dim(spe) # 30 rows, 27 columns
spe
# The data was collected from the Doubs river at 30 different locations, each 
# location is represented by each row.The very first row contains abbreviations 
# for 27 fish species names. Each column represents a species of fish, it first 
# reads 28 due to the extra column provided for locations. Due to the numbers 
# within each cell exceeding 1 the data frame contains abundance data. A 0 value
# mean that the species (represented by the column) does not occur at that location
# (represented by the row number), any number greater indicates the amount of individuals
# observed in the area.


# Question 2 --------------------------------------------------------------
# Would we use Bray-Curtis or Jaccard dissimilarities?

# The data frame contains abundance data, therefore the Bray-Curtis statistical 
# analysis methods will be used.


# Question 3 --------------------------------------------------------------
# Apply the calculation.
SPE <- round(vegdist(spe, method = "bray"), 4)
SPE 
 

# Question 4 --------------------------------------------------------------
# Explain the meaning of the results in broad terms.

# Each column  represents a location and each row represents a different location.
# Dissimilarity between data shows the dissimilarity of the species between at a site 
# represented by a column and corresponding sites represented by the row. i.e. 
# site 1 and site 2 have a dissimilarity of 0.6000.


# Question 5 --------------------------------------------------------------
# Examine it more closely: what general pattern comes out?
# Locations become ever increasingly dissimilar, as the distance from the first site
# increase. After site 15 there is a slight increase in similarity, followed by a increase 
# in dissimilarity. 

# Question 6 --------------------------------------------------------------
# Plot this pattern (hint, it is best seen in the 1st column of the dissimilarity matrix).

spe_data <- as.matrix(SPE) # create a data matrix
spe_ <- as.data.frame(spe_data) # transform the data matrix into a data frame
dim(spe_) # observe the dimensions
# create plot:


spe_plot <- ggplot(data = spe_, aes(x = 1:30, y = spe_[,30])) +
  geom_area(col = "coral3", fill = "coral3") +
  xlab("Location number") + ylab("species dissimilarity") +
  ggtitle ("Species dissimilarity of location 30 from 29 other locations") +
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
spe_plot

# Question 7 --------------------------------------------------------------
# What explanation can you offer for this pattern?

# The pattern shows a high level of dissimilarity until location 15 where a decline of
# dissimilarity occurs followed by a sheep increase of dissimilarity then another wave of similarity
# increase. The graph shows a steep increase of similarity in near location 5.


# Question 8 --------------------------------------------------------------
#	Using the decostand() function, create presence/absence data, and apply 
# the appropriate vegdist() function to obtain a suitable dissimilarity matrix.


# create calculate dissimilarity by creating jaccard index
 
 sp <- decostand(spe, method = "pa") # transform the data into a presence/absense matrix
sp_dis <- vegdist(sp,method = "jaccard", binary = TRUE)
sp


# Question 9 --------------------------------------------------------------
# Create another plot and explain the pattern.

sp_data <- as.matrix(sp_dis) # create a data matrix
sp_jac <- as.data.frame(sp_data) # transform the data matrix into a data frame
dim(sp_jac) # observe the dimensions
# create plot:


sp_data_plot <- ggplot(data = sp_jac, aes(x = 1:30, y = sp_jac[,1])) +
  geom_area(col = "yellowgreen", fill = "yellowgreen") +
  xlab("Location number") + ylab("species dissimilarity") +
  ggtitle ("Species dissimilarity of location 30 from 29 other locations") +
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
sp_data_plot

# The graph shows a level of consistency within the data between 1 and  0. 
# The data no longer shows a large declines in similarity between site one and other sites.
# However the declines and increases of dissimilarity are evident.


# Discussion --------------------------------------------------------------

# The abundant data was statistically analysised using Bray-Curtis and a graph from this
# new data. The same data was converted into a presence/absence matrix and a new graph
# was created from this matrix once Jaccard index was used on the presence/absence data.
# The two graphs where created to see the difference in variation of results due to 
# the data matrix types being different hence changing the same data slightly.
# The pattern shows by the data remains evident however one shows a more defined 
# environmental change than the other.

# Question?
# can you create/ are you suppose to create the presence/absence  using 
# method = "standardize"?

# Because i used the coding below the night before and it worked despite the 
# errors that showed up and it gave me a graph that looked similar to all the other graph. 
# Its not working now but i if you can use it through method = standardize

# Other method --------------------------------------------------------------
# Is this another method to create a data matrix? 

s <- decostand(spe, method = "standardize")
s
f <- vegdist(s,method = "sor", binary = TRUE)
f

sor<- as.matrix(f) # create a data matrix
sore <- as.data.frame(sor) # transform the data matrix into a data frame


sore_2 <- ggplot(data = sore, aes(x = 1:30, y = sore[,1])) +
  geom_area(col = "mediumseagreen", fill = "mediumseagreen") +
  xlab("Location number") + ylab("species dissimilarity") +
  ggtitle ("Species dissimilarity of location 30 from 29 other locations") +
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
sore_2



