# Title: Quantitative Ecology
# Purpose: Environmental Measures
# Author: Robyn Manuel
# Date: 30 June 2021

library(vegan)
library(ggplot2)
#install.packages("geodist")
library(geodist)              #distances between lat and long
#install.packages("ggpubr")
library(ggpubr)
library(readr)
library(ggplot2)


# Fetch and load the data -------------------------------------------------

data <- "https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/diversity/Euclidian_distance_demo_data_xyz.csv" 
xyz <- read.csv(url(data))

# examine the data:

dim(xyz) #dimensions 
str(xyz)
xyz


# Euclidean distances -----------------------------------------------------


# Distance between two points capture within a volume of x,y,z
xyz_euc <- round(vegdist(xyz[, 2:4], method = "euclidian", upper = FALSE, diag = TRUE), 4) # select only cols 2, 3 and 4
# limit data to columns 2, 3, 4.
# upper = false no upper triangle visible
xyz_euc
# values =  shortest straight line distance between two points in a 3D space.
# diagonal = distance between site and itself is -> 0.
# i.e. distance between point 2 and 3 = 1.7321
class(xyz_euc)

# difficult to apply calculations to a distance matrix so create a df:
xyz_df <- as.data.frame(as.matrix(xyz_euc))
# first create a matrix then create a data frame

xyz_df

# know locations relative to a fixed point.



# Environmental Data Example ----------------------------------------------

data_2 <- "https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/diversity/Euclidian_distance_demo_data_env.csv" 
env_fict <- read.csv(url(data_2))
head(env_fict, 2) # print first two rows only
# How different are the sites.

env_f_euc <- round(vegdist(env_fict[, 2:4], method = "euclidian", upper = FALSE, diag = TRUE), 4) # select only cols 2, 3 and 4
env_f_euc

# seaweed environmental data ----------------------------------------------
# corresponding environmental data to seaweed data.

load("data/SeaweedEnv.RData")

# lets look at the data
dim(env)
 env

#veiw row 1 - 5 and column 1 - 5, round off to 4th decimal.
round(env[1:5, 1:5], 4)

# last 5 rows and columns:
round(env[(nrow(env) - 5):nrow(env), (ncol(env) - 5):ncol(env)], 4)

# view column names
colnames(env)
# select for required variables: 

env1 <- dplyr::select(env, febMean, febRange, febSD, augMean,
                      augRange, augSD, annMean, annRange, annSD)

dim(env1)

# z-scores
# standardization allow everything to have a mean of 0 and a SD of 1
# scales all data.

E1 <- round(decostand(env1, method = "standardize"), 4)
E1[1:5, 1:5]


# Euclidean distance

# vegdist apply on standardization 
E1_euc <- round(vegdist(E1, method = "euclidian", upper = TRUE), 4)
E1_df <- as.data.frame(as.matrix(E1_euc ))
E1_df[1:10, 1:10] # the first 10 rows and columns


Euc_dist <- ggplot(data = E1_df, (aes(x = 1:58, y = `1`))) +
  geom_line() + xlab("Coastal section, west to east") + ylab("Environmental distance") +
  ggtitle ("Euclidean distance") +
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


# Euclidean distances of geographical data --------------------------------



geo <- read.csv("data/sites.csv")
 
# Examine the data:
dim(geo)
head(geo)


# Determine geographical distance between sites ---------------------------

dists <- geodist(geo, paired = TRUE, measure = "geodesic")
dists_df <- as.data.frame(as.matrix(dists))
colnames(dists_df) <- seq(1:58)
dists_df[1:5, 1:5]


# Plot the site distances --------------------------------------------------

plt1<- ggplot(data = dists_df, (aes(x = 1:58, y = `1`/1000))) +
  geom_line() + 
  xlab("Coastal section, west to east") + ylab("Distance (km)") + 
  ggtitle ("Actual geographic distance") +
  theme(panel.grid.major = element_line(size = 0.7, linetype = 2),
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
plt1

# Geographical distances using euclidean ----------------------------------

# euclidean distance using lat and long:

dists_euc <- vegdist(geo, method = "euclidian")
dists_euc_df <- round(as.data.frame(as.matrix(dists_euc)), 4)
dists_euc_df[1:5, 1:5]

plt2 <- ggplot(data = dists_euc_df, (aes(x = 1:58, y = `1`))) +
  geom_line() +
  xlab("Coastal section, west to east") + ylab("Euclidian distance") +
  ggtitle("Euclidian distance") +
  theme(panel.grid.major = element_line(size = 0.7, linetype = 2),
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

plt2

  ggarrange(plt1, plt2, nrow = 2) +
    ggtitle("Euclidian distance compared to Geographical distances \n") +
    theme(panel.grid.major = element_line(size = 0.7, linetype = 2),
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

# Discussion --------------------------------------------------------------

# euclidean matrices (vegdist <- method = euclidean ) can be used to determine how similar two environments are
# based on similarities between the variables recorded in each environment.
# This data can also tell us where for example where vegetation may begins to change and  
# why vegetation will change (due to change in environmental factors quantity changing).
# The Euclidean matrices data can also tell us the shortest straight line distance between
# two points in a 3D space. 
# You would have to turn the distance data or variable values into a euclidean matrix. 
# Variables will have to be standardize if different unit measurements are used, to ensure one variable does 
# not sway the information due to its amount the unit of measurement produces.
  

