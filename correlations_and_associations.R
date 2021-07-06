# Title: Quantitative Ecology
# Author: Robyn Manuel
# Purpose: Correlations and associations (Topic 6)
# Date :6 July 2021 


# Load packages:
 library(tidyverse)
 library(vegan)
 library(Hmisc)
 library(ggpubr)
 library(corrplot)
 
 # load data
  
data <- "https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/DoubsEnv.csv"
ENV <-read.csv(url(data))
ENV

# Examine data:
dim(ENV)
head(ENV, 1)# allows you to observe the first line in the data frame
str(ENV)

# Remove the first column:

ENV <- dplyr::select(ENV, -1)
head(ENV, 1)

# correlations: used to observe how the environmental variables relate to each other across 
# the sample site.

round(cor(ENV), 2)

# statistical significance (p-values)
 rcorr(as.matrix(ENV))


# Question (A) ------------------------------------------------------------

# 1. create a plot of pairwise correlations

# continuous data
 
env <- as.matrix(round(cor(ENV), 2))


corrplot(env, method = "circle", type = "upper", 
         col = colorRampPalette(c("darkorange","bisque1","cadetblue"))(100)) 


# 2. Name to two top positive and two top negative statistically-significant correlations.

# Positive:
# Ammonium and phosphate concentrations
# distance from the source and water flow

# Negative:
# water flow and Altitude
# Dissolved oxygen and Biological oxygen demands

# 3. For each, discussion the mechanism behind the relationship exist? 

# Ammonium and phosphate concentrations:
# Ammonium is comprised of nitrogen and hydrogen, phosphate is comprised of 
# phosphorus and oxygen. As water molecules breaks down hydrogen and oxygen atoms
# begin to increase in the water. This allows for phosphate and nitrogen atoms to attach
# as hydrogen atoms becomes available oxygen atoms do as well.

# distance from the source and water flow:
# The structure of the surface the water is traveling over will have an affect 
# on the rate of water flow. Water sources are often found at higher altitudes 
# than the river itself.In this way water running from high ground to lower 
# ground will move faster due gravity, and water closest to the source will move 
# faster when moving away from the source. Turbulent waters provide upwelled 
# nutrients and other particles for the fish to feed on which may present an inviting area for feeding at.

# Negative:

# water flow and Altitude:
# This relationship is linked to the force of gravity.The higher the altitude the
# faster the water will move to get to lower grounds.

# Dissolved oxygen and Biological oxygen demands:
# As oxygen declines in the waters Anaerobic process' will begin to take place.
# Organisms which require oxygen will begin to die hence demand for biological 
# oxygen will decrease. 

# Association between species ---------------------------------------------

data_1 <- "https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/DoubsSpe.csv"
spe <- read.csv(url(data))

spe <- dplyr::select(spe, -1)

head(spe)

spe_t <- t(spe)


# Section B ---------------------------------------------------------------
# 1. Why do we need to transpose the data?
# The data produced from the correlation matrix above compares each of 
# the environmental conditions. The data will need to be transposed in order to 
# compare site and its associated species by each other on how their 
# environmental conditions differ.
# In order to see a pattern.
  
# 2. What are the properties of a transposed species table?

# If you take a transposed matrix and transpose is once again the matrix 
# will formed will be the original.
# If you add two matrices, add the together then transpose the final matrix it 
# will be equal to transposing each individual matrix then finding their sum total.
# By first transposing a given matrix followed by multiplying it  by a constant
# the final matrix will still be equal to first multiplying the matrix by he
# constant followed by transposing it.
# A matrix formed when two matrices are multiplied will be equal to the matrix produced 
# when two matrices are transposed and multiplied in the reverse order which 
# the first mention matrix was formed



# --- ---------------------------------------------------------------------

spp_assoc1 <- vegdist(spe_t, method = "jaccard")
as.matrix(spp_assoc1)


spp_assoc2 <- vegdist(spe_t, method = "jaccard", binary = TRUE)
spp2 <- as.data.frame(as.matrix(spp_assoc2))
spp2




# Question C --------------------------------------------------------------

# 2. What are the properties of an association matrix? .
# 

# How do these properties differ from that of a: 
#i) species dissimilarity matrix and from a 

# dissimilarity matrices contain either a 1 or a 0 to depict presence and 
# absence, where as the the association matrix contains decimals to
# indicate association. Additionally, species dissimilarity matrices compare 
# species similarity between location

#ii) correlation matrix?
# Correlation matrices show how much environmental variables have an influence
# on each other. 

#2. What is the difference between spp_assoc1 and spp_assoc2? Is the information 
# contained in each markedly different from the other?
   
# spp_assoc1 shows how the environmental variables affect the species distributions 
# in each site.
# The binary true function form as a standardization on the data.

# Explain the kind of insight we are able to glean from a species association matrix.


# Discussion --------------------------------------------------------------

# Part A looks at the environmental variables it each site and how influential 
# each variable recorded will be on all other variables and if there is any
# statistical influence of interest.The association between environmental 
# variable at each site and the species which occur there are of importance 
# in order to see how major external factor influencing 
# one variable will influence other variables and the environment as a whole. 









