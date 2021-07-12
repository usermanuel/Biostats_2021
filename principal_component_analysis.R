# Title: Quantitative Ecology
# Author: Robyn Manuel
# Purpose: Principal Component Analysis (PCA)
# Date: 12/06/2021



# Load packages -----------------------------------------------------------

library(tidyverse)
library(vegan)


# load the data -----------------------------------------------------------

data <- "https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/DoubsEnv.csv"
ENV <- read.csv(url(data))

# remove first column
env <- dplyr::select(ENV, -1)
head(env)


# Create PCA --------------------------------------------------------------

env_pca <- rda(env, scale = TRUE)
env_pca

round(env_pca$CA$eig[1],3)

# Total inertia:

sum(env_pca$CA$eig)

# Proprtion of variation

round(env_pca$CA$eig[1]/ sum(env_pca$CA$eig) * 100, 1)


# Question A --------------------------------------------------------------

# Why can a PCA, or any ordination for that matter,
# not explain all of the variation in a dataset? In other words, 
# why is it best to only use the first few Principal Components for 
# insight into the drivers of variability? What is 'explained' by the remaining PC axes?

# Ordinations merely highlight the gradients present within the system. Some variables are more 
# influential than others. Some variables have negligible effects on the ecosystem and therefore 
# have no need to be included. The ordination making use of 3 plains, more than 3 plains 
# cannot be displayed visually. Ordinations therefor Focus on the most significant variables.
 
# -- ----------------------------------------------------------------------

summary(env_pca)


# Graphical representations of ordinations --------------------------------




biplot(env_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(env_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))
# type in console: par("mar")
# 5.1.1....etc
# type in console: par(mar = c(1,1,1,1))


#  Cleanplot.pca Biplot ----------------------------------------------


source("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/cleanplot.pca.R")
cleanplot.pca(env_pca, scaling = 1)
cleanplot.pca(env_pca, scaling = 2)



biplot(env_pca, type = c("text", "points"), col = c("black", "black"))
ordisurf(env_pca ~ bod, env, add = TRUE, col = "turquoise", knots = 1)
ordisurf(env_pca ~ alt, env, add = TRUE, col = "salmon", knots = 1)


# Question B --------------------------------------------------------------

# 1. Replicate the analysis shown above on the environmental data included with these datasets:

# A. bird communities along elevation gradient in Yushan Mountain, --------


# load data from site:

data1 <- "https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/ybirds_env.txt"

env1 <- read.delim(url(data1)) %>% 
  select(-`Veg.`, -Station, -Veg_ext) # remove the descriptive variables

head(env1)

# Create PCA:

envT_pca <- rda(env1, scale = TRUE)
envT_pca

round(envT_pca$CA$eig[1],3)

# Total inertia:

sum(envT_pca$CA$eig)

# Proprtion of variation

round(envT_pca$CA$eig[1]/ sum(envT_pca$CA$eig) * 100, 1)

# Graphical Representation

biplot(envT_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(envT_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))

cleanplot.pca(envT_pca, scaling = 1)
cleanplot.pca(envT_pca, scaling = 2)


biplot(envT_pca, type = c("text", "points"), col = c("black", "black"))
ordisurf(envT_pca ~ bod, env, add = TRUE, col = "turquoise", knots = 1)
ordisurf(envT_pca ~ alt, env, add = TRUE, col = "salmon", knots = 1)




# B. Alpine plant communities in Aravo, France. ---------------------------

# load data from site:

library("readxl")


data2 <- as_tibble(read_excel("data/aravo.xlsx")) %>% 
  select(-`...1`, -`ZoogD`) # remove the descriptive variables
view(data2)

# Create PCA:

envA_pca <- rda(data2, scale = TRUE)
envA_pca

round(envA_pca$CA$eig[1],3)

# Total inertia:

sum(envA_pca$CA$eig)

# Proprtion of variation

round(envA_pca$CA$eig[1]/ sum(envA_pca$CA$eig) * 100, 1)

# Graphical Representation

biplot(envA_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(envA_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))

cleanplot.pca(envA_pca, scaling = 1)
cleanplot.pca(envA_pca, scaling = 2)


biplot(envA_pca, type = c("text", "points"), col = c("black", "black"))
ordisurf(envA_pca ~ bod, env, add = TRUE, col = "turquoise", knots = 1)
ordisurf(envA_pca ~ alt, env, add = TRUE, col = "salmon", knots = 1)


# 2. Discuss the patterns observed:

# explain the ordination diagram with particular reference to the major patterns shown;
# provide a mechanistic explanation for the existence of the patterns seen with respect
# to elevation/altitude; and if there are significant positive or negative correlations 
# between the environmental variables, provide mechanistic reasons for how they came about.

# A. explain the ordination diagram with particular reference to the major patterns shown:

# Within the communities the major contributing variables where slope, snow, form, 
# aspect (north or south facing), and physical disturbances.Site falling within the 
# 4th quadrant (counting clockwise), have a string correlation with slope angle.
# Larger site number occur mostly near the base of the plane, indicating that physical disturbances
# and mean snow melt date are strong environmental determinants in these areas.



# B.provide a mechanistic explanation for the existence of the patterns seen with
# respect to elevation/altitude.

# Snow melt, slope and aspect are among the most influential variables here, all of which show a 
# strong relationship with regard to elevation. South facing slopes will melt first due to
# the affects of the sun, in this regard vegetation will in these sites will be exposed to sun first
# encouraging the growth of vegetation requiring lots of sun. North facing slopes will experience less
# sun exposure and taking longer melt. This vegetation will take longer to grow back and be also be
# prefer less sun than those on south facing slopes. 

# C. if there are significant positive or negative correlations between the 
# environmental variables, provide mechanistic reasons for how they came about.


# South facing experiences more sun and is therefore warmer, slope and aspect are
# linked which is why they have a close association. Snow patterns would show a 
# opposite effect of these two variables given that snow will occur on higher,
# north face (colder aspects).
# Physical processes which influence topography will determine the physical disturbances, 
# which is why physical disturbances and Form show a opposite correlating relationship.
# The large the disturbance the less likely formation of vegetation as the soil is not
# in the "correct" state.




