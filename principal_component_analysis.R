# Title: Quantitative Ecology
# Author: Robyn Manuel
# Purpose: Principal Component Analysis (PCA)
# Date: 13/06/2021



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

biplot(envT_pca, scaling = 1, choices = c(1, 2), main = "PCA scaling 1")
biplot(envT_pca, scaling = 2, choices = c(1, 2), main = "PCA scaling 2")


source("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/cleanplot.pca.R")
cleanplot.pca(envT_pca, scaling = 1)
cleanplot.pca(envT_pca, scaling = 2)



biplot(envT_pca, type = c("text", "points"), col = c("black", "black"))
ordisurf(envT_pca ~ bod, env, add = TRUE, col = "turquoise", knots = 1)
ordisurf(envT_pca ~ alt, env, add = TRUE, col = "salmon", knots = 1)





# 2. Discuss the patterns observed for A:

# 2.a.explain the ordination diagram with particular reference to the major patterns shown:

# The sites are scattered along the Cartesian plane in anti-clockwise direction.
# Sites one to sixteen occur in the fourth quadrant, within this quadrant 
# aspect, secondary tree cover, tree density, tree species density and herb cover are the 
# largest environmental influences. Sites 17 to 31 occur within the third quadrant; total 
# foliage volume, canopy height, canopy cover, foliage height diversity index, Tree basal 
# area and tree species diversity. Sites 32 to 45 fall within the second quadrant, mean 
# tree DBH, slope, elevation, conifer percentage and exposure. only four sites (46 to 50) 
# occur within the first quadrant where ground cover and shrub cover contribute towards the characteristics of these sites.
# Foliage height diversity index, mean tree DBH, standard deviation of tree DBH, elevation and 
# tree basal area have a the largest influence on all the dimensions.  


# 2.b.provide a mechanistic explanation for the existence of the patterns seen with respect to elevation/altitude.

# As site number increases the elevation increases. Sites 33 to 50 occur higher than those at lower numbers 
# which is why they occur with the positive elevation vector. Variables being strong drivers within the lower
# variables such as tree density, tree species density and herb cover can be expected as at lower heights there 
# is less exposure to hashes weather elements which would influence vegetation and tree grow. 
# The highest points occurring at sites from 46 to 50 show high influences from high shrub cover 
# and ground cover, which would be more sustainable in these weather conditions.

# 2.c. If there are significant positive or negative correlations between the 
# environmental variables, provide mechanistic reasons for how they came about.

# Tree basal area and standard deviation of tree have a negative correlation to shrub coverage. In areas which are more 
# conducive to tree growth, trees would cover most of the area blocking sun from many smaller vegetation such as shrubs.
# This is why shrubs populations would be higher in area where trees are less prevalent. Similarly elevation and conifer 
# percentage have a positive correlation as conifers can better survive in these elevated environments compared to that of trees




# B. Alpine plant communities in Aravo, France. ---------------------------

# load data from site:

library("readxl")


data2 <- as_tibble(read_excel("data/aravo.xlsx")) %>% 
  select(-`...1`, -`ZoogD`) # remove the descriptive variables


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

# 2. Discuss the patterns observed for A:

# 2.a.explain the ordination diagram with particular reference to the major patterns shown:
# There is no obvious patterns with regards to the sites location. Slope, microtopographic landform index
# and physical disturbances play large parts in all dimensions.Aspect apears to have very small influence over environments.
# Sites 45, 41, 40, 53, 44 show a very strong relationship to form, indicating the sites may occur along
# concave grounds. The lack of site occurrence patterns shows that site
# occur on very different terrain. 
# Many of the sites with larger number occur within or near the second quadrant.


# 2.b.provide a mechanistic explanation for the existence of the patterns seen with respect to elevation/altitude.
# Sites with larger numbers occur on low laying areas, where snowmelt will start.

# 2.c. If there are significant positive or negative correlations between the 
# environmental variables, provide mechanistic reasons for how they came about.
# Aspect has a strong positive correlation with slope, and both show a negative 
# correlation with snowmelt. South facing slopes in the Northern hemisphere 
# are exposed to more sun than those facing north, as such slopes facing southward will
# will experience sun exposure melting the snow at the lower points first and therefore
# sites occurring at those points will experience snow melt first. Areas at higher sites will melt at a later date.
# Form has a negative correlation to physical disturbances. Convex grounds will have 
# slightly more expose to weather elements such as sun, allowing for more conducive 
# environments for vegetation growth. Concave ground make a ideal area for snow to collect
# and make the environment less suitable for vegetation growth.
































