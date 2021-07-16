# Title: Quantitative Ecology
# Author: Roby Manuel
# Purpose: Correspondence Analysis
# Date: 16 July 2021


# Load packages -----------------------------------------------------------


library(tidyverse)
library(vegan)


# Load data: --------------------------------------------------------------


data <- "https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/DoubsSpe.csv"
spe <- read.csv(url(data))

spe <- dplyr:: select(spe, -1) # remove title column
head(spe, 6)


# Do CA -------------------------------------------------------------------

# Do not specify constraints

# Prepare data ------------------------------------------------------------

spe_ca <- cca(spe)
# error meaning that one of the rows = 0. Therefore need to find and omit it.

apply(spe, 1, sum) # calculate sum for each row to find the error row.

spe <- spe[rowSums(spe) > 0, ] # remove rows = less than 0
         
 head(spe, 8)

spe_ca <- cca(spe)
spe_ca

# Inertia = 1.167

summary(spe_ca)

# Total inertia = 1.16691
round(sum(spe_ca$CA$eig), 5)

# Inertia for first axis = 0.60099
round(spe_ca$CA$eig[1], 5)

# Inertia for CA1 and CA2 0.74536
round(sum(spe_ca$CA$eig[1:2]), 5)

# The fraction of variance :
round(sum(spe_ca$CA$eig[1:2])/sum(spe_ca$CA$eig)*100,2)


# Ordination Diagrams -----------------------------------------------------

plot(spe_ca, scaling = 1, main = "CA fish abundance - biplot scaling 1")
plot(spe_ca, scaling = 2, main = "CA fish abundance - biplot scaling 2")


require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(spe, tmp <- ordisurf(spe_ca ~ Satr, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Satr"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Scer, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Scer"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Teso, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Teso"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Cogo, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Cogo"))
abline(h = 0, v = 0, lty = 3)

# a posteriori projection of environmental variables in a CA
data2 <- "https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/DoubsEnv.csv"
env <- read.csv(url(data2))
env <- dplyr::select(env, -1)

# we removed the 8th row in spe, so do it here too
env <- dplyr::slice(env, -8)

# the last plot produced (CA scaling 2) must be active
# scaling 2 is default
(spe_ca_env <- envfit(spe_ca, env, scaling = 2))
plot(spe_ca_env, col = "grey40")
plot(spe_ca_env, p.max = 0.05, col = "red") # plot significant variables with a different colour

# RESET PLOT PARAMETERS : par(mfrow = c(1,1))


# Questions ---------------------------------------------------------------


# 1. How would you explain the patterns seen in the four panels of the above figure?

# Satr has a large influence on the community structures at sites above the lower areas.
# At the higher points altitude and slope are the positively correlated environmental 
# factors. Flow, nitrogen content and distance from site are negatively correlated at 
# sites containing with large Satr abundances at higher points.
# Scer has a large occurrence at sites located at lower locations. Scer has very
# little influences at higher locations, which are more abundantly populated with satr.
# At sites where Scer is abundant, sites are positively correlated with water flow,
# nitrogen content and distance from the site. 
# Teso and Cogo influence the community structure at sites located at the middle area
# of the river. High levels of oxygen are present at sites which contain Teso and Cogo.

  
# 2. Apply approaches taken from the analysis shown immediately above to these datasets:
  
# A. bird communities along elevation gradient in Yushan Mountain, Taiwan;

# Load data:
data3 <-"https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/ybirds_spe.txt" 
spe_T <- read.delim(url(data3))
head(spe_T) # view data
spe_T <- dplyr:: select(spe_T, -1) 
 
# Perform CA:

apply(spe_T, 1, sum) # calculate sum for each row to find the error row.
spe.bird <- spe_T[rowSums(spe) > 0, ] 

     
spe.bird_ca <- cca(spe.bird)
summary(spe.bird_ca)

# Inertia = 2.008
round(sum(spe.bird_ca$CA$eig), 5)

# Inertia for CA1 and CA2 
round(sum(spe.bird_ca$CA$eig[1:2]), 5)

# The fraction of variance :
round(sum(spe.bird_ca$CA$eig[1:2])/sum(speT_ca$CA$eig)*100,2)

# Ordination Diagrams
plot(spe.bird_ca, scaling = 1, main = "CA bird abundance - biplot scaling 1")
plot(spe.bird_ca, scaling = 2, main = "CA bird abundance - biplot scaling 2")

# influential sp CA1: (positive) ALA, WRN and (negative)  SWP, ILT

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(spe.bird, tmp <- ordisurf(spe.bird_ca ~ ALA , bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "ALA "))
abline(h = 0, v = 0, lty = 3)
with(spe.bird, tmp <- ordisurf(spe.bird_ca ~ WRN, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "WRN"))
abline(h = 0, v = 0, lty = 3)
with(spe.bird, tmp <- ordisurf(spe.bird_ca ~ SWP, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "SWP"))
abline(h = 0, v = 0, lty = 3)
with(spe.bird, tmp <- ordisurf(spe.bird_ca ~ ILT, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "ILT"))
abline(h = 0, v = 0, lty = 3)

# a posteriori projection of environmental variables in a CA
data3en<- "https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/ybirds_env.txt"

env_bird <- as_tibble(read.delim(url(data3en))) %>% 
  select(-`Veg.`, -`Veg_ext`) 
env_bird <- dplyr::select(env_bird, -1)

head(env_bird)

# the last plot produced (CA scaling 2) must be active
# scaling 2 is default
(spe.bird_ca_env <- envfit(spe.bird_ca, env_bird, scaling = 2))
plot(spe.bird_ca_env, col = "grey40")
plot(spe.bird_ca_env, p.max = 0.05, col = "blue") # plot significant variables with a different colour

# 3. Discuss the patterns observed, and explain the ordination diagrams with particular
#    reference to how the species are influenced by the major environmental drivers.

# Species ALA is  46 to 50, where ground cover plays the largest part in a large 
# part in environmental structure. Species WRN occurs in site number 33 and 
# higher, there sites occur at higher locations.Ground cover at there sites are high.
# Species SWP is abundant at Site 2, and scares at other sites. site 2 is at a low laying area.
# Site two is an area where shrubs are a common occurrence, which is why it is negatively mean tree 
# height. In areas with lower tree cover shrub vegetation have better access to resources 
# such as sunlight.Similarly ILT is a common species to occur in areas which have a positive correlation with 
# shrub growth, hence its ILT's occurrence at sites 1 through 5.

# B. alpine plant communities in Aravo, France.

library("readxl")

# Load data:
data3 <- "data/aravo_spp.xlsx" 
spe_ar <- read_excel(data3)
head(spe_ar) # view data

spe_ar <- dplyr:: select(spe_ar, -1) # remove first column
head(spe_ar)

# Perform CA:
spe.ar_ca <- cca(spe_ar)
spe.ar_ca
summary(spe.ar_ca)


# Inertia = 4.214
round(sum(spe.ar_ca$CA$eig), 5)

# Inertia for CA1 and CA2 
round(sum(spe.ar_ca$CA$eig[1:2]), 5)

# The fraction of variance :
round(sum(spe.ar_ca$CA$eig[1:2])/sum(speF_ca$CA$eig)*100,2)

# Ordination Diagrams
plot(spe.ar_ca, scaling = 1, main = "CA plant abundance - biplot scaling 1")
plot(spe.ar_ca, scaling = 2, main = "CA plant abundance - biplot scaling 2",display = "sites")



# influential sp CA1: (positive) Anth.alpe, Drya.octo (Bart.alpi,Sali.retu  ) and (negative)  Poa.supi, Card.alpi


require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(spe_ar, tmp <- ordisurf(spe.ar_ca ~ Anth.alpe, bubble = 3,
                            family = quasipoisson, knots = 2, col = 6,
                            display = "sites", main = "Anthyllis vulneraria L.  subsp. alpestris (Kit.) Ascherson & Graebner"))
abline(h = 0, v = 0, lty = 3)
with(spe_ar, tmp <- ordisurf(spe.ar_ca ~ Drya.octo, bubble = 3,
                            family = quasipoisson, knots = 2, col = 6,
                            display = "sites", main = "Dryas octopetala L."))
abline(h = 0, v = 0, lty = 3)
with(spe_ar, tmp <- ordisurf(spe.ar_ca ~ Poa.supi, bubble = 3,
                            family = quasipoisson, knots = 2, col = 6,
                            display = "sites", main = "Poa supina Schrader"))
abline(h = 0, v = 0, lty = 3)
with(spe_ar, tmp <- ordisurf(spe.ar_ca ~ Card.alpi, bubble = 3,
                            family = quasipoisson, knots = 2, col = 6,
                            display = "sites", main = "Cardamine bellidifolia L.  subsp. alpina (Willd.) B.M.G. Jones"))
abline(h = 0, v = 0, lty = 3)

# par (mfrow = c(1,1))
# a posteriori projection of environmental variables in a CA
library("readxl")

env_ar <- as_tibble(read_excel("data/aravo.xlsx")) %>% 
  select(-`...1`, -`ZoogD`) 
head(env_ar)

# activate CA scaling 2

# scaling 2 is default

(spe.ar_ca_env <- envfit(spe.ar_ca, env_ar, scaling = 2))
plot(spe.ar_ca_env, p.max = 0.05, col = "blue") 

# 3. Discuss the patterns observed, and explain the ordination diagrams with particular
#    reference to how the species are influenced by the major environmental drivers.

# Anthyllis vulneraria L.  subsp. alpestris is very abundant at sites 2 and 11. Where physical disturbance
# has a very positive correlation with environmental variables as well as slope. 
# Dryas octopetala L. has a large influence on structures at sites 2 and 6, and 
# very small abundances at other sites. Poa supina Schraderhas a large abundance at 
# sites 49, 50, 51 and at sites 54, 61, 66 although small their abundance is still
# significant. At these location microtopographic land forms are a large environmental influence.
# Cardamine bellidifolia L.  subsp. alpina (Willd.) is very closely correlated with 
# microtopographic land forms. Sites 59, 60, 58, 53 and 55 have a large abundance and
# direct correlation with microtopographic land forms. Sites with similar composition
# moving away contain Cardamine bellidifolia L.  subsp. alpina (Willd.) in smaller compositions 
# and the farthest sites contains very little to none. With an exception of site 61 and 51
# which have a significantly large population. 

# Dryas octopetala L. and Anthyllis vulneraria L.  subsp. alpestris can be found in
# environmnents with similar species sturctures with these two species likely 
# being the one of the large structure influences in the environment. Where 
# Dryas octopetala L. and Anthyllis vulneraria L.  subsp. alpestris are found in very
# low abundances Poa supina and Cardamine bellidifolia L.  subsp. alpina (Willd.) are found 
# where they occur over a wide variety of sites. These site are also very similar  species structure.
#

# Dryas octopetala L., Anthyllis vulneraria L.  subsp. alpestris and Poa supina
# occur more abundantly in selected sites, as such Cardamine bellidifolia L.  subsp. alpina (Willd.)
# is left with the remaining niche available which is why it occurs along a wider range of sites.
# Physical disturbances likely influencing the occurrence of Dryas octopetala L.and 
# Anthyllis vulneraria L.  subsp. alpestris. Microtopographical landforms likely being 
# an environmental determinant for the occurence of Poa supina. However, Cardamine 
# bellidifolia L.  subsp. alpina (Willd.) does apprear to have a stronger correlation with 
# form than Poa supina. Cardamine bellidifolia is associated with wetlands which is 
# likely why it has a close correlation to form and snow as it would lkely near 
# areas where run-off from melted snow will occur.







