# Title: Quantitative Ecology 
# Author: Robyn Manuel
# Purpose: non-Metric multidimensional scaling
# Date Due: 23 July 2021


# Load packages -----------------------------------------------------------

library(tidyverse)
library(vegan)


# Load  species data -------------------------------------------------------------

data1 <- "https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/DoubsSpe.csv"
spe <- read.csv(url(data1))
spe

# Prepare species data ----------------------------------------------------

spe <- dplyr::select(spe, -1) # remove first column
spe <- dplyr::slice(spe, -8) # remove row where sum equals 0

# Do the nMDS -------------------------------------------------------------

spe.nmds <- metaMDS(spe, distance ="bray")

spe.nmds # stress = 0.102


# Ordination Diagrams -----------------------------------------------------

# console: par(mfrow = c(2, 2))

# Shepard plot ------------------------------------------------------------

stressplot(spe.nmds, main = "Shepard plot")
ordiplot(spe.nmds, type = "t", cex = 1.5, main = paste0("nMDS stress = ", round(spe.nmds$stress, 2)))


# Goodness of fit ---------------------------------------------------------

gof = goodness(spe.nmds)
plot(spe.nmds, type = "t", main = "Goodness of fit")
points(spe.nmds, display = "sites", cex = gof * 200) # bigger bubbles indicate a worse fit


# nMDS fish abundances ----------------------------------------------------

pl <- ordiplot(spe.nmds, type = "none", main = "nMDS fish abundances ")
points(pl, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl, "species", col = "blue4", cex = 0.9)
text(pl, "sites", col = "red4", cex = 0.9)



require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(spe, tmp <- ordisurf(spe.nmds ~ Satr, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Satr"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe.nmds ~ Scer, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Scer"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe.nmds ~ Teso, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Teso"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe.nmds ~ Cogo, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Cogo"))
abline(h = 0, v = 0, lty = 3)


# Add environmental data --------------------------------------------------

data2 <- ("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/DoubsEnv.csv")
env <- read.csv(url(data2))

env <- dplyr::select(env, -1) 
env <- dplyr::slice(env, -8) # remove row which doesn't have species data

spe.nmds.env <- envfit(spe.nmds, env) 
plot(spe.nmds.env, col = "grey40")
plot(spe.nmds.env, p.max = 0.05, col = "red")





# Questions ---------------------------------------------------------------


# Questions 1 -------------------------------------------------------------
# Using two unconstrained ordination techniques of your choice, analyse the 
# mite data in the vegan package. Provide a brief description and discussion 
# of what you have found, and produce the R code.


# About the mite data -----------------------------------------------------
# The species occurrence of mites at a location was tested by Daniel Borcard in 1989.
# Seventy different soil samples where taken from different locations and species
# found in each sample were counted and identified, these details where recorded. 
# Environmental data from each site was collected and recorded. 

# CA mite data ------------------------------------------------------------
 
# Mite species data will be examined using correspondence analysis, this will allow for 
# the none linearity of the data to be preserved. Species data is commonly less 
# linear than that of environmental data. CA allows for the retention of the none linearity 
# more than Principle components analysis. 

# Load the data -----------------------------------------------------------
#data(package = "vegan") to observe available data in packages.

data(mite) # load the data on to the script
mite # veiw the data

# Examine the data --------------------------------------------------------

# console : ?mite
# mite.env (environmental data)
# mite.xy (geographic coordinates)

summary(mite) 
# summaried details of seach mite species.
head(mite, 5) # view first 5 rows


# Perform Ordination ------------------------------------------------------
# Perform Corresponding analysis due to it being species data.
# CA will allow for a better analysis as it preserves the linearly.
mite.ca <- cca(mite)

# Examine mite.ca
summary(mite.ca) 

# Inertia = 1.696
round(sum(mite.ca$CA$eig), 3)

# The fraction of variance : 44.36
round(sum(mite.ca$CA$eig[1:2])/sum(mite.ca$CA$eig)*100,2)

# Ordination Diagram ------------------------------------------------------

plot(mite.ca, scaling = 1, main = "CA mite abundance - biplot scaling 1")
plot(mite.ca, scaling = 2, main = "CA mite abundance - biplot scaling 2")

# Trhypch1  0.84085
# LCIL      0.99944
# Miniglmn -1.01370
# RARD     -1.01857

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(mite, tmp <- ordisurf(mite.ca ~ Trhypch1, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Trhypch1"))
abline(h = 0, v = 0, lty = 3)
with(mite, tmp <- ordisurf(mite.ca ~ LCIL, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "LCIL"))
abline(h = 0, v = 0, lty = 3)
with(mite, tmp <- ordisurf(mite.ca ~ Miniglmn, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Miniglmn"))
abline(h = 0, v = 0, lty = 3)
with(mite, tmp <- ordisurf(mite.ca ~ RARD, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "RARD"))
abline(h = 0, v = 0, lty = 3)

# Add environmental variables ---------------------------------------------
data(mite.env) # load mite data

(mite.ca_env <- envfit(mite.ca, mite.env, scaling = 2)) # prepares the environmental vectors for to be place on ordination
plot(mite.ca_env, col = "grey40")
plot(mite.ca_env, p.max = 0.05, col = "red") 

# Mite analysis: CA -----------------------------------------------------------
# Based on scaling 1 sites with similar species composition and species frequency
# are sites 5 to 29 (excluding sites 1, 2,3,4,9,15,19,23 and 28); ONOV, Brachy, 
# MPRO and PWIL are the most abundant species at these locations. Sites 39 to 42,
# 48, 52, 50, 55, 57, 61, 65 and 68 form a clustering of sites as well, another 
# cluster of site containing 19, 23, 30, 32 to 37, 46,69 and 63. Sites 43, 45, 
# 59 and 67 form a very small cluster where Trhypch1 and LCIL are large roles
# in community composition.

# Scaling two shows the relationship between species within sites. Within quadrant 
# 4 there is the largest custering of species, Miniglmn, TVEL and RARD being part of it.
# LRUG, PLAG2, Ceratoz3 are similar in species abundance at sites similarly Eupelops
# SUCT, NPRA, ONOV and Brachy are clustered together indicating their species relationship

# The most influential mite species are Trhypch1, LCIL, Miniglmn and RARD, these
# species affect what the communities across the space.
# Trhypch1 has significantly high abundances at sites 43, 44, 59, 65 and 69. LCIL's
# largest abundance occurs at site 67. Trhypch1 and LCIL are very closely related;
# Both occur at similar abundances and at sites where they commonly occur the species
# compositions are similar.
# Miniglmn has large abundances at site 14 and 18. As the site numbers increase 
# Miniglmn abundance becomes less. RARD occurs in high level of abundances at sites 
# 5, 6, 11, 12 and 13. Miniglmn and RARD are closely related based on the sites
# where they occur are similar in the species compositions.

# Trhypch1 occurs in higher abundances at more sites than LCIL, and the sites where 
# they occur share a similarity in the species composition. Give Trhypch1's higher 
# frequency at sites, it is likely that Trhypch1 is the better competitor at these
# sites compared to LCIL. Miniglmn and RARD show a similar relationships; where RARD
# has higher frequencies at multile sites, and Miniglmn has high abundances at few sights
# with similar relationships (of species composition) between there site where they occur.
# Indicating the RARD is a stronger competitor than Miniglmn. 

# Environmental factors which are most dominant at sites where RARD and Miniglmn are 
# Microtopography Hummock, with many shrubs present and substrate type
# Sphagn3 and 4. LCIL have high shrub density with the substrate type being Barepeat
# Trhypch1 has a high abundance at site 69 the substrate type is interface.
# At site 65, Trhypch1 has a high abundance, this site lacks shrubs and 
# has a blanket microtopography.

# PCoA of mite data -------------------------------------------------------

# mite data was already loaded in the previous ordination.

mite.bray <- vegdist(mite)

#mite.pcoa <- cmdscale(mite.bray, k = nrow(mite) -1, eig = TRUE)
mite.pcoa <- capscale(mite.bray ~ 1)

mite.pcoa <- capscale(mite ~ 1, distance = "bray")

# Examine data ------------------------------------------------------------
mite.pcoa
# inertia 14.696
 summary(mite.pcoa)
 # inertia = 16.74

# Ordination diagram ------------------------------------------------------

 plot(mite.pcoa, scaling = 1, main = "PCoA mite abundance - biplot scaling 1")
 plot(mite.pcoa, scaling = 2, main = "PCoA mite abundance - biplot scaling 2")
  
 
 pl1 <- ordiplot(mite.pcoa, type = "none", scaling = 1, main = "PCoA mite abundances - biplot scaling 1")
 points(pl1, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
 points(pl1, "species", pch = 21, col = "turquoise", arrows = TRUE)
 text(pl1, "species", col = "blue4", cex = 0.9)
 text(pl1, "sites", col = "red4", cex = 0.9)
 
 pl2 <- ordiplot(mite.pcoa, type = "none", scaling = 2, main = "PCoA mite abundances - biplot scaling 2")
 points(pl2, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
 points(pl2, "species", pch = 21, col = "turquoise", arrows = TRUE)
 text(pl2, "species", col = "blue4", cex = 0.9)
 text(pl2, "sites", col = "red4", cex = 0.9)
 
 
 require('viridis')
 palette(viridis(8))
 par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
 with(mite, tmp <- ordisurf(mite.pcoa ~ Trhypch1, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Trhypch1"))
 abline(h = 0, v = 0, lty = 3)
 with(mite, tmp <- ordisurf(mite.pcoa ~ LCIL, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "LCIL"))
 abline(h = 0, v = 0, lty = 3)
 with(mite, tmp <- ordisurf(mite.pcoa ~ Miniglmn, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Miniglmn"))
 abline(h = 0, v = 0, lty = 3)
 with(mite, tmp <- ordisurf(mite.pcoa ~ RARD, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "RARD"))
 abline(h = 0, v = 0, lty = 3)
 
 data("mite.env")
 (mite.pcoa.env <- envfit(mite.pcoa, mite.env, scaling = 2)) 
 plot(mite.pcoa.env, col = "grey40")
 plot(mite.pcoa.env, p.max = 0.05, col = "red")

# PCoA mite analysis ------------------------------------------------------

# biplot 1 shows that sites 1, 2,3, 4, 38, 43, 59, 61 and 67 are scattered from the other 
# sites which are more clustered. This indicated that most sights are not very varied 
# in species frequencies. The second biplot confirms this given that most species
# occur at very similar frequencies at the sites where they occur. LCIL, ONOV,
# SUCT, TVEL, Brachy, HMIN, Trhypchi and LRUG are the species which have a noticable different
# occurrences at  certain sites. The species which influence the differentiation of sites
# community structure are RARD, LCIL, Miniglmn and Trhypch1.
# Trhypch1 has a high occurrence at sites 44, 59, 53 and 69. 53 and 69 have the same relative
# frequency. Trhypch1 occurrence declines significantly after site 29, these site
# are dominated by substrate type 3 and 4. 
# LCIL experiences a high occurrence at site 67, its occurrences at other sites begin to decrease
# as site numbers decrease. At site 67 substrate density is high, as the substrate type 
# changes to Sphagn LCIL occurrence declines significantly. Sphagn substrate type correlates 
# negatively with shrub density. LCIL occurrence is dependent on the environmental presence of 
# shrubs and substrate type Sphagn.
# Miniglmn has high occurrences at sites with low allocated numbers, 14 having the highest occurrence.
# Miniglmn occurnces decline as site numbers increase. At locations where miniglmn abundances
# are high the substrate type is Sphagn 3, at sites where the substrate type changes Miniglmn
# occurrences decrease significantly. This shows that Miniglmn has a positive correlation with 
# Sphagn 3 substrate type. 
# Similarly RARD shows a occurrence at sites with lower numbers and declines as the sites increase
# Given RARD and Miniglmn occurrence sites where the community has similar community structures
# both of whih plays a large role in determining the community composition where the site is 
# located in an area where the substrate type is Sphagn 3 or 4.
# Trhypch1 and LCIL share a similar relationship, however LCIL occurrence at sites 
# less than Trhypch1. Trhypch1 and LCIL have a negative correlation with the occurrence of 
# RARD and Miniglmn. The negative correlation is likely linked to the environmental facts
# negatively correlating at the sites where the occur. 


# Questions 2 -------------------------------------------------------------
# Using two unconstrained ordination techniques of your choice (not already 
# used in 1, above) analyse the dune data in the vegan package. Provide a brief 
# description and discussion of what you have found, and produce the R code.

# The dune meadow vegetation data, dune, has cover class values of 30 species on 20 sites. 
# dune doesnt have site data
# data(dune)
# data(dune.env)
# therefore use CA and nMDS

# CA dune data ------------------------------------------------------------
# The abundance of 30 different species at 20 locations where collected. For
# each location the environmental vectors were collected.  
 
# Load the data -----------------------------------------------------------
 #data(package = "vegan") to observe available data in packages.
 
data("dune")
 
 # Examine the data --------------------------------------------------------
 dim(dune)
 # 20 rows, 30 columns
 summary(dune)
 head(dune, 5)
 
 # Perform Ordination ------------------------------------------------------
 # Perform Corresponding analysis due to it being species data.
 # CA will allow for a better analysis as it preserves the lineariy.
 dune.ca <- cca(dune)

 
 # Examine dune.ca
 summary(dune.ca)


 # Inertia = 2.115
 round(sum(dune.ca$CA$eig), 3)
 
 # The fraction of variance : 44.26
 round(sum(dune.ca$CA$eig[1:2])/sum(dune.ca$CA$eig)*100,2)
 
 # Ordination Diagram ------------------------------------------------------
 
 plot(dune.ca, scaling = 1, main = "CA dune abundance - biplot scaling 1")
 plot(dune.ca, scaling = 2, main = "CA dune abundance - biplot scaling 2") 
 
 # Callcusp  1.95199
 # Comapalu  1.91690
 # Trifprat -0.88116
 # Airaprae -1.00434
 
 require('viridis')
 palette(viridis(8))
 par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
 with(dune, tmp <- ordisurf(dune.ca ~ Callcusp, bubble = 3,
                            family = quasipoisson, knots = 2, col = 6,
                            display = "sites", main = "Callcusp"))
 abline(h = 0, v = 0, lty = 3)
 with(dune, tmp <- ordisurf(dune.ca ~ Comapalu, bubble = 3,
                            family = quasipoisson, knots = 2, col = 6,
                            display = "sites", main = "Comapalu"))
 abline(h = 0, v = 0, lty = 3)
 with(dune, tmp <- ordisurf(dune.ca ~ Trifprat, bubble = 3,
                            family = quasipoisson, knots = 2, col = 6,
                            display = "sites", main = "Trifprat"))
 abline(h = 0, v = 0, lty = 3)
 with(dune, tmp <- ordisurf(dune.ca ~ Airaprae, bubble = 3,
                            family = quasipoisson, knots = 2, col = 6,
                            display = "sites", main = "Airaprae"))
 abline(h = 0, v = 0, lty = 3)
 
 # Add environmental variables ---------------------------------------------
 data(dune.env)
 (dune.ca.env <- envfit(dune.ca, dune.env, scaling = 2))
 plot(dune.ca.env, col = "grey40")
 plot(dune.ca.env, p.max = 0.05, col = "red") 
 
# Mite analysis: CA -----------------------------------------------------------
 
# Sites one to 13, and site 18 are clustered close together in scaling one indication a similar 
# frequency of species. All other sites are scattered and not very similar in frequency composition.
# The most similar sites the following species found within their vegetation at very similar abundances:
# Bromhord, Bellpere, Poaprat, Rumeacet, Trifprat, Vicilath, Planlanc, Scorautu, Achimill, Bracruta, 
# Sagiproc, Trifrepe, Poatriv, Alopgeni, Juncbufo and Agrostol. Sites 20, 15 and 18 
# have a similar species frequency within their communities, containing  Callcusp, Comapalu, Eleopalu
# and Ranuflam, which are important in definig the vegetation within these communities.
# Sites 17 and 19 species frequency within their communities are very similar,the vegetation influencing the structure
# within these communities are Empenigr, Airaprae and Hyporadi.
 
# Callcusp, Comapalu, Trifprat and Airaprae are species which influence the communities within.
# Callcusp is the community defining species at sites 20, 16 and 14,  all other sites
# have very low abundances of this species. These sites have a high soil
# thickness and a high moisture level. Similarly sites 15 and 14 share Comapalu
# species frequency. Trifprat occurs in higher frequencies at sites 5, 6 and 7
# and low frequencies at all other sites. At sites associated with these plant species
# low levels of manure was present, locations where biological or hobby farming, 
#  reduced amount of moisture is present at these sites. Site 17 and 19 have high frequencies
# of Airaprae where the land is commonly used as a hayfield, these sites 
# negatively correlate with medium manure levels, high levels of moisture.

# These four species which influence the community structure at these locations 
# each species is high abundances a significant distance from other sites. Because 
# the plots show a negative correlation between the frequency of Comapalu and Trifprat.
# Airaprae and Callcusp are negatively correlated with each other at the locations.
# Because these sites do not have a positive correlation, environmental vectors
# are the determinant for the variation of species composition along the anvironmental
# gradients. 
 

# nMDS on dune ------------------------------------------------------------
# dune data was previously loaded 
 # Do the nMDS -------------------------------------------------------------
 
 dune.nmds <- metaMDS(dune, distance ="bray")
 
 dune.nmds
 # stress = 0.118
 dune.nmds$stress 
 # stress , 0.2, therefore data is trustworthy
 
 # Ordination Diagrams -----------------------------------------------------
 
 # console: par(mfrow = c(2, 2))
 
 # Shepard plot ------------------------------------------------------------
 
 stressplot(dune.nmds, main = "Shepard plot")
 ordiplot(dune.nmds, type = "t", cex = 1.5, main = paste0("nMDS stress = ", round(dune.nmds$stress, 2)))
 
 
 # Goodness of fit ---------------------------------------------------------
 
 gof = goodness(dune.nmds)
 plot(dune.nmds, type = "t", main = "Goodness of fit")
 points(dune.nmds, display = "sites", cex = gof * 200) # bigger bubbles indicate a worse fit
 
 
 # nMDS fish abundances ----------------------------------------------------
 
 pl.dune <- ordiplot(dune.nmds, type = "none", main = "nMDS dune meadow vegetation abundances ")
 points(pl.dune, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
 points(pl.dune, "species", pch = 21, col = "turquoise")#, arrows = TRUE)
 #text(pl.dune, "species", col = "blue4", cex = 0.9)
 text(pl.dune, "sites", col = "red4", cex = 0.9)
 
 # Callcusp  1.95199
 # Comapalu  1.91690
 # Trifprat -0.88116
 # Airaprae -1.00434
 
 require('viridis')
 palette(viridis(8))
 par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
 with(dune, tmp <- ordisurf(dune.nmds ~ Callcusp, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Callcusp"))
 abline(h = 0, v = 0, lty = 3)
 with(dune, tmp <- ordisurf(dune.nmds ~ Comapalu, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Comapalu"))
 abline(h = 0, v = 0, lty = 3)
 with(dune, tmp <- ordisurf(dune.nmds ~ Trifprat, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Trifprat"))
 abline(h = 0, v = 0, lty = 3)
 with(dune, tmp <- ordisurf(dune.nmds ~ Airaprae, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Airaprae"))
 abline(h = 0, v = 0, lty = 3)
 
 
 # Add environmental data --------------------------------------------------
data("dune.env")
 dune.nmds.env <- envfit(dune.nmds, dune.env) 
 plot(dune.nmds.env, col = "grey40")
 plot(dune.nmds.env, p.max = 0.05, col = "red")
 

# nMDS dune analysis ------------------------------------------------------
 
# The data have a stress value less than 0.2, indicating the data is trustworthy.
# This is observed on the shepards plot where little few outliers are present. As the 
# distance increases away from the first site so does the dissimilarity between 
# the species frequency within each site. The horse shoe effect is still present 
# within this data

# Sites 14, 15 and 20 are grouped together indicating they have a close similarity in 
# community structure where Airaprae, Empenigr and Hyporadi are the determinants.
# At sites 5, 6, 7 and 10 their community structure is determined by Rumeacet, Trifprat
# and Achimill. 
 
# Callcusp has a high occurrence at sites 14, 16 and 20; its occurrence at all other 
# sites are significantly smaller. The environmental factors closely associated at 
# these sites is thick soil and lots of moisture.
# Comapalu has low occurrences at all sites except sites 14 and 15; Comapalu e
# experiences the environmental vectors as Callcusp.  
 
# Trifprat has high occurrences at sites 5, 6 and 7; Trifprat has lower occurrences
# at the remaining sites. These sites are located in areas where moisture is low
# in the environment. Sites 19 and 17 have high species frequency of Airaprae
 
# Trifprat is negatively correlated with Callcusp and Comapalu. the moisture gradient
# is the reason for this correlation as Trifprat occurrs in low miosture areas
# but Callcusp and Comapalu occur in moisture high environments.