# Title: Quantitative Ecology
# Purpose: Biodiversity Measures
# Name: Robyn Manuel
# Date: 29 June 2021

library(vegan)
library(ggplot2)
library(betapart)


# Load data ---------------------------------------------------------------

spp <- read.csv("data/SeaweedsSpp.csv")
# presence and absense data


# view the data -----------------------------------------------------------

dim(spp) 
# 58 observations (rows), 848 variables (columns)

spp[1:15, 1:15] # view first 15 rows [rows, columns]

spp <- dplyr::select(spp,-1) # remove first line contains row number, which we already have and is not needed.


# Alpha diversity ---------------------------------------------------------

library(BiodiversityR)
spp_richness <- diversityresult(spp, index = 'richness', method = 'each site') 
# (data_frame, index = 'what we want to calculate', method = each site create independent count of columns with 1 in it)

specnumber(spp, MARGIN = 1)

# Above same data. First presented as data frame and second presented as variables.

ggplot(data = spp_richness, (aes(x = 1:58, y = richness))) +
  geom_line(col = "blueviolet")+
  xlab("Coastal section (west to east)") + ylab("Species richness") +
  labs(title = "The species richness occuring along the coast (from west to east))") +
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

ggplot(data = spp_richness, (aes(x = 1:58, y = richness))) +
  geom_area(col = "lightgoldenrod4", fill = "lightgoldenrod4") +
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


# Univariated diversity indices  ------------------------------------------
# Require additional information of how much of species' are present.
light <- read.csv("data/light_levels.csv")
# view data:
light

# Jaccard Dissimilarity:
light_div <- data.frame(
  site = c("low_light", "mid_light", "high_light"),
  richness = specnumber(light[, 2:7], MARGIN = 1),
  shannon = round(diversity(light[, 2:7], MARGIN = 1, index = "shannon"), 2),
  simpson = round(diversity(light[, 2:7], MARGIN = 1, index = "simpson"), 2))

light_div
# shannon: more emphasis on abundance
# simpson: more emphasis on evenness, the more even the higher value for simpson will be.

# mid_light: highest eveness and highest variation between
# abundance and number of species doesnt vary very much.
# high_light: some species are more abundant than others  

# Dissimilarity -----------------------------------------------------------
# square: no. rows = no. columns
# symmetrical
# diagonal of 0



 # Sorenson similarity :
 
sor <- vegdist(spp, binary = TRUE, diag = TRUE)
# diag = false (removes all the 0 ie. does not show site similarity to itself)
class(sor)
str(sor)
# to convert it to a data frame you can use/work with:
sor_df <- round(as.matrix(sor), 4)
class(sor_df)
dim(sor_df) # view the dimensions

sor_df[1:20, 1:20]


# Gamma diversity ---------------------------------------------------------
# number of columns = number of species in South Africa =  gamma diversity
ncol(spp)

diversityresult(spp, index = "richness", method = "pooled") # Pooled data accross entire data set




# Beta- diversity ---------------------------------------------------------
#Whittaker's concept of β‑diversity
# True beta diverty 
# divide sp. divesity by number of sp.
true_beta <- data.frame(  
  beta <- specnumber(spp, MARGIN = 1) / ncol(spp),
   section_no = c(1:58))

ggplot(data = true_beta,(aes(x = section_no, y = beta))) +
  geom_line(size = 0.7, col = "indianred4")+
  xlab("Coastal section, west to east") + ylab("True beta diversity") +
  labs(title = "The true beta diversity occuring along the coast (from west to east))") +
  xlab("Coastal section (west to east)") + ylab("True beta diversity") +
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

# Absolute beta diversity:

# total diversity (gamma) - every site the number of species present

abs_beta <- data.frame(
  beta = ncol(spp) - specnumber(spp, MARGIN = 1),
  section_no = c(1:58))

ggplot(data = abs_beta, (aes(x = section_no, y = beta))) +
  geom_line(size = 0.8, col = "darkslateblue") + 
  xlab("Coastal section, west to east") + ylab("Absolute beta diversity") +
  labs(title = "Absolute beta diversity occuring along the coast (from west to east))") +
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


# Contemporary definitions β‑diversity ------------------------------------
# turnover and nestedness

Y.core <- betapart.core(spp) # set up basic quantities neccessary

# Break it up into:
# 1. turnover
# 2. nestedness-resultant component
# 3. total dissimilarity ( sum of 1. and 2. )
Y.pair <- beta.pair(Y.core, index.family = "sor") 
str(Y.pair)

# Turnover:
Y1 <- as.matrix(Y.pair$beta.sim)

# Nestedness
Y2 <- as.matrix(Y.pair$beta.sne)

round(Y1[1:10, 1:20], 4)


# Alpha diversity can calculated diversity present in sites by using a presence 
# absense matrix or Univariated diversity indices.
# the univariated diversity indices makes us of the Jaccard dissimilarity or 
# sorsenson similarity indices.
# Jaccard looks at the richness, shannon wiener index and the simpsons 
# index to determine details with regards to the aplha diversity. Shannon index is focused
# on the abundance information where as the Simpson index focuses on the evenness of the data. 
# When the simpsons index is large there is a great amount of evenness in the data and the sites 
# are likes very similar to each other.
# Gamma diversity looks at the number of species present in a selected site.
# True bet diversity and absolute beta diversity are not frequently used. Absolute beta diversity 
# is determined by subtracting number of species present for each site from the gamma diversity 
# True beta diversity calculates the beta diversity by dividing the species diversity by the
# number of species present.Contemporary beta divrsity is more commonly used, which calculates 
# and makes use of the turnover and nestedness values of the data. The turnover and nestedness is then 
# used to calculate the total dissimilarity of the data.







