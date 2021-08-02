# Title: Quantitative Ecology
# Author: Robyn Manuel
# Purpose: Cluster Analysis
# Date : 02 August 2021


# Load packages -----------------------------------------------------------


library(tidyverse) 
library(cluster)
library(ggcorrplot)
library(factoextra)
library(vegan)
library(ggpubr)


# Load data ---------------------------------------------------------------

  SDGs <- read_csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/SDG_complete.csv")
  SDGs[1:5, 1:8] # view first 5 rows and 8 columns
  
  length(SDGs$Location)
  

# create correalation matrix ---------------------------------------------------

  
  corr <- round(cor(SDGs[3:ncol(SDGs)]), 1)
  ggcorrplot(corr, type = 'upper', outline.col = "white", 
             colors = c("turquoise1", "white", "violet"), 
             lab = TRUE)
  

# Create optimal cluster graphs -------------------------------------------

  
  SDGs_std <- decostand(SDGs[3:ncol(SDGs)], method = "standardize") # standardize the data
  # SDGs_euc <- vegdist(SDGs_std, method = "euclidian")
  rownames(SDGs_std) <- SDGs$Location # carry location names into output
  
  
  # using silhouette analysis
  plt1 <- fviz_nbclust(SDGs_std, cluster::pam, method = "silhouette") + theme_grey()
  
  # total within cluster sum of square / elbow analysis
  plt2 <- fviz_nbclust(SDGs_std, cluster::pam, method = "wss") + theme_grey()
  
  # gap statistics
  plt3 <- fviz_nbclust(SDGs_std, cluster::pam, method = "gap_stat") + theme_grey()
  
  ggarrange(plt1, plt2, plt3, nrow = 3)
  

# Create cluster graph ----------------------------------------------------

  
  SDGs_pam <- pam(SDGs_std, metric = "euclidean", k = 3)
  
  fviz_cluster(SDGs_pam, geom = "point", ellipse.type = "convex", 
               palette = c( "deeppink","yellow","royalblue1"), 
               ellipse.alpha = 0.05) +
    geom_text(aes(label = SDGs$Location), size = 2.5)
  
  
  
  # scale SA bigger for plotting
  SDGs <- SDGs |> 
    mutate(col_vec = ifelse(Location == "South Africa", "black", "grey50"),
           scale_vec = ifelse(Location == "South Africa", 3.5, 2.5))
  
  fviz_cluster(SDGs_pam, geom = "point", ellipse.type = "convex",
               palette = c("deeppink","blueviolet","royalblue1"),
               ellipse.alpha = 0.05, pointsize = 2.0) +
    geom_text(aes(label = SDGs$Location), size = SDGs$scale_vec, col = SDGs$col_vec)
  
  
  
  fviz_cluster(SDGs_pam, palette = c("deeppink","blueviolet", "royalblue1"), 
               ellipse.type = "euclid", 
               star.plot = TRUE, repel = TRUE, 
               pointsize = SDGs$scale_vec * 0.8) + # SA, no 147, plotted slightly bigger
    theme_grey()
  
  
  fviz_silhouette(SDGs_pam, palette = c( "deeppink","yellow","royalblue1"), ggtheme = theme_grey())
  
  
  
  
  SDGs_centroids <- SDGs |> 
    mutate(cluster = SDGs_pam$clustering) |> 
    group_by(cluster) |> 
    summarise_at(vars(other_1:SDG3.b_5), median, na.rm = TRUE)
  SDGs_centroids
  
  
  
SDGs_pam$medoids

pairs(SDGs[, 3:10], col = c("#FC4E07", "violetred3", "deepskyblue3")[SDGs_pam$clustering])




# Question --------------------------------------------------------------



# 1. What happens if we use pam() to create four, five, or even six clusters?
# With 4 clusters pam creates a new cluster within a previously existing one. With 5 clusters,
# one previous known cluster in broken into 3 additional cluster, making 5 when two 
# of the orgional ses of clusters are formed. completely new clustered are formed,
# clustering countries which were not initially placed together.

SDGs_pam <- pam(SDGs_std, metric = "euclidean", k = 3)

fviz_cluster(SDGs_pam, geom = "point", ellipse.type = "convex", 
             palette = c( "deeppink","yellow","royalblue1"), 
             ellipse.alpha = 0.05) +
  geom_text(aes(label = SDGs$Location), size = 2.5)


SDGs_pam <- pam(SDGs_std, metric = "euclidean", k = 6)

fviz_cluster(SDGs_pam, geom = "point", ellipse.type = "convex", 
             palette = c( "deeppink","yellow","royalblue1","seagreen3", "red4", "blue3"), 
             ellipse.alpha = 0.05) +
  geom_text(aes(label = SDGs$Location), size = 2.5)

# 2. In your reasoned opinion, what would be the optimal number of clusters to use?

# Three in the max number of clusters I would recommend as the more clusters there are, the increased chanced of 
# overlapping clusters.

# 3. Repeat the analysis using either kmeans() or hclust(), and feel free to use the 
# factoextra helper functions and visualisations. Are the results markedly different?
# Which clustering approach do you wish to proceed with---i.e., pam(), hclust() or kmeans()?

# kmeans ------------------------------------------------------------------


km <- kmeans(SDGs_std, centers = 3, iter.max = 10, nstart = 1,
                  algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
                                "MacQueen"), trace = FALSE)


fviz_cluster(km,SDGs_std, geom = "point", ellipse.type = "convex", 
             palette = c( "deeppink","yellow","royalblue1", "seagreen3"), 
             ellipse.alpha = 0.05) +
  geom_text(aes(label = SDGs$Location), size = 2.5)

# hclust ------------------------------------------------------------------


DD <- dist(scale(SDGs_std), method = "euclidean")
hc <- hclust(DD, method = "ward.D2")

plot(hc, labels = NULL, hang = 0.1, check = TRUE,
     axes = TRUE, frame.plot = FALSE, ann = TRUE,
     main = "Cluster Dendrogram")

# 4. Build upon the narrative that you have already developed in the previous assignment 
# and describe the patterns that you observe at the end of your final cluster selection
# (i.e. based on the optimal number of clusters and whichever cluster technique you deem 
# most appropriate), and explain these patterns in terms of the global socio-political/socio-economic landscape.

# The countries in the yellow circle experience extreme poverty, poorly developed services, 
# mostly rely on primary economic sector. There are large income gap between the classes. 
# These countries deal with dated stereotypes within their communities i.e sexism, homophobia, etc.

# The countries in the blue cluster are poor but better developed than those in the yellow. 

# Many of the countries falling in the red cluster are first world countries, well off high 
# standards of living, well developed education and services sector.

# 5. Regardless of how many clusters you choose, South Africa often seems to teeter 
# at the edge between the group of African countries and some other parent location. 
# Why?
  
# African countries are classes as third world countries, many African countries are impoverished
# and don't have a very well developed education and services systems to better developed the country.
# South Africa shows many of the same characteristics as of other African countries however, South Africa 
# is slightly more developed. Better schooling systems and growth opportunities. However, because 
# there is still substantial work to be done on many social, socio-political and socio-economic issues South Africa
# has not developed past the third world status.


