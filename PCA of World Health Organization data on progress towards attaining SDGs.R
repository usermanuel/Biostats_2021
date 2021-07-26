# Title: Quantitative Ecology
# Author: Robyn Manuel
# Purpose: PCA of World Health Organization data on progress towards attaining SDGs
# Date: 26 July 2021


# Load packages -----------------------------------------------------------


library(tidyverse)
library(vegan)
# install.packages("missMDA")
library(missMDA) # to impute missing values
library(ggcorrplot) # for the correlations


# Load data ---------------------------------------------------------------

# SDG 1.a Domestic general government health expenditure (GGHE-D) as percentage 
# of general government expenditure (GGE) (%)-as-percentage-of-general-government-expenditure-(gge)

# Question 1:
# "read.csv"s loads the csv file on to the r script as a data frame, from the URL site.
# "filter" the data frame for 2016, removing all other years data. "select" selects 
# the columns which needs to be retained. "mutate" changes the data frame name and in some sections.
# to change the column name. All column names are required to be the same in order for "rbind" to work.
# These functions are used to filter out information which needs to be focused on. 
# It also ensure that all the information have the same information/ titles
# in order to make a data frame which contains all the information which is easy to read.



SDG1.a <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG1.a_domestic_health_expenditure.csv") %>%
  filter(Period == 2016) %>% 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG1.a") 


# SDG 3.1 Maternal mortality ratio (per 100 000 live births))

SDG3.1_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.1_maternal_mort.csv") %>%
  filter(Period == 2016,
         Indicator == "Maternal mortality ratio (per 100 000 live births)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.1_1")

# SDG 3.1 Births attended by skilled health personnel (%))

  SDG3.1_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.1_skilled_births.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.1_2")

# SDG 3.2 Number of neonatal deaths (Child mortality)
  
    SDG3.2_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.2_neonatal_deaths.csv") %>%
    filter(Period == 2016,
           Dim1 == "Both sexes") %>%
    select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
    mutate(SDG = "SDG3.2_1")
    
# SDG 3.2 Number of under-five deaths (Child mortality)
    
   SDG3.2_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.2_under_5_deaths.csv") %>%
      filter(Period == 2016,
             Dim1 == "Both sexes") %>%
      select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
      mutate(SDG = "SDG3.2_2")
   
# SDG 3.2 Number of infant deaths (Child mortality)
   
   SDG3.2_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.2_infant_deaths.csv") %>%
     filter(Period == 2016,
            Dim1 == "Both sexes") %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG3.2_3")

# SDG 3.3 New HIV infections (per 1000 uninfected population))
   
   SDG3.3_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.3_new_HIV_infections.csv") %>%
     filter(Period == 2015,
            Dim1 == "Both sexes") %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG3.3_1")
      
# SDG 3.3 Incidence of tuberculosis (per 100 000 population per year))
   
   SDG3.3_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.3_TB.csv") %>%
     filter(Period == 2016) %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG3.3_2")
   
# SDG 3.3 Malaria incidence (per 1 000 population at risk))   
   
   SDG3.3_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.3_malaria.csv") %>%
     filter(Period == 2016) %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG3.3_3")
   
# SDG 3.3 Hepatitis B surface antigen (HBsAg) prevalence among children under 5
# years-prevalence-among-children-under-5-years)   
   
   SDG3.3_4 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.3_hepatitis_B.csv") %>%
     filter(Period == 2015) %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG3.3_4")
   
   
# SDG 3.3 Reported number of people requiring interventions against NTDs
   
   SDG3.3_5 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.3_NCD_interventions.csv") %>%
     filter(Period == 2016) %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG3.3_5")
   
# SDG 3.4 Adult mortality rate (probability of dying between 15 and 60 years per 1000 population))
   
   SDG3.4_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_adult_death_prob.csv") %>%
     filter(Period == 2016,
            Dim1 == "Both sexes") %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG3.4_1")

# SDG 3.4 Number of deaths attributed to non-communicable diseases, by type of disease and sex
   
   SDG3.4_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_NCD_by_cause.csv") %>%
     filter(Period == 2016,
            Dim1 == "Both sexes",
            Dim2 == "Diabetes mellitus") %>%
     mutate(Indicator = Dim2) %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG3.4_2")
   
   SDG3.4_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_NCD_by_cause.csv") %>%
     filter(Period == 2016,
            Dim1 == "Both sexes",
            Dim2 == "Cardiovascular diseases") %>%
     mutate(Indicator = Dim2) %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG3.4_3")
   
   SDG3.4_4 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_NCD_by_cause.csv") %>%
     filter(Period == 2016,
            Dim1 == "Both sexes",
            Dim2 == "Respiratory diseases") %>%
     mutate(Indicator = Dim2) %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG3.4_4")
   
# SDG 3.4 Crude suicide rates (per 100 000 population) (SDG 3.4.2))
   
   SDG3.4_5 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_suicides.csv") %>%
     filter(Period == 2016,
            Dim1 == "Both sexes") %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG3.4_5")
   
   
# SDG3.4 Total NCD Deaths (in thousands)
   
   SDG3.4_6 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_NCD_data_total.csv") %>%
     filter(Period == 2016,
            Dim1 == "Both sexes") %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG3.4_6")

   
# SDG 3.5 Alcohol, total per capita (15+) consumption (in litres of pure 
# alcohol) (SDG Indicator 3.5.2)-alcohol-per-capita-(15-)-consumption)
   
   SDG3.5 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.5_alcohol_consumption.csv") %>%
     filter(Period == 2015,
            Dim1 == "Both sexes") %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG3.5")
   
   
# SDG 3.6 Estimated road traffic death rate (per 100 000 population))
 
   SDG3.6 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.6_traffic_deaths_prop.csv") %>%
     filter(Period == 2016,
            Dim1 == "Both sexes") %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG3.6")
   
# SDG 3.7 Adolescent birth rate (per 1000 women aged 15-19 years))
   
   SDG3.7 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.7_adolescent_births.csv") %>%
     filter(Period == 2016) %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG3.7")
   
# SDG 3.8 UHC Index of service coverage (SCI)
   
   SDG3.8_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.8_UHC_data_availability.csv") %>%
     filter(Period == "2013-2017") %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG3.8_1")
   
# SDG 3.9 Poison control and unintentional poisoning
   
   SDG3.9_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.9_unintentional_poisoning_prop.csv") %>%
     filter(Period == 2016,
            Dim1 == "Both sexes") %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG3.9_1")
   
# SDG 3.9 Mortality rate attributed to exposure to unsafe WASH services (per 
# 100 000 population) (SDG 3.9.2)-(sdg-3-9-2))
   
   SDG3.9_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.9_WASH_mortalities.csv") %>%
     filter(Period == 2016,
            Dim1 == "Both sexes") %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG3.9_3")
   
# SDG 16.1 Estimates of rate of homicides (per 100 000 population)
   
   SDG16.1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG16.1_homicides.csv") %>%
     filter(Period == 2016,
            Dim1 == "Both sexes") %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG16.1")

# SDG 3.a Prevalence of current tobacco use among persons aged 15 years a
# older (age-standardized rate)
   
   SDG3.a <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.a_tobacco_control.csv") %>%
     filter(Period == 2016,
            Dim1 == "Both sexes") %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG3.a")
   
# SDG 3.b Total net official development assistance to medical research and 
# basic health sectors per capita (US$), by recipient country
   
   SDG3.b_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.b_dev_assistence_for_med_research.csv") %>%
     filter(Period == 2016) %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG3.b_1")

# SDG 3.b Measles-containing-vaccine second-dose (MCV2) immunization coverage 
# by the nationally recommended age (%)-immunization-coverage-by-the-nationally-recommended-age-(-))
   
   SDG3.b_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.b_measles_vaccine.csv") %>%
     filter(Period == 2016) %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG3.b_2")
   
# SDG 3.b Diphtheria tetanus toxoid and pertussis (DTP3) immunization coverage 
# among 1-year-olds (%)-immunization-coverage-among-1-year-olds-(-))   
   
   SDG3.b_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.b_diphtheria_vaccine.csv") %>%
     filter(Period == 2016) %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG3.b_3")

# SDG 3.b Pneumococcal conjugate vaccines (PCV3) immunization coverage among 
# 1-year-olds (%)-immunization-coverage-among-1-year-olds-(-))
   
   SDG3.b_4 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.b_pneumococcal_vaccine.csv") %>%
     filter(Period == 2016) %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG3.b_4")
   
# SDG 3.b Girls aged 15 years old that received the recommended doses of HPV vaccine
   
   SDG3.b_5 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.b_HPV_vaccine.csv") %>%
     filter(Period == 2016) %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG3.b_5")
   
# SDG 3.c SDG Target 3.c | Health workforce: Substantially increase health 
# financing and the recruitment, development, training and retention of the health 
# workforce in developing countries, especially in least developed countries and small island developing States
   
   SDG3.c_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.c_health_workforce.csv")  %>%
     filter(Period == 2016,
            Indicator == "Medical doctors (per 10,000)") %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG3.c_1")
   
   SDG3.c_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.c_health_workforce.csv")  %>%
     filter(Period == 2016,
            Indicator == "Nursing and midwifery personnel (per 10,000)") %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG3.c_2")
   
   SDG3.c_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.c_health_workforce.csv")  %>%
     filter(Period == 2016,
            Indicator == "Dentists (per 10,000)") %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG3.c_3")
   
   SDG3.c_4 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.c_health_workforce.csv")  %>%
     filter(Period == 2016,
            Indicator == "Pharmacists  (per 10,000)") %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG3.c_4")
   
# SDG 3.d Average of 13 International Health Regulations core capacity scores, SPAR version
   
   SDG3.d_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.d_health_risks.csv")  %>%
     filter(Period == 2016) %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "SDG3.d_1")
   
# Other Life expectancy at birth (years))   
   
   other_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_Other_life_expectancy.csv") %>%
     filter(Period == 2015,
            Dim1 == "Both sexes",
            Indicator == "Life expectancy at birth (years)") %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "other_1")
   
# Other Life expectancy at age 60 (years))
   
   other_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_Other_life_expectancy.csv") %>%
     filter(Period == 2015,
            Dim1 == "Both sexes",
            Indicator == "Life expectancy at age 60 (years)") %>%
     select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
     mutate(SDG = "other_2")
   

# rbind the data ----------------------------------------------------------
# Question 1:
# "do.call" and "rbind" are functions which will create a data frame containing
# all the loaded data frames in the previous step(s). "do.call creates the new 
# data frame according to the requirements listed in the brackets. "rbind"
# ensures all the information is added within the same column grouping. 
   
   health <- do.call("rbind", lapply(ls(),get))
   head(health)   
   

# Create list of SDGs used ------------------------------------------------
# Looks at all the available variables, focusing on row 5, column 1
   unique(health[, c(5, 1)]) 
   # write_csv(unique(health[, c(5, 1)]), file = "data/SDG_description.csv")   
   

# Pivot wider -------------------------------------------------------------
# Question 1:
# " <- " creates a new data frame from a previous data frame (health), with the 
# specific arguments which are stated after. "arrange" sorts the data rows according to 
# the country, in alphabetical order. "select" with the "-" removes the Indicator
# column. "pivot_wider" ensures that for every country the Indicator value is placed 
# under the column which the value occurs within the correct column entitled the
# name of the data frame."as_tibble" forces listed arrangements to create a new data 
# frame.
   health_wide <- health %>%
     arrange(Location) %>%
     select(-Indicator) %>%
     pivot_wider(names_from = SDG, values_from = FactValueNumeric) %>%
     as_tibble()
   


# Add world population data -----------------------------------------------
# Question 1:
# read_csv create a data frame from the information of the URL. This data 
# is filtered looking specifically at the year 2016, and "rename" changes the
# column name to match the other data frames within the r script. "select"
# then isolated the important columns within the data frame.The functions within 
# "mutate" ensures that the numerical variables are recognized as such. The numbers
# are then multiplied by 1000.
# "left_join" adds the newly created popl data frame to the health_wide data frame.
   
   
   popl <- read_csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_population.csv") %>%
     filter(Year == 2016) %>%
     rename(popl_size = `Population (in thousands) total`,
            Location = Country) %>%
     select(Location, popl_size) %>%
     mutate(popl_size = as.numeric(gsub("[[:space:]]", "", popl_size)) * 1000)
   
   health_wide <- health_wide %>%
     left_join(popl)
   

# Express some variables to unit of population size -----------------------

   health_wide <- health_wide %>%
     mutate(SDG3.4_4 = SDG3.4_4 / popl_size * 100000,
            SDG3.4_3 = SDG3.4_3 / popl_size * 100000,
            SDG3.4_2 = SDG3.4_2 / popl_size * 100000,
            SDG3.4_6 = SDG3.4_6 / 100,
            SDG3.2_2 = SDG3.2_2 / popl_size * 100000,
            SDG3.2_3 = SDG3.2_3 / popl_size * 100000,
            SDG3.2_1 = SDG3.2_1 / popl_size * 100000)

# Histograms of missing values, and correlations --------------------------

   # calculate histograms
   health_wide$na_count <- apply(health_wide[, 3:(ncol(health_wide) - 1)], 1, function(x) sum(is.na(x)))
   hist(health_wide$na_count, breaks = 14, plot = TRUE)
   
   # remove rows where there are more than 10 NAs
   health_wide <- health_wide %>%
     filter(na_count <= 10) %>%
     select(-na_count)
   
   # calculate pairwise correlations
   corr <- round(cor(health_wide[, 3:(ncol(health_wide) - 1)]), 1)
   
   # visualization of the correlation matrix
   ggcorrplot(corr, type = 'upper', outline.col = "grey60",
              colors = c("#1679a1", "white", "#f8766d"),
              lab = TRUE)
   

# Impute remaining NAs ----------------------------------------------------
# Question 1:
# these functions prepare the health_wide data frame to create a PCA. 
# it removed the Location and ParentLocation column however, each place 
# has an assigned number.
   health_wide_complete <- imputePCA(health_wide[, 3:(ncol(health_wide) - 1)])$completeObs   
   

# Scale and center the data and do the PCA --------------------------------
# Question 1:
# before a PCA can be created the data needs to be standardized, "decostand" 
# is the function used to standardize ecological data. This ensure than one one 
# variable have more sway over the results than another. "rda" is the funcation
# used to perform the PCA.
   health_wide_complete_std <- decostand(health_wide_complete, method = "standardize")
   health_pca <- rda(health_wide_complete_std)
   health_pca
   # summary(health_pca)   
   

# Graphical displays -----------------------------------------------------

   biplot(health_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2), expand = 20, xlim = c(-2, 1) , ylim = c(-1, 2) )
   biplot(health_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2), expand = 20, xlim = c(-2, 1) , ylim = c(-1, 2))
   
   pl1 <- ordiplot(health_pca, type = "none", scaling = 1, main = "PCA WHO/SDG", air = 100)
   points(pl1, "sites", pch = 21, cex = 1.0, col = "grey20", bg = "grey80")
   points(pl1, "species", pch = 21, col = "turquoise", arrows = TRUE)
   text(pl1, "species", col = "blue4", cex = 0.9)
   text(pl1, "sites", col = "red4", cex = 0.9)
   
   pl2 <- ordiplot(health_pca, type = "none", scaling = 2, main = "PCA WHO/SDG", air = 100 )
   points(pl2, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
   points(pl2, "species", pch = 21, col = "turquoise", arrows = TRUE)
   text(pl2, "species", col = "blue4", cex = 0.9)
   text(pl2, "sites", col = "red4", cex = 0.9)
   
   
   site_scores <- tibble(ParentLocation = health_wide$ParentLocation,
                         Location = health_wide$Location)
   site_scores <- tibble(cbind(site_scores, scores(health_pca, display = "sites", choices = c(1:7))))
   species_scores <- data.frame(scores(health_pca, display = "species", choices = c(1:7)))
   species_scores$species <- rownames(species_scores)
   species_scores <- tibble(species_scores)
   
   ggplot(data = site_scores, aes(x = PC1, y = PC2)) +
     geom_point(aes(col = ParentLocation)) +
     geom_segment(data = species_scores, 
                  aes(x = 0, y = 0, xend = PC1, yend = PC2),
                  arrow = arrow(length = unit(0.4, "cm"), type = "closed"), 
                  color = "lightseagreen", alpha = 1, size = 0.3) +
     geom_text(data = species_scores, 
               aes(x = PC1, y = PC2, label = species),
               color = "black") +
     xlab("PC1") + ylab("PC2") + 
     ggtitle("WHO SDGs, Scaling 2")
   

# Questions ---------------------------------------------------------------

# 1. Explain the code section-by-section in long-form text. Include also the reasoning/rationale behind each section.

   
# 2. Discuss and explain the patterns observed. How does South Africa fare 
# interms of attaining SDGs? Contrast with some key countries of your choice to 
# make your points. Label the key countries that you refer to in your text by updating the code accordingly.

# The countries which are outliers are Lesotho, Eswatini, Kiribati and Central African Republic,
# Micronesia including South Africa. All other sites are clustered near the center.
# the outlier sites are very dissimilar in their SDG responses. Despite being an
# outlier, South African is also close in similarity to the the larger cluster of sites.
# 
   
   
    contrast.site <- site_scores %>% 
      filter(ParentLocation %in% c( "Africa", "Europe"))
   
   
    ggplot(data = contrast.site, aes(x = PC1, y = PC2)) +
      geom_point(aes(col = ParentLocation)) +
      geom_segment(data = species_scores, 
                   aes(x = 0, y = 0, xend = PC1, yend = PC2),
                   arrow = arrow(length = unit(0.4, "cm"), type = "closed"), 
                   color = "lightseagreen", alpha = 1, size = 0.3) +
       geom_text(data = contrast.site, 
                 aes(x = PC1, y = PC2, label = Location),
                 color = "red")+
      geom_text(data = species_scores, 
                aes(x = PC1, y = PC2, label = species),
                color = "black") +
      xlab("PC1") + ylab("PC2") + 
      ggtitle("WHO SDGs (focused on Africa), Scaling 2")
# Lesotho Eswatini, South Africa, Botswana, Namibia, Mauritius, Mozambique, Central African Republic
# Saychelles, Cabo, Algeria, Gabon and Zambia are spaced significantly far from the 
# the larger cluster.Lesotho Eswatini, South Africa, Botswana and Namibia have a very strong
# correlation with SDG3.3_1 (HIV infections), as well as high tuberculosis infections.
# Mauritius has high suicide rates (SDG3.4_5) and deaths due to non-communicable diseases 
# (SDG3.4_2). Mozambique, Central African Republic and Zimbabwe have high adult (between ages 15 and 60)
# morality. Most of the European countries are found on the right side of the plot, where as African countries 
# are found on the left. The countries which are located on the right of the plot 
# correlate with births conducted by skilled professionals, vaccinations and UHC index service.
# The left hand side where most African countries are present correlates with high road traffic deaths,
# Hepatitis B antigens in children, adult morality (between ages 15- 60) and adolescent (15 - 19) births.

   
   
# 3. Provide a discourage about possible explanations for the patterns observed globally and regionally.
   
# Well developed countries have better education levels and therefore have qualified people to offer services.
# countries found on the left hand side are associated with under developed education services
# and poverty among many of their populations. This would account for a high adolescent birth rate and
# high adult morality, a lack of sexual education and poor medical services.

   
   
   
