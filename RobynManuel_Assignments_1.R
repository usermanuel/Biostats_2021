# Script Name: Assignment One
# Purpose: Biostatistic and Intro_R recap assignment: 
#           Data Manipulation, Anlyses and Visualisation
# Author: Robyn Manuel
# Date: 19/04/2021

library(tidyverse)
# Section_1 ---------------------------------------------------------------

# type nrow(BOD) into the console
# a. true
# b. true
# c. true
# d. false


# Section_2 ---------------------------------------------------------------

library(dplyr)
#install.packages("dslabs")
library(dslabs)
data(murders)

?murders
glimpse(murders)
head(murders)

nrow(murders)
# 51 observations
str(murders)
# 5 variables: state, abb, region, population, total
unique(murders$state)
unique(murders$region)
 
# The murder data looks at the gun data collected for each state population in America. There are
# 51 states, and 5 variables in the data. Those state variable is further divided into regions within america.
#


murders %>% 
  select("state", "population") %>% 
  filter(murders$state != "Florida") #remove a row

no_south <- murders %>% 
  filter(murders$region != "South")
unique(no_south$state)
nrow(no_south)
 # there are 34 states in no_south

# Calculate the population size of the South and West regionally
 #South:
south <- murders %>% 
  filter(region == "South")

sum(south$population)
 # 115674434

#West:
west <- murders %>% 
  filter(region == "West")
sum(west$population)
# 71945553

# Create a new data frame with only the population size of the Northeast region
NE_pop <- murders %>% 
  filter(region == "Northeast") %>% 
  select("population")
# Create two plots of your choice and explain visible trends

library("ggplot2")
 # boxplot
ggplot(murders, aes(x = region, y = total)) +
  geom_point(alpha = 0.3, col = "salmon", size = 3)+
  geom_boxplot(fill = "grey", col = "black", outlier.colour = "red") +
  ggtitle("Statistics for each region") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Region", y = "Total murders commited with a gun")
# The graph shows the total number of murders comited using a gun
# in the four regions of America. The west region shows the least
# amount of murders committed with the use of a gun. The south region
# shows the most statistically normal data. The north central and north east data
# is skewed towards a lower number of murders.
  
murder
# point graph
ggplot(murders, aes(x = state, y = total)) +
  geom_line(aes(group = region, col = region))  +
  geom_area(aes(group = region, col = region, fill = region, alpha = 0.6)) +
  ggtitle("Gun related murders occuring in each state")+
  theme(plot.title = element_text(hjust = 1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# The graph created displays the gun related murders in each state in America. It
# is colour co-ordinated to display states occurring in specific regions. For the 
# south region Tennessee and the district of columbia show the highest gun related murders. 
# The west region has a very low total gun gun related murders. The Northern central region
# peaks at Idaho, Massaschusetts, Mississippi and North Dakota. 

# Line graph:

# four seperate line graphs

ggplot(murders, aes(x = as.factor(state), y = total, group = region)) +
  geom_line(aes(colour = region)) +
  geom_smooth(aes(group = region), method = "lm", colour = "grey40") +
  facet_wrap(~region) +
  ggtitle("Gun related murders occuring in each state for each region")+
  theme(plot.title = element_text(hjust = 1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "State", y = "Population")
# the above code codes for the same variables as the code before it just structured differently.
# The graph created displays the gun related murders in each state in America. It
# is colour co-ordinated to display states occurring in specific regions. For the 
# south region Tennessee and the district of columbia show the highest gun related murders. 
# The west region has a very low total gun gun related murders. The Northern central region
# peaks at Idaho, Massaschusetts, Mississippi and North Dakota. 

#Create a new data frame where the total>20 but <100 and to exclude the value 120
murders_alter <- murders %>% 
  filter(total > 20, total < 100)

murders_no120 <- murders %>% 
  filter(murders$total != 120 )

#Create an object, containing from 10th to 24th row and 26th row. Hint: consider using the slice() function.
slice <- slice(murders, 10:24, 26, preserve = FALSE)

murders_tibble <- as_tibble(murders)

murders_tibbs <- as_tibble(murders) %>% 
  group_by(region)

# Section_3 ---------------------------------------------------------------

data(heights)
glimpse(heights)
summary(heights)
head(heights)
tail(heights)
nrow(heights)
#There are 1050 observations.
unique(heights$sex)
# two unique variables, Female and male.
str(heights)
#has two levels: male and female, associated with numerical data

heights %>% 
  filter(sex == "Female") %>% 
  nrow()
#There are 238 females

heights %>% 
  filter(sex == "Male") %>% 
  nrow()
# there are 812 males 


#   Height is a data set containing 1050 observations (total sample size),
#   two variables are found in it. The two variables are sexes male or female. 
#   The two levels represent height in numerical data. There are 238 observations 
#    for the female variable and 812 observations for the male variable.



# For Females:

heights %>% 
  filter(sex == "Female") %>% 
  summarise(F_mean = mean(height),
            F_sd = sd(height),
            F_med = median(height),
            F_min = min(height),
            F_max = max(height))


# For Males:

heights %>% 
  filter(sex == "Male") %>% 
  summarise(M_mean = mean(height),
            M_sd = sd(height),
            M_med = median(height),
            M_min = min(height),
            M_max = max(height))



# Section_4 ---------------------------------------------------------------

x <-  c( -1, 6, 21, 19 , NA, 73, NA)
y <-  c(NA, NA, 3, NA, 13, 24, NA)

table(is.na(x))
# therefore there are 2 missing elements for x

table(is.na(y))
# therefore there are 4missing elements for y
#Transform the code, used above (a), into a function

count_NA <- function(x){
  missing<- table(is.na())
  return(missing)
}
count_NA(y)

center <- function(data, midpoint) {
  new_data <- (data - mean(data)) + midpoint
  return(new_data)

mean(x, na.rm = TRUE)
nrow(x, na.rm = TRUE)



# Section_5 ---------------------------------------------------------------

Seasonal_data <- data.frame(year = c(2015, 2016, 2017, 2018),
                            winter = c(41, 39, 47, 40),
                            spring = c(41, 46, 57, 45),
                            summer = c(75, 52, 85, 66),
                            Autumn = c(57, 66, 52, 56))
season_Org <- Seasonal_data %>% 
  gather(winter, spring, summer, Autumn, key = "season", value = "visits" )
# A zoo gathers all information over 4 years of how many visiter visit the zoo, within specific month. 
# It can be hypothesised that the zoo will experience the fewest guests during winter due to the cold weather.
Seasonal_data %>% 
  

ggplot(season_Org, aes(x = season, y = visits, group = year)) +
  geom_line() +
  geom_smooth(aes(group = year), method = "lm", colour = "grey40") +
  facet_wrap(~year) +
  ggtitle("tracking zoo foot traffic") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "Seeason", y = "Visits")

# It was found that the fewest visits occur during the season of winter. Autumn
# and spring visits are few while summer visits are the highest. Therefore the 
# hypothesis is correct, during winter the zoo experiences the least visits 
# and summer is the preferred season to visit.

#ggplot(season_Org, aes(x = year, y = visits, group = season)) +
#  geom_line() +
 # geom_smooth(aes(group = season), method = "lm", colour = "grey40") +
#  facet_wrap(~season) +
#  ggtitle("tracking zoo foot traffic") +
#  theme(plot.title = element_text(hjust = 0.5)) +
#  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#  labs(x = "Seeason", y = "Visits")



cats_data<- tibble(cats = c("A", "B", "C"),
                   position = c("1-2-3", "3-1-2", "2-3-1")),
                   minutes = c(3, 3, 3),
                   seconds = c(12, 44, 15))
cats_data

cats_place <- cats_data %>% 
  separate(position, c("first_place", "second_place", "third_place"))

cats_place %>% 
  unite(total_time, minutes, seconds, sep = ":")

# Section 6 ---------------------------------------------------------------
 library(boot)
data(calcium)
glimpse(calcium)
#gather(): place multiple variables into a new column. the data in the packages are already sorted

temperature<- data.frame(year = c(2018, 2019, 2020, 2021),
                            winter = c(41, 39, 47, 40),
                            spring = c(41, 46, 57, 45),
                            summer = c(75, 52, 85, 66),
                            Autumn = c(57, 66, 52, 56))
temp <- temperature %>% 
  gather(winter, spring, summer, Autumn, key = "season", value = "degrees")

calcium %>% 
  gather
#spread(): the opposite of gather, convert multiple observation in a column for variables
temp %>% 
  spread(season, degrees)

temp_mess <- tibble(season = c("winter", "summer", "autumn", "spring"),
                   temp = c("12-10-15","31-25-36", "23-14-20", "20-30-16"),
                   rain_event = c(3, 0, 2, 1))


library("dbplyr")

library("tidyr")
  #joining(): 
#separate(): separate muplitple data points recorded as one observation needs to be divided into its own column

temp_mess %>% 
  separate(temp, c("day1", "day2", "day3"))

data("breslow")

head(breslow)

unique(breslow$age)
  
temp_mess %>% 
  select(season)
#joining(
arrange()
#select(): isolated specfic coloumn which you request in the code.
#group_by(): focuses the codes actions on a specific group of data which has a common identifier.
#mutate() create a new column, from data located in other columns










