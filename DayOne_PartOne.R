# Script name: Day_One
# Description:
# Author: Robyn Manuel
# Date 19 April 2021

# Chapter 2:\ -------------------------------------------------------------


# Stats is to make sense of large amounts of data.
# Find patterns in data, therefore need to summaries data in a statistic.
# Statistics: summaries of data...(missed some stuuf)

# Descriptive stats:classify data in small collection of data, used to describe the larger data.

# Inferential stats: Allow to make inferenced aboput data. 
#Allows us to ask question such as if xperiment was success full, if it is different in a similar siotuation when we exclude some thing from  a comtrol.
# did the findinfg happen by chance or is it a feature of the data-

# Numerical data first data work with.(Quantitative data)
# Nominal/ descrete data: count data, whole full numbers
# Countinous data: fractional numberical data (floating point numbers)
# dates are a class of data in R

#FRIENDS DONT LET FRIENDS MAKE PIE CHARTS


# Looking at the data that comes with R -----------------------------------

                #type in console:
#data()
#data(package = .packages(all.available = TRUE))
# ?BOD
# str(BOD) ......time and demand both numberical values. 
            #......(STR structure tell you about type/structure of data)
# summary(BOD)

#unique(InsectSpray$spray)
#?Loblolly
#str(Loblolly)
# what could do when get a data set for the first time

# Exercise: pick data sets for you to understand data ---------------------

#where would start to understand data

#> ?HairEyeColor 
#> str(HairEyeColor)
#summary(HairEyeColor)


# Load built in data as an R object ---------------------------------------

pines <-  Loblolly
str(pines) #th structure of data
head(pines)
# in console= 
#class of data in a comloum of a partictualr data set.
# class(pines$height)
