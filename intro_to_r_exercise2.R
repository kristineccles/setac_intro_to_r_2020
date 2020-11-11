#############################################################
# Exercise 2 - Introduction to R 
#############################################################
# Load Libraries
library(datasets)
library(OneR)
library(ggplot2)
library(psych) 
library(car) 
library(corrplot)
library(stats)
library(lmtest)

# Load data
# the dataset we will use is a a dataset built into R and be called from the datasets package

df=airquality
df

# if you want to write this to a csv file- where does the file save to?
write.csv(airquality, "air_quality.csv")

# Do you need to modify the data in any way?
#for the sake of doing a t-test we need to dive up temp to high and low 
df$temp_bin=bin(df$Temp, nbins = 2, labels = c('low','high'), method = c("content"))

# Any other modifications?

#############################################################
# Summarize the dataframe

# Check for missing data and address it if necessary

#############################################################
# Visualize the data 
# Make a histogram of the variable ozone


# Data distribution ozone by subset month


# Boxplot of ozone by month, with whiskers
# Hint: What type of variable is month? Boxplots require 1 categorical variables and 
# 1 continuous variable

#### T-Test #####
# test to see if there is a difference between ozone in the first and second half of the month 
# (using the binned variable from above)

# plot the data

# Test assumptions
# test for normality of raw data


# If you fail try a transformation


# test homogenity of variance


# Run the T-test


# Can you interpret the results? Is the test significant?


#### ANOVA ####
# test to see if there is a difference in the mean ozone concentration measured by month
# plot the data 

# What are the little black dots on this plot?

# Test assumptions
# test for normality of raw data

# test homogenity of variance


# run an anova

# Is the test significant? Should we do a post-hoc follow up?

# p-value is low so we reject the H0, there is a difference between the groups
# need to follow this up with a Tukey's post-hoc test

# plot the differences

# What is your interpreation of these results?


#### Correlation #####
# test if there is there a linear assoication between temperature and ozone 
# Scatterplot of ozone as a function of temperature


# Now colour the points by month

# testing single correlation


#What is your interpretation of these results?
# positive association between Temp and Ozone

# Test other combinations
# Matrix of scatterplots of all pairs of variables in air quality dataset

# make a correlation matrix

# visualize the correlations using corrplot


#### Linear Regression ####
# Make a plot

# Linear model


# test assumptions on RESIDUALS


# Are there any other variables that can be used to make a multivariate model?


# test assumptions on RESIDUALS

# test for multicolinearity

# What model is better?