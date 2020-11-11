#############################################################
# Exercise 1 - Introduction to R 
# By: Kristin Eccles
# Written in R 3.5.0

#############################################################
# Load Libraries
# Requires the ggplot2 package that is not part of base installation
# To install the package, uncomment the following line
#install.packages("ggplot2", dependencies=TRUE)
library(ggplot2)
library(lmtest)
library(car)
library(graphics)
library(corrplot)
library(psych)

# Load data
sturgeon= read.csv("sturgeon.csv")
sturgeon

#if the project file was set up properly then the wd (working directory) 
# will be set to the folder you created
getwd()
#setwd("/Users/kristineccles/Desktop/intro_to_r_setac")

#############################################################

#### Explore and modify the data ####

# Check to see if we have missing data
# Summarize the contents of the sturgeon dataframe
summary(sturgeon)
# the NA's will tell you how missing data values you have in each variable

# more detailed descriptive/ summary stats
describe(sturgeon)

# Clean the data if necessary
# if we had missing data we can remove 
# create new dataset without missing data 
sturgeon_complete = na.omit(sturgeon)
summary(sturgeon_complete)
#no missing data now

hist(sturgeon_complete$fklngth)

# Create a subset

sturgeon.female = subset(sturgeon, sex == "FEMALE")
hist(sturgeon.female$fklngth)

sturgeon.female.1978 <- subset(sturgeon, sex == "FEMALE" &
                                 year == "1978")
hist(sturgeon.female.1978$fklngth)

#############################################################
# Visualize the data 
# Make a histogram of the variable fklngth in the sturgeon
# this uses the base R plotting 
hist(sturgeon$fklngth)

# use "sturgeon" dataframe to make plot called mygraph
# and define x axis as representing fklngth

plot1 <- ggplot(sturgeon) +
  geom_histogram(aes(x = fklngth, y = ..density..),
                 binwidth = 2, fill = "grey", color = "black")
plot1
# Try changing the binwidth- what happens?

# Data distribution fklngth by subsets of sex and year
# split previous graph per year (rows) and sex (columns)
plot2 <- ggplot(sturgeon) +
  geom_histogram(aes(x = fklngth, y = ..density..),
                 binwidth = 5, fill = "grey", color = "black")+
  facet_grid(sex ~ year)
plot2

# QQ plot de fklngth
qqnorm(sturgeon$fklngth)
qqline(sturgeon$fklngth)
# Are there outliers?

# Boxplot of fklngth by sex, with whiskers
# base graphics
boxplot(sturgeon$fklngth ~ sturgeon$sex, notch = TRUE)

#ggplot2 version
plot3<-ggplot(sturgeon, aes(x=sex, y=fklngth))+ 
  geom_boxplot()+
  theme_minimal()
plot3

##############################################
#### T-Test ####
# Test to see if there is a difference in the fork length of male and female fish

# First test assumptions

# test for normality of raw data
shapiro.test(sturgeon_complete$fklngth)
# fail- these test are highly influenced by n
hist(sturgeon_complete$fklngth)
# not normal but ok- you can transform the variable to achieve normality

# test homogeneity of variance
leveneTest(fklngth~sex, data=sturgeon_complete)
# meets assumption of homogeneity of variance between groups

# Run the t-test- defualt is the Welch's two sample t-test
t.test(data=sturgeon_complete, fklngth~sex)
# there is a statically significant difference between fork length in the two groups

#### Anova ####
# Test to see if there is a difference in the fork length in females by year
# make some plots

plot4<-ggplot(sturgeon.female, aes(x=as.factor(year), y=fklngth))+ 
  geom_boxplot()+
  theme_minimal()
plot4

# Test assumptions
# test for normality of raw data
shapiro.test(sturgeon.female$fklngth) #fail test
hist(sturgeon.female$fklngth)
#not normal

#transform the variable 
shapiro.test(log10(sturgeon.female$fklngth)) #fail
hist(log10(sturgeon.female$fklngth))
#looks better

# test homogenity of variance
leveneTest(fklngth ~ as.factor(year), data=sturgeon.female)
# the variance is  homogenous between the three groups

# This is a typeI anova- testing between groups
anova1=anova(lm(fklngth~as.factor(year), data=sturgeon.female))
anova1

# p-value is high so we accept the H0, thereis no difference between the groups
# if we reject the H0 we would need to follow this up with a Tukey's post-hoc test
Tukey1= TukeyHSD(aov(fklngth~as.factor(year), data=sturgeon.female))
Tukey1
# no difference between groups

# plot the differences
plot(Tukey1)


##########################################################
#### Relational Statistics ####
# Scatterplot of fklngth as a function of age

# Matrix of scatterplots of all pairs of variables in dataframe sturgeon
# with lowess trace
pairs(sturgeon_complete[,1:7], panel = panel.smooth)

#### Correlation ####
cor=cor(sturgeon_complete[,1:7])
cor
#visualize the correlation martrix
corrplot(cor)

# testing correlation between two variables 
cor.test(sturgeon_complete$age, sturgeon_complete$fklngth)

#### Lienar regression ####
# test the relationship between fork length and age- linear model
lm1=lm(data=sturgeon_complete, fklngth~age)
summary(lm1)

# now just the females
lm2=lm(data=sturgeon.female, fklngth~age)
summary(lm2)

# test assumptions of residuals
# normality of residuals 
shapiro.test(resid(lm2)) #pass

# homoscedasticity of the residuals
bptest(lm2) #pass

# lack of serial autocorrelation in residuals
durbinWatsonTest(lm2) #pass

# linearity of residuals
resettest(lm2) #pass

# base graphics
plot(sturgeon_complete$fklngth ~sturgeon_complete$age)

#ggplot2 version
plot4<-ggplot(sturgeon, aes(x=age, y=fklngth))
plot4+ geom_point(size=5, shape=25, color="blue")
