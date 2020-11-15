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
df$month_cat=as.factor(df$Month)
df$log_ozone=log10(df$Ozone)

#############################################################
# Summarize the dataframe
summary(df)
describe(df)

# Check for missing data and address it if necessary
df_clean <- na.omit(df)
#############################################################
# Visualize the data 
# Make a histogram of the variable ozone
hist1 <- ggplot(data=df_clean) +
  geom_histogram(aes(x = Ozone, y = ..density..),
                 binwidth = 10, fill = "grey", color = "black")
hist1

# Data distribution ozone by subset month
hist2 <- ggplot(df_clean) +
  geom_histogram(aes(x = Ozone, y = ..density..),
                 binwidth = 20, fill = "grey", color = "black")+
  facet_grid(~as.factor(Month))
hist2

# Boxplot of ozone by month, with whiskers
# Hint: What type of variable is month? Boxplots require 1 categorical variables and 
# 1 continuous variable
boxplot(df_clean$Ozone ~ df_clean$Month, notch = FALSE)

boxplot2 <- ggplot(df_clean) +
  geom_boxplot(aes(x = as.factor(Month), y =Ozone))
boxplot2

#### T-Test #####
# test to see if there is a difference between ozone in the first and second half of the month 
# (using the binned variable from above)
# plot the data
plot1 = ggplot(df_clean, aes(x=temp_bin, y=log_ozone))+
  geom_boxplot()
plot1

# Test assumptions
# test for normality of raw data
shapiro.test(df_clean$Ozone) #fail

# If you fail try a transformation
shapiro.test(log10(df_clean$Ozone))
hist(df_clean$log_ozone)
#better but still fail

# test homogenity of variance
leveneTest(log_ozone ~ temp_bin, data=df_clean)
# the variance is homogenous between the two groups


# Run the T-test
t.test(log_ozone ~ temp_bin, data=df_clean)

# Can you interpret the results? Is the test significant?


#### ANOVA ####
# test to see if there is a difference in the mean ozone concentration measured by month

# plot the data 
plot2 = ggplot(df_clean, aes(x=month_cat, y=log_ozone))+
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot()+
  # add error bars to the plot
  xlab("Month")+
  ylab("Ozone")+
  theme_minimal()
plot2

# What are the little black dots on this plot?

# Test assumptions
# test for normality of raw data
shapiro.test(df_clean$log_ozone)
# fail
hist(df_clean$log_ozone)
# not normal but ok

# test homogenity of variance
leveneTest(log_ozone ~ month_cat, data=df_clean)
# the variance is homogenous between the groups

# run an anova
anova1=anova(lm(log_ozone ~ month_cat, data=df_clean))
anova1
# Is the test significant? Should we do a post-hoc follow up?

# p-value is low so we reject the H0, there is a difference between the groups
# need to follow this up with a Tukey's post-hoc test
Tukey1= TukeyHSD(aov(log_ozone ~ month_cat, data=df_clean))
Tukey1

# plot the differences
plot(Tukey1)

#What is your interpreation of these results?
# 7-5, 8-5, and 9-7 are different 
# This pattern appears to be early and late months- this may have something to do with temperature

#### Correlation #####
# test if there is there a linear assoication between temperature and ozone 
# Scatterplot of ozone as a function of temperature
scatter1<-ggplot(df_clean, aes(x=Temp, y=Ozone))+
  geom_point()
scatter1

# Now colour the points by month
scatter2<-ggplot(df_clean, aes(x=Temp, y=Ozone, color=as.factor(Month)))+
  geom_point()
scatter2

# This uses the default R colours

# Extra code for changing the colour palletts
# You can use a colour or a hexcode (e.g.#fc120c)
#check out www.colorbrewer.com to help you pick colours
scatter3<-ggplot(df_clean, aes(x=Temp, y=Ozone, color=as.factor(Month)))+
  geom_point()+
  scale_color_manual(values=c("blue","#fc120c","yellow","orange","purple"))
scatter3

# or you can use pre-created colour palletts
install.packages("viridis")
library(viridis)

plot6<-ggplot(df_clean, aes(x=Temp, y=Ozone, color=as.factor(Month)))+
  geom_point()+
  scale_color_viridis(discrete=TRUE)+
  theme_minimal()+
  xlab("Temperature")+
  ylab("Ozone")+
  labs(colour = "Months") 
plot6

# testing single correlation
cor.test(df_clean$Temp, df_clean$log_ozone)

#What is your interpretation of these results?
# positive association between Temp and Ozone

# Test other combinations
# Matrix of scatterplots of all pairs of variables in air quality dataset
pairs(df_clean, panel = panel.smooth)

# make a correlation matrix
cor=cor(df_clean [,1:6])
cor
# visualize the correlations using corrplot
corrplot(cor)


#### Linear Regression ####
# Make a plot
ggplot(data = df_clean, aes(x = Temp, y = log_ozone)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = TRUE)
# Looks pretty linear

# Linear model
lm1=lm(log_ozone~Temp, data=df_clean)
summary(lm1)

# test assumptions on RESIDUALS
resettest(lm1) # pass
dwtest(lm1) # pass
bptest(lm1) # fail
shapiro.test(resid(lm1)) # fail
hist(resid(lm1))

# Are there any other variables that can be used?
lm2 = lm(log_ozone~Temp+Wind+Solar.R, data=df_clean)
summary(lm2)

# test assumptions on RESIDUALS
resettest(lm2) # fail
dwtest(lm2) # fail
bptest(lm2) # pass
shapiro.test(resid(lm2)) # pass
hist(resid(lm2))

# test for multicolinearity
vif(lm2)

# What model is better?