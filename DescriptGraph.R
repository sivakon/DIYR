#
#
############### Some Descriptive Statistics & Associated Graphing ###############
#
# This script assumes you have worked through all the previous notes from 
# the web page and you have downloaded, installed, and updated all available
# R packages. 

# If an additional library is needed, it will be listed in the script and loaded.

library(Rcmdr)

# Start by reading the data file into R from the web address.

example3 <- read.table("http://www.unt.edu/rss/class/Jon/R_SC/Module3/ExampleData3.txt",
                       header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)
summary(example3)

# Get rid of the goofy characters in the name of the age variable; by 
# using the 'names' function to change whatever the first name [1] of 
# example3 is (in this case: ï..age) to "age".

names(example3)
names(example3) [1] <- "age"
names(example3)

# Attach the data so we can refer to variable names directly (without $).

attach(example3)

# Get some quick summaries of each variable in the data.
# WHEN USED WITH A SINGLE VARIABLE OR A DATA FRAME: The 'summary' function 
# returns some basic descriptive statistics for each variable: minimum, 
# 1st quantile, median, mean, 3rd quantile,maximum, and number of NA (missing 
# data) if present. If the variable is nominal; then 'summary' returns the 
# number of cases for each value of the variable (and number of NA if present). 

summary(gender)
summary(age)

summary(example3)

# Mean of age for each category of gender.

tapply(example3$age, list(gender=example3$gender), mean, na.rm=TRUE)

# Summary of age BY gender.

numSummary(example3[,"age"], groups=example3$gender, statistics=c("mean", 
                                                                  "sd", "quantiles"), quantiles=c(0,.25,.5,.75,1))

# Count missing values in each vector/variable of example3.

sapply(example3, function(x)(sum(is.na(x))))

# Mean, variance, standard deviation, sum and product of age.

mean(age)
var(age)
sd(age)
sum(age)
prod(age)

# Number of rows/cases of example3 (data frame) and age (vector).

nrow(example3)
length(age)

# Median of age.

median(age)

# Trimmed mean: 20% trim of age, remove any missing data (NA).

mean(age, tr=.2, na.rm=TRUE)

# Weighted mean: mean of age with weights from marital.

weighted.mean(age, marital)

# Weighted standard deviation function -- examples of how to write basic functions:

weighted.sd <- function(x, w){
  sum.w <- sum(w)
  sum.w2 <- sum(w^2)
  mean.w <- sum(x * w) / sum(w)
  x.sd.w<-sqrt((sum.w / (sum.w^2 - sum.w2)) * sum(w * (x - mean.w)^2))
  return(x.sd.w)
}

weighted.sd(age, marital)

# Skewness & Kurtosis functions.

skew <- function(x){
  ((sum(((x - mean(x))/(sd(x)))^3))/(length(x) - 1))
}

kurt <- function(x,k.type){
  ifelse(k.type == 2,
         ((sum(((x - mean(x))/(sd(x)))^4))/(length(x) - 1)),
         (((sum(((x - mean(x))/(sd(x)))^4))/(length(x) - 1)) - 3)
  )
}

skew(age)
kurt(age,1)
kurt(age,2)

##############################################
##### Some basic/routine graphics. #####

# If you ever want to change the color of something; you'll likely want to take a 
# look at the color names available...there are something like 657 colors in R.

colors()

# Histogram of age (how it's suppose to look). Keep in mind, a later tutorial covers 
# how to manipulate the x- and y- axis scales, labels/titles, colors (col), and other graphical 
# parameters. 

hist(age)

hist(age, col = "lightblue1")

# Histogram/bar graph (how it's NOT suppose to look).

barplot(age)

# Bar chart for gender (remember 3202 females & 3198 males; so the plot is 
# rather boring).

plot(gender)

# Convert gender to a numeric vector version of gender. Changing 
# the values from m:f to 1:2 for males & females if you prefer.

genderRC <- as.numeric(gender)
plot(genderRC)

# Boxplot of age.

boxplot(age)

# Basic scatter plot.

plot(age, income)

# Another scatter plot.

sunflowerplot(age, income)

# Sub-group scatter plot; income predicted by age with separate plots for 
# males and females. *Note the convention (in graphics and model specification
# when we get to it) of using "y ~ x..." to say "y predicted by x...". 

coplot(income ~ age | gender)

# Interaction plot (typically used with ANOVA); looking at the interaction of  
# gender and marital (status: 1 = married, 0 = not married) on (mean) age. 

interaction.plot(gender, marital, age)

# Create a data frame of age & income & employ (years employed); demographic variables.

demog <- data.frame(age, income, employ)

# Simple scatter plot matrix.

pairs(demog)

###############################################################################

# One of the really strong benefits to using R is the ability to 
# graph just about anything and also the ability to modify and 
# specify everything in a graph. 
# As we will see; the graphics from this script represent 
# just the very tip of the iceberg when it comes to complexity 
# in graphing with R. 

# I may add to this at a later date (as is always the case...); but for 
# now I think we can move forward. 

# End: minor updates, July 18, 2012.
