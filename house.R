# Kaggle: House Prices: Advanced Regression Techniques
# Annie Eby
# 11/27/2018
#
#
# Download Data: https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data 

##################################################################################
################################### PREPARE FILE #################################
##################################################################################

# load libraries using pacman to make the file more easily shareable
if(!require(pacman)){install.packages('pacman')}
pacman::p_load(dplyr, tidyr, ggplot2, caret,randomForest, rpart, 
               rpart.plot, RColorBrewer, rattle, party, Hmisc, mlr, data.table)


# set working directory and files; read.csv automatically makes strings into factors; override this.
setwd('~/Documents/Programming Projects/Data Sets/house_all/')
train = read.csv("train.csv", stringsAsFactors = FALSE)
test = read.csv("test.csv", stringsAsFactors = FALSE)

##################################################################################
################################ OVERVIEW OF DATA ################################ 
##################################################################################

# view class to verify it is a data frame
class(train)

# view dimensions: 1460 rows, 81 columns
dim(train)

# look at column names
names(train)

# take a quick look at the dataframe
str(train)
summary(train)

#tbl_df(train)

# columns with NA's: LotFrontage, MasVnrArea, GarageYrBlt; many more but <NA> is char
# ways to deal with NA's: train[complete.cases(train), ]; na.omit(train)

head(train)

# note: data is already tidy with all columns as variables (contains all values that measure the same attribute across units) and all rows as observations (contains all values measured on the same unit across attributes); one type of observational unit per table
# because data is tidy, we do not need to rearrange rows and columns using gather() or spread()
# separate(dataset, name of colum to separate, c('new column', 'new column'), sep); unite() does reverse: unite(data, 'new column', ...[selected columns], sep = _)

# Ideas to tidy:
# separate: BldgType, HouseStyle
# make variables into binary variables?
# Make 'Gd' values numeric (in ExterQual, BsmtQual)

##################################################################################
################################ INITIAL INSIGHTS ################################ 
##################################################################################


# Visualize SalePrice distribution
boxplot(train$SalePrice, horizontal = TRUE) #outliers are anything above $350K
# boxplot(train) not possible to plot whole data frame bc non-numeric arguments
hist(train$SalePrice, breaks = 50) # most houses $150 - $200K
plot(train$SalePrice, main="SalePrice", sub="Full Training Data Set",
     xlab="Observations", ylab="Price") # most houses $100K = $300K 

# Use the pairs command to plot all variables against eachother
# Use slice to plot only the first 100 observations.	
# pairs(train %>% slice(1:100)) # does not work bc non-numerc arguments

plot(SalePrice ~ MSSubClass, data=train)
plot(SalePrice ~ MSZoning, data=train)
plot(SalePrice ~ LotFrontage, data=train)
plot(SalePrice ~ LotArea, data=train)
plot(SalePrice ~ Street, data=train) #no plot
plot(SalePrice ~ Alley, data=train) #no plot
plot(SalePrice ~ LotShape, data=train) #no plot
plot(SalePrice ~ LandContour, data=train) #no plot
plot(SalePrice ~ Utilities, data=train) #no plot
plot(SalePrice ~ LotConfig, data=train) 
plot(SalePrice ~ LandSlope, data=train) 
plot(SalePrice ~ Neighborhood, data=train) 

#Sale price shows strong correlation with Overall Quality
plot(SalePrice ~ OverallQual, data=train) 

#Sale price has high price for middle quality (non-linear correlation)
plot(SalePrice ~ OverallCond, data=train) 

#Sale price somewhat (minimally) correlated to year built
plot(SalePrice ~ YearBuilt, data=train) 

#Sale price somewhat (minimally) correlated to year remodel added
plot(SalePrice ~ YearRemodAdd, data=train) 

##################################################################################
################################## DATA CLEANING AND #############################
################################# FEATURE ENGINEERING  ###########################
##################################################################################


# DPLYR DATA MANIPULATION REFRESHER
# on variables:
# select(df, 1:4, -2)
# mutate(df, z = x + y)
# on observations:
# filter(df, a > 0)
#     x %in% c(a, b, c), TRUE if x is in the vector c(a, b, c)
# on groups:
# summarize()
# pipe operator: object %>% function()
# group_by()

# Regex: https://www.rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf

# create a new table selecting only the last four variables from above
year_cond <- select(train, OverallQual, OverallCond, YearBuilt, YearRemodAdd)
hist(year_cond)
plot(SalePrice ~ year_cond, data=train) # does not work

# filter houses to include only 2 story houses (445 observations)
filter(train, HouseStyle == '2Story')

# filter houses to include only houses with equal to or more than 1500 sqft of ground floor living area
filter(train, GrLivArea >= 1500)

# arrange by SalePrice, then by YrSold
arrange(train, SalePrice, YrSold)

# summarize
# summarize(train, sum = sum(SalePrice, na.rm = TRUE), avg = mean(SalePrice, na.rm = TRUE), var = var(SalePrice, na.rm = TRUE))
# doesn't work


