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
pacman::p_load(dplyr, tidyr, purrr, ggplot2, caret,randomForest, rpart, 
               rpart.plot, RColorBrewer, rattle, party, Hmisc, mlr, data.table, GGally, ggcorrplot, corrplot)


# set working directory and files; read.csv automatically makes strings into factors; override this.
setwd('~/Documents/Programming Projects/Data Sets/house_all/')
train = read.csv("train.csv", stringsAsFactors = FALSE)
test = read.csv("test.csv", stringsAsFactors = FALSE)

##################################################################################
################################ OVERVIEW OF DATA ################################ 
##################################################################################

# The data set used in this project describes the sale of individual residential
# property in Ames, Iowa from 2006 to 2010. The data set contains 2930
# observations and 80 explanatory variables (23 nominal, 23
# ordinal, 14 discrete, and 20 continuous) involved in assessing home values. [A description of the data is linked here.](https://github.com/annieeby/Capstone-House-Prices/blob/master/data_description.txt)

# view class to verify it is a data frame
class(train)

# view dimensions: 1460 rows, 81 columns
dim(train)

# look at column names
names(train)

# take a quick look at the dataframe
str(train)
summary(train)
head(train)



##################################################################################
################################## PRE-PROCESSING ################################
##################################################################################

# The data is already tidy, with all columns as variables (contains all values
# that measure the same attribute across units) and all rows as observations
# (contains all values measured on the same unit across attributes); one type of
# observational unit per table


################################# Combine Dataframes ############################# 

# In order to manipulate our variables, we need to concatenate the train and
# test sets. We'll make a new dataframe called “combi” with all the same rows as
# the original two datasets, stacked in the order in which we specified: train
# first, and test second.

# Use bind_rows(), which automatically creates new columns with NA values, as needed
# dataset = 'string' creates a column with 'string' as value for easier mapping later.

combi = bind_rows(train %>% mutate(dataset = 'train'), 
                  test %>% mutate(dataset = 'test')) 

# Observe the structure of the combined dataframe.
str(combi)

##################################################################################
################################### Remove Outliers ##############################

# For linear regression to be effective, the model must be robust to outliers.
# There may be some more sophisticated ways of handling outliers. A more refined
# approach called robust regression keeps outliers but attaches very little
# weight to them. Another called Least Trimmed Squares, or LTS Regression,
# defines outliers as the points least fitting for the regression models, rather
# than filtering them out beforehand. Filtering outliers in advance could
# potentially make the model easier to understand but there may be an accuracy
# tradeoff and potential for bias and error.  One person notes "If you remove
# any proportion of the data in order to optimize some statistic, the remaining
# data no longer represent the population in any way that would be correctly
# analyzed using standard statistical procedures (that is, those which assume
# your data are a random sample). Specifically, all confidence intervals,
# prediction intervals, and p-values would be erroneous--and perhaps extremely
# so when as much as 10% of the data are removed."

# Outliers can be removed manually based on visualization or based on the data
# point's distance from the Inter-Quartile Range. They can also be removed by
# percent (e.g. bottom 10%, top 10%).

# The dataset's author recommends **removing the data points with more than 4000
# square feet** from the data set -- three of them are true outliers (Partial
# Sales that likely don’t represent actual market values) and two of them are
# simply unusual sales (very large houses priced relatively appropriately). A
# plot of SALE PRICE versus GR LIV AREA quickly indicates these points.

plot(SalePrice ~ GrLivArea, data=train, xlab = "Goundfloor Living Area", ylab = "Sale Price", main = "Checking for Outliers") 

# Remove GrLivArea >= 4000 from the dataset.

train <- subset(train, GrLivArea<=4000)

#<span style="color:red">QUESTION: Correct to apply to train not combi?</span> 

# Observe new GrLivArea
plot(SalePrice ~ GrLivArea, data=train) 

#Observe the dataset is now two observations fewer. <span style="color:red">Check previously: [ ]</span>
str(combi)

##################################################################################

# Filter numeric vars:
numeric_train <- Filter(is.numeric, subset(combi, dataset == "train"))
str(numeric_train)

##################################################################################

# Compute IQR; could also use the function IQR() 
lowerq = quantile(numeric_train, na.rm = TRUE)[2]
upperq = quantile(numeric_train, na.rm = TRUE)[4]
iqr = upperq - lowerq 

# Compute the bounds for a mild outlier:
mild.threshold.upper = upperq + (iqr * 1.5) 
mild.threshold.lower = lowerq - (iqr * 1.5)

# mild outliers
mild_outliers <- which(numeric_train > mild.threshold.upper | numeric_train < mild.threshold.lower)
str(mild_outliers)
  
# Compute the bounds for an extreme outlier:
extreme.threshold.upper = (iqr * 3) + upperq
extreme.threshold.lower = lowerq - (iqr * 3)
  
# extreme outliers
extreme_outliers <- which(numeric_train > extreme.threshold.upper | numeric_train < extreme.threshold.lower)
str(extreme_outliers)
  
# all outliers = mild + extreme outliers
all_outliers <- which(numeric_train > mild.threshold.upper | numeric_train < mild.threshold.lower)
str(all_outliers)

# Option 1: remove extreme outliers only
numeric_train <- numeric_train[-extreme_outliers]

# Option 2: remove all outliers
# numeric_train <- numeric_train[-all_outliers]

# Determine if outliers were correctly removed.
str(numeric_train)
#<span style="color:red">QUESTION: Did this actually remove any data and how do I know which data / how many data points? Also, since I did this in a new dataframe, will it alter my original combi frame as well?</span> 

# Another method of removing outliers: replace based on percentiles. E.g. remove
# all bottom 5% and top 95% data points.
# percSalePrice =quantile(combi$SalePrice,probs = c(.05,.95), na.rm = TRUE)
# combi = combi %>% 
#   mutate(SalePrice = ifelse(SalePrice < percSalePrice[1], percSalePrice[1], 
#                             ifelse(SalePrice > percSalePrice[2], percSalePrice[2], SalePrice)))

############################## Impute Missing Values #############################

#There are some simple ways to deal with NA's: train[complete.cases(train), ] or
#na.omit(train). But that would remove far too much data. Moreover, this
#data set has many NA's that do not indicate _missing_ data but rather mean “not
#present”. For example, the NA’s in categorical variables like Alley,
#LotFrontage, MasVnrArea, GarageYrBlt. Those NA's should indicate that the
#feature doesn't exist for this observation. But other NAs would be better
#represented as a mean. Still others, would be better replaced with a string
#that indicates "typical". The dataset requires going through each
#variable one by one to determine what kind of replacement is best suited.

head(combi)  
  
#NA counts for each column in a dataframe:
sapply(combi, function(x) sum(is.na(x)))

# Replace multiple columns of NAs with most appropriate values

# Replace NA with "None"
vars_to_none = c("Alley", "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinSF1", "BsmtFinType2", "FireplaceQu", "GarageType", "GarageYrBlt", "GarageFinish", "GarageQual", "GarageCond", "PoolQC", "Fence", "MiscFeature", "MasVnrType")
combi[vars_to_none] <- sapply(combi %>% select(vars_to_none), function(x) x = ifelse(is.na(x), "None", x))

# Replace NA with mean
vars_to_mean = c("LotFrontage", "MasVnrArea", "GarageYrBlt")
combi[vars_to_mean] <- sapply(combi %>% select(vars_to_mean), function(x) x = ifelse(is.na(x), mean(x, na.rm = T), x))

# Replace NA with 0
vars_to_zero = c("BedroomAbvGr", "GarageCars", "GarageArea", "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF", "BsmtFullBath", "BsmtHalfBath")
combi[vars_to_zero] <- sapply(combi %>% select(vars_to_zero), function(x) x = ifelse(is.na(x), 0, x))

# Replace NA with "Typ"
vars_to_typ = c("Functional")
combi[vars_to_typ] <- sapply(combi %>% select(vars_to_typ), function(x) x = ifelse(is.na(x), "Typ", x))

# Replace NA with "TA"
vars_to_ta = c("KitchenQual")
combi[vars_to_ta] <- sapply(combi %>% select(vars_to_ta), function(x) x = ifelse(is.na(x), "TA", x))

# Unclear what to do with NA: MSZoning, Utilities, Exterior1st, Exterior2nd,  Electrical  
# Change this one when figure out how to handle.
vars_to_temp = c("MSZoning", "Utilities", "Exterior1st", "Exterior2nd",  "Electrical")  
combi[vars_to_temp] <- sapply(combi %>% select(vars_to_temp), function(x) x = ifelse(is.na(x), "Temp", x))

head(combi)
summary(combi)

######################## Discretize Categorical Attributes #######################

# Some numerical features are actually categories. The 2919 numerics can be
# factored to the appropriate number of levels.

# MSSubClass identifies the type of dwelling involved in a sale. It should be a factor with 16 levels.
str(combi$MSSubClass)
combi$MSSubClass <- factor(combi$MSSubClass)
str(combi$MSSubClass)

# Mosold means "month sold". It should be a factor with 12 levels.
str(combi$MoSold)
combi$MoSold <- factor(combi$MoSold)
str(combi$MoSold)

################################# Feature Engineering ############################

# Feature engineering is one of the most important aspect of machine learning.
# Here, we want to create variables that provide the best information for the machine
# learning models. Some features in the dataset are overlapping or very specific or unclear. By
# combining like variables we can get a simplified, and perhaps better
# overarching understanding of the data. For example, a better correlation to sale price than square footage and bathrooms by floor
# level might be total square footage and the total number of bathrooms.

#-----------------------------------Binary Variables-----------------------------#

# Has masonry veneer or not
combi$HasMasVnr <- ifelse((combi$MasVnrType == "None"), 0, 1)

# House completed before sale or not
combi$BoughtOffPlan <- ifelse((combi$SaleCondition == "Partial"), 1, 0) 

# Check for added column names
colnames(combi)


#--------------------------- Stack Overflow Answer Start -----------------------#

qual_cols <- c("ExterQual", "ExterCond", "BsmtQual", "BsmtCond", "HeatingQC", "KitchenQual", "FireplaceQu", "GarageQual", "GarageCond", "PoolQC")
grades <- c( "Po", "Fa", "TA", "Gd", "Ex")

newdata <- combi[qual_cols]
newdata[] <- lapply(combi[qual_cols], function(x) 
  setNames(c(1, 2, 4, 6, 11), grades)[x])
nm1 <- grep("(Cond|Qual)$", names(newdata), value = TRUE)
nm2 <- sub("[A-Z][a-z]+$", "", nm1)
nm3 <- paste0(unique(nm2), 'Grade')
newdata[nm3] <- lapply(split.default(newdata[nm1], nm2), function(x) Reduce(`*`, x))

set.seed(24)
combi <- as.data.frame(matrix(sample(grades, 10 * 5, replace = TRUE), 
                              ncol = 10, dimnames = list(NULL, qual_cols)), stringsAsFactors = FALSE)

#--------------------------- Stack Overflow Answer End -----------------------#



# Overall quality of the house
combi$OverallGrade <- combi$OverallQual * combi$OverallCond
head(combi$OverallGrade)

# Overall quality of the garage
combi$GarageGrade <- combi$GarageQual * combi$GarageCond
head(combi$GarageGrade)

# Overall quality of the exterior
combi$ExterGrade <- combi$ExterQual * combi$ExterCond
head(combi$ExterGrade)

# Overall kitchen score
combi$KitchenScore <- combi$KitchenAbvGr * combi$KitchenQual
head(combi$KitchenScore)

# Overall fireplace score
combi$FireplaceScore <- combi$Fireplaces * combi$FireplaceQu
head(combi$FireplaceScore)

# Overall garage score
combi$GarageScore <- combi$GarageArea * combi$GarageQual
head(combi$GarageScore)

# Overall pool score
combi$PoolScore <- combi$PoolArea * combi$PoolQC

# Simplified overall quality of the house
combi$SimplOverallGrade <- combi$SimplOverallQual * combi$SimplOverallCond

# Simplified overall quality of the exterior
combi$SimplExterGrade <- combi$SimplExterQual * combi$SimplExterCond

# Simplified overall pool score
combi$SimplPoolScore <- combi$PoolArea * combi$SimplPoolQC

# Simplified overall garage score
combi$SimplGarageScore <- combi$GarageArea * combi$SimplGarageQual

# Simplified overall fireplace score
combi$SimplFireplaceScore <- combi$Fireplaces * combi$SimplFireplaceQu

# Simplified overall kitchen score
combi$SimplKitchenScore <- combi$KitchenAbvGr * combi$SimplKitchenQual

# Total number of bathrooms
combi$TotalBath <- combi$BsmtFullBath + (0.5 * combi$BsmtHalfBath) + 
  combi$FullBath + (0.5 * combi$HalfBath)

# Total SF for house (incl. basement)
combi$AllSF <- combi$GrLivArea + combi$TotalBsmtSF

# Total SF for 1st + 2nd floors
combi$AllFlrsSF <- combi$X1stFlrSF + combi$X2ndFlrSF

# Total SF for porch
combi$AllPorchSF <- combi$OpenPorchSF + combi$EnclosedPorch + 
  combi$X3SsnPorch + combi$ScreenPorch

########################### Transform Skewed Attributes ##########################

# Regression techniques require a normal distribution. 
# A histogram plot shows the distribution of the target variable ‘SalePrice’ as
# being was right-skewed. A "skewed right" distribution is one in which the tail
# is on the right side. Most sales prices are on the low side of the scale and
# there are very few high sales prices.  

combi_train <- combi[combi$dataset == "train", ]

# Histogram of SalePrice with skewed-right distribution 
ggplot(data=combi_train, aes(SalePrice)) + 
  ggtitle("Histogram for Sale Price") + 
  xlab("Sale Price") +
  ylab("Count")+
  geom_histogram(binwidth = 10000)

# We can normalize the data by way of log-transformation
combi_train$SalePrice <- log1p(combi_train$SalePrice)

# Histogram of SalePrice with normal distribution 
ggplot(data=combi_train, aes(SalePrice)) + 
  ggtitle("Histogram for Sale Price") + 
  xlab("Sale Price") +
  ylab("Count")+
  geom_histogram()

# It's possible to transform any numeric variable using log transformation, e.g.:
hist(combi$TotalBath)
combi$TotalBath <- log1p(combi$TotalBath)
hist(combi$TotalBath)

########################### ASK HOW TO VIEW MULTIPLE ####################

# View all numeric variable distributions

sapply(combi %>% select(numeric_train), function(x) hist(x) %>%
         x <- log1p(x) %>%
         hist(x))

# sapply(numeric_train %>% function(x) hist(x))
# <span style="color:red">Doesn't work^</span>
       
# Log transform and view normalized distribution for all numeric variables
sapply(combi %>% filter(is.numeric, x), function(x) hist(x) %>% 
         x <- log1p(x) %>%
         hist(x))
# <span style="color:red">Doesn't work^</span>

########################### Bivariate Relationship Analysis ######################

# Create a boxplot for every numeric variable with variable names on the x-axis
# and SalePrice on the y-axis.

sapply(combi %>% filter(is.numeric, x), function(x) 
  ggplot(train, aes(x = x, y = SalePrice, fill = x)) + 
    geom_boxplot())
# <span style="color:red">Doesn't work^</span>

############################### Correlation Matrix ###############################

# Correlation is a measure of the linear relationship between variables. 
# +1 = perfect positive linear relationship
# 0 = no linear relationship
# -1 = perfect negative linear relationship
# Both negative and positive linear relationships indicate highly correlated variables.
# Typically a correlation greater that +0.7 or less than -0.7 is cause for concern, though there is no definitive cutoff threshold.


# Examples:
# Ground floor living area and Sale price are correlated at 0.7205163
cor(train$GrLivArea, train$SalePrice)

# All correlations (numeric only)
cor(numeric_train)

# Multicollinearity exists whenever two or more of the predictors in a regression 
# model are moderately or highly correlated. This correlation is a problem because 
# independent variables should be independent. If the degree of correlation between 
# variables is high enough, it can cause problems when you fit the model and interpret 
# the results. The idea is that you can change the value of one independent variable 
# and not the others. However, when independent variables are correlated, it indicates 
# that changes in one variable are associated with shifts in another variable. The 
# stronger the correlation, the more difficult it is to change one variable without 
# changing another. It becomes difficult for the model to estimate the relationship 
# between each independent variable and the dependent variable independently because the 
# independent variables tend to change in unison. 

# To check for multicollinearity, I will create a
# correlation matrix with respect to the target variable ‘SalePrice’. Some
# variables are highly correlated such as [TBD - e.g. GarageArea and GarageCars].  This
# makes sense because [TBD -e.g. the size of a garage determines('GarageArea') the number 
# of cars that fit in it ('GarageCars')]. Other highly correlated variables show similar 
# dependency.

############################### Problematic Code Start ###############################

forcorrplot <- cor(numeric_train)
corrplot(forcorrplot, method="color")


# Compute a correlation matrix
corr_train <- combi_train %>% keep(is.numeric) %>% subset(combi_train, -train$Id)
data(corr_train)
corr <- round(cor(corr_train), 1)
head(corr[, 1:6])

# Compute a matrix of correlation p-values.
# A note on p-values: The P-value is the probability that you would have found the current result if the correlation coefficient were in fact zero (null hypothesis). If this probability is lower than the conventional 5% (P<0.05) the correlation coefficient is called statistically significant.
p.mat <- cor_pmat(train)
head(p.mat[, 1:4])

# Visualize the correlation matrix
# method = "square" (default)
ggcorrplot(corr)

# using hierarchical clustering (hc.order) and correlation coefficients (lab), barring the no significant coefficient (p.mat)
ggcorrplot(corr, hc.order = TRUE, outline.col = "white", lab = TRUE, p.mat = p.mat)


cor(train %>%  select(numericalVars)) %>% View()

########################### Select Attributes for Training #######################
# Now we can select the feature subset. We should be able to eliminate some of
# the original variables that are represented by the new engineered combination
# variables. 

#-------------------------- Remove Polynomial Attributes ------------------------#

# create a vector with all the variables used to create the polynomial attributes
vars_to_remove <- c("OverallQual", "OverallCond", "GarageQual", "GarageCond", 
                    "ExterQual", "ExterCond", "KitchenAbvGr", "KitchenQual", "Fireplaces", 
                    "FireplaceQu", "GarageArea", "GarageQual", "PoolArea", "PoolQC",
                    "SimplOverallQual", "SimplOverallCond", "SimplExterQual", "SimplExterCond",
                    "PoolArea", "SimplPoolQC", "GarageArea", "SimplGarageQual", "Fireplaces", 
                    "SimplFireplaceQu", "KitchenAbvGr", "SimplKitchenQual", "BsmtFullBath", 
                    "BsmtHalfBath", "FullBath", "HalfBath", "GrLivArea", "TotalBsmtSF", 
                    "X1stFlrSF", "X2ndFlrSF", "OpenPorchSF", "EnclosedPorch", "X3SsnPorch",
                    "ScreenPorch")

# remove variables
combi <- combi[, !(colnames(combi) %in% c(vars_to_remove))]

# check updated variable names
names(combi)

#-------------------------- Remove Correlated Attributes ------------------------#

# The correlation matrices reveal attribute pairs whose correlation values are
# more than ... These attributes can be
# dropped as well. 


#-------------------------Remove Non-Normal Distributions------------------------#

#Some attributes will perform poorly because they are not normally distributed
#even after transformation. These attributes can be removed.


############################## SPLIT DATAFRAMES ################################

# Split the test and training sets back into their original states

train <- combi[combi$dataset=='train',]
test <- combi[combi$dataset=='test',]
glimpse(train)


##################################################################################
################################# REGRESSION MODELS ##############################
##################################################################################

# Evaluation approach: 10-fold cross validation + Root-Mean-Square-Error (RMSE) 
# to evaluate the performance of models.
# http://www.shihaiyang.me/2018/04/16/house-prices/


############################### Linear Regression ################################

# R Squared
# Adjusted R Squared adjusts the R Squared value to account for the number of
# independent variables used relative to the number of data points. Multiple
# R-Squared will always increase if you add more independent variables, but
# adjusted R-squared will decrease if you add an independent variable that
# doesn't help the model. This is a good way to determine if a variable should
# even be included in the model.
# R-squared can be calculated as 1 - SSE/SST

# Coefficients
# A coefficient of zero means that the value of the indpendent valriable does
# not change our prediction for the dpendent variable. If the coefficient is not
# significantly different from zero, we should remove it from the model since
# it's not helping the prediction.

# Std. Error
# The standard error gives the measure of how much the coefficient is likely to vary from the estimate value

# t value
# The t-value is the estimate divided by the standard error. It's positive
# negative sign follows the estimate's sign. The larger the absolute value of
# the t-value, the more likely the coefficient is to be significant. So we want
# independent variables with a large absolute value in this column.

# Pr(>|t|)
# Gives the measure of how plausible it is that the coefficient is actually
# zero, given the data used to build the model. The smaller the probability
# number, the less likely it is that the coefficient is actually zero. The size of this number is inverse to the size of the t value. We want independent variables with small values in this column. 

# The star coding scheme is the easiest way to see if a variable is significant.
# Three stars is the highest level of significance and corresponds to the
# smallest possible probabilities. A period or dot means a variable is _almost_
# significant. Nothing at the end of a row means the variable is insignificant
# in the model. The number of stars can change for the same variable from model to model. 
# This is due to multicollinearity.


#-------------------------- Fit 1 ------------------------#
fit1<- lm(SalePrice ~ OverallQual, data = combi)
summary(fit1)

SSE = sum(fit1$residuals^2)
SSE

#-------------------------- Fit 2 ------------------------#
# OverallQual and GrLivArea both have ***, indicating these are very significant variables. 
fit2<- lm(SalePrice ~ OverallQual + GrLivArea, data = combi)
summary(fit2)

SSE = sum(fit2$residuals^2)
SSE

#-------------------------- Fit 3 ------------------------#
# Notice adding AllPorchSF yields Adjusted R-squared goes down while Multiple
# R-squared stays the same; indicates it is not a very important determining
# variable. We also see that AllPorchSF has a high Probability value and no Signif. 
# codes, indicating it is not a significant variable.

fit3<- lm(SalePrice ~ OverallQual + GrLivArea + AllPorchSF, data = combi)
summary(fit3)

SSE = sum(fit3$residuals^2)
SSE

#-------------------------- Fit 4 ------------------------#
# Try a linear model on two independent variables
cor(numeric_train)
fit4<- lm(train$TotRmsAbvGrd ~ train$BedroomAbvGr, data = combi)
summary(fit4)

SSE = sum(fit4$residuals^2)
SSE
str(train)

#-------------------------- Prediction ------------------------#

predictTest = predict(fit4, [newdata = train?])
predictTest

###################################### CART ######################################


#-------------------------------- Split Training Data ---------------------------#

# See Analytics Edge Unit 3, Modeling the Expert

library(caTools)

# Randomly split data
set.seed(88)
split = sample.split(train$SalePrice, SplitRatio = 0.75)
split

# Create training and testing sets (Q: Does this boolean split apply for non-logistic-regression?)
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)

#--------------------------------- rpart Model ---------------------------------#

SalesTree <- rpart(SalePrice ~ [TBD_variable + TBD_variable + TBD_variable])

# Step function:
# R provides a function, step, that will automate the procedure of trying different combinations of variables to find a good compromise of model simplicity and R2. This trade-off is formalized by the Akaike information criterion (AIC) - it can be informally thought of as the quality of the model with a penalty for the number of variables in the model.

# The step function has one argument - the name of the initial model. It returns
# a simplified model. Use the step function in R to derive a new model, with the
# full model as the initial model (HINT: If your initial full model was called
# "climateLM", you could create a new model with the step function by typing
# step(climateLM). Be sure to save your new model to a variable name so that you
# can look at the summary. For more information about the step function, type
# ?step in your R console.)

stepfit3 <- step(fit3)

##################################### SUBMIT #####################################


# build a dataframe with results
submit.house <- data.frame(Id = test$Id, SalePrice = [TBD])

# write results to .csv for submission
write.csv(submit.house, file="house_modelTBD.csv",row.names=FALSE)


#################################### RESOURCES ###################################

#https://nycdatascience.com/blog/student-works/machine-learning-project-house-prices-advanced-regression-techniques/ 
#http://web.cse.ohio-state.edu/~shi.876/slides/5523.pdf
#http://www.shihaiyang.me/2018/04/16/house-prices/ 
#https://nycdatascience.com/blog/student-works/machine-learning-project-house-prices-advanced-regression-techniques/
#https://towardsdatascience.com/predicting-housing-prices-using-advanced-regression-techniques-8dba539f9abe
#https://www.kaggle.com/juliencs/a-study-on-regression-applied-to-the-ames-dataset  
