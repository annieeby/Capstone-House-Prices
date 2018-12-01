setwd('~/Documents/Programming Projects/Data Sets/house_all/')
train = read.csv("train.csv", stringsAsFactors = FALSE)
test = read.csv("test.csv", stringsAsFactors = FALSE)


combi = bind_rows(train %>% mutate(dataset = 'train'), 
                  test %>% mutate(dataset = 'test')) 
str(combi)

#---------------------------- Numericize Character Values ------------------------#

qual_cols <- c("ExterQual", "ExterCond", "BsmtQual", "BsmtCond", "HeatingQC", "KitchenQual", "FireplaceQu", "GarageQual", "GarageCond", "PoolQC")
quals <- c( "Po", "Fa", "TA", "Gd", "Ex")
c(0, 1, 2, 3, 4, 5)

bsmt_fin <- c("BsmtFinType1", "BsmtFinType2")
c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")
c(0, 1, 2, 3, 4, 5, 6)

"Functional"
c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ")
c(1, 2, 3, 4, 5, 6, 7, 8)

"LandSlope"
c("Sev", "Mod", "Gtl")
c(1, 2, 3)

"LotShape"
c("IR3", "IR2", "IR1", "Reg")
c(1, 2, 3, 4)

"PavedDrive"
c("N", "P", "Y")
c(1, 2, 3)

"BsmtExposure"
c("None", "Mn", "Av", "Gd")
c(0, 1, 2, 3)

"Alley"  
c("None", "Grvl", "Pave")
c(0, 1, 2)

#---------------------------- Single Variable Generation ------------------------#

c( "Po", "Fa", "TA", "Gd", "Ex")
c(0, 1, 2, 3, 4, 5)

# Create ExterGrade

set.seed(22)
ExterQual <- sample(grades, 2919, replace = TRUE)
ExterCond <- sample(grades, 2919, replace = TRUE)
combi$ExterGrade <- match(ExterQual, grades) * match(ExterCond, grades)
head(combi$ExterQual) # (4, 3, 4, 3, 4, 3)
head(combi$ExterCond) # (3, 3, 3, 3, 3, 3)
head(combi$ExterGrade) # expected: (12, 9, 12, 9, 12, 9)

# Create GarageGrade

GarageQual <- sample(grades, 2919, replace = TRUE)
GarageCond <- sample(grades, 2919, replace = TRUE)
combi$GarageGrade <- match(GarageQual, grades) * match(GarageCond, grades)
head(combi$GarageQual) # (3, 3, 3, 3, 3, 3)
head(combi$GarageCond) # (3, 3, 3, 3, 3, 3)
head(combi$GarageGrade) # expected: (9, 9, 9, 9, 9, 9)

#---------------------------- Multiple Variable Generation ------------------------#


qual_cols <- c("ExterQual", "ExterCond", "BsmtQual", "BsmtCond", "HeatingQC", "KitchenQual", "FireplaceQu", "GarageQual", "GarageCond", "PoolQC")

combi[qual_cols] <- lapply(combi %>% select(qual_cols), function(x) x = sample(grades, 20, replace = TRUE))
# or
combi[qual_cols] <- sapply(combi[qual_cols], match, grades)
# ?

#--------------------------- Manual Variable Multiplication -----------------------#
# Note: some of these, eg OverallGrade, TotalBath, AllFlrsSF, and AllPorchSF 
# work because they are products of originally numeric variables

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
head(combi$PoolScore)

# Simplified overall quality of the house
combi$SimplOverallGrade <- combi$SimplOverallQual * combi$SimplOverallCond
head(combi$SimplOverallGrade)

# Simplified overall quality of the exterior
combi$SimplExterGrade <- combi$SimplExterQual * combi$SimplExterCond
head(combi$SimplExterGrade)

# Simplified overall pool score
combi$SimplPoolScore <- combi$PoolArea * combi$SimplPoolQC
head(combi$SimplPoolScore)

# Simplified overall garage score
combi$SimplGarageScore <- combi$GarageArea * combi$SimplGarageQual
head(combi$SimplGarageScore)

# Simplified overall fireplace score
combi$SimplFireplaceScore <- combi$Fireplaces * combi$SimplFireplaceQu
head(combi$SimplFireplaceScore)

# Simplified overall kitchen score
combi$SimplKitchenScore <- combi$KitchenAbvGr * combi$SimplKitchenQual
head(combi$SimplKitchenScore)

# Total number of bathrooms
combi$TotalBath <- combi$BsmtFullBath + (0.5 * combi$BsmtHalfBath) + 
  combi$FullBath + (0.5 * combi$HalfBath)
head(combi$TotalBath)

# Total SF for house (incl. basement)
combi$AllSF <- combi$GrLivArea + combi$TotalBsmtSF
head(combi$AllSF)

# Total SF for 1st + 2nd floors
combi$AllFlrsSF <- combi$X1stFlrSF + combi$X2ndFlrSF
head(combi$AllFlrsSF)

# Total SF for porch
combi$AllPorchSF <- combi$OpenPorchSF + combi$EnclosedPorch + 
  combi$X3SsnPorch + combi$ScreenPorch
head(combi$AllPorchSF)