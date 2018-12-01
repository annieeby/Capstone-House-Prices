setwd('~/Documents/Programming Projects/Data Sets/house_all/')
train = read.csv("train.csv", stringsAsFactors = FALSE)
test = read.csv("test.csv", stringsAsFactors = FALSE)


combi = bind_rows(train %>% mutate(dataset = 'train'), 
                  test %>% mutate(dataset = 'test')) 
str(combi)

#---------------------------- Stack Overflow Solution Begin ------------------------#
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
