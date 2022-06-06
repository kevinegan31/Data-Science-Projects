library(tidyverse)
library(data.table)
library(ggplot2)
library(randomForest)
library(corrplot)
library(knitr)
library(kableExtra)
library(easyGgplot2)
library(caret)
library(glmnet)
library(xgboost)
library(Metrics)

### Set Working Directory
setwd(paste("~/Documents/R Code/Personal_Projects/Kaggle Competitions",
            "/House Prices - Advanced Regression Techniques",
            "/Initial Data", sep = ""))
train <- read.csv("./train.csv", stringsAsFactors = FALSE)
test <- read.csv("./test.csv", stringsAsFactors = FALSE)

### Save the ID column so that we can drop it from merged dataset (combi)
train_ID <- train$Id
test_ID <- test$Id

### test doesn't have SalePrice column, so add it.
test$SalePrice <- NA
### Removing Outliers
train <- train[-which(train$GrLivArea > 4000 & train$SalePrice < 3e+05), ]
## Log transformation of the target variable
train$SalePrice <- log(train$SalePrice + 1)
### Combine train and test
combi <- rbind(train, test)
### Dropping Id as it is unnecessary for the prediction process.
combi <- combi[, -1]
#######################################################################
######################### Imput Missing Data ##########################
#######################################################################
## For some variables, fill NA with 'None'
for (x in c("Alley", "PoolQC", "MiscFeature", "Fence", "FireplaceQu", "GarageType", 
            "GarageFinish", "GarageQual", "GarageCond", "BsmtQual", "BsmtCond", 
            "BsmtExposure", "BsmtFinType1", "BsmtFinType2", "MasVnrType")) {
  combi[is.na(combi[, x]), x] = "None"
}

# Group by neighborhood and fill in missing value by the median
# LotFrontage of all the neighborhood
temp <- aggregate(LotFrontage ~ Neighborhood, data = combi, median)
temp2 <- c()
for (str in combi$Neighborhood[is.na(combi$LotFrontage)]) {
  temp2 <- c(temp2, which(temp$Neighborhood == str))
}
combi$LotFrontage[is.na(combi$LotFrontage)] = temp[temp2, 2]

## Replacing missing data with 0
for (col in c("GarageYrBlt", "GarageArea", "GarageCars", "BsmtFinSF1", 
              "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF", "BsmtFullBath", "BsmtHalfBath", 
              "MasVnrArea")) {
  combi[is.na(combi[, col]), col] <- 0
}

## Replace missing MSZoning values by 'RL'
combi$MSZoning[is.na(combi$MSZoning)] <- "RL"

## Remove Utilities as it has zero variance
combi <- combi[, -9]

## Replace missing Functional values with 'Typ'
combi$Functional[is.na(combi$Functional)] <- "Typ"

## Replace missing Electrical values with 'SBrkr'
combi$Electrical[is.na(combi$Electrical)] <- "SBrkr"

## Replace missing KitchenQual values by 'TA'
combi$KitchenQual[is.na(combi$KitchenQual)] <- "TA"

## Replace missing SaleType values by 'WD'
combi$SaleType[is.na(combi$SaleType)] <- "WD"

## Replace missing Exterior1st and Exterior2nd values by 'VinylSd'
combi$Exterior1st[is.na(combi$Exterior1st)] <- "VinylSd"
combi$Exterior2nd[is.na(combi$Exterior2nd)] <- "VinylSd"

combi$MSSubClass <- as.character(combi$MSSubClass)
combi$OverallCond <- as.character(combi$OverallCond)
combi$YrSold <- as.character(combi$YrSold)
combi$MoSold <- as.character(combi$MoSold)

cols <- c("FireplaceQu", "BsmtQual", "BsmtCond", "GarageQual", "GarageCond", 
          "ExterQual", "ExterCond", "HeatingQC", "PoolQC", "KitchenQual", "BsmtFinType1", 
          "BsmtFinType2", "Functional", "Fence", "BsmtExposure", "GarageFinish", 
          "LandSlope", "LotShape", "PavedDrive", "Street", "Alley", "CentralAir", 
          "MSSubClass", "OverallCond", "YrSold", "MoSold")

FireplaceQu <- c("None", "Po", "Fa", "TA", "Gd", "Ex")
BsmtQual <- c("None", "Po", "Fa", "TA", "Gd", "Ex")
BsmtCond <- c("None", "Po", "Fa", "TA", "Gd", "Ex")
GarageQual <- c("None", "Po", "Fa", "TA", "Gd", "Ex")
GarageCond <- c("None", "Po", "Fa", "TA", "Gd", "Ex")
ExterQual <- c("Po", "Fa", "TA", "Gd", "Ex")
ExterCond <- c("Po", "Fa", "TA", "Gd", "Ex")
HeatingQC <- c("Po", "Fa", "TA", "Gd", "Ex")
PoolQC <- c("None", "Fa", "TA", "Gd", "Ex")
KitchenQual <- c("Po", "Fa", "TA", "Gd", "Ex")
BsmtFinType1 <- c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")
BsmtFinType2 <- c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")
Functional <- c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ")
Fence <- c("None", "MnWw", "GdWo", "MnPrv", "GdPrv")
BsmtExposure <- c("None", "No", "Mn", "Av", "Gd")
GarageFinish <- c("None", "Unf", "RFn", "Fin")
LandSlope <- c("Sev", "Mod", "Gtl")
LotShape <- c("IR3", "IR2", "IR1", "Reg")
PavedDrive <- c("N", "P", "Y")
Street <- c("Pave", "Grvl")
Alley <- c("None", "Pave", "Grvl")
MSSubClass <- c("20", "30", "40", "45", "50", "60", "70", "75", "80", "85", 
                "90", "120", "150", "160", "180", "190")
OverallCond <- NA
MoSold <- NA
YrSold <- NA
CentralAir <- NA
levels <- list(FireplaceQu, BsmtQual, BsmtCond, GarageQual, GarageCond, 
              ExterQual, ExterCond, HeatingQC, PoolQC, KitchenQual, BsmtFinType1, 
              BsmtFinType2, Functional, Fence, BsmtExposure, GarageFinish, LandSlope, 
              LotShape, PavedDrive, Street, Alley, CentralAir, MSSubClass, OverallCond, 
              YrSold, MoSold)
i = 1
for (c in cols) {
  if (c == "CentralAir" | c == "OverallCond" | c == "YrSold" | c == "MoSold") {
    combi[, c] <- as.numeric(factor(combi[, c]))
  } else combi[, c] <- as.numeric(factor(combi[, c], levels = levels[[i]]))
  i <- i + 1
}
### Adding feature
combi$TotalSF <- combi$TotalBsmtSF + combi$X1stFlrSF + combi$X2ndFlrSF
### Getting Dummy Variables
feature_classes <- sapply(names(combi), function(x) {
  class(combi[[x]])
})
numeric_feats <- names(feature_classes[feature_classes != "character"])

### get names of categorical features
categorical_feats <- names(feature_classes[feature_classes == "character"])

### use caret dummyVars function for hot one encoding for categorical
### features
library(caret)
dummies <- dummyVars(~., combi[categorical_feats])
categorical_1_hot <- predict(dummies, combi[categorical_feats])
### Determine skew for each numeric feature
library(moments)
library(MASS)
skewed_feats <- sapply(numeric_feats, function(x) {
  skewness(combi[[x]], na.rm = TRUE)
})

### Keep only features that exceed a threshold (0.75) for skewness
skewed_feats <- skewed_feats[abs(skewed_feats) > 0.75]

### Transform skewed features with boxcox transformation
for (x in names(skewed_feats)) {
  bc <- BoxCoxTrans(combi[[x]], lambda = 0.15)
  combi[[x]] <- predict(bc, combi[[x]])
  # combi[[x]] <- log(combi[[x]] + 1)
}
### Reconstruct all data with pre-processed data.
combi <- cbind(combi[numeric_feats], categorical_1_hot)
## Let us look at the dimensions of combi.
dim(combi)
### Splitting training set further
training <- combi[1:1458, ]
testing <- combi[1459:2917, ]
set.seed(222)
inTrain <- createDataPartition(y = training$SalePrice, p = 0.7, list = FALSE)
Training <- training[inTrain, ]
Validation <- training[-inTrain, ]

set.seed(123)
### Do not need to center y because
### log transformation gives it a normal distribution.
x <- as.matrix(Training[, -59])
y <- Training[, 59] %>%
  as.matrix()
ols <- lm(y ~ x)
# The intercept estimate should be dropped.
weights <- as.numeric(coef(ols))[-1]
weights[is.na(weights)] <- 0
ridge <- cv.glmnet(x, y, alpha = 0)
## The intercept estimate should be dropped.
weights <- as.numeric(coef(ridge))[-1]
# alpha=1, lasso
alasso <- cv.glmnet(x, y, alpha = 1,
                    penalty.factor = 1 / abs(weights))

coef <- as.vector(coef(alasso, s = alasso$lambda.min))[-1]
coef_nonzero <- coef != 0
ls.obj <- lm(y ~ x[, coef_nonzero, drop = FALSE])
ls_coef <- (ls.obj$coefficients)[-1]
coef[coef_nonzero] <- ls_coef

preds <- predict(alasso, newx = as.matrix(Validation[, -59]))
rmse(Validation$SalePrice, preds)
## Predictions
preds <- data.frame(exp(predict(alasso,newx = as.matrix(testing[, -59]),
                             s = "lambda.min")) - 1)
x <- data.frame(Id = test$Id, SalePrice = preds)
colnames(x)[2] <- "SalePrice"
write.csv(x, row.names = F, file = "./submission_alasso.csv")
##############################################################################
######################### Kaggle Model #######################################
##############################################################################

set.seed(123)
cv_lasso <- cv.glmnet(as.matrix(Training[,-59]), Training[,59])
preds <- predict(cv_lasso,
                 newx = as.matrix(Validation[, -59]),
                 s = "lambda.min")
rmse(Validation$SalePrice, preds)
### GBM
library(iterators)
library(parallel)
library(doMC)
set.seed(222)
## detectCores() returns 16 cpus
registerDoMC(16)
## Set up caret model training parameters
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",number=5,repeats=5,
                                 verboseIter=FALSE,allowParallel=TRUE)
gbmFit <- train(SalePrice~.,method="gbm",metric="RMSE",maximize=FALSE,
               trControl=CARET.TRAIN.CTRL,
               tuneGrid=expand.grid(n.trees=(4:10)*50,interaction.depth=c(5),shrinkage=c(0.05),
                                    n.minobsinnode=c(10)),data=Training,verbose=FALSE)
preds1 <- predict(gbmFit,newdata=Validation)
rmse(Validation$SalePrice, preds1)
### xGBoost
library(xgboost)
set.seed(123)
## Model parameters trained using xgb.cv function
xgbFit <- xgboost(data=as.matrix(Training[,-59]),
                  nfold=5,label=as.matrix(Training$SalePrice),
                  nrounds=2200,verbose=FALSE,objective='reg:linear',
                  eval_metric='rmse',nthread=8,eta=0.01,gamma=0.0468,
                  max_depth=6,min_child_weight=1.7817,subsample=0.5213,
                  colsample_bytree=0.4603)
##print(xgbFit)
## Predictions
preds2 <- predict(xgbFit,newdata=as.matrix(Validation[,-59]))
rmse(Validation$SalePrice,preds2)
### RMSE score for Simple Average of the three models
rmse(Validation$SalePrice,(preds+preds1+preds2)/3)




