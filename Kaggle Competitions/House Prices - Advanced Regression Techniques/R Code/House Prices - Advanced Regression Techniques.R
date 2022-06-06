# Original code comes from https://rstudio-pubs-static.s3.amazonaws.com/358187_2157f6fed1e243d998c5cae2af2eca55.html
# However, I have changed non-tidy methods to tidy and will update regression methods.
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
#######################################################################
#########################Import Data###################################
#######################################################################
# File download
# if (!file.exists(paste("~/Documents/R Code/Kaggle Competitions",
#                        "/House Prices - Advanced Regression Techniques",
#                        "/Initial Data", sep = ""))) {
#   
#   dir.create(paste("~/Documents/R Code/Kaggle Competitions",
#                    "/House Prices - Advanced Regression Techniques",
#                    "/Initial Data", sep = ""))
#   
#   download.file(paste("https://www.kaggle.com",
#                       "/c/house-prices-advanced-regression-techniques",
#                       "/download/test.csv", sep = ""),
#                 paste("./Documents/R Code/Kaggle Competitions",
#                       "/House Prices - Advanced Regression Techniques",
#                       "/Initial Data/test.csv", sep = ""))
#   download.file(paste("https://www.kaggle.com",
#                       "/c/house-prices-advanced-regression-techniques",
#                       "/download/train.csv", sep = ""),
#                 paste("./Documents/R Code/Kaggle Competitions",
#                       "/House Prices - Advanced Regression Techniques",
#                       "/Initial Data/train.csv", sep = ""))
# }

# Set Working Directory
setwd(paste("~/Documents/R Code/Kaggle Competitions",
            "/House Prices - Advanced Regression Techniques",
            "/Initial Data", sep = ""))
# Data import
raw_train <- read_csv("train.csv")
raw_test <- read_csv("test.csv")

# Bind and label both train and test sets
full_dt <- bind_rows(dplyr::select(raw_train, 
                                   -one_of("SalePrice")),
                     raw_test)
full_dt <- full_dt %>%
  add_column(Set = c(rep("Train", times = dim(raw_train)[1]),
                     rep("Test", times = dim(raw_test)[1])))
# Rpubs method
fulldt_original <- rbind(raw_train[, -81], raw_test)
fulldt_original <- cbind(fulldt_original, Set = c(rep("Train",
                                                      times = dim(raw_train)[1]),
                                                  rep("Test",
                                                      times = dim(raw_test)[1])))
#######################################################################
#########################Clean Data####################################
#######################################################################
# Check for missing values
x_NA <- full_dt %>%
  is.na() %>%
  colSums()
# Set table
x_NA_tbl <- tibble(Variables = names(x_NA),
                NA_count = as.vector(x_NA))
# From RPubs
x2 <- data.frame(Variables = names(x_NA), NA.Count = x_NA); rownames(x_NA) <- c()
# Remove variables that don't have missing values
x_NA_tbl <- x_NA_tbl %>%
  filter(NA_count > 0)
# View variables with NAs
kable(x_NA_tbl, "html") %>%
  kable_styling(full_width = F)
# Let's replace these NA values with 0
# We do this because we assume the NA values mean they don't exist
zero_variables <- c("LotFrontage", "MasVnrArea", "BsmtFinSF2", "BsmtUnfSF",
                    "TotalBsmtSF", "BsmtFullBath", "BsmtHalfBath")
# Replace missing values of factor variables with "None"
# These are variables that are not likely to have anything in that category
factor_variables <- c("Alley", "BsmtQual", "BsmtExposure", "BsmtFinType1",
                      "BsmtFinType2", "FireplaceQu", "PoolQC", "Fence",
                      "MiscFeature", "GarageType", "GarageFinish", "GarageQual",
                      "GarageCond", "BsmtCond")
# Replace NA with most common value.
# We do this to these variables because they are more likely to actually be missing
# rather than not have any value at all.
mode_variables <- c("MSZoning", "Utilities", "Exterior1st", "Exterior2nd",
                    "MasVnrType", "Electrical", "KitchenQual", "Functional", "SaleType")
# Replace NA with median value
# We do this because it is similar to the mode variables and median or mean are often good to use
# for numerical variables
median_variables <- c("GarageCars", "GarageArea", "BsmtFinSF1")
full_dt <- full_dt %>%
  mutate(
    across(all_of(zero_variables), ~ replace_na(.x, 0))
  ) %>%
  mutate(
    across(all_of(factor_variables), ~ replace_na(.x, "None"))
  ) %>%
  mutate(
    across(all_of(mode_variables), ~ replace_na(.x, names(which.max(table(.x)))))
  ) %>%
  mutate(
    across(all_of(mode_variables), ~ replace_na(.x, median(.x, na.rm = T)))
  )
# Garage year build is special. If there is a missing value here, we assume that it was built
# the same year as the house.
full_dt <- full_dt %>%
  mutate(GarageYrBlt = coalesce(GarageYrBlt, YearBuilt))
##############################################################################
#########################Feature Engineering##################################
##############################################################################
# Create a "total area" feature by adding the basement area and ground living area
full_dt <- full_dt %>%
  mutate(TotalArea = GrLivArea + TotalBsmtSF)
# Create a "total number of baths" feature by adding all bathroom features
full_dt <- full_dt %>%
  mutate(TotalBaths = BsmtFullBath + BsmtHalfBath + FullBath + HalfBath)
# Create a "area aboveground" feature by adding the areas of the first and second floor
full_dt <- full_dt %>%
  mutate(AreaAbvground = `1stFlrSF` + `2ndFlrSF`)
##############################################################################
#########################Feature Selection####################################
##############################################################################
# Numerical Variables
# Subset numerical variables that are from the "train" set
full_dt_num_train <- full_dt %>%
  filter(Set == "Train") %>%
  select(which(sapply(., is.integer)), which(sapply(., is.numeric))) %>%
  #Add the "SalePrice" variable
  mutate(SalePrice = raw_train$SalePrice)
# Develop Correlation Plot of numeric variables
correlation <- round(cor(full_dt_num_train), 2)
corrplot(correlation, method = "circle")
# Are the new features performing any better than the original ones?
# Set a table with "SalePrice" correlation
feature_check <- tibble(Variables = rownames(correlation), 
                        Cor = correlation[, "SalePrice"])

# Order it by correlation
feature_check <- feature_check %>%
  arrange(desc(Cor))
# Pick only values that have strong positive and negative correlation
feature_check_new <- feature_check %>%
  filter(Cor > 0.5 | Cor < -0.5)

rownames(feature_check_new) <- c()

kable(feature_check_new, "html") %>%
  kable_styling(full_width = F)
##############################################################################
#########################Factor Variables#####################################
##############################################################################
# Subset numerical variables that are from the "test" set
full_dt_fac_train <- full_dt %>%
  filter(Set == "Train") %>%
  select(Id, which(sapply(., is.character))) %>%
  # Add SalePrice variable
  mutate(SalePrice = raw_train$SalePrice)
# Test set
full_dt_fac_test <- full_dt %>%
  filter(Set == "Test") %>%
  select(Id, which(sapply(., is.character)))
# Run RF algorithm will all factor variables
rf <- randomForest(SalePrice ~ ., data = full_dt_fac_train, importance = T)
# Create Table with importance values
importance_table <- tibble(Names = rownames(importance(rf)),
                           '%IncMSE' = importance(rf)[, 1])
# Order table
importance_table <- importance_table %>%
  arrange(desc(importance_table[, 2]))
rownames(importance_table) <- c()  
# Subset first 10 values
kable(importance_table[1:10, ], "html") %>%
  kable_styling(full_width = F)
##############################################################################
######################### EDA Plotting #######################################
##############################################################################
density <- ggplot(full_dt_fac_train, aes(x = SalePrice)) + 
  geom_density()
qqnorm(full_dt_fac_train$SalePrice, pch = 1, frame = FALSE)

area_price <- ggplot(full_dt_num_train, aes(x = TotalArea, y = SalePrice)) +
  geom_point() +
  geom_smooth(method = lm)
# Log transformation to fix right skewness, normality, and heteroscedasticity
full_dt_num_train$SalePrice <- log(full_dt_num_train$SalePrice)
full_dt_fac_train$SalePrice <- log(full_dt_fac_train$SalePrice)
# Subset the train rows and selected features
dt_train <- full_dt %>%
  filter(Set == "Train") %>%
  dplyr::select("OverallQual", "TotalArea", "AreaAbvground", "GrLivArea",
                "Neighborhood", "ExterQual", "GarageType", "BldgType") %>%
  mutate(SalePrice = log(raw_train$SalePrice)) 
# Same for the test features
dt_test <- full_dt %>%
  filter(Set == "Test") %>%
  dplyr::select("OverallQual", "TotalArea", "AreaAbvground", "GrLivArea",
                "Neighborhood", "ExterQual", "GarageType", "BldgType")
# Random Forest model
fit <- randomForest(SalePrice ~ ., data = dt_train, importance = T)
fit_importance_table <- tibble(Names = rownames(importance(fit)),
                               '%IncMSE' = importance(fit)[, 1])
# Order table
fit_importance_table <- fit_importance_table %>%
  arrange(desc(fit_importance_table[, 2]))
rownames(fit_importance_table) <- c()  
# Use new model to predict SalePrice values from the test set
pred <- exp(predict(fit, newdata = dt_test))
# Random Forest Submission
write.csv(x = data.frame(Id = raw_test$Id, SalePrice = pred), row.names = F, file = "./submission2.csv")

glm_test <- glm(SalePrice ~ OverallQual + TotalArea + factor(Neighborhood),
                data = dt_train)
pred2 <- exp(predict(glm_test, newdata = dt_test))


# glm with Overall quality, Total Area, Neighborhood as factor, GarageType as factor
write.csv(x = data.frame(Id = raw_test$Id, SalePrice = pred2), row.names = F, file = "./submission.csv")
# glm with Overall quality, Total Area, Neighborhood as factor
write.csv(x = data.frame(Id = raw_test$Id, SalePrice = pred2), row.names = F, file = "./submission4.csv")
##############################################################################
#########################Gradient Boosting####################################
##############################################################################
# Create custom summary function in proper format for caret
custom_summary <- function(data, lev = NULL, model = NULL){
  out = rmsle(data[, "obs"], data[, "pred"])
  names(out) = c("rmsle")
  out
}

# Create control object
control <- trainControl(method = "cv",  # Use cross validation
                       number = 5,     # 5-folds
                       summaryFunction = custom_summary)

# Create grid of tuning parameters
grid <- expand.grid(nrounds = c(100, 200, 400, 800), # Test 4 values for boosting rounds
                   max_depth = c(4, 6),           # Test 2 values for tree depth
                   eta = c(0.1, 0.05, 0.025),      # Test 3 values for learning rate
                   gamma = c(0.1), 
                   colsample_bytree = c(1), 
                   min_child_weight = c(1),
                   subsample = c(1))

xgb_tree_model <-  train(SalePrice ~ .,      # Predict SalePrice using all features
                        data = dt_train,
                        method = "xgbTree",
                        trControl = control, 
                        tuneGrid = grid, 
                        metric = "rmsle",     # Use custom performance metric
                        maximize = FALSE)   # Minimize the metric

prediction <- exp(predict(xgb_tree_model, dt_test))
colnames(prediction) <- "prediction"
write.csv(x = data.frame(Id = raw_test$Id, SalePrice = prediction), row.names = F, file = "./submission_xgb.csv")


x <- full_dt_num_train %>%
  select(-SalePrice) %>%
  as.matrix()
y <- full_dt_num_train %>%
  select(SalePrice) %>%
  scale(center = TRUE, scale = FALSE) %>%
  as.matrix()

ols <- lm(y ~ x)
# The intercept estimate should be dropped.
weights <- as.numeric(coef(ols))[-1]
weights[is.na(weights)] <- 0
alasso <- cv.glmnet(x, y, alpha = 1,
                    penalty.factor = 1 / abs(weights))

coef <- as.vector(coef(alasso, s = alasso$lambda.min,
                       exact = T, x = x, y = y,
                       penalty.factor = 1 / abs(weights)))[-1]
coef_nonzero <- coef != 0
if (sum(coef_nonzero) > 0) {
  ls.obj <- lm(y ~ x[, coef_nonzero, drop = FALSE])
  ls_coef <- (ls.obj$coefficients)[-1]
  coef[coef_nonzero] <- ls_coef
} 




