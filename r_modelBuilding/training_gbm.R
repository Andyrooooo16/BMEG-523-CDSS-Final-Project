## Clear workspace
rm(list = ls())

## TeamName
team <- "Example"

## Load data helper
source("load_data.R")

# ---------------------------------------------------------------
# 1. Load raw training and test datasets
# ---------------------------------------------------------------
train_raw <- load_data("train")
test_raw  <- load_data("test")

cat("Train mortality rate =", round(mean(train_raw$inhospital_mortality), 4), "\n")
cat("Test mortality rate  =", round(mean(test_raw$inhospital_mortality), 4), "\n")


# ---------------------------------------------------------------
# 2. Clean string values BEFORE imputation
# ---------------------------------------------------------------
clean_strings <- function(df) {
  df[] <- lapply(df, function(col) {
    if (is.character(col)) {
      col <- trimws(col)
      col[col %in% c("NA", "<NA>", "")] <- NA
    }
    col
  })
  df
}

train <- clean_strings(train_raw)
test  <- clean_strings(test_raw)


# ---------------------------------------------------------------
# 3. Identify numeric, categorical, and logical variables
# ---------------------------------------------------------------
num_vars  <- names(train)[sapply(train, is.numeric)]
cat_vars  <- names(train)[sapply(train, function(x) is.character(x) || is.factor(x))]
logi_vars <- names(train)[sapply(train, is.logical)]


# ---------------------------------------------------------------
# 4. Numeric imputation using MICE (PMM)
# ---------------------------------------------------------------
library(mice)

mice_numeric <- mice(train[num_vars], method = "pmm", m = 1)
train[num_vars] <- complete(mice_numeric)

# Test set: impute numeric with training medians
for (v in num_vars) {
  med <- median(train[[v]], na.rm = TRUE)
  test[[v]][is.na(test[[v]])] <- med
}


# ---------------------------------------------------------------
# 5. Categorical + logical imputation using mode
# ---------------------------------------------------------------
mode_fill <- function(x) {
  ux <- na.omit(x)
  if (length(ux) == 0) return(x)
  mode_value <- names(sort(table(ux), decreasing = TRUE))[1]
  x[is.na(x)] <- mode_value
  x
}

train[cat_vars] <- lapply(train[cat_vars], mode_fill)
test[cat_vars]  <- lapply(test[cat_vars], mode_fill)

train[logi_vars] <- lapply(train[logi_vars], as.integer)
test[logi_vars]  <- lapply(test[logi_vars], as.integer)


# ---------------------------------------------------------------
# 6. Fit the LightGBM model
# ---------------------------------------------------------------
library(lightgbm)

train_matrix <- as.matrix(subset(train, select = -inhospital_mortality))
train_label  <- train$inhospital_mortality

myTree <- lightgbm(
  data      = train_matrix,
  label     = train_label,
  objective = "binary",
  nrounds   = 10
)

#Rank Top 20 Features based on Importance
importance <- lgb.importance(myTree, percentage = TRUE)
cat("\nTop 20 Most Important Features:\n")
print(head(importance, 20))

# Optional: plot
lgb.plot.importance(importance, top_n = 20)

# ---------------------------------------------------------------
# 7. Predictions and threshold selection
# ---------------------------------------------------------------
train$probGBM <- predict(myTree, as.matrix(subset(train, select = -inhospital_mortality)))
test$probGBM  <- predict(myTree, as.matrix(subset(test, select = -inhospital_mortality)))

library(pROC)

roc_train <- roc(inhospital_mortality ~ probGBM, data = train)
plot(roc_train, main = paste0("AUC = ", round(roc_train$auc, 3)))

# IMPORTANT FIX: ask coords() for a single numeric threshold only
best_thresh <- coords(
  roc_train,
  "b",
  best.method = "youden",
  ret = "threshold",
  transpose = FALSE
)

# Ensure pure numeric scalar
threshold <- as.numeric(best_thresh)

roc_test <- roc(inhospital_mortality ~ probGBM, data = test)
plot(roc_test, add = TRUE, col = "red")
text(0.3, 0.3, paste0("AUC_test = ", round(roc_test$auc, 3)), col = "red")


# ---------------------------------------------------------------
# 8. Prepare model for submission
# ---------------------------------------------------------------
myModel <- list()
myModel$thresh <- round(threshold, 3)
dput(myModel)

lgb.save(myTree, paste0(team, "_lightgbm.model"))
round(threshold, 3)


# ---------------------------------------------------------------
# 9. Evaluate using assignment scoring script
# ---------------------------------------------------------------
source(file.path("..", "scoring", "evaluate_performance.R"))

res <- NULL
res <- rbind(res, evaluate_model(train$inhospital_mortality, train$probGBM, threshold, "Training", 0))
res <- rbind(res, evaluate_model(test$inhospital_mortality,  test$probGBM,  threshold, "Testing",  0))

print(res)
