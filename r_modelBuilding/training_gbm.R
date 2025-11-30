## Clear workspace
rm(list = ls())
set.seed(123)

team <- "Team 4"

## 1. Load all the data so we can quickly combine it and explore it. 

source("load_data.R")

rawTrain <- load_data("train")
rawTest  <- load_data("test")

cat("Train mortality rate =", round(mean(rawTrain$inhospital_mortality), 4), "\n")
cat("Test mortality rate  =", round(mean(rawTest$inhospital_mortality), 4), "\n")

## 2. Clean string-valued columns
fixStrings <- function(df) {
  for(nm in names(df)) {
    col <- df[[nm]]
    if(is.character(col)) {
      col <- trimws(col)
      badVals <- col %in% c("", "NA", "<NA>")
      if(any(badVals)) col[badVals] <- NA
      df[[nm]] <- col
    }
  }
  df
}

train <- fixStrings(rawTrain)
test  <- fixStrings(rawTest)

## 3. Identify numeric vs categorical
numCols <- names(train)[sapply(train, is.numeric)]
catCols <- names(train)[sapply(train, function(x) is.factor(x) || is.character(x))]
numCols <- numCols[numCols != "inhospital_mortality"]

## 4. Numeric median imputation
numMeds <- sapply(train[numCols], function(x) median(x, na.rm = TRUE))

for(v in numCols){
  if(any(is.na(train[[v]]))) train[[v]][is.na(train[[v]])] <- numMeds[[v]]
  if(any(is.na(test[[v]])))  test[[v]][is.na(test[[v]])]  <- numMeds[[v]]
}

## 5. Categorical mode imputation
mode_of <- function(x){
  x_no_na <- x[!is.na(x)]
  if(length(x_no_na) == 0) return(NA)
  names(sort(table(x_no_na), decreasing = TRUE))[1]
}

catModes <- sapply(train[catCols], mode_of)

for(v in catCols){
  if(is.factor(train[[v]])){
    train[[v]] <- droplevels(train[[v]])
    test[[v]]  <- factor(test[[v]], levels = levels(train[[v]]))
  } else {
    train[[v]] <- as.character(train[[v]])
    test[[v]]  <- as.character(test[[v]])
  }
  m <- catModes[[v]]
  train[[v]][is.na(train[[v]])] <- m
  test[[v]][is.na(test[[v]])]   <- m
  train[[v]] <- as.factor(train[[v]])
  test[[v]]  <- factor(test[[v]], levels = levels(train[[v]]))
}

## 5.5 Remove height/weight outliers by age bin (train only)
library(dplyr)

train$age_bin <- cut(
  train$agecalc_adm,
  breaks = seq(0, 72, by = 6),
  include.lowest = TRUE,
  right = FALSE
)

flag_outliers <- function(x){
  Q1  <- quantile(x, 0.25, na.rm = TRUE)
  Q3  <- quantile(x, 0.75, na.rm = TRUE)
  IQRv <- Q3 - Q1
  x < (Q1 - 1.5 * IQRv) | x > (Q3 + 1.5 * IQRv)
}

train_clean <- train %>%
  group_by(age_bin) %>%
  mutate(
    weight_outlier = flag_outliers(weight_kg_adm),
    height_outlier = flag_outliers(height_cm_adm)
  ) %>%
  ungroup()

cat("Original rows:", nrow(train), "\n")
train_clean <- train_clean %>% filter(!weight_outlier & !height_outlier)
cat("Cleaned rows:", nrow(train_clean), "\n")
cat("Rows removed:", nrow(train) - nrow(train_clean), "\n")

train_clean <- train_clean %>%
  select(-age_bin, -weight_outlier, -height_outlier)

train <- train_clean

## 6. Clinical engineered features
train$MAP <- (train$sysbp_mmhg_adm + 2 * train$diasbp_mmhg_adm) / 3
test$MAP  <- (test$sysbp_mmhg_adm  + 2 * test$diasbp_mmhg_adm)  / 3

train$pulse_pressure <- train$sysbp_mmhg_adm - train$diasbp_mmhg_adm
test$pulse_pressure  <- test$sysbp_mmhg_adm  - test$diasbp_mmhg_adm

train$SI <- train$hr_bpm_adm / pmax(train$sysbp_mmhg_adm, 1e-3)
test$SI  <- test$hr_bpm_adm  / pmax(test$sysbp_mmhg_adm,  1e-3)

train$resp_failure_flag <- as.integer(
  train$spo2site1_pc_oxi_adm < 92 |
    (!is.na(train$respdistress_adm) & train$respdistress_adm != "F")
)
test$resp_failure_flag <- as.integer(
  test$spo2site1_pc_oxi_adm < 92 |
    (!is.na(test$respdistress_adm) & test$respdistress_adm != "F")
)

coma_score_fun <- function(eye, motor, verbal){
  e <- ifelse(eye == "Watches or follows", 2L, 1L)
  m <- ifelse(motor == "Localizes painful stimulus", 2L, 1L)
  v <- ifelse(!is.na(verbal) & grepl("Cries appropriately|speaks", verbal), 2L, 1L)
  e + m + v
}

train$coma_score <- mapply(coma_score_fun, train$bcseye_adm, train$bcsmotor_adm, train$bcsverbal_adm)
test$coma_score  <- mapply(coma_score_fun, test$bcseye_adm, test$bcsmotor_adm, test$bcsverbal_adm)

train$coma_flag <- as.integer(train$coma_score <= 4)
test$coma_flag  <- as.integer(test$coma_score <= 4)

train$jaundice_flag <- as.integer(!is.na(train$symptoms_adm_jaundice) &
                                    train$symptoms_adm_jaundice != "F")
test$jaundice_flag  <- as.integer(!is.na(test$symptoms_adm_jaundice) &
                                    test$symptoms_adm_jaundice != "F")

train$fever_flag <- as.integer(train$temp_c_adm > 38 | train$temp_c_adm < 36)
test$fever_flag  <- as.integer(test$temp_c_adm  > 38 | test$temp_c_adm  < 36)

train$hypotension_flag <- as.integer(train$MAP < 60)
test$hypotension_flag  <- as.integer(test$MAP < 60)

## 7. Absolute z-scores using non-sepsis group
nonSep <- subset(train, inhospital_mortality == 0)

zVars <- c(
  "hr_bpm_adm",
  "rr_brpm_app_adm",
  "sysbp_mmhg_adm",
  "diasbp_mmhg_adm",
  "temp_c_adm",
  "muac_mm_adm"
)

zMeans <- list()
zStds  <- list()

for(v in zVars){
  mu  <- mean(nonSep[[v]], na.rm = TRUE)
  sdv <- sd(nonSep[[v]],  na.rm = TRUE)
  zMeans[[v]] <- mu
  zStds[[v]]  <- sdv
  
  if(is.na(sdv) || sdv < 1e-6){
    train[[paste0(v, "_z")]] <- 0
    test[[paste0(v, "_z")]]  <- 0
  } else {
    train[[paste0(v, "_z")]] <- abs(train[[v]] - mu) / sdv
    test[[paste0(v, "_z")]]  <- abs(test[[v]] - mu) / sdv
  }
}

## 8. One-hot encoding
y_train <- train$inhospital_mortality
if(is.factor(y_train)) y_train <- as.numeric(as.character(y_train))

x_train <- subset(train, select = -inhospital_mortality)
x_test  <- subset(test,  select = -inhospital_mortality)

trMat <- model.matrix(~ . - 1, data = x_train)
tsMat <- model.matrix(~ . - 1, data = x_test)

colnames(trMat) <- make.names(colnames(trMat), unique = TRUE)
colnames(tsMat) <- make.names(colnames(tsMat), unique = TRUE)

cat("Final number of encoded features:", ncol(trMat), "\n")

## 9. LightGBM model with CV
library(lightgbm)
library(pROC)

dTrain <- lgb.Dataset(trMat, label = y_train)

lgbParams <- list(
  objective        = "binary",
  metric           = "auc",
  learning_rate    = 0.03,
  num_leaves       = 20,
  max_depth        = 4,
  min_data_in_leaf = 40,
  feature_fraction = 0.8,
  bagging_fraction = 0.8,
  bagging_freq     = 1,
  lambda_l2        = 1,
  verbose          = -1
)

cvRes <- lgb.cv(
  params                = lgbParams,
  data                  = dTrain,
  nrounds               = 400,
  nfold                 = 5,
  stratified            = TRUE,
  early_stopping_rounds = 40,
  verbose               = -1
)

bestIter <- cvRes$best_iter
cat("Best CV iteration =", bestIter, "\n")

myTree <- lightgbm(
  data    = dTrain,
  label   = y_train,
  params  = lgbParams,
  nrounds = bestIter,
  verbose = -1
)

## 10. Predictions
train$prob <- predict(myTree, trMat)
test$prob  <- predict(myTree, tsMat)

## 11. Threshold selection
source(file.path("..", "scoring", "evaluate_performance.R"))

thrGrid <- seq(0.04, 0.12, by = 0.002)
bestThr <- thrGrid[1]
bestScore <- -9999

for(t in thrGrid){
  predNow <- as.numeric(train$prob >= t)
  if(length(unique(predNow)) < 2) next
  
  resNow <- evaluate_model(
    labels = y_train,
    prediction_probability = train$prob,
    threshold = t,
    dataset_label = "Train",
    inference_speed = 0
  )
  scr <- resNow$weighted_score
  
  if(!is.na(scr) && scr > bestScore){
    bestScore <- scr
    bestThr   <- t
  }
}

threshold <- bestThr
cat("Picked threshold:", round(threshold, 3),
    "| training weighted_score:", round(bestScore, 3), "\n")

## 11.5 Youden check
roc_tmp <- roc(train$inhospital_mortality, train$prob)
ydStuff <- coords(
  roc_tmp,
  "b",
  best.method = "youden",
  input = "threshold",
  transpose = TRUE,
  ret = c("threshold", "sensitivity", "specificity")
)
cat("Youden threshold:", round(ydStuff["threshold"], 3),
    "Sens:", round(ydStuff["sensitivity"], 3),
    "Spec:", round(ydStuff["specificity"], 3), "\n")

## 13. Final evaluation
resTbl <- NULL
resTbl <- rbind(
  resTbl,
  evaluate_model(train$inhospital_mortality, train$prob, threshold, "Training", 0)
)
resTbl <- rbind(
  resTbl,
  evaluate_model(test$inhospital_mortality,  test$prob,  threshold, "Testing",  0)
)
print(resTbl)

## 14. Save model + meta file
lgb.save(myTree, paste0(team, "_lightgbm.model"))
cat("Saved LightGBM model:", paste0(team, "_lightgbm.model"), "\n")

meta <- list(
  thresh = round(threshold, 3),
  zMeans = zMeans,
  zStds  = zStds
)

saveRDS(meta, paste0(team, "_model_meta.rds"))
cat("Saved meta file:", paste0(team, "_model_meta.rds"), "\n")

round(threshold, 3)
