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

# convert logicals to integers (0/1) so LightGBM can consume them directly
train[logi_vars] <- lapply(train[logi_vars], as.integer)
test[logi_vars]  <- lapply(test[logi_vars], as.integer)


# ---------------------------------------------------------------
# 5.5 Feature Creation (engineered, SIRS-like, SOFA-lite)
# ---------------------------------------------------------------

## 5.5.1 Age-adjusted HR z score (based on healthy pediatric ranges)
get_hr_z <- function(age, hr) {
  if (age < 1) {             # 0 - 1 month
    mean <- 130; sd <- (160 - 100) / 4
  } else if (age < 12) {     # 1 - 12 months
    mean <- 110; sd <- (140 - 80) / 4
  } else if (age < 36) {     # 1 - 3 years
    mean <- 105; sd <- (130 - 80) / 4
  } else if (age < 60) {     # 3 - 5 years
    mean <- 95;  sd <- (110 - 80) / 4
  } else if (age < 144) {    # 6 - 12 years
    mean <- 85;  sd <- (100 - 70) / 4
  } else {                   # adolescents
    mean <- 80;  sd <- (100 - 60) / 4
  }
  (hr - mean) / sd
}

train$hr_z <- mapply(get_hr_z, train$agecalc_adm, train$hr_bpm_adm)
test$hr_z  <- mapply(get_hr_z,  test$agecalc_adm,  test$hr_bpm_adm)


## 5.5.2 Basic haemodynamic features
# Pulse pressure
train$pulse_pressure <- train$sysbp_mmhg_adm - train$diasbp_mmhg_adm
test$pulse_pressure  <- test$sysbp_mmhg_adm  - test$diasbp_mmhg_adm

# Mean arterial pressure (MAP)
train$MAP <- (train$sysbp_mmhg_adm + 2 * train$diasbp_mmhg_adm) / 3
test$MAP  <- (test$sysbp_mmhg_adm  + 2 * test$diasbp_mmhg_adm)  / 3

# Shock index (HR / SBP)
train$SI <- train$hr_bpm_adm / train$sysbp_mmhg_adm
test$SI  <- test$hr_bpm_adm  / test$sysbp_mmhg_adm

# Modified shock index (HR / MAP)
train$modified_shock_index <- train$hr_bpm_adm / train$MAP
test$modified_shock_index  <- test$hr_bpm_adm  / test$MAP


## 5.5.3 SIRS-like features (vitals-only version)

# Temperature flag (fever or hypothermia)
train$sirs_temp_flag <- with(train, as.integer(temp_c_adm > 38 | temp_c_adm < 36))
test$sirs_temp_flag  <- with(test,  as.integer(temp_c_adm > 38 | temp_c_adm < 36))

# Heart rate flag (simple tachycardia threshold)
train$sirs_hr_flag <- with(train, as.integer(hr_bpm_adm > 90))
test$sirs_hr_flag  <- with(test,  as.integer(hr_bpm_adm > 90))

# Respiratory rate flag (tachypnea)
train$sirs_rr_flag <- with(train, as.integer(rr_brpm_app_adm > 20))
test$sirs_rr_flag  <- with(test,  as.integer(rr_brpm_app_adm > 20))

# SIRS-like total score (0–3) and positive indicator (>=2)
train$sirs_score    <- train$sirs_temp_flag + train$sirs_hr_flag + train$sirs_rr_flag
test$sirs_score     <- test$sirs_temp_flag  + test$sirs_hr_flag  + test$sirs_rr_flag

train$sirs_positive <- as.integer(train$sirs_score >= 2)
test$sirs_positive  <- as.integer(test$sirs_score  >= 2)


## 5.5.4 Respiratory SOFA-like proxies

# Respiratory failure flag: low SpO2 or documented respiratory distress
train$resp_failure_flag <- with(
  train,
  as.integer(spo2site1_pc_oxi_adm < 92 | respdistress_adm > 0)
)
test$resp_failure_flag <- with(
  test,
  as.integer(spo2site1_pc_oxi_adm < 92 | respdistress_adm > 0)
)

# Respiratory severity score (0–2): RR high + low SpO2
train$resp_severity_score <- with(
  train,
  as.integer(rr_brpm_app_adm >= 22) + as.integer(spo2site1_pc_oxi_adm < 92)
)
test$resp_severity_score <- with(
  test,
  as.integer(rr_brpm_app_adm >= 22) + as.integer(spo2site1_pc_oxi_adm < 92)
)


## 5.5.5 Cardiovascular SOFA-like proxies

# Hypotension flag
train$hypotension_flag <- as.integer(train$MAP < 65)
test$hypotension_flag  <- as.integer(test$MAP  < 65)

# Cardiovascular failure flag: more severe hypotension or marked shock index
train$cv_failure_flag <- as.integer(train$MAP < 60 | train$modified_shock_index > 2)
test$cv_failure_flag  <- as.integer(test$MAP  < 60 | test$modified_shock_index  > 2)


## 5.5.6 Renal proxies (symptom-based)

# Oliguria and abnormal urine color are stored as 0/1 after logi -> integer conversion
train$renal_flag_oliguria   <- as.integer(train$symptoms_adm_oliguria   > 0)
test$renal_flag_oliguria    <- as.integer(test$symptoms_adm_oliguria    > 0)

train$renal_flag_urinecolor <- as.integer(train$symptoms_adm_urinecolor > 0)
test$renal_flag_urinecolor  <- as.integer(test$symptoms_adm_urinecolor  > 0)

# Simple renal symptom score (0–2)
train$renal_score <- train$renal_flag_oliguria + train$renal_flag_urinecolor
test$renal_score  <- test$renal_flag_oliguria  + test$renal_flag_urinecolor

# Kidney performance ratio: 1 = no symptoms, 0 = both present
train$kidney_performance_ratio <- 1 - pmin(train$renal_score, 2) / 2
test$kidney_performance_ratio  <- 1 - pmin(test$renal_score,  2) / 2

# Any renal involvement flag
train$renal_flag <- as.integer(train$renal_score > 0)
test$renal_flag  <- as.integer(test$renal_score  > 0)


## 5.5.7 CNS proxy using Blantyre-like coma scale information

coma_score_fun <- function(eye, motor, verbal) {
  # map best-observed states to higher scores, everything else lower
  e <- ifelse(!is.na(eye)   & eye   == "Watches or follows", 2L, 1L)
  m <- ifelse(!is.na(motor) & motor == "Localizes painful stimulus", 2L, 1L)
  # verbal: look for "Cries appropriately" or "speaks" anywhere in the string
  v <- ifelse(!is.na(verbal) & grepl("Cries appropriately|speaks", verbal), 2L, 1L)
  e + m + v
}

train$coma_score <- mapply(
  coma_score_fun,
  train$bcseye_adm,
  train$bcsmotor_adm,
  train$bcsverbal_adm
)

test$coma_score <- mapply(
  coma_score_fun,
  test$bcseye_adm,
  test$bcsmotor_adm,
  test$bcsverbal_adm
)

# CNS impairment flag: lower scores imply worse neurologic status
train$coma_flag <- as.integer(train$coma_score <= 4)
test$coma_flag  <- as.integer(test$coma_score  <= 4)


## 5.5.8 Liver / systemic flags

# Jaundice symptom as crude liver dysfunction proxy
train$jaundice_flag <- as.integer(train$symptoms_adm_jaundice > 0)
test$jaundice_flag  <- as.integer(test$symptoms_adm_jaundice  > 0)


## 5.5.9 Combined SOFA-lite score (organ dysfunction summary)

train$sofa_lite_score <- with(
  train,
  resp_failure_flag +
    cv_failure_flag +
    renal_flag +
    coma_flag +
    jaundice_flag
)

test$sofa_lite_score <- with(
  test,
  resp_failure_flag +
    cv_failure_flag +
    renal_flag +
    coma_flag +
    jaundice_flag
)


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

# Rank Top 20 Features based on Importance
importance <- lgb.importance(myTree, percentage = TRUE)
cat("\nTop 20 Most Important Features:\n")
print(head(importance, 20))

# Optional: plot importance
lgb.plot.importance(importance, top_n = 20)


# ---------------------------------------------------------------
# 7. Predictions and threshold selection
# ---------------------------------------------------------------
train$probGBM <- predict(myTree, as.matrix(subset(train, select = -inhospital_mortality)))
test$probGBM  <- predict(myTree, as.matrix(subset(test, select = -inhospital_mortality)))

library(pROC)

roc_train <- roc(inhospital_mortality ~ probGBM, data = train)
plot(roc_train, main = paste0("AUC = ", round(roc_train$auc, 3)))

# Get a single numeric threshold (Youden)
best_thresh <- coords(
  roc_train,
  "b",
  best.method = "youden",
  ret = "threshold",
  transpose = FALSE
)

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
