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


##
## @jodie we have to some some consolidation and cleaning for the columns with characters. I think we remove white space and also make sure any
## not appropriate/blank values are converted to NA
## @Andrew I put this in below that does that function, good enoguh?
##instead of MICE i think we clean the string columns by hand because they contain messy text
## like blank entries, “NA”, “<NA>”, etc. also mice isn’t meant for this type of cleanup.
# Converting these to real NA and filling with the most common value is safer
# and avoids creating fake symptom values
cleanupStrings <- function(df) {
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

# Clean up both the train and test set
train <- cleanupStrings(rawTrain)
test  <- cleanupStrings(rawTest)



## 3. Identify numeric vs categorical

##splitting data into numeric vs categorical and also removing the final outcome column so we don't bias the trianing rpcoess

numCols <- names(train)[sapply(train, is.numeric)]
catCols <- names(train)[sapply(train, function(x) is.factor(x) || is.character(x))]
numCols <- setdiff(numCols, "inhospital_mortality")




##@jodie do we replace the NAs with mean or median?? 
##@andrew median is probably the way to go because we still have outliers therefore i think median is more stable

## 4. Numeric median imputation
numMeds <- sapply(train[numCols], function(x) median(x, na.rm = TRUE))

# here we replace any NA values with the median values in both training and test
for(v in numCols){
  if(any(is.na(train[[v]]))) train[[v]][is.na(train[[v]])] <- numMeds[[v]]
  if(any(is.na(test[[v]])))  test[[v]][is.na(test[[v]])]  <- numMeds[[v]]
}


## @jodie i think for the categorical NA values, we should probably replace it with the most common outcome in that column? can you double check if what i did was right

## 5. Categorical mode imputation

# finding the most common value in a categorical column
mode_of <- function(x){
  x_no_na <- x[!is.na(x)]
  if(length(x_no_na) == 0) return(NA)
  names(sort(table(x_no_na), decreasing = TRUE))[1]
}

# find the common value in every column 
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




# when plotting the age vs height and age vs weight, there was clear outliers probalby from human imputation error
# we replaced those outliers with the age group mean because, after removing extremes, the mean gives a better estimate of typical growth than the median

## 5.5 Remove height/weight outliers by age bin (train only)
library(dplyr)

# use 6 month gaps because it has children physiologically similar, but large enough to ensure we still have multiple patients per bin for IQR calculations
train$age_bin <- cut(
  train$agecalc_adm,
  breaks = seq(0, 72, by = 6),
  include.lowest = TRUE,
  right = FALSE
)

outliers <- function(x){
  Q1  <- quantile(x, 0.25, na.rm = TRUE)
  Q3  <- quantile(x, 0.75, na.rm = TRUE)
  IQRv <- Q3 - Q1
  x < (Q1 - 1.5 * IQRv) | x > (Q3 + 1.5 * IQRv)
}

# need to create a copy so we can do our modifications
train_clean <- train

for (bin in unique(train$age_bin)) {
  
  # rows belonging to this bin
  rows <- which(train$age_bin == bin)
  
  weight <- train$weight_kg_adm[rows]
  height <- train$height_cm_adm[rows]
  
  # find outliers
  w_out <- outliers(weight)
  h_out <- outliers(height)
  
  # compute the mean but not including the outliers or else it will heavily distort our mean
  w_mean_clean <- mean(weight[!w_out], na.rm = TRUE)
  h_mean_clean <- mean(height[!h_out], na.rm = TRUE)
  
  # replace outliers with the mean
  train_clean$weight_kg_adm[rows][w_out] <- w_mean_clean
  train_clean$height_cm_adm[rows][h_out] <- h_mean_clean
}

# remove the age bin column since test set doesnt have it and it will mess up our algorithm later on
train_clean$age_bin <- NULL

#overwrite train
train <- train_clean


## 6. New Features addded

# Shock Index (this was already given from base code)
train$SI <- train$hr_bpm_adm / train$sysbp_mmhg_adm
test$SI  <- test$hr_bpm_adm  / test$sysbp_mmhg_adm


# Mean Arterial Pressure
train$MAP <- (train$sysbp_mmhg_adm + 2 * train$diasbp_mmhg_adm) / 3
test$MAP  <- (test$sysbp_mmhg_adm  + 2 * test$diasbp_mmhg_adm)  / 3

# Pulse Pressure
train$pulse_pressure <- train$sysbp_mmhg_adm - train$diasbp_mmhg_adm
test$pulse_pressure  <- test$sysbp_mmhg_adm  - test$diasbp_mmhg_adm


# @jodie for these z scores, should we use real life data for the mean and standard deviation for calculating z scores, or
# do we use the mean and standard deviation of the healthy patients in our dataset? i already found some data that we can use from online
# @andrew i think we can stick with your code

# Age Adjusted HR Z score (based on healthy children).the values are based on a source i found on healthy children
get_hr_z <- function(age, hr) {
  if (age < 1) {             
    mean <- 130; sd <- (160 - 100) / 4
  } else if (age < 12) {     
    mean <- 110; sd <- (140 - 80) / 4
  } else if (age < 36) {     
    mean <- 105; sd <- (130 - 80) / 4
  } else if (age < 60) {    
    mean <- 95;  sd <- (110 - 80) / 4
  } else if (age < 144) {    
    mean <- 85;  sd <- (100 - 70) / 4
  } else {                   
    mean <- 80;  sd <- (100 - 60) / 4
  }
  (hr - mean) / sd
}

train$hr_z <- mapply(get_hr_z, train$agecalc_adm, train$hr_bpm_adm)
test$hr_z  <- mapply(get_hr_z,  test$agecalc_adm,  test$hr_bpm_adm)


# Age Adjusted RR Z score (based on healthy patients).the values are based on a source i found on healthy children
get_rr_z <- function(age, rr) {
  
  if (age < 12) {                     
    mean <- 45;  sd <- (60 - 30) / 4  
    
  } else if (age < 36) {              
    mean <- 32;  sd <- (40 - 24) / 4  
    
  } else if (age < 72) {              
    mean <- 28;  sd <- (34 - 22) / 4  
    
  } else if (age < 144) {             
    mean <- 24;  sd <- (30 - 18) / 4 
    
  } else {                            
    mean <- 14;  sd <- (16 - 12) / 4  
  }
  
  (rr - mean) / sd
}

train$rr_z <- mapply(get_rr_z, train$agecalc_adm, train$rr_brpm_app_adm)
test$rr_z  <- mapply(get_rr_z,  test$agecalc_adm,  test$rr_brpm_app_adm)



# Respiratory Failure Flag
# mark a child as having respiratory failure if their oxygen saturation (SpO2) is below 92%, or if there is any documented respiratory distress
train$resp_failure_flag <- as.integer(
  train$spo2site1_pc_oxi_adm < 92 |
    (!is.na(train$respdistress_adm) & train$respdistress_adm != "F")
)
test$resp_failure_flag <- as.integer(
  test$spo2site1_pc_oxi_adm < 92 |
    (!is.na(test$respdistress_adm) & test$respdistress_adm != "F")
)


# Coma Score
# for each variable, give a score of 2 if they show normal response, give a score of 1 if the response is reduced
coma_score_fun <- function(eye, motor, verbal){
  e <- ifelse(eye == "Watches or follows", 2L, 1L)
  m <- ifelse(motor == "Localizes painful stimulus", 2L, 1L)
  v <- ifelse(!is.na(verbal) & grepl("Cries appropriately|speaks", verbal), 2L, 1L)
  
  #total score (range 3–6) where lower scores represent worse neurological status
  e + m + v
}
train$coma_score <- mapply(coma_score_fun, train$bcseye_adm, train$bcsmotor_adm, train$bcsverbal_adm)
test$coma_score  <- mapply(coma_score_fun, test$bcseye_adm, test$bcsmotor_adm, test$bcsverbal_adm)


#Coma Flag (flag if lower than 4)
train$coma_flag <- as.integer(train$coma_score <= 4)
test$coma_flag  <- as.integer(test$coma_score <= 4)


#Jaundice Flag
#jaundice is the yellowing of skin, associated with liver dysfunction
#if patient is reported as T, then we flag them
train$jaundice_flag <- as.integer(train$symptoms_adm_jaundice != "F")
test$jaundice_flag  <- as.integer(test$symptoms_adm_jaundice  != "F")


#Fever/ Abnormal temperature flag
train$fever_flag <- as.integer(train$temp_c_adm > 37.5 | train$temp_c_adm < 36.5)
test$fever_flag  <- as.integer(test$temp_c_adm  > 37.5 | test$temp_c_adm  < 36.5)


#Hypotension Flag
train$hypotension_flag <- as.integer(train$MAP < 60)
test$hypotension_flag  <- as.integer(test$MAP < 60)



## @andrew i found one hot coding to be good for light gbm cuz it essentially turns our categorical variables into numbers for our algorithm to use later

## 7. One-hot encoding
y_train <- train$inhospital_mortality
if(is.factor(y_train)) y_train <- as.numeric(as.character(y_train))

x_train <- subset(train, select = -inhospital_mortality)
x_test  <- subset(test,  select = -inhospital_mortality)

trMat <- model.matrix(~ . - 1, data = x_train)
tsMat <- model.matrix(~ . - 1, data = x_test)

colnames(trMat) <- make.names(colnames(trMat), unique = TRUE)
colnames(tsMat) <- make.names(colnames(tsMat), unique = TRUE)

cat("Final number of encoded features:", ncol(trMat), "\n")



## 8. LightGBM model with CV
library(lightgbm)
library(pROC)

#need to convert our training matrix to proper lgbm format
dTrain <- lgb.Dataset(trMat, label = y_train)


# @jodie TA said to try prevent overfitting, what parameters can we adjust to do that?
# @andrew these parameters i found online seem to affect the overfitting, lets try fixing these
lgbParams <- list(
  objective        = "binary",  
  metric           = "auc",
  learning_rate    = 0.03,   
  num_leaves       = 20,       
  max_depth        = 4,    
  min_data_in_leaf = 40,   
  feature_fraction = 0.7,    
  bagging_fraction = 0.7,
  bagging_freq     = 1,
  lambda_l2        = 1,
  verbose          = -1
)


#5 fold cross validation to find the best number of boosting rounds which lets model stop early if performance stops improving, 
#which helps prevent overfitting.
cvRes <- lgb.cv(
  params                = lgbParams,
  data                  = dTrain,
  nrounds               = 400,
  nfold                 = 5,
  stratified            = TRUE,
  early_stopping_rounds = 40,
  verbose               = -1
)

#looks at all rounds and finds where AUC was the greatest and saves it as best iteration
bestIter <- cvRes$best_iter
cat("Best CV iteration =", bestIter, "\n")


#train the model using the number of rounds that was found to be the most optimal in cross
myTree <- lightgbm(
  data    = dTrain,
  label   = y_train,
  params  = lgbParams,
  nrounds = bestIter,
  verbose = -1
)

## 9. Predictions
train$prob <- predict(myTree, trMat)
test$prob  <- predict(myTree, tsMat)


## 10. Threshold selection

#load the scoring function so we can test different cutoff values
source(file.path("..", "scoring", "evaluate_performance.R"))


#try a range of thresholds and keep the one that scores best
ths <- seq(0.04, 0.12, 0.002)
best_t <- NA
best_w <- -Inf

for(t in ths){
  
  #binary predictions at this threshold
  p <- as.numeric(train$prob >= t)
  
  #skip if model predicts only one class
  if(length(unique(p)) < 2) next
  
  #gets the score for this threshold
  out <- evaluate_model(
    labels = y_train,
    prediction_probability = train$prob,
    threshold = t,
    dataset_label = "Train",
    inference_speed = 0
  )
  
  w <- out$weighted_score
  
  #keep the threshold if it performs better
  if(!is.na(w) && w > best_w){
    best_w <- w
    best_t <- t
  }
}

threshold <- best_t
cat("Chosen threshold:", round(threshold, 3),
    "| training weighted_score:", round(best_w, 3), "\n")


## 10.1 aub Section Youden check
roc_tmp <- roc(train$inhospital_mortality, train$prob)
ydStuff <- coords(
  roc_tmp,
  "b",
  best.method = "youden",
  input = "threshold",
  transpose = TRUE,
  ret = c("threshold", "sensitivity", "specificity")
)

#print the youden threshold so we can compare it to our chosen threshold
cat("Youden threshold:", round(ydStuff["threshold"], 3),
    "Sens:", round(ydStuff["sensitivity"], 3),
    "Spec:", round(ydStuff["specificity"], 3), "\n")

## 11. Final evaluation
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

## FINALLY Save model + meta file
lgb.save(myTree, paste0(team, "_lightgbm.model"))
cat("Saved LightGBM model:", paste0(team, "_lightgbm.model"), "\n")

meta <- list(
  thresh = round(threshold, 3)
)

saveRDS(meta, paste0(team, "_model_meta.rds"))
cat("Saved meta file:", paste0(team, "_model_meta.rds"), "\n")


round(threshold, 3)
