## Clear workspace
rm(list = ls()); set.seed(123)

team <- "Team 4"
library(dplyr); library(lightgbm); library(pROC)

## Load all the data so we can quickly combine it and explore it. 
source("load_data.R")
rawTrain <- load_data("train"); rawTest <- load_data("test")
cat("Train mortality rate =", round(mean(rawTrain$inhospital_mortality), 4), "\n")
cat("Test mortality rate  =", round(mean(rawTest$inhospital_mortality), 4), "\n")

## Do some data cleaning, e.g. imputation for missing values
## @jodie Matthias code previously used mice package but I can't get it to work with our new engineered features
## I think basic imputation is easier to put together: https://apxml.com/courses/intro-data-cleaning-preprocessing/chapter-2-handling-missing-data/basic-imputation-mean-median-mode
## @jodie we have to do  some some consolidation and cleaning for the columns with characters. I think we remove white space and also make sure any
## not appropriate/blank values are converted to NA
## @Andrew I put this in below that does that function, good enoguh?

## 1. Clean string-valued columns
cleanupStrings <- function(df){
  for(nm in names(df)){
    col <- df[[nm]]
    if(is.character(col)){
      col <- trimws(col); badVals <- col %in% c("", "NA", "<NA>")
      if(any(badVals)) col[badVals] <- NA
      df[[nm]] <- col
    }
  }
  df
}

train <- cleanupStrings(rawTrain); test <- cleanupStrings(rawTest)
#wait do we need to add a line here so it works with the evaluation code?
#@Andrew I don't think so because our model is being used for evaluation not the training gbm code. 

## 3. Identify numeric vs categorical
##splitting data into numeric vs categorical and also removing the final outcome column so we don't bias the trianing rpcoess
numCols <- names(train)[sapply(train, is.numeric)]
catCols <- names(train)[sapply(train, function(x) is.factor(x) || is.character(x))]
numCols <- setdiff(numCols, "inhospital_mortality")

## 4. Numeric median imputation
## @jodie After graphing data points, mean probably isn't best choice here because some of the data have long tails. 
## Probably safer to compute the median for every numeric column instead, will also replace any NA values with the median values in both training and test sets. 
numcolmedian <- sapply(train[numCols], function(x) median(x, na.rm = TRUE))
for(v in numCols){
  if(any(is.na(train[[v]]))) train[[v]][is.na(train[[v]])] <- numcolmedian[[v]]
  if(any(is.na(test[[v]])))  test[[v]][is.na(test[[v]])]  <- numcolmedian[[v]]
}

## 5. Categorical mode imputation
# finding the most common value in a categorical column. Essentially rinse and repeat what we did just before but for 
##categorical data. 
mode_of <- function(x){
  x_no_na <- x[!is.na(x)]; if(length(x_no_na)==0) return(NA)
  names(sort(table(x_no_na), decreasing = TRUE))[1]
}
catcolModes <- sapply(train[catCols], mode_of)

for(v in catCols){
  if(is.factor(train[[v]])){
    train[[v]] <- droplevels(train[[v]]); test[[v]] <- factor(test[[v]], levels = levels(train[[v]]))
  } else {
    train[[v]] <- as.character(train[[v]]); test[[v]] <- as.character(test[[v]])
  }
  m <- catcolModes[[v]]
  train[[v]][is.na(train[[v]])] <- m; test[[v]][is.na(test[[v]])] <- m
  train[[v]] <- as.factor(train[[v]]); test[[v]] <- factor(test[[v]], levels = levels(train[[v]]))
}

## 6 Remove height/weight outliers by age bin (train only)
# @jodie why did we choose bin of 6 months?
## @Andrew, age distribution is min age ~5.9 months, and max ~61.8 momths, I think babies develop fast enough for a 6 month to be a grouping
## Also choosing 66 as upper bound because max age in training set is ~61.8 months old. 
train$age_bin <- cut(train$agecalc_adm, breaks = seq(0,66,6), include.lowest = TRUE, right = FALSE)

outliers <- function(x){
  Q1 <- quantile(x, 0.25, na.rm=TRUE); Q3 <- quantile(x, 0.75, na.rm=TRUE)
  IQRv <- Q3 - Q1; x < (Q1 - 1.5*IQRv) | x > (Q3 + 1.5*IQRv)
}

train_clean <- train
for(bin in unique(train$age_bin)){
  rows <- which(train$age_bin == bin); if(length(rows) < 3) next
  w <- train$weight_kg_adm[rows]; h <- train$height_cm_adm[rows]
  w_out <- outliers(w); h_out <- outliers(h)
  train_clean$weight_kg_adm[rows][w_out] <- mean(w[!w_out], na.rm=TRUE)
  train_clean$height_cm_adm[rows][h_out] <- mean(h[!h_out], na.rm=TRUE)
}
train_clean$age_bin <- NULL; train <- train_clean

## Add some derived variables, e.g. shock index= HR/SBP
## @jodie, I know we initially computed SIRS and SOFA because of clinical relevance, but I  
## removed them because they were not improving our weighted scores. 

# Mean Arterial Pressure
train$MAP <- (train$sysbp_mmhg_adm + 2*train$diasbp_mmhg_adm)/3
test$MAP  <- (test$sysbp_mmhg_adm + 2*test$diasbp_mmhg_adm)/3

# Pulse Pressure
train$pp <- train$sysbp_mmhg_adm - train$diasbp_mmhg_adm
test$pp <- test$sysbp_mmhg_adm - test$diasbp_mmhg_adm

# Shock Index
train$SI <- train$hr_bpm_adm / pmax(train$sysbp_mmhg_adm,1e-3)
test$SI  <- test$hr_bpm_adm  / pmax(test$sysbp_mmhg_adm,1e-3)

# Respiratory Failure Flag
# mark a child as having respiratory failure if their oxygen saturation (SpO2) is below 92%, 
# or if there is any documented respiratory distress
# @jodie Why did we choose 92%? 
# @Andrew see this: National Institute for Health and Care Excellence. (2021). Evidence reviews for criteria for referral, admission, oxygen supplementation, and discharge — Bronchiolitis in children: diagnosis and management (NICE Guideline No. 9). NCBI Bookshelf. https://www.ncbi.nlm.nih.gov/books/NBK573296/

train$resp_failure_flag <- as.integer(train$spo2site1_pc_oxi_adm < 92 | (!is.na(train$respdistress_adm) & train$respdistress_adm!="F"))
test$resp_failure_flag  <- as.integer(test$spo2site1_pc_oxi_adm  < 92 | (!is.na(test$respdistress_adm ) & test$respdistress_adm !="F"))

# Coma Score
# for each variable, give a score of 2 if they show normal response, give a score of 1 if the response is reduced
#score clinically relevant because deterioration neurologically can be an indication of organ failure. 

coma_score_fun <- function(eye,motor,verbal){
  e <- ifelse(eye=="Watches or follows",2L,1L)
  m <- ifelse(motor=="Localizes painful stimulus",2L,1L)
  v <- ifelse(!is.na(verbal)&grepl("Cries appropriately|speaks",verbal),2L,1L)
  e+m+v
}
train$coma_score <- mapply(coma_score_fun,train$bcseye_adm,train$bcsmotor_adm,train$bcsverbal_adm)
test$coma_score  <- mapply(coma_score_fun,test$bcseye_adm ,test$bcsmotor_adm ,test$bcsverbal_adm)
train$coma_flag <- as.integer(train$coma_score<=4); test$coma_flag <- as.integer(test$coma_score<=4)

#Jaundice Flag
#jaundice is the yellowing of skin, associated with liver dysfunction
# relevant to peds sepsis as mentioned in: Cleveland Clinic. (n.d.). Sepsis in newborns (neonatal sepsis). Cleveland Clinic. https://my.clevelandclinic.org/health/diseases/15371-sepsis-in-newborns
#if patient is reported as T, then we flag them
train$jaundice_flag <- as.integer(train$symptoms_adm_jaundice!="F")
test$jaundice_flag  <- as.integer(test$symptoms_adm_jaundice !="F")

#Fever/ Abnormal temperature flag
train$fever_flag <- as.integer(train$temp_c_adm>37.5 | train$temp_c_adm<36.5)
test$fever_flag  <- as.integer(test$temp_c_adm >37.5 | test$temp_c_adm <36.5)

#Hypotension Flag
train$hypotension_flag <- as.integer(train$MAP<60)
test$hypotension_flag  <- as.integer(test$MAP<60)

##Age Adjusted Z Scores, @Andrew Roz mentioned we could explore adding in zscore derivatives of HR, RR, 
# Age Adjusted HR Z score
get_hr_z <- function(age,hr){
  if(age<1){mean<-130;sd<-(160-100)/4}
  else if(age<12){mean<-110;sd<-(140-80)/4}
  else if(age<36){mean<-105;sd<-(130-80)/4}
  else if(age<60){mean<-95;sd<-(110-80)/4}
  else if(age<144){mean<-85;sd<-(100-70)/4}
  else{mean<-80;sd<-(100-60)/4}
  (hr-mean)/sd
}
train$hr_z <- mapply(get_hr_z, train$agecalc_adm, train$hr_bpm_adm)
test$hr_z  <- mapply(get_hr_z, test$agecalc_adm,  test$hr_bpm_adm)

# Age Adjusted RR Z score (based on healthy patients)
# the values are based on an article i found on healthy children
get_rr_z <- function(age,rr){
  if(age<12){mean<-45;sd<-(60-30)/4}
  else if(age<36){mean<-32;sd<-(40-24)/4}
  else if(age<72){mean<-28;sd<-(34-22)/4}
  else if(age<144){mean<-24;sd<-(30-18)/4}
  else{mean<-14;sd<-(16-12)/4}
  (rr-mean)/sd
}
train$rr_z <- mapply(get_rr_z,train$agecalc_adm,train$rr_brpm_app_adm)
test$rr_z  <- mapply(get_rr_z,test$agecalc_adm ,test$rr_brpm_app_adm)

## Encoding
## Decided to do one hot because our dataset is relatively small. Since we don't want to overfit, rules out target encoding, apply to all nominal categorical data. 
## I think this is the part that broke mice as well before and why we had to switch to mode and median imputation

y_train <- train$inhospital_mortality
if(is.factor(y_train)) y_train <- as.numeric(as.character(y_train))
x_train <- subset(train, select=-inhospital_mortality)
x_test  <- subset(test , select=-inhospital_mortality)
trMat <- model.matrix(~ . - 1, data=x_train)
tsMat <- model.matrix(~ . - 1, data=x_test)
colnames(trMat) <- make.names(colnames(trMat), unique=TRUE)
colnames(tsMat) <- make.names(colnames(tsMat), unique=TRUE)
cat("Final number of encoded features:", ncol(trMat), "\n")

## Build a gradient boosted tree model using all the training data 
#need to convert our training matrix to proper lgbm format
## Roz mentioned we should either do cross validation or do gradient search

dTrain <- lgb.Dataset(trMat, label=y_train)
lgbParams <- list(
  objective="binary", metric="auc", learning_rate=0.03,
  num_leaves=20, max_depth=4, min_data_in_leaf=40,
  feature_fraction=0.7, bagging_fraction=0.7, bagging_freq=1,
  lambda_l2=1, verbose=-1
)

cvRes <- lgb.cv(params=lgbParams, data=dTrain, nrounds=400, nfold=5,
                stratified=TRUE, early_stopping_rounds=40, verbose=-1)
bestIter <- cvRes$best_iter
cat("Best CV iteration =", bestIter, "\n")

myTree <- lightgbm(
  data=dTrain, label=y_train, params=lgbParams,
  nrounds=bestIter, verbose=-1
)

## Predictions
train$prob <- predict(myTree, trMat)
test$prob  <- predict(myTree, tsMat)

## Find threshold
source(file.path("..","scoring","evaluate_performance.R"))

prob_summary <- quantile(train$prob, c(.01,.05,.50,.95,.99))
print(prob_summary)

sorted_probs <- sort(train$prob)
diffs <- diff(sorted_probs)
cat("\nSummary of consecutive differences:\n"); print(summary(diffs))

# Results from above ^
# the 1-5% quantiles are ~0.009–0.011, so thresholds below ~0.01 classify almost everything as positive.
# The median is ~0.022, so most scores are low.
# The 95th percentile is ~0.122, meaning almost all predictions fall below 0.12.
# Based on this, the range where the model meaningfully separates classes is roughly 0.02 to 0.12.
# Values below ~0.02 behave like all positives, and values above ~0.12 behave like all negatives.Best way is to selecte lower bounds of 0.02 and upper bounds of 0.12

ths <- seq(0.02,0.12,0.002); best_t <- NA; best_w <- -Inf
for(t in ths){
  
  # Binary predictions at this threshold
  p <- as.numeric(train$prob >= t)
  
  # Skip thresholds that collapse to a single class
  if(length(unique(p)) < 2) next
  
  out <- evaluate_model(
    labels=y_train,
    prediction_probability=train$prob,
    threshold=t,
    dataset_label="Train",
    inference_speed=0
  )
  
  w <- out$weighted_score
  if(!is.na(w) && w > best_w){ best_w <- w; best_t <- t }
}

threshold <- best_t
cat("Chosen threshold:", round(threshold,3),
    "| training weighted_score:", round(best_w,3), "\n")

#Plot the AUC
roc_GBM <- roc(inhospital_mortality ~ prob, data=train)
plot(roc_GBM, main=paste0("AUC=", round(roc_GBM$auc,3)))
thresh <- coords(
  roc_GBM, "b", best.method="youden", input="threshold", transpose=TRUE,
  ret=c("threshold","sensitivity","specificity","ppv","npv","fp","tp","fn","tn")
)

roc_GBM_test <- roc(inhospital_mortality ~ prob, data=test)
plot(roc_GBM_test, add=TRUE, col="red")
text(0.3, 0.3, paste0("AUC_test=", round(roc_GBM_test$auc,3)), col="red")

## FINALLY Save model + meta files
lgb.save(myTree, paste0(team,"_lightgbm.model"))
cat("Saved LightGBM model:", paste0(team,"_lightgbm.model"), "\n")

meta <- list(thresh=round(threshold,3))
saveRDS(meta, paste0(team,"_model_meta.rds"))
cat("Saved meta file:", paste0(team,"_model_meta.rds"), "\n")

round(threshold,3)

## Quick Performance evaluation evaluate_model(label, prediction_probability, threshold)
resTbl <- NULL
resTbl <- rbind(resTbl, evaluate_model(train$inhospital_mortality, train$prob, threshold, "Training", 0))
resTbl <- rbind(resTbl, evaluate_model(test$inhospital_mortality, test$prob, threshold, "Testing", 0))
print(resTbl)
