## Clear workspace
rm(list=ls())

## TeamName
team<-"Example"

## Load all the data so we can quickly combine it and explore it. 
source("load_data.R")
SEPSISdat_train<-load_data("train")
sum(SEPSISdat_train$inhospital_mortality)/nrow(SEPSISdat_train)
SEPSISdat_test<-load_data("test")
sum(SEPSISdat_test$inhospital_mortality)/nrow(SEPSISdat_test)

##some values are NA
#consider front fill/back fill? Or is the mice function below enough?
#only important for time series data?


## Do some data cleaning, e.g. imputation for missing values
library(mice)
SEPSISdat_train<-complete(mice(SEPSISdat_train, method = "pmm",m=1))
SEPSISdat_test<-complete(mice(SEPSISdat_test, method = "pmm",m=1))

## Add some derived variables, e.g. shock index= HR/SBP
SEPSISdat_train$SI<-SEPSISdat_train$hr_bpm_adm/SEPSISdat_train$sysbp_mmhg_adm
SEPSISdat_test$SI<-SEPSISdat_test$hr_bpm_adm/SEPSISdat_test$sysbp_mmhg_adm


##change variables based on age, but only the vital signs that DO change with age


## 5.5.1 Age adjusted HR z score (based on healthy pediatric ranges)
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

SEPSISdat_train$hr_z <- mapply(get_hr_z, SEPSISdat_train$agecalc_adm, SEPSISdat_train$hr_bpm_adm)
SEPSISdat_test$hr_z  <- mapply(get_hr_z,  SEPSISdat_test$agecalc_adm,  SEPSISdat_test$hr_bpm_adm)


#Age Adjusted RR Z score
get_rr_z <- function(age, rr) {
  
  if (age < 12) {                     # 0 to 1 year (0–11 months)
    mean <- 45;  sd <- (60 - 30) / 4  # 30–60
    
  } else if (age < 36) {              # 1–3 years
    mean <- 32;  sd <- (40 - 24) / 4  # 24–40
    
  } else if (age < 72) {              # 3–6 years
    mean <- 28;  sd <- (34 - 22) / 4  # 22–34
    
  } else if (age < 144) {             # 6–12 years
    mean <- 24;  sd <- (30 - 18) / 4  # 18–30
    
  } else {                            # 12–18 years (adolescent)
    mean <- 14;  sd <- (16 - 12) / 4  # 12–16
  }
  
  (rr - mean) / sd
}

SEPSISdat_train$rr_z <- mapply(get_rr_z, SEPSISdat_train$agecalc_adm, SEPSISdat_train$rr_brpm_app_adm)
SEPSISdat_test$rr_z  <- mapply(get_rr_z,  SEPSISdat_test$agecalc_adm,  SEPSISdat_test$rr_brpm_app_adm)



## 5.5.2 Basic haemodynamic features
# Pulse pressure
SEPSISdat_train$pulse_pressure <- SEPSISdat_train$sysbp_mmhg_adm - SEPSISdat_train$diasbp_mmhg_adm
SEPSISdat_test$pulse_pressure  <- SEPSISdat_test$sysbp_mmhg_adm  - SEPSISdat_test$diasbp_mmhg_adm

# Mean arterial pressure (MAP)
SEPSISdat_train$MAP <- (SEPSISdat_train$sysbp_mmhg_adm + 2 * SEPSISdat_train$diasbp_mmhg_adm) / 3
SEPSISdat_test$MAP  <- (SEPSISdat_test$sysbp_mmhg_adm  + 2 * SEPSISdat_test$diasbp_mmhg_adm)  / 3

# Shock index (HR / SBP)
SEPSISdat_train$SI <- SEPSISdat_train$hr_bpm_adm / SEPSISdat_train$sysbp_mmhg_adm
SEPSISdat_test$SI  <- SEPSISdat_test$hr_bpm_adm  / SEPSISdat_test$sysbp_mmhg_adm

# Modified shock index (HR / MAP)
SEPSISdat_train$modified_shock_index <- SEPSISdat_train$hr_bpm_adm / SEPSISdat_train$MAP
SEPSISdat_test$modified_shock_index  <- SEPSISdat_test$hr_bpm_adm  / SEPSISdat_test$MAP


## 5.5.3 SIRS-like features (vitals-only version)

# Temperature flag (fever or hypothermia)
SEPSISdat_train$sirs_temp_flag <- with(SEPSISdat_train, as.integer(temp_c_adm > 38 | temp_c_adm < 36))
SEPSISdat_test$sirs_temp_flag  <- with(SEPSISdat_test,  as.integer(temp_c_adm > 38 | temp_c_adm < 36))

# Heart rate flag (simple tachycardia threshold)
SEPSISdat_train$sirs_hr_flag <- with(SEPSISdat_train, as.integer(hr_bpm_adm > 90))
SEPSISdat_test$sirs_hr_flag  <- with(SEPSISdat_test,  as.integer(hr_bpm_adm > 90))

# Respiratory rate flag (tachypnea)
SEPSISdat_train$sirs_rr_flag <- with(SEPSISdat_train, as.integer(rr_brpm_app_adm > 20))
SEPSISdat_test$sirs_rr_flag  <- with(SEPSISdat_test,  as.integer(rr_brpm_app_adm > 20))

# SIRS-like total score (0–3) and positive indicator (>=2)
SEPSISdat_train$sirs_score    <- SEPSISdat_train$sirs_temp_flag + SEPSISdat_train$sirs_hr_flag + SEPSISdat_train$sirs_rr_flag
SEPSISdat_test$sirs_score     <- SEPSISdat_test$sirs_temp_flag  + SEPSISdat_test$sirs_hr_flag  + SEPSISdat_test$sirs_rr_flag

SEPSISdat_train$sirs_positive <- as.integer(SEPSISdat_train$sirs_score >= 2)
SEPSISdat_test$sirs_positive  <- as.integer(SEPSISdat_test$sirs_score  >= 2)


## 5.5.4 Respiratory SOFA-like proxies

# Respiratory failure flag: low SpO2 or documented respiratory distress
SEPSISdat_train$resp_failure_flag <- with(
  SEPSISdat_train,
  as.integer(spo2site1_pc_oxi_adm < 92 | respdistress_adm > 0)
)
SEPSISdat_test$resp_failure_flag <- with(
  SEPSISdat_test,
  as.integer(spo2site1_pc_oxi_adm < 92 | respdistress_adm > 0)
)

# Respiratory severity score (0–2): RR high + low SpO2
SEPSISdat_train$resp_severity_score <- with(
  SEPSISdat_train,
  as.integer(rr_brpm_app_adm >= 22) + as.integer(spo2site1_pc_oxi_adm < 92)
)
SEPSISdat_test$resp_severity_score <- with(
  SEPSISdat_test,
  as.integer(rr_brpm_app_adm >= 22) + as.integer(spo2site1_pc_oxi_adm < 92)
)


## 5.5.5 Cardiovascular SOFA-like proxies

# Hypotension flag
SEPSISdat_train$hypotension_flag <- as.integer(SEPSISdat_train$MAP < 65)
SEPSISdat_test$hypotension_flag  <- as.integer(SEPSISdat_test$MAP  < 65)

# Cardiovascular failure flag: more severe hypotension or marked shock index
SEPSISdat_train$cv_failure_flag <- as.integer(SEPSISdat_train$MAP < 60 | SEPSISdat_train$modified_shock_index > 2)
SEPSISdat_test$cv_failure_flag  <- as.integer(SEPSISdat_test$MAP  < 60 | SEPSISdat_test$modified_shock_index  > 2)


## 5.5.6 Renal proxies (symptom-based)

# Oliguria and abnormal urine color are stored as 0/1 after logi -> integer conversion
SEPSISdat_train$renal_flag_oliguria   <- as.integer(SEPSISdat_train$symptoms_adm_oliguria   > 0)
SEPSISdat_test$renal_flag_oliguria    <- as.integer(SEPSISdat_test$symptoms_adm_oliguria    > 0)

SEPSISdat_train$renal_flag_urinecolor <- as.integer(SEPSISdat_train$symptoms_adm_urinecolor > 0)
SEPSISdat_test$renal_flag_urinecolor  <- as.integer(SEPSISdat_test$symptoms_adm_urinecolor  > 0)

# Simple renal symptom score (0–2)
SEPSISdat_train$renal_score <- SEPSISdat_train$renal_flag_oliguria + SEPSISdat_train$renal_flag_urinecolor
SEPSISdat_test$renal_score  <- SEPSISdat_test$renal_flag_oliguria  + SEPSISdat_test$renal_flag_urinecolor

# Kidney performance ratio: 1 = no symptoms, 0 = both present
SEPSISdat_train$kidney_performance_ratio <- 1 - pmin(SEPSISdat_train$renal_score, 2) / 2
SEPSISdat_test$kidney_performance_ratio  <- 1 - pmin(SEPSISdat_test$renal_score,  2) / 2

# Any renal involvement flag
SEPSISdat_train$renal_flag <- as.integer(SEPSISdat_train$renal_score > 0)
SEPSISdat_test$renal_flag  <- as.integer(SEPSISdat_test$renal_score  > 0)


## 5.5.7 CNS proxy using Blantyre-like coma scale information

coma_score_fun <- function(eye, motor, verbal) {
  # map best-observed states to higher scores, everything else lower
  e <- ifelse(!is.na(eye)   & eye   == "Watches or follows", 2L, 1L)
  m <- ifelse(!is.na(motor) & motor == "Localizes painful stimulus", 2L, 1L)
  # verbal: look for "Cries appropriately" or "speaks" anywhere in the string
  v <- ifelse(!is.na(verbal) & grepl("Cries appropriately|speaks", verbal), 2L, 1L)
  e + m + v
}

SEPSISdat_train$coma_score <- mapply(
  coma_score_fun,
  SEPSISdat_train$bcseye_adm,
  SEPSISdat_train$bcsmotor_adm,
  SEPSISdat_train$bcsverbal_adm
)

SEPSISdat_test$coma_score <- mapply(
  coma_score_fun,
  SEPSISdat_test$bcseye_adm,
  SEPSISdat_test$bcsmotor_adm,
  SEPSISdat_test$bcsverbal_adm
)

# CNS impairment flag: lower scores imply worse neurologic status
SEPSISdat_train$coma_flag <- as.integer(SEPSISdat_train$coma_score <= 4)
SEPSISdat_test$coma_flag  <- as.integer(SEPSISdat_test$coma_score  <= 4)


## 5.5.8 Liver / systemic flags

# Jaundice symptom as crude liver dysfunction proxy
SEPSISdat_train$jaundice_flag <- as.integer(SEPSISdat_train$symptoms_adm_jaundice > 0)
SEPSISdat_test$jaundice_flag  <- as.integer(SEPSISdat_test$symptoms_adm_jaundice  > 0)


## 5.5.9 Combined SOFA-lite score (organ dysfunction summary)

SEPSISdat_train$sofa_lite_score <- with(
  SEPSISdat_train,
  resp_failure_flag +
    cv_failure_flag +
    renal_flag +
    coma_flag +
    jaundice_flag
)

SEPSISdat_test$sofa_lite_score <- with(
  SEPSISdat_test,
  resp_failure_flag +
    cv_failure_flag +
    renal_flag +
    coma_flag +
    jaundice_flag
)


##calculate feature importance gives us insight into maybe scaling or into which features to cut
## Build a gradient boosted tree model using all the training data

library(lightgbm)
library(pROC)

#this matrix is all our predictive features
train_matrix <- as.matrix(subset(SEPSISdat_test, select = -inhospital_mortality))

#this is the binary outcome that we are testing for?
train_label  <- SEPSISdat_test$inhospital_mortality

#default value (found on lightgbm parameter website) should be in the range 
#of the values in our grid search
#these values are what the grid search will try
#essentially trying every combination
#NOTE: if i included more parameters, runtime increases a lot, so what parameters
#should we prioritize? 

grid_nrounds       <- c(100, 400)
grid_max_depth     <- c(3, 5, 7)
grid_min_data_leaf <- c(20, 50)

#initialize our variables to track our model
best_auc <- -Inf
best_params <- list()
best_model <- NULL

#loop goes through each combination of the parameters we included above
for (nr in grid_nrounds) {
  for (md in grid_max_depth) {
      for (minleaf in grid_min_data_leaf) {
        
        #this is our parameter list for the lightgbm model
        params <- list(
          objective = "binary",
          metric = "auc",
          max_depth = md,
          min_data_in_leaf = minleaf,
          verbose = -1
        )
        
        #this is the cross validation and produces the average cross validation results
        #prevents overfitting?
        cv_res <- lgb.cv(
          params = params,
          data = lgb.Dataset(train_matrix, label = train_label),
          nrounds = nr,
          nfold = 5,
          stratified = TRUE,
          verbose = -1
        )
        
        #extracts the cross validated AUC
        cv_auc_values <- unlist(cv_res$record_evals$valid$auc$eval)
        cv_auc <- max(cv_auc_values)
        
        cat("nrounds =", nr,
            "| depth =", md,
            "| minleaf =", minleaf,
            "| CV AUC =", round(cv_auc, 4), "\n")
        
        #checks if model is better than the best one
        if (cv_auc > best_auc) {
          best_auc <- cv_auc
          best_params <- list(
            nrounds = nr,
            max_depth = md,
            min_data_in_leaf = minleaf
          )
          
          best_model <- lightgbm(
            data = train_matrix,
            label = train_label,
            params = params,
            nrounds = nr,
            verbose = -1
          )
        }
      }
    }
}

cat("\nBest params found by CV:\n")
print(best_params)
cat("Best CV AUC =", round(best_auc, 4), "\n")

myTree <- best_model

# Rank Top 20 Features based on Importance
importance <- lgb.importance(myTree, percentage = TRUE)
cat("\nTop 20 Most Important Features:\n")
print(head(importance, 20))

# Optional: plot importance
lgb.plot.importance(importance, top_n = 20)


#need to control how trees develop when fitting to data
#number of iterations (try increasing number) --> by default model doesnt run long enough
#learning rate --> has to do with how algorithm revists errors
#max depth
#min data in leaf (may work against max depth)

#look at parameters that can be used to deal with overfitting

#bagging fraction not important


#cross fold validation: finds optimal value for parameters
#only do cross validation on training set
#allows us to see how hyperparameters perform on a specific set
#takes the data set and splits it and the picks the best model

#hyperparameters optimization through grid search? (grid is something a lil below and above the default)

#allows us to get the best hyper parameters (do not include learning rate in this method)


#light GBM uses multiple decision trees
#decision trees predicts outcomes by asking a series of questions about the variables
#in the dataset and it eventually leads to final decision
#each node represents a decision point, ex is HR > 120, then it branches off
#at the very end, we will reach a leaf that says patient has sepsis or no sepsis





## Quick but not necessarily great way to find a threshold
SEPSISdat_train$probSepsisGBM <- predict(myTree,newdata=as.matrix(subset(SEPSISdat_train,select=-c(inhospital_mortality))))
SEPSISdat_test$probSepsisGBM <- predict(myTree,newdata=as.matrix(subset(SEPSISdat_test,select=-c(inhospital_mortality))))
# Plot the AUC
library('pROC')
roc_GBM <- roc(inhospital_mortality ~ probSepsisGBM,data=SEPSISdat_train)
plot(roc_GBM,main=paste0('AUC=',round(roc_GBM$auc,3)))
thresh<-coords(roc_GBM, "b", best.method="youden", input = "threshold", transpose = T,
               ret = c("threshold", "sensitivity","specificity","ppv","npv","fp","tp","fn","tn"))
roc_GBM_test <- roc(inhospital_mortality ~ probSepsisGBM,data=SEPSISdat_test)
plot(roc_GBM_test,add=T,col='red')
text(0.3,0.3,paste0('AUC_test=',round(roc_GBM_test$auc,3)),col="red")
threshold<-thresh[1]

## Prepare the things needed for submission:
## Report the values to put into my get_sepsis_score's load_sepsis_model function
myModel<- NULL
myModel$thresh <- round(thresh[1],3)
dput(myModel)

# Save the model and get the threshold for use as a model
lgb.save(myTree,paste0(team,"_","lightgbm.model"))
round(thresh[1],3)

## Quick Performance evaluation evaluate_model(label, prediction_probability, threshold)
source(file.path("..","scoring","evaluate_performance.R"))
res<-NULL
res<-rbind(res,evaluate_model(SEPSISdat_train$inhospital_mortality,SEPSISdat_train$probSepsisGBM,threshold,"Training",0))
res<-rbind(res,evaluate_model(SEPSISdat_test$inhospital_mortality,SEPSISdat_test$probSepsisGBM,threshold,"Testing",0))
print(res)