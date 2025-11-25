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

## Do some data cleaning, e.g. imputation for missing values
library(mice)
SEPSISdat_train<-complete(mice(SEPSISdat_train, method = "pmm",m=1))
SEPSISdat_test<-complete(mice(SEPSISdat_test, method = "pmm",m=1))

## Add some derived variables, e.g. shock index= HR/SBP
SEPSISdat_train$SI<-SEPSISdat_train$hr_bpm_adm/SEPSISdat_train$sysbp_mmhg_adm
SEPSISdat_test$SI<-SEPSISdat_test$hr_bpm_adm/SEPSISdat_test$sysbp_mmhg_adm


#addition 1) abnormal heart rate based on article
is_abnormal_hr <- function(age_months, hr) {
  if (age_months < 1) {                   # Preterm (0 months)
    return(hr < 120 | hr > 180)
  } else if (age_months < 12) {           # Newborn–Infant (0–12 months)
    return(hr < 100 | hr > 160)
  } else if (age_months < 36) {           # Toddler (1–3 years)
    return(hr < 80 | hr > 130)
  } else if (age_months < 60) {           # Preschool (3–5 years)
    return(hr < 80 | hr > 110)
  } else if (age_months < 144) {          # School age (6–12 years)
    return(hr < 70 | hr > 100)
  } else {                                # Adolescents (12+ years)
    return(hr < 60 | hr > 100)
  }
}

SEPSISdat_train$hr_abnormal <- mapply(is_abnormal_hr,SEPSISdat_train$agecalc_adm,SEPSISdat_train$hr_bpm_adm)
SEPSISdat_test$hr_abnormal <- mapply(is_abnormal_hr,SEPSISdat_test$agecalc_adm,SEPSISdat_test$hr_bpm_adm
)

SEPSISdat_train$hr_abnormal <- mapply(is_abnormal_hr,SEPSISdat_train$agecalc_adm,SEPSISdat_train$hr_bpm_adm)
SEPSISdat_test$hr_abnormal <- mapply(is_abnormal_hr,SEPSISdat_test$agecalc_adm,SEPSISdat_test$hr_bpm_adm
)


##maybe instead create a z score and see how far away the age group is away from the normal?

#z = (patient - mean)/standard deviation
#for mean use the value of actual healthy score
#it shows how far away from good the patient is

#addition 2) pulse pressure
SEPSISdat_train$pulse_pressure <- SEPSISdat_train$sysbp_mmhg_adm - SEPSISdat_train$diasbp_mmhg_adm
SEPSISdat_test$pulse_pressure  <- SEPSISdat_test$sysbp_mmhg_adm - SEPSISdat_test$diasbp_mmhg_adm

## Build a gradient boosted tree model using all the training data
library('lightgbm')
myTree <- lightgbm(
  data = as.matrix(subset(SEPSISdat_train,select=-c(inhospital_mortality)))
  , label = SEPSISdat_train$inhospital_mortality
  , objective = "binary"
  , nrounds=10
)

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