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

## Build a regression model
cols <- colnames(SEPSISdat_train)
# Drop the tobacco_adm variable as the observations change which cause issues
cols <- cols[!(cols %in% c("studyid_adm","inhospital_mortality","tobacco_adm"))]

formula <- as.formula(paste0("inhospital_mortality ~ ",paste0(cols,sep="",collapse="+")))
logReg <- glm(formula,data=SEPSISdat_train,family=binomial(link='logit'))
summary(logReg)


## Quick but not necessarily great way to find a threshold
SEPSISdat_train$probSepsisLR <- predict(logReg,newdata=subset(SEPSISdat_train,select=-c(inhospital_mortality)))
SEPSISdat_test$probSepsisLR <- predict(logReg,newdata=subset(SEPSISdat_test,select=-c(inhospital_mortality)))

# Plot the AUC
library('pROC')
roc_LR <- roc(inhospital_mortality ~ probSepsisLR,data=SEPSISdat_train)
plot(roc_LR,main=paste0('AUC=',round(roc_LR$auc,3)))
thresh<-coords(roc_LR, "b", best.method="youden", input = "threshold", transpose = T,
               ret = c("threshold", "sensitivity","specificity","ppv","npv","fp","tp","fn","tn"))
roc_LR_test <- roc(inhospital_mortality ~ probSepsisLR,data=SEPSISdat_test)
plot(roc_LR_test,add=T,col='red')
text(0.3,0.3,paste0('AUC_test=',round(roc_LR_test$auc,3)),col="red")
threshold<-thresh[1]

## Prepare the things needed for submission:
## Report the values to put into my get_sepsis_score's load_sepsis_model function
myModel<- NULL
myModel$thresh <- round(thresh[1],3)
myModel$const <- logReg$coefficients[1]
myModel$coeffs <- logReg$coefficients[2:length(logReg$coefficients)]
dput(myModel)

# Save the model and get the threshold for use as a model
save(logReg, file=paste0(team,"_","LR.RData"))

## Quick Performance evaluation evaluate_model(label, prediction_probability, threshold)
source(file.path("..","scoring","evaluate_performance.R"))
res<-NULL
res<-rbind(res,evaluate_model(SEPSISdat_train$inhospital_mortality,SEPSISdat_train$probSepsisLR,threshold,"Training",0))
res<-rbind(res,evaluate_model(SEPSISdat_test$inhospital_mortality,SEPSISdat_test$probSepsisLR,threshold,"Testing",0))
print(res)