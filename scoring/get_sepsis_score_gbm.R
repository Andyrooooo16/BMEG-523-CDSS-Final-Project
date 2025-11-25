#!/usr/bin/Rscript

## You can change these functions however you see fit, but the input and output arguments MUST remain unchanged.
get_sepsis_score = function(data, myModel){
  # Impute missing data
  library(mice)
  data<-complete(mice(data, method = "pmm",m=1))
  
  # Add the new columns you made
  data$SI<-data$hr_bpm_adm/data$sysbp_mmhg_adm
    
  # Make the prediction
  probSepsis <- predict(myModel$bst,newdata=as.matrix(data))
  label <- probSepsis >= myModel$thresh
  
  #Return a dataframe
  return(data.frame(probSepsis,label))
}

load_sepsis_model <- function(){
  library('lightgbm')
  myModel<-list(thresh = c(threshold = 0.086))
  myModel$bst<-lgb.load("Example_lightgbm.model")
  return(myModel)
}
