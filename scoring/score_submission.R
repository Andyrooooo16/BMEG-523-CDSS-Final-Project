## Clear workspace
rm(list=ls())

## Load all the data 
source(file.path("..","r_modelBuilding","load_data.R"))
SEPSISdat_train<-load_data("train")
SEPSISdat_test<-load_data("test")
SEPSISdat_validate<-load_data("validate")
last_col<-ncol(SEPSISdat_train)-1

## Apply the model
# Load the definitions once
filename<-"get_sepsis_score_gbm.R"
filename<-"get_sepsis_score_LR.R"
source(file.path("..","scoring",filename))
myModel<-load_sepsis_model()
# Run the three models
start_time <- Sys.time()
result_train<-get_sepsis_score(SEPSISdat_train[,1:last_col],myModel)
end_time<-Sys.time()
inference_speed_train<-round(as.numeric((end_time-start_time)/nrow(SEPSISdat_train)),6)
#
start_time <- Sys.time()
result_test<-get_sepsis_score(SEPSISdat_test[,1:last_col],myModel)
end_time<-Sys.time()
inference_speed_test<-round(as.numeric((end_time-start_time)/nrow(SEPSISdat_train)),6)
##
start_time <- Sys.time()
result_validate<-get_sepsis_score(SEPSISdat_validate[,1:last_col],myModel)
end_time<-Sys.time()
inference_speed_validate<-round(as.numeric((end_time-start_time)/nrow(SEPSISdat_validate)),6)

source(file.path("..","scoring","evaluate_performance.R"))
res<-NULL
res<-rbind(res,evaluate_model(SEPSISdat_train$inhospital_mortality,result_train[,1],myModel$thresh,"Training",inference_speed_train))
res<-rbind(res,evaluate_model(SEPSISdat_test$inhospital_mortality,result_test[,1],myModel$thresh,"Testing",inference_speed_test))
if (sum(SEPSISdat_validate$inhospital_mortality)>0){
  res<-rbind(res,evaluate_model(SEPSISdat_validate$inhospital_mortality,result_validate[,1],myModel$thresh,"Evaluation",inference_speed_validate))
}
View(res)
write.csv(res,file=paste0("Results_",filename,".csv"),row.names=F)