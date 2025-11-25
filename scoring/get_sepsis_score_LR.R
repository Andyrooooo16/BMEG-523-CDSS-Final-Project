#!/usr/bin/Rscript

## You can change these functions however you see fit, but the input and output arguments MUST remain unchanged.
get_sepsis_score = function(data, myModel){
  # We need to drop tobacco_adm variable as the observations change which cause issues
  data<-subset(data,select=-c(tobacco_adm))
  
  # Impute missing data
  library(mice)
  data<-complete(mice(data, method = "pmm",m=1))
  
  # Add the new columns you made
  data$SI<-data$hr_bpm_adm/data$sysbp_mmhg_adm
    
  # Make the prediction
  probSepsis <- predict(myModel$logreg,newdata=data)
  label <- probSepsis >= myModel$thresh
  
  #Return a dataframe
  return(data.frame(probSepsis,label))
}

load_sepsis_model <- function(){
  myModel<-list(thresh = c(threshold = -2.58), const = c(`(Intercept)` = -4.40168207927402), 
                coeffs = c(agecalc_adm = 0.00471946922680516, height_cm_adm = 0.0599626999331076, 
                           weight_kg_adm = -0.224871383485509, muac_mm_adm = -0.0160800771431163, 
                           hr_bpm_adm = -0.0111014208007509, rr_brpm_app_adm = -0.00323978692702602, 
                           sysbp_mmhg_adm = 0.00484690314649159, diasbp_mmhg_adm = -0.0165036719566855, 
                           temp_c_adm = 0.070244305191889, spo2site1_pc_oxi_adm = 0.0446753166743692, 
                           spo2site2_pc_oxi_adm = -0.038563651371107, momagefirstpreg_adm = -0.0233957634078041, 
                           householdsize_adm = 0.0508547116615842, alivechildren_adm = -0.171328785560501, 
                           deadchildren_adm = 0.171284401982916, hematocrit_gpdl_adm = 0.00361741444810405, 
                           lactate_mmolpl_adm = 0.0291292047229842, glucose_mmolpl_adm = 0.0956966425074466, 
                           sex_admMale = -0.666860784327872, spo2onoxy_admTRUE = 1.3233795559631, 
                           respdistress_admTRUE = 0.0665737836611252, caprefill_admTRUE = 0.371685031644914, 
                           `bcseye_admFails to watch or follow` = 0.329374972374237, 
                           `bcsmotor_admWithdraws limb from painful stimulus` = 1.31352976294965, 
                           `bcsmotor_admNo response or inappropriate response` = 2.8977166115651, 
                           `bcsverbal_admMoan or abnormal cry with pain` = 0.408582335096482, 
                           `bcsverbal_admNo vocal response to pain` = 0.318723371945924, 
                           bcgscar_admTRUE = -1.25342988613946, vaccmeasles_admTRUE = 0.47765438211283, 
                           vaccpneumoc_admTRUE = 0.533343151398602, vaccdpt_admTRUE = -1.06877143922495, 
                           priorweekabx_admTRUE = -0.0185143875573639, priorweekantimal_admTRUE = -0.187833692976751, 
                           symptoms_adm_rashTRUE = -0.0638020908432207, symptoms_adm_coughTRUE = -0.022531746954536, 
                           symptoms_adm_cough_chronicTRUE = 0.118213633534016, symptoms_adm_diarrheaTRUE = -0.195984792575328, 
                           symptoms_adm_diarrhea_chronicTRUE = -0.105631551079861, symptoms_adm_feverTRUE = 1.63679981963067, 
                           symptoms_adm_fever_chronicTRUE = 1.97376622546381, symptoms_adm_vomitingTRUE = -0.264432795048846, 
                           symptoms_adm_sleepyTRUE = 0.471030952225759, symptoms_adm_edemafeetTRUE = -0.0121284622794855, 
                           symptoms_adm_urinecolorTRUE = 0.31353104642056, symptoms_adm_oliguriaTRUE = -0.930436677049254, 
                           symptoms_adm_bloodstoolTRUE = 0.324114018434244, symptoms_adm_seizuresTRUE = -0.115945860656327, 
                           symptoms_adm_jaundiceTRUE = 0.864173056829495, comorbidity_adm_airwayTRUE = -1.65490288197844, 
                           comorbidity_adm_cardiacTRUE = -0.81286594004841, comorbidity_adm_sicklecellTRUE = -15.7486568740461, 
                           comorbidity_adm_tuberculosisTRUE = -16.2858450811407, comorbidity_adm_disabilityTRUE = 0.471706929648882, 
                           `priorhosp_adm< 7 days` = 0.0954281058653122, `priorhosp_adm7 days - 1 month` = 1.22975935318046, 
                           `priorhosp_adm1 month - 1 year` = 0.194007848306354, `priorhosp_adm> 1 year` = 0.305077884742251, 
                           prioryearwheeze_admTRUE = -0.772699987774193, prioryearcough_admTRUE = -0.52746787623224, 
                           diarrheaoften_admTRUE = 0.287109894660294, tbcontact_admTRUE = -0.0580580790995071, 
                           `feedingstatus_admFeeding poorly` = -0.575116168356928, `feedingstatus_admNot feeding at all` = -0.669752332168834, 
                           birthdetail_adm_prematureTRUE = -0.984423004289215, `travelmethod_admTaxi/special hire` = -0.406546168312883, 
                           travelmethod_admAmbulance = 1.18346717412852, `travelmethod_admPrivate vehicle` = 0.210331682198303, 
                           travelmethod_admWalking = 0.299544684934746, travelmethod_admOther = -13.7772914910316, 
                           `traveldist_adm30 minutes - 1 hour` = 0.381763552211816, 
                           `traveldist_adm1 - 2 hours` = 0.245860017701893, `traveldist_adm2 - 3 hours` = 0.186444374958887, 
                           `traveldist_adm3 - 4 hours` = 1.26994605804973, `traveldist_adm4 - 8 hours` = -0.322147775984005, 
                           `traveldist_adm> 8 hours` = 4.17753058673896, `badhealthduration_adm< 1 week` = 1.34342201801334, 
                           `badhealthduration_adm1 week - 1 month` = -0.312904566770939, 
                           `badhealthduration_adm1 month - 1 year` = -0.47761033166527, 
                           `badhealthduration_adm> 1 year` = 0.640157330805943, waterpure_admTRUE = -0.151147366306774, 
                           `cookloc_admOutdoors in the open` = -0.346671029344347, `cookloc_admIn the house where you sleep` = -0.895010593288019, 
                           lightfuel_admCandles = 0.229635539856691, `lightfuel_admElectric bulbs (national grid)` = 0.273522358199021, 
                           `lightfuel_admKerosene lamp` = 0.603389258497891, `lightfuel_admOther (specify):` = -0.736971384184976, 
                           `lightfuel_admSolar lantern` = -1.0317660167531, `lightfuel_admSolar powered bulbs` = 0.00909851914584233, 
                           lightfuel_admTadooba = -0.123270201096204, bednet_admSometimes = -0.587854302590897, 
                           bednet_admAlways = -0.750832075751247, hctpretransfusion_admTRUE = -1.12739792936104, 
                           hivstatus_admTRUE = 1.00175646796102, malariastatuspos_admTRUE = -1.70550639104473, 
                           SI = 0.595204385375402))
  load("Example_LR.RData")
  myModel$logreg<-logReg
  return(myModel)
}
