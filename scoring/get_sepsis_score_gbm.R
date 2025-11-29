#!/usr/bin/Rscript

## You can change these functions however you see fit, but the input and output arguments MUST remain unchanged.
get_sepsis_score = function(data, myModel){
  # Impute missing data
  library(mice)
  data<-complete(mice(data, method = "pmm",m=1))
  
  # Add the new columns you made
  data$SI<-data$hr_bpm_adm/data$sysbp_mmhg_adm
    
  
  

# Basic haemodynamic features
  
  # Pulse pressure
  data$pulse_pressure <- data$sysbp_mmhg_adm - data$diasbp_mmhg_adm
  
  # Mean arterial pressure (MAP)
  data$MAP <- (data$sysbp_mmhg_adm + 2 * data$diasbp_mmhg_adm) / 3
  
  # Shock index (HR / SBP)
  data$SI <- data$hr_bpm_adm / data$sysbp_mmhg_adm
  
  # Modified shock index (HR / MAP)
  data$modified_shock_index <- data$hr_bpm_adm / data$MAP
  
  # Age-adjusted HR z score (healthy pediatric ranges)

  get_hr_z <- function(age, hr) {
    if (age < 1) {              # 0–1 month
      mean <- 130; sd <- (160 - 100) / 4
    } else if (age < 12) {      # 1–12 months
      mean <- 110; sd <- (140 - 80) / 4
    } else if (age < 36) {      # 1–3 years
      mean <- 105; sd <- (130 - 80) / 4
    } else if (age < 60) {      # 3–5 years
      mean <- 95;  sd <- (110 - 80) / 4
    } else if (age < 144) {     # 6–12 years
      mean <- 85;  sd <- (100 - 70) / 4
    } else {                    # adolescents
      mean <- 80;  sd <- (100 - 60) / 4
    }
    (hr - mean)/sd
  }
  data$hr_z <- mapply(get_hr_z, data$agecalc_adm, data$hr_bpm_adm)
  
  
  # Age-adjusted RR z score

  get_rr_z <- function(age, rr) {
    if (age < 12) {                 # 0–11 months
      mean <- 45; sd <- (60 - 30) / 4
    } else if (age < 36) {          # 1–3 years
      mean <- 32; sd <- (40 - 24) / 4
    } else if (age < 72) {          # 3–6 years
      mean <- 28; sd <- (34 - 22) / 4
    } else if (age < 144) {         # 6–12 years
      mean <- 24; sd <- (30 - 18) / 4
    } else {                        # adolescent
      mean <- 14; sd <- (16 - 12) / 4
    }
    (rr - mean)/sd
  }
  data$rr_z <- mapply(get_rr_z, data$agecalc_adm, data$rr_brpm_app_adm)
  
  

  #  SIRS-like features (vitals-only version)
  
  data$sirs_temp_flag <- as.integer(data$temp_c_adm > 38 | data$temp_c_adm < 36)
  data$sirs_hr_flag   <- as.integer(data$hr_bpm_adm > 90)
  data$sirs_rr_flag   <- as.integer(data$rr_brpm_app_adm > 20)
  
  data$sirs_score     <- data$sirs_temp_flag + data$sirs_hr_flag + data$sirs_rr_flag
  data$sirs_positive  <- as.integer(data$sirs_score >= 2)
  
  

  # Respiratory SOFA-like proxies

  data$resp_failure_flag <- as.integer(
    data$spo2site1_pc_oxi_adm < 92 | data$respdistress_adm > 0
  )
  
  data$resp_severity_score <-
    as.integer(data$rr_brpm_app_adm >= 22) +
    as.integer(data$spo2site1_pc_oxi_adm < 92)
  
  

  # Cardiovascular SOFA-like proxies

  data$hypotension_flag <- as.integer(data$MAP < 65)
  
  data$cv_failure_flag <- as.integer(
    data$MAP < 60 | data$modified_shock_index > 2
  )
  
  

  #  Renal proxies (symptom-based)

  data$renal_flag_oliguria   <- as.integer(data$symptoms_adm_oliguria > 0)
  data$renal_flag_urinecolor <- as.integer(data$symptoms_adm_urinecolor > 0)
  
  data$renal_score <- data$renal_flag_oliguria + data$renal_flag_urinecolor
  
  data$kidney_performance_ratio <- 1 - pmin(data$renal_score, 2) / 2
  
  data$renal_flag <- as.integer(data$renal_score > 0)
  
  

  #  CNS proxy (Blantyre-like coma scale)

  coma_score_fun <- function(eye, motor, verbal) {
    e <- ifelse(!is.na(eye)   & eye == "Watches or follows", 2L, 1L)
    m <- ifelse(!is.na(motor) & motor == "Localizes painful stimulus", 2L, 1L)
    v <- ifelse(!is.na(verbal) & grepl("Cries appropriately|speaks", verbal), 2L, 1L)
    e + m + v
  }
  
  data$coma_score <- mapply(
    coma_score_fun,
    data$bcseye_adm,
    data$bcsmotor_adm,
    data$bcsverbal_adm
  )
  
  data$coma_flag <- as.integer(data$coma_score <= 4)
  
  

  # Liver / systemic flags

  data$jaundice_flag <- as.integer(data$symptoms_adm_jaundice > 0)
  
  

  # Combined SOFA-lite score (organ dysfunction summary)

  data$sofa_lite_score <- 
    data$resp_failure_flag +
    data$cv_failure_flag +
    data$renal_flag +
    data$coma_flag +
    data$jaundice_flag
  
  
  
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
