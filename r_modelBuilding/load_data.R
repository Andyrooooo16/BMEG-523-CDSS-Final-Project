load_data<-function(kind){
  file<-file.path("..","Data",paste0(kind,"_2025-11-12.csv"))
  if (!file.exists(file)){
    return(NULL)
  }
  dat<-read.csv(file,na.strings=c("","NA"))
  tfs<-c("spo2onoxy_adm","respdistress_adm","caprefill_adm",
         "bcgscar_adm","vaccmeasles_adm","vaccpneumoc_adm","vaccdpt_adm","priorweekabx_adm","priorweekantimal_adm",	
         "symptoms_adm_rash","symptoms_adm_cough","symptoms_adm_cough_chronic","symptoms_adm_diarrhea","symptoms_adm_diarrhea_chronic","symptoms_adm_fever",
         "symptoms_adm_fever_chronic","symptoms_adm_vomiting","symptoms_adm_sleepy","symptoms_adm_edemafeet","symptoms_adm_urinecolor","symptoms_adm_oliguria",
         "symptoms_adm_bloodstool","symptoms_adm_seizures","symptoms_adm_jaundice",
         "comorbidity_adm_airway","comorbidity_adm_cardiac","comorbidity_adm_sicklecell","comorbidity_adm_tuberculosis","comorbidity_adm_disability",
         "prioryearwheeze_adm","prioryearcough_adm","diarrheaoften_adm","tbcontact_adm","birthdetail_adm_premature",
         "waterpure_adm","hctpretransfusion_adm","hivstatus_adm","malariastatuspos_adm")
  for (tf in tfs){
    eval(parse(text=paste0("dat$",tf,"<-factor(dat$",tf,",labels=c(F,T),levels=c('No','Yes'))")))
  }
  # Some factors have an order of 'goodness', so we make the healthiest option the reference category if that doesn't happen alphabetically
  dat$priorhosp_adm<-factor(dat$priorhosp_adm,levels=c("Never","< 7 days","7 days - 1 month","1 month - 1 year","> 1 year"))
  dat$bednet_adm<-factor(dat$bednet_adm,levels=c("Never","Sometimes","Always"))
  dat$tobacco_adm<-factor(dat$tobacco_adm,levels=c("Never","Less than monthly","Monthly","Weekly","Daily"))
  dat$badhealthduration_adm<-factor(dat$badhealthduration_adm,levels=c("In good health prior to this illness","< 1 week","1 week - 1 month","1 month - 1 year","> 1 year"))
  dat$bcseye_adm<-factor(dat$bcseye_adm,levels=c("Watches or follows","Fails to watch or follow"))
  dat$bcsmotor_adm<-factor(dat$bcsmotor_adm,levels=c("Localizes painful stimulus","Withdraws limb from painful stimulus","No response or inappropriate response"))
  dat$cookloc_adm<-factor(dat$cookloc_adm,levels=c("In a separate building/building space used as a kitchen","Outdoors in the open","In the house where you sleep"))
  dat$traveldist_adm<-factor(dat$traveldist_adm,levels=c("< 30 minutes","30 minutes - 1 hour","1 - 2 hours","2 - 3 hours","3 - 4 hours","4 - 8 hours","> 8 hours"))
  dat$travelmethod_adm<-factor(dat$travelmethod_adm,levels=c("Motorcycle","Taxi/special hire","Ambulance","Private vehicle","Walking","Other"))
  dat$feedingstatus_adm<-factor(dat$feedingstatus_adm,levels=c("Feeding well","Feeding poorly","Not feeding at all"))
  facts<-c("sex_adm","priorhosp_adm","bcsverbal_adm","lightfuel_adm")
  for (f in facts){
    eval(parse(text=paste0("dat$",f,"<-as.factor(dat$",f,")")))
  }
  # Drop the patient ID column
  dat<-subset(dat,select=-c(studyid_adm))
  return(dat)
}