#!/usr/bin/env python
import warnings
warnings.filterwarnings('ignore')
import numpy as np
import lightgbm as lgb
import pandas as pd
from sklearn.preprocessing import LabelEncoder
from fancyimpute import IterativeImputer

def get_sepsis_score(data, model):
    cat_feats = model["cat_feats"]
    thresh = model["thresh"]
    mymodel = model["model"]

    # Encode the categorical variables
    labelencoder = LabelEncoder()
    for col in cat_feats:
        data[col] = labelencoder.fit_transform(data[col])
        data[col] = data[col].astype('int')

    # Replace missing data (Note - a better approach is to save the imputation structure from the set you trained on
    imputer = IterativeImputer()
    data = pd.DataFrame(imputer.fit_transform(data),columns=data.columns)

    # Add some derived variables, e.g. shock index= HR/SBP
    data = data.assign(SI=data.hr_bpm_adm/data.sysbp_mmhg_adm)

    # Apply the pre-trained model and threshold the output
    score = mymodel.predict(data)
    label = score >= thresh

    return score, label

def load_sepsis_model():
    cat_feats = ['sex_adm', 'spo2onoxy_adm','respdistress_adm','caprefill_adm','bcseye_adm','bcsmotor_adm',
             'bcsverbal_adm', 'bcgscar_adm', 'vaccmeasles_adm', 'vaccpneumoc_adm', 'vaccdpt_adm', 
             'priorweekabx_adm', 'priorweekantimal_adm', 'symptoms_adm_rash', 'symptoms_adm_cough', 
             'symptoms_adm_cough_chronic', 'symptoms_adm_diarrhea', 'symptoms_adm_diarrhea_chronic', 
             'symptoms_adm_fever', 'symptoms_adm_fever_chronic', 'symptoms_adm_vomiting', 'symptoms_adm_sleepy', 
             'symptoms_adm_edemafeet', 'symptoms_adm_urinecolor', 'symptoms_adm_oliguria', 'symptoms_adm_bloodstool', 
             'symptoms_adm_seizures', 'symptoms_adm_jaundice', 'comorbidity_adm_airway', 'comorbidity_adm_cardiac', 
             'comorbidity_adm_sicklecell', 'comorbidity_adm_tuberculosis', 'comorbidity_adm_disability', 
             'priorhosp_adm', 'prioryearwheeze_adm', 'prioryearcough_adm', 'diarrheaoften_adm', 'tbcontact_adm', 
             'feedingstatus_adm', 'birthdetail_adm_premature', 'travelmethod_adm', 'traveldist_adm', 
             'badhealthduration_adm', 'waterpure_adm', 'cookloc_adm', 'lightfuel_adm', 'tobacco_adm', 
             'bednet_adm', 'hctpretransfusion_adm', 'hivstatus_adm', 'malariastatuspos_adm']
    thresh = 0.0992
    model = lgb.Booster(model_file='Example_lightgbm_python.model')
    # Return things - the names don't matter but one must be called thresh !
    return {
        "cat_feats": cat_feats,
        "model": model,
        "thresh": thresh
    }