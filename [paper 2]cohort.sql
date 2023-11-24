with cohort AS
(SELECT hadm_id  FROM mimiciv_hosp.diagnoses_icd 
where icd_code in ('34500','34501','34510','34511','34540','34541','34550','34551','34560','34561','34570','34571','34580','34581','34590','34591','64940','64941','64942','64943','64944','78033','F445   ','G40    ','G400   ','G4000  ','G40001 ','G40009 ','G4001  ','G40011 ','G40019 ','G401   ','G4010  ','G40101 ','G40109 ','G4011  ','G40111 ','G40119 ','G402   ','G4020  ','G40201 ','G40209 ','G4021  ','G40211 ','G40219 ','G403   ','G4030  ','G40301 ','G40309 ','G4031  ','G40311 ','G40319 ','G404   ','G4040  ','G40401 ','G40409 ','G4041  ','G40411 ','G40419 ','G405   ','G4050  ','G40501 ','G40509 ','G408   ','G4080  ','G40801 ','G40802 ','G40803 ','G40804 ','G40811 ','G40812 ','G40813 ','G40814 ','G4082  ','G40821 ','G40822 ','G40823 ','G40824 ','G4089  ','G409   ','G4090  ','G40901 ','G40909 ','G4091  ','G40911 ','G40919 ','G40A   ','G40A0  ','G40A01 ','G40A09 ','G40A1  ','G40A11 ','G40A19 ','G40B   ','G40B0  ','G40B01 ','G40B09 ','G40B1  ','G40B11 ','G40B19 ','G8384  ','R561   ')and seq_num = 1 ),--第一诊断的ICD编码
drug AS
(SELECT hadm_id,
       MAX(CASE WHEN LOWER(te.drug) = 'pregabalin' THEN 1 ELSE 0 END) AS pregabalin,
       MAX(CASE WHEN LOWER(te.drug) = 'rufinamide' THEN 1 ELSE 0 END) AS rufinamide,
       MAX(CASE WHEN LOWER(te.drug) = 'oxcarbazepine' THEN 1 ELSE 0 END) AS oxcarbazepine,
       MAX(CASE WHEN LOWER(te.drug) = 'eslicarbazepine' THEN 1 ELSE 0 END) AS eslicarbazepine,
       MAX(CASE WHEN LOWER(te.drug) = 'phenobarbital' THEN 1 ELSE 0 END) AS phenobarbital,
       MAX(CASE WHEN LOWER(te.drug) = 'carbamazepine' THEN 1 ELSE 0 END) AS carbamazepine,
       MAX(CASE WHEN LOWER(te.drug) = 'brivaracetam' THEN 1 ELSE 0 END) AS brivaracetam,
       MAX(CASE WHEN LOWER(te.drug) = 'clobazam' THEN 1 ELSE 0 END) AS clobazam,
       MAX(CASE WHEN LOWER(te.drug) = 'ethosuximide' THEN 1 ELSE 0 END) AS ethosuximide,
       MAX(CASE WHEN LOWER(te.drug) = 'felbamate' THEN 1 ELSE 0 END) AS felbamate,
       MAX(CASE WHEN LOWER(te.drug) = 'fosphenytoin' THEN 1 ELSE 0 END) AS fosphenytoin,
       MAX(CASE WHEN LOWER(te.drug) = 'gabapentin' THEN 1 ELSE 0 END) AS gabapentin,
       MAX(CASE WHEN LOWER(te.drug) = 'levetiracetam' THEN 1 ELSE 0 END) AS levetiracetam,
       MAX(CASE WHEN LOWER(te.drug) = 'lacosamide' THEN 1 ELSE 0 END) AS lacosamide,
       MAX(CASE WHEN LOWER(te.drug) = 'lamotrigine' THEN 1 ELSE 0 END) AS lamotrigine,
       MAX(CASE WHEN LOWER(te.drug) = 'perampanel' THEN 1 ELSE 0 END) AS perampanel,
       MAX(CASE WHEN LOWER(te.drug) = 'phenytoin' THEN 1 ELSE 0 END) AS phenytoin,
       MAX(CASE WHEN LOWER(te.drug) = 'tiagabine' THEN 1 ELSE 0 END) AS tiagabine,
       MAX(CASE WHEN LOWER(te.drug) = 'topiramate' THEN 1 ELSE 0 END) AS topiramate,
       MAX(CASE WHEN LOWER(te.drug) LIKE 'valpro%' THEN 1 ELSE 0 END) AS valproic,
       MAX(CASE WHEN LOWER(te.drug) = 'vigabatrin' THEN 1 ELSE 0 END) AS vigabatrin,
       MAX(CASE WHEN LOWER(te.drug) = 'zonisamide' THEN 1 ELSE 0 END) AS zonisamide
FROM (SELECT p.* FROM mimiciv_hosp.prescriptions p INNER JOIN cohort C
ON p.hadm_id = c.hadm_id) te
WHERE LOWER(te.drug) IN ('pregabalin', 'rufinamide', 'oxcarbazepine', 'eslicarbazepine', 'phenobarbital', 'carbamazepine', 'brivaracetam', 'clobazam', 'ethosuximide', 'felbamate', 'fosphenytoin', 'gabapentin', 'levetiracetam', 'lacosamide', 'lamotrigine', 'perampanel', 'phenytoin', 'tiagabine', 'topiramate', 'valproic', 'vigabatrin', 'zonisamide')
GROUP BY te.hadm_id) 
SELECT i.subject_id,i.hadm_id,i.stay_id,i.gender,i.los_hospital,i.admission_age,i.race,i.hospital_expire_flag,i.los_icu,
       EXTRACT(DAY FROM (dod - admittime)) AS surtime_from_admit,    --入院起的生存时间
       EXTRACT(DAY FROM (dod - icu_intime)) AS surtime_from_icu,     --入ICU起的生存时间
			 b.lactate_max, b.ph_min, b.ph_max, b.po2_min,b.pco2_max,b.pao2fio2ratio_min, gc.gcs_min, fh.height, 
			 fw.weight,(fl.hematocrit_min+fl.hematocrit_max)/2 as hematocrit_mean, (fl.hemoglobin_min+fl.hemoglobin_max)/2 as hemoglobin_mean, 
			 (fl.platelets_min+fl.platelets_max)/2 as platelets_mean, (fl.wbc_min+fl.wbc_max)/2 as wbc_mean,
			 (fl.albumin_min+fl.albumin_max)/2 as albumin_mean,(fl.aniongap_min+fl.aniongap_max)/2 as aniongap_mean, 
			 (fl.bicarbonate_min+fl.bicarbonate_max)/2 as bicarbonate_mean, (fl.bun_min+fl.bun_max)/2 as bun_mean,
			 (fl.calcium_min+fl.calcium_max)/2 as calcium_mean, (fl.chloride_min+fl.chloride_max)/2 as chloride_mean,
			 (fl.creatinine_min+fl.creatinine_max)/2 as creatinine_mean,
			 (fl.sodium_min+fl.sodium_max)/2 as sodium_mean,(fl.potassium_min+fl.potassium_max)/2 as potassium_mean,
			 (fl.abs_lymphocytes_min+fl.abs_lymphocytes_max)/2 as abs_lymphocytes_mean,(fl.pt_min+fl.pt_max)/2 as pt_mean,
			 (fl.ptt_min+fl.ptt_max)/2 as ptt_mean,(fl.alt_min+fl.alt_max)/2 as alt_mean,(fl.ast_min+fl.ast_max)/2 as ast_mean,
			 (fl.bilirubin_total_min+fl.bilirubin_total_max)/2 as bilirubin_total_mean,(fl.ck_cpk_min+fl.ck_cpk_max)/2 as ck_cpk_mean,
			 fr.dialysis_active,    --第一天是否启动rrt
			 fs.sofa, fo.urineoutput,
			 fv.heart_rate_min, fv.heart_rate_max, fv.heart_rate_mean,fv.mbp_min,fv.mbp_max,fv.mbp_mean, fv.resp_rate_min, fv.resp_rate_max,
			 fv.resp_rate_mean, fv.temperature_min,fv.temperature_max,fv.temperature_mean,fv.spo2_min, fv.spo2_max, fv.spo2_mean,
			 fv.glucose_min, fv.glucose_max, fv.glucose_mean, oa.oasis, ap.apsiii,
			 CASE WHEN ve.ventilation = 1 THEN 1 ELSE 0 END  as ventilation, --第一天是否启动机械通气
			 CASE WHEN va.vasoactive = 1 THEN 1 ELSE 0 END  as vasoactive,     --第一天是否启动血管活性药物
			 CASE WHEN ch.myocardial_infarct > 0 THEN 1 ELSE 0 END  as myocardial_infarct, --以下均为既往史
			 CASE WHEN ch.congestive_heart_failure > 0 THEN 1 ELSE 0 END  as congestive_heart_failure,
			 CASE WHEN ch.peripheral_vascular_disease > 0 THEN 1 ELSE 0 END  as peripheral_vascular_disease,
			 CASE WHEN ch.cerebrovascular_disease > 0 THEN 1 ELSE 0 END  as cerebrovascular_disease,
			 CASE WHEN ch.dementia > 0 THEN 1 ELSE 0 END  as dementia,
       CASE WHEN ch.chronic_pulmonary_disease > 0 THEN 1 ELSE 0 END  as chronic_pulmonary_disease,
			 CASE WHEN ch.rheumatic_disease > 0 THEN 1 ELSE 0 END  as rheumatic_disease,
			 CASE WHEN ch.peptic_ulcer_disease > 0 THEN 1 ELSE 0 END  as peptic_ulcer_disease,
			 CASE WHEN ch.mild_liver_disease > 0 THEN 1 ELSE 0 END  as mild_liver_disease,
			 CASE WHEN ch.diabetes_with_cc > 0 THEN 1 ELSE 0 END  as diabetes_with_cc,
			 CASE WHEN ch.diabetes_without_cc > 0 THEN 1 ELSE 0 END  as diabetes_without_cc,
			 CASE WHEN ch.paraplegia > 0 THEN 1 ELSE 0 END  as paraplegia,
			 CASE WHEN ch.renal_disease > 0 THEN 1 ELSE 0 END  as renal_disease,
			 CASE WHEN ch.malignant_cancer > 0 THEN 1 ELSE 0 END  as malignant_cancer,
			 CASE WHEN ch.severe_liver_disease > 0 THEN 1 ELSE 0 END  as severe_liver_disease,
			 CASE WHEN ch.metastatic_solid_tumor > 0 THEN 1 ELSE 0 END  as metastatic_solid_tumor,
			 CASE WHEN ch.aids > 0 THEN 1 ELSE 0 END  as aids, 
			 dr.pregabalin, dr.rufinamide, dr.oxcarbazepine, dr.eslicarbazepine, dr.phenobarbital, dr.carbamazepine, dr.brivaracetam, dr.clobazam, dr.ethosuximide, dr.felbamate, dr.fosphenytoin, dr.gabapentin, dr.levetiracetam, dr.lacosamide, dr.lamotrigine, dr.perampanel, dr.phenytoin, dr.tiagabine, dr.topiramate, dr.valproic, dr.vigabatrin, dr.zonisamide, 
			 (dr.pregabalin+ dr.rufinamide+ dr.oxcarbazepine+ dr.eslicarbazepine+ dr.phenobarbital+ dr.carbamazepine+ dr.brivaracetam+ dr.clobazam+ dr.ethosuximide+ dr.felbamate+ dr.fosphenytoin+ dr.gabapentin+ dr.levetiracetam+ dr.lacosamide+ dr.lamotrigine+ dr.perampanel+ dr.phenytoin+ dr.tiagabine+ dr.topiramate+ dr.valproic+ dr.vigabatrin+ dr.zonisamide) as drug_count, fl.rdw_max, fl.ntprobnp_max
FROM mimiciv_derived.icustay_detail i 
RIGHT JOIN cohort c
ON i.hadm_id = c.hadm_id
LEFT JOIN first_day_bg_art b
ON i.stay_id = b.stay_id
LEFT JOIN first_day_gcs gc
ON i.stay_id = gc.stay_id
LEFT JOIN first_day_height fh
ON i.stay_id = fh.stay_id
LEFT JOIN first_day_weight fw
ON i.stay_id = fw.stay_id
LEFT JOIN first_day_lab fl
ON i.stay_id = fl.stay_id
LEFT JOIN first_day_rrt fr
ON i.stay_id = fr.stay_id
LEFT JOIN first_day_sofa fs
ON i.stay_id = fs.stay_id
LEFT JOIN first_day_urine_output fo
ON i.stay_id = fo.stay_id
LEFT JOIN first_day_vitalsign fv
ON i.stay_id = fv.stay_id
LEFT JOIN apsiii ap
ON i.stay_id = ap.stay_id
LEFT JOIN first_day_vasoactive va
ON i.stay_id = va.stay_id
LEFT JOIN first_day_ventilation ve
ON i.stay_id = ve.stay_id
LEFT JOIN oasis oa
ON i.stay_id = oa.stay_id
LEFT JOIN charlson ch
ON i.hadm_id = ch.hadm_id
LEFT JOIN drug dr
ON i.hadm_id = dr.hadm_id
WHERE i.first_icu_stay = 't' AND i.los_icu >= 1   --筛选条件，第一次入ICU，ICU时间大于等于1天
ORDER BY subject_id
