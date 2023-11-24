with temp as 
(SELECT c.patientunitstayid, a.admitdxname 
FROM admissiondx a INNER JOIN mp_cohort c
ON a.patientunitstayid = c.patientunitstayid 
WHERE a.admitdxname like 'Seizures (primary-no structural brain disease)' and a.admitdxenteredoffset < 1440 and c.exclusion_non_adult = 0 AND c.exclusion_secondary_icu_stay =0 and exclusion_by_apache =0
order by patientunitstayid)
SELECT t.patientunitstayid, b.age, case b.unittype when 'Cardiac ICU' then 'CCU' when 'CCU-CTICU' then 'CCU' when 'CSICU' then 'CCU' when 'CTICU' then 'CCU' when 'Med-Surg ICU' then 'ICU' else b.unittype end as unittype,pa.hospitalid,pa.wardid, b.gender,i.ethnicity, b.hosp_mortality, b.icu_mortality, b.icu_los_hours,i.apache_iv, i.admissionheight, i.admissionweight,l.rdw, l.mch, l.mchc, l.aniongap, 
l.albumin, l.bands, l.bicarbonate, l.hco3, l.bilirubin, l.creatinine, l.chloride,
l.glucose, l.hematocrit, l.hemoglobin, l.lactate, l.platelet, l.potassium, l.ptt, l.inr, l.pt, 
l.sodium, l.bun, l.wbc, l.alt, l.ast, cast(v.heartrate as INT) as hr, cast(v.nibp_systolic as INT) as sbp, cast(v.nibp_diastolic as INT) as dbp,
ch.charlson as charlson, mc.stroke2 as stroke_flag, mc.dementia1 as dementia_flag, mc.tia1 as tia_flag  
FROM temp t LEFT JOIN basic b
on t.patientunitstayid = b.patientunitstayid
LEFT JOIN mp_sofa s
on t.patientunitstayid = s.patientunitstayid
LEFT JOIN icustay_detail i
on t.patientunitstayid = i.patientunitstayid
LEFT JOIN patient pa
on t.patientunitstayid = pa.patientunitstayid
LEFT JOIN firstlabs l
ON b.patientunitstayid = l.patientunitstayid
left JOIN firstvitals v
ON b.patientunitstayid = v.patientunitstayid
LEFT JOIN charlson ch
on t.patientunitstayid = ch.patientunitstayid
left JOIN mp_charlson mc
on t.patientunitstayid = mc.patientunitstayid
where b.icu_los_hours >= 4 AND b.icu_mortality IS NOT NULL and b.age != '> 89' and b.gender IS NOT NULL AND b.hosp_mortality IS NOT NULL
order by t.patientunitstayid 
