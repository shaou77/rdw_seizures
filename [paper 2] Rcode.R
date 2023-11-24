library(tidyverse)
library(ggplot2)
library(readr)
library(purrr)
library(naniar)
library(gtsummary)
library(ggsci)
library(patchwork)
library(mlr3verse)
library(missForest)
library(Boruta)
library(iml)
library(eoffice)
library(viridis)
data <- read_csv("data.csv", col_types = cols(gender = col_factor(levels = c("F", "M")), hospital_expire_flag = col_factor(levels = c("0", "1"))))
cols_to_factor <- c(5,39,62:102)
data <- data %>% 
  mutate(dialysis_active = ifelse(is.na(dialysis_active), 0, dialysis_active),
         surtime_from_admit= ifelse(is.na(surtime_from_admit), 9999, surtime_from_admit),
         surtime_from_icu= ifelse(is.na(surtime_from_icu), 9999, surtime_from_icu),
         death_90day = ifelse(surtime_from_icu<=90,1,0)) %>%
         mutate(across(81:103, ~replace(., is.na(.), 0))) %>% 
         map_at(cols_to_factor, as.factor) %>% as.data.frame() 
data1 <- data[,-c(1,3,5,6,7,8,9,42,43,45,46,48,49,51,52,55,56,57,58,80:102)]
# 制作table1
data1 %>% tbl_summary(by = death_90day, missing = 'no', digits = list(all_continuous() ~ 2)) %>% 
  add_p() %>% modify_spanning_header(c("stat_1", "stat_2") ~ "**90 day mortality**")  %>% 
  add_stat_label()%>% add_overall()
# 绘图描述统计
p1 <- data1 %>% ggplot(aes(death_90day, gcs_min,fill= death_90day))+geom_boxplot(width=0.4)+scale_fill_lancet()+theme_bw()+ylab('The minimal GCS score')+ theme(legend.position = "none")
p2 <- data1 %>% ggplot(aes(death_90day, admission_age,fill= death_90day))+geom_boxplot(width=0.4)+scale_fill_lancet()+theme_bw()+ylab('Age')+ theme(legend.position = "none")
p3 <- ggplot(data1, aes(x = death_90day, fill = gender)) + geom_bar(position = "stack",width = 0.4) +theme_bw() +scale_fill_lancet()
p4 <- ggplot(data1,aes(x=as.factor(drug_count),fill=as.factor(drug_count)))+geom_bar(show.legend = FALSE)+xlab('The amout of antiepileptic drugs')+theme_bw() +scale_fill_lancet()
(p1/p2)|(p3/p4)
#缺失值
print(miss_var_summary(data1), n=20)
miss_summary <- miss_var_summary(data1)
cols_to_keep <- miss_summary %>%  filter(pct_miss <= 5) %>%  pull(variable) #大于5%的均去掉，共去掉了15个变量
data1_filter <- data1 %>% select(cols_to_keep)
#插补 ---- 使用学习器
# data1_filter <- data1_filter %>%   mutate(id = row_number())
# task_imp <- as_task_classif(data1_filter, target = 'death_90day',id='id')
# po_imp <-  po("imputelearner", lrn("regr.lightgbm"))
# task <-  po_imp$train(list(task_imp))[[1]]
#插补 ---- 使用missForest
data_imp <- missForest(data1_filter)$ximp
#变量筛选，包装法
# data_select <- as_task_classif(data_imp, target = 'death_90day',id='id')
# learner = lrn("classif.rpart",predict_type = "prob")
# instance = fselect(
#   method = 'exhaustive_search',
#   task = data_select,
#   learner = learner,
#   resampling = rsmp("cv", folds = 4),
#   measure = msr("classif.auc"),
#   term_evals = 10,
#   store_models = TRUE)
# instance$result
fsel <- Boruta(death_90day ~ ., data = data_imp, doTrace = 2)
fsel$finalDecision
data_c <-data_imp[,c(2,4,8,9,13,16,18,20,23,25,27,28,44,45,48)] 
task <- as_task_classif(data_c,target = 'death_90day',id='task')
po1 <- po('scale')
po2 <- po('yeojohnson')
# po3 <- po("encode", method ="one-hot")  
po4 <- po("colapply", applicator = as.numeric,affect_columns = selector_type("integer")) %>>%
  po("encodeimpact") %>>%  po("smote", K = 5, dup_size = 9)
po_dat <- po1 %>>% po2  %>>% po4 
task1 <- po_dat$train(task)[1][[1]]            
lrn_compare =lrns(c("classif.glmnet", "classif.kknn", "classif.nnet", "classif.ranger", "classif.svm","classif.lightgbm"),predict_type ="prob")
design =benchmark_grid(task1,lrn_compare,rsmps("cv",folds=4))
bmr = benchmark(design)
msrs_list =list(msr("classif.acc"), msr("classif.ce"),msr("classif.auc"),msr("classif.precision"),msr("classif.recall"),msr('classif.fbeta'),msr('classif.logloss'))  
result <- bmr$aggregate(msrs_list) 
msr_table <- result[,c(4,7:12)]            
msr_table
write.csv(msr_table,file = 'table2.csv')
f3 <- autoplot(bmr,measure = msr('classif.fbeta'))+scale_fill_lancet()+scale_color_lancet()
f4 <- autoplot(bmr,type='roc')
topptx(f3,filename = 'figure3a.pptx')
topptx(f4,filename = 'figure3b.pptx')
learner_final <-  lrn("classif.lightgbm")
search_space <-  ps(
  max_depth = p_int(lower = 3, upper = 5),
  num_leaves = p_int(lower = 2, upper = 30),
  bagging_fraction = p_dbl(lower = 0.8, upper = 1),
  feature_fraction = p_dbl(lower = 0.8, upper = 1),
  lambda_l1 = p_int(lower = 0, upper = 1000),
  lambda_l2 = p_int(lower = 0, upper = 1000))
at <-  auto_tuner(
  tuner = tnr("random_search"),
  learner = learner_final,
  search_space = search_space,
  resampling = rsmp("cv", folds = 4L), 
  measure = msr("classif.auc"),
  term_evals = 10)
set.seed(8177)
at$train(task1)
autoplot(at$tuning_instance, type = "performance")
learner_final$param_set$values <- at$tuning_result$learner_param_vals[[1]]
learner_final$train(task1)
##验证
eicu <- read_csv("eicu.csv", 
                 col_types = cols(severe_liver_disease = col_factor(levels = c("0",  "1")),
                                  malignant_cancer = col_factor(levels = c("0", "1")),
                                  death_90day = col_factor(levels = c("0", "1"))   ))

eicu <- eicu %>% 
  distinct(patientunitstayid, .keep_all = TRUE) %>% 
  filter(!is.na(death_90day)) %>% 
  select(-patientunitstayid) %>% 
  mutate(admission_age = ifelse(admission_age == ">89", "90", admission_age)) %>% 
  mutate(admission_age = ifelse(is.na(admission_age), NA, as.numeric(admission_age))) %>% 
  filter(admission_age >= 18)
print(miss_var_summary(eicu), n=20)
eicu_c <- missForest(eicu)$ximp
task_test <- as_task_classif(eicu_c, target = 'death_90day', id = 'task_test')
po3 <- po("colapply", applicator = as.numeric,affect_columns = selector_type("integer")) %>>%
  po("encodeimpact")
po_dat2 <- po1 %>>% po2%>>% po3
task2 <- po_dat2$train(task_test)[1][[1]]
learner_final$predict(task2)$score(msrs_list)
##解释
# data_sha <- task1$data()
# mod = Predictor$new(learner_final, data = data_sha, y = "death_90day")
# x <- data_sha[which(names(data_sha) != "death_90day")]
data_sha <- data_c
data_sha_new <- data_sha %>%
  pivot_longer(cols = c(malignant_cancer, severe_liver_disease), names_to = "var") %>%
  mutate(var = paste(var, value, sep = ".")) %>%
  select(-value) %>%
  pivot_wider(names_from = var, values_from = value)

mod = Predictor$new(learner_final, data = data_sha, y = "death_90day")
x <- data_sha[which(names(data_sha) != "death_90day")]
x.interest <- data.frame(data_sha[1, ])
shapley <- Shapley$new(mod, x.interest = x.interest)
x.interest2 <- data.frame(data_sha[22, ])
shapley2 <- Shapley$new(mod, x.interest = x.interest2)
plot(shapley,sort=TRUE)
plot(shapley2,sort=TRUE)

num_features = c("admission_age", "apsiii", "creatinine_mean","heart_rate_mean",
                 "hematocrit_mean","hemoglobin_mean","oasis","rdw_max",
                 "resp_rate_mean","temperature_mean","urineoutput","weight")
effect = FeatureEffects$new(mod)
plot(effect, features = num_features)
imp <-  FeatureImp$new(mod, loss = "ce",compare = 'difference')
p7 <- plot(effect, features = num_features)
topptx(p7,filename = 'figure5.pptx')
df_sorted <- imp$results[1:12,] %>% arrange(desc(importance)) %>%  mutate(feature = reorder(feature, importance))
p6 <- ggplot(df_sorted, aes(x = feature, y = importance, ymin = importance.05, ymax = importance.95, color= feature)) +
  geom_pointrange() + labs(x = "Feature", y = "Importance", title = "Feature Importance with Error Bars") +
  theme_minimal()+scale_fill_lancet()+coord_flip() +  guides(color = "none")
topptx(p6, filename = 'figure3.pptx')
p5 <- ggplot(data1,aes(x=as.factor(drug_count),fill=as.factor(death_90day)))+geom_bar(position = "stack")+xlab('The amount of antiepileptic drugs')+theme_bw() +scale_fill_lancet()
topptx(p5,filename = 'figure2.pptx')
