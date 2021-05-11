library(readr)
library(pROC)
library(tidyverse)
library(tableone)
library(ggplot2)
library(mice)
library(PredictABEL)
library(survival)
library(survminer)
library(forestplot)
library(RColorBrewer)
library(ResourceSelection)
#载入数据
seizures <- read_csv("seizuresv3.csv", col_types = cols(age = col_integer(), 
                                                        apache_iv = col_integer(), gender = col_factor(levels = c("1", "2")), 
                                                        hosp_mortality = col_factor(levels = c("0","1")), icu_los_hours = col_integer(), 
                                                        icu_mortality = col_factor(levels = c("0","1")), patientunitstayid = col_integer()))
seizures$stroke_flag <- as.factor(seizures$stroke_flag)
seizures$dementia_flag <- as.factor(seizures$dementia_flag)
seizures$tia_flag <- as.factor(seizures$tia_flag)
#取rdw的完整值病例
seizures_rdw <- seizures[complete.cases(seizures$rdw),]
#转换为因子
seizures_rdw$unittype <- as.factor(seizures_rdw$unittype)
seizures_rdw$ethnicity <- as.factor(seizures_rdw$ethnicity)
#通过ROC曲线来获得最佳截点
roc_rdw <- roc(seizures_rdw$hosp_mortality,seizures_rdw$rdw)
plot(roc_rdw, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     max.auc.polygon=TRUE, print.thres=TRUE)
#得出最佳 截点14.65
#根据截点分组
rdw_group <- cut(seizures_rdw$rdw, breaks = c(-Inf,14.65,Inf),labels = c('normal','high') )
seizures_rdw <- cbind(seizures_rdw[,],rdw_group)
#清洗不合理身高体重
seizures_rdw[which(seizures_rdw$admissionheight<20),12] <- NA
seizures_rdw[which(seizures_rdw$admissionweight<20),13] <- NA
#计算BMI和MAP
seizures_rdw <- seizures_rdw %>% mutate(MAP = round((sbp+2*dbp)/3,1))
seizures_rdw <- seizures_rdw %>% mutate(BMI = round(admissionweight*10000/(admissionheight*admissionheight),2))
#计算tableone
vars <- c('age','gender','ethnicity',
          "apache_iv",'BMI','MAP','hr','rdw','mch','mchc','albumin','bun','creatinine',
          'glucose','wbc','hematocrit','hemoglobin','platelet','potassium','sodium','inr','lactate',
          'alt','ast','bilirubin','charlson','stroke_flag','dementia_flag','tia_flag')
table1 <- CreateTableOne(vars = vars, strata = c("hosp_mortality"), data = seizures_rdw, 
                         testExact = fisher.test, testNonNormal = kruskal.test)
p1<-print(table1, nonnormal=c('age',"apache_iv",'BMI','MAP',
                              'hr','rdw','mch','mchc','albumin','bun','creatinine','glucose','wbc',
                              'hematocrit','hemoglobin','platelet','potassium','sodium','inr','lactate','alt','ast','bilirubin','charlson'),
          argsExact=c('gender','ethnicity','stroke_flag','dementia_flag','tia_flag'))
tableall <- CreateTableOne(vars = vars, data = seizures_rdw)
p2<-print(tableall, nonnormal=c('age',"apache_iv",'BMI','MAP',
                                'hr','rdw','mch','mchc','albumin','bun','creatinine','glucose','wbc',
                                'hematocrit','hemoglobin','platelet','potassium','sodium','inr','lactate','alt','ast','bilirubin','charlson'),
          argsExact=c('gender','ethnicity','stroke_flag','dementia_flag','tia_flag'))
#保存table1
write.csv(p1,file = "table1v3.csv")
write.csv(p2,file = "tableallv3.csv")
#绘制ICU情况
ggplot(seizures_rdw,aes(unittype))+geom_bar(aes(fill=hosp_mortality))+ coord_flip()+ 
  labs(fill = 'Hospital Mortality')+scale_fill_discrete(labels=c("Alive", "Dead"))+
  scale_x_discrete(labels=c('CCU','ICU','MICU','NICU','SICU'), name='ICU type')
#绘制ROC
roc.list <- roc(hosp_mortality~rdw+apache_iv, data= seizures_rdw)
ggroc(roc.list, size = 1.2,alpha=.8)+guides(color=guide_legend(title = NULL))+
  scale_color_discrete(labels=c('RDW','APACHE IV scores'))+theme(legend.position=c(0.9,0.1))+
  theme(legend.key = element_blank())+xlab('Specificity')+ylab('Sensitivity')+geom_abline(slope = 1,intercept = 1,linetype=3)
#计算AUC
auc(hosp_mortality~rdw,data=seizures_rdw)
ci.auc(hosp_mortality~rdw,data=seizures_rdw)
auc(hosp_mortality~apache_iv,data=seizures_rdw)
ci.auc(hosp_mortality~apache_iv,data=seizures_rdw)
#计算model1
model1 <- glm(hosp_mortality~rdw_group+apache_iv,data=seizures_rdw,family = binomial)
hoslem.test(model1$y,fitted(model1))
#计算NRI和IDI
rdwtemp <- seizures_rdw[,c(8,11,14)]
temp <- as.numeric(rdwtemp[,1])
temp <- temp-1
rdwtemp <- cbind(rdwtemp,temp)
m1 <- glm(hosp_mortality ~ apache_iv,family = binomial('logit'),data = rdwtemp,x=TRUE)
m2 <- glm(hosp_mortality ~ apache_iv+rdw,family = binomial('logit'),data = rdwtemp,x=TRUE)
mm1 <- predRisk(m1)
mm2 <- predRisk(m2)
reclassification(data=rdwtemp,cOutcome = 4,predrisk1 = mm1,predrisk2 = mm2,cutoff=c(0,0.3,0.60,1))
#多重插补
missing.percent <- unlist(lapply(seizures_rdw, function(x) sum(is.na(x))))/nrow(seizures_rdw)
rdwmice <- seizures_rdw[,-c(1,2,3,4,5,7,9,10,12,13,14,15,17,18,19,20,21,22,23,25,26,27,28,31,32,33,34,35,36,37,38,39,40,41,43:45,47)]
imp <- mice(rdwmice,seed = 8177)
#手工循环做单变量分析，找出有意义变量，纳入多变量分析
fit <- with(imp,glm(hosp_mortality~charlson,family = binomial))
pooled <- pool(fit)
summary(pooled)
1.96*finalfit <- with(imp,glm(hosp_mortality~age+apache_iv+mchc+albumin+hematocrit+
                           hemoglobin+potassium+bun+spo2+rdw_group+MAP,family = binomial))
finalpooled <- pool(finalfit)
summary(finalpooled)
finalfit2 <- with(imp,glm(hosp_mortality~apache_iv+mchc+potassium+rdw_group+charlson,family = binomial))
finalpooled2 <- pool(finalfit2)
summary(finalpooled2)
seizureslos <- seizures_rdw[which(seizures_rdw$hosp_mortality==0),]
mm <- rep(1,1616)
seizureslos <- cbind(seizureslos,mm)
rdwfit <- survfit(Surv(icu_los_hours,mm)~rdw_group,data=seizureslos)
ggsurvplot(rdwfit,pval = TRUE, conf.int = TRUE,legend.labs=c('RDW≤13.95%','RDW>13.95%'),
           legend.title='Strata',ylab='Percentage of Patients Stay in ICU',xlab='Hours in ICU',
           xlim=c(0,200),break.x.by=50, risk.table = T)
#绘制森林图
rs_forest <- read.csv('or.csv',header = FALSE)
forestplot(labeltext = as.matrix(rs_forest[,1:2]), title="Odds Ratio",
           mean = rs_forest$V3,
           lower = rs_forest$V4, 
           upper = rs_forest$V5, 
           is.summary=c(T,F,F,F,F,F,F,F,F,F,F,F),
           zero = 1, 
           boxsize = 0.4, 
           lineheight = unit(8,'mm'),
           colgap = unit(2,'mm'),
           lwd.zero = 2,#设置参考线的粗细
           lwd.ci = 2,#设置区间估计线的粗细
           col=fpColors(box='#458B00',summary="#8B008B",lines = 'black',zero = '#7AC5CD'),
           xlab="The estimates",#设置x轴标签
           lwd.xaxis=2,#设置X轴线的粗细
           lty.ci = "solid",
           graph.pos = 2)
