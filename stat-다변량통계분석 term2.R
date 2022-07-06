####다변량 term project2
library(MVT)
library(MVN)
library(dplyr)
library(qqplotr)
library(car)
library(multifluo)
library(chemometrics)
library(DMwR)
library(tidyr)
library(data.table)
library(corrplot)
library(factoextra)
library(FactoMineR)
library(ggplot2)
library(biotools)
library(nparcomp)
library(Hotelling)
library(ICSNP)

death <- read.csv(file = "D:/대학원/다변량/영유아사망/Infant and toddler death_raw.csv", header = TRUE)
death <- death %>% as.data.frame()
death %>% summary()

death_model <- death %>% select(pregnant_weeks,baby_weight,mother_age,Survival,mother_education,father_education)
death_model %>% summary()

death_model$pregnant_weeks <- death_model$pregnant_weeks %>% as.numeric()
death_model$baby_weight <- death_model$baby_weight %>% as.numeric()
death_model$mother_age <- death_model$mother_age %>% as.numeric()
death_model$Survival <- death_model$Survival %>% as.factor()
death_model$mother_education <- death_model$mother_education %>% as.factor()
death_model$father_education <- death_model$father_education %>% as.factor()

death_model <- death_model %>% as.data.frame()
death_model <- death_model %>% filter(mother_education == 4 | mother_education == 5 | mother_education == 6)
death_model <- death_model %>% filter(father_education == 4 | father_education == 5 | father_education == 6)

write.csv(death_model, file = "D:/death_model.csv", row.names=FALSE)

death_model_n <- death_model %>% select(pregnant_weeks,baby_weight,mother_age)

death_sur <- death_model %>% select(Survival)
death_sur <- death_sur$Survival %>% as.factor()

death_fedu <- death_model %>% select(father_education)
death_fedu <- death_fedu$father_education %>% as.factor()

death_medu <- death_model %>% select(mother_education)
death_medu <- death_medu$mother_education %>% as.factor()

death_sur %>% length()
death_fedu %>% length()
death_medu %>% length()

### outlier - mahalanobis
death_model_m <- death_model_n %>% Moutlier(quantile = 0.90)

death_model_cd <- death_model_m$md
death_model_rd <- death_model_m$rd
death_model_cut <- death_model_m$cutoff

death_model_cd <- death_model_cd %>% as.vector()
death_model_cd <- death_model_cd %>% unlist()
death_model_cd <- death_model_cd %>% as.numeric()

death_model_rd <- death_model_rd %>% as.vector()
death_model_rd <- death_model_rd %>% unlist()
death_model_rd <- death_model_rd %>% as.numeric()

death_model_cut <- death_model_cut %>% as.vector()
death_model_cut <- death_model_cut %>% unlist()
death_model_cut <- death_model_cut %>% as.numeric()


## classical Mahalanobis distance
outlier_cd <- c()

for(i in 1:length(death_model_cd)){
  if (death_model_cd[i] >= death_model_cut) {
    outlier_cdi <- i
    outlier_cd <- append(outlier_cd, outlier_cdi)}
}

death_model_cd <- death_model_n[-outlier_cd,]

death_model_cd_mvn <- mvn(death_model_cd, multivariatePlot = "qq")


## robust Mahalanobis distance
outlier_rd <- c()

for(i in 1:length(death_model_rd)){
  if (death_model_rd[i] >= death_model_cut) {
    outlier_rdi <- i
    outlier_rd <- append(outlier_rd, outlier_rdi)}
}

death_model_rd <- death_model_n[-outlier_rd,]

death_model_rd_mvn <- mvn(death_model_rd, multivariatePlot = "qq")

### normality
death_model_cd_mvn <- mvn(death_model_cd, multivariatePlot = "qq")
death_model_rd_mvn <- mvn(death_model_rd, multivariatePlot = "qq")

### homogenity
##survival
death_sur_cd <- death_sur[-outlier_cd]
death_sur_rd <- death_sur[-outlier_rd]
death_boxM_sur_cd <- boxM(death_model_cd,death_sur_cd)
death_boxM_sur_rd <- boxM(death_model_rd,death_sur_rd) # 공분산 만족

##father-education
death_fedu_cd <- death_fedu[-outlier_cd]
death_fedu_rd <- death_fedu[-outlier_rd]
death_boxM_fedu_cd <- boxM(death_model_cd,death_fedu_cd)
death_boxM_fedu_rd <- boxM(death_model_rd,death_fedu_rd)

##mother-education
death_medu_cd <- death_medu[-outlier_cd]
death_medu_rd <- death_medu[-outlier_rd]
death_boxM_medu_cd <- boxM(death_model_cd,death_medu_cd) # 공분산 만족
death_boxM_medu_rd <- boxM(death_model_rd,death_medu_rd)


### normality
death_model_cd_mvn <- mvn(death_model_cd, multivariatePlot = "qq")
death_model_rd_mvn <- mvn(death_model_rd, multivariatePlot = "qq") # 정규성 일부 만족


####correlation
death_cor <- death %>% select(pregnant_weeks,baby_weight,mother_age)

death_cor <- death_cor %>% cor()
death_corplot <- death_cor %>% corrplot()


### hotelling's T
death_cd <- cbind(death_model_cd,death_sur_cd,death_fedu_cd,death_medu_cd)
death_cd %>% summary()
death_cd <- death_cd %>% as.data.frame()

death_rd <- cbind(death_model_rd,death_sur_rd,death_fedu_rd,death_medu_rd)
death_rd %>% summary()
death_rd <- death_rd %>% as.data.frame()


### manova with covariate
## survival-cd
death_lm_sur1 <- lm(cbind(pregnant_weeks,baby_weight) ~ death_sur_cd + mother_age, data = death_cd)
death_lm_sur1 %>% summary.aov()

## father-edu-cd
death_lm_fedu1 <- lm(cbind(pregnant_weeks,baby_weight) ~ death_fedu_cd + mother_age, data = death_cd)
death_lm_fedu1 %>% summary.aov()

## mother-edu-cd
death_lm_medu1 <- lm(cbind(pregnant_weeks,baby_weight) ~ death_medu_cd + mother_age, data = death_cd)
death_lm_medu1 %>% summary.aov()


## survival-rd
death_lm_sur2 <- lm(cbind(pregnant_weeks,baby_weight) ~ death_sur_rd + mother_age, data = death_rd)
death_lm_sur2 %>% summary.aov()

## father-edu-rd
death_lm_fedu2 <- lm(cbind(pregnant_weeks,baby_weight) ~ death_fedu_rd + mother_age, data = death_rd)
death_lm_fedu2 %>% summary.aov()

## mother-edu-rd
death_lm_medu2 <- lm(cbind(pregnant_weeks,baby_weight) ~ death_medu_rd + mother_age, data = death_rd)
death_lm_medu2 %>% summary.aov()