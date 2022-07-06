####다변량 term project
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

death <- read.csv(file = "D:/대학원/다변량/영유아사망/Infant and toddler death_raw.csv", header = TRUE)
death <- death %>% as.data.frame()
death %>% summary()

death_model <- death %>% select(Father_age,mother_age,parents_marriage_year,pregnant_weeks,baby_weight)

death_model$Father_age <- death_model$Father_age %>% as.numeric()
death_model$mother_age <- death_model$mother_age %>% as.numeric()
death_model$parents_marriage_year <- death_model$parents_marriage_year %>% as.numeric()
death_model$pregnant_weeks <- death_model$pregnant_weeks %>% as.numeric()
death_model$baby_weight <- death_model$baby_weight %>% as.numeric()

death_model <- death_model %>% as.data.frame()


death_sur <- death %>% select(Survival)
death_sur <- death_sur$Survival %>% as.factor()

death_fedu <- death %>% select(father_education)
death_fedu <- death_fedu$father_education %>% as.factor()

death_fedu <- death %>% select(father_education)
death_fedu <- death_fedu$father_education %>% as.factor()


### homogenity
death_boxM <- boxM(death_model,death_sur)

### normality
death_mvn <- mvn(death_model, multivariatePlot = "qq")

### hotelling's T
library(Hotelling)
death_ht <- hotelling.test(.~death_model, data=)

death_ht %>% summary()
death_ht %>% plot()

### manova with covariate
## father-edu
death_lm1 <- manova(cbind(Father_age,mother_age,parents_marriage_year,pregnant_weeks) ~ father_education + baby_weight, data = death)
death_lm1 %>% summary.aov()


## mother-edu
death_lm2 <- lm(cbind(Father_age,mother_age,parents_marriage_year,pregnant_weeks) ~ mother_education + baby_weight, death)
death_lm2 %>% summary.aov()






### outlier - mahalanobis
death_model_m <- death_model %>% Moutlier(quantile = 0.95)

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

outlier제거_cd <- death_model[-outlier_cd,]

outlier제거_cd_mvn <- mvn(outlier제거_cd, multivariatePlot = "qq")


## robust Mahalanobis distance
outlier_rd <- c()

for(i in 1:length(death_model_rd)){
  if (death_model_rd[i] >= death_model_cut) {
    outlier_rdi <- i
    outlier_rd <- append(outlier_rd, outlier_rdi)}
}

outlier제거_rd <- death_model[-outlier_rd,]

outlier제거_rd_mvn <- mvn(outlier제거_rd, multivariatePlot = "qq")



## Local Outlier Factor
death_model_LOF <- death_model %>% lofactor(k = 5) 
# K : The number of neighbours that will be used in the calculation of the local outlier factors.

death_model_LOF_density <- death_model_LOF %>% density()
death_model_LOF_plot <- death_model_LOF_density %>% plot(main = "Local Outlier Factor score")
death_model_LOF_sort <- death_model_LOF %>% sort(decreasing = TRUE)
death_model_LOF_sort %>% head()

outlier_LOF <- c()

for(i in 1:length(death_model_LOF)){
  if (death_model_LOF[i] >= 3) {
    outlier_LOFi <- i
    outlier_LOF <- append(outlier_LOF, outlier_LOFi)}
}

outlier제거_LOF <- death_model[-outlier_LOF,]

outlier제거_LOF_mvn <- mvn(outlier제거_LOF, multivariatePlot = "qq")



### Q-Q plot
## Father_age
Father_age_df <- data.frame(Father_age_score = outlier제거_rd$Father_age)

Father_age_qqplot2 <- ggplot(data = Father_age_df , mapping = aes(sample = Father_age_score)) +
  ggtitle("Q-Q plot-ggplotr-Father_age") +
  geom_qq_band(bandType = "pointwise", mapping = aes(x = Father_age_score, fill = "Normal"), alpha = 0.5) +
  geom_qq_band(bandType = "boot", mapping = aes(x = Father_age_score, fill = "Bootstrap"), alpha = 0.5) +
  geom_qq_band(bandType = "ks", mapping = aes(x = Father_age_score, fill = "KS"), alpha = 0.5) +
  geom_qq_band(bandType = "ts", mapping = aes(x = Father_age_score, fill = "TS"), alpha = 0.5) +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  scale_fill_discrete("Bandtype")

## mother_age
mother_age_df <- data.frame(mother_age_score = outlier제거_rd$mother_age)

mother_age_qqplot2 <- ggplot(data = mother_age_df , mapping = aes(sample = mother_age_score)) +
  ggtitle("Q-Q plot-ggplotr-mother_age") +
  geom_qq_band(bandType = "pointwise", mapping = aes(x = mother_age_score, fill = "Normal"), alpha = 0.5) +
  geom_qq_band(bandType = "boot", mapping = aes(x = mother_age_score, fill = "Bootstrap"), alpha = 0.5) +
  geom_qq_band(bandType = "ks", mapping = aes(x = mother_age_score, fill = "KS"), alpha = 0.5) +
  geom_qq_band(bandType = "ts", mapping = aes(x = mother_age_score, fill = "TS"), alpha = 0.5) +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  scale_fill_discrete("Bandtype")

## parents_marriage_year
parents_marriage_year_df <- data.frame(parents_marriage_year_score = outlier제거_rd$parents_marriage_year)

parents_marriage_year_qqplot2 <- ggplot(data = parents_marriage_year_df , mapping = aes(sample = parents_marriage_year_score)) +
  ggtitle("Q-Q plot-ggplotr-parents_marriage_year") +
  geom_qq_band(bandType = "pointwise", mapping = aes(x = parents_marriage_year_score, fill = "Normal"), alpha = 0.5) +
  geom_qq_band(bandType = "boot", mapping = aes(x = parents_marriage_year_score, fill = "Bootstrap"), alpha = 0.5) +
  geom_qq_band(bandType = "ks", mapping = aes(x = parents_marriage_year_score, fill = "KS"), alpha = 0.5) +
  geom_qq_band(bandType = "ts", mapping = aes(x = parents_marriage_year_score, fill = "TS"), alpha = 0.5) +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  scale_fill_discrete("Bandtype")

## pregnant_weeks
pregnant_weeks_df <- data.frame(pregnant_weeks_score = outlier제거_rd$pregnant_weeks)

pregnant_weeks_qqplot2 <- ggplot(data = pregnant_weeks_df , mapping = aes(sample = pregnant_weeks_score)) +
  ggtitle("Q-Q plot-ggplotr-pregnant_weeks") +
  geom_qq_band(bandType = "pointwise", mapping = aes(x = pregnant_weeks_score, fill = "Normal"), alpha = 0.5) +
  geom_qq_band(bandType = "boot", mapping = aes(x = pregnant_weeks_score, fill = "Bootstrap"), alpha = 0.5) +
  geom_qq_band(bandType = "ks", mapping = aes(x = pregnant_weeks_score, fill = "KS"), alpha = 0.5) +
  geom_qq_band(bandType = "ts", mapping = aes(x = pregnant_weeks_score, fill = "TS"), alpha = 0.5) +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  scale_fill_discrete("Bandtype")





### 주성분 분석
##변수간 상관계수
death_model_cor <- death_model %>% cor()
death_model_corplot <- death_model_cor %>% corrplot()

##주성분 분석
death_model_pca <- death_model %>% PCA(graph = FALSE)
death_model_pca %>% summary()

# comp의 변동
death_model_pca_comp <- death_model_pca$eig

# screeplot
death_model_scree <- death_model_pca %>% fviz_screeplot()


## 주성분 Biplot
#biplot1
death_model %>% PCA()

#biplot2
death_model_biplot2 <- death_model_pca %>% fviz_pca_var(col.var="contrib",
                                            gradient.cols = c("#a18b6b", "#002554"),
                                            repel = TRUE)
#biplot3
death_model_biplot3 <- death_model_pca %>% fviz_pca_biplot(repel = FALSE)


## 주성분 가중치
death_model_coord <- death_model_pca$var$coord
death_model_coord %>% View()

## 주성분 정규성 검정
death_model_coord_df <- death_model_coord %>% data.frame()

death_model_coord_Dim1 <- death_model_coord_df$Dim.1
death_model_coord_Dim2 <- death_model_coord_df$Dim.2

# Dim 1
death_model_coord_Dim1 %>% shapiro.test()

# Dim 2
death_model_coord_Dim2 %>% shapiro.test()
