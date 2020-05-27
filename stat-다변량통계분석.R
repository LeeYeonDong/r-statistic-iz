###### 다변량 통계분석 HW

library(MVT)
library(MVN)
library(dplyr)
library(qqplotr)
library(car)
library(multifluo)

##### 2.6.7
data(examScor)
examScor <- examScor %>% na.omit()

examScor %>% head()
examScor %>% summary()

####과목별 분리
메카 <- examScor %>% select(mechanics)
벡터 <- examScor %>% select(vectors)
대수 <- examScor %>% select(algebra)
분석 <- examScor %>% select(analysis)
통계 <- examScor %>% select(statistics)

메카 %>% class()

#### numeric변환
### mechanics
메카_vector <- 메카 %>% as.vector()
메카_vector <- 메카_vector %>% unlist()
메카_num <- 메카_vector %>% as.numeric()

### vectors
벡터_vector <- 벡터 %>% as.vector()
벡터_vector <- 벡터_vector %>% unlist()
벡터_num <- 벡터_vector %>% as.numeric()

### algebra
대수_vector <- 대수 %>% as.vector()
대수_vector <- 대수_vector %>% unlist()
대수_num <- 대수_vector %>% as.numeric()

### analysis
분석_vector <- 분석 %>% as.vector()
분석_vector <- 분석_vector %>% unlist()
분석_num <- 분석_vector %>% as.numeric()

### statistics
통계_vector <- 통계 %>% as.vector()
통계_vector <- 통계_vector %>% unlist()
통계_num <- 통계_vector %>% as.numeric()


### (1)
## mechanics
# Q-Q plot (1)
n <- 메카_num %>% length()
p <- (1:n)/(n+1)
q <- qnorm(p, mean(메카_num), sd(메카_num))

sort_q <- q %>% sort()
메카_sort <- 메카_num %>% sort()

메카_QQ <- plot(sort_q, 메카_sort, xlab = "Quantiles from Normal Distribution", ylab = "Sample Quantiles", main = "Q-Q plot(1)-mechanics")
abline(0,1,col="Red")

cor(sort_q, 메카_sort)

# Q-Q plot (2)
메카_norm <- qqnorm(메카_num, pch = 1, main = "Q-Q plot(2)-mechanics")
qqline(메카_num, col = "Blue", lwd = 2)

메카_sort_norm_x <- 메카_norm$x %>% sort()
메카_sort_norm_y <- 메카_norm$y %>% sort()

cor(메카_sort_norm_x, 메카_sort_norm_y)

# Q-Q plot (3)
메카_qqplot <- 메카_num %>% qqPlot(main = "Q-Q plot(3)-mechanics")


# 참고 : Q-Q plot - ggplotr
메카_df <- data.frame(메카_score = 메카_num)

메카_qqplot2 <- ggplot(data = 메카_df , mapping = aes(sample = 메카_score)) +
  ggtitle("Q-Q plot-ggplotr-mechanics") +
  geom_qq_band(bandType = "pointwise", mapping = aes(x = 메카_score, fill = "Normal"), alpha = 0.5) +
  geom_qq_band(bandType = "boot", mapping = aes(x = 메카_score, fill = "Bootstrap"), alpha = 0.5) +
  geom_qq_band(bandType = "ks", mapping = aes(x = 메카_score, fill = "KS"), alpha = 0.5) +
  geom_qq_band(bandType = "ts", mapping = aes(x = 메카_score, fill = "TS"), alpha = 0.5) +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  scale_fill_discrete("Bandtype")


# K-S Test
ks.test(메카_num, pnorm, mean(메카_num), sd(메카_num), alternative = "two.sided")

# Shapiro Test
shapiro.test(메카_num)



## vectors
# Q-Q plot (1)
n <- 벡터_num %>% length()
p <- (1:n)/(n+1)
q <- qnorm(p, mean(벡터_num), sd(벡터_num))

sort_q <- q %>% sort()
벡터_sort <- 벡터_num %>% sort()

벡터_QQ <- plot(sort_q, 벡터_sort, xlab = "Quantiles from Normal Distribution", ylab = "Sample Quantiles", main = "Q-Q plot(1)-vectors")
abline(0,1,col="Red")

cor(sort_q, 벡터_sort)

# Q-Q plot (2)
벡터_norm <- qqnorm(벡터_num, pch = 1, main = "Q-Q plot(2)-vectors")
qqline(벡터_num, col = "Blue", lwd = 2)

벡터_sort_norm_x <- 벡터_norm$x %>% sort()
벡터_sort_norm_y <- 벡터_norm$y %>% sort()

cor(벡터_sort_norm_x, 벡터_sort_norm_y)

# Q-Q plot (3)
벡터_qqplot <- 벡터_num %>% qqPlot(main = "Q-Q Normal Q-Q plot(3)-vectors")

# 참고 : Q-Q plot - ggplotr
벡터_df <- data.frame(벡터_score = 벡터_num)

벡터_qqplot2 <- ggplot(data = 벡터_df , mapping = aes(sample = 벡터_score)) +
  ggtitle("Q-Q plot-ggplotr-vectors") +
  geom_qq_band(bandType = "ks", mapping = aes(x = 벡터_score, fill = "KS"), alpha = 0.5) +
  geom_qq_band(bandType = "ts", mapping = aes(x = 벡터_score, fill = "TS"), alpha = 0.5) +
  geom_qq_band(bandType = "pointwise", mapping = aes(x = 벡터_score, fill = "Normal"), alpha = 0.5) +
  geom_qq_band(bandType = "boot", mapping = aes(x = 벡터_score, fill = "Bootstrap"), alpha = 0.5) +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  scale_fill_discrete("Bandtype")


# K-S Test
ks.test(벡터_num, pnorm, mean(벡터_num), sd(벡터_num), alternative = "two.sided")

# Shapiro Test
shapiro.test(벡터_num)



## algebra
# Q-Q plot (1)
n <- 대수_num %>% length()
p <- (1:n)/(n+1)
q <- qnorm(p, mean(대수_num), sd(대수_num))

sort_q <- q %>% sort()
대수_sort <- 대수_num %>% sort()

대수_QQ <- plot(sort_q, 대수_sort, xlab = "Quantiles from Normal Distribution", ylab = "Sample Quantiles", main = "Q-Q plot(1)-algebra")
abline(0,1,col="Red")

cor(sort_q, 대수_sort)

# Q-Q plot (2)
대수_norm <- qqnorm(대수_num, pch = 1, main = "Q-Q plot(2)-algebra")
qqline(대수_num, col = "Blue", lwd = 2)

대수_sort_norm_x <- 대수_norm$x %>% sort()
대수_sort_norm_y <- 대수_norm$y %>% sort()

cor(대수_sort_norm_x,대수_sort_norm_y)

# Q-Q plot (3)
대수_qqplot <- 대수_num %>% qqPlot(main = "Q-Q plot(3)-algebra")

# 참고 : Q-Q plot - ggplotr
대수_df <- data.frame(대수_score = 대수_num)

대수_qqplot2 <- ggplot(data = 대수_df , mapping = aes(sample = 대수_score)) +
  ggtitle("Q-Q plot-ggplotr-algebra") +
  geom_qq_band(bandType = "ks", mapping = aes(x = 대수_score, fill = "KS"), alpha = 0.5) +
  geom_qq_band(bandType = "ts", mapping = aes(x = 대수_score, fill = "TS"), alpha = 0.5) +
  geom_qq_band(bandType = "pointwise", mapping = aes(x = 대수_score, fill = "Normal"), alpha = 0.5) +
  geom_qq_band(bandType = "boot", mapping = aes(x = 대수_score, fill = "Bootstrap"), alpha = 0.5) +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  scale_fill_discrete("Bandtype")

# K-S Test
ks.test(대수_num, pnorm, mean(대수_num), sd(대수_num), alternative = "two.sided")

# Shapiro Test
shapiro.test(대수_num)


## analysis
# Q-Q plot (1)
n <- 분석_num %>% length()
p <- (1:n)/(n+1)
q <- qnorm(p, mean(분석_num), sd(분석_num))

sort_q <- q %>% sort()
분석_sort <- 분석_num %>% sort()

분석_QQ <- plot(sort_q, 분석_sort, xlab = "Quantiles from Normal Distribution", ylab = "Sample Quantiles", main = "Q-Q plot(1)-analysis")
abline(0,1,col="Red")

cor(sort_q, 분석_sort)

# Q-Q plot (2)
분석_norm <- qqnorm(분석_num, pch = 1, main = "Q-Q plot(2)-analysis")
qqline(분석_num, col = "Blue", lwd = 2)

분석_sort_norm_x <- 분석_norm$x %>% sort()
분석_sort_norm_y <- 분석_norm$y %>% sort()

cor(분석_sort_norm_x,분석_sort_norm_y)

# Q-Q plot (3)
분석_qqplot <- 분석_num %>% qqPlot(main = "Q-Q plot(3)-analysis")

# 참고 : Q-Q plot - ggplotr
분석_df <- data.frame(분석_score = 분석_num)

분석_qqplot2 <- ggplot(data = 분석_df , mapping = aes(sample = 분석_score)) +
  ggtitle("Q-Q plot-ggplotr-analysis") +
  geom_qq_band(bandType = "ks", mapping = aes(x = 분석_score, fill = "KS"), alpha = 0.5) +
  geom_qq_band(bandType = "ts", mapping = aes(x = 분석_score, fill = "TS"), alpha = 0.5) +
  geom_qq_band(bandType = "pointwise", mapping = aes(x = 분석_score, fill = "Normal"), alpha = 0.5) +
  geom_qq_band(bandType = "boot", mapping = aes(x = 분석_score, fill = "Bootstrap"), alpha = 0.5) +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  scale_fill_discrete("Bandtype")

# K-S Test
ks.test(분석_num, pnorm, mean(분석_num), sd(분석_num), alternative = "two.sided")

# Shapiro Test
shapiro.test(분석_num)



## statistics
# Q-Q plot (1)
n <- 통계_num %>% length()
p <- (1:n)/(n+1)
q <- qnorm(p, mean(통계_num), sd(통계_num))

sort_q <- q %>% sort()
통계_sort <- 통계_num %>% sort()

통계_QQ <- plot(sort_q, 통계_sort, xlab = "Quantiles from Normal Distribution", ylab = "Sample Quantiles", main = "Q-Q plot(1)-statistics")
abline(0,1,col="Red")

cor(sort_q, 통계_sort)

# Q-Q plot (2)
통계_norm <- qqnorm(통계_num, pch = 1, main = "Q-Q plot(2)-statistics")
qqline(통계_num, col = "Blue", lwd = 2)

통계_sort_norm_x <- 통계_norm$x %>% sort()
통계_sort_norm_y <- 통계_norm$y %>% sort()

cor(통계_sort_norm_x,통계_sort_norm_y)

# Q-Q plot (3)
통계_qqplot <- 통계_num %>% qqPlot(main = "Q-Q Normal Q-Q plot(3)-statistics")

# 참고 : Q-Q plot - ggplotr
통계_df <- data.frame(통계_score = 통계_num)

통계_qqplot2 <- ggplot(data = 통계_df , mapping = aes(sample = 통계_score)) +
  ggtitle("Q-Q plot-ggplotr-statistics") +
  geom_qq_band(bandType = "ks", mapping = aes(x = 통계_score, fill = "KS"), alpha = 0.5) +
  geom_qq_band(bandType = "ts", mapping = aes(x = 통계_score, fill = "TS"), alpha = 0.5) +
  geom_qq_band(bandType = "pointwise", mapping = aes(x = 통계_score, fill = "Normal"), alpha = 0.5) +
  geom_qq_band(bandType = "boot", mapping = aes(x = 통계_score, fill = "Bootstrap"), alpha = 0.5) +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  scale_fill_discrete("Bandtype")

# K-S Test
ks.test(통계_num, pnorm, mean(통계_num), sd(통계_num), alternative = "two.sided")

# Shapiro Test
shapiro.test(통계_num)




### (2) 두 가지 방식 ( C )와 ( O )에 따라 이변량 정규분포와 삼변량 정규분포를 검토하라
## mechanics + vectors
메카.벡터 <- examScor %>% select(mechanics, vectors)

# mvn
메카.벡터_mvn <- mvn(메카.벡터, multivariatePlot = "qq")

## algebra + analysis + statistics
대수.분석.통계 <- examScor %>% select(algebra, analysis, statistics)

# mvn
대수.분석.통계_mvn <- mvn(대수.분석.통계, multivariatePlot = "qq")


### (3) 다변량 정규성을 만족하는지를 시각적 방법과 다양한 검정 방법으로 검토하라

# mvn
메카.벡터_mvn <- mvn(메카.벡터, multivariatePlot = "qq")

# mvn
대수.분석.통계_mvn <- mvn(대수.분석.통계, multivariatePlot = "qq")

# srivastava test-두 비교대상이 paired 할때 적용가능
메카.벡터 <- examScor %>% select(mechanics, vectors)
메카.벡터_mat <- 메카.벡터 %>% as.matrix()

대수.분석 <- examScor %>% select(algebra, analysis)
대수.분석_mat <- 대수.분석 %>% as.matrix()

sri.test(메카.벡터_mat,대수.분석_mat) 


### (4) 다변량 정규성에 영향을 미치는 이상치를 검토하라

## mahalanobis
library(chemometrics)

examScor_outlier_m <- examScor %>% Moutlier(quantile = 0.99)

examScor_outlier_md <- examScor_outlier_m$md
examScor_outlier_rd <- examScor_outlier_m$rd
examScor_outlier_cut <- examScor_outlier_m$cutoff

examScor_outlier_md <- examScor_outlier_md %>% as.vector()
examScor_outlier_md <- examScor_outlier_md %>% unlist()
examScor_outlier_md <- examScor_outlier_md %>% as.numeric()

examScor_outlier_rd <- examScor_outlier_rd %>% as.vector()
examScor_outlier_rd <- examScor_outlier_rd %>% unlist()
examScor_outlier_rd <- examScor_outlier_rd %>% as.numeric()

examScor_outlier_cut <- examScor_outlier_cut %>% as.vector()
examScor_outlier_cut <- examScor_outlier_cut %>% unlist()
examScor_outlier_cut <- examScor_outlier_cut %>% as.numeric()


# classical Mahalanobis distance

outlier_md <- c()

for(i in 1:length(examScor_outlier_md)){
  if (examScor_outlier_md[i] >= examScor_outlier_cut) {
    outlier_mdi <- print(i)
    outlier_md <- append(outlier_md, outlier_mdi)}
}


# robust Mahalanobis distance

outlier_rd <- c()

for(i in 1:length(examScor_outlier_rd)){
  if (examScor_outlier_rd[i] >= examScor_outlier_cut) {
    outlier_rdi <- print(i)
    outlier_rd <- append(outlier_rd, outlier_rdi)}
}


## Local Outlier Factor
library(DMwR)

examScor_outlier_L <- examScor %>% lofactor(k = 4) 
# K : The number of neighbours that will be used in the calculation of the local outlier factors.


examScor_outlier_L_density <- examScor_outlier_L %>% density()

examScor_outlier_L_plot <- examScor_outlier_L_density %>% plot(main = "Local Outlier Factor score")

examScor_outlier_L_sort <- examScor_outlier_L %>% sort(decreasing = TRUE)
examScor_outlier_L_sort %>% head()


outlier_LOF <- c()

for(i in 1:length(examScor_outlier_L)){
  if (examScor_outlier_L[i] >= 1.6) {
    outlier_LOFi <- print(i)
    outlier_LOF <- append(outlier_LOF, outlier_LOFi)}
}


### (5) 이상치를 제거하여 다변량 정규분포를 만족하는 자료를 제공하여 보라
## classical mahalanobis 방법으로 이상치 제거
# media test
outlier제거_cm <- examScor[-outlier_cm,]

outlier제거_cm_mvn <- mvn(outlier제거_cm, multivariatePlot = "qq")

# srivastava test
메카.벡터outlier제거_cm <- outlier제거_cm %>% select(mechanics, vectors)
메카.벡터outlier제거_cm_mat <- 메카.벡터outlier제거_cm %>% as.matrix()

대수.분석outlier제거_cm <- outlier제거_cm %>% select(algebra, analysis)
대수.분석outlier제거_cm_mat <- 대수.분석outlier제거_cm %>% as.matrix()

sri.test(메카.벡터outlier제거_cm_mat, 대수.분석outlier제거_cm_mat)



## robust Mahalanobis distance 방법으로 이상치 제거
# media test
outlier제거_md <- examScor[-outlier_md,]

examScor_mvn <- mvn(examScor, multivariatePlot = "qq")
outlier제거_md_mvn <- mvn(outlier제거_md, multivariatePlot = "qq")

# srivastava test
메카.벡터outlier제거_md <- outlier제거_md %>% select(mechanics, vectors)
메카.벡터outlier제거_md_mat <- 메카.벡터outlier제거_md %>% as.matrix()

대수.분석outlier제거_md <- outlier제거_md %>% select(algebra, analysis)
대수.분석outlier제거_md_mat <- 대수.분석outlier제거_md %>% as.matrix()

sri.test(메카.벡터outlier제거_md_mat, 대수.분석outlier제거_md_mat)


## Local Outlier Factor 방법으로 이상치 제거
# media test
outlier제거_LOF <- examScor[-outlier_LOF,]

outlier제거_LOF_mvn <- mvn(outlier제거_LOF, multivariatePlot = "qq")

# srivastava test
메카.벡터outlier제거_LOF <- outlier제거_LOF %>% select(mechanics, vectors)
메카.벡터outlier제거_LOF_mat <- 메카.벡터outlier제거_LOF %>% as.matrix()

대수.분석outlier제거_LOF <- outlier제거_LOF %>% select(algebra, analysis)
대수.분석outlier제거_LOF_mat <- 대수.분석outlier제거_LOF %>% as.matrix()

sri.test(메카.벡터outlier제거_LOF_mat, 대수.분석outlier제거_LOF_mat)





##### 2.6.8

##데이터불러오기
klpga <- read.table("D:/대학원/다변량/rmtda/klpga.txt", header = TRUE) 
klpga <- klpga %>% data.frame()

klpga %>% head()

### (1) 기술요인변수군과 경기성적요인변수군의 각각의 다변량 정규성을 카이제곱 그림으로 살펴보라
기술 <- klpga %>% select(그린적중율,파세이브율,파브레이크율,상금율)
경기성적 <- klpga %>% select(평균퍼팅수, 평균타수)

## 기술요인변수군(그린적중율+파세이브율+파브레이크율+상금율)
기술_mvn <- mvn(기술, multivariatePlot = "qq")

## 경기성적요인변수군(그린적중율+파세이브율+파브레이크율+상금율)
경기성적_mvn <- mvn(경기성적, multivariatePlot = "qq")

### (2) 자료 전체의 다변량 정규성을 만족하는지 카이제곱그림으로 검토하라
klpga_mvn <- mvn(klpga, multivariatePlot = "qq")


### (3) (1)과 (2)의 시각적 방법에서 직선성을 상관계수 검정을 통해 검토하라
##(1)
# 기술요인변수군
기술_n <- 기술 %>% nrow()
기술_p <- 기술 %>% ncol()
기술_s <- 기술 %>% cov()
기술_bar <- 기술 %>% colMeans()
기술_m <- mahalanobis(기술, 기술_bar, 기술_s)
기술_m <- 기술_m %>% sort()
기술_id <- seq(1,기술_n)
기술_pt <- (기술_id-0.5)/기술_n
기술_q <- qchisq(기술_pt,기술_p)

기술_rq <- cor(기술_q, 기술_m)


# 경기성적요인변수군
경기성적_n <- 경기성적 %>% nrow()
경기성적_p <- 경기성적 %>% ncol()
경기성적_s <- 경기성적 %>% cov()
경기성적_bar <- 경기성적 %>% colMeans()
경기성적_m <- mahalanobis(경기성적, 경기성적_bar, 경기성적_s)
경기성적_m <- 경기성적_m %>% sort()
경기성적_id <- seq(1,경기성적_n)
경기성적_pt <- (경기성적_id-0.5)/경기성적_n
경기성적_q <- qchisq(경기성적_pt,경기성적_p)

경기성적_rq <- cor(경기성적_q, 경기성적_m)


## 일변량
그린 <- klpga %>% select(그린적중율)

그린_vector <- 그린 %>% as.vector()
그린_vector <- 그린_vector %>% unlist()
그린_num <- 그린_vector %>% as.numeric()

그린_norm <- qqnorm(그린_num, pch = 1, main = "Q-Q plot(2)-green")
qqline(그린_num, col = "Blue", lwd = 2)

그린_sort_norm_x <- 그린_norm$x %>% sort()
그린_sort_norm_y <- 그린_norm$y %>% sort()

cor(그린_sort_norm_x, 그린_sort_norm_y)


##(2)
klpga_n <- klpga %>% nrow()
klpga_p <- klpga %>% ncol()
klpga_s <- klpga %>% cov()
klpga_bar <- klpga %>% colMeans()
klpga_m <- mahalanobis(klpga, klpga_bar, klpga_s)
klpga_m <- klpga_m %>% sort()
klpga_id <- seq(1,klpga_n)
klpga_pt <- (klpga_id-0.5)/klpga_n
klpga_q <- qchisq(klpga_pt,klpga_p)

klpga_rq <- cor(klpga_q, klpga_m)


### (4) 주성분 분석을 활용한 시각적 방법과 검정방법에서 설명력이 높은 두 성분을 활용하여 다변량 정규성을 검토하라
library(tidyr)
library(data.table)
library(corrplot)
library(factoextra)
library(FactoMineR)

##변수간 상관계수
klpga_cor <- klpga %>% cor()
klpga_corr <- klpga_cor %>% corrplot()

##주성분 분석
klpga_pca <- klpga %>% PCA(graph = FALSE)
klpga_pca %>% summary()

## comp의 변동
klpga_pca_comp <- klpga_pca$eig
klpga_pca_comp %>% View()

## screeplot
klpga_pca_scree <- klpga_pca %>% fviz_screeplot()


## 주성분 Biplot
#biplot1
klpga_pca <- klpga %>% PCA()

#biplot2
klpga_biplot2 <- klpga_pca %>% fviz_pca_var(col.var="contrib",
                                            gradient.cols = c("#a18b6b", "#002554"),
                                            repel = TRUE)
#biplot3
klpga_biplot3 <- klpga_pca %>% fviz_pca_biplot(repel = FALSE)



## 주성분 가중치
klpga_pca_coord <- klpga_pca$var$coord
klpga_pca_coord %>% View()

## 주성분 정규성 검정
klpga_pca_coord_df <- klpga_pca_coord %>% data.frame()

klpga_pca_coord_Dim1 <- klpga_pca_coord_df$Dim.1
klpga_pca_coord_Dim2 <- klpga_pca_coord_df$Dim.2

klpga_pca_coord_Dim1 %>% shapiro.test()
klpga_pca_coord_Dim2 %>% shapiro.test()



###(4) 다변량 정규성에 영향을 미치는 이상치를 검토하라
## mahalanobis
library(chemometrics)

klpga_outlier_m <- klpga %>% Moutlier(quantile = 0.975)

klpga_outlier_md <- klpga_outlier_m$md
klpga_outlier_rd <- klpga_outlier_m$rd
klpga_outlier_cut <- klpga_outlier_m$cutoff

klpga_outlier_md <- klpga_outlier_md %>% as.vector()
klpga_outlier_md <- klpga_outlier_md %>% unlist()
klpga_outlier_md <- klpga_outlier_md %>% as.numeric()

klpga_outlier_rd <- klpga_outlier_rd %>% as.vector()
klpga_outlier_rd <- klpga_outlier_rd %>% unlist()
klpga_outlier_rd <- klpga_outlier_rd %>% as.numeric()

klpga_outlier_cut <- klpga_outlier_cut %>% as.vector()
klpga_outlier_cut <- klpga_outlier_cut %>% unlist()
klpga_outlier_cut <- klpga_outlier_cut %>% as.numeric()


# classical Mahalanobis distance

outlier_cm <- c()

for(i in 1:length(klpga_outlier_md)){
  if (klpga_outlier_md[i] >= klpga_outlier_cut) {
    outlier_cmi <- print(i)
    outlier_cm <- append(outlier_cm, outlier_cmi)}
}

# robust Mahalanobis distance

outlier_rm <- c()

for(i in 1:length(klpga_outlier_rd)){
  if (klpga_outlier_rd[i] >= klpga_outlier_cut) {
    outlier_rmi <- print(i)
    outlier_rm <- append(outlier_rm, outlier_rmi)}
}


## Local Outlier Factor
library(DMwR)

klpga_outlier_L <- klpga %>% lofactor(k = 4) 
# K : The number of neighbours that will be used in the calculation of the local outlier factors.


klpga_outlier_L_density <- klpga_outlier_L %>% density()

klpga_outlier_L_plot <- klpga_outlier_L_density %>% plot(main = "Local Outlier Factor score")

klpga_outlier_L_sort <- klpga_outlier_L %>% sort(decreasing = TRUE)
klpga_outlier_L_sort %>% head()


outlier_LOF <- c()

for(i in 1:length(klpga_outlier_L)){
  if (klpga_outlier_L[i] >= 1.6) {
    outlier_LOFi <- print(i)
    outlier_LOF <- append(outlier_LOF, outlier_LOFi)}
}


### (6) 이상치를 제거하여 다변량 정규분포를 만족하는 자료를 제공하여 보라
## classical mahalanobis 방법으로 이상치 제거
# media test
outlier제거_cm <- klpga[-outlier_cm,]

outlier제거_cm_mvn <- mvn(outlier제거_cm, multivariatePlot = "qq")

## robust Mahalanobis distance 방법으로 이상치 제거
# media test
outlier제거_rm <- klpga[-outlier_rm,]

outlier제거_rm_mvn <- mvn(outlier제거_rm, multivariatePlot = "qq")


## Local Outlier Factor 방법으로 이상치 제거
# media test
outlier제거_LOF <- klpga[-outlier_LOF,]

outlier제거_LOF_mvn <- mvn(outlier제거_LOF, multivariatePlot = "qq")





###### 다변량 통계분석 HW 3
#### 3.6.5
###데이터 불러오기
blood <- read.table("D:/대학원/다변량/rmtda/blood.txt", header = TRUE) 
blood <- blood %>% data.frame()

blood$respon <- blood$respon %>% as.factor()

### (1) 자료의 산점도를 통해 두 군집간의 차이를 살펴보라
library(ggplot2)

ggplot(data=blood, aes(x = fibrin, y = globul, colour = respon)) +   
  geom_point(shape=19, size=2) +
  ggtitle("Scatter Plot by respon, using different Colours")

### (2) 두 군집의 다변량 정규성을 검토하라

## respon 0 다변량 정규성
blood_0 <- blood %>% select(fibrin, globul)
blood_0_mvn <- mvn(blood_0, multivariatePlot = "qq")

## respon 1 다변량 정규성
blood_1 <- blood %>% select(fibrin, globul)
blood_1_mvn <- mvn(blood_1, multivariatePlot = "qq")


### (3) 두 군집의 공분산행렬의 동질성을 검토하라
library(biotools)

blood_fg <- blood %>% select(fibrin, globul)

blood_res <- blood %>% select(respon)
blood_res <- blood_res$respon %>% as.factor()

blood_boxM <- boxM(blood_fg,blood_res)


### (4) 두 군집의 평균벡터에 대한 H0 : μ1 = μ2를 검정하라
library(nparcomp)

blood_bf <- npar.t.test(fibrin+globul~respon, data = blood, method = "normal", alternative = "two.side", info = FALSE)

blood_bf %>% summary()
blood_bf %>% plot()


#### 3.6.10 (1) 상위 25명과 하위 25명의 두 군집에 대해 체격요인변수군 관점에서 다변량 정규성과 공분산행렬의 동질성 검토를 포함하여 두 평균벡터 검정을 실시하라
tennis <- read.table("D:/대학원/다변량/rmtda/tennis.txt", header = TRUE) 
tennis <- tennis %>% data.frame()

high <- rep("high", times = 25)
low <- rep("low", times = 25)

grade <- append(high,low)

tennis <- tennis %>% transform(grade = grade)
tennis$grade <- tennis$grade %>% as.factor()

### 두 군집의 다변량 정규성
##체격 요인-상위 25명
tennis_x <- tennis %>% select(x1,x2,x3,x4,x5,x6,x7,x8,grade)

tennis_x_h <- tennis %>% filter(grade == "high") 
tennis_x_h <- tennis_x_h %>% select(x1,x2,x3,x4,x5,x6,x7,x8)

tennis_x_h_mvn <- mvn(tennis_x_h, multivariatePlot = "qq")

##체격 요인-하위 25명
tennis_x_l <- tennis %>% filter(grade == "low") 
tennis_x_l <- tennis_x_l %>% select(x1,x2,x3,x4,x5,x6,x7,x8)

tennis_x_l_mvn <- mvn(tennis_x_l, multivariatePlot = "qq")

### 두 군집의 공분산행렬의 동질성을 검토하라
tennis_x_18 <- tennis_x %>% select(x1,x2,x3,x4,x5,x6,x7,x8)

tennis_x_g <- tennis_x %>% select(grade)
tennis_x_g <- tennis_x_g$grade %>% as.factor()

tennis_boxM <- boxM(tennis_x_18,tennis_x_g)

### 두 군집의 평균벡터에 대한 H0 : μ1 = μ2를 검정하라
tennis_x_bf <- npar.t.test(x1+x2+x3+x4+x5+x6+x7+x8~grade, data = tennis_x, method = "normal", 
                           alternative = "two.side", info = FALSE)

tennis_x_bf %>% summary()
tennis_x_bf %>% plot()


### 두 군집의 다변량 정규성
##체력 요인-상위 25명
tennis_y <- tennis %>% select(y1,y2,y3,y4,y5,y6,y7,y8,grade)

tennis_y_h <- tennis_y %>% filter(grade == "high") 
tennis_y_h <- tennis_y_h %>% select(y1,y2,y3,y4,y5,y6,y7,y8)

tennis_y_h_mvn <- mvn(tennis_y_h, multivariatePlot = "qq")

##체력 요인-하위 25명
tennis_y_l <- tennis %>% filter(grade == "low") 
tennis_y_l <- tennis_y_l %>% select(y1,y2,y3,y4,y5,y6,y7,y8)

tennis_y_l_mvn <- mvn(tennis_y_l, multivariatePlot = "qq")


### 두 군집의 공분산행렬의 동질성을 검토하라
tennis_y_18 <- tennis_y %>% select(y1,y2,y3,y4,y5,y6,y7,y8)

tennis_y_g <- tennis_y %>% select(grade)
tennis_y_g <- tennis_y_g$grade %>% as.factor()

tennis_boxM <- boxM(tennis_y_18,tennis_y_g)


### 두 군집의 평균벡터에 대한 H0 : μ1 = μ2를 검정하라
tennis_y_bf <- npar.t.test(y1+y2+y3+y4+y5+y6+y7+y8~grade, data = tennis_y, method = "normal", 
                           alternative = "two.side", info = FALSE)

tennis_y_bf %>% summary()
tennis_y_bf %>% plot()


### 두 군집의 다변량 정규성
##기초기술 요인-상위 25명
tennis_z <- tennis %>% select(z1,z2,z3,grade)

tennis_z_h <- tennis_z %>% filter(grade == "high") 
tennis_z_h <- tennis_z_h %>% select(z1,z2,z3)

tennis_z_h_mvn <- mvn(tennis_z_h, multivariatePlot = "qq")

##기초기술 요인-하위 25명
tennis_z_l <- tennis %>% filter(grade == "low") 
tennis_z_l <- tennis_z_l %>% select(z1,z2,z3)

tennis_z_l_mvn <- mvn(tennis_z_l, multivariatePlot = "qq")


### 두 군집의 공분산행렬의 동질성을 검토하라
tennis_z_13 <- tennis_z %>% select(z1,z2,z3)

tennis_z_g <- tennis_z %>% select(grade)
tennis_z_g <- tennis_z_g$grade %>% as.factor()

tennis_boxM <- boxM(tennis_z_13,tennis_z_g)


### 두 군집의 평균벡터에 대한 H0 : μ1 = μ2를 검정하라
tennis_z_bf <- npar.t.test(z1+z2+z3~grade, data = tennis_z, method = "normal", 
                           alternative = "two.side", info = FALSE)

tennis_z_bf %>% summary()
tennis_z_bf %>% plot()




#### 4.5.6 
### (1) 세 군집의 다변량 정규성을 검토하라
ear <- read.table("D:/대학원/다변량/R-codes-MET/ear.txt", header = TRUE) 
ear <- ear %>% data.frame()

ordinary <- ear %>% filter(group == "ordinary") 
ordinary <- ordinary %>% select(left,right)

lunatic <- ear %>% filter(group == "lunatic") 
lunatic <- lunatic %>% select(left,right)

other <- ear %>% filter(group == "other") 
other <- other %>% select(left,right) 

ordinary_mvn <- mvn(ordinary, multivariatePlot = "qq")
lunatic_mvn <- mvn(lunatic, multivariatePlot = "qq")
other_mvn <- mvn(other, multivariatePlot = "qq")


### (2) 세 군집의 공분산 행렬이 동질한지 살펴보라
ear_lr <- ear %>% select(left,right) 

ear_g <- ear %>% select(group) 
ear_g <- ear_g$group %>% as.factor()

ear_boxM <- boxM(ear_lr,ear_g)


### (3) 세 군집의 평균벡터에 대한 H0 : μ1 = μ2 = μ3를 검정하라
ear$group <- ear$group %>% factor()

ear_left <- ear %>% select(id,left,group) 
ear_right <- ear %>% select(id,right,group) 

ear_lm <- lm(cbind(left, right) ~ group, ear)
ear_lm %>% summary()

ear_lm %>% Manova(test.statistic = "Wilks")
ear_lm %>% Manova(test.statistic = "Pillai")
ear_lm %>% Manova(test.statistic = "Hotelling-Lawley")
ear_lm %>% Manova(test.statistic = "Roy")



##left-anova
ear_id <- rep(1:20,3)
ear$id <- ear_id %>% factor()

ear_left_g <- ggplot(data=ear_left,aes(x=group,y=left)) +
              geom_line(aes(group=id,col=id))


ear_left_ms_lu <- ear_left %>% 
                 filter(group == "lunatic") %>% 
                 summarize(mean_left = mean(left), sd_left = sd(left))

ear_left_ms_or <- ear_left %>% 
                 filter(group == "ordinary") %>% 
                 summarize(mean_left = mean(left), sd_left = sd(left))

ear_left_ms_ot <- ear_left %>% 
                 filter(group == "other") %>% 
                 summarize(mean_left = mean(left), sd_left = sd(left))

ear_left_ms <- rbind(ear_left_ms_lu,ear_left_ms_or,ear_left_ms_ot)
group <- c("lunatic","ordinary","other")
ear_left_ms$group <- group %>% factor()


ear_left_ms_g <- ggplot(ear_left_ms,aes(x=sd_left,y=mean_left,color=group)) +
                    geom_point()

ear_left_aov <- aov(left ~ group + Error(id/group), data=ear_left) 
ear_left_aov %>% summary()

##right-anova
ear_right_g <- ggplot(data=ear_right,aes(x=group,y=right)) +
  geom_line(aes(group=id,col=id))


ear_right_ms_lu <- ear_right %>% 
  filter(group == "lunatic") %>% 
  summarize(mean_right = mean(right), sd_right = sd(right))

ear_right_ms_or <- ear_right %>% 
  filter(group == "ordinary") %>% 
  summarize(mean_right = mean(right), sd_right = sd(right))

ear_right_ms_ot <- ear_right %>% 
  filter(group == "other") %>% 
  summarize(mean_right = mean(right), sd_right = sd(right))

ear_right_ms <- rbind(ear_right_ms_lu,ear_right_ms_or,ear_right_ms_ot)
group <- c("lunatic","ordinary","other")
ear_right_ms$group <- group %>% factor()


ear_right_ms_g <- ggplot(ear_right_ms,aes(x=sd_right,y=mean_right,color=group)) +
  geom_point()

ear_right_aov <- aov(right ~ group + Error(id/group), data=ear_right) 
ear_right_aov %>% summary()



#### 4.5.7 
### (1) [자료 4.5.2]에서 산모의 몸무게는 어떤 영향을 미치며 그 역할은 무엇인지 설명하라

#산모의 몸무게는 Drug, Apgar1, Apgar2 자료의 구조에 선형적 영향을 미치는 공변량이다. 공변량이란 여러 설명변수들이 공통적으로 공유하고 있는 변량을 의미한다. 여러개의 공변량과 반응변수로 구성관 모형은 MANCOVA로 분석한다

### (2) (1)에서 고려한 산모의 몸무게를 고려하지 않고 네가지 약의 종류에 대한 귀무가설 H0 : μ1 = μ2 = μ3 = μ4를 유의수준 5%내에서 검정하라
drug <- read.table("D:/대학원/다변량/R-codes-MET/drug.txt", header = TRUE) 
drug <- drug %>% data.frame()

drug$Drug <- drug$Drug %>% factor()

drug_lm1 <- lm(cbind(Apgar1, Apgar2) ~ Drug, drug)
drug_lm1 %>% summary()

drug_lm1 %>% Manova(test.statistic = "Wilks")
drug_lm1 %>% Manova(test.statistic = "Pillai")
drug_lm1 %>% Manova(test.statistic = "Hotelling-Lawley")
drug_lm1 %>% Manova(test.statistic = "Roy")


### (3) (1)에서 고려한 산모의 몸무게를 고려하고 네가지 약의 종류에 대한 귀무가설 H0 : μ1 = μ2 = μ3 = μ4를 유의수준 5%내에서 검정하라
drug_lm2 <- lm(cbind(Apgar1, Apgar2) ~ Drug + wt, drug)
drug_lm2 %>% summary()

drug_lm2 %>% Manova(test.statistic = "Wilks")
drug_lm2 %>% Manova(test.statistic = "Pillai")
drug_lm2 %>% Manova(test.statistic = "Hotelling-Lawley")
drug_lm2 %>% Manova(test.statistic = "Roy")


### (4) (2)와 (3)의 차이를 설명하라