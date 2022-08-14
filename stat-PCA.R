library(tidyr)
library(data.table)
library(corrplot)
library(factoextra)
library(FactoMineR)
library(MVT)
library(MVN)
library(dplyr)
library(qqplotr)
library(car)
library(multifluo)
library(tidyverse)

##데이터불러오기
klpga <- read.table("D:/대학원/다변량/rmtda/klpga.txt", header = TRUE)
klpga <- klpga %>% as.data.frame()

klpga %>% head()

# 그린적중율 변수만 상관계수를 구하여 직선성을 구해보겠습니다

그린 <- klpga %>% select(그린적중율)

그린_vector <- 그린 %>% as.vector()
그린_vector <- 그린_vector %>% unlist()
그린_num <- 그린_vector %>% as.numeric()

그린_norm <- qqnorm(그린_num, pch = 1, main = "Q-Q plot(2)-green")
qqline(그린_num, col = "Blue", lwd = 2)

그린_sort_norm_x <- 그린_norm$x %>% sort()
그린_sort_norm_y <- 그린_norm$y %>% sort()

cor(그린_sort_norm_x, 그린_sort_norm_y)

# correlation(상관계수)
# 실제 데이터가 아니지만 0.983정도면 굉장히 높은 수치입니다
# 그만큼 직선(qqline)에 관측치가 잘 모여있다고 할 수 있습니다

### 다변량 데이터 상관계수

# klpga 데이터를 살펴보겠습니다
# 기술요인변수와 경기성적요인변수로 구성되어있습니다
# 
# 변수명	요인
# 평균퍼팅수	경기성적요인
# 그린적중율	기술요인
# 파세이브율	기술요인
# 파브레이크율	기술요인
# 평균타수	경기성적요인
# 상금율	기술요인


# 요인별로 변수를 분리해보겠습니다

기술 <- klpga %>% select(그린적중율,파세이브율,파브레이크율,상금율)
경기성적 <- klpga %>% select(평균퍼팅수, 평균타수)


# 기술 data.frame만 다변량 상관계수를 구해보겠습니다

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


#### 주성분 분석을 활용한 시각적 방법과 검정방법에서 설명력이 높은 두 성분을 활용하여 다변량 정규성을 검토

### 주성분 분석 활용 
# 작업을 위해 추가적으로 필요한 라이브러리는 다음과 같습니다
library(tidyr)
library(data.table)
library(corrplot)
library(factoextra)
library(FactoMineR)


# 상관계수
# 데이터 전체를 파악할 겸 변수간 상관계수를 구해봅시다
klpga_cor <- klpga %>% cor()


# 흐름을 좀 더 직관적으로 파악하기 위해 시각화 해봅시다
klpga_corr <- klpga_cor %>% corrplot()


### 주성분 분석
# 주성분 분석(Principal component analysis : PCA)은 여려 변수들로 이루어진 다차원의 데이터를 축소하는 기법입니다
# 주성분 분석은 데이터를 한개의 축으로 사상시켰을 때 그 분산이 가장 커지는 축을 첫 번째 주성분, 두 번째로 커지는 축을 두 번째 주성분으로 놓이도록 새로운 좌표계로 데이터를 선형 변환합니다

## 주성분 분석 실행하기
# 주성분 분석을 해보겠습니다
# 
# 간략한 정보를 보기위해 summary() 함수도 사용하겠습니다

klpga_pca <- klpga %>% PCA(graph = FALSE) 
klpga_pca %>% summary()

## 주성분(comp) 파악하기
# 
# 앞서 언급하였지만, 주성분 분석은 데이터를 한개의 축으로 사상시켰을 때 그 분산이 가장 커지는 축을 첫 번째 주성분, 두 번째로 커지는 축을 두 번째 주성분으로 놓이도록 새로운 좌표계로 데이터를 선형 변환합니다
# 
# 그럼 첫 번째 주성분, 두 번째 주성분을 파악해보겠습니다
klpga_pca_comp <- klpga_pca$eig
klpga_pca_comp %>% View()

# screeplot
klpga_pca_scree <- klpga_pca %>% fviz_screeplot()
# x축은 주성분, y축은 분산비중(%)입니다


## 주성분(comp) 시각화하기(Biplot)
# 주성분을 다양한 Biplot으로 시각화하겠습니다

# Biplot1
klpga_pca <- klpga %>% PCA()

# Biplot2
klpga_biplot2 <- klpga_pca %>% fviz_pca_var(col.var="contrib",
                                            gradient.cols = c("#a18b6b", "#002554"),
                                            repel = TRUE)
# Biplot3
klpga_biplot3 <- klpga_pca %>% fviz_pca_biplot(repel = FALSE)


## 주성분(comp) 정규성 검정
# 주성분 정규성은 가중치를 뽑아서 검정합니다

klpga_pca_coord <- klpga_pca$var$coord
klpga_pca_coord %>% View()



# 뽑아낸 주성분 가중치를 데이터 프레임으로 만들고
# 주성분 1, 주성분 2에 각각에 정규성 검정(shapiro-wilk test)을 합니다

klpga_pca_coord_df <- klpga_pca_coord %>% data.frame()

klpga_pca_coord_Dim1 <- klpga_pca_coord_df$Dim.1
klpga_pca_coord_Dim2 <- klpga_pca_coord_df$Dim.2

klpga_pca_coord_Dim1 %>% shapiro.test()
klpga_pca_coord_Dim2 %>% shapiro.test()

# 주성분 1은 정규성을 만족합니다
# 주성분 2는 아슬하지만 정규성을 만족합니다


