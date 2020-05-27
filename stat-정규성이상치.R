여가생활조사 <- read.csv("D:/여가생활조사-필터링.csv",header=TRUE)

행복수준 <- 여가생활조사 %>% select(행복수준)
일생활균형_3그룹 <- 여가생활조사 %>% select(일생활균형_3그룹)


### 행복수준
행복수준_vector <- 행복수준$행복수준 %>% as.vector()
행복수준_vector <- 행복수준_vector %>% unlist()
행복수준_num <- 행복수준_vector %>% as.numeric()


### QQ plot
n <- 행복수준_num %>% length()
p <- (1:n)/(n+1)
q <- qnorm(p, mean(행복수준_num), sd(행복수준_num))

sort_q <- q %>% sort()
행복수준_sort <- 행복수준_num %>% sort()

행복수준_QQ <- plot(sort_q, 행복수준_sort, xlab = "Quantiles from Normal Distribution", ylab = "Sample Quantiles", main = "행복수준")
abline(0,1,col="Red")

cor(sort_q, 행복수준_sort)


# Shapiro Test
shapiro.test(행복수준_num)



# mahalanobis-이상치

df <- data.frame(행복수준,일생활균형_3그룹)

df_outlier_m <- df %>% Moutlier(quantile = 0.99)

df_outlier_md <- df_outlier_m$md
df_outlier_rd <- df_outlier_m$rd
df_outlier_cut <- df_outlier_m$cutoff

df_outlier_md <- df_outlier_md %>% as.vector()
df_outlier_md <- df_outlier_md %>% unlist()
df_outlier_md <- df_outlier_md %>% as.numeric()

df_outlier_rd <- df_outlier_rd %>% as.vector()
df_outlier_rd <- df_outlier_rd %>% unlist()
df_outlier_rd <- df_outlier_rd %>% as.numeric()

df_outlier_cut <- df_outlier_cut %>% as.vector()
df_outlier_cut <- df_outlier_cut %>% unlist()
df_outlier_cut <- df_outlier_cut %>% as.numeric()


df_md <- c()

for(i in 1:length(df_outlier_md)){
  if (df_outlier_md[i] >= df_outlier_cut) {
    df_mdi <- print(i)
    df_md <- append(df_md, df_mdi)}
}




### 일생활균형_3그룹
일생활균형_3그룹_vector <- 일생활균형_3그룹 %>% as.vector()
일생활균형_3그룹_vector <- 일생활균형_3그룹_vector %>% unlist()
일생활균형_3그룹_num <- 일생활균형_3그룹_vector %>% as.numeric()


### QQ plot
n <- 일생활균형_3그룹_num %>% length()
p <- (1:n)/(n+1)
q <- qnorm(p, mean(일생활균형_3그룹_num), sd(일생활균형_3그룹_num))

sort_q <- q %>% sort()
일생활균형_3그룹_sort <- 일생활균형_3그룹_num %>% sort()

일생활균형_3그룹_QQ <- plot(sort_q, 일생활균형_3그룹_sort, xlab = "Quantiles from Normal Distribution", ylab = "Sample Quantiles", main = "일생활균형_3그룹")
abline(0,1,col="Red")

cor(sort_q, 일생활균형_3그룹_sort)


# Shapiro Test
shapiro.test(일생활균형_3그룹_num)


