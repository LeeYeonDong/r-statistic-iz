library(tidyverse)

rawdata <- read_csv("D:/대학원/상담/유학생/data_modified_0819.csv", locale=locale("ko", encoding = "utf-8"))

rawdata <- rawdata %>%
  mutate(across(-id, as.numeric))

rawdata %>% glimpse()

library(dplyr)
library(tidyr)

#### 1. 통계 의뢰내용
# 현재 데이터를 spss프로그램을 이용하여 아래내용의 통계를 의뢰합니다(<표 1> 참조):
#   - 빈도분석: 응답자의 일반적 특성

# 1. 필요한 변수 선택
selected_data <- rawdata %>%
  select(id, `x1-1`:`x1-7`)

# 2. 빈도와 비율 계산
freq_table <- selected_data %>%
  gather(key = "variable", value = "value", -id) %>%
  group_by(variable, value) %>%
  summarise(n = n()) %>%
  mutate(percentage = (n / sum(n)) * 100)

# 3. 평균 및 표준편차 계산
mean_sd_table <- selected_data %>%
  summarise(across(`x1-1`:`x1-7`, 
                   list(mean = ~mean(.), sd = ~sd(.)), 
                   .names = "{col}_{fn}")) %>%
  pivot_longer(cols = everything(), 
               names_to = c("variable", ".value"), 
               names_sep = "_")

# 4. 빈도/비율과 평균/표준편차 결합
final_table <- freq_table %>%
  left_join(mean_sd_table, by = "variable")

# 5. 테이블 출력
final_table

write.csv(final_table, "D:/대학원/상담/유학생/일반적 특성_0819.csv", row.names = FALSE, fileEncoding = 'cp949')


####
# - 각 변인(한국어능력, 외향성, 친화성, 성실성, 신경증, 개방성, 대학생활적응)의 평균 및 표준편차((그림 1) 참조) 분석

library(dplyr)
library(psych)

rawdata %>% glimpse()

# 1. 변수 그룹화
groups <- list(
  한국어능력 = rawdata %>% select(`x1-4`),
  외향성 = rawdata %>% select(`x2-1`:`x2-7`),
  친화성 = rawdata %>% select(`x2-10`:`x2-14`), # 8 9문항 제외
  성실성 = rawdata %>% select(`x2-15`:`x2-21`),
  신경증 = rawdata %>% select(`x2-22`:`x2-28`),
  개방성 = rawdata %>% select(`x2-29`:`x2-34`),
  대학생활적응 = rawdata %>% select(`x3-1`:`x3-23`)
)

# 2. 그룹별 기술 통계량 및 신뢰도 계산
results <- lapply(names(groups), function(group_name) {
  data <- groups[[group_name]]
  
  min_values <- apply(data, 2, min, na.rm = TRUE)
  max_values <- apply(data, 2, max, na.rm = TRUE)
  mean_values <- apply(data, 2, mean, na.rm = TRUE)
  sd_values <- apply(data, 2, sd, na.rm = TRUE)
  
  # 신뢰도는 1개의 변수를 가진 경우 계산할 수 없음
  reliability <- if (ncol(data) > 1) psych::alpha(data)$total$raw_alpha else NA
  
  tibble(
    도구 = group_name,
    최소값 = min(min_values),
    최대값 = max(max_values),
    평균 = mean(mean_values),
    표준편차 = mean(sd_values),
    신뢰도 = reliability
  )
})

# 3. 결과 테이블 결합
final_table <- bind_rows(results)

# 4. 결과 테이블 출력
print(final_table)

write.csv(final_table, "D:/대학원/상담/유학생/변인_기술통계_수정.csv", row.names = FALSE, fileEncoding = 'cp949')


#### 연구 도구의 상관관계
library(corrplot)

# 1. 변수 그룹화
grouped_data <- list(
  외향성 = rowMeans(rawdata %>% select(`x2-1`:`x2-7`), na.rm = TRUE),
  친화성 = rowMeans(rawdata %>% select(`x2-10`:`x2-14`), na.rm = TRUE),
  성실성 = rowMeans(rawdata %>% select(`x2-15`:`x2-21`), na.rm = TRUE),
  신경증 = rowMeans(rawdata %>% select(`x2-22`:`x2-28`), na.rm = TRUE),
  개방성 = rowMeans(rawdata %>% select(`x2-29`:`x2-34`), na.rm = TRUE),
  대학생활적응 = rowMeans(rawdata %>% select(`x3-1`:`x3-23`), na.rm = TRUE)
)

# 2. 데이터프레임으로 변환
cor_data <- as.data.frame(grouped_data)

# 3. 상관관계 계산 및 p-value 계산
cor_result <- corr.test(cor_data, use = "pairwise", method = "pearson")

# 4. 상관계수와 p-value 추출
cor_matrix <- cor_result$r
p_matrix <- cor_result$p

# 5. 상관계수와 p-value를 결합하여 표시
cor_with_p <- matrix(paste0(round(cor_matrix, 3), "\n(p = ", round(p_matrix, 3), ")"), 
                     nrow = nrow(cor_matrix), 
                     ncol = ncol(cor_matrix))
rownames(cor_with_p) <- colnames(cor_data)
colnames(cor_with_p) <- colnames(cor_data)

# 6. 결과 출력
print(cor_with_p)

write.csv(cor_with_p, "D:/대학원/상담/유학생/연구도구_상관관계_수정.csv", row.names = FALSE, fileEncoding = 'cp949')


# 2. 연구가설
# 이 연구에서는 성격 5요인(외향성, 친화성, 성실성, 신경증, 개방성) 및 한국어능력과 대학생활적응과의 관계에 관한 가설을 검증하고 대학생활적응에 영향을 미치는 하위요인의 영향력을 비교분석하고자 합니다.

library(dplyr)
library(car)       # VIF calculation
library(lm.beta)   # Standardized regression coefficients
library(psych)     # Durbin-Watson test
library(broom)     # Tidy output of model results

# 1. 다중 회귀 분석: 성격 5요인이 대학생활적응에 미치는 영향# 데이터 준비
grouped_data <- data.frame(
  한국어능력 = rowMeans(rawdata %>% select(`x1-4`), na.rm = TRUE),
  외향성 = rowMeans(rawdata %>% select(`x2-1`:`x2-7`), na.rm = TRUE),
  친화성 = rowMeans(rawdata %>% select(`x2-10`:`x2-14`), na.rm = TRUE),
  성실성 = rowMeans(rawdata %>% select(`x2-15`:`x2-21`), na.rm = TRUE),
  신경증 = rowMeans(rawdata %>% select(`x2-22`:`x2-28`), na.rm = TRUE),
  개방성 = rowMeans(rawdata %>% select(`x2-29`:`x2-34`), na.rm = TRUE),
  대학생활적응 = rowMeans(rawdata %>% select(`x3-1`:`x3-23`), na.rm = TRUE),
  학업적응 = rowMeans(rawdata %>% select(`x3-1`:`x3-5`), na.rm = TRUE),
  사회적응 = rowMeans(rawdata %>% select(`x3-6`:`x3-10`), na.rm = TRUE),
  개인_정서적응 = rowMeans(rawdata %>% select(`x3-11`:`x3-15`), na.rm = TRUE),
  대학환경적응 = rowMeans(rawdata %>% select(`x3-16`:`x3-19`), na.rm = TRUE),
  문화적응 = rowMeans(rawdata %>% select(`x3-20`:`x3-23`), na.rm = TRUE)
)

# 다중 회귀 분석
multiple_regression <- lm(대학생활적응 ~ 외향성 + 친화성 + 성실성 + 신경증 + 개방성, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(multiple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(multiple_regression)

# 공선성 진단 (VIF)
vif_values <- vif(multiple_regression)
tolerance_values <- 1 / vif_values

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(multiple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "외향성", "친화성", "성실성", "신경증", "개방성"),
  B = coef(multiple_regression),
  SE = coef(summary(multiple_regression))[, "Std. Error"],
  β = coef(standardized_coefficients),
  t = coef(summary(multiple_regression))[, "t value"],
  p = coef(summary(multiple_regression))[, "Pr(>|t|)"],
  공차한계 = c(NA, tolerance_values),
  VIF = c(NA, vif_values)
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
r_square <- regression_summary$r.squared
adj_r_square <- regression_summary$adj.r.squared
f_stat <- regression_summary$fstatistic
f_p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)

model_stats <- data.frame(
  R_square = r_square,
  Adj_R_square = adj_r_square,
  F_statistic = f_stat[1],
  F_p_value = f_p_value,
  Durbin_Watson = dw_stat$dw
)

# 결과 출력
print(result_table)
print(model_stats)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_1_수정.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_1_수정.csv", row.names = FALSE, fileEncoding = 'cp949')


#  1-1. 외향성은 대학생활적응에 유의한 영향을 미칠 것이다.
# 단순 회귀 분석
simple_regression <- lm(대학생활적응 ~ 외향성, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(simple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(simple_regression)

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(simple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "외향성"),
  B = coef(simple_regression),
  SE = coef(summary(simple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[2]),
  t = coef(summary(simple_regression))[, "t value"],
  p = coef(summary(simple_regression))[, "Pr(>|t|)"]
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
r_square <- regression_summary$r.squared
adj_r_square <- regression_summary$adj.r.squared
f_stat <- regression_summary$fstatistic
f_p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)

model_stats <- data.frame(
  R_square = r_square,
  Adj_R_square = adj_r_square,
  F_statistic = f_stat[1],
  F_p_value = f_p_value,
  Durbin_Watson = dw_stat$dw
)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_1-1_수정.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_1-1.csv", row.names = FALSE, fileEncoding = 'cp949')


# 1-2. 친화성은 대학생활적응에 유의한 영향을 미칠 것이다.
simple_regression <- lm(대학생활적응 ~ 친화성, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(simple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(simple_regression)

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(simple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "친화성"),
  B = coef(simple_regression),
  SE = coef(summary(simple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[2]),
  t = coef(summary(simple_regression))[, "t value"],
  p = coef(summary(simple_regression))[, "Pr(>|t|)"]
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
r_square <- regression_summary$r.squared
adj_r_square <- regression_summary$adj.r.squared
f_stat <- regression_summary$fstatistic
f_p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)

model_stats <- data.frame(
  R_square = r_square,
  Adj_R_square = adj_r_square,
  F_statistic = f_stat[1],
  F_p_value = f_p_value,
  Durbin_Watson = dw_stat$dw
)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_1-2_수정.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_1-2_수정.csv", row.names = FALSE, fileEncoding = 'cp949')

# 1-3. 성실성은 대학생활적응에 유의한 영향을 미칠 것이다.
simple_regression <- lm(대학생활적응 ~ 성실성, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(simple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(simple_regression)

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(simple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "성실성"),
  B = coef(simple_regression),
  SE = coef(summary(simple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[2]),
  t = coef(summary(simple_regression))[, "t value"],
  p = coef(summary(simple_regression))[, "Pr(>|t|)"]
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
r_square <- regression_summary$r.squared
adj_r_square <- regression_summary$adj.r.squared
f_stat <- regression_summary$fstatistic
f_p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)

model_stats <- data.frame(
  R_square = r_square,
  Adj_R_square = adj_r_square,
  F_statistic = f_stat[1],
  F_p_value = f_p_value,
  Durbin_Watson = dw_stat$dw
)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_1-3.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_1-3.csv", row.names = FALSE, fileEncoding = 'cp949')

# 1-4. 신경증은 대학생활적응에 유의한 영향을 미칠 것이다.
simple_regression <- lm(대학생활적응 ~ 신경증, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(simple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(simple_regression)

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(simple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "신경증"),
  B = coef(simple_regression),
  SE = coef(summary(simple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[2]),
  t = coef(summary(simple_regression))[, "t value"],
  p = coef(summary(simple_regression))[, "Pr(>|t|)"]
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
r_square <- regression_summary$r.squared
adj_r_square <- regression_summary$adj.r.squared
f_stat <- regression_summary$fstatistic
f_p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)

model_stats <- data.frame(
  R_square = r_square,
  Adj_R_square = adj_r_square,
  F_statistic = f_stat[1],
  F_p_value = f_p_value,
  Durbin_Watson = dw_stat$dw
)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_1-4.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_1-4.csv", row.names = FALSE, fileEncoding = 'cp949')

# 1-5. 개방성은 대학생활적응에 유의한 영향을 미칠 것이다.
simple_regression <- lm(대학생활적응 ~ 개방성, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(simple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(simple_regression)

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(simple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "개방성"),
  B = coef(simple_regression),
  SE = coef(summary(simple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[2]),
  t = coef(summary(simple_regression))[, "t value"],
  p = coef(summary(simple_regression))[, "Pr(>|t|)"]
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
r_square <- regression_summary$r.squared
adj_r_square <- regression_summary$adj.r.squared
f_stat <- regression_summary$fstatistic
f_p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)

model_stats <- data.frame(
  R_square = r_square,
  Adj_R_square = adj_r_square,
  F_statistic = f_stat[1],
  F_p_value = f_p_value,
  Durbin_Watson = dw_stat$dw
)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_1-5.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_1-5.csv", row.names = FALSE, fileEncoding = 'cp949')

# 2. 한국어능력은 대학생활적응에 유의한 영향을 미칠 것이다.
simple_regression <- lm(대학생활적응 ~ 한국어능력, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(simple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(simple_regression)

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(simple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "개방성"),
  B = coef(simple_regression),
  SE = coef(summary(simple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[2]),
  t = coef(summary(simple_regression))[, "t value"],
  p = coef(summary(simple_regression))[, "Pr(>|t|)"]
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
r_square <- regression_summary$r.squared
adj_r_square <- regression_summary$adj.r.squared
f_stat <- regression_summary$fstatistic
f_p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)

model_stats <- data.frame(
  R_square = r_square,
  Adj_R_square = adj_r_square,
  F_statistic = f_stat[1],
  F_p_value = f_p_value,
  Durbin_Watson = dw_stat$dw
)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_2.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_2.csv", row.names = FALSE, fileEncoding = 'cp949')

# 3.1 성격 5요인은 학업적응에 유의한 영향을 미칠 것이다.
# 다중 회귀 분석
multiple_regression <- lm(학업적응 ~ 외향성 + 친화성 + 성실성 + 신경증 + 개방성, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(multiple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(multiple_regression)

# 공선성 통계량 (VIF와 공차한계)
vif_values <- vif(multiple_regression)
tolerance_values <- 1 / vif_values

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(multiple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "외향성", "친화성", "성실성", "신경증", "개방성"),
  B = coef(multiple_regression),
  SE = coef(summary(multiple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[-1]), # Intercept에는 β 값이 없음
  t = coef(summary(multiple_regression))[, "t value"],
  p = coef(summary(multiple_regression))[, "Pr(>|t|)"],
  공차한계 = c(NA, tolerance_values),  # Intercept에는 공차한계 값이 없음
  VIF = c(NA, vif_values)  # Intercept에는 VIF 값이 없음
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
model_stats <- data.frame(
  R_square = regression_summary$r.squared,
  Adj_R_square = regression_summary$adj.r.squared,
  F_statistic = regression_summary$fstatistic[1],
  F_p_value = pf(regression_summary$fstatistic[1], regression_summary$fstatistic[2], regression_summary$fstatistic[3], lower.tail = FALSE),
  Durbin_Watson = dw_stat$dw
)

# 결과 출력
print(result_table)
print(model_stats)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_3.1_수정.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_3.1_수정.csv", row.names = FALSE, fileEncoding = 'cp949')

# 3-1-A 외향성은 학업적응에 유의한 영향을 미칠 것이다.
# 단순 회귀 분석: 외향성 -> 학업적응
simple_regression <- lm(학업적응 ~ 외향성, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(simple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(simple_regression)

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(simple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "외향성"),
  B = coef(simple_regression),
  SE = coef(summary(simple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[-1]),  # Intercept에는 β 값이 없음
  t = coef(summary(simple_regression))[, "t value"],
  p = coef(summary(simple_regression))[, "Pr(>|t|)"]
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
model_stats <- data.frame(
  R_square = regression_summary$r.squared,
  Adj_R_square = regression_summary$adj.r.squared,
  F_statistic = regression_summary$fstatistic[1],
  F_p_value = pf(regression_summary$fstatistic[1], regression_summary$fstatistic[2], regression_summary$fstatistic[3], lower.tail = FALSE),
  Durbin_Watson = dw_stat$dw
)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_3-1-A.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_3-1-A.csv", row.names = FALSE, fileEncoding = 'cp949')

# 3-1-B 외향성은 사회적응에 유의한 영향을 미칠 것이다.
# 단순 회귀 분석
simple_regression <- lm(사회적응 ~ 외향성, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(simple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(simple_regression)

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(simple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "외향성"),
  B = coef(simple_regression),
  SE = coef(summary(simple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[-1]),  # Intercept에는 β 값이 없음
  t = coef(summary(simple_regression))[, "t value"],
  p = coef(summary(simple_regression))[, "Pr(>|t|)"]
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
model_stats <- data.frame(
  R_square = regression_summary$r.squared,
  Adj_R_square = regression_summary$adj.r.squared,
  F_statistic = regression_summary$fstatistic[1],
  F_p_value = pf(regression_summary$fstatistic[1], regression_summary$fstatistic[2], regression_summary$fstatistic[3], lower.tail = FALSE),
  Durbin_Watson = dw_stat$dw
)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_3-1-B.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_3-1-B.csv", row.names = FALSE, fileEncoding = 'cp949')

# 3-1-C 외향성은 개인_정서적응에 유의한 영향을 미칠 것이다.
# 단순 회귀 분석
simple_regression <- lm(개인_정서적응 ~ 외향성, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(simple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(simple_regression)

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(simple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "외향성"),
  B = coef(simple_regression),
  SE = coef(summary(simple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[-1]),  # Intercept에는 β 값이 없음
  t = coef(summary(simple_regression))[, "t value"],
  p = coef(summary(simple_regression))[, "Pr(>|t|)"]
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
model_stats <- data.frame(
  R_square = regression_summary$r.squared,
  Adj_R_square = regression_summary$adj.r.squared,
  F_statistic = regression_summary$fstatistic[1],
  F_p_value = pf(regression_summary$fstatistic[1], regression_summary$fstatistic[2], regression_summary$fstatistic[3], lower.tail = FALSE),
  Durbin_Watson = dw_stat$dw
)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_3-1-C.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_3-1-C.csv", row.names = FALSE, fileEncoding = 'cp949')


# 3-1-E 외향성은 문화적응에 유의한 영향을 미칠 것이다.
# 단순 회귀 분석
simple_regression <- lm(문화적응 ~ 외향성, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(simple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(simple_regression)

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(simple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "외향성"),
  B = coef(simple_regression),
  SE = coef(summary(simple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[-1]),  # Intercept에는 β 값이 없음
  t = coef(summary(simple_regression))[, "t value"],
  p = coef(summary(simple_regression))[, "Pr(>|t|)"]
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
model_stats <- data.frame(
  R_square = regression_summary$r.squared,
  Adj_R_square = regression_summary$adj.r.squared,
  F_statistic = regression_summary$fstatistic[1],
  F_p_value = pf(regression_summary$fstatistic[1], regression_summary$fstatistic[2], regression_summary$fstatistic[3], lower.tail = FALSE),
  Durbin_Watson = dw_stat$dw
)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_3-1-E.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_3-1-E.csv", row.names = FALSE, fileEncoding = 'cp949')

# 3-2-A 친화성은 학업적응에 유의한 영향을 미칠 것이다.
# 단순 회귀 분석
simple_regression <- lm(학업적응 ~ 친화성, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(simple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(simple_regression)

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(simple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "친화성"),
  B = coef(simple_regression),
  SE = coef(summary(simple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[-1]),  # Intercept에는 β 값이 없음
  t = coef(summary(simple_regression))[, "t value"],
  p = coef(summary(simple_regression))[, "Pr(>|t|)"]
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
model_stats <- data.frame(
  R_square = regression_summary$r.squared,
  Adj_R_square = regression_summary$adj.r.squared,
  F_statistic = regression_summary$fstatistic[1],
  F_p_value = pf(regression_summary$fstatistic[1], regression_summary$fstatistic[2], regression_summary$fstatistic[3], lower.tail = FALSE),
  Durbin_Watson = dw_stat$dw
)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_3-2-A_수정.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_3-2-A_수정.csv", row.names = FALSE, fileEncoding = 'cp949')

# 3-2-B 친화성은 사회적응에 유의한 영향을 미칠 것이다.
# 단순 회귀 분석
simple_regression <- lm(사회적응 ~ 친화성, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(simple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(simple_regression)

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(simple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "친화성"),
  B = coef(simple_regression),
  SE = coef(summary(simple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[-1]),  # Intercept에는 β 값이 없음
  t = coef(summary(simple_regression))[, "t value"],
  p = coef(summary(simple_regression))[, "Pr(>|t|)"]
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
model_stats <- data.frame(
  R_square = regression_summary$r.squared,
  Adj_R_square = regression_summary$adj.r.squared,
  F_statistic = regression_summary$fstatistic[1],
  F_p_value = pf(regression_summary$fstatistic[1], regression_summary$fstatistic[2], regression_summary$fstatistic[3], lower.tail = FALSE),
  Durbin_Watson = dw_stat$dw
)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_3-2-B_수정.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_3-2-B_수정.csv", row.names = FALSE, fileEncoding = 'cp949')


# 3-2-C 친화성은 개인_정서적응에 유의한 영향을 미칠 것이다.
# 단순 회귀 분석
simple_regression <- lm(개인_정서적응 ~ 친화성, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(simple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(simple_regression)

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(simple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "친화성"),
  B = coef(simple_regression),
  SE = coef(summary(simple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[-1]),  # Intercept에는 β 값이 없음
  t = coef(summary(simple_regression))[, "t value"],
  p = coef(summary(simple_regression))[, "Pr(>|t|)"]
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
model_stats <- data.frame(
  R_square = regression_summary$r.squared,
  Adj_R_square = regression_summary$adj.r.squared,
  F_statistic = regression_summary$fstatistic[1],
  F_p_value = pf(regression_summary$fstatistic[1], regression_summary$fstatistic[2], regression_summary$fstatistic[3], lower.tail = FALSE),
  Durbin_Watson = dw_stat$dw
)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_3-2-C_수정.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_3-2-C_수정.csv", row.names = FALSE, fileEncoding = 'cp949')



# 3-2-D 친화성은 대학환경적응에 유의한 영향을 미칠 것이다.
# 단순 회귀 분석
simple_regression <- lm(대학환경적응 ~ 친화성, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(simple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(simple_regression)

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(simple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "친화성"),
  B = coef(simple_regression),
  SE = coef(summary(simple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[-1]),  # Intercept에는 β 값이 없음
  t = coef(summary(simple_regression))[, "t value"],
  p = coef(summary(simple_regression))[, "Pr(>|t|)"]
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
model_stats <- data.frame(
  R_square = regression_summary$r.squared,
  Adj_R_square = regression_summary$adj.r.squared,
  F_statistic = regression_summary$fstatistic[1],
  F_p_value = pf(regression_summary$fstatistic[1], regression_summary$fstatistic[2], regression_summary$fstatistic[3], lower.tail = FALSE),
  Durbin_Watson = dw_stat$dw
)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_3-2-D_수정.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_3-2-D_수정.csv", row.names = FALSE, fileEncoding = 'cp949')


# 3-2-E 친화성은 문화적응에 유의한 영향을 미칠 것이다. 것이다.
# 단순 회귀 분석
simple_regression <- lm(문화적응 ~ 친화성, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(simple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(simple_regression)

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(simple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "친화성"),
  B = coef(simple_regression),
  SE = coef(summary(simple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[-1]),  # Intercept에는 β 값이 없음
  t = coef(summary(simple_regression))[, "t value"],
  p = coef(summary(simple_regression))[, "Pr(>|t|)"]
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
model_stats <- data.frame(
  R_square = regression_summary$r.squared,
  Adj_R_square = regression_summary$adj.r.squared,
  F_statistic = regression_summary$fstatistic[1],
  F_p_value = pf(regression_summary$fstatistic[1], regression_summary$fstatistic[2], regression_summary$fstatistic[3], lower.tail = FALSE),
  Durbin_Watson = dw_stat$dw
)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_3-2-E_수정.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_3-2-E_수정.csv", row.names = FALSE, fileEncoding = 'cp949')


# 3-3-A 성실성은 학업적응에 유의한 영향을 미칠 것이다.
# 단순 회귀 분석
simple_regression <- lm(학업적응 ~ 성실성, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(simple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(simple_regression)

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(simple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "성실성"),
  B = coef(simple_regression),
  SE = coef(summary(simple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[-1]),  # Intercept에는 β 값이 없음
  t = coef(summary(simple_regression))[, "t value"],
  p = coef(summary(simple_regression))[, "Pr(>|t|)"]
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
model_stats <- data.frame(
  R_square = regression_summary$r.squared,
  Adj_R_square = regression_summary$adj.r.squared,
  F_statistic = regression_summary$fstatistic[1],
  F_p_value = pf(regression_summary$fstatistic[1], regression_summary$fstatistic[2], regression_summary$fstatistic[3], lower.tail = FALSE),
  Durbin_Watson = dw_stat$dw
)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_3-3-A.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_3-3-A.csv", row.names = FALSE, fileEncoding = 'cp949')


# 3-3-B 성실성은 사회적응에 유의한 영향을 미칠 것이다.
# 단순 회귀 분석
simple_regression <- lm(사회적응 ~ 성실성, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(simple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(simple_regression)

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(simple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "성실성"),
  B = coef(simple_regression),
  SE = coef(summary(simple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[-1]),  # Intercept에는 β 값이 없음
  t = coef(summary(simple_regression))[, "t value"],
  p = coef(summary(simple_regression))[, "Pr(>|t|)"]
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
model_stats <- data.frame(
  R_square = regression_summary$r.squared,
  Adj_R_square = regression_summary$adj.r.squared,
  F_statistic = regression_summary$fstatistic[1],
  F_p_value = pf(regression_summary$fstatistic[1], regression_summary$fstatistic[2], regression_summary$fstatistic[3], lower.tail = FALSE),
  Durbin_Watson = dw_stat$dw
)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_3-3-B.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_3-3-B.csv", row.names = FALSE, fileEncoding = 'cp949')

# 3-3-C 성실성은 개인_정서적응에 유의한 영향을 미칠 것이다.
# 단순 회귀 분석
simple_regression <- lm(개인_정서적응 ~ 성실성, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(simple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(simple_regression)

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(simple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "성실성"),
  B = coef(simple_regression),
  SE = coef(summary(simple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[-1]),  # Intercept에는 β 값이 없음
  t = coef(summary(simple_regression))[, "t value"],
  p = coef(summary(simple_regression))[, "Pr(>|t|)"]
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
model_stats <- data.frame(
  R_square = regression_summary$r.squared,
  Adj_R_square = regression_summary$adj.r.squared,
  F_statistic = regression_summary$fstatistic[1],
  F_p_value = pf(regression_summary$fstatistic[1], regression_summary$fstatistic[2], regression_summary$fstatistic[3], lower.tail = FALSE),
  Durbin_Watson = dw_stat$dw
)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_3-3-C.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_3-3-C.csv", row.names = FALSE, fileEncoding = 'cp949')


# 3-3-D 성실성은 대학환경적응에 유의한 영향을 미칠 것이다.
# 단순 회귀 분석
simple_regression <- lm(대학환경적응 ~ 성실성, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(simple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(simple_regression)

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(simple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "성실성"),
  B = coef(simple_regression),
  SE = coef(summary(simple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[-1]),  # Intercept에는 β 값이 없음
  t = coef(summary(simple_regression))[, "t value"],
  p = coef(summary(simple_regression))[, "Pr(>|t|)"]
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
model_stats <- data.frame(
  R_square = regression_summary$r.squared,
  Adj_R_square = regression_summary$adj.r.squared,
  F_statistic = regression_summary$fstatistic[1],
  F_p_value = pf(regression_summary$fstatistic[1], regression_summary$fstatistic[2], regression_summary$fstatistic[3], lower.tail = FALSE),
  Durbin_Watson = dw_stat$dw
)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_3-3-D.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_3-3-D.csv", row.names = FALSE, fileEncoding = 'cp949')


# 3-3-E 성실성은 문화적응에 유의한 영향을 미칠 것이다.
# 단순 회귀 분석
simple_regression <- lm(문화적응 ~ 성실성, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(simple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(simple_regression)

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(simple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "성실성"),
  B = coef(simple_regression),
  SE = coef(summary(simple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[-1]),  # Intercept에는 β 값이 없음
  t = coef(summary(simple_regression))[, "t value"],
  p = coef(summary(simple_regression))[, "Pr(>|t|)"]
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
model_stats <- data.frame(
  R_square = regression_summary$r.squared,
  Adj_R_square = regression_summary$adj.r.squared,
  F_statistic = regression_summary$fstatistic[1],
  F_p_value = pf(regression_summary$fstatistic[1], regression_summary$fstatistic[2], regression_summary$fstatistic[3], lower.tail = FALSE),
  Durbin_Watson = dw_stat$dw
)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_3-3-E.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_3-3-E.csv", row.names = FALSE, fileEncoding = 'cp949')


# 3-4-A 신경증은 학업적응에 유의한 영향을 미칠 것이다.
# 단순 회귀 분석
simple_regression <- lm(학업적응 ~ 신경증, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(simple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(simple_regression)

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(simple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "신경증"),
  B = coef(simple_regression),
  SE = coef(summary(simple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[-1]),  # Intercept에는 β 값이 없음
  t = coef(summary(simple_regression))[, "t value"],
  p = coef(summary(simple_regression))[, "Pr(>|t|)"]
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
model_stats <- data.frame(
  R_square = regression_summary$r.squared,
  Adj_R_square = regression_summary$adj.r.squared,
  F_statistic = regression_summary$fstatistic[1],
  F_p_value = pf(regression_summary$fstatistic[1], regression_summary$fstatistic[2], regression_summary$fstatistic[3], lower.tail = FALSE),
  Durbin_Watson = dw_stat$dw
)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_3-4-A.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_3-4-A.csv", row.names = FALSE, fileEncoding = 'cp949')


# 3-4-C 신경증은 개인_정서적응에 유의한 영향을 미칠 것이다.
# 단순 회귀 분석
simple_regression <- lm(개인_정서적응 ~ 신경증, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(simple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(simple_regression)

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(simple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "신경증"),
  B = coef(simple_regression),
  SE = coef(summary(simple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[-1]),  # Intercept에는 β 값이 없음
  t = coef(summary(simple_regression))[, "t value"],
  p = coef(summary(simple_regression))[, "Pr(>|t|)"]
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
model_stats <- data.frame(
  R_square = regression_summary$r.squared,
  Adj_R_square = regression_summary$adj.r.squared,
  F_statistic = regression_summary$fstatistic[1],
  F_p_value = pf(regression_summary$fstatistic[1], regression_summary$fstatistic[2], regression_summary$fstatistic[3], lower.tail = FALSE),
  Durbin_Watson = dw_stat$dw
)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_3-4-C.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_3-4-C.csv", row.names = FALSE, fileEncoding = 'cp949')


# 3-4-E 신경증은 문화적응에 유의한 영향을 미칠 것이다.
# 단순 회귀 분석
simple_regression <- lm(문화적응 ~ 신경증, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(simple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(simple_regression)

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(simple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "신경증"),
  B = coef(simple_regression),
  SE = coef(summary(simple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[-1]),  # Intercept에는 β 값이 없음
  t = coef(summary(simple_regression))[, "t value"],
  p = coef(summary(simple_regression))[, "Pr(>|t|)"]
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
model_stats <- data.frame(
  R_square = regression_summary$r.squared,
  Adj_R_square = regression_summary$adj.r.squared,
  F_statistic = regression_summary$fstatistic[1],
  F_p_value = pf(regression_summary$fstatistic[1], regression_summary$fstatistic[2], regression_summary$fstatistic[3], lower.tail = FALSE),
  Durbin_Watson = dw_stat$dw
)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_3-4-E.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_3-4-E.csv", row.names = FALSE, fileEncoding = 'cp949')


# 3-5-A 개방성은 학업적응에 유의한 영향을 미칠 것이다.
# 단순 회귀 분석
simple_regression <- lm(학업적응 ~ 개방성, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(simple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(simple_regression)

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(simple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "개방성"),
  B = coef(simple_regression),
  SE = coef(summary(simple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[-1]),  # Intercept에는 β 값이 없음
  t = coef(summary(simple_regression))[, "t value"],
  p = coef(summary(simple_regression))[, "Pr(>|t|)"]
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
model_stats <- data.frame(
  R_square = regression_summary$r.squared,
  Adj_R_square = regression_summary$adj.r.squared,
  F_statistic = regression_summary$fstatistic[1],
  F_p_value = pf(regression_summary$fstatistic[1], regression_summary$fstatistic[2], regression_summary$fstatistic[3], lower.tail = FALSE),
  Durbin_Watson = dw_stat$dw
)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_3-5-A.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_3-5-A.csv", row.names = FALSE, fileEncoding = 'cp949')


# 3-5-B 개방성은 사회적응에 유의한 영향을 미칠 것이다.
# 단순 회귀 분석
simple_regression <- lm(사회적응 ~ 개방성, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(simple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(simple_regression)

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(simple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "개방성"),
  B = coef(simple_regression),
  SE = coef(summary(simple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[-1]),  # Intercept에는 β 값이 없음
  t = coef(summary(simple_regression))[, "t value"],
  p = coef(summary(simple_regression))[, "Pr(>|t|)"]
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
model_stats <- data.frame(
  R_square = regression_summary$r.squared,
  Adj_R_square = regression_summary$adj.r.squared,
  F_statistic = regression_summary$fstatistic[1],
  F_p_value = pf(regression_summary$fstatistic[1], regression_summary$fstatistic[2], regression_summary$fstatistic[3], lower.tail = FALSE),
  Durbin_Watson = dw_stat$dw
)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_3-5-B.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_3-5-B.csv", row.names = FALSE, fileEncoding = 'cp949')


# 3-5-B 개방성은 사회적응에 유의한 영향을 미칠 것이다.
# 단순 회귀 분석
simple_regression <- lm(사회적응 ~ 개방성, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(simple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(simple_regression)

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(simple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "개방성"),
  B = coef(simple_regression),
  SE = coef(summary(simple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[-1]),  # Intercept에는 β 값이 없음
  t = coef(summary(simple_regression))[, "t value"],
  p = coef(summary(simple_regression))[, "Pr(>|t|)"]
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
model_stats <- data.frame(
  R_square = regression_summary$r.squared,
  Adj_R_square = regression_summary$adj.r.squared,
  F_statistic = regression_summary$fstatistic[1],
  F_p_value = pf(regression_summary$fstatistic[1], regression_summary$fstatistic[2], regression_summary$fstatistic[3], lower.tail = FALSE),
  Durbin_Watson = dw_stat$dw
)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_3-5-B.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_3-5-B.csv", row.names = FALSE, fileEncoding = 'cp949')


# 3-5-C 개방성은 개인_정서적응에 유의한 영향을 미칠 것이다.
# 단순 회귀 분석
simple_regression <- lm(개인_정서적응 ~ 개방성, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(simple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(simple_regression)

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(simple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "개방성"),
  B = coef(simple_regression),
  SE = coef(summary(simple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[-1]),  # Intercept에는 β 값이 없음
  t = coef(summary(simple_regression))[, "t value"],
  p = coef(summary(simple_regression))[, "Pr(>|t|)"]
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
model_stats <- data.frame(
  R_square = regression_summary$r.squared,
  Adj_R_square = regression_summary$adj.r.squared,
  F_statistic = regression_summary$fstatistic[1],
  F_p_value = pf(regression_summary$fstatistic[1], regression_summary$fstatistic[2], regression_summary$fstatistic[3], lower.tail = FALSE),
  Durbin_Watson = dw_stat$dw
)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_3-5-C.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_3-5-C.csv", row.names = FALSE, fileEncoding = 'cp949')


# 3-5-D 개방성은 대학환경적응에 유의한 영향을 미칠 것이다.
# 단순 회귀 분석
simple_regression <- lm(대학환경적응 ~ 개방성, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(simple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(simple_regression)

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(simple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "개방성"),
  B = coef(simple_regression),
  SE = coef(summary(simple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[-1]),  # Intercept에는 β 값이 없음
  t = coef(summary(simple_regression))[, "t value"],
  p = coef(summary(simple_regression))[, "Pr(>|t|)"]
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
model_stats <- data.frame(
  R_square = regression_summary$r.squared,
  Adj_R_square = regression_summary$adj.r.squared,
  F_statistic = regression_summary$fstatistic[1],
  F_p_value = pf(regression_summary$fstatistic[1], regression_summary$fstatistic[2], regression_summary$fstatistic[3], lower.tail = FALSE),
  Durbin_Watson = dw_stat$dw
)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_3-5-D.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_3-5-D.csv", row.names = FALSE, fileEncoding = 'cp949')


# 3-5-E 개방성은 문화적응에 유의한 영향을 미칠 것이다.
# 단순 회귀 분석
simple_regression <- lm(문화적응 ~ 개방성, data = grouped_data)

# 회귀 분석 결과 요약
regression_summary <- summary(simple_regression)

# 표준화 회귀계수
standardized_coefficients <- lm.beta(simple_regression)

# Durbin-Watson 통계량
dw_stat <- durbinWatsonTest(simple_regression)

# 결과 데이터 프레임 생성
result_table <- data.frame(
  변수 = c("Intercept", "개방성"),
  B = coef(simple_regression),
  SE = coef(summary(simple_regression))[, "Std. Error"],
  β = c(NA, standardized_coefficients$standardized.coefficients[-1]),  # Intercept에는 β 값이 없음
  t = coef(summary(simple_regression))[, "t value"],
  p = coef(summary(simple_regression))[, "Pr(>|t|)"]
)

# R-square, Adjusted R-square, F(p), Durbin-Watson 추가
model_stats <- data.frame(
  R_square = regression_summary$r.squared,
  Adj_R_square = regression_summary$adj.r.squared,
  F_statistic = regression_summary$fstatistic[1],
  F_p_value = pf(regression_summary$fstatistic[1], regression_summary$fstatistic[2], regression_summary$fstatistic[3], lower.tail = FALSE),
  Durbin_Watson = dw_stat$dw
)

write.csv(result_table, "D:/대학원/상담/유학생/회귀_결과_3-5-E.csv", row.names = FALSE, fileEncoding = 'cp949')
write.csv(model_stats, "D:/대학원/상담/유학생/회귀_모델_3-5-E.csv", row.names = FALSE, fileEncoding = 'cp949')



# "친화성" 변수를 구성하는 문항 중에서 1개 또는 2개를 제거한 모든 조합에 대해 신뢰도를 계산하는 방법
library(dplyr)
library(psych)

# 친화성 데이터를 추출
친화성_data <- rawdata %>% select(`x2-8`:`x2-14`)

# 신뢰도 계산 함수
calculate_reliability <- function(data) {
  if (ncol(data) > 1) {
    return(psych::alpha(data)$total$raw_alpha)
  }
  return(NA)  # 문항이 1개 이하인 경우 NA 반환
}

# 1개 문항을 제거한 신뢰도 계산
reliability_1_removed <- combn(ncol(친화성_data), ncol(친화성_data) - 1, function(cols) {
  selected_data <- 친화성_data[, cols]
  calculate_reliability(selected_data)
}, simplify = TRUE)

# 2개 문항을 제거한 신뢰도 계산
reliability_2_removed <- combn(ncol(친화성_data), ncol(친화성_data) - 2, function(cols) {
  selected_data <- 친화성_data[, cols]
  calculate_reliability(selected_data)
}, simplify = TRUE)

# 3개 문항을 제거한 신뢰도 계산
reliability_3_removed <- combn(ncol(친화성_data), ncol(친화성_data) - 3, function(cols) {
  selected_data <- 친화성_data[, cols]
  calculate_reliability(selected_data)
}, simplify = TRUE)

# 결과 테이블 생성
reliability_results <- tibble(
  제거된_문항_수 = c(rep(1, length(reliability_1_removed)), 
               rep(2, length(reliability_2_removed)), 
               rep(3, length(reliability_3_removed))),
  제거된_조합 = c(apply(combn(names(친화성_data), ncol(친화성_data) - 1), 2, paste, collapse = ", "),
             apply(combn(names(친화성_data), ncol(친화성_data) - 2), 2, paste, collapse = ", "),
             apply(combn(names(친화성_data), ncol(친화성_data) - 3), 2, paste, collapse = ", ")),
  신뢰도 = c(reliability_1_removed, reliability_2_removed, reliability_3_removed)
)

# 결과 출력
print(reliability_results)
reliability_results %>% view()

