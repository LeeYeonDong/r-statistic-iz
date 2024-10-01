library(tidyverse)

rawdata <- read_csv("D:/대학원/상담/유학생/data_modified_0819.csv", locale=locale("ko", encoding = "utf-8"))

rawdata %>% glimpse()

library(dplyr)

# 종속변수의 평균값 계산
rawdata <- rawdata %>%
  mutate(
    대학생활적응 = rowMeans(select(., starts_with("x3-")), na.rm = TRUE),
    한국어능력 = `x1-4`,
    외향성 = rowMeans(select(., starts_with("x2-1"):starts_with("x2-7")), na.rm = TRUE),
    친화성 = rowMeans(select(., starts_with("x2-8"):starts_with("x2-14")), na.rm = TRUE),
    성실성 = rowMeans(select(., starts_with("x2-15"):starts_with("x2-21")), na.rm = TRUE),
    신경증 = rowMeans(select(., starts_with("x2-22"):starts_with("x2-28")), na.rm = TRUE),
    개방성 = rowMeans(select(., starts_with("x2-29"):starts_with("x2-34")), na.rm = TRUE)
  )

# 1. 다중 회귀 분석
multi_model <- lm(대학생활적응 ~ 한국어능력 + 외향성 + 친화성 + 성실성 + 신경증 + 개방성, data = rawdata)

# 결과 요약
summary(multi_model)



# 2. 종속변수와 독립변수 2-6와의 개별 단순회귀
simple_models <- list()

for (var_name in c("외향성", "친화성", "성실성", "신경증", "개방성")) {
  formula <- as.formula(paste("대학생활적응 ~", var_name))
  simple_models[[var_name]] <- lm(formula, data = rawdata)
}

# 결과 요약
lapply(simple_models, summary)



# 3. 종속변수와 독립변수1과의 단순회귀
# 단순 회귀 분석 (한국어능력)
simple_model_var1 <- lm(대학생활적응 ~ 한국어능력, data = rawdata)

# 결과 요약
summary(simple_model_var1)


