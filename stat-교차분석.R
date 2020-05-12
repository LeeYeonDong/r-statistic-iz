library(dplyr)
library(gmodels)

rawdata <- read.csv("D:/사회가족실설문지/설문지.csv", header=TRUE)


rawdata <- rawdata %>% filter(w06psvy == 1)
rawdata <- subset(rawdata, select=-c(pid,income_month_range))
rawdata <- rawdata %>% filter(intention == 0 |intention == 1)

rawdata_dim <- rawdata %>% dim()


pvalue.c <- c()


for(i in 1:rawdata_dim[2]){
  tryCatch({
    table.df <- CrossTable(rawdata[,i],rawdata$intention, expected=TRUE, chisq=TRUE)
    pvalue.c[i] <- table.df$chisq$p.value}, error = function(e) print="error")
}

pvalue.c %>% View()
