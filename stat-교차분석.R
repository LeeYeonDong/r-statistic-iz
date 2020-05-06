rawdata  <- read.csv("D:/사회가족실설문지/설문지.csv", header=TRUE)



rawdata <- rawdata %>% filter(w06psvy == 1)
rawdata <- subset(rawdata, select=-c(pid,income_month_range))
rawdata <- rawdata %>% filter(intention == 0 |intention == 1)


table.df <- data.frame()
corr.df <- data.frame()
pvalue.m <- matrix()



for(i in 1:945){
  tryCatch({
    table.df <- CrossTable(rawdata[,i],rawdata$intention, expected=TRUE, chisq=TRUE,format =c("SPSS"))
    pvalue.m[i] <- table.df$CST$p.value}, error = function(e) print="error")
}
