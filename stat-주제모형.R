# ??형????
library(alr3)

snake %>% head()

names(snake) <- c("content", "yield")

par(mfrow = c(1,1))
plot(snake, xlab = "water content of snow", ylab = "water yield", main = "Sctterplot of Snow vs. Yield")
abline(yield.fit, lwd=3, col="red")

yield.fit <- lm(snake$yield ~ snake$content)
yield.fit %>% summary()


par(mfrow = c(2,2))
plot(yield.fit)

yield.fit %>% qqPlot()


# ??????선??회귀
water %>% head()
social.water <- water[,-1]

water.cor <- social.water %>% cor()
water.cor %>% corrplot(method = "square")

pairs(~., data=social.water)

library(leaps)
social.water.fit <- lm(BSAAM~., data=social.water)
social.water.fit %>% summary()

sub.fit <- regsubsets(BSAAM~., data=social.water)
sub.fit.summary <- sub.fit %>% summary()
names(sub.fit.summary)

sub.fit.summary$rss %>% which.min()



##textmining
install.packages(c("tidyverse","tm","qdap","corpus"))
install.packages("wordcloud2")
library(tidyverse)
library(tm)
library(qdap)
library(corpus)
library(wordcloud2)

name <- file.path("D:/대학원/선형모형론/2학기/text/")
docs <- Corpus(DirSource(name))

docs %>% inspect()

docs$content

docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeWords, c("the", "and", stopwords("english")))
docs <- tm_map(docs, removeWords, c("applause","can","cant","will","that","weve","dint","wont","youll","youre"))
docs <- tm_map(docs, stripWhitespace)

inspect(docs[1])

dtm <- DocumentTermMatrix(docs)
dtm %>% dim()

dtm <- removeSparseTerms(dtm, 0.75)
dtm %>% dim()

freq <- colSums(as.matrix(dtm))

findFreqTerms(dtm,125)
findAssocs(dtm, "jobs", corlimit = 0.85)

freq_order <- sort(freq, decreasing = TRUE)
freq_order20 <- freq_order[1:20]
freq_order20 <- freq_order20 %>% as.table()

freq_order20 %>% class()

##워드크라우드
freq_order20_워드크라우드 <- freq_order20 %>% wordcloud2(minRotation = 0, maxRotation = 0, shape ="circle")

##빈도그래프
freq_order20df <- freq_order20 %>% as.data.frame()
freq_order20_빈도그래프 <- ggplot(freq_order20df, aes(x=Var1, y=Freq)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x=element_text(angle=90, hjust=1))


##주제모형
install.packages("topicmodels")
library(topicmodels)

set.seed(12)
topic_lda <- LDA(dtm, k=3, method = "Gibbs")
topic_lda %>% summary()
topic_lda %>% topics()
terms(topic_lda,20)


sou15 <- readLines("D:/대학원/선형모형론/2학기/text/sou2015.txt")

speech15 <- sou15 %>% paste0()
speech15 <- iconv(speech15, "latin1", "ASCII","")
prep15 <- speech15 %>% qprep()
prep15 <- prep15 %>% replace_contraction()
prep15 <- prep15 %>% rm_stopwords(Top100Words, separate = FALSE)
prep15 <- prep15 %>% strip(char.keep = c("?","."))

sent15 <- data.frame(speech = prep15)
sent15 <- sentSplit(sent15, "speech")
sent15$year <- "2015"


sou10 <- readLines("D:/대학원/선형모형론/2학기/text/sou2010.txt")

speech10 <- sou10 %>% paste0()
speech10 <- iconv(speech10, "latin1", "ASCII","")
prep10 <- speech10 %>% qprep()
prep10 <- prep10 %>% replace_contraction()
prep10 <- prep10 %>% rm_stopwords(Top100Words, separate = FALSE)
prep10 <- prep10 %>% strip(char.keep = c("?","."))

sent10 <- data.frame(speech = prep10)
sent10 <- sentSplit(sent10, "speech")
sent10$year <- "2010"

sent1015 <- rbind(sent10,sent15)
sentences <- sent1015 %>% data.frame()
sentences %>% View()

sen_speech <- sentences$speech
sen_year <- sentences$year
terms <- freq_terms(sen_speech)
terms %>% plot()

ws <- word_stats(sen_speech, sen_year, rm.incomplete = TRUE)
plot(ws, label = TRUE, lab.digit = 2)


sapply(sentences,class)

pol <- polarity(sentences$speech, sentences$year)

pol_plot <- pol %>% plot()


## 가장 부정적-긍정적 문장 찾기
pol.df <- pol$all
most.neg <- which.min(pol.df$polarity)
most.pos <- which.max(pol.df$polarity)

pol.df$text.var[most.neg]
pol.df$text.var[most.pos]


## 가독성
ari <- automated_readability_index(sentences$speech,sentences$year)

## 형식성(품사)
form <- formality(sentences$speech,sentences$year)
form$form.prop.by

## 다양성
div <- diversity(sentences$speech,sentences$year)

## 분산성
dispersion_plot(sentences$speech,rm.vars = sentences$year, c("security","jobs","ecomomy"),
                color = "black", bg.color = "white")
  
