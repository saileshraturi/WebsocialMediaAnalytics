#install.packages("twitteR")
#install.packages("RCurl")
#install.packages("httr")
#install.packages("syuzhet")
library(twitteR)
library(RCurl)
library(httr)
library(tm)
library(wordcloud)
library(syuzhet)

consumer_key = "E1f3LfNcr8nC6OTxDYuVJ819e"
consumer_secret = "Yi155Fbnqyg4rIu1TM60oac2t8FS5feCQewe3GCJV9CBtuY6rH"
access_token = "39058787-r9wr3dM1kpOVHrxfmGjo2UtP07qDRUeNtykbJucP3"
access_secret ="0yMoNLveO7Ib4R740j1f7yyesDhDTHioRGJ7lKuZ3AD3x"

setup_twitter_oauth(consumer_key,consumer_secret,access_token, access_secret)

tweets = searchTwitter("Brexit", n = 200, lang = "en")

tweets.df = twListToDF(tweets)

## CLEANING TWEETS

tweets.df$text=gsub("&amp", "", tweets.df$text)
tweets.df$text = gsub("&amp", "", tweets.df$text)
tweets.df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df$text)
tweets.df$text = gsub("@\\w+", "", tweets.df$text)
tweets.df$text = gsub("[[:punct:]]", "", tweets.df$text)
tweets.df$text = gsub("[[:digit:]]", "", tweets.df$text)
tweets.df$text = gsub("http\\w+", "", tweets.df$text)
tweets.df$text = gsub("[ \t]{2,}", "", tweets.df$text)
tweets.df$text = gsub("^\\s+|\\s+$", "", tweets.df$text)

tweets.df$text <- iconv(tweets.df$text, "UTF-8", "ASCII", sub="")

sent.value <- get_sentiment(tweets.df$text)
nrc_sent.value = get_nrc_sentiment(tweets.df$text)

simple_plot(sent.value)

corpusABG = Corpus(VectorSource(tweets.df$text))


wordcloud(corpusABG,colors=rainbow(7),max.words=50)


corpusABG = tm_map(corpusABG, tolower)


corpusABG = tm_map(corpusABG, removePunctuation)





corpusABG = tm_map(corpusABG, removeWords, c(stopwords("english")))


corpusABG = tm_map(corpusABG, stemDocument)

frequenciesABG = DocumentTermMatrix(corpusABG)

sparseABG = removeSparseTerms(frequenciesABG, 0.995)

ABGSparse = as.data.frame(as.matrix(sparseABG))

colnames(ABGSparse) = make.names(colnames(ABGSparse))

category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))

ABGSparse$Polarity = category_senti

table(ABGSparse$Polarity)

# Build a CART model

library(rpart)
library(rpart.plot)

tweetCART = rpart(Polarity ~ ., data=ABGSparse, method="class")

prp(tweetCART,extra=2)

szCARTPred=predict(tweetCART,ABGSparse,type="class")
table(ABGSparse$Polarity,szCARTPred)

ABGSparse$Polarity=as.factor(ABGSparse$Polarity)
szRF=randomForest(Polarity~.,data=ABGSparse)
varImpPlot(szRF)

szRFPred=predict(szRF,ABGSparse,type="class")
table(ABGSparse$Polarity,szRFPred)

library(e1071)

convert_values = function(x) {
  x = ifelse(x > 0, "Yes", "No")
}

szLables=ABGSparse$Polarity

szNB=ABGSparse
szNB$Polarity=NULL

szNB=apply(szNB, MARGIN = 2,
                  convert_values)


szNBModel= naiveBayes(szNB,szLables)
szNBModel

szNB_pred = predict(szNBModel, szNB)

table(ABGSparse$Polarity,szNB_pred)
