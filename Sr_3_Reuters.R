rt = read.csv("03_RT.csv", stringsAsFactors=FALSE)

table(rt$DV)
rt$Topic=NULL

# Install new packages

corpusRT = Corpus(VectorSource(rt$Feed))


library(wordcloud)
wordcloud(corpusRT,colors=rainbow(7),max.words=50)

# Convert to lower-case

corpusRT = tm_map(corpusRT, tolower)



corpusRT = tm_map(corpusRT, removePunctuation)


# Remove stopwords and apple

corpusRT = tm_map(corpusRT, removeWords, c("said", stopwords("english")))



# Stem document 

corpusRT = tm_map(corpusRT, stemDocument)

frequenciesRT = DocumentTermMatrix(corpusRT)




sparseRT = removeSparseTerms(frequenciesRT, 0.995)



rtSparse = as.data.frame(as.matrix(sparseRT))

# Make all variable names R-friendly

colnames(rtSparse) = make.names(colnames(rtSparse))

# Add dependent variable

rtSparse$DV = rt$DV

# Build a CART model

library(rpart)
library(rpart.plot)

rtCART = rpart(DV ~ ., data=rtSparse, method="class")

prp(rtCART)

rtCARTPredict=predict(rtCART,rtSparse,type="class")

table(rtSparse$DV,rtCARTPredict)


rtSparse$DV=as.factor(rtSparse$DV)
rtRF=randomForest(DV ~ ., data=rtSparse) ## Takes time
varImpPlot(rtRF)

rtRFPred=predict(rtRF,rtSparse,type="class")
table(rtSparse$DV,rtRFPred)


## Naive Bayes

library(e1071)

convert_values = function(x) {
  x = ifelse(x > 0, "Yes", "No")
}

rtLables=rtSparse$DV

RTSparseNB=rtSparse
RTSparseNB$DV=NULL

RTSparseNB=apply(RTSparseNB, MARGIN = 2,
                  convert_values)


RTNBModel= naiveBayes(RTSparseNB,rtLables)
RTNBModel

RTNB_pred = predict(RTNBModel, RTSparseNB)

table(rtSparse$DV,RTNB_pred)
