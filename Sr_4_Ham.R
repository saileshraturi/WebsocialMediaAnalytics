ham = read.csv("sms_spam.csv", stringsAsFactors=FALSE)

table(ham$target)


# Install new packages

library(tm)
library(SnowballC)
library(wordcloud)




corpusHam = Corpus(VectorSource(ham$text))



wordcloud(corpusHam,colors=rainbow(7),max.words=50)

# Convert to lower-case

corpusHam = tm_map(corpusHam, tolower)



corpusHam = tm_map(corpusHam, removePunctuation)


# Remove stopwords and apple

corpusHam = tm_map(corpusHam, removeWords, c(stopwords("english")))



# Stem document 

corpusHam = tm_map(corpusHam, stemDocument)

frequenciesHam = DocumentTermMatrix(corpusHam)




sparseHam = removeSparseTerms(frequenciesHam, 0.995)



HamSparse = as.data.frame(as.matrix(sparseHam))

# Make all variable names R-friendly

colnames(HamSparse) = make.names(colnames(HamSparse))

# Add dependent variable

HamSparse$DV = ham$target

# Build a CART model

library(rpart)
library(rpart.plot)

HamCART = rpart(DV ~ ., data=HamSparse, method="class")

prp(HamCART, extra=2)

hamPred=predict(HamCART,HamSparse, type = "class")
table(HamSparse$DV,hamPred)


library(randomForest)

HamSparse$DV=as.factor(HamSparse$DV)

HamRF=randomForest(DV~.,data=HamSparse)

varImpPlot(HamRF)

HamRFPred=predict(HamRF, HamSparse, type="response")
table(HamSparse$DV,HamRFPred)

library(e1071)

convert_values = function(x) {
  x = ifelse(x > 0, "Yes", "No")
}

hamLables=HamSparse$DV

HamSparseNB=HamSparse
HamSparseNB$DV=NULL

HamSparseNB=apply(HamSparseNB, MARGIN = 2,
                  convert_values)


HamNBModel= naiveBayes(HamSparseNB,hamLables)
HamNBModel

HamNB_pred = predict(HamNBModel, HamSparseNB)

table(HamSparse$DV,HamNB_pred)

