shark = read.csv("Shark Tank Companies.csv", stringsAsFactors=FALSE)

table(shark$deal)

library(tm)
library(SnowballC)
library(wordcloud)

corpusShark = Corpus(VectorSource(shark$description))



wordcloud(corpusShark,colors=rainbow(7),max.words=50)

# Convert to lower-case

corpusShark = tm_map(corpusShark, tolower)



corpusShark = tm_map(corpusShark, removePunctuation)


# Remove stopwords and apple

corpusShark = tm_map(corpusShark, removeWords, c(stopwords("english")))



# Stem document 

corpusShark = tm_map(corpusShark, stemDocument)

frequenciesShark = DocumentTermMatrix(corpusShark)




sparseShark = removeSparseTerms(frequenciesShark, 0.995)



SharkSparse = as.data.frame(as.matrix(sparseShark))

# Make all variable names R-friendly

colnames(SharkSparse) = make.names(colnames(SharkSparse))

# Add dependent variable

SharkSparse$DV = shark$deal
SharkSparse$DV=as.factor(SharkSparse$DV)
# Build a CART model

library(rpart)
library(rpart.plot)

SharkCART = rpart(DV ~ ., data=SharkSparse, method="class")

prp(SharkCART,extra=2)

library(randomForest)

SharkRF=randomForest(DV~.,data=SharkSparse)
varImpPlot(SharkRF)

SharkLogit=glm(DV~.,data=SharkSparse,family="binomial")
SharkPred=predict(SharkLogit,data=SharkSparse,type="response")
table(SharkSparse$DV,SharkPred>0.5)
(143+114)/495

SharkSparse$category=NULL

SharkLogit2=glm(DV~ratio,data=SharkSparse,family="binomial")
SharkLogit2Pred=predict(SharkLogit2,data=SharkSparse,type="response")
table(SharkSparse$DV,SharkLogit2Pred>0.5)

SharkSparse$ratio=(shark$askedFor/shark$valuation)

