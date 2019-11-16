# Read in the data

products = read.csv("00_products.csv", stringsAsFactors=FALSE)

products$is_there_an_emotion_directed_at_a_brand_or_product=as.factor(products$is_there_an_emotion_directed_at_a_brand_or_product)

table(products$is_there_an_emotion_directed_at_a_brand_or_product)




# Install new packages

install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)


# Create corpus

corpusProduct = Corpus(VectorSource(products$tweet_text))

# Visualizing Corpus for Frequent Terms

install.packages("wordcloud")

library(wordcloud)
wordcloud(corpusProduct,colors=rainbow(7),max.words=50)

# Convert to lower-case

corpusProduct = tm_map(corpusProduct, tolower)


# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred after this video was recorded.

# corpus = tm_map(corpus, PlainTextDocument)


# Remove punctuation

corpusProduct = tm_map(corpusProduct, removePunctuation)



# Look at stop words 
stopwords("english")[1:10]

# Remove stopwords and apple

corpusProduct = tm_map(corpusProduct, removeWords, c(stopwords("english")))



# Stem document 

corpusProduct = tm_map(corpusProduct, stemDocument)

frequencies = DocumentTermMatrix(corpusProduct)





# Check for sparsity

findFreqTerms(frequencies, lowfreq=20)

# Remove sparse terms

sparse = removeSparseTerms(frequencies, 0.995)


# Convert to a data frame

tweetsSparse = as.data.frame(as.matrix(sparse))

# Make all variable names R-friendly

colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

# Add dependent variable

tweetsSparse$Sentiment = products$is_there_an_emotion_directed_at_a_brand_or_product

# Build a CART model

library(rpart)
library(rpart.plot)

tweetCART = rpart(Sentiment ~ ., data=tweetsSparse, method="class")

prp(tweetCART,extra=2)

productPred=predict(tweetCART,tweetsSparse, type = "class")
table(tweetsSparse$Sentiment,productPred)


## Airline

airline = read.csv("00_airline.csv", stringsAsFactors=FALSE)

airline$airline_sentiment=as.factor(airline$airline_sentiment)

table(airline$airline_sentiment)




# Install new packages

install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)


# Create corpus

corpusAirline = Corpus(VectorSource(airline$text))

# Visualizing Corpus for Frequent Terms

install.packages("wordcloud")

library(wordcloud)
wordcloud(corpusAirline,colors=rainbow(7),max.words=50)

# Convert to lower-case

corpusAirline = tm_map(corpusAirline, tolower)


# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred after this video was recorded.

# corpus = tm_map(corpus, PlainTextDocument)


# Remove punctuation

corpusAirline = tm_map(corpusAirline, removePunctuation)



# Look at stop words 
stopwords("english")[1:10]

# Remove stopwords and apple

corpusAirline = tm_map(corpusAirline, removeWords, c(stopwords("english")))



# Stem document 

corpusAirline = tm_map(corpusAirline, stemDocument)

frequenciesAirline = DocumentTermMatrix(corpusAirline)





# Check for sparsity

findFreqTerms(frequenciesAirline, lowfreq=20)

# Remove sparse terms

sparseAirlines = removeSparseTerms(frequenciesAirline, 0.995)


# Convert to a data frame

airlineSparse = as.data.frame(as.matrix(sparseAirlines))

# Make all variable names R-friendly

colnames(airlineSparse) = make.names(colnames(airlineSparse))

# Add dependent variable

airlineSparse$Sentiment = airline$airline_sentiment

# Build a CART model

library(rpart)
library(rpart.plot)

airlineCART = rpart(Sentiment ~ ., data=airlineSparse, method="class")

prp(airlineCART,extra=2)

