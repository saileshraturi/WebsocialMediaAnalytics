# Read in the data

tweets = read.csv("02_tweets.csv", stringsAsFactors=FALSE)



# Create dependent variable

tweets$Negative = as.factor(tweets$Avg <= -1)

table(tweets$Negative)


# Install new packages

install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)


# Create corpus

corpus = Corpus(VectorSource(tweets$Tweet))

# Visualizing Corpus for Frequent Terms

install.packages("wordcloud")

library(wordcloud)
wordcloud(corpus,colors=rainbow(7),max.words=50)

# Convert to lower-case

corpus = tm_map(corpus, tolower)


# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred after this video was recorded.

# corpus = tm_map(corpus, PlainTextDocument)


# Remove punctuation

corpus = tm_map(corpus, removePunctuation)



# Look at stop words 
stopwords("english")[1:10]

# Remove stopwords and apple

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))



# Stem document 

corpus = tm_map(corpus, stemDocument)

frequencies = DocumentTermMatrix(corpus)



# Look at matrix 

inspect(frequencies[1000:1005,505:515])

# Check for sparsity

findFreqTerms(frequencies, lowfreq=20)

# Remove sparse terms

sparse = removeSparseTerms(frequencies, 0.995)


# Convert to a data frame

tweetsSparse = as.data.frame(as.matrix(sparse))

# Make all variable names R-friendly

colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

# Add dependent variable

tweetsSparse$Negative = tweets$Negative

# Build a CART model

library(rpart)
library(rpart.plot)

tweetCART = rpart(Negative ~ ., data=tweetsSparse, method="class")

prp(tweetCART,extra=2)

# Evaluate the performance of the model
predictCART = predict(tweetCART, data=tweetsSparse, type="class")

table(tweetsSparse$Negative, predictCART)

# Compute accuracy



# Baseline accuracy 

table(tweetsSparse$Negative)



# Random forest model

library(randomForest)
set.seed(123)

tweetRF = randomForest(Negative ~ ., data=tweetsSparse)
varImpPlot(tweetRF)
# Make predictions:
predictRF = predict(tweetRF, data=tweetsSparse)

table(tweetsSparse$Negative, predictRF)

