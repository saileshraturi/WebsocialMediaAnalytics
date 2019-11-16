google=read.csv("google.csv", stringsAsFactors = FALSE)
googleCorpus=Corpus(VectorSource(google$Search.Query))
wordcloud(googleCorpus, max.words = 50)
googleDTM=DocumentTermMatrix(googleCorpus)
googleDTM=removeSparseTerms(googleDTM,0.995)


googleDF=as.data.frame(as.matrix(googleDTM))

google2=read.csv("google.csv")

googleDF$ap=as.numeric(google2$Average.Position)

googleDF$Impression=NULL
googleLR=lm(ap~.,data=googleDF)
summary(googleLR)
