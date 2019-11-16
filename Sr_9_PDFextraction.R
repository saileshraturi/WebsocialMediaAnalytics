install.packages("pdftools")


library(pdftools)

text = pdf_text(choose.files())

text
library(tm)
library(syuzhet)
corpus=Corpus(VectorSource(text))
library(wordcloud)
wordcloud(corpus,colors=rainbow(7),max.words=50)

sentences=get_sentences(text)

sent.value=get_sentiment(sentences)

simple_plot(sent.value)


