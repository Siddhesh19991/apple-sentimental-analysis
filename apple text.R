apple <- read.csv("~/Downloads/apple.csv")


library(tm)
corpus <- iconv(apple$text, to = "utf-8") #to get only the text from the data
corpus<-Corpus(VectorSource(corpus))

#cleaning 

corpus<-tm_map(corpus,tolower)
corpus<-tm_map(corpus,removePunctuation)
corpus<-tm_map(corpus,removeNumbers)
corpus<-tm_map(corpus,removeWords,stopwords("english"))



removeURL<-function(x)gsub('http[[:alnum:]]*','',x)  #to remove URLs from the data
corpus<-tm_map(corpus,content_transformer(removeURL))


corpus<-tm_map(corpus,removeWords,c("aapl","apple","headã¢â,¬â¦"))
corpus<-tm_map(corpus,stripWhitespace)
corpus<-tm_map(corpus,gsub,pattern="stocks",replacement="stock")

#DTM


dtm<-DocumentTermMatrix(corpus)


fre<-colSums(as.matrix(dtm))
ord<-order(fre,decreasing = TRUE)
fre[head(ord,10)]
fre[tail(ord)]

w<-subset(fre,fre>50)
barplot(w,las=2)
#earnings is the most common word as this data comes right before the release of the earnings report of APPLE.



library(wordcloud)
library(slam)
library(RColorBrewer)
set.seed(222)

wordcloud(words=names(fre),freq=fre,colors = rainbow(20))


#correlation

corr<-findAssocs(dtm,c("iphone","sylvacap","stock"),0.6)

#sentiment analysis
library(syuzhet)

Tweets<- iconv(apple$text, to = "utf-8")
score<-get_nrc_sentiment(Tweets)

barplot(colSums(score),las=2)
#their is a negaive atmosphere about the earings of APPLE.