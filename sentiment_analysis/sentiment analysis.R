rm(list=ls())

######################
# Sentiment Analysis #
######################
library(syuzhet)
DAT<-read.csv("data.csv", stringsAsFactors = F)
Text<-DAT[85]
class(Text$Text)
s_v<-get_sentences(Text$Text)
sentiment<-get_sentiment(s_v, method="syuzhet")
summary(sentiment)
hist(sentiment)


nrc_data <- get_nrc_sentiment(s_v)

install.packages("pander")
library(pander)
pander::pandoc.table(nrc_data[, 1:8], split.table = Inf)
pander::pandoc.table(nrc_data[, 9:10])
?pandoc.table
valence <- (nrc_data[, 9]*-1) + nrc_data[, 10]
valence
## Use the following to write a csv file with the percentage 
## occurance with each response. 
table<-as.data.frame(colSums(prop.table(nrc_data[, 1:8]), na.rm=T))
test<-nrc_data[, 1:8]
?colSums()
?prop.table()
write.csv(table,"table.csv")

barplot(
  sort(colSums(prop.table(nrc_data[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions Regarding Reconfiguration", xlab="Percentage"
)

d<-as.data.frame(valence)


# function to get various sentiment scores, using the syuzhet package
scoreSentiment = function(tab)
{
  tab$syuzhet = get_sentiment(tab$Text, method="syuzhet")
  tab$bing = get_sentiment(tab$Text, method="bing")
  tab$afinn = get_sentiment(tab$Text, method="afinn")
  tab$nrc = get_sentiment(tab$Text, method="nrc")
  emotions = get_nrc_sentiment(tab$Text)
  n = names(emotions)
  for (nn in n) tab[, nn] = emotions[nn]
  return(tab)
}

# get the sentiment scores for the tweets
sent = scoreSentiment(Text)
write.csv(sent, "sent2.csv")

prop.table(sent$anger)



prop.table(table(sent$trust))
tbl <- table(sent$trust)

tblFun <- function(x){
tbl <- table(x)
res <- cbind(tbl,round(prop.table(tbl)*100,2))
colnames(res) <- c('Count','Percentage')
res
}

table<-do.call(rbind,lapply(sent[7],tblFun))


######################
#    Word Clouds     #
######################
library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(wordcloud)
library(wordcloud2)

corpus = Corpus(VectorSource(Text$Text))
corpus =tm_map(corpus, tolower)
corpus=tm_map(corpus, removePunctuation)
corpus=tm_map(corpus, removeNumbers)
corpus=tm_map(corpus, removeWords, c("School", "etc", "will", "also", "just", "sure", stopwords("english")))


#wordcloud from data.frame
dtm <-DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.995)
m<-as.matrix(dtm)
v<-sort(colSums(m))
words<-names(v)
d<-data.frame(word=words, freq=v)
#write.csv(d, "OE Word Frequency Table.csv")
wordcloud(d$word, d$freq, min.freq=5,rot.per = 0, colors=brewer.pal(3,"Blues"), random.color = F, random.order = F )
# The order of words is completely random, but the size is directly proprotional to the frequency of occurence of the word in text.
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf 


frequencyT<-slam::row_sums(TermDocumentMatrix(corpus))
F<-as.data.frame(frequencyT)

write.csv(F, "Frequency Table.csv")
T<- read.csv("Frequency Table.csv")
#T<-filter(T,frequencyT>4)

wordcloud2(T, figPath="C:\\Users\\acarroll\\Desktop\\R Learning Lunch - 5-19-17 Text Analysis\\apple.png", size=1, color ="black")
wordcloud2(T, figPath="C:\\Users\\acarroll\\Desktop\\R Learning Lunch - 5-19-17 Text Analysis\\cap.png", size=1, color=brewer.pal(8,"Blues"))
wordcloud2(T, fontFamily = "Calibri", color=brewer.pal(3,"Blues"), shape="circle")
?wordcloud2()
