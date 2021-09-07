#loading required libraries
library(dplyr)
library(stringr)
library(dsEssex)
library(ggplot2)
library(lubridate)
library(wordcloud)
library(tm)
library(syuzhet)
library(tidytext)
library(textdata)
library(tibble)
library(reshape2)

#reading the data into R
#replacing empty strings with NA and eliminating them from the data
child_data=read.csv("55_The Wonderful Wizard of Oz.csv",stringsAsFactors = F,na.strings = c("", "NA"))
child_data = child_data%>% na.omit()
str(child_data)

head(child_data,10)

#building corpus on text variable since we dont require numeric data
child_corpus=Corpus(VectorSource(child_data$text))
inspect(child_corpus)

#cleaning text
#converting the entire text data into lowercase since R is case sensitive
child_corpus=tm_map(child_corpus,tolower)
#inspect(child_corpus)
#Removing punctuation marks
child_corpus=tm_map(child_corpus,removePunctuation)
#removing numeric data if present
child_corpus=tm_map(child_corpus,removeNumbers)
#removing stop words(commonly used unimportant words)
child_corpus=tm_map(child_corpus,removeWords, stopwords('english'))
#removing some user defined stop words
child_corpus=tm_map(child_corpus,removeWords,c('now','like','said','shall','will','can'))
#removing white spaces
child_corpus=tm_map(child_corpus,stripWhitespace)
inspect(child_corpus)

#term document matrix
tdm= TermDocumentMatrix(child_corpus)
tdm=as.matrix(tdm)
tdm[1:10,1:20]

#plotting most frequent words
w=rowSums(tdm)
w=subset(w,w>=50)
barplot(w,las=2,col=rainbow(50),ylab = "count",
        main="Most frequent Words")

#finding out the top 10 frequent words
sort_tdm <- sort(rowSums(tdm),decreasing=TRUE)
freq_words <- data.frame(word = names(sort_tdm),freq=sort_tdm)
#top 10 most frequent words
head(freq_words, 10)

#barplot of most frequent words
barplot(freq_words[1:10,]$freq,names.arg = freq_words[1:10,]$word,col="violet",
        main="Most frequent words")

#wordcloud
set.seed(222)
wordcloud(words = freq_words$word,
          freq = freq_words$freq,
          max.words = 300,
          random.order = F,
          min.freq = 3,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(3, 0.3), rot.per = 0.5)

#getting sentiment score of each word 
syuzhet_vector <- get_sentiment(child_data$text, method="syuzhet")
syuzhet_vector

score=iconv(child_data$text)
s=get_nrc_sentiment(score)
head(s)

#getting sentiment score of a specific word
child_data[2,]
get_nrc_sentiment('Wizard')

#classification of emotions
barplot(colSums(s),las=2,col=rainbow(10),main = "Emotions of The Wizard of Oz")

#2
#getting sentiments from 2 seperate dictionaries
get_sentiments("afinn")
get_sentiments("bing")

#reading data
adult_data=read.csv("1260_Jane Eyre-An Autobiography.csv",na.strings = c("","NA"))
adult_data = adult_data%>%
  select(text)%>%
  na.omit()

head(adult_data)

#seperate into tokens
databyword=adult_data%>%
  mutate(linenumber=row_number())%>%
  unnest_tokens(word,text)
databyword

#removing stop words
data(stop_words)
clean_data=databyword%>%
  anti_join(stop_words)

#count most common words
clean_data%>%
  count(word,sort=T)

#plotting common words
p=clean_data%>%
  count(word,sort=T)%>%
  filter(n>150)%>%
  mutate(word=reorder(word,n))%>%
  ggplot(aes(word,n))+
  geom_col()+
  xlab("words")+
  ylab("Count")+
  coord_flip()
p+ggtitle("Popular words of Jane Eyre")

#creating wordcloud
set.seed(123)
clean_data%>%
  anti_join(stop_words)%>%
  count(word)%>%
  with(wordcloud(word,n,max.words=150))

#comparing negative and positive words
clean_data%>%
  inner_join(get_sentiments("afinn"))%>%
  inner_join(get_sentiments("bing"))%>%
  anti_join(stop_words)%>%
  count(word,sentiment,sort=TRUE)%>%
  acast(word~sentiment,value.var="n",fill=0)%>%
  comparison.cloud(colors=c("red","blue"),
                   max.words=100)

#sentiment and count of words
sentiments=clean_data%>%
  inner_join(get_sentiments("bing"))%>%
  count(word,sentiment,sort=T)
head(sentiments)

#differentiating positive and negative words
pos_sentiment=subset(sentiments,sentiment=="positive")
neg_sentiment=subset(sentiments,sentiment=="negative")

#number of positive and negative words.
pos_score=aggregate(n~sentiment,data=pos_sentiment,sum)
neg_score=aggregate(n~sentiment,data=neg_sentiment,sum)

#classification of emotions
emotions_adult=iconv(adult_data$text)
s1=get_nrc_sentiment(emotions_adult)
tail(s1,10)
barplot(colSums(s1),las=2,col=rainbow(10),main="Emotions of Jane Eyre")




