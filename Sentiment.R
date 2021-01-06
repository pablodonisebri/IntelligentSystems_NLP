library(tm)
# The openNLPmodels.en library is not in CRAN; it has to be installed from another repository
#install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at")
library(NLP)
library(openNLP) 
library(openNLPmodels.en)

#These are the packages that I import, the above ones are taken from Raul's Hands on
library(tidyverse)
#For renaming the columns in the dataset with the column rename
library(plyr)
#We change our working directory 


#For getting the tokens from the function tokenize_words()
library(tokenizers)

#We use the pakcage tidytext for getting the lexicon that we will we need 
#the function we will use is get_sentiments
library(tidytext)

#Auxiliar functions created by myself, they will be used later on for the work


#Function for getting the text into a raw form (as Raul does in the HandsOn)
prepareText= function(doc){
  #we remove the stop words
  #Be careful with this because it might remove words like not, see stopwords()
  doc = tm_map(doc,removeWords,stopwords())
  #we remove the punctuation
  doc = tm_map(doc,removePunctuation)
  #we remove the numerical characters
  doc = tm_map(doc,removeNumbers)
  #We remove the extra white spaces
  doc = tm_map(doc,stripWhitespace)
  #we put each word into its primitive form
  #With stem we might lose the positive or negative meaning of some words so better not to do it
  #doc = tm_map(doc,stemDocument)
  #we return the preprocessed text
  return(doc)
}


#This function returns the count of positive and negative sentiments in a dataframe
get_sentiments_from_text = function(doc){
  doc<-prepareText(doc)
  #Verbose syntax for accesing the content of the text and converting to format for the join
  word<-tokenize_words(doc$content)[[1]]
  tokens<- as.data.frame(word,columns=c("word"))
  
  #We want to see with the inner join the number of sentiments that appear in the text and that 
  #are found in the lexicon, we add supressMesagge to remove a message that appeared
  #sentiments_in_text<-inner_join(tokens, get_sentiments("bing"))
  sentiments_in_text<-suppressMessages(inner_join(tokens, get_sentiments("bing")))
  
  # we have to group the sentiments in the text in positive or negative to make the count 
  sentiments_in_text.grouped<- group_by(sentiments_in_text,sentiment)
  #we make the count of positive and negative sentiments
  sentiments.count<-sentiments_in_text.grouped %>% dplyr::count(sentiment)
  
  #we got to make an exception for when there is no positive words or negative words the function will return
  # the same number for both and that is an error.
  if(length(sentiments.count$sentiment)==0) return(data.frame(sentiment=c('positive', 'negative'),
                                                              count= c(0,0)) )
  if(length(sentiments.count$sentiment)==1){
    if(sentiments.count$sentiment=='positive') return( data.frame(sentiment=c('positive', 'negative'),
                                                                  count= c(sentiments.count$n,0)))
    else return( data.frame(sentiment=c('positive', 'negative'),
                            count= c(0,sentiments.count$n)))
  }
  
  #we get the count of negative sentiments 
  negative.count<-sentiments.count$n[sentiments.count$sentiment=='negative']
  #We get the count of positive sentiments 
  positive.count<-sentiments.count$n[sentiments.count$sentiment=='positive']
  return( data.frame(sentiment=c('positive', 'negative'),
                     count= c(positive.count,negative.count)))
}


#Function for returning whether a text is positive or negative from the dataframe
#we make in the function get_sentiments_from_text
sentiment<- function(doc){
  sentiments.df<- get_sentiments_from_text(doc)
  positives.count<- sentiments.df$count[sentiments.df$sentiment=='positive']
  negatives.count<- sentiments.df$count[sentiments.df$sentiment=='negative']
  #if more positive words are found then we classify the text as positive
  if(positives.count > negatives.count) return('positive')
  #if more negative words are found then we classify the text as negative
  else if(positives.count < negatives.count) return('negative')
  #If the same number of positive and negative words then the text is neutral
  else return('neutral')
  
}


#At this point starts the analysis and work of the analyis of sentiments from
#reviews of amazon products 


#Data  was got from: https://www.kaggle.com/datafiniti/consumer-reviews-of-amazon-products?select=1429_1.csv

#We read the csv from a URL so there is no need to store the file anywhere
library(RCurl)
url <- getURL("https://raw.githubusercontent.com/pablodonisebri/IntelligentSystems_NLP/master/1429_1.csv")
data <- read.csv(text = url)

#We only want the name of the product and the reviews column for this work 
reviews<- as.data.frame(dplyr::select(data,c('reviews.title','reviews.text','name')))

#In order to create a corpus from the dataframe we need a column with the document id
#and we will use the title of the review for that, another column named text and the rest 
#will be metadata (the name of the product the review is about)
reviews<-plyr::rename(reviews, c('reviews.title'='doc_id', 'reviews.text'='text','name'='product'))

#We want to convert the dataframe into a corpora for using the functions that we have for the handsOn
corpus = Corpus(DataframeSource(reviews))





#We get the lexicon, we are using the Bing lexicon 
listSentiments<-get_sentiments("bing")
#We build the list of positive words from the lexicon
sentiments.positive<- listSentiments$word[listSentiments$sentiment=="positive"]
#we build the list of negative words from the lexicon 
sentiments.negative<-listSentiments$word[listSentiments$sentiment=="negative"]

#we take a look at them 
sentiments.positive

sentiments.negative




#Now we want to classify each document
text.sentiments<-c()
for(i in 1:length(corpus)) {
  text.sentiments<-c(text.sentiments,sentiment(corpus[i]))
  }
  
#We look at a positive comment 
text.sentiments[1000]
corpus[1000]$content

#We look at a negative comment 
text.sentiments[491]
corpus[491]$content

#We look at a neutral  comment 
text.sentiments[109]
corpus[109]$content

#We look at a missclassified instance
#We see that is negative
text.sentiments[39] 
#The content shows that is not negative (not disapointed is not classfied well)
corpus[39]$content


#We add to the metadata the sentiment of the text
meta(corpus,tag = 'sentiment') <- text.sentiments

#The positive comments grouped in a corpus
corpus.positiveComments<-corpus[meta(corpus)$sentiment == 'positive']
#The negative comments grouped in a corpus
corpus.negativeComments<-corpus[meta(corpus)$sentiment == 'negative']
#The neutral comments grouped in a corpus
corpus.neutralComments<-corpus[meta(corpus)$sentiment == 'neutral']

#We can now do a bit of analysis in the positive comments corpus to see the words that 
#are found more frequently (taken from the handsOn code)
tdm.positive = TermDocumentMatrix(corpus.positiveComments)

#We get the count for the frequency of each word (taken from the handsOn code)
freq<-rowSums(as.matrix(tdm.positive))
#We want to see the frequency of the positive sentiments
sentiments.positive.frequency<-freq[sentiments.positive]
#We drop the null values that appeared in the previous line of code
sentiments.positive.frequency<- na.omit(sentiments.positive.frequency)
#We sort the results in a descending manner
sentiments.positive.frequency<- sort(sentiments.positive.frequency,decreasing=T)
#We look at the top 20
head(sentiments.positive.frequency,20)




#We can do the same for negative words in the comments
tdm.negative = TermDocumentMatrix(corpus.negativeComments)


freq<-rowSums(as.matrix(tdm.negative))
sentiments.negative.frequency<-freq[sentiments.negative]
sentiments.negative.frequency<- na.omit(sentiments.negative.frequency)

sentiments.negative.frequency<- sort(sentiments.negative.frequency,decreasing=T)

head(sentiments.negative.frequency,20)


