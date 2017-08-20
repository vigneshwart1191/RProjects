##**********Steps to Set up authorization to connect and extract tweets********

library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(httr)
library(wordcloud)
library(tm)
library(Rstem)
library(sentiment) ##need to install package at the last

oauth_endpoint(authorize = "https://api.twitter.com/oauth",
               access = "https://api.twitter.com/oauth/access_token")

#connect to API
download.file(url='http://curl.haxx.se/ca/cacert.pem', destfile='cacert.pem')
reqURL <- 'https://api.twitter.com/oauth/request_token'
accessURL <- 'https://api.twitter.com/oauth/access_token'
authURL <- 'https://api.twitter.com/oauth/authorize'

### Twitter Application
consumerKey="n7xyzTCSp81Ybi6cBpDjWLKRt"
consumerSecret="Q399VeeylVzlugZFWdnfVfXxWo1oUDxGOwSeUFDe9rkrS63xam"
accesstoken="147087358-UkpzsB8IWmza8zWyVxXmYpE7Yd7CWwXVaH5UXH7L"
accesssecret="vcJF64uAy4bfnOk5aW88YOH6mWOOQr9BuPFvWPcJUqh8Y"

Cred <- OAuthFactory$new(consumerKey=consumerKey,
                         consumerSecret=consumerSecret,
                         requestURL=reqURL,
                         accessURL=accessURL,
                         authURL=authURL)
Cred$handshake(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'RCurl')) #There is URL in Console. You need to go to it, get code and enter it on Console

##### Authorization PIN -DYNAMIC

save(Cred, file='twitter authentication.Rdata')

load('twitter authentication.Rdata') 
#Once you launch the code first time, you can start from this line in the future (libraries should be connected)

setup_twitter_oauth(consumer_key=consumerKey, consumer_secret=consumerSecret, access_token=accesstoken, access_secret=accesssecret)

##****************Step 3: Perform tweets extraction and data cleaning****************

# Harvest the tweets
all_tweets = searchTwitter("ops", n=2000, lang="en")

(n.all_tweets <- length(all_tweets))


# get the text
tweet_txt = sapply(all_tweets, function(x) x$getText())


# remove retweet entities
tweet_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweet_txt)
# remove at people
tweet_txt = gsub("@\\w+", "", tweet_txt)
# remove punctuation
tweet_txt = gsub("[[:punct:]]", "", tweet_txt)
# remove numbers
tweet_txt = gsub("[[:digit:]]", "", tweet_txt)
# remove html links
tweet_txt = gsub("http\\w+", "", tweet_txt)
# remove unnecessary spaces
tweet_txt = gsub("[ \t]{2,}", "", tweet_txt)
tweet_txt = gsub("^\\s+|\\s+$", "", tweet_txt)


# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}

# lower case using try.error with sapply 
tweet_txt = sapply(tweet_txt, try.error)
class(tweet_txt)
typeof(tweet_txt)
# remove NAs in tweet_txt
tweet_txt = tweet_txt[!is.na(tweet_txt)]
names(tweet_txt) = NULL




class_emo = classify_emotion(tweet_txt, algorithm="bayes", prior=1.0)
View(class_emo)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(tweet_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]

## *********Create data frame with the results and obtain some general statistics******
# data frame with results
sent_df = data.frame(text=tweet_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

sent_df1 = within(sent_df,
                  polarity <- factor(polarity, levels=names(sort(table(polarity), decreasing=TRUE))))


##*************Plot the obtained results*****************

# plot distribution of Emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets", title="classification based on emotion") 


## plot distribution of Polarity
ggplot(sent_df1, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="Dark2")+labs(x="polarity categories", y="number of tweets",title="classification based on polarity")

##### Cloud comparision
# First, separate the words according to emotions
tweet_emotions = levels(factor(sent_df$emotion))
n_tweet_emotions = length(tweet_emotions)
tweet_emotions_docs = rep(" ", n_tweet_emotions)
for (i in 1:n_tweet_emotions)
{
  tmp = tweet_txt[emotion == tweet_emotions[i]]
  tweet_emotions_docs[i] = paste(tmp, collapse=" ")
}


###### Remove stopwords- Data Cleaning Step*************** 

tweet_emotions_docs = removeWords(tweet_emotions_docs, stopwords(kind = "en"))

###### Vector creation to represent on A CORPORA- PACKAGE -tm

TweetData.corpus = Corpus(VectorSource(tweet_emotions_docs))
TweetData.tdm = TermDocumentMatrix(TweetData.corpus)
TweetData.tdm = as.matrix(TweetData.tdm)
colnames(TweetData.tdm) = tweet_emotions

# creating, comparing and plotting the words on the cloud
comparison.cloud(TweetData.tdm, colors = brewer.pal(n_tweet_emotions, "Dark2"),scale = c(4,.5), random.order = TRUE, title.size = 1.5)






