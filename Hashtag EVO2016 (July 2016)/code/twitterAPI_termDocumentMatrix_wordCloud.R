## How to connect to Twitter's API, get Tweets, clean them up, make a term document matrix
## for word counts, and make a word cloud

##Load packages
library(twitteR) ##acess Twitter
library(wordcloud) ##make wordloud
library(tm) ##text mining
library(ggplot2)
## If you haven't already, create a Twitter account and an app to gain acess to the Twitter API.
## Link is here https://apps.twitter.com/
## Look in the Settings for your info and look in Keys and Tokens for other needed info.

## connecting looks like this
##
## consumer_key <- 'your key'
## consumer_secret <- 'your secret'
## access_token <- 'your access token'
## access_secret <- 'your access secret'
## setup_twitter_oauth(consumer_key,
##                     consumer_secret,
##                     access_token,
##                     access_secret)


##Set up authorization to acess Twitter
twit <- setup_twitter_oauth(consumer_key = "Kah8BprmHGgrzXme2GdfvVg0R", 
                            consumer_secret = "FM3yv08vLW9Xtf0tEPd3JNuvBoEONq4lzkUvTupIiJYPt3fLwf", 
                            access_token = "240510227-KPcXGxD0zudgaM0TnuEVfcbvcIeRL0Nmm6FydOW0", 
                            access_secret = "m2QRf6CCPqwS8j009d8aHIIrEiXPiRk7t9Pgfs3sP0Vx2")


## Test Connection
##dmSend(text = "test", user = "Don_Meka")

## Getting Tweets
tweets <- searchTwitter("#SaySomethingCompletelyHonest", n=1000)
tweets.meta <- twListToDF(tweets) ##metadata for tweets
tweets.text <- sapply(tweets, function(x) x$getText())


## Cleaning up Tweets for analysis
# convert all text to lower case
tweets.text <- tolower(tweets.text)
# Replace blank space ("rt")
tweets.text <- gsub("rt", "", tweets.text)
# Replace @UserName
tweets.text <- gsub("@\\w+", "", tweets.text)
# Remove punctuation
tweets.text <- gsub("[[:punct:]]", "", tweets.text)
# Remove links
tweets.text <- gsub("http\\w+", "", tweets.text)
# Remove tabs
tweets.text <- gsub("[ |\t]{2,}", "", tweets.text)
# Remove blank spaces at the beginning
tweets.text <- gsub("^ ", "", tweets.text)
# Remove blank spaces at the end
tweets.text <- gsub(" $", "", tweets.text)

##May have to run that sequence twice. Somtimes one conversion requires another to be run first
##depending on if a tweet has certain weird characters


#create corpus
tweets.text.corpus <- Corpus(VectorSource(tweets.text))
#clean up by removing stop words
tweets.text.corpus <- tm_map(tweets.text.corpus, function(x)removeWords(x,stopwords()))

#Optional Create Term Document Matrix
tweets.tdm <- TermDocumentMatrix(tweets.text.corpus, control = list(minWordLength = 1))
tdm.matrix <- as.matrix(tweets.tdm)
word.count <- sort(rowSums(tdm.matrix), decreasing = TRUE)


#generate wordcloud
wordcloud(tweets.text.corpus,min.freq = 2, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),  random.color= TRUE, random.order = FALSE, max.words = 150)

##graphing most used words
##convert to data.frame
df <- data.frame("word" = names(word.count), "count" = unname(word.count))
df$word <- factor(df$word, levels = df$word[order(df$count)])


##and graph
## note that often times the number 1 and 2 most popular words are just the "hashtag" and
## the "rthashtag" so i skipped the first two results. check hte word count data frame first though

ggplot(df[3:13,], aes(reorder(word, -count), count, fill = word)) + 
  geom_bar(stat = "identity") +
  labs(title = "Gamecube V.S. Competition") +
  labs(x = NULL, y = expression('Units Sold '[Millions]*'')) +
  theme(plot.title = element_text(face = "bold"), legend.position = "none") + 
  geom_text(aes(label = count), size = 4, color = "black", vjust = -.25)

