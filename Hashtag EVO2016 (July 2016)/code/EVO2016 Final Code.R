######Load packages#####
library(twitteR) ##acess Twitter
library(wordcloud) ##make wordloud
library(tm) ##text mining
library(ggplot2)
library(data.table)
#####Set up authorization to acess Twitter#####
twit <- setup_twitter_oauth(consumer_key = "Kah8BprmHGgrzXme2GdfvVg0R", 
                            consumer_secret = "FM3yv08vLW9Xtf0tEPd3JNuvBoEONq4lzkUvTupIiJYPt3fLwf", 
                            access_token = "240510227-KPcXGxD0zudgaM0TnuEVfcbvcIeRL0Nmm6FydOW0", 
                            access_secret = "m2QRf6CCPqwS8j009d8aHIIrEiXPiRk7t9Pgfs3sP0Vx2")

#####Getting/Saving Tweets#####

##Getting Tweets Day 1 (wasn't able to acess tweets because they were 10 days old by the 
##time of attempt)
tweets1 <- searchTwitter("#EVO2016", n=21589, since = "2016-7-15", until = "2016-7-16")
tweets.meta1 <- twListToDF(tweets1) ##metadata for tweets
tweets.text1 <- sapply(tweets1, function(x) x$getText())
##approximately 21589 tweets

##Getting Tweets day 2
tweets2 <- searchTwitter("#EVO2016", n=39271, since = "2016-7-16", until = "2016-7-17")
tweets.meta2 <- twListToDF(tweets2) ##metadata for tweets
tweets.text2 <- sapply(tweets2, function(x) x$getText())
##approximately 39271 tweets

## Getting Tweets day 3
tweets3 <- searchTwitter("#EVO2016", n=76350, since = "2016-7-17", until = "2016-7-18")
tweets.meta3 <- twListToDF(tweets3) ##metadata for tweets
tweets.text3 <- sapply(tweets3, function(x) x$getText())
##approximately 137455 tweets

save(tweets2, file = "tweets2.RData") ##Raw twiiter data
write(tweets.text2, "tweets2.txt") ##list of tweets
write.csv(tweets.meta2, "tweets2.csv") #data frame of tweets

save(tweets3, file = "tweets3.RData") ##Raw twiiter data
write(tweets.text3, "tweets3.txt") ##list of tweets
write.csv(tweets.meta3, "tweets3.csv") #data frame of tweets


##Loading Saved Tweet data frames
tweets2 <- read.csv("C:/Users/Emeka/Desktop/tweets2.csv", comment.char="#")
tweets3 <- read.csv("C:/Users/Emeka/Desktop/tweets3.csv", comment.char="#")
tweets.meta <- data.table(rbind(tweets2, tweets3))
##Sometimes the "created" variable converts to factor instead date.
##Convert back to date-time with this code
tweets.meta$created <- as.POSIXct(strptime(as.character(tweets.meta$created),'%Y-%m-%d %H:%M:%S'))
rm(tweets2, tweets3)


write.csv(tweets.meta, "tweets.csv") #data frame of tweets

tweets.text <- c(tweets.text2, tweets.text3)

#####EVO at a Glance#####
##Number of Tweets
length(tweets.text)
##115584 (plus the 21589 missing tweets from friday = grand total of 137173)


##Number tweets by day
21589 from friday
length(tweets.text2)
39234
length(tweets.text3)
76350


##Time Chart

ggplot(data = tweets.meta, aes(x = created)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Time (Eastern Standard)") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "red") +
  scale_y_continuous(expand = c(0, 500)) 

#####Word Cloud#####

tweets <- tweets.meta$text
tweets.text <- gettext(tweets)


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


#create corpus
tweets.text.corpus <- Corpus(VectorSource(tweets.text))
#clean up by removing stop words
tweets.text.corpus <- tm_map(tweets.text.corpus, function(x)removeWords(x,stopwords()))

###Optional Create Term Document Matrix
##tweets.tdm <- TermDocumentMatrix(tweets.text.corpus, control = list(minWordLength = 1000))
##tdm.matrix <- as.matrix(tweets.tdm)
##word.count <- sort(rowSums(tdm.matrix), decreasing = TRUE)



##WOrd Coud
wordcloud(tweets.text.corpus,min.freq = 100000, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),  random.color= TRUE, random.order = FALSE, max.words = 100)

#####EVO Top 8#####

##Top 8 most favorited tweets
head(select(tweets.meta, text, favoriteCount, screenName)[order(-favoriteCount)], n = 8) 

##Top 8 most retweeted tweets
head(select(tweets.meta, text, retweetCount, screenName)[order(-retweetCount)], n = 1) 

##Users who tweeted the most
head(summary(as.factor(tweets.meta$screenName)), n = 8)
