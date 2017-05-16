
##Tweets by day
rweets <- searchTwitter("#SaySomethingCompletelyHonest", n = 9999, since = "2016-7-6", until = "2016-7-7")
rweets.meta <- twListToDF(rweets) ##metadata for tweets


##search evo2016 by game
yweets <- searchTwitter("#TEKKEN7+#CEO2016", n = 9999)
yweets.meta <- twListToDF(yweets) ##metadata for tweets


##most favorited tweet
twListToDF() ##order by favorites

##most retweeted tweet
twListToDF() ##order by retweets


## users with most tweets
summary(as.factor(rweets.meta$screenName))

##graph by timestamp
ggplot(data = rweets.meta, aes(x = created)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")



scale_fill_gradient(low = "midnightblue", high = "aquamarine4")