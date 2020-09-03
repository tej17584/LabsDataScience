install.packages("twitteR")
install.packages("RCurl")
install.packages("wordcloud")
install.packages("tm")

library(twitteR)
library(RCurl)
library(wordcloud)
library(tm)

consumerKey<-"K8x1yVtCpHvMnMzVhR46yuu19"
consumerSecret<-"dYhw54JKCEKEhewHWSGPvJ4YxRokT3Ov1bfVmUbf87wnfXSPmc"

accessToken <-	"409713602-aBMMJOsk0Vc1zWCOUSyBPz56nnboWjHLYsyKNeM9"
accessTokenSecret <-	"XK3jfmZjB8QOGAoTMndpZLs3HFvjIhtmi02gAYCf44sUH"

setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)
tweets<-searchTwitteR("traficogt",n=150,lang = "es")
tweetdf<-twListToDF(tweets)
dif_UTC_gt<-6*60*60
tweetdf$created2<-tweetdf$created-dif_UTC_gt
write.csv(tweetdf,"tweets_aux.csv")

#getFollowers
user<-getUser("NuestroDiario")
location(user)
followers<-user$getFollowers()
