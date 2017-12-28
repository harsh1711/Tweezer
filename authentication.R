#make sure you have installed it first by: 
#install.packages("twitteR") 
#install.packages("ROAuth")

library(ROAuth)
library(twitteR)

# access twitter API
#Insert your consumer key,consumer_secret,access_token,access_secret

consumer_key <- 'gl3I2S5RuBoZxdy4da0EY'
consumer_secret <- 'vb645D35mBc4HzGF5onJrFCW1ddw3dhECVzoBhegrzY2D'
access_token <- '299772531-tj2GvCzpnGj6JfAGdzFudyEAJdyE9hso3'
access_secret <- '947HM80CwE1D5mbuhyPYYQgBYCMwd0Q1IVYEH'
setup_twitter_oauth(consumer_key = consumer_key,
                    consumer_secret = consumer_secret,
                    access_token = access_token, access_secret = access_secret)
