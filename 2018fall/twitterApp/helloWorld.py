# October 8 2018
# Hannah Fresques
# try out twitter api

import tweepy

consumer_key = "mJaLOxPlXMLcDJ4YPb3RYK8NY"
consumer_secret = "aObhyPnD4I8MlUN4NkpX0p3FoTHsMUsoN7U26FOXr1q8nrtQoI"

auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)

api = tweepy.API(auth)

public_tweets = api.home_timeline()
for tweet in public_tweets:
    print(tweet.text)