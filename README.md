# TwitterAnalysis
## Analysis of Twitter data as part of PhD research work
Using R-Studio and data scraped from Internet. Some sample data is available.
For our current purpose we either have the data hosted in [Postgres or SAP HANA](/DBConnection).

The goal is to extract features from the tweets to ultimately build a identity deception indicator. This deception indicator's purpose will be to highlight potential accounts which does not belong to the current corpus.
The current corpus was scraped using specific key words to identity a target group of people. In our case we used the words 'school' and 'homework' as these will hopefully give us a corpus of children or young adults.

First we need to analyse our data for features to be used as part of the identity deception indicator.
But what [information](/TweetInfo/TweetStructure.md) have we got available per tweet?

####Understanding the [content of the tweets](/TweetInfo/TweetCorpusInfo.md)
- [x] Amount of tweets in corpus
- [x] Dates of tweets
- [x] Times of tweets
- [x] Time of tweet per timezone
- [ ] Time of tweet per location
- [ ] Amount of retweets
- [ ] Tweets with URL link
- [ ] Retweets vs followers
- [ ] Retweets vs URL link

####Sentiment analysis of tweets
- For corpus
- For one person
- Sentiment vs location

Predictive techniques applied
- Sentiment analysis

####Location analysis of tweets
- extract locations from content
- compare content with actual stated location
- compare location with GEO indicators

Predictive techniques applied
- predict timezone based on location (does actual agree?)
- predict location based on timezone (does actual agree?)

####Twitter images
- amount of empty images
- tagging profile pictures
- tagging content images
- build potential word cloud
- compare images to sentiment

Predictive techniques applied
- image recognition

####Origin of tweets
- Find which phone was used for tweeting
- What is the spread amongst the corpus

####Comparison to other social media/web content
- find similar image on web connected to similar name

####Other
- Gender indicator
- How populated profile is
- Amount of followers
- Followers vs commonality of name
- retweet ratio vs 1to1 comms
- account start dates vs ?
- time since last tweet
- name on criminal database


##Deception
1. Finding outliers in sentiment (individuals whose content as further from the norm)
  * per all
  * per continent
  * accross continent
2. Finding outliers in location
  * individuals who are further from locations they are talking about
  * tweets times are weird for their location
3. Images
  * default images
  * non human
  * not related to content
4. How many people have more than one phone

####Deception indicator
- euclidian distance
- entropy
- elbow method
- poison distribution
- chi squared test
    
####Realtime
- use indicator to point out potential threats as time pass
