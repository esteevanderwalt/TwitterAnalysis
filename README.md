# TwitterAnalysis
## Analysis of Twitter data as part of PhD research work
Using R-Studio and data scraped from social media (Twitter to be specific). 
For our current purpose we either have the data hosted in [Postgres or SAP HANA](/DBConnection).
There are [pro's and con's](/DBConnection/pro_con.md) to choosing the current environment.

The goal is to extract features from the tweets to ultimately build a identity deception indicator. This deception indicator's purpose will be to highlight potential accounts which does not belong to the current corpus.
The current corpus was scraped using specific key words to identity a target group of people. In our case we used the words 'school' and 'homework' as these will hopefully give us a corpus of children or young adults. We would like to ultimately identify outliers, ie people who are not children but talking to them (potentially identifying threats).

But lets first understand the [structures of tweets](/TweetInfo/TweetStructure.md) in general.
We then need to analyse our data for features to be used as part of our final identity deception indicator.

####Understanding the [content of the tweets](/TweetInfo/TweetCorpusInfo.md)
In this section we are only interested in understanding what data we have, whether it makes sense.
This might also help us to gain insight into the features we could look at or incorporate in our final identity deception indicator.
- [x] Amount of tweets in corpus
- [x] Dates of tweets
- [x] Times of tweets
- [x] Time of tweet per timezone
- [ ] Time of tweet per location
- [ ] Amount of retweets
- [ ] Tweets with URL link
- [ ] Retweets vs followers
- [ ] Retweets vs URL link
- [x] [Multi variate analysis] (/TweetInfo/MultiVarAnalysis.md)
- [x] [Outliers] (/TweetInfo/TweetOutliers.md)

####Feature extraction via clustering techniques
There are many techniques to extract features from high dimentional data. Here we will apply the techniques blindly to see whether we get some insight into what features could define who is tweeting.
- [x] [Kmeans] (/FeatureExtraction/Kmeans.md)
- [x] [Hierarchical clustering] (/FeatureExtraction/HierarchClust.md)
- [x] [Model based clustering] (/FeatureExtraction/ModelClust.md)
- [x] Self organized maps - [SOM] (/FeatureExtraction/SOM.md)

####Dimentionality reduction
In machine learning and statistics, dimensionality reduction or dimension reduction is the process of reducing the number of random variables under consideration, via obtaining a set of principal variables. It can be divided into feature selection and feature extraction.
- [x] Principal Component Analysis - [PCA] (/DimensionReduction/PCA.md)
- [ ] t-Distributed Stochastic Neighbor Embedding (t-SNE)

####Network graph analysis
In this section we would like to view the network over time, either how connections are formed and who talks to who. For this we are planning to extract nodes and edges from the data and use a tool (Gephi) to view the graph maps.
- [ ] [Friends and followers vs location](/NetworkAnalysis/FriendsVsFollowers.md)
- [ ] Network graph of only people who actually replied to others

####Sentiment analysis of tweets
In this section we aim to analyse the content of tweets. We are only interested in original tweets (no retweets) and we will clean the data a bit to perform both word frequence and sentiment analysis on the tweets.
- [x] [For one person](/Sentiment/SentimentAnalysisSingle.md)
- [x] [For corpus](/Sentiment/SentimentAnalysis.md)
- [ ] Sentiment vs location

####Location analysis of tweets
In this sections we are interested to understand the location if the tweets. We will try to understand what the correlation are between location, timezone, GEO indicators and potential placed mentioned in the tweet content itself
- [ ] extract locations from content
- [ ] compare content with actual stated location
- [ ] compare location with timezone
- [ ] compare tweet times to timezone
- [ ] compare location with GEO indicators
- [ ] combine all dimentions

####Twitter images
Here we would like to understand what the images can tell us about the person
- [ ] amount of empty images
- [ ] understanding if an image is a person or not
- [ ] tagging profile pictures -> machine learning to extract features from an image
- [ ] tagging content images
- [ ] build potential word cloud
- [ ] compare images to sentiment

####Origin of tweets
This section is simply to understand what devices the corpus is using. We could for example hypothesize that children to not have money for expensive devices like iPhones.
- [ ] Find which phone was used for tweeting
- [ ] What is the spread amongst the corpus
- [ ] Spread per continent

####Comparison to other social media/web content
In this section we would like to find the same person on another social media platform and compare features to understand if the person is giving the same information through accross different platforms.
- [ ] find similar names
- [ ] find similar image on web connected to similar name
- [ ] similar sentiment
- [ ] similar content

####Other
Here are some other ideas to mention:
- [ ] Gender indicator
- [ ] How populated/complete profile is
- [ ] Amount of followers
- [ ] Followers vs commonality of name
- [ ] retweet ratio vs 1to1 comms
- [ ] account start dates vs ?
- [ ] time since last tweet
- [ ] name on criminal database
- [ ] sentiment vs that of a criminal database


##Deception
This is where the main work is done. There are many idea I am playing with:
- just like books are compared to be similar, so we can compare twitter accounts
- build a 'DNA' string of a twitter accounts and compare them to find if they are similar

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
- eigenvectors
- entropy
- elbow method
- poison distribution
- chi squared test
    
####Realtime
- use indicator to point out potential threats as time pass
