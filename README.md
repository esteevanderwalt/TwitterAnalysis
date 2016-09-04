# TwitterAnalysis
## Analysis of Twitter data as part of PhD work
Using R-Studio and data scraped from Internet. Some sample data is available.
For our current purpose we either have the data hosted in Postgres or SAP HANA.

The goal is to extract features from the tweets to ultimately build a identity deception indicator. This deception indicator's purpose will be to highlight potential accounts which does not belong to the current corpus.
The current corpus was scraped using specific key words to identity a target group of people. In our case we used the words 'school' and 'homework' as these will hopefully give us a corpus of children or young adults.

First we need to analyse our data for features to be used as part of the identity deception indicator.
But what information have we got available per tweet?

Understanding the content of the tweets
- Amount of tweets in corpus
- Dates of tweets
- Times of tweets
- Time of tweet per timezone
- Time of tweet per location
- Amount of retweets
- Tweets with URL link
- Retweets vs followers
- Retweets vs URL link

Sentiment analysis of tweets
- For corpus
- For one person
- Sentiment vs location

Location analysis of tweets
- extract locations from content
- compare content with actual stated location
- compare location with GEO indicators

Twitter images
- amount of empty images
- tagging profile pictures
- tagging content images
- build potential word cloud
- compare images to sentiment

Deception
- Finding outliers in sentiment (individuals whose content as further from the norm)
  * per all
  * per continent
  * accross continent
- Finding outliers in location
  * individuals who are further from locations they are talking about
  * tweets times are weird for their location
- Images
  * default images
  * non human
  * not related to content

Deception indicator
- euclidian distance
- entropy
- elbow method
- poison distribution
- chi squared test
    
Realtime
- use indicator to point out potential threats as time pass
