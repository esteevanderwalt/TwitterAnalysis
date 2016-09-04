# TwitterAnalysis
Analysis of Twitter data as part of PhD work
- using R-Studio
- data scraped from Internet

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
    -> per all
    -> per continent
    -> accross continent
- Finding outliers in location
    -> individuals who are further from locations they are talking about
    -> tweets times are weird for their location
- Images
    -> default images
    -> non human
    -> not related to content

Deception indicator
    -> euclidian distance
    -> entropy
    -> elbow method
    -> poison distribution
    -> chi squared test
    
Realtime
    -> use indicator to point out potential threats as time pass
