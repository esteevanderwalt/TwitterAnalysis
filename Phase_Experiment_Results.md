# TwitterAnalysis
## The experiment results
##Deception
This is where the main work is done. There are many ideas I am playing with:
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

####Anomaly detection / Outlier detection
The definition for abnormal, or outlier, is an element which does not follow the behaviour of the majority.
- [x] Anomaly detection from Twitter (AnomalyDetection/TwitterAnomalyDetection.md)

####Deception indicator
- euclidian distance
- eigenvectors
- entropy
- elbow method
- poison distribution
- chi squared test
    
####Realtime
- use indicator to point out potential threats as time pass
- the score of a user can change
- older data should carry less weight




