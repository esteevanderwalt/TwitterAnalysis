# Feature Identification V2










```
## Loading required package: DBI
```

```
## [1] TRUE
```



##Investigate name related attributes
The following attributes of the user set has been identified to relate to names of the individual:

- Name (The name given by the account holder)
- Screenname (The name that will be displayed to others)

####Potential engineered name features

- Distance between name and screenname (levenshtein and hamming distance)
- Is a valid name?

##Residency related attributes
The following attributes of the user set has been identified towards potential residency deception:

- Location
- Langiage
- Timezone
- UTC Offset
- Geo Enabled
- Latitude
- Longitude

####Potential engineered residency features

- Distance between location and GEO (Haversine method)
- Distance between location and Timezone (Haversine method)
- Continent

##ID related attributes
The following attributes of the user set has been identified towards potential ID deception:

- Profile image
- Is default profile?
- Is default profile image?

####Potential engineered ID features

- Name gender
- Image gender
- No of faces in the image
- Unique image?

##DOB related attributes
The following attributes of the user set has been identified towards potential DOB deception:

- Created

####Potential engineered DOB features

- Image age

##Other

Some social media features could contibute

- Friends count
- Followers count
- Status count
- Listed count

####Engineered features
- Last tweet time
- Average tweet hour
- Sentiment
- Emotion
- No of devices
- Friend/Follower ratio

