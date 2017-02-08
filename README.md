# TwitterAnalysis
## Analysis of Twitter data as part of PhD research work
Using R-Studio and data scraped from social media (Twitter to be specific). 
For our current purpose we either have the data hosted in [Postgres or SAP HANA](/DBConnection).
There are [pro's and con's](/DBConnection/pro_con.md) to choosing the current environment.

The goal is to extract features from the tweets to ultimately build an identity deception indicator. This deception indicator's purpose will be to highlight potential accounts which does not belong to the current corpus. For our study we will inject dummy harmful accounts.

The current corpus was scraped using specific key words to identify a target group of people. In our case we used the words 'school' and 'homework' as these will hopefully give us a corpus of children or young adults. Note these two words were found to be most common in a research study on internet usage across age bands. We would like to ultimately identify outliers, i.e. people who are not children but talking to them (potentially identifying threats).

Hypothesis to potentially answer during our study:

1. Features can be engineered to enrich an identity's profile.
2. Identity features can be used to identify deception in social media data.

Some [definitions](/Definitions/Definitions.md)

The study consists of the following phases

1. [Understanding the data](/Engine/Phase_Data_Mining.md)
2. The experiment - [the features](/Engine/Phase_Experiment_Variables.md)
3. [The results](/Engine/Phase_Experiment_Results.md)
4. Future Potential work

##The Identity Deception Detection Engine
The identity deception engine that is built has part of this research has a few main components

1. Factor selection/engineering
2. The deception classification
3. Evaluation of results
4. A IDI over time

###Inner workings of the engine


####Data cleaning
Here we will start with a trained dataset. 
(Data Cleaning) The following was done to clean data

1. Remove all retweets
2. Remove all accounts with more than 1000 tweets a day (probably bot)
3. Remove all accounts with more than 10000 followers (probably celeb)
4. Remove all accounts marked as verified (probably celeb)
5. Remove all accounts with less than 10 tweets (new accounts)
6. Remove all users which do not exist anymore (probably closed/banned)


####Data protection
With regards to privacy protection the following measures have been taken

1. Names are obfuscated by not reporting on them


####Data enrichment for feature engineering
A few additional features will be added via various means

1. Names (levenshtein distance between screenname and actual account name, does the actual name exist, get gender of name)
2. Residency (get the coords of the stated location and timezone, calculate distances between location, timezone and lon/lat)
3. ID (get details from the images, whether it is face or not, age, gender, amount of people in picture)
4. Social (get sentiment of tweets and hashtags, get the distance per user against that of total group)


####Feature extraction
The next step is to identify those identity features that could indicate deception.
The idea is to take each attribute, find correlations to others and determine whether they will be useful input to a anomoly detection model.


####Injection of dummy accounts
Some dummy accounts will be generated that are deceptive. These will be injected into the result set


####Scoring
For each user a deception score (DS) will be calculated per day.
An overall IDI (identity deception indicator) will be created per user as a combination of all DS over several days to understand the average deceptiveness of the user.


####Evaluating results
Calculate f-scores will determine the effectiveness of the IDI concept introduced.
This mechanism differ from previous research in that previous research focussed on text only and not user attributes as well. The believe is that by adding these identity features to the process, detection can be enhanced.