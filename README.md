# TwitterAnalysis
## Analysis of Twitter data as part of PhD research work
Using R-Studio and data scraped from social media (Twitter to be specific). 
For our current purpose we either have the data hosted in [Postgres or SAP HANA](/DBConnection).
There are [pro's and con's](/DBConnection/pro_con.md) to choosing the current environment.

The goal is to extract features from the tweets to ultimately build an identity deception indicator. This deception indicator's purpose will be to highlight potential accounts which does not belong to the current corpus. For our study we will inject dummy harmful accounts.

The current corpus was scraped using specific key words to identify a target group of people. In our case we used the words 'school' and 'homework' as these will hopefully give us a corpus of children or young adults. Note these two words were found to be most common in a research study on internet usage across age bands. We would like to ultimately identify outliers, i.e. people who are not children but talking to them (potentially identifying threats).

Hypothesis to potentially answer during our study:

1. Indentity features can be used to identify deception in social media data.
2. It is possible to identify identity features that can be used towards identity deception

Some [definitions](/Definitions/Definitions.md)

The study consists of the following phases

1. [Understanding the data](/Phase_Data_Mining.md)
2. The experiment - [the features](/Phase_Experiment_Variables.md)
3. [The results](/Phase_Experiment_Results.md)
4. Future Potential work

##The Identity Deception Detection Engine
The identity deception engine that is built has part of this research has 3 main components

1. The factor training engine
2. The deception scoring
3. Network analytics and results

###Inner workings of the engine
####Data cleaning
Here we will start with a trained dataset. 
(Data Cleaning) The following was done to clean data

1. Remove all retweets
2. Remove all accounts with more than 1000 tweets a day
3. Remove all accounts with more than 1000 followers
4. Remove all celebrity accounts
5. Remove all accounts with less than 10 tweets

####Data protection
With regards to privacy protection the following measures have been taken

1. Names are obfuscated

####Data enrichment
A few additional features will be added via various means

1. Distance between geo location and stated location
2. Overall sentiment and emotion of the user
3. The continent of the user
4. Male/Female indicator

####Feature extraction
The next step is to identify those identity features that could indicate deception.
The idea is to take each feature, model it via some clustering algorithm, compare the results and rank than according to entropy.
These results will then be matched with the pshycological literature review that was done as part of the research to determine to best set of features for identity deception.

####Injection of dummy accounts
Some dummy accounts will be generated that are deceptive. These will be injected into the result set

####Scoring (DS)
For each user a deception score (DS) will be calculated per feature.
First the scoring will be done without entropy and then with the entropy results from the feature extraction done previously.
An overall IDI (identity deception indicator) will be created per user.

####Evaluating results
Calculate f-scores will determine the effectiveness of the IDI concept introduced.
This mechanism differ from previous research in that previous research focussed on text only and not user attributes as well. The believe is that by adding these identity features to the process, detection can be enhanced.

