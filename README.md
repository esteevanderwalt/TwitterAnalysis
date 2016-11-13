# TwitterAnalysis
## Analysis of Twitter data as part of PhD research work
Using R-Studio and data scraped from social media (Twitter to be specific). 
For our current purpose we either have the data hosted in [Postgres or SAP HANA](/DBConnection).
There are [pro's and con's](/DBConnection/pro_con.md) to choosing the current environment.

The goal is to extract features from the tweets to ultimately build an identity deception indicator. This deception indicator's purpose will be to highlight potential accounts which does not belong to the current corpus.
The current corpus was scraped using specific key words to identify a target group of people. In our case we used the words 'school' and 'homework' as these will hopefully give us a corpus of children or young adults. Note these two words were found to be most common in a research study on internet usage across age bands. We would like to ultimately identify outliers, i.e. people who are not children but talking to them (potentially identifying threats).

Hypothesis to potentially answer during our study:

1. The sentiment of people's tweets in the corpus can highlight group of outliers (IDI with 1 variable)&nbsp;
2. The usage of accounts can highlight outliers (IDI with 1 variable)&nbsp;
3. The types of images used can highlight outliers (IDI with 1 variable)&nbsp;
4. Number of devices per person (IDI with 1 variable)&nbsp;
5. The above  variables differ between timezone/location&nbsp;
6. The combination of the multiple variables makes the IDI/outlier detection stronger (IDI with multiple variables)&nbsp;
7. Adding the timezone/locations makes the IDI/outlier stronger&nbsp;
8. Using the above to learn about past outliers, can be applied to detect outliers in real time&nbsp;

Some [definitions](/Definitions/Definitions.md)

The study consists of the following phases
1. [Understanding the data] (/Phase_Data_Mining.md)
2. The experiment - [the variables] (/Phase_Experiment_Variables.md)
3. [The results] (/Phase_Experiment_Results.md)
4. Future Potential work

##The Identity Deception Engine
The identity deception engine that is built has part of this research has 3 main components
1. The factor training engine
2. The deception scoring
3. Network analytics and results

####Inner workings of the engine
Here we will start with a trained dataset. 
(Data Cleaning) All duplicates have been remove from the set.
Enhance the data with more factors

For each factor in the evaluation
  Determine individual score
    Perform clustering (WDD) or hard set score
      Show elbow graph
    Apply chosen Kmeans and show final result
  Save prediction
  Indicate which cluster/score will highlight outliers
  Save k + outlier indicator
  Determine entropy
  
Testing will be done one various training sets. The idea is to see if we can get better trained results with more data. We also want to understand whether this information of the factor will give us more information gain (entropy).
