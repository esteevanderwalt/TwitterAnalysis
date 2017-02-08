# TwitterAnalysis
## The experiment variables

It is important to understand the difference between an attributes and a feature.
An attribute is used to describe any field in the dataset, whether it be categorical or numerical.
Features on the other hand are those attributes that can be used to model and solve a problem.

First the existing attributes must be analysed to understand what is available.

During the first round of the experiment the following attributes were identified/engineered to contribute to deception. The below is the analysis of each individual attributes to understand how they could be scored and contribute towards the overall IDI

1. [No of devices](/Engine/FactorTraining/FactorAnalysis-NoOfDevices.md)
2. [No of friends vs followers](/Engine/FactorTraining/FactorAnalysis-FriendsFollowers.md)
3. [Type of image](/Engine/FactorTraining/FactorAnalysis-ImgType.md)
4. [Timezone vs AVG tweet time](/Engine/FactorTraining/FactorAnalysis-AvgTweetTime.md)
5. [Distance between TZ and geo location](/Engine/FactorTraining/FactorAnalysis-Distances.md)
6. [Sentiment per user](/Engine/FactorTraining/FactorAnalysis-Sentiment.md)

This was however not sufficient and no scientific reason exist for the above features except maybe that they could be found in some pshycology research. The decision was then made to investigate all attributes available and understand any potential correlation between these attributes first before extracting or engineering any new features.
The results can be found here: [Feature selection/filtering](/Engine/FeatureIdentification/FeatureIdentification.md).

In a follow up we have found that criminal deception can be found in the following sets of attributes:

- Name
- Residency
- ID
- Birth date

It was then decided to redo the analysis but grouping the attributes in the above sets which will make more sense [Feature selection/filtering V2](/Engine/FeatureIdentification/FeatureIdentification2.md). 



  
