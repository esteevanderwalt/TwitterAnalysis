# Experiment results







##Results of the experiment to find identity deception

The experiments were broken into several sections. We initially start with a Twitter dataset and injected false accounts. The false accounts are classified as "deceptive" whereas the other accounts are classified as "trustworthy". This leaves us with a classic two class problem although the class is heavily skewed.

We have found from Criminal Deception taxonomy that deception occur on the following attributes:

- Name
- Residency
- ID
- Date of birth

All experiments use this knowledge as a base. Here follows a list of experiments executed:

1. Use the attributes, by default part of the Twitter feed, to try and create a model that correctly classifies the accounts ([..see results](/Results_asis.md))
2. Enrich the data with additional information from other sources to build new models ([..see results](/Results_engineered.md))
3. Score the enriched data as being deceptive or not before building a new model
4. Add social media knowledge to 1,2,3
5. Add weights to 1,2,3
