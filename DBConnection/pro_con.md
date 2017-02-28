#Choosing your environment

I currently pulled my data using Flume in Hadoop. The initial idea was to only use Hadoop due to its scalability. 
There were various problems though to remain in this environment alone:
- I only had one node in my cluster, therefore could not benefit from scalability
- My node only has 12GB of RAM
- My knowledge of MLib was limited and it seems that there are a lot more mature libraries at this moment
- I tried Jupyter notebook as a frontend over the data but found it still very buggy and vizualizations where perceived as not mature yet

The FSOC lab in Potsdam did provide me with a SAP HANA instance with almost 2TB of RAM. This deemed a better solution at the time.
All initial tweets were pushed to SAP HANA for storage. A java proc pulled further tweets of the network (that user's history of tweets and those of their friends).
The proc ran in 10 parallel streams and pulled data at a rate of about 45,000 tweets per hour.
I managed to gather a table of over 1TB of data. I did however start to have major performance issues in SAP HANA.
After investigation it was found that all the data was pushed to a non partitioned row store. Research on similar issues on the internet eluded to the fact that this is a very bad idea.
The next step is to move this data to a column store table structure instead. Problems:
- there is not enough RAM to move the data as is. data need to be moved in chunks
- even then the RAM gets filled up
- alternative ideas is to
  - export the data, and write a java app to push the data back into the column store one by one
  - created multiple column store tables to break up the dataset
Other problems with SAP HANA
  - when it exports data the encoding seems to loose emoticons
In terms of machine learning, I could not play around with the libraries due to the current memory problems. Vizualizations did however seem still in its infancy compared to R or mathlab.

In the meantime a smaller dataset was exported an imported in PostgreSQL. 
This worked well except that all emoticons are lost as mentioned before. In the end Latin1 encoding produced the best result.

For working with the data we are currently using RStudio.
My current VM only has 8GB of RAM and therefor I have also installed RStudio server on the VM in Potsdam with 12GB of RAM. The alternative is to add more RAM to my laptop or request more RAM at Potsdam.
The idea was also to be able to connect and use the data directly from SAP HANA via RStudio. The problem again however is the emoticons. I cannot get the ODBC drivers to read the tweets with these encoded characters.
An alternative would be to first clean the data in SAP HANA itself.
It was also found that RStudio in Windows will not display emoticons (UTF-8) encoded. Linux should but I cannot confirm yet. The problem is that I require an ODBC connection to SAP HANA from the Linux VM.

Current standings
- full set of data in SAP HANA (44GB tweets - after cleaned and reduced from Hadoop)
- only data required for the experiment (50MB) imported into PostgreSQL
- RStudio on Windows
- Rmarkdown published results hosted in github
