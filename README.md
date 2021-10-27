# Sentiment about COVID-19 vaccinations versus vaccination rate


These are the accompanivsdv script and datasets for my sentiment and linear regression analysis. 


### The script:
Fully writen in R, this script executes the entire research. It has 6 different steps:
1. Loading and cleaning the data
2. Perform the sentiment analysis
3. Localize the tweets - _Please note that this takes a long time to do. For every tweet the OpenStreetMap API is called._
4. Analyses of usable tweets
5. Perform linear regression
6. Optimization of the model


### The data:
All the datasets are collected and made public by [Gabriel Preda](https://www.kaggle.com/gpreda) on Kaggle. The datasets I used, created at 22-06-2021, are found in this 
repository. If you require newer data, the Twitter data can be found [here](https://www.kaggle.com/gpreda/all-covid19-vaccines-tweets) and the vaccination data is available 
at [this](https://www.kaggle.com/gpreda/covid-world-vaccination-progress) location.
