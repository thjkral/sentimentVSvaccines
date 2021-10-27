setwd("E://Files//Big Data")

# install.packages("osmdata")
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("rworldmap")
library(dplyr)
library(tidyverse)
library(osmdata)
library(sp)
library(rworldmap)
library(tidytext)
library(glue)
library(stringr)
library(ggplot2)
require(maps)
require(viridis)
library(lubridate)


###################
# 1. Prepare data #
###################

#####
# STEP 1: Load all datasets
#####


allTweets_raw <- read.csv("Covid-19 Vax\\vaccination_all_tweets.csv", header = TRUE, encoding="UTF-8", stringsAsFactors = FALSE)
allVaccinations_raw <- read.csv("Covid-19 Vax\\country_vaccinations.csv", header = TRUE, encoding="UTF-8")


#####
# STEP 2: preprocess the Twitter data
#####


# remove unnecessary columns and rows
# remove missing values
# make sure no retweets are present
allTweets_subset <- allTweets_raw[, c("id", "user_location", "date", "text", "is_retweet")]
allTweets_subset <- na.omit(allTweets_subset)
allTweets_subset <- subset(allTweets_subset, user_location != "" & is_retweet == "False")

for (row in 1:nrow(allTweets_subset)) { # clean the texts

  allTweets_subset$text <- as.character(allTweets_subset$text)
  gsub("[^[:alnum:]/w/s]", "", allTweets_subset$text)
  allTweets_subset$text <- str_replace_all(allTweets_subset$text, "[[:punct:]]", "")
  allTweets_subset$text <- trimws(allTweets_subset$text)
  allTweets_subset$text <- tolower(allTweets_subset$text)
  gsub("http\\w+", "", allTweets_subset$text)

}

#####
# STEP 3: preprocess the vaccination data
#####


allVaccinations_subset <- allVaccinations_raw[, c("country", "date", "daily_vaccinations")]
allVaccinations_subset <- na.omit(allVaccinations_subset)

for (row in 1:nrow(allVaccinations_subset)) {
  allVaccinations_subset$date <- as.Date(allVaccinations_subset$date)
}

allVaccinations_total <- aggregate(daily_vaccinations ~ country, allVaccinations_subset, sum)

summary(allTweets_subset)
str(allTweets_subset)
summary(allVaccinations_subset)




#########################
# 2. SENTIMENT ANALYSIS #
#########################


#####
# STEP 1: Calculate scores by comparing every tweet to the lexicon
#####


map_df(allTweets_subset$text, ~{ # make a seperate table with scores for every text. Even if all values are zero
  tibble(text = .x) %>% 
    unnest_tokens(word, text, drop = FALSE) %>%
    inner_join(get_sentiments("bing")) -> tmp
  if(nrow(tmp) == 0) tibble(sentiment = 0, positive = 0, negative = 0)
  else {
    tmp %>%
      count(sentiment) %>% 
      complete(sentiment = c('positive', 'negative'), fill = list(n = 0)) %>%
      pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
      mutate(sentiment = positive - negative)
  }
}) -> sentimentData


#####
#STEP 2: analyze the scores
#####


#calculate the amount of words used in scoring a tweet
sentimentData$wordsScored <- rowSums(sentimentData[,2:3])
summary(sentimentData)

# remove rows that are unnecessary from this point forward
sentimentData_compressed <- subset(sentimentData, select = c("sentiment", "wordsScored")) #condenseer de sentiment analyse resultaten


#####
#STEP 3: combine tweets with their scores
#####


scoredTweets <- cbind(allTweets_subset, sentimentData_compressed)
scoredTweets <- subset(scoredTweets, wordsScored != 0) # remove unscores tweets
summary(scoredTweets)


######################
# 3. LOCALIZE TWEETS #
######################


#####
# STEP 1: determine coordinates for every location the user has provided
#####

tweetScoredLocated <- scoredTweets
counter <- 0

for(i in 1:nrow(tweetScoredLocated)){ # function to determine which latitude and longitude belong to the location
  
  coordinates = getbb(tweetScoredLocated$user_location[i],featuretype = "boundary", limit = 1, silent = FALSE)
  tweetScoredLocated$long[i] = (coordinates[1,1] + coordinates[1,2])/2
  tweetScoredLocated$lat[i] = (coordinates[2,1] + coordinates[2,2])/2
  
  counter <- counter + 1
  print(counter)
  
}

# remove all tweets with an untracable location 
tweetScoredLocated <- na.omit(tweetScoredLocated)

# uncomment the code below to save intermediate results to a file
#write.csv(x=tweetScoredLocated, file="Covid-19 Vax\\allTweets_longLat.csv", row.names = F)

# uncomment the code below to read intermediate results
#tweetScoredLocated <- read.csv("Covid-19 Vax\\allTweets_longLat.csv")


#####
# STEP 2: create a SpatialPointDataFrame to convert coordinates to names of countries
#####

# create a SpatialPointDataFrame with the coordinates
points <- tweetScoredLocated
points$n <-NULL
points$user_location <-NULL
coordinates(points) <- ~long+lat

# deterine locations with the coordinates
countriesSP <- getMap(resolution='low')
pointsSP = SpatialPoints(points, proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))

# create a dataframe with all the information that can be derived from the coordinates
indices = over(pointsSP, countriesSP)
indices$ADMIN
str(indices)
points$ADMIN  <- indices$ADMIN

# match country names to tweets and remove missing values
usableTweets <- cbind(tweetScoredLocated, country = indices$ADMIN)
usableTweets <- na.omit(usableTweets)




#############################
# 4. ANALYSIS USABLE TWEETS #
#############################


#####
# STEP 1: create and clean a dataset with all the usable information
#####


# remove rows that are unnecessary from this point forward
usableTweets <- subset(usableTweets, select = c(id, date, sentiment, country))
# remove the time from all dates
for (row in 1:nrow(usableTweets)) {
  usableTweets$date <- as.Date(usableTweets$date)
  usableTweets$country <- as.character(usableTweets$country)
}

# remove countries with to few tweets
tweetsPerCountry <- usableTweets %>% count(country)
summary(tweetsPerCountry)

tweetsPerCountry <- tweetsPerCountry[order(tweetsPerCountry$n, decreasing = TRUE),]

barplot(tweetsPerCountry$n,
        main = "Amount of tweets per country",
        ylab = "tweets",
        xlab = "countries")

plot(tweetsPerCountry$n,
     type = "o",
     main = "Tweets per country",
     ylab = "tweets",
     xaxt = 'n')
abline(h = mean(tweetsPerCountry$n), col = "red")

countries_100_df <- subset(tweetsPerCountry, n >= 100)
countries_100_v <- as.character(countries_100_df[,1])
remove(countries_100_df)
usableTweets_100 <- usableTweets %>% filter(country %in% countries_100_v)



#####
# STAP 2: create a dataframe with the mean sentiment per day, per country
#####

sentimentDateCountryMean <- aggregate(. ~ date + country, usableTweets_100[,2:4], mean) # mean sentiment per day and country
sentimentDateCountryMean[3] <- scale(sentimentDateCountryMean[3]) # normalize the sentiment

sentimentCountryMean <- aggregate(sentiment ~ country, usableTweets_100, mean) # mean sentiment per country
sentimentCountryMean[2] <- scale(sentimentCountryMean[2]) # normalize the sentiment


#####
# STAP 3: Create maps with an average sentiment per country
#####

# create a map for the entire world
world_map <- map_data("world")
colnames(world_map)[colnames(world_map) == 'region'] <- 'country'

for (row in 1:nrow(world_map)) { # change some country names
  world_map$country <- gsub("USA", "United States of America", world_map$country)
  world_map$country <- gsub("UK", "United Kingdom", world_map$country)
}

sentimentMap <- left_join(sentimentCountryMean, world_map, by = "country")

worldplot <- ggplot(sentimentMap, aes(long, lat, group = group))+
  geom_polygon(aes(fill = sentiment ), color = "gray47")+
  scale_fill_viridis_c(option = "C")+
  theme_void()+
  borders("world")


worldplot +labs(title= "Sentiment about COVID-19 vaccinations per country")

# create a map with all memberstates of the European Union
eu_member_states <- c(
  "Portugal", "Spain", "France", "Switzerland", "Germany",
  "Austria", "Belgium", "UK", "Netherlands",
  "Denmark", "Poland", "Italy", 
  "Croatia", "Slovenia", "Hungary", "Slovakia",
  "Czech republic", "Sweden", "Norway", "Finland", "Iceland",
  "Latvia", "Bulgaria", "Lithuania", "Luxembourg", "Cyprus",
  "Malta", "Estonia", "Romania", "Greece", "Ireland"
)

eu_map <- map_data("world", region = some.eu.countries)
colnames(eu_map)[colnames(eu_map) == 'region'] <- 'country'

for (row in 1:nrow(eu_map)) { # change the name for the United Kingdom
  eu_map$country <- gsub("UK", "United Kingdom", eu_map$country)
}

sentimentMap_eu <- left_join(sentimentCountryMean, eu_map, by = "country")

country_label_data <- eu_map %>%
  group_by(country) %>%
  summarise(long = mean(long), lat = mean(lat))

euplot <- ggplot(sentimentMap_eu, aes(long, lat))+
  coord_fixed(1.5)+
  geom_polygon(aes( group = group, fill = sentiment), color = "gray47")+
  geom_text(aes(label = country), data = country_label_data,  size = 3, hjust = 0.5, color = "black")+
  scale_fill_viridis_c(option = "inferno")+
  theme_void()+
  borders(regions = eu_member_states)
  
euplot +labs(title = "Sentiment about COVID-19 vaccination per EU member state")




#################################
# 5. LINEAR REGRESSION ANALYSIS #
#################################


#####
#STEP 1: combine all data
#####

# align names of countries
for (row in 1:nrow(sentimentDateCountryMean)) {
  sentimentDateCountryMean$country <- gsub("United States of America", "United States", sentimentDateCountryMean$country)
  sentimentDateCountryMean$country <- gsub("Hong Kong S.A.R.", "Hong Kong", sentimentDateCountryMean$country)
}

for (row in 1:nrow(sentimentCountryMean)) {
  sentimentCountryMean$country <- gsub("United States of America", "United States", sentimentCountryMean$country)
  sentimentCountryMean$country <- gsub("Hong Kong S.A.R.", "Hong Kong", sentimentCountryMean$country)
}


# Link all data by country and date
combinedDataSet_date_country <- left_join(allVaccinations_subset, sentimentDateCountryMean, by = c("date", "country"))
combinedDataSet_date_country <- na.omit(combinedDataSet_date_country) # data waarop geen sentiment is vastgelegd verwijderen
combinedDataSet_date_country$daily_vaccinations <- scale(combinedDataSet_date_country$daily_vaccinations) # aantal vaccinaties normaliseren

# Link all data by country only
combinedDataSet_country <- left_join(allVaccinations_total, sentimentCountryMean, by = "country")
combinedDataSet_country <- na.omit(combinedDataSet_country) # landen waarin geen sentiment is vastgelegd verwijderen
combinedDataSet_country$daily_vaccinations <- scale(combinedDataSet_country$daily_vaccinations) # aantal vaccinaties normaliseren



#####
# STEP 2: Determine correlation
#####


correlation_country_date <- round(cor(combinedDataSet_date_country[,3:4]), 3)
correlation_country_date

plot(daily_vaccinations ~ sentiment,
     data = combinedDataSet_date_country,
     main = "Relationship between sentiment and the amount of vaccines",
     ylab = "Total amount of vaccines",
     xlab = "Average sentiment",
     col = "red",
     pch = 20)


correlation_country <- round(cor(combinedDataSet_country[,2:3]), 3)
correlation_country

plot(daily_vaccinations ~ sentiment,
     data = combinedDataSet_country,
     main = "Relationship between sentiment and the amount of vaccines",
     ylab = "Total amount of vaccines",
     xlab = "Average sentiment",
     col = "red",
     pch = 20)


#####
# STEP 3: Training the model
#####


# per country, per date
model_date_country <- lm(daily_vaccinations ~ sentiment, data = combinedDataSet_date_country)
model_date_country
summary(model_date_country)

# per country
model_country <- lm(daily_vaccinations ~ sentiment, data = combinedDataSet_country)
model_country
summary(model_country)



##################################
# 6. Optimalization of the Model #
##################################


#####
# STAP 1: Introduce higher order terms
#####


combinedDataSet_date_country$x2 <- combinedDataSet_date_country$sentiment * combinedDataSet_date_country$sentiment
model_date_country_x2 <- lm(formula = daily_vaccinations ~ sentiment + x2, data = combinedDataSet_date_country)
summary(model_date_country_x2)

correlation_country_date_x2 <- round(cor(combinedDataSet_date_country[,3:5]), 3)
correlation_country_date_x2

combinedDataSet_country$x2 <- combinedDataSet_country$sentiment * combinedDataSet_country$sentiment
model_country_x2 <- lm(formula = daily_vaccinations ~ sentiment + x2, data = combinedDataSet_country)
summary(model_country_x2)

correlation_country_x2 <- round(cor(combinedDataSet_country[,2:4]), 3)
correlation_country_x2


# This improved the model. The techniques below didn't



#####
# STAP 2: Introduce interaction
#####


model_date_country_int <- lm(daily_vaccinations ~ sentiment + country + sentiment:country, data = combinedDataSet_date_country)
model_date_country_int
summary(model_date_country_int)

model_country_int <- lm(daily_vaccinations ~ sentiment + country + sentiment:country, data = combinedDataSet_country)
model_country_int
summary(model_country_int)


#####
# STAP 3: Use smaller subsets
#####


top4 <- c("India", "United States", "Canada", "United Kingdom")
combinedDataSet_date_country_top4 <- combinedDataSet_date_country[combinedDataSet_date_country$country %in% top4, ]

model_date_country_top4 <- lm(daily_vaccinations ~ sentiment, data = combinedDataSet_date_country_top4)
model_date_country_top4
summary(model_date_country_top4)

combinedDataSet_date_country_top4 <- combinedDataSet_date_country[combinedDataSet_date_country$country %in% top4, ]
remove(combinedDataSet_date_country_top4)

combinedDataSet_country_top4 <- combinedDataSet_country[combinedDataSet_country$country %in% top4, ]

model_country_top4 <- lm(daily_vaccinations ~ sentiment, data = combinedDataSet_country_top4)
model_country_top4
summary(model_country_top4)



