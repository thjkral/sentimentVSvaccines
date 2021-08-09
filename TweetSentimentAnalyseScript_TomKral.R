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


########################
# 1. Voorbewerken data #
########################

#####
# STAP 1: Laden van alle datasets
#####


allTweets_raw <- read.csv("Covid-19 Vax\\vaccination_all_tweets.csv", header = TRUE, encoding="UTF-8", stringsAsFactors = FALSE)
allVaccinations_raw <- read.csv("Covid-19 Vax\\country_vaccinations.csv", header = TRUE, encoding="UTF-8")


#####
# STAP 2: Voorbewerken tweetdata
#####


# verwijder onnodige kolommen en rijen, verwijder lege waarden en zorg dat er geen getweets zijn
allTweets_subset <- allTweets_raw[, c("id", "user_location", "date", "text", "is_retweet")]
allTweets_subset <- na.omit(allTweets_subset)
allTweets_subset <- subset(allTweets_subset, user_location != "" & is_retweet == "False")

for (row in 1:nrow(allTweets_subset)) { # opschonen van de tekst

  allTweets_subset$text <- as.character(allTweets_subset$text)
  gsub("[^[:alnum:]/w/s]", "", allTweets_subset$text)
  allTweets_subset$text <- str_replace_all(allTweets_subset$text, "[[:punct:]]", "")
  allTweets_subset$text <- trimws(allTweets_subset$text)
  allTweets_subset$text <- tolower(allTweets_subset$text)
  gsub("http\\w+", "", allTweets_subset$text)

}

#####
# STAP 3: Voorbewerken vaccinatiedata
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




########################
# 2. SENTIMENT ANALYSE #
########################


#####
# STAP 1: vergelijk elke tweet met de lexicon om de score te bepalen
#####


map_df(allTweets_subset$text, ~{ # maakt een aparte tabel met scores voor elke text, ook als deze null is
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
#STAP 2: analyseer de scores
#####


sentimentData$wordsScored <- rowSums(sentimentData[,2:3]) # bereken hoeveel woorden per tekst zijn gescoord
summary(sentimentData)

sentimentData_compressed <- subset(sentimentData, select = c("sentiment", "wordsScored")) #condenseer de sentiment analyse resultaten


#####
#STAP 3: verbind de scores aan de tweets
#####


scoredTweets <- cbind(allTweets_subset, sentimentData_compressed) # verbind sentimentscores aan de tweets
scoredTweets <- subset(scoredTweets, wordsScored != 0) #verwijder de tweets die niet gescoord zijn
summary(scoredTweets)


#########################
# 3. LOCALISEREN TWEETS #
#########################


#####
# STAP 1: bepaal voor elke tweet coordinaten op basis van de locatie die door de user is opgegeven
#####

tweetScoredLocated <- scoredTweets # maak een kopie van gescoorde tweets waar de locatie aan verbonden kan worden
counter <- 0

for(i in 1:nrow(tweetScoredLocated)){ # functie om per tweet te bepalen wel lengte- en breedtegraad bij de locatie hoort
  
  coordinates = getbb(tweetScoredLocated$user_location[i],featuretype = "boundary", limit = 1, silent = FALSE)
  tweetScoredLocated$long[i] = (coordinates[1,1] + coordinates[1,2])/2
  tweetScoredLocated$lat[i] = (coordinates[2,1] + coordinates[2,2])/2
  
  counter <- counter + 1
  print(counter)
  
}

# verwijder alle tweets waar geen coordinaten voor gevonden zijn en schrijf tussentijdse resulten naar een file 
# tweetScoredLocated <- na.omit(tweetScoredLocated)
# write.csv(x=tweetScoredLocated, file="Covid-19 Vax\\allTweets_longLat.csv", row.names = F)

tweetScoredLocated <- read.csv("Covid-19 Vax\\allTweets_longLat.csv")


#####
# STAP 2: maak een SpatialPointDataFrame om de coordinaten tot landnamen om te zetten
#####

# maak een SpatialPointDataFrame van de coordinaten
points <- tweetScoredLocated
points$n <-NULL
points$user_location <-NULL
coordinates(points) <- ~long+lat

# bepaal locaties aan de hand van de coordinaten
countriesSP <- getMap(resolution='low')
pointsSP = SpatialPoints(points, proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))

# maak een dataframe met alle informatie die uit de coordinaten te halen is
indices = over(pointsSP, countriesSP)
indices$ADMIN
str(indices)
points$ADMIN  <- indices$ADMIN

# koppel landnamen aan tweets en verwijder missende waarden
usableTweets <- cbind(tweetScoredLocated, country = indices$ADMIN)
usableTweets <- na.omit(usableTweets)




###############################
# 4. ANALYSE BRUIKBARE TWEETS #
###############################


#####
# STAP 1: maak een dataset met alle bruikbare info en schoon deze op
#####


# verwijder kolommen die vanaf deze stap niet meer nodig zijn
usableTweets <- subset(usableTweets, select = c(id, date, sentiment, country))
# verwijder de tijd uit de datums
for (row in 1:nrow(usableTweets)) {
  usableTweets$date <- as.Date(usableTweets$date)
  usableTweets$country <- as.character(usableTweets$country)
}

# verwijderen landen met te weinig tweets voor een goed sentiment
tweetsPerCountry <- usableTweets %>% count(country)
summary(tweetsPerCountry)

tweetsPerCountry <- tweetsPerCountry[order(tweetsPerCountry$n, decreasing = TRUE),]

barplot(tweetsPerCountry$n,
        main = "Aantal tweets per land",
        ylab = "Aantal tweets",
        xlab = "Landen")

plot(tweetsPerCountry$n,
     type = "o",
     main = "Tweets per land",
     ylab = "Aantal tweets",
     xaxt = 'n')
abline(h = mean(tweetsPerCountry$n), col = "red")

countries_100_df <- subset(tweetsPerCountry, n >= 100)
countries_100_v <- as.character(countries_100_df[,1])
remove(countries_100_df)
usableTweets_100 <- usableTweets %>% filter(country %in% countries_100_v)



#####
# STAP 2: Maak een dataframe met het gemiddelde sentiment per dag, per land
#####

sentimentDateCountryMean <- aggregate(. ~ date + country, usableTweets_100[,2:4], mean) #gemiddelde sentiment per dag en land
sentimentDateCountryMean[3] <- scale(sentimentDateCountryMean[3]) # normaliseer het sentiment

sentimentCountryMean <- aggregate(sentiment ~ country, usableTweets_100, mean) # gemiddelde sentiment per land
sentimentCountryMean[2] <- scale(sentimentCountryMean[2])


#####
# STAP 3: Maak een kaart waarop het gemiddelde sentiment per land staat
#####

# maak een kaart waar alle landen in de wereld op staan
world_map <- map_data("world")
colnames(world_map)[colnames(world_map) == 'region'] <- 'country'

for (row in 1:nrow(world_map)) {
  world_map$country <- gsub("USA", "United States of America", world_map$country)
  world_map$country <- gsub("UK", "United Kingdom", world_map$country)
}

sentimentMap <- left_join(sentimentCountryMeans, world_map, by = "country")

worldplot <- ggplot(sentimentMap, aes(long, lat, group = group))+
  geom_polygon(aes(fill = sentiment ), color = "gray47")+
  scale_fill_viridis_c(option = "C")+
  theme_void()
worldplot +labs(title= "Sentiment about COVID-19 vaccinations per country")

# kaart specifiek voor Europa met alle lidstaten van de unie
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

for (row in 1:nrow(eu_map)) {
  eu_map$country <- gsub("UK", "United Kingdom", eu_map$country)
}

sentimentMap_eu <- left_join(sentimentCountryMeans, eu_map, by = "country")

country_label_data <- eu_map %>%
  group_by(country) %>%
  summarise(long = mean(long), lat = mean(lat))

euplot <- ggplot(sentimentMap_eu, aes(long, lat))+
  coord_fixed(1.5)+
  geom_polygon(aes( group = group, fill = sentiment), color = "gray47")+
  geom_text(aes(label = country), data = country_label_data,  size = 3, hjust = 0.5, color = "black")+
  scale_fill_viridis_c(option = "inferno")+
  theme_void()
  
euplot +labs(title = "Sentiment about COVID-19 vaccinations per EU member states")




################################
# 5. LINEAIRE REGRESSIEANALYSE #
################################


#####
#STAP 1: Het samen voegen van alle data
#####

# namen van landen gelijk trekken
for (row in 1:nrow(sentimentDateCountryMean)) {
  sentimentDateCountryMean$country <- gsub("United States of America", "United States", sentimentDateCountryMean$country)
  sentimentDateCountryMean$country <- gsub("Hong Kong S.A.R.", "Hong Kong", sentimentDateCountryMean$country)
}

for (row in 1:nrow(sentimentCountryMean)) {
  sentimentCountryMean$country <- gsub("United States of America", "United States", sentimentCountryMean$country)
  sentimentCountryMean$country <- gsub("Hong Kong S.A.R.", "Hong Kong", sentimentCountryMean$country)
}


# Alle gegevens koppelen aan land en datum
combinedDataSet_date_country <- left_join(allVaccinations_subset, sentimentDateCountryMean, by = c("date", "country"))
combinedDataSet_date_country <- na.omit(combinedDataSet_date_country) # data waarop geen sentiment is vastgelegd verwijderen
combinedDataSet_date_country$daily_vaccinations <- scale(combinedDataSet_date_country$daily_vaccinations) # aantal vaccinaties normaliseren

# Alle gegevens koppelen aan aantallen per land
combinedDataSet_country <- left_join(allVaccinations_total, sentimentCountryMean, by = "country")
combinedDataSet_country <- na.omit(combinedDataSet_country) # landen waarin geen sentiment is vastgelegd verwijderen
combinedDataSet_country$daily_vaccinations <- scale(combinedDataSet_country$daily_vaccinations) # aantal vaccinaties normaliseren



#####
# STAP 2: Bepalen correlatie
#####


correlation_country_date <- round(cor(combinedDataSet_date_country[,3:4]), 3)
correlation_country_date

plot(daily_vaccinations ~ sentiment,
     data = combinedDataSet_date_country,
     main = "Relatie tussen sentiment en totaal aantal vaccinaties",
     ylab = "Totaal aantal vaccinaties",
     xlab = "Gemiddeld sentiment",
     col = "red",
     pch = 20)


correlation_country <- round(cor(combinedDataSet_country[,2:3]), 3)
correlation_country

plot(daily_vaccinations ~ sentiment,
     data = combinedDataSet_country,
     main = "Relatie tussen sentiment en totaal aantal vaccinaties",
     ylab = "Totaal aantal vaccinaties",
     xlab = "Gemiddeld sentiment",
     col = "red",
     pch = 20)


#####
# STAP 3: Trainen model
#####


# per land en per datum
model_date_country <- lm(daily_vaccinations ~ sentiment, data = combinedDataSet_date_country)
model_date_country
summary(model_date_country)

# per land
model_country <- lm(daily_vaccinations ~ sentiment, data = combinedDataSet_country)
model_country
summary(model_country)



##########################
# 6. Optimalisatie Model #
##########################


#####
# STAP 1: Introduceren van hogere orde termen
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


#####
# STAP 2: Gebruik van interactie
#####


model_date_country_int <- lm(daily_vaccinations ~ sentiment + country + sentiment:country, data = combinedDataSet_date_country)
model_date_country_int
summary(model_date_country_int)

model_country_int <- lm(daily_vaccinations ~ sentiment + country + sentiment:country, data = combinedDataSet_country)
model_country_int
summary(model_country_int)


#####
# STAP 3: Nog kleinere subsets
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



