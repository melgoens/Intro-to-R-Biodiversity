#Intro-to-R-Biodiversity

#Questions of interest: What type of habitat do Coccinellidae thrive the most in? Is there a correlation with most inhabited habitat type and most inhabited world location?

#Attaching libraries----
library(mapproj)
library(oce)
library(dplyr)
library(tidyverse)
library(vegan)

#Loading the Data----

#Obtaining data
#dfCoccinellidae<-read_tsv("http://www.boldsystems.org/index.php/API_Public/combined?taxon=Coccinellidae&format=tsv")

#Exporting data to file. (For reproducibility and )
#write.csv(dfCoccinellidae, "dfCoccinellidae.csv")

#Exploring the Data----

dfCoccinellidae
summary(dfCoccinellidae)
dim(dfCoccinellidae)
class(dfCoccinellidae)
is.na(dfCoccinellidae)

#From what countries do majority of the data come from?
names<-unique(dfCoccinellidae$country)

#Viewing the countries to create manual input of countries by continents.
print(names)

#Creating a vector for each continent
North_America<-c("Canada", "United States", "Greenland", "Costa Rica", "Mexico", "Guatemala", "Honduras", "Panama", "Puerto Rico")

Europe<-c("Germany", "Spain", "Finland", "Italy", "Norway", "United Kingdom", "Poland", "Portugal", "Moldova", "Greece", "Czech Republic", "Sweden", "Switzerland", "Bulgaria", "France", "Belgium", "Netherlands", "Belarus", "Slovakia", "Ukraine", "Estonia", "Austria", "Slovenia")

Asia<-c("Malaysia", "Russia", "Bangladesh", "Pakistan", "Indonesia", "India", "Japan", "Thailand", "China", "Philippines", "Saudi Arabia", "Vietnam", "Israel", "Oman", "Iran", "Taiwan", "Syria", "Uzbekistan", "Laos", "Turkey", "Sri Lanka", "Armenia", "Myanmar", "South Korea")

South_America<-c("Venezuela", "Argentina", "Ecuador", "Brazil", "Bolivia", "Chile")

Oceania<-c("Australia", "French Polynesia", "Papua New Guinea", "New Zealand", "West Papua", "New Caledonia", "Solomon Islands")

Africa<-c("South Africa", "Gabon", "Madagascar", "Egypt", "Zambia", "Kenya", "Cameroon", "Ethiopia", "Uganda", "Tanzania", "Rwanda", "Senegal")

#Creating continent vector
continents<-c()
for(i in 1:nrow(dfCoccinellidae)){
  value_check<-0
  if(is.na(dfCoccinellidae$country) == FALSE){
    for(j in 1:length(North_America)){
      if(identical(dfCoccinellidae$country[i], North_America[j])){
        continents<-c(continents, "North_America")
        value_check<-1
      }
    }
    for(j in 1:length(Europe)){
      if(identical(dfCoccinellidae$country[i], Europe[j])){
        continents<-c(continents, "Europe")
        value_check<-1
      }
    }
    for(j in 1:length(Asia)){
      if(identical(dfCoccinellidae$country[i], Asia[j])){
        continents<-c(continents, "Asia")
        value_check<-1
      }
    }
    for(j in 1:length(South_America)){
      if(identical(dfCoccinellidae$country[i], South_America[j])){
        continents<-c(continents, "South_America")
        value_check<-1
      }
    }
    for(j in 1:length(Oceania)){
      if(identical(dfCoccinellidae$country[i], Oceania[j])){
        continents<-c(continents, "Oceania")
        value_check<-1
      }
    }
    for(j in 1:length(Africa)){
      if(identical(dfCoccinellidae$country[i], Africa[j])){
        continents<-c(continents, "Africa")
        value_check<-1
      }
    }
  }


  if((is.na(dfCoccinellidae$country[i])) == TRUE){
    continents<-c(continents, NA)
    value_check<-1
  }
  
  #Provides leeway for changes in the data
  if(value_check == 0){
    print(sprintf("Continent of %s not found", dfCoccinellidae$country[i]))
    manual_input<-readline(prompt = "Please enter continent manually:")
    continents<-c(continents, manual_input)
  }
}

View(continents)
View(dfCoccinellidae$country)

###Diversity Question 1:BIN based Question ----
dfCoccinellidae$bin_uri

dfCount_by_BIN<-dfCoccinellidae %>%
  group_by(bin_uri) %>%
  count(bin_uri)
dfCount_by_BIN

dfCount_by_Country<-dfCoccinellidae %>%
  group_by(country) %>%
  count(country)
dfCount_by_Country

view(dfCount_by_BIN)

names(dfCount_by_BIN)<-c("BINs", "Frequency")
dfCount_by_BIN

#removing NAs
dfCount_by_BIN2<-na.omit(dfCount_by_BIN)
dfCount_by_BIN2

#my x axis
col1_dfCount_by_BIN2<-dfCount_by_BIN2[ , 1]
col1_dfCount_by_BIN2
dim(col1_dfCount_by_BIN2)
class(col1_dfCount_by_BIN2)

#my y axis
dfCoccinellidae$country
unique(dfCoccinellidae$country)

BINs_by_Country <- data_frame(dfCoccinellidae$bin_uri, dfCoccinellidae$country)
view(BINs_by_Country)

###Diversity Question 2:Location based Question - Latitudinal and Habitat Categorization ----

Countries<-table(dfCoccinellidae$country)

table(dfCoccinellidae$habitat)

table(dfCoccinellidae$lon)
sorted_dfCoccinellidae_lat<-sort(dfCoccinellidae$lat)
ZerotoThirty<-sorted_dfCoccinellidae_lat[sorted_dfCoccinellidae_lat > 0 & sorted_dfCoccinellidae_lat < 30]
ZerotoThirty

###Maps----

#lat and lon on North America ONLY
maps::map(database= "world", ylim=c(45,90), xlim=c(-160,-50), col="grey80", fill=TRUE, projection="gilbert", orientation= c(90,0,225))
coord <- mapproject(dfCoccinellidae$lon, dfCoccinellidae$lat, proj="gilbert", orientation=c(90, 0, 225))  #convert points to projected lat/long
points(coord, pch=20, cex=1.2, col="red")  #plot converted points

#world map
maps::map(database= "world", col="grey80", fill=TRUE, projection="gilbert")
coord <- mapproject(dfCoccinellidae$lon, dfCoccinellidae$lat, proj="gilbert")  #convert points to projected lat/long
points(coord, pch=20, cex=1.2, col="red")  #plot converted points

#End of Assignment 1
#Melanie Goens October 28 2021
