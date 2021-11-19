#Original file by Melanie Goens
#Edits by Jacqueline Wu
#Edits approved by Melanie Goens, Amy Pitura & Raamkumaar Sivakumar

#Intro-to-R-Biodiversity

#Questions of interest: What type of habitat do Coccinellidae thrive the most in? Is there a correlation with most inhabited habitat type and most inhabited world location?

#Attaching libraries----
library(dplyr)
library(mapproj)
library(oce)
library(tidyverse)
library(vegan)

#Loading the Data----

#Obtaining data
dfCoccinellidae<-read_tsv("http://www.boldsystems.org/index.php/API_Public/combined?taxon=Coccinellidae&format=tsv")

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

#Viewing the countries to create manual input of countries by continents (to a personal database).
print(names)

#Reading-in a personal database of continents and countries
continent_library_file<-file.choose()
continent_library<-read.csv(continent_library_file, sep = ";")

#Creating continent vector
continents<-c()
North_America_count<-c()
South_America_count<-c()
Asia_count<-c()
Oceania_count<-c()
Europe_count<-c()
Africa_count<-c()
for(i in 1:nrow(dfCoccinellidae)){
  value_check<-0
  if(is.na(dfCoccinellidae$country) == FALSE){
    for(j in 1:length(continent_library$North_America)){
      if(identical(dfCoccinellidae$country[i], continent_library$North_America[j])){
        continents<-c(continents, "North_America")
        value_check<-1
        North_America_count<-North_America_count+1
      }
    }
    for(j in 1:length(continent_library$Europe)){
      if(identical(dfCoccinellidae$country[i], continent_library$Europe[j])){
        continents<-c(continents, "Europe")
        value_check<-1
        Europe_count<-Europe_count+1
      }
    }
    for(j in 1:length(continent_library$Asia)){
      if(identical(dfCoccinellidae$country[i], continent_library$Asia[j])){
        continents<-c(continents, "Asia")
        value_check<-1
        Asia_count<-Asia_count+1
      }
    }
    for(j in 1:length(continent_library$South_America)){
      if(identical(dfCoccinellidae$country[i], continent_library$South_America[j])){
        continents<-c(continents, "South_America")
        value_check<-1
        South_America_count<-South_America_count+1
      }
    }
    for(j in 1:length(continent_library$Oceania)){
      if(identical(dfCoccinellidae$country[i], continent_library$Oceania[j])){
        continents<-c(continents, "Oceania")
        value_check<-1
        Oceania_count<-Oceania_count+1
      }
    }
    for(j in 1:length(continent_library$Africa)){
      if(identical(dfCoccinellidae$country[i], continent_library$Africa[j])){
        continents<-c(continents, "Africa")
        value_check<-1
        Africa_count<-Africa_count+1
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
    manual_input<-readline(prompt = "Please enter continent manually: ")
    continents<-c(continents, manual_input)
    
    if(identical(manual_input, "North_America")){
      continent_library_North_America_no_blanks = continent_library$North_America[continent_library$North_America != ""]
      if(length(continent_library$North_America) > length(continent_library_North_America_no_blanks)){
        continent_library$North_America[(length(continent_library_North_America_no_blanks) + 1)] = dfCoccinellidae$country[i]
      }
    }
    if(identical(manual_input, "South_America")){
      continent_library_South_America_no_blanks = continent_library$South_America[continent_library$South_America != ""]
      if(length(continent_library$South_America) > length(continent_library_South_America_no_blanks)){
        continent_library$South_America[(length(continent_library_South_America_no_blanks) + 1)] = dfCoccinellidae$country[i]
      }
    }
    if(identical(manual_input, "Europe")){
      eur_length<-length(continent_library$Europe)
      continent_library_Europe_no_blanks = c(continent_library$Europe[continent_library$Europe != ""], dfCoccinellidae$country[i])
      if(length(continent_library$Europe) > length(continent_library_Europe_no_blanks)){
        continent_library$Europe[(length(continent_library_Europe_no_blanks) + 1)] = dfCoccinellidae$country[i]
      }
    }
    if(identical(manual_input, "Oceania")){
      continent_library_Oceania_no_blanks = c(continent_library$Oceania[continent_library$Oceania != ""], dfCoccinellidae$country[i])
      if(length(continent_library$Oceania) > length(continent_library_Oceania_no_blanks)){
        continent_library$Oceania[(length(continent_library_Oceania_no_blanks) + 1)] = dfCoccinellidae$country[i]
      }
    }
    if(identical(manual_input, "Asia")){
      asi_length<-length(continent_library$Asia)
      continent_library_Asia_no_blanks = c(continent_library$Asia[continent_library$Asia != ""], dfCoccinellidae$country[i])
      if(length(continent_library$Asia) > length(continent_library_Asia_no_blanks)){
        continent_library$Asia[(length(continent_library_Asia_no_blanks) + 1)] = dfCoccinellidae$country[i]
      }
    }
    if(identical(manual_input, "Africa")){
      continent_library_Africa_no_blanks = c(continent_library$Africa[continent_library$Africa != ""], dfCoccinellidae$country[i])
      if(length(continent_library$Africa) > length(continent_library_Africa_no_blanks)){
        continent_library$Africa[(length(continent_library_Africa_no_blanks) + 1)] = dfCoccinellidae$country[i]
      }
    }
            
    #Updating the personal database
    write.csv2(x = continent_library, file = paste("Personal_db_Continents_and_Countries_updated", Sys.Date(), collapse = "", sep = ""), quote = FALSE, row.names = FALSE)
  }
}

continent_library$North_America
continent_library$Europe
continent_library$Africa
continent_library$Asia
continent_library$South_America
continent_library$Oceania

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

col1_dfCount_by_BIN2<-dfCount_by_BIN2[ , 1]
col1_dfCount_by_BIN2
dim(col1_dfCount_by_BIN2)
class(col1_dfCount_by_BIN2)

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
maps::map(database= "world", col="grey80", fill=TRUE, projection="mercator")
coord <- mapproject(dfCoccinellidae$lon, dfCoccinellidae$lat, proj="mercator")  #convert points to projected lat/long
points(coord, pch=20, cex=1.2, col="red")  #plot converted points

#End of Assignment 1
#Melanie Goens October 28 2021
