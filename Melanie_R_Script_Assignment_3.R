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
dfCoccinellidae<-read_tsv("http://www.boldsystems.org/index.php/API_Public/combined?taxon=Coccinellidae&format=tsv")

#Exporting data to file. (For reproducibility and )
write.csv(dfCoccinellidae, "dfCoccinellidae.csv")

#Exploring the Data----

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

Collection_Sites<-as.data.frame(cbind(Country_Data,North_American_Countries))
View(Collection_Sites)
str(Collection_Sites)
summary(Countries)

#Sample size of my data only
Total_Specimen_Records<-29558
my_data<-sum(Country_Data)/Total_Specimen_Records*100
my_data

dfCoccinellidae
summary(dfCoccinellidae)
dim(dfCoccinellidae)
class(dfCoccinellidae)
is.na(dfCoccinellidae)

###Diversity Question 1:BIN based Question ----
dfCoccinellidae$bin_uri

dfCount_by_BIN<-dfCoccinellidae %>%
  group_by(bin_uri) %>%
  count(bin_uri)
dfCount_by_BIN

dfCount.by.Country<-dfCoccinellidae %>%
  group_by(country) %>%
  count(country)
dfCount.by.Country

view(dfCount.by.BIN)

names(dfCount.by.BIN) <- c("BINs", "Frequency")
dfCount.by.BIN

#removing NAs
dfCount.by.BIN2 <-na.omit(dfCount.by.BIN)
dfCount.by.BIN2

#my x axis
col1.dfCount.by.BIN2 <- dfCount.by.BIN2[ , 1]
col1.dfCount.by.BIN2
dim(col1.dfCount.by.BIN2)
class(col1.dfCount.by.BIN2)

#my y axis
dfCoccinellidae$country
unique(dfCoccinellidae$country)

BINs_by_Country <- data_frame(dfCoccinellidae$bin_uri, dfCoccinellidae$country)
view(BINs_by_Country)

###Diversity Question 2:Location based Question - Latitudinal and Habitat Categorization ----

Countries<-table(dfCoccinellidae$country)

table(dfCoccinellidae$habitat)

table(dfCoccinellidae$lon) #trying to sort data based on longitude
sorted.dfCoccinellidae.lat<-sort(dfCoccinellidae$lat)
ZerotoThirty<-sorted.dfCoccinellidae.lat[0:30]#I'm accidentally pulling out the first 30 elements as opposed to the values that are 0 to 30
ZerotoThirty

###Maps----

#lat and lon on North America ONLY
maps::map(database= "world", ylim=c(45,90), xlim=c(-160,-50), col="grey80", fill=TRUE, projection="gilbert", orientation= c(90,0,225))
coord <- mapproject(dfCoccinellidae$lon, dfCoccinellidae$lat, proj="gilbert", orientation=c(90, 0, 225))  #convert points to projected lat/long
points(coord, pch=20, cex=1.2, col="red")  #plot converted points

#whole world no coordinates
data(coastlineWorld)
par(mar=c(1.5, 1, 1.5, 1))
data(topoWorld)
topo <- decimate(topoWorld, 2) # coarsen grid: 4X faster plot
lon <- dfCoccinellidae$lon
lat <- dfCoccinellidae$lat
z <- topo[["z"]]
cm <- colormap(name="gmt_globe")
drawPalette(colormap=cm)
mapPlot(coastlineWorld, projection="+proj=moll", grid=FALSE, col="lightgray")
mapImage(lon, lat, z, colormap=cm)

#End of Assignment 1
#Melanie Goens October 28 2021
