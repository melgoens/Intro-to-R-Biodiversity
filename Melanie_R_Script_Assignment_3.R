#Intro-to-R-Biodiversity

#Questions of interest: What type of habitat do Coccinellidae thrive the most in? Is there a correlation with most inhabited habitat type and most inhabited world location?

#Attaching libraries----
library(mapproj)
library(oce)
library(dplyr)
library(tidyverse)
library(vegan)

#Loading the Data----

#Obtaining data first time around (comment out if it's not the first time running the script)
dfCoccinellidae<-read_tsv("http://www.boldsystems.org/index.php/API_Public/combined?taxon=Coccinellidae&format=tsv")

#Exporting data to file. (For reproducibility and )
write.csv(dfCoccinellidae, "dfCoccinellidae.csv")

#Exploring the Data----

#From what countries do majority of the data come from?
names<-unique(dfCoccinellidae$country)

Country_Data<-c()
for(i in 1:length(names)){
  temporary_variable<-nrow(dfCoccinellidae[dfCoccinellidae$country==names[i], ])
  assign(sprintf("%s_data_points", names[i]), temporary_variable)
  Country_Data<-c(Country_Data, temporary_variable)
}

#checking that it is set to character data before I change it to categorical data in the next step to be then plotted
class(Country_Data)

#It is not (in both the original and edited versions)
Country_Data<-as.character(Country_Data)

#Viewing the countries to create manual input of countries by continents.
print(names)

#Creating a vector for each continent
North_America<-c("Canada", "United States", "Greenland", "Costa Rica", "Mexico", "Guatemala", "Honduras", "Panama", "Puerto Rico")

Europe<-c("Germany", "Spain", "Findland", "Italy", "Norway", "United Kingdom", "Poland", "Portugal", "Moldova", "Greece", "Czech Republic", "Sweden", "Switzerland", "Bulgaria", "France", "Belgium", "Netherlands", "Belarus", "Slovakia", "Ukraine", "Estonia", "Austria", "Slovenia")

Asia<-c("Malaysia", "Russia", "Bangladesh", "Pakistan", "Indonesia", "India", "Japan", "Thailand", "China", "Philipines", "Saudi Arabia", "Vietnam", "Israel", "Oman", "Iran", "Taiwan", "Syria", "Uzbekistan", "Laos", "Turkey", "Sri Lanka", "Armenia", "Myanmar", "South Korea")

South_America<-c("Venezuela", "Argentina", "Ecuador", "Brazil", "Bolivia", "Chile")

Oceania<-c("Australia", "French Polynesia", "Papua New Guinea", "New Zealand", "West Papua", "New Caledonia", "Solomon Islands")

Afirca<-c("South Africa", "Gabon", "Madagascar", "Egypt", "Zambia", "Kenya", "Cameroon", "Ethiopia", "Uganda", "Tanzania", "Rwanda", "Senegal")

#Creating continent vector
continents<-c()
for(i in 1:nrow(dfCoccinellidae)){
  value_check<-0
  if(is.na(dfCoccinellidae$country) == FALSE){
    for(j in 1:len(North_America)){
      if(dfCoccinellidae$country[i] == North_America[j]){
        continents<-c(continents, "North America")
        value_check<-1
      }
    }
    for(j in 1:len(Europe)){
      if(dfCoccinellidae$country[i] == Europe[j]){
        continents<-c(continents, "Europe")
        value_check<-1
      }
    }
    for(j in 1:len(Asia)){
      if(dfCoccinellidae$country[i] == Asia[j]){
        continents<-c(continents, "Asia")
        value_check<-1
      }
    }
    for(j in 1:len(South_America)){
      if(dfCoccinellidae$country[i] == South_America[j]){
        continents<-c(continents, "South America")
        value_check<-1
      }
    }
    for(j in 1:len(Oceania)){
      if(dfCoccinellidae$country[i] == Oceania[j]){
        continents<-c(continents, "Oceania")
        value_check<-1
      }
    }
    for(j in 1:len(Africa)){
      if(dfCoccinellidae$country[i] == Africa[j]){
        continents<-c(continents, "Africa")
        value_check<-1
      }
    }
  }


  if(is.na(dfCoccinellidae$country[i])){
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
#61.5% of the total number of Specimen Records found through Bold Systems is graphed here



###Part 2:Loading the Data----

dfCoccinellidae
table(dfCoccinellidae$institution_storing)

summary(dfCoccinellidae)
dim(dfCoccinellidae)
class(dfCoccinellidae)
is.na(dfCoccinellidae)


###Diversity Question 1:BIN based Question ----
dfCoccinellidae$bin_uri

dfCount.by.BIN<-dfCoccinellidae %>%
  group_by(bin_uri) %>%
  count(bin_uri)
dfCount.by.BIN

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

plot(dfCount.by.BIN2, xlab="BINs", ylab="Frequency")
#or
plot(col1.dfCount.by.BIN2, col2.dfCount.by.BIN2, xlab="BINs", y="Frequency")
#both won't work?

#This works!
BINs.by.Country <- data_frame(dfCoccinellidae$bin_uri, dfCoccinellidae$country)
BINs.by.Country
view(BINs.by.Country)

plot(BINs.by.Country)

#trying to make a data frame of BINs in the different countries

###Diversity Question 2:Location based Question - Latitudinal and Habitat Categorization ----

Countries<-table(dfCoccinellidae$country)

table(dfCoccinellidae$habitat)

table(dfCoccinellidae$lon) #trying to sort data based on longitude
sorted.dfCoccinellidae.lat<-sort(dfCoccinellidae$lat)
ZerotoThirty<-sorted.dfCoccinellidae.lat[0:30]#I'm accidentally pulling out the first 30 elements as opposed to the values that are 0 to 30
ZerotoThirty

group_by()#maybe?
summarize(sorted.dfCoccinellidae.lat) #maybe?


###Maps----

#lat and lon on North America ONLY
map(database= "world", ylim=c(45,90), xlim=c(-160,-50), col="grey80", fill=TRUE, projection="gilbert", orientation= c(90,0,225))
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
