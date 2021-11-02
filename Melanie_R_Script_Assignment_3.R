#Intro-to-R-Biodiversity

#Questions of interest: What type of habitat do Coccinellidae thrive the most in? Is there a correlation with most inhabited habitat type and most inhabited world location?

#Attaching libraries----
library(mapproj)
library(oce)
library(dplyr)
library(tidyverse)
library(vegan)

#Loading the Data----
dfCoccinellidae<-read_tsv("http://www.boldsystems.org/index.php/API_Public/combined?taxon=Coccinellidae&format=tsv")

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

North_American_Countries_Template<-read_table("https://www.newworldencyclopedia.org/entry/list_of_countries_by_continent#NorthAmerica")

North_American_Countries<-c(FALSE,TRUE,FALSE,FALSE)
sum(North_American_Countries) #True=1, False=0 - So the sum of 1 would make sense here
unique(North_American_Countries) #Making sure I didn't spell anything wrong - we can also see this by taking the sum

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

dfCount.by.BIN <- dfCoccinellidae %>%
  group_by(bin_uri) %>%
  count(bin_uri)
dfCount.by.BIN

dfCount.by.Country <- dfCoccinellidae %>%
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
