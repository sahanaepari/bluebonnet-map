library(sf)
library(tmaptools)
library(dplyr)
library(tmap)
library(stringr)
library(leaflet)

#Reading files
VoteBuilderData <- read.csv("SD 46 Registered Voters Export.csv")
shapefile <- st_read("Precincts.shp")


x <- filter(shapefile, COUNTY_NAM == "AVERY") #Avery
y <- filter(shapefile,COUNTY_NAM  =="BURKE") #Burke 
z <- filter(shapefile,COUNTY_NAM  =="CALDWELL") #Caldwell
nc46 <- rbind(x,y,z)

#Cleaning Precinct/Shapefile Data

nc46$ENR_DESC <- gsub("^00.._","",nc46$ENR_DESC)
nc46$ENR_DESC <- gsub("^PR...","",nc46$ENR_DESC)
nc46$ENR_DESC <- gsub("^[0-1]._","",nc46$ENR_DESC)
nc46$ENR_DESC <- gsub("^00..-","",nc46$ENR_DESC)
nc46$ENR_DESC <- tolower(nc46$ENR_DESC)
nc46$ENR_DESC <- str_to_title(nc46$ENR_DESC)
nc46$ENR_DESC <- gsub("A_quaker Meadows #1-A","Quaker Meadows #1-A",nc46$ENR_DESC)
nc46$ENR_DESC <- gsub("B_quaker Meadows #1-B","QUAKER MEADOWS #1-B",nc46$ENR_DESC)
nc46$ENR_DESC <- gsub("B_lower Fork-B","LOWER FORK-B",nc46$ENR_DESC)
nc46$ENR_DESC <- gsub("A_lower Fork-A","Lower Fork-A",nc46$ENR_DESC)
nc46$ENR_DESC <- gsub("Lower Creek #2\r\n", "Lower Creek #2",nc46$ENR_DESC)


#Age Average by Precinct 
ABC <- aggregate(x = VoteBuilderData$Age, by = list(VoteBuilderData$PrecinctName), 
                 FUN = mean, na.rm = TRUE)
colnames(ABC) <- c("PREC_NAME","Average Age")
#Joining Data
finaldata <- as.data.frame(merge(ABC, nc46, by.x = "PREC_NAME", by.y = "ENR_DESC"))

#Convert to sf class + changing column names
st_write(finaldata, "finaldata.shp", append=TRUE)
finaldata <- st_read("finaldata.shp")
colnames(finaldata) <- c("PREC_NAME","Average Age",
                         "PREC_ID","COUNTY_NAME","OF_PREC","COUNTY_ID","geometry")


#Plotting Map
map <- tm_shape(finaldata) + tm_polygons(col = c("Average Age"), palette="Greens", title = "Average Age \n by Precinct") +tm_legend(outside=TRUE)

tmap_leaflet(map, mode = "view", show = FALSE) %>% setView(1249033,2682288,zoom=11)%>% fitBounds(-72, 40, -70, 43)%>% clearBounds() 



