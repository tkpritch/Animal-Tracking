#######################################################
#
# Animal Movement R Workshop
#
#######################################################
### LOAD AND INSTALL PACKAGES
library(dplyr)
library(tidyr)
library(lubridate)
library(dplyr)
library(ggmap)
#library(VTrack) no package called VTrack
library(ggplot2)
library(basemaps)
library(ggOceanMaps)
library(sp)
library(leaflet)
library(mapview)
library(sf)
library(tidyverse)
library(ggspatial)
library(sp)
library(adehabitatHR)
library(devtools)


### Check your working directory. This is where all your files will be stored and can be accessed from
getwd()

### This data set downloaded from IMOS contains four sharks from our projects tagged internally with acoustic tags.
### (one scalloped hammerhead, one great hammerhead, one tiger shark and one bull shark). All but the great hammerhead
### were tagged in the Hinchinbrook Channel off Lucinda (we will make a map later). The great hammerhead was tagged at Orpheus Island
### https://animaltracking.aodn.org.au/

### Load in the data set and the metadata

MB5371_Meta <- read.csv("C:/Users/Admin/OneDrive/Documents/MB5371_Tech2/Animal_Tracking/Animal-Tracking/data/MB5371_Metadata.csv",header=T)
MB5371_Detections <- read.csv("C:/Users/Admin/OneDrive/Documents/MB5371_Tech2/Animal_Tracking/Animal-Tracking/data/MB5371_Detections.csv", header = TRUE)
MB5371_Receivers <- read.csv("C:/Users/Admin/OneDrive/Documents/MB5371_Tech2/Animal_Tracking/Animal-Tracking/data/MB5371_Receivers.csv", header = TRUE)

### Let's adjust the date time column format to a better one to work with and change time zone to Australia as IMOS outputs in UTC. 
### Also, make a new column that only has the date. IMOS output only gives a combined Date - Time column but for many analyses
### we are not interested in the time

MB5371_Detections$detection_datetime <- as.POSIXct(MB5371_Detections$detection_datetime,format="%d/%m/%Y %H:%M",tz="UTC")
MB5371_Detections$detection_datetime <- as.POSIXct(MB5371_Detections$detection_datetime, format = "%Y-%m-%d %H:%M",tz="UTC")

MB5371_Detections$detection_datetime <-
  MB5371_Detections$detection_datetime %>% ymd_hms(tz="UTC") %>% with_tz(tzone="Australia/Brisbane")

MB5371_Detections$Date<- as.Date(MB5371_Detections$detection_datetime, "Australia/Brisbane")

### Now arrange the dataframe from newest detection to oldest

MB5371_Detections <- MB5371_Detections %>% dplyr::arrange(desc(detection_datetime))

### Inspect the data, it is always a good idea to understand how your data is structured and what you are working with

head(MB5371_Detections)
head(MB5371_Receivers)

### First, we should plot our study site and the receiver locations to see where tagged sharks 
### could potentially be detected

### Transform into spatial object

MB5371_Receivers_SPATIAL <- MB5371_Receivers
coordinates(MB5371_Receivers_SPATIAL) <- c("receiver_deployment_longitude", "receiver_deployment_latitude")

### Simple Plot

data.frame(MB5371_Receivers_SPATIAL) %>% ggplot(mapping = aes(x = receiver_deployment_longitude, y = receiver_deployment_latitude)) +  
  xlab("longitude") + ylab("latitude") + geom_point()

### Let's get a real map

### Extract our receiver locations and plot them in an interactive map

Receiver_Locations <- MB5371_Receivers %>% 
  dplyr::select(receiver_deployment_latitude, receiver_deployment_longitude, station_name)

mapview(Receiver_Locations, xcol = "receiver_deployment_longitude", ycol = "receiver_deployment_latitude", crs = 4269, grid = FALSE)  

### Let's see how many different sharks and different species are in the data set (Should be 4 !!), we can save those ID codes as an object for later

Shark_IDs <- MB5371_Detections %>% distinct(transmitter_id)
Species <- MB5371_Detections %>% distinct(species_common_name)

### How many detections does each shark have?

MB5371_Detections %>% dplyr::group_by(transmitter_id) %>% summarise(NumDetections = n())

### We can extend this by looking at detections at each receiver too and store it in another object for easier viewing

Detections_Per_Rec <- MB5371_Detections %>%
  group_by(transmitter_id, station_name) %>%
  summarise(NumDetections = n())

### Now extract where sharks were detected
### First, extract the receiver lat and lons for the receivers in the detection data frame by filtering by species
### Then, transform into a spatial object

Scalloped_Locations <- MB5371_Detections %>% dplyr::filter(species_common_name == "Scalloped Hammerhead") %>% 
  dplyr::select(receiver_deployment_latitude, receiver_deployment_longitude, station_name)
Bull_Locations <- MB5371_Detections %>% dplyr::filter(species_common_name == "Bull Shark") %>% 
  dplyr::select(receiver_deployment_latitude, receiver_deployment_longitude, station_name)
Tiger_Locations <- MB5371_Detections %>% dplyr::filter(species_common_name == "Tiger Shark") %>% 
  dplyr::select(receiver_deployment_latitude, receiver_deployment_longitude, station_name)
Great_Locations <- MB5371_Detections %>% dplyr::filter(species_common_name == "Great Hammerhead") %>% 
  dplyr::select(receiver_deployment_latitude, receiver_deployment_longitude, station_name)

coordinates(Scalloped_Locations) <- c("receiver_deployment_longitude", "receiver_deployment_latitude")
coordinates(Bull_Locations) <- c("receiver_deployment_longitude", "receiver_deployment_latitude")
coordinates(Tiger_Locations) <- c("receiver_deployment_longitude", "receiver_deployment_latitude")
coordinates(Great_Locations) <- c("receiver_deployment_longitude", "receiver_deployment_latitude")

### Now simply plot with the mapview package

# Scalloped Hammerhead
mapview(Receiver_Locations, xcol = "receiver_deployment_longitude", ycol = "receiver_deployment_latitude", crs = 4269, grid = FALSE) + Scalloped_Locations 

# Bull
mapview(Receiver_Locations, xcol = "receiver_deployment_longitude", ycol = "receiver_deployment_latitude", crs = 4269, grid = FALSE) + Bull_Locations 

# Tiger
mapview(Receiver_Locations, xcol = "receiver_deployment_longitude", ycol = "receiver_deployment_latitude", crs = 4269, grid = FALSE) + Tiger_Locations 

#Great Hammerhead
mapview(Receiver_Locations, xcol = "receiver_deployment_longitude", ycol = "receiver_deployment_latitude", crs = 4269, grid = FALSE) + Great_Locations 

### Another great visulation tool for acoustic data is a timeline to see potential 
### seasonal patterns and presence/absence at a location

MB5371_Detections %>% 
  ggplot(mapping = aes(x = detection_datetime, y = as.factor(transmitter_id))) + xlab("Date") + ylab("Tag") + geom_point()

### We can also look at how many detections per day each shark had. Remember the Date column we made earlier?
### That is coming in handy now

MB5371_Detections %>%
  group_by(transmitter_id, Date) %>%  summarise(DailyDetections= n()) %>% 
  ggplot(mapping = aes(x = transmitter_id, y = DailyDetections)) +
  xlab("Tag") + ylab("Number of detections per day") +
  geom_boxplot() 


### We can also make a time line by receivers to get a general overview of time and space

MB5371_Detections %>%
  ggplot(mapping = aes(x = detection_datetime, y = as.factor(station_name))) + 
  xlab("Date") + ylab("Receiver station") +
  geom_point() +
  facet_wrap(~transmitter_id, nrow=1)

### Home range analysis

### Let's first compare the scalloped hammerhead to the great hammerhead
### Two species so closely related, yet so far
### To do this we extract them from the main data set

Hammer_Detections <- MB5371_Detections %>% dplyr::filter(species_common_name == 
                                                           "Great Hammerhead" | species_common_name == "Scalloped Hammerhead")

### Now we need to extract the data we want for home range analysis: ID, Time, Lat, Lon

Hammer_Detections <- Hammer_Detections %>% dplyr::select(transmitter_id, detection_datetime, 
                                                         receiver_deployment_longitude, receiver_deployment_latitude)

### We need to transform our detection dataframe into a spatial object again

coordinates(Hammer_Detections)=c("receiver_deployment_longitude","receiver_deployment_latitude")
head(Hammer_Detections)

### Run a very simple version of the model (Note, the units are not correct)

cp <- mcp(Hammer_Detections[,1], percent=95)
plot(cp)
plot(Hammer_Detections, add=TRUE)
as.data.frame(cp)

kud <- kernelUD(Hammer_Detections[,1], h="href")
image(kud)

### The great hammerhead has a much larger home range than the scalloped hammerhead
### Lets have a look at the powerpoint to see a much more complicated version of these models and what they can look like
### When you plot them properly

### Lastly for this data set, lets have a bit more of a detailed look at the scalloped hammerhead,
### for which we identified a change in space use just by visualising its detections.
### To do this, we are going to trick the R packag by splitting its detections when the change likely occured
### First, isolate the scalloped hammerhead and select only the columns we want.

Scalloped_Detections <- MB5371_Detections %>% dplyr::filter(species_common_name == "Scalloped Hammerhead")

Scalloped_Detections <- Scalloped_Detections %>% dplyr::select(transmitter_id, detection_datetime, 
                                                               receiver_deployment_longitude, receiver_deployment_latitude, station_name)

### Lets look at the time line for the scalloped hammerhead for detections per receiver

Scalloped_Detections %>%
  ggplot(mapping = aes(x = detection_datetime, y = as.factor(station_name))) + 
  xlab("Date") + ylab("Receiver station") +
  geom_point() 

### Inspect the data set for the first detections at the Jewfish Site
### They first appeared on: 2022-09-16 20:18:00
### Now we will treat this as two different animal IDs so that Adehabitat calculates two different home ranges for the same animal

Scalloped_Detections$transmitter_id <- ifelse(Scalloped_Detections$detection_datetime < "2022-09-16 20:18:00", "Earlier", "Later")

head(Scalloped_Detections)

### We need to transform our detection dataframe into a spatial object again

coordinates(Scalloped_Detections)=c("receiver_deployment_longitude","receiver_deployment_latitude")

### Run a very simple version of the model (Note, the units are not correct)

cp <- mcp(Scalloped_Detections[,1], percent=95)
plot(cp)
plot(Hammer_Detections, add=TRUE)
as.data.frame(cp)

kud <- kernelUD(Scalloped_Detections[,1], h="href")
image(kud)

### What could cause such a change?



#################################### Lets switch to a data set with lots of tiger sharks

MB5371_Tiger_Sharks <- read.csv("MB5371_Tiger_Sharks.csv", header = TRUE)
MB5371_Tiger_Sharks_Meta <- read.csv("MB5371_Metadata_Tigers.csv", header = TRUE)


MB5371_Tiger_Sharks$detection_datetime <- as.POSIXct(MB5371_Tiger_Sharks$detection_datetime,format="%d/%m/%Y %H:%M",tz="UTC")

MB5371_Tiger_Sharks$detection_datetime <- as.POSIXct(MB5371_Tiger_Sharks$detection_datetime, format = "%Y-%m-%d %H:%M",tz="UTC")

MB5371_Tiger_Sharks$detection_datetime <-
  MB5371_Tiger_Sharks$detection_datetime %>% ymd_hms(tz="UTC") %>% with_tz(tzone="Australia/Brisbane")

MB5371_Tiger_Sharks$Date<- as.Date(MB5371_Tiger_Sharks$detection_datetime, "Australia/Brisbane")

### Now arrange the dataframe from newest detection to oldest

MB5371_Tiger_Sharks <- MB5371_Tiger_Sharks %>% dplyr::arrange(desc(detection_datetime))

### Cut out detections from before May 2021 as this is the time at which we had most tigers tagged (Think about why we delete before)

MB5371_Tiger_Sharks <- MB5371_Tiger_Sharks %>% dplyr::filter(detection_datetime > "2021-05-01 00:00:00")

### Filter all detections to one detection per day per shark

MB5371_Tiger_Sharks_Day <- MB5371_Tiger_Sharks %>% dplyr::group_by(transmitter_id) %>% dplyr::distinct(Date, .keep_all = TRUE)

### Make each tiger shark ID a factor for easier distinction

MB5371_Tiger_Sharks_Day$transmitter_id <- as.factor(MB5371_Tiger_Sharks_Day$transmitter_id)

### Now count how many tiger sharks were detected in all of the Whitsundays on each day

MB5371_Tiger_Sharks_Day <- MB5371_Tiger_Sharks_Day %>% dplyr::group_by(Date) %>% dplyr::summarise(n_distinct(transmitter_id))

### Rename column

MB5371_Tiger_Sharks_Day <- dplyr::rename(MB5371_Tiger_Sharks_Day, Count = `n_distinct(transmitter_id)`)

### So now we have a data frame with daily tiger shark counts in the Whitsundays.
### To look at potential drivers of numbers we can look at temperature. Lets combine a temperature dataframe for daily values
### in °C with our tigersharks counts and plot it. Sadly, the tiger shark count dataframe may not have all days in it as sometimes,
### There were 0 tiger sharks, those days will be missing. However, the temperature dataframe is continous. With some clever data wrangling,
### We can combine the two and fill the gaps of tiger shark counts with "0",

MB5371_Whitsundays_Temp <- read.csv("MB5371_Whitsundays_Temp.csv", header = TRUE)
MB5371_Whitsundays_Temp$Date <- as.POSIXct(MB5371_Whitsundays_Temp$Date,format="%d/%m/%Y",tz="UTC")

### Combine
Combined <- dplyr::left_join(MB5371_Whitsundays_Temp, MB5371_Tiger_Sharks_Day, by="Date")

### Now you see the days of 0 tiger sharks have "NA" in it, we can easily fill this with 0

Combined[is.na(Combined)] <- 0

### Lets plot to see if there is a pattern

ggplot(data=Combined, mapping = aes(x=Date, y=Count)) + geom_line()
ggplot(data=Combined, mapping = aes(x=Date, y=degree_C)) + geom_line()

### But is temperature really a driver? What else could factor into it?

### Maybe size and sex? Back to our detection date frame
### Lets c

MB5371_Tiger_Sharks <- dplyr::left_join(MB5371_Tiger_Sharks, MB5371_Tiger_Sharks_Meta)

### Ignore the warning, check the data frame

head(MB5371_Tiger_Sharks)

### We could count the total days each shark was detected in the Whitsundays and see how that relates to size and sex. Basically,
### We are using presence in Whitsundays as a proxy for movement. If they are not detected in the Whitsundays they may have moved somewhere else
### For this we need to exclude data from before July 2022 because we were still tagging tigers in the Whitsundays then
### Think about it: A tiger shark tagged in 2020 has more time to add up days than one tagged in 2022.

MB5371_Tiger_Sharks <- MB5371_Tiger_Sharks %>% dplyr::filter(Date > "2022-08-01")
MB5371_Tiger_Sharks_Presence <- MB5371_Tiger_Sharks %>% dplyr::group_by(transmitter_id) %>% dplyr::summarise(n_distinct(Date))
MB5371_Tiger_Sharks_Presence <- MB5371_Tiger_Sharks_Presence %>% dplyr::distinct(transmitter_id, .keep_all = TRUE)
MB5371_Tiger_Sharks_Presence <- dplyr::left_join(MB5371_Tiger_Sharks_Presence, MB5371_Tiger_Sharks_Meta)

### Plot

MB5371_Tiger_Sharks_Presence <- dplyr::rename(MB5371_Tiger_Sharks_Presence, Count = `n_distinct(Date)`)

ggplot(data=MB5371_Tiger_Sharks_Presence, mapping = aes(x=Count, y=measurement_value)) + geom_point()

### Anything?

### What about sex? We have to rejoin the data sets...more wrangling

MB5371_Tiger_Sharks_Sex <- MB5371_Tiger_Sharks %>% dplyr::select(transmitter_id, animal_sex)
MB5371_Tiger_Sharks_Sex <- MB5371_Tiger_Sharks_Sex %>% dplyr::distinct(transmitter_id, .keep_all = TRUE)
MB5371_Tiger_Sharks_Sex  <- dplyr::left_join(MB5371_Tiger_Sharks_Presence, MB5371_Tiger_Sharks_Sex)

MB5371_Tiger_Sharks_Sex <- MB5371_Tiger_Sharks_Sex  %>% group_by(animal_sex) %>% summarise(n = n(), mean = mean(Count, na.rm=T))

### Now that we have seen some patterns, what do you think could drive this, given what you know about shark behaviour/ecology/reproduction etc?

### Include some of the results from this workshop and write a little discussion of what this might mean and why it could be important
