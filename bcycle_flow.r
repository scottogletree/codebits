```r
# Date: 2018-02-18
# S Ogletree
# Description: Flows for Denver BCycle

library(tidyverse)
# read in data, sourced from https://www.denverbcycle.com/company
trips <- readxl::read_excel("2017denverbcycletripdata_public.xlsx", sheet = 1)
# stations <- readxl::read_excel("2018-kiosks-info.xlsx", sheet = 1)

# reduce to origin and destination kiosks
str(trips)
OD <- trips %>% select(orig = `Checkout Kiosk`, dest = `Return Kiosk`)
# reduce to kiosk and lat/lon
# str(stations)
# colnames(stations)[1] <- "kname"
# kiosk <- stations %>% select(kname, lon = Longitude, lat = Latitude)
# the station data was not quite correct, so I had to add a few stations
# write.csv(kiosk, "kiosk_loc.csv", row.names = F, na = "")

kiosk <- read.csv("kiosk_loc.csv", stringsAsFactors = F)


# count up origin and destination combos
OD <- OD %>% group_by(orig, dest) %>% summarise(tcount = n())

# find trips that have orig and dest the same, and NA's
OD$same <- OD$orig == OD$dest
# remove those
OD <- OD %>% filter(!is.na(same) & same == FALSE)

# also remove the 'Dead and Missing Bikes' entries
OD <- OD %>% filter(orig != "Dead and Missing Bikes")
OD <- OD %>% filter(dest != "Dead and Missing Bikes")
# and 
OD <- OD %>% filter(!grepl("DBS LARIMER WAREHOUSE", orig))
OD <- OD %>% filter(!grepl("DBS LARIMER WAREHOUSE", dest))
# and couldn't determine what 'TDNC' kiosk was
OD <- OD %>% filter(orig != "TDNC")
OD <- OD %>% filter(dest != "TDNC")

# Now to join location info to kiosks
ODdata <- OD %>% left_join(kiosk, by = c("orig"="kname")) %>% rename(oX = lon, oY = lat)
ODdata <- ODdata %>% left_join(kiosk, by = c("dest"="kname")) %>% rename(dX = lon, dY = lat)

# all of the kiosk names are not the same, se how many were not matched
sum(is.na(ODdata$oX))
sum(is.na(ODdata$dX))
# there were a number of issues here with stations not in the kiosk list - I had to fix those
# There are some random returns that are NA, just remove those
ODdata <- ODdata %>% filter(!is.na(dX))

# lets try to build the plot, riffing off of http://spatial.ly/2015/03/mapping-flows/

xquiet<- scale_x_continuous("", breaks=NULL)
yquiet<-scale_y_continuous("", breaks=NULL)
quiet<-list(xquiet, yquiet)

ggplot(ODdata[which(ODdata$tcount>10),], aes(oX, oY))+
  #The next line tells ggplot that we wish to plot line segments. The "alpha=" is line transparency and used below 
  geom_segment(aes(x=oX, y=oY,xend=dX, yend=dY, alpha=tcount), col="white")+
  #Here is the magic bit that sets line transparency - essential to make the plot readable
  scale_alpha_continuous(range = c(0.03, 0.3))+
  #Set black background, ditch axes and fix aspect ratio
  theme(panel.background = element_rect(fill='black',colour='black'))+quiet+coord_equal()+ggtitle("Denver BCycle - 2017")
  ```
