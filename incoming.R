# Incoming!

# Read EDN Geocode----
library(RODBC)
edn.con = odbcConnectAccess2007("c:/Users/bzp3/desktop/EDN geocode.accdb")
# all geocoded address.  Adresses are the final destination.  Will need to add starting points
# select most recent 1,000 records
edn = sqlQuery( edn.con, "Select * From Table1 where Status = 'OK' and now() - ArrivalDate <30 and Country is not null")
# all those that are updates after arrrival
scnd.migration = sqlQuery( edn.con, "SELECT * from SecondaryMigration")
scnd = subset(scnd.migration,, c(AddressID, Latitude.1, Longitude.1)) # columns need to assign start point

# Get avg lat-long for countries
# library(XML)
# url ="http://www.maxmind.com/app/country_latlon"
#   iso3166countries = read.csv("c:/Users/bzp3/desktop/R/Maps/iso3166countries.txt", as.is=T) # don't convert values to factors
#   iso3166countries[155,]
#   # convert 1st column to country
#   colnames(iso3166countries)[1] <- "Country"
#   colnames(iso3166countries)[2] <- "CountryLat"
#   colnames(iso3166countries)[3] <- "CountryLong"
#   # convert <NA> to NA, row 155
#   iso3166countries[155,1] <- "NA"
#   # convert to data.table
#   # iso3166countries = data.table(iso3166countries)
#   # save
#   save(iso3166countries,file="iso3166countries.RData")
load("iso3166countries.RData")

# join with edn data
incoming = merge(edn, iso3166countries, by="Country" )
# List of incoming countries
table(incoming$Country)
#rename country lat long to StartLat, StartLong
names(incoming)[names(incoming)=="CountryLat"] = "StartLat"
names(incoming)[names(incoming)=="CountryLong"] = "StartLong"
              
# When notification is secondary migration, replace StartLat StartLong with values from scnd.migration table
incoming = merge(incoming, scnd, by.x="AddressID", by.y="AddressID", all.x=TRUE )
# Assign stating points
incoming$StartLat = ifelse(!is.na(incoming$Latitude.1), incoming$Latitude.1, incoming$StartLat)          
incoming$StartLong = ifelse(!is.na(incoming$Longitude.1), incoming$Longitude.1, incoming$StartLong)          

# calculate routes -- Dateline Break FALSE, otherwise we get a bump in the shifted ggplots----
library(geosphere)
rts <- gcIntermediate( incoming[,c('StartLong', 'StartLat')], incoming[,c('Longitude', 'Latitude')], 
                       100, breakAtDateLine=FALSE, addStartEnd=TRUE, sp=TRUE) 
require(ggplot2)
source ("fortify-spatial.r")
rts.ff <- fortify.SpatialLinesDataFrame(rts) # convert into something ggplot can plot

incoming$id <-as.character(c(1:nrow(incoming))) # that rts.ff$id is a char
gcircles <- merge(rts.ff, incoming, all.x=T, by="id") # join attributes, we keep them all, just in case
save(gcircles, file="gcircles.RData")

# Recenter data and maps----
require(maps)
worldmap <- map_data ("world")
states <- map_data("state")
load("gcircles.RData")

### Recenter

center <- 275 # positive values only - US centered view is 260

# shift coordinates to recenter great circles
gcircles$long.recenter <-  ifelse(gcircles$long  < center - 180 , gcircles$long + 360, gcircles$long) 

# shift coordinates to recenter worldmap
worldmap$long.recenter <-  ifelse(worldmap$long  < center - 180 , worldmap$long + 360, worldmap$long)
states$long.recenter <- ifelse(states$long  < center - 180 , states$long + 360, states$long)

### Function to regroup split lines and polygons
# takes dataframe, column with long and unique group variable, returns df with added column named group.regroup
RegroupElements <- function(df, longcol, idcol){  
  g <- rep(1, length(df[,longcol]))
  if (diff(range(df[,longcol])) > 300) {          # check if longitude within group differs more than 300 deg, ie if element was split
    d <- df[,longcol] > mean(range(df[,longcol])) # we use the mean to help us separate the extreme values
    g[!d] <- 1     # some marker for parts that stay in place (we cheat here a little, as we do not take into account concave polygons)
    g[d] <- 2      # parts that are moved
  }
  g <-  paste(df[, idcol], g, sep=".") # attach to id to create unique group variable for the dataset
  df$group.regroup <- g
  df
}

### Function to close regrouped polygons
# takes dataframe, checks if 1st and last longitude value are the same, if not, inserts first as last and reassigns order variable
ClosePolygons <- function(df, longcol, ordercol){
  if (df[1,longcol] != df[nrow(df),longcol]) {
    tmp <- df[1,]
    df <- rbind(df,tmp)
  }
  o <- c(1: nrow(df))  # rassign the order variable
  df[,ordercol] <- o
  df
}

# now regroup
library(plyr)
gcircles.rg <- ddply(gcircles, .(id), RegroupElements, "long.recenter", "id")
save(gcircles.rg, file="gcircles.rg.RData")
worldmap.rg <- ddply(worldmap, .(group), RegroupElements, "long.recenter", "group")
states.rg <- ddply(states, .(group), RegroupElements, "long.recenter", "group")

# close polys
worldmap.cp <- ddply(worldmap.rg, .(group.regroup), ClosePolygons, "long.recenter", "order")  # use the new grouping var
save(worldmap.cp, file="worldmap.cp.RData")
states.cp <- ddply(states.rg, .(group.regroup), ClosePolygons, "long.recenter", "order")  # use the new grouping var
save(states.cp, file="states.cp.RData")

#### retreive map data
load("worldmap.cp.RData")
load("states.cp.RData")
load("gcircles.rg.RData")

# plot----
require(ggplot2)
g= ggplot() +
  geom_polygon(aes(long.recenter,lat, group=group.regroup), size = 0.2, fill="#f9f9f9", colour = "grey5", data=worldmap.cp) +
  geom_polygon(aes(long.recenter,lat, group=group.regroup), size = 0.2, fill="White", colour = "black", alpha=.1, data=states.cp) +  
  opts(panel.background = theme_blank(), 
       panel.grid.minor = theme_blank(), panel.grid.major = theme_blank(),  
       axis.ticks = theme_blank(), 
       axis.title.x = theme_blank(), axis.title.y = theme_blank(), 
       axis.text.x = theme_blank(), axis.text.y = theme_blank()
       , legend.position = "none"
       ) +
  ylim(-60, 90) +
  coord_equal()

routes = geom_line(aes(long.recenter,lat, group=group.regroup, colour=Country, type=Class), alpha=0.1, lineend="round",lwd=0.1, 
                   data= gcircles.rg)
g + routes
#   scale_colour_gradient(low="#fafafa", high="#EE0000") +  
ggsave("Incoming.pdf", width=8, height=6, dpi=300)
ggsave("Incoming.svg")

# Zoom into US
g + routes + ylim(10, 55) + xlim(230, 310)

# view secondary migration
secondary = geom_line(aes(long.recenter,lat, group=group.regroup, colour=Class, alpha=Class), lineend="round",lwd=0.2, 
                      data= gcircles.rg[!is.na(gcircles.rg$Latitude.1),])
zoom = g + ylim(12, 80) + xlim(175, 310)
zoom + secondary + ylim(12, 80) + xlim(175, 310)

#### save images and convert them to a single GIF
library(animation)
library(caTools)
library(lubridate)

# create factor for dates
gcircles.rg$day = factor(day(gcircles.rg$ArrivalDate))

# Function to plot arrivals on same day (need to add month...)
plots = function(.id){
    g + geom_line( aes(long.recenter, lat, group=group.regroup, colour=Country, alpha=Country), 
                          lineend="round",lwd=0.3, 
                          data= subset( gcircles.rg, as.numeric(day)==.id ) )
}

#### HTML movie
oopt = ani.options(interval = 0.2, nmax = 50, ani.dev = png, ani.type = "png",
                   ani.height = 350, ani.width = 500,
                   title = "Demonstration of Polygons with Gradient Colors",
                   description = "The graph actually consists of a series of polygons, 
    each with a redder color starting from yellow.")
ani.start()
opar = par(mar = c(3, 3, 1, 0.5), mgp = c(2, .5, 0), tcl = -0.3,
           cex.axis = 0.8, cex.lab = 0.8, cex.main = 1)


saveMovie({
    for (i in nrow(nlevels(gcircles.rg$day)))  {
      print( plots(i))
      } 
    }, movie.name = "incoming.gif", clean = T, 
          interval = 0.1, ani.width = 600, ani.height = 600, ani.options("convert") )
      

##  map code from from spatial analysis----
# This step removes the axes labels etc when called in the plot.
xquiet<- scale_x_continuous("", breaks=NULL)
yquiet<-scale_y_continuous("", breaks=NULL, limits=c(-60, 90))
opts <- opts(panel.background = theme_rect(fill='grey',colour='grey'))
quiet<-list(xquiet, yquiet, opts)

# Create a base plot
base<- ggplot(worldmap, aes(x = long, y = lat))

# Then build up the layers
wrld<-c(geom_polygon(aes(long,lat,group=group), size = 0.1, colour= "grey", fill="white", alpha=1, data=worldmap))
# urb<- c(geom_polygon(aes(long,lat,group=group), size = 0.1, colour= "#FCFFF1", fill="#FCFFF1", alpha=1, data=urbanareas))

# Bring it all together with the great circles
base + WorldMap + coord_equal()+ quiet +
geom_path(data=gcircles, aes(long,lat, group=group, colour=Status),alpha=0.1, lineend="round",lwd=0.1)