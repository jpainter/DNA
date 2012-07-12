# Incoming!

# Read EDN Geocode ----
library(RODBC)
edn.con = odbcConnectAccess2007("c:/Users/bzp3/desktop/EDN geocode.accdb")
# Adresses are the final destination.  Will need to add starting points
qry = "SELECT * FROM Table1 WHERE (((DateDiff('d',[ArrivalDate],Date()))<=14) 
      AND ((Table1.Status)='OK') AND ((Table1.Country) Is Not Null))"
edn = sqlQuery( edn.con, qry)
# all those that are updates after arrrival
scnd.migration = sqlQuery( edn.con, "SELECT * from SecondaryMigration")
scnd = subset(scnd.migration,, c(AddressID, Latitude.1, Longitude.1)) # columns need to assign start point

# Get avg lat-long for source countries ----
library(XML)
url ="http://www.maxmind.com/app/country_latlon"  # has lat-long for each country, by iso code
  iso = read.csv("c:/Users/bzp3/desktop/R/Maps/iso3166countries.txt", as.is=T) # don't convert values to factors
  # convert 1st column to country
  colnames(iso)[1] <- "ISO2"
  colnames(iso)[2] <- "CountryLat"
  colnames(iso)[3] <- "CountryLong"
  # convert <NA> to NA, row 155
  iso[155,1] <- "NA"

# join with list of FIPS codes (used by EDN)  
  url ="http://cloford.com/resources/codes/index.htm" # Copy and paste table into notepad
  fips = read.delim(file="C:/Users/bzp3/Desktop/R/Maps/fips-iso-codes.txt", header=TRUE, sep="\t", fill = TRUE, comment.char="")
  colnames(fips)[6] <- "ISO2"
  fips[fips$FIPS=="CF", "FIPS"] <- "CG"  # EDN for Rep of Congo is CG not CF
# merge
  country.lat.long = merge(iso, fips, by="ISO2")
# save
save(country.lat.long,file="country.lat.long.RData")  

# join with edn data, find unmatched, edit iso values, then remerge
  incoming = merge(edn, country.lat.long, by.x="Country", by.y="FIPS")
  unmatched = merge(edn, country.lat.long, by.x="Country", by.y="FIPS", all=TRUE )
  missing = as.data.frame(table( unmatched[is.na(unmatched$CountryLat), "Country"] ))
  missing
 

#   ----
load("country.lat.long.RData")
# join with edn data, find unmatched, edit iso values, then remerge
incoming = merge(edn, country.lat.long, by.x="Country", by.y="FIPS")

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


#### retreive map data ----
load("worldmap.cp.RData")
load("states.cp.RData")
load("gcircles.rg.RData")

# plot----
require(ggplot2)
g= ggplot() +
  opts(panel.background = theme_rect(fill='grey95',colour='grey95')) + 
  geom_polygon(aes(long.recenter,lat, group=group.regroup), size = 0.2, fill="#f9f9f9", colour = "grey", data=worldmap.cp) +
  geom_polygon(aes(long.recenter,lat, group=group.regroup), size = 0.2, fill="White", colour = "grey35", alpha=.1, data=states.cp) +  
  opts(panel.grid.minor = theme_blank(), panel.grid.major = theme_blank(),  
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

# Save ----
# ggsave("Incoming.pdf", width=8, height=6, dpi=300)
ggsave("images/Incoming.png", width=4, height=3)
# ggsave("Incoming.svg")

# Zoom into US ---
g + routes + ylim(10, 55) + xlim(230, 310)

# Save zoom  
ggsave("images/Incoming-zoom.png", width=4, height=3)

# view secondary migration ----
secondary = geom_line(aes(long.recenter,lat, group=group.regroup, colour=Class, alpha=Class), lineend="round",lwd=0.2, 
                      data= gcircles.rg[!is.na(gcircles.rg$Latitude.1),])
zoom = g + ylim(12, 80) + xlim(175, 310)
zoom + secondary + ylim(12, 80) + xlim(175, 310)

# Save secondary zoom 
ggsave("images/Incoming-secondary.png", width=8, height=6)

#### save images and convert them to a single GIF----
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

#### HTML movie----
saveHTML({
  ani.options(interval = 1 , nmax = 30)
  for (i in 1:nlevels(gcircles.rg$day))  {
    print( plots(i))
    }
    },  outdir = getwd(), img.name = "incoming-movie", 
         title = "EDN: Incoming, June 2012", 
         description = "each line is a new arrival")
dev.off() 

### GIF animations ###----
saveGIF({
  ani.options(nmax = 30)
  for (i in 1:nlevels(gcircles.rg$day))  {
    print( plots(i))
  }
},interval = 0.5, outdir = getwd(), movie.name = "incoming.gif", 
        ani.width = 600, ani.height = 600)

# another way... ----
saveMovie({
    for (i in nrow(nlevels(gcircles.rg$day)))  {
      print( plots(i))
      } 
    }, movie.name = "incoming.gif", clean = T, 
          interval = 0.1 , ani.width = 600, ani.height = 600, ani.options("convert") )
      

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