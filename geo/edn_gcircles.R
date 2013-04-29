

# ==== EDN data
     
     # load edn.geo from secure directory
     directory = "//cdc/project/ccid_ncpdcid_dgmq/IRMH/_Epi Team/PanelDB/"
     load(paste(directory, "edn.geo.rda", sep=""))

     # Set date range:
     library(lubridate)
     date1 = mdy(.date1, quiet=TRUE)  
     date2 = mdy(.date2, quiet=TRUE)

     # filter by date 
     edn.geo = edn.geo[edn.geo$ArrivalDate >= date1 & 
                            edn.geo$ArrivalDate <= date2, ]


# ==== gcircles
     
     # Incoming aggregated by country, visa and destination state ----
     
     library(plyr)
     countryToState = ddply(.data = edn.vx, .(Country, state, VisaType), 
                            summarize, 
                            count = length(state)
     )
     
     #  Insert lat/long data for place of origin 
     incoming.country = merge(countryToState, country.lat.long, 
                              by.x="Country", by.y="Country", all.x=TRUE)
     
 
     incoming.vx = merge(incoming.country, state.codes, 
                         by.x="state", by.y="state", all.x = TRUE)
     
     # Remove missing records with missing country or state
     incoming.vx = incoming.vx[ !is.na(incoming.vx$CountryLong) & 
                                     !is.na(incoming.vx$stateLong),]
     
     # save
     save(incoming.vx, file="incoming.vx.rda")

     # calculate routes ----
     library(geosphere)
     rts <- gcIntermediate( incoming.vx[ ,c('CountryLong', 'CountryLat')], 
                            incoming.vx[ ,c('stateLong', 'stateLat')], 
                            30, breakAtDateLine=FALSE, addStartEnd=TRUE, sp=TRUE) 
     #- Dateline Break FALSE, 
     # otherwise we get a bump in the shifted ggplots
     
     require(ggplot2)
     source ("fortify-spatial.r")
     rts.ff <- fortify.SpatialLinesDataFrame(rts) # convert into something ggplot can plot
     
     # join attributes, we keep them all, just in case
     incoming.vx$id <-as.character(c(1:nrow(incoming.vx))) # that rts.ff$id is a char
     gcircles.vx <- merge(rts.ff, incoming.vx, all.x=T, by="id") # ggplot
     
     save(gcircles.vx, file="gcircles.vx.RData")

     ### Functions to regroup split lines and polygons when recentering 
     # takes dataframe, column with long and unique group variable, returns df with added column named group.regroup
     RegroupElements <- function(df, longcol, idcol){  
          browser
          g <- rep(1, length(df[,longcol]))
          if (diff(range(df[,longcol])) > 300) {          
               # check if longitude within group differs more than 300 deg, ie if element was split
               # we use the mean to help us separate the extreme values
               d <- df[,longcol] > mean(range(df[,longcol]))
               # some marker for parts that stay in place 
               # (we cheat here a little, as we do not take into account concave polygons)
               g[!d] <- 1     
               g[d] <- 2      # parts that are moved
          }
          # attach to id to create unique group variable for the dataset
          g <-  paste(df[, idcol], g, sep=".") 
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
     
     # Recenter data ----
     load("gcircles.vx.RData")
     center <- 275 # positive values only - US centered view is 260
     
     # shift coordinates to recenter great circles
     gcircles.vx$long.recenter <-  ifelse(gcircles.vx$long  < center - 180 , 
                                          gcircles.vx$long + 360, gcircles.vx$long) 

     # now regroup lines
     library(plyr)
     
     # reallly reallly long function.....(used to be.  Now, that lines are grouped it is fast)
     gcircles.vx.rc <- ddply(gcircles.vx, .(id), RegroupElements, "long.recenter", "id")
     
     save(gcircles.vx.rc, file="gcircles.vx.rc.RData")

