#great circle lines

library(maps)
library(geosphere)
library(RODBC)

# arrivals
     #MUST BE USING 32-BIT R to run odbcConnectAccess2007
     edn.geocode.db<-odbcConnectAccess2007("//cdc/project/ccid_ncpdcid_dgmq/IRMH/_Epi Team/PanelDB/EDN Geocode.accdb")
  arrivals <- sqlQuery(edn.geocode.db, "select Latitude, Longitude, State, NotificationDate, 
                                        Country, Class, Consulate, AlienType, TBClassID from Table1")
#   arrivals.test <- arrivals[1:10,]
  states <- data.frame(table(arrivals$State))
  maxcnt <- max(states$Freq)
     save(arrivals,file="arrivals")

# Color
  pal <- colorRampPalette(c("#191919", "white", "blue"))
  colors <- pal(50)

# Map
  xlim <- c(-180, 180)
  ylim <- c(-60, 90)
  orient <- c(90, 0, -90)
  map<- map("world", proj = "mercator", col="black", bg="grey", 
            lwd=0.05, xlim=xlim, ylim=ylim, orient = orient, wrap=TRUE)

map<- map("world", proj = "mercator", col="white", bg="grey", 
          lwd=0.05, xlim=xlim, ylim=ylim, orient = NULL, wrap=TRUE, 
          parameters= NULL )

# draw each line
    # test line
      air1 <- c(100, 13)
      air2 <- c(arrivals[1,"Longitude"], arrivals[1,"Latitude"]) 
      route <- gcIntermediate( air1 , 
                               air2, 
                               n=100, 
                               addStartEnd=TRUE, sp=FALSE, sepNA=TRUE)


          
# Rhumb line
     b <- bearingRhumb(air2, air1)
     dr <- distRhumb(air1, air2) # distance between points
     pr <- destPointRhumb(air1 , b, d=round(dr/100) * 1:100)

      # adjust route to stay below 70th latitude so that it draws better
      lowerRoute <- route
      i <- 1:(length(route)/2)
      midi <- length(route)/4
      lowerRoute[,2] <- (1- abs(midi - i)/midi)
      map("world", bg="grey", fill=TRUE, col="white")
      lines(lowerRoute, col="black", lwd=2)

     for (j in 1:100) {
        air1 <- c(100, 13)
        air2 <- arrivals[j,]
      
        route <- gcIntermediate( c(air1[1], air1[2]) , c(air2$Longitude, air2$Latitude), n=100, addStartEnd=TRUE)
        
#         colindex <- round( ( subset(states, Var1==arrivals[j,]$State)$Freq  / maxcnt) * length(colors) )
#         lines(route, col=colors[colindex], lwd=0.2)
        
        lines(route, col="black", lwd=0.2)
        }