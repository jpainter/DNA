# diveristy
# in degree decimal coordinates, 1 degree~70 miles; 0.1~7miles

#digit: significant digit dictates width of square area for clusters
     # 1 (deg dec +-0.1 ~ 7 miles); 0 (deg dec +-0.1 ~ 70 miles)


     directory = "//cdc/project/ccid_ncpdcid_dgmq/IRMH/_Epi Team/PanelDB/"
     load(paste(directory, "edn.geo.rda", sep=""))

#      library(lubridate)
#      date1 = mdy(.date1, quiet=TRUE)  
#      date2 = mdy(.date2, quiet=TRUE)
#      
#      # filter by date 
#      edn.geo = edn.geo[edn.geo$ArrivalDate >= date1 & 
#                             edn.geo$ArrivalDate <= date2, ]
#      
#      # Adresses are the final destination.  Will need to add starting points
#      

#      
#      
#      # filter by country
#      if (is.na(.countries)){edn.geo.country = edn.geo } else {
#           edn.geo.country = edn.geo[edn.geo$Country %in% .countries,]
#      }
#      
     
### map function
     hot.spot.map <- function ( edn.data = edn.geo,
                         digit = 0 , 
                         min=500, 
                         top=20 ,
                         country = NA ,
                         state = NA , 
                         visa = c('Immigrant', 
                                    'Refugee', 
                                    'Asylee', 
                                    'Special Immigrant Visa', 
                                    'Parolee with refugee benefits', 
                                    'Parolee no benefits'), 
                         TbClassID = 0,
                         date1 = "January 1 2012" ,
                         date2 = "December 31 2012",
                         color.var = "country"
                           ) {

# if only no country chosen, then color spots by visa
     if (sum(is.na(country)) > 0){
          color.var = "visa"
     }
# data filters
     
     library(lubridate)
     DATE = edn.data$ArrivalDate>= mdy(date1, quiet=TRUE) & 
          edn.data$ArrivalDate<= mdy(date2, quiet=TRUE)
     
     if(country[1] %in% c("all", "All") | is.na(country[1]))   {
          COUNTRY =  rep(TRUE, nrow(edn.data))
          country = NA
          } else {
          COUNTRY =  edn.data$Country %in% country
          # arrange coutnries alphabetically
          country= country[order(country)]
          }
     
     if(state[1] %in% c("all", "All") | is.na(state[1]))   {
          STATE =  rep(TRUE, nrow(edn.data))
          state = NA
     } else {
          STATE =  edn.data$state %in% tolower(state)
          # arrange states alphabetically
          state= state[order(state)]
     }
     
     VISA = edn.data$VisaType %in% visa
     
     TB = edn.data$TBClassID >= TbClassID 

# data selection
     latlong = with(edn.data[which(COUNTRY & STATE & VISA & TB & DATE),],
                 data.frame(lat=round(Latitude, digit) , 
                            long=round(Longitude, digit) ,
                            visa = VisaType ,
                            country = Country ,
                            tb = TBClassID
                            )
                 )

 # remove missing geocodes.    
     latlong = latlong[which(!is.na(latlong$long) & !is.na(latlong$lat)),]  

  
     library(plyr)  
     hot.spots = ddply(latlong, .var = c("lat", "long", "visa", "country"), .fun=nrow )
     names(hot.spots)[5] = "count"    
     
     
     if (!is.na(top)){
          if (nrow(hot.spots)< top){top = nrow(hot.spots)}
          
          hot.spots = hot.spots[rev(order(hot.spots$count)),]
          row.names(hot.spots) <- seq(length=nrow(hot.spots))
          hot.spots = hot.spots[1:top,]
      } else {
          hot.spots = hot.spots[which(hot.spots$count>min),]
     }
     
#      Add column used to color dots
     hot.spots$color.var = hot.spots[, color.var]
 
## Plot
     require(ggplot2)
     require(grid) # for setting graph margin
     
     require(maps)
     require(mapdata)
     state.map <- map_data("state")
     
     if(state[1] %in% c("all", "All") | is.na(state[1]))   {
          
          STATES =  rep(TRUE, nrow(state.map))
          state = NA
          
     } else {

          STATES =  which(state.map$region %in% tolower(state))
          state= state[order(state)] # arrange states alphabetically
     }
     
     # for Hawaii and Alaska, get maps from mapdata package
     if(state[1] %in% c("hawaii"))  {
          state.map = map("worldHires", "Hawaii")
     } else {
          if (state[1] %in% c("alaska"))  {
          state.map = map("worldHires", "USA:Alaska")
     } else {
          state.map = state.map[STATES,]
     }}
     
     # === title    
     title.line1 =  "EDN "
     
     if (!is.na(top)){
          cluster.type = paste("Top ", top, " notification clusters", sep  = "")
     } else {
          cluster.type = paste("Areas with at least", min, "arrivals")
     }
     title = paste( cluster.type, 
                    "\n",
                   date1 , " - " , date2, ",\n",
                   paste( ifelse(is.na(country), 
                                 "All countries", country), collapse=", ") , 
                   sep = "")
     
     # US map
     base.map = ggplot() +
          
          theme(panel.background = element_rect(fill='grey95', colour='grey95')  , 
                panel.grid.minor = element_blank(), 
                panel.grid.major = element_blank(),  
                axis.ticks = element_blank(), 
                axis.title.x = element_blank(), 
                axis.title.y = element_blank(), 
                axis.text.x = element_blank(), 
                axis.text.y = element_blank(), 
                panel.border = element_blank(),
                plot.background =  element_blank() 
            ) +
           geom_polygon(aes( x = long, y = lat, group = group), 
                         size = 0.2, fill = "White", colour = "grey35", alpha=1, 
                         data = state.map) +
          coord_equal() +
          ggtitle(title) + 
          guides(color=guide_legend(title=color.var)) + # rename legend
          theme(legend.position="bottom")
     
     
     # hot spots
     spots = base.map +
          geom_point( aes(x=long, y=lat, size=count, color = color.var), 
                   alpha = .75,
                   # position =  position_jitter(w = 0.45 , h=0) ,
                   data= hot.spots) +
       
           scale_size_continuous(limits=c(min(hot.spots$count), 
                                          max(hot.spots$count)),
                                 range = c(2, 10)
                             ) +
          scale_colour_brewer(type="seq", palette="Set1") 
 
     # print
     if ( sum(rownames(hot.spots)=="NA")>0){ print(base.map)} else {print(spots)}
     }
