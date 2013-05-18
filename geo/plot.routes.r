
plot.routes = function( data = gcircles.rc,
                        country = c("Nepal","Thailand", "Malaysia", "Ethiopia", "Kenya"), 
                        date1 = "January 1 2012", 
                        date2 = "December 31 2012",
                        visa = c('Immigrant', 
                                 'Refugee', 
                                 'Asylee', 
                                 'Special Immigrant Visa', 
                                 'Parolee with refugee benefits', 
                                 'Parolee no benefits'), 
                        size = "count",
                        alpha = "count",
                        ylim = c(-10, 90),                     
                        min = 200,
                         color.var = "Country",
                        colors = c("red", "blue")
){
     require(ggplot2)
     require(grid)

     load("worldmap.cp.RData")
     load("states.cp.RData")
     
     # Country
     if(country[1] %in% "all")   {COUNTRY =  rep(TRUE, nrow(data))} else 
                              {COUNTRY =  data$Country %in% country}
     
     # arrange coutnries alphabetically
     country= country[order(country)]
     
     # Dates
     library(lubridate)
 
     
     title = paste("EDN notification of arrivals from ", paste(country, collapse=", ") , ",\n",  
                     date1 , " - " , date2, 
                     sep = "")
     
     # Minimum number of routes to display
     MIN = data$count > min 
     
     # visa type
     VISA = data$VisaType %in% visa
     
     # data selection     
     route.select=which( COUNTRY & MIN & VISA )  
     route.data = data[route.select,]


     
     ## plot -----
     countries = which( worldmap.cp$region %in% country)
     
     states = which( states.cp$region %in% unique(route.data$state))
     
     g= ggplot() +
 
          # world map
          geom_polygon(aes(x = long.recenter, y = lat, group = group.regroup), 
                       size = 0.2, fill = "grey99", colour = "grey55", alpha=.1,
                       data=worldmap.cp) +
          
          # highlight country of origin
          geom_polygon(aes(long.recenter, lat, group=group.regroup),  
                       size = 0.2, fill="grey35", colour = "white", 
                       data=worldmap.cp[countries,]) +
          
          # US map
          geom_polygon(aes( x = long.recenter, y = lat, group = group.regroup), 
                       size = 0.2, fill = "White", colour = "grey35", alpha=.1, 
                       data = states.cp) +
          
          # highlight states
          geom_polygon(aes(long.recenter, lat, group=group.regroup),  
                       size = 0.2, fill="grey35", colour = "white", 
                       data = states.cp[states,]) +
          
          theme(panel.background = element_blank() , 
                panel.grid.minor = element_blank(), 
                panel.grid.major = element_blank(),  
                axis.ticks = element_blank(), 
                axis.title.x = element_blank(), 
                axis.title.y = element_blank(), 
                axis.text.x = element_blank(), 
                axis.text.y = element_blank(), 
                panel.border = element_blank(),
                plot.background =  element_blank(),
#                 plot.title =  element_blank(),
#                 legend.position = "none" ,
                legend.position="bottom", 
                plot.margin = unit(c(0,0,0,0), "cm")
          ) +
          ylim(-60, 90) +
          coord_equal() +
          ggtitle(title)
 
     route.data$color.var = route.data[, color.var]
     routes = geom_line(aes(long.recenter, lat, group=group.regroup  
#                             colour = color.var, #AlienType,
#                             size = count,
#                             alpha = count
                            ), 
                        linetype='solid',
                        lineend ="round" ,
                        alpha = 1,
                        data = route.data  #data = gcircles.rc #route.data 
     )
     g  + routes
  
# rec-sclae line size
     gg = g + routes + 
     #           scale_color_manual(values = colors)   + 
               scale_colour_brewer(type="qual", palette="Set1") +
               scale_size_continuous(limits=c(min(route.data$count), 
                                              max(route.data$count))
                                     ,
                                              range = c(.1, 1.5)
                                     ) +
               guides(color=guide_legend(title=color.var))  # rename legend
#                + guides(size=guide_legend(title=NULL))

               

     
print(gg)
      
}
