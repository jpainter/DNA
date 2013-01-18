## Intergovernmental Map

#Immigrant and Refugee Arrivals from 'Big 3' countries:  US, Australia, and New Zealand

# Load spreadsheet
wb <- loadWorkbook("c:/users/bzp3/desktop/dna/Final Analysis Joined Spreadsheet with REFUGEES Cumulative Totals.xlsx")
us <- readWorksheet(wb, sheet = "U.S. Arrivals", startRow=1, startCol=1, header = TRUE)
aus <- readWorksheet(wb, sheet = "Australia Arrivals", startRow=1, startCol=1, header = TRUE)
nz <- readWorksheet(wb, sheet = "New Zealand Arrivals", startRow=1, startCol=1, header = TRUE)

us.tb <- readWorksheet(wb, sheet = "U.S. TB", startRow=1, startCol=1, header = TRUE)
aus.tb <- readWorksheet(wb, sheet = "Australia TB", startRow=1, startCol=1, header = TRUE)
nz.tb <- readWorksheet(wb, sheet = "New Zealand TB", startRow=1, startCol=1, header = TRUE)

## cleaning .....
# remove large number of 'arrivals' from NZ to Nz (status adjusters?)
nz[nz[,1] %in% "New Zealand", 2] = 0

# for some reason, us sheed has rows of missing data at end. limit to 201 rows with data
us = us[1:201,]

# Add TB columns to main sheet
us = cbind(us, us.tb)[,c(1,2,4,5)]  # select columns except 3, which is duplicate origin
aus = cbind(aus, aus.tb)[,c(1,2,4,5)]
nz = cbind(nz, nz.tb)[,c(1,2,4,5)]

# rename columns
names(us) = c("origin", "arrivals", "TB", "MDR")
names(aus) = c("origin", "arrivals", "TB", "MDR")
names(nz) = c("origin", "arrivals", "TB", "MDR")

# calculate percent of total 
us$percent = 100* us$arrivals/ sum(us$arrivals, na.rm=TRUE)
aus$percent = 100* aus$arrivals/ sum(aus$arrivals, na.rm=TRUE)
nz$percent = 100* nz$arrivals/ sum(nz$arrivals, na.rm=TRUE)

us$percent.tb = 100* us$TB/ sum(us$TB, na.rm=TRUE)
aus$percent.tb = 100* aus$TB/ sum(aus$TB, na.rm=TRUE)
nz$percent.tb = 100* nz$TB/ sum(nz$TB, na.rm=TRUE)

us$percent.mdr = 100* us$"MDR"/ sum(us$"MDR", na.rm=TRUE)
aus$percent.mdr = 100* aus$"MDR"/ sum(aus$"MDR", na.rm=TRUE)
nz$percent.mdr = 100* nz$"MDR"/ sum(nz$"MDR", na.rm=TRUE)

# JOin into one table (big3)
us$host = "US"
aus$host = "Aus"
nz$host = "NZ"
big3 = rbind(us, aus, nz)

require(maps)
worldmap <- map_data ("world")

require(ggplot2)
g= ggplot() +
  theme(panel.background = element_rect(fill='grey95',colour='grey95')) + 
  geom_polygon(aes(long,lat, group=group), size = 0.2, fill="#f9f9f9", colour = "grey", data=worldmap) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),  
       axis.ticks = element_blank(), 
       axis.title.x = element_blank(), axis.title.y = element_blank(), 
       axis.text.x = element_blank(), axis.text.y = element_blank()
       , legend.position = "none"
  ) +
    ylim(-60, 90) +
    coord_equal()

# to see origin countries shared by host, order by host (aus first), then size .  
big3$origin <- reorder(big3$origin, big3$percent)

a = ggplot(data=big3) +
    geom_bar(aes(x= factor(origin), y=arrivals , fill=factor(host) )) + 
    coord_flip()

b = ggplot(data=big3[big3$percent>=.5,]) +
  geom_bar(aes(x= origin , y=percent , fill=host )) + 
  coord_flip()

# to see origin countries shared by host, order by host (aus first), then size .  
big3$origin <- reorder(big3$origin, big3$percent.tb)

b.tb = ggplot(data=big3[big3$percent.tb>=1,]) +
  geom_bar(aes(x= origin , y=percent.tb , fill=host )) + 
  coord_flip()

# to see origin countries shared by host, order by host (aus first), then size .  
big3$origin <- reorder(big3$origin, big3$percent.mdr)

b.mdr = ggplot(data=big3[big3$percent.mdr>=1,]) +
  geom_bar(aes(x= origin , y=percent.mdr , fill=host )) + 
  coord_flip()

# chart us arrivals
c = ggplot(data=big3[big3$host %in% "US" & big3$percent>=.1,]) +
  geom_bar(aes(x= origin , y=percent , fill=host )) + 
  coord_flip()

require(ggsubplot)
g + geom_subplot
