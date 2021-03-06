# Calendar chart
# Refugee arrivals per day'

require(quantmod)
require(ggplot2)
require(reshape2)
require(plyr)
require(scales)

# Load spreadsheet
wb <- loadWorkbook("//cdc/project/ccid_ncpdcid_dgmq/IRMH/DNA/DNAproduction_Notifications.xlsx")
# list of sheets:
# TbByStateYear, TbByPresentCountryYear, 
# RefugeesByStateYear, RefugeesByLocationYear, RefugeesArrivalsByRegionByDay, RefugeesByStateNationality

# Refugee arrivals by day
####
RefugeeArrivalDay <- readWorksheet(wb, sheet = "RefugeesArrivalsByRegionByDay", startRow=4, startCol=1, header = TRUE) # start row is row with cell "Row Labels"
str(RefugeeArrivalDay)

# Make a dataframe
dat<-RefugeeArrivalDay
names(dat)[1] <- "date"
#get date (Wednesday, March 2 2012) into R 
dat$date <- strptime(dat$date, format="%A, %B %d %Y")

# We will facet by year ~ month, and each subgraph will
# show week-of-month versus weekday
# the year is simple
dat$year<-as.numeric(as.POSIXlt(dat$date)$year+1900)
# limit to years >2002
dat <- dat[dat$year>2002 & !is.na(dat$year),]

# the month too 
dat$month<-as.numeric(as.POSIXlt(dat$date)$mon+1)
# but turn months into ordered facors to control the appearance/ordering in the presentation
dat$monthf<-factor(dat$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
# the day of week is again easily found
dat$weekday = as.POSIXlt(dat$date)$wday
# again turn into factors to control appearance/abbreviation and ordering
# I use the reverse function rev here to order the week top down in the graph
# you can cut it out to reverse week order
dat$weekdayf<-factor(dat$weekday,levels=rev(0:6),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE)
# the monthweek part is a bit trickier
# first a factor which cuts the data into month chunks
dat$yearmonth<-as.yearmon(dat$date)
dat$yearmonthf<-factor(dat$yearmonth)
# then find the "week of year" for each day
dat$week <- as.numeric(format(dat$date,"%W"))
# and now for each monthblock we normalize the week to start at 1 
dat<-ddply(dat,.(yearmonthf),transform,monthweek=1+week-min(week))
#convert Grand.Total to total; remove comma from values >1,000
dat$total <- as.numeric(gsub(",","", dat$Grand.Total))

# Now for the plot
P<- ggplot( dat, aes(monthweek, weekdayf, fill=total, alpha=total) ) + 
  geom_tile(colour="white") + facet_grid(year~monthf) +
  scale_fill_continuous(low = "blue",
                        high = "blue",  na.value = "grey50") +
#   scale_fill_gradient( low="white", high="blue" ) +
  opts(title = "Refugee Arrivals") +  xlab("Week of Month") + ylab("")
P

ggsave("Refugee-Arrival-HeatMap.svg")
ggsave("images/Refugee-Arrival-HeatMap.png", width=4, height=3)

# substitute region for day----
#Reshape data
require(reshape)
dat.melt = melt(dat, id=c("year", "monthf", "monthweek"), measure.vars=c(2:7), variable_name="region", na.rm=TRUE)
  # examine results
  library(Hmisc)  # for weighted tables
  x = dat.melt[dat.melt$year==2012 & dat.melt$monthf=="May",]  # look at one month's arrival
  wtd.table(x$region, weights=x$value)  # totals seem beleivable

P<- ggplot( dat.melt, aes(monthweek, region, fill=region , alpha=value ) ) + 
  geom_tile(colour="white") + facet_grid(year~monthf) +
  opts(title = "Refugee Arrivals") +  xlab("Week of Month") + ylab("Region")
P

ggsave("images/Refugee-Arrival-HeatMap-Regions.png", width=4, height=3)

