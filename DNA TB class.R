## @knitr UnfoldingTB
# chart annual refugees by country

# Load the XLConnect package
require(XLConnect)

# Load spreadsheet
wb <- loadWorkbook("//cdc/project/ccid_ncpdcid_dgmq/IRMH/DNA/DNAproduction_Notifications.xlsx")
# list of sheets:
# TbByStateYear, TbByPresentCountryYear, 
# RefugeesByStateYear, RefugeesByLocationYear, RefugeesArrivalsByRegionByDay, RefugeesByStateNationality

# TB by State
####
TB <- readWorksheet(wb, sheet = "TbByStateYear", startRow=5, startCol=1, header = TRUE) # start row is row with cell "Row Labels"
str(TB)
# Limit to countries with at least 1,000
TB = TB[TB$Grand.Total>1000,]
# remove column with totals
TB = TB[,!( names(TB) %in% "Grand.Total")] 
# remove unknown row
TB = TB[!( TB$Row.Labels %in% "Unknown"),]
# remove row with totals
TB = TB[!( TB$Row.Labels %in% "Grand Total"),]
str(TB)

#Reshape data
require(reshape)
TB.melt = melt.data.frame(TB, variable_name="year", na.rm=TRUE)
# remove years with no TB
TB.melt = TB.melt[TB.melt$value >0,]
#rename years (remove 'calendar')
TB.melt$year = substr(TB.melt$year, 10,13)
head(TB.melt)

#calculate cumulative sums
require(plyr)
TB.sum = ddply(TB.melt, .(State=Row.Labels), summarize, year=I(year), total=cumsum(value))
# remove countries with less than ...
TB.sum = TB.sum[TB.sum$total >1000,]

#Rename
TBstate = TB.sum

# Unfolding plot function
# data set of the last observation (for points and/or labels)
last.point = ddply(TBstate, .(State), function(x) x[c(nrow(x)),])

p = ggplot( data=TBstate, aes(x=year, y=total, group=State)) + 
  theme_bw() + 
  geom_line(size=.75, colour="grey") +
  geom_point(data = last.point , aes(x=year, y=total), size=4, colour="grey") +
  geom_text(data = last.point, aes(label = State), hjust = 1, vjust = 0 , size=4) +
  scale_y_continuous( "TB Notifications\n", labels=comma_format(digits=6) )

p+ opts(title="Unfolding TB Arrivals by State:\nCumulative Class B Arrivals Since 2003")

ggsave("TB-by-state.svg")
ggsave("images/TB-by-state.png", width=4, height=3)
ggsave("TB-by-state.pdf", width=8, height=6)

####

#TB by PresentCountry
####
TB <- readWorksheet(wb, sheet = "TbByPresentCountryYear", startRow=5, startCol=1, header = TRUE) # start row is row with cell "Row Labels"
str(TB)
# Limit to countries with at least 1,000
TB = TB[TB$Grand.Total>1000,]
# remove column with totals
TB = TB[,!( names(TB) %in% "Grand.Total")] 
# remove unknown row
TB = TB[!( TB$Row.Labels %in% "Unknown"),]
# remove row with totals
TB = TB[!( TB$Row.Labels %in% "Grand Total"),]
str(TB)

#Reshape data
require(reshape)
TB.melt = melt.data.frame(TB, variable_name="year", na.rm=TRUE)
# remove years with no TB
TB.melt = TB.melt[TB.melt$value >0,]
#rename years (remove 'calendar')
TB.melt$year = substr(TB.melt$year, 10,13)
head(TB.melt)

#calculate cumulative sums
require(plyr)
TB.sum = ddply(TB.melt, .(country=Row.Labels), summarize, year=I(year), total=cumsum(value))
# remove countries with less than ...
TB.sum = TB.sum[TB.sum$total >1000,]
# Rename
TBcountry = TB.sum

#Chart
#### 
require(ggplot2)
require(scales)
require(directlabels)


# Unfolding plot function
# data set of the last observation (for points and/or labels)
last.point = ddply(TBcountry, .(country), function(x) x[c(nrow(x)),])

p = ggplot( data=TBcountry, aes(x=year, y=total, group=country)) + 
  theme_bw() + 
  geom_line(size=.75, colour="grey") +
  geom_point(data = last.point , aes(x=year, y=total), size=4, colour="grey") +
  geom_text(data = last.point, aes(label = country), hjust = 1, vjust = 0 , size=4) +
  scale_y_continuous( "TB Notifications\n", labels=comma_format(digits=6) )

p+ opts(title="Unfolding TB Arrivals by Country:\nCumulative Class B Arrivals Since 2003")

# ggsave("TB-by-country.svg")
# ggsave("images/TB-by-country.png", width=4, height=3)
# ggsave("TB-by-country.pdf", width=8, height=6)



