# chart annual refugees by country

# Load the XLConnect package
require(XLConnect)

# Load spreadsheet
wb <- loadWorkbook("//cdc/project/ccid_ncpdcid_dgmq/IRMH/_Epi Team/DNA/DNA-TBclass.xlsx")
TB <- readWorksheet(wb, sheet = "Sheet1", startRow=5, startCol=1, header = TRUE)
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
# TB.sum

# Chart
require(ggplot2)
require(directlabels)

# data set of the last observation (for points and/or labels)
last.point = ddply(TB.sum, .(country), function(x) x[c(nrow(x)),])

p = ggplot( data=TB.sum, aes(x=year, y=total, group=country)) + 
  theme_bw() + 
  geom_line(size=.75, colour="grey") +
  geom_point(data = last.point , aes(x=year, y=total), size=4, colour="grey") +
  geom_text(data = last.point, aes(label = country), hjust = 1, vjust = 0 , size=4)
  opts(title="Unfolding TB Arrivals:\nCumulative Class B Arrivals Since 2003") 

p+ scale_y_continuous( "Class B TB\n", labels=comma_format() , breaks=( seq(0, 100000, 10000) ) )

ggsave("TB class cumsum.pdf", width=8, height=6)

# Zoom
p+ scale_y_continuous( "Class B TB\n", labels=comma_format() , breaks=(seq(0, 10000, 1000) ), limits=c(0,10000) )


