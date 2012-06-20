# chart annual refugees by country

# Load the XLConnect package
require(XLConnect)

# Load spreadsheet
wb <- loadWorkbook("DNA arrivals.xlsx")
refugees <- readWorksheet(wb, sheet = "refugees", startRow=4, startCol=1, header = TRUE)
  # Limit to countries with at least 1,000
  refugees = refugees[refugees$Grand.Total>1000,]
  # remove column with totals
  refugees = refugees[,!( names(refugees) %in% "Grand.Total")] 
  # remove unknown row
  refugees = refugees[!( refugees$Row.Labels %in% "Unknown"),]
  # remove row with totals
  refugees = refugees[!( refugees$Row.Labels %in% "Grand Total"),]
str(refugees)

#Reshape data
require(reshape)
refugees.melt = melt.data.frame(refugees, variable_name="year", na.rm=TRUE)
# remove years with no movement
refugees.melt = refugees.melt[refugees.melt$value >0,]
#rename years (remove 'calendar')
refugees.melt$year = substr(refugees.melt$year, 10,13)
head(refugees.melt)

#calculate cumulative sums
require(plyr)
refugees.sum = ddply(refugees.melt, .(country=Row.Labels), summarize, year=I(year), total=cumsum(value))                        
head(refugees.sum)

# Chart
require(ggplot2)
require(directlabels)


ggplot( data=refugees.sum, aes(x=year, y=total, group=country)) + 
      theme_bw() + 
      geom_line(size=.75, colour="grey") +
      geom_text(data = refugees.sum[refugees.sum$year == "2012",], aes(label = country), hjust = 1, vjust = 1, size=4)
ggsave("refugee arrival cumsum.pdf", width=8, height=6)
