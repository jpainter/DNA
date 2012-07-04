# chart annual refugees by country

# Load the XLConnect package
require(XLConnect)

# Load spreadsheet
wb <- loadWorkbook("//cdc/project/ccid_ncpdcid_dgmq/IRMH/_Epi Team/DNA/DNA arrivals.xlsx")
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
# Limit to years with at least 1,000
refugees.melt = refugees.melt[refugees.melt$value>500,]
#rename years (remove 'calendar')
refugees.melt$year = substr(refugees.melt$year, 10,13)
head(refugees.melt)

#calculate cumulative sums
require(plyr)
refugees.sum = ddply(refugees.melt, .(country=Row.Labels), summarize, year=I(year), total=cumsum(value))      
head(refugees.sum)

# Chart
require(ggplot2)
require(scales)

# require(directlabels)
#   require(quadprog)
#   require(proto)

# change Dem. Rep Congo to DRC
refugees.sum[refugees.sum$country=="Dem. Rep. Congo", c("country")] = "DRC"

# data set of the last observation (for points and/or labels)
last.point = ddply(refugees.sum, .(country), function(x) x[c(nrow(x)),])

p = ggplot( data=refugees.sum, aes(x=year, y=total, group=country)) + 
      theme_bw()  +
      geom_line(size=.75, colour="grey") +
      geom_point(data = last.point , aes(x=year, y=total), size=4, colour="grey") +
      geom_text(data = last.point, aes(label = country), hjust = .5, vjust = -.5 , size=4) +
      opts(title="Unfolding Refuge Resettlement:\nCumulative Refugee Arrivals Since 2003 by Nationality (>500 arrivals per year)") 
p+ scale_y_continuous( "Refugee Arrivals\n", breaks= seq(0, 100000, 10000), labels=comma(seq(0, 100000, 10000)))
      

ggsave("refugee arrival cumsum.pdf", width=8, height=6)
