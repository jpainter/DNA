# Disease Notification Analysis 
## DNA
## John Painter
## July 5, 2012

# What is DNA?
- a system for easily viewing and analyzing EDN
- "EDN"" -> "ADN"" -> "DNA""
- Reduces complex database to drag-and-drop pivot table
- All aggregate data
- but can drillthrough to individual records

# Where Does DNA Fit In?

![](file:images/DNA-High-level-001.jpg)

# DNA harmonizes data from:
 - 3 notification systems
 -- AIS (1997--2000)
 -- IMP (2000--2009)
 -- EDN (2007--present)
 - Mulitple DS-forms 

# How can I access DNA?
- via SQL-Server 2008 Business Intelligence Design Studio
- Excel
 -- make connection
 -- drag-and-drop
 -- update when needed
 
# All DGMQ granted read-only access
* Drillthrough (linelist with names) available, when appropriate
* John and Zanju can change design 

# Where is DNA Documentation?
## //cdc/project/ccid_ncpdcid_dgmq/IRMH/_Epi%20Team/DNA
![](file:images/DNA documentation location.png)


# What is counted?
- DNA counts records (other values, eg. sum are available)
- vwEDN = count of EDN records, vwIMP= count of IMP, etc.
- 'All' = sum of all records from AIS, IMP, EDN
- For AIS and IMP, limitted data 
 -- Only arrival date, TB class, country of birth 
-For EDN  
- - Every field in DS forms available
- WRAPS for counts of refugee records (DOS)
 -- Special variables not captured on DS forms
 -- Nationality, ethnicity, language, marital status, relationship
- Immigration Yearbook for counts of Total Immigrants (DHS)
 -- Can be used as denominator data for calculating rates among immigrants

# DNA dimensions
- dimensions are like a variable, but may be cascading
- First, Lets look at some traditional, 'flat', dimensions (WRAPS, Syphilis, AlienChestXray)
- now, lets look at some Cascading dimensions (BirthCountry, TBClass)

# Date ranges
- Fiscal or calendar year
- Month of year (May, 2012) or calendar month (May)
- Same for quarter, week, and day

# Tables 
- placeholder for online demo
 -- arrivals by Q station
 -- arrivals by Visa Type and Class B TB

# Visualize Data
- Import data into other packages (R)

# Refugees 

![](file:images/refugee-arrival-cumsum.png)

# TB Notifications (Immigrants and Refugees)
- By Country

![](file:images/TB-by-country.png)

# TB Notifications (Immigrants and Refugees)
- By State

![](file:images/TB-by-state.png)

# Refugee Arrivals by Day

- 2003--2008, strong seasonal pattern
 -- 1,179 arrived September27, 2008
- Since 2007, fewer extreme days

![](file:images/Refugee-Arrival-HeatMap.png)


# EDN Notifications, June 2012
- Each line represents a person

![](file:images/Incoming.png)

# EDN Notifications, June 2012
- Zoom into continental US

![](file:images/Incoming-zoom.png)

# HTML5 slides
- Several programs generate slides from a generic markup file (.md)
- R creates the .md file
- PanDoc converts to 

  -- Slidy
   ---pandoc -s -S -i -t slidy --mathjax DNA-Presentation.md -o DNA-Presentation-slidy.html
   --- OR
    ---- library(slidify)
    ---- slidify("DNA-Presentation.Rmd")

