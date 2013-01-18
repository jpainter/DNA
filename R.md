



# R 

Programming Literate and Reproducible Research

John Painter

# Visualizating Data

- The larger the data, the greater the need to see it in pictures
- Challenge to visualize data with 'traditional' software
 * SAS has limited options
 * Excel poor choice for data management
 * Propriatary software expensive (ArcGis) and limited (ArcGis=maps only)
- Some of best visualizations done by newer (and cheaper) software

# New York Times

- The staff of New York Times info graphics department is...[video please](http://vimeo.com/21020824)

- NYT [blog](http://chartsnthings.tumblr.com/) about how they brainstorm and design effective graphics

- Former intern has popular book and web-site on visualizations, [Flowing Data](http://flowingdata.com/) 

- Basic workflow is [R](http://www.r-project.org/) for data and chart, Adobe for post chart clean up, and convert to [D3 (d3js.org)](http://d3js.org/) for interactive, web-based version

# R is
- Free
- More advanced than SAS, STATA, et al.
- Better visualizations
- Better for large data sets (CERN)

# Free

- IBM wrote first code, "S", and published it for anyone to use
- S-plus licensed a version
- Two guys in NZ (Ross I. and Robert G.) published a free version, "R"
- Add-on code downloaded as needed from 87 reposisitories (17 in US)
- [RStudio (rstudio.org)](http://rstudio.org/) for [R (r-project.org)](http://www.r-project.org/) provides GUI for organizing projects, files, and plots

# Advanced

- Around 3,500 specialized packages
- Authors are stats profressors, engineers, end-users
- Becoming a primary software for academic biostatistics, like [Vanderbilt](http://biostat.mc.vanderbilt.edu/wiki/Main/RS)

# Visualizations
- Wilkinson and Tufte
- 'Grammar of Graphics' 
- Examples online are motivational
 * [Mariano Rivera's saves](http://www.nytimes.com/interactive/2012/05/05/sports/baseball/mariano-rivera-and-his-peers.html?ref=baseball)
 * [Maritime trade routes](http://spatialanalysis.co.uk/2012/03/mapped-british-shipping-1750-1800/)
 * [London cycle journey](http://spatialanalysis.co.uk/2012/02/great-maps-ggplot2/)
 * [Wind patterns](http://hint.fm/wind/)
 
# GoogleMaps from R data 
## The map below is an example of how to send data through google and get a dynamic map back.  Note:  need internet connection to get map (a disadvantage compared with D3 interactive images)
<div class="warning"><pre class="knitr">## Warning: package 'googleVis' was built under R version 2.15.1
</pre></div><!-- GeoMap generated in R 2.15.0 by googleVis 0.2.16 package -->
<!-- Fri Jul 06 04:33:20 2012 -->


<!-- jsHeader -->
<script type="text/javascript" src="http://www.google.com/jsapi">
</script>
<script type="text/javascript">
 
// jsData 
function gvisDataGeoMapID67c2332783b ()
{
  var data = new google.visualization.DataTable();
  var datajson =
[
 [
 "Alabama",
        2.1 
],
[
 "Alaska",
        1.5 
],
[
 "Arizona",
        1.8 
],
[
 "Arkansas",
        1.9 
],
[
 "California",
        1.1 
],
[
 "Colorado",
        0.7 
],
[
 "Connecticut",
        1.1 
],
[
 "Delaware",
        0.9 
],
[
 "Florida",
        1.3 
],
[
 "Georgia",
          2 
],
[
 "Hawaii",
        1.9 
],
[
 "Idaho",
        0.6 
],
[
 "Illinois",
        0.9 
],
[
 "Indiana",
        0.7 
],
[
 "Iowa",
        0.5 
],
[
 "Kansas",
        0.6 
],
[
 "Kentucky",
        1.6 
],
[
 "Louisiana",
        2.8 
],
[
 "Maine",
        0.7 
],
[
 "Maryland",
        0.9 
],
[
 "Massachusetts",
        1.1 
],
[
 "Michigan",
        0.9 
],
[
 "Minnesota",
        0.6 
],
[
 "Mississippi",
        2.4 
],
[
 "Missouri",
        0.8 
],
[
 "Montana",
        0.6 
],
[
 "Nebraska",
        0.6 
],
[
 "Nevada",
        0.5 
],
[
 "New Hampshire",
        0.7 
],
[
 "New Jersey",
        1.1 
],
[
 "New Mexico",
        2.2 
],
[
 "New York",
        1.4 
],
[
 "North Carolina",
        1.8 
],
[
 "North Dakota",
        0.8 
],
[
 "Ohio",
        0.8 
],
[
 "Oklahoma",
        1.1 
],
[
 "Oregon",
        0.6 
],
[
 "Pennsylvania",
          1 
],
[
 "Rhode Island",
        1.3 
],
[
 "South Carolina",
        2.3 
],
[
 "South Dakota",
        0.5 
],
[
 "Tennessee",
        1.7 
],
[
 "Texas",
        2.2 
],
[
 "Utah",
        0.6 
],
[
 "Vermont",
        0.6 
],
[
 "Virginia",
        1.4 
],
[
 "Washington",
        0.6 
],
[
 "West Virginia",
        1.4 
],
[
 "Wisconsin",
        0.7 
],
[
 "Wyoming",
        0.6 
] 
];
data.addColumn('string','state.name');
data.addColumn('number','Illiteracy');
data.addRows(datajson);
return(data);
}
 
// jsDrawChart
function drawChartGeoMapID67c2332783b() {
  var data = gvisDataGeoMapID67c2332783b();
  var options = {};
options["dataMode"] = "regions";
options["width"] =    600;
options["height"] =    400;
options["region"] = "US";

     var chart = new google.visualization.GeoMap(
       document.getElementById('GeoMapID67c2332783b')
     );
     chart.draw(data,options);
    

}
  
 
// jsDisplayChart 
function displayChartGeoMapID67c2332783b()
{
  google.load("visualization", "1", { packages:["geomap"] }); 
  google.setOnLoadCallback(drawChartGeoMapID67c2332783b);
}
 
// jsChart 
displayChartGeoMapID67c2332783b()
 
<!-- jsFooter -->  
//-->
</script>
 
<!-- divChart -->
  
<div id="GeoMapID67c2332783b"
  style="width: 600px; height: 400px;">
</div>



# Reproducible Research

- Prime focus of international R conference, [UserR! 2012](http://biostat.mc.vanderbilt.edu/wiki/Main/UseR-2012)
- [Criteria for Reproducible Research](http://www.reproducibleresearch.net/index.php/Main_Page)
- Literate programming: 
  * Documentation (and perhaps a statistical report) and code are maintained in one file *
- An example?  *This* presentation is written in R, using [knitr](http://yihui.name/en/2012/05/how-to-make-html5-slides-with-knitr/) to make HTML5 slides with [Slidy](http://www.w3.org/Talks/Tools/Slidy2/#(1))

# HTML5 slides
- Several programs generate slides from a generic markup file (.md)
- R creates the .md file
- PanDoc converts to 

 -- dzslides 
  ---pandoc -s -S -i -t dzslides --mathjax DNA-Presentation.md -o DNA-Presentation.html

 -- Slidy
   ---pandoc -s -S -i -t slidy --mathjax R.md -o R-slidy.html
   --- OR
    ---- library(slidify)
    ---- slidify("DNA-Presentation.Rmd")


