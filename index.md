---
title       : Hypothesis Testing of the Rising of Earth Surface Temperature
subtitle    : 
author      : Bertrand Rigaldies
job         : Data Science Enthusiast
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---



## Climate Change - Is your country warming up?

1. Some climate change questions that have captivated us:
    + Have temperatures been rising in your country?
    + Since when, or during what period?
    + Is the temperature rise statistical significant?
1. Data Exploration
1. Statistical Inference
1. Check out my [Shinyapp](https://brigaldies.shinyapps.io/ClimateChange/).

--- .class #id 

## The Data: Earth Surface Temperature Data





1. Source: [Kaggle](https://www.kaggle.com/berkeleyearth/climate-change-earth-surface-temperature-data)
2. Sample data (New York, United States, 2013)

<!-- html table generated in R 3.1.3 by xtable 1.8-2 package -->
<!-- Fri May 27 11:52:22 2016 -->
<table border=1>
<tr> <th>  </th> <th> Date </th> <th> Country </th> <th> City </th> <th> AvgTemperature </th> <th> AvgTempUncertainty </th> <th> Lat </th> <th> Long </th>  </tr>
  <tr> <td align="right"> 1 </td> <td> 2013-01-01 </td> <td> United States </td> <td> New York </td> <td align="right"> -0.968 </td> <td align="right"> 0.290 </td> <td align="right"> 40.99N </td> <td align="right"> 74.56W </td> </tr>
  <tr> <td align="right"> 2 </td> <td> 2013-02-01 </td> <td> United States </td> <td> New York </td> <td align="right"> -1.365 </td> <td align="right"> 0.241 </td> <td align="right"> 40.99N </td> <td align="right"> 74.56W </td> </tr>
  <tr> <td align="right"> 3 </td> <td> 2013-03-01 </td> <td> United States </td> <td> New York </td> <td align="right"> 2.518 </td> <td align="right"> 0.255 </td> <td align="right"> 40.99N </td> <td align="right"> 74.56W </td> </tr>
  <tr> <td align="right"> 4 </td> <td> 2013-04-01 </td> <td> United States </td> <td> New York </td> <td align="right"> 9.723 </td> <td align="right"> 0.355 </td> <td align="right"> 40.99N </td> <td align="right"> 74.56W </td> </tr>
  <tr> <td align="right"> 5 </td> <td> 2013-05-01 </td> <td> United States </td> <td> New York </td> <td align="right"> 15.544 </td> <td align="right"> 0.281 </td> <td align="right"> 40.99N </td> <td align="right"> 74.56W </td> </tr>
  <tr> <td align="right"> 6 </td> <td> 2013-06-01 </td> <td> United States </td> <td> New York </td> <td align="right"> 20.892 </td> <td align="right"> 0.273 </td> <td align="right"> 40.99N </td> <td align="right"> 74.56W </td> </tr>
  <tr> <td align="right"> 7 </td> <td> 2013-07-01 </td> <td> United States </td> <td> New York </td> <td align="right"> 24.722 </td> <td align="right"> 0.279 </td> <td align="right"> 40.99N </td> <td align="right"> 74.56W </td> </tr>
  <tr> <td align="right"> 8 </td> <td> 2013-08-01 </td> <td> United States </td> <td> New York </td> <td align="right"> 21.001 </td> <td align="right"> 0.323 </td> <td align="right"> 40.99N </td> <td align="right"> 74.56W </td> </tr>
  <tr> <td align="right"> 9 </td> <td> 2013-09-01 </td> <td> United States </td> <td> New York </td> <td align="right"> 17.408 </td> <td align="right"> 1.048 </td> <td align="right"> 40.99N </td> <td align="right"> 74.56W </td> </tr>
   </table>

--- .class #id 

## Temperature Measurement Uncertainty

Temperature **measurement uncertainty** has decreased over time: For example, for New York, United States, the measurements have improved since the early 1900's. Something to consider when choosing a time period to analyze the data.

<img src="assets/fig/temperature-uncertainty-1.png" title="plot of chunk temperature-uncertainty" alt="plot of chunk temperature-uncertainty" style="display: block; margin: auto;" />

--- .class #id 

## Temperature Increase Hypothesis Testing

Hypothesis H0: The temperature in your city has **NOT** increased.
T.Test result: Rejected for all cities in the United States.

<img src="assets/fig/temperature-trend-and-ho-test-1.png" title="plot of chunk temperature-trend-and-ho-test" alt="plot of chunk temperature-trend-and-ho-test" style="display: block; margin: auto;" />
