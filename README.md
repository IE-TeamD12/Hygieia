# IE_Hygieia_Shiny_Apps
All the Shiny Application development that took place in the final semester's Industrial Experience project as part of the Hygieia team at Monash University.

## Twitter Extraction Widget

The Twitter Shiny application helps the user understand what people are talking about on infections in the online world. It also helps them understand which organizations are concerned about one’s health (e.g. WHO, CDC) by looking at the most popular twitter handles which tweet about a particular infection, pandemic news etc. Although it is a general Tweets extraction application, we have left it upto the user to search whatever he/she wants. 

In the making of this application, a Twitter developer account was used which can be accessed here : https://developer.twitter.com/en , this platform helps the developer, like us, to create an API (Application Programming Interface) to connect to core Twitter data over the past few days and bring them into a statistical analysis software like R, SPSS etc. To be successful in connecting to Twitter, we used access tokens and other keys to connect to our developer account application on Twitter developer. In our case, we used to connect the API in R, so that we could import the data into the Shiny framework to build our application. 

Shiny is an open-source R package that makes it easy to build interactive web apps straight from R. We used shinyapp.io to host our webapp, as it is as simple as pushing a button to deploy the web application to cloud. We have used a dynamic wordcloud to show the most number of words that pop up for one particular topic, as we believe that it is pretty good at summarizing what is being talked about one particular topic on online. 


## Plotly Viz Widget
This application helps the user understand the distribution of certain top diseases across industries, how they spread and what has been their trend over the past few years as well as how gender and age plays an important role in spread.  The data has been collected from one of the Australian Government agencies which are like CDC in the United States. The data shows us the prevalent infections with number of cases in each gender and in which year the cases were recorded. 

We have used Plotly, Open Source Graphing Library Examples of how to make line plots, scatter plots, area charts, bar charts, error bars, box plots, histograms, heatmaps, subplots, multiple-axes, and 3D (WebGL based) charts. Plotly R is free and open source and you can view the source, report issues or contribute on GitHub. You can access Plotly here : https://plotly.com/r/


## Weather API Widget

This applications warns the user about drastic changes in not just the temperature, but also wind speed, humidity and pressure. The user can also look at the various visualizations to better understand the weather for the next 5 days an prepare his/her day well in advance. In our personal experience, we have seen people who are very sensitive to minor temperature fluctuations and fall sick. The app has been made specifically for Melbourne, Victoria, Australia as a proof of concept and can be extended to include other major cities in other countries as well. This application paints an overall good picture with what’s to expect in the next 5 days with respect to weather and has state-of-the-art forecasts which makes everyone’s life easier.

The data is being fetched from OpenWeather API which can be accessed here : https://openweathermap.org/ . The platform/API allows us as developers to access weather data (both current and forecasted) by using an ‘API key’ to connect to our developer account. We have used the key to connect and call the API to and from R, so that we could build up our visualizations and statistics on Shiny framework. Shiny is an open-source R package that makes it easy to build interactive web apps straight from R. We used shinyapp.io to host our webapp, as it is as simple as pushing a button to deploy the web application to cloud. 

As something extra, we also implemented customized warnings which popup using the Shinyalert package in R, when there is a fluctuation in temperature of more than 3 degrees on both sides. This, we believe will help the more temperature-sensitive part of our user base to prepare for the day well in advance. 


## Nearest Hospital Finder

This application helps the user find the nearest hospital in Victoria, Australia based on LGA (Local Government Area), Type of Hospital (Public or Private) and most importantly, by the preferred language of the user (English, Hindi, Mandarin, Arabic etc.). The list of hospital datasets comes from a government website and includes the coordinates for the hospitals, their names, which LGA they are situated in, their postcode etc. To make things interesting and to develop a proof of concept, we randomly added languages as a new column to showcase that we can match people with Hospitals based on their preferred language. For e.g. if an Iranian man wants to see a doctor who speaks Farsi, as the man is not very good at English, then it would be one hell of a problem to locate a doctor who could. But not with our application, we can pinpoint exactly where the Hospital is located on the map and give information about it to the concerned person.

We used Leaflet, which is one of the most popular open-source javascript library for interactive maps. It’s used by websites like Github, NY Times, Flickr etc. for their platforms. Leaflet allows us to integrate an interactive map and build on top of it, it allows us to add markers, add polygons, add different shapes, play around with the coordinate system, draw points to cover an area, cluster points on a map as one etc. We can also use custom map layers as basemaps, create popups for the user and much more. The best part is that it works effortlessly with R and especially Shiny, it has it’s own render functions for input and output. You can read more about the library at : https://rstudio.github.io/leaflet/ 

# IE Tableau

Consists of 2 tableau workbooks, and the data is included with the workbook,the descriptions of the workbooks are as follows:

1. **Infection Prevention** - It is a dashboard that maps the preventive measures to each infection;

2. **Vaccines** - It is comaprision of vaccines available in Australia against the ones that maybe available in your origin country. The dashboard provides a type in filter where you can type in your origin country to search for the vaccines available in your country. 
- The filter is implemented by creating a Calculated Field with the calculation for the filter and it is then mapped to a Parameter called Type in Search Filter which is displayed as the filter tab on the dashboard.
