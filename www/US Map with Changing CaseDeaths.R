##===================US Map with Changing Case/Deaths================##
##Sources:
## Map:      https://www.census.gov/geographies/mapping-files/2016/geo/carto-boundary-file.html
## Covid-19: https://github.com/nytimes/covid-19-data/blob/master/live/us-counties.csv
## Census Data: https://map-rfun.library.duke.edu/02_choropleth.html#variables
##Required Packages
library(geojsonio)
library(leaflet)
library(htmltools)
library(scales)
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)##For str_to_title
library(tidycensus)
##Example
##
# 
# us_his<-fread('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv')
# 
# census_api_key("4b9473175cd5c1c8673b2e7c40375e0f47d64589",
#                install = TRUE,
#                overwrite=TRUE)
# population <- get_acs(geography = "state",
#                       variables = "B01003_001"
#                       )%>%
#   select('NAME',"estimate")
# names(population)[names(population) == "NAME"] <- "state"
# names(population)[names(population) == "estimate"] <- "Population"
# data<-left_join(us_his,population,by="state")%>%
#   mutate(Population=ifelse(state=='Virgin Islands',106977,
#                            ifelse(state=='Guam',165768,
#                            ifelse(state=="Northern Mariana Islands",56882,Population))))
# COVID_USmap(data,T,F)
COVID_USmap<-function(data, Cases, Change){
  
  us_latest<-data
  us_latest1<-us_latest%>%
    arrange(state,date)%>%
    group_by(state) %>%
    mutate(lag.case = lag(cases, n = 1, default = NA),
           lag.death= lag(deaths,n=1, default = NA),
           case_rate=(cases/Population)*100000,
           death_rate=(deaths/Population)*100000,
           change_case=(cases-lag.case),
           change_death=(deaths-lag.death),
           change_case_rate=(cases-lag.case)/lag.case,
           change_death_rate=(deaths-lag.death)/lag.death)
  us_latest2<-us_latest1%>%
    group_by(state)%>%
    filter(date==last(date))
  states <- geojson_read("https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", 
                         what = "sp")
  
  us_latest3<-subset(us_latest2,is.element(us_latest2$state,states$name))
  us_latest3<-us_latest3[order(match(us_latest3$state,
                                     states$name)),]
  
  if (Cases){
    if(Change){
      bins <- c(0,0.01,0.05,0.08,0.1,0.15, Inf)
      pal <- colorBin("Oranges", 
                      domain = us_latest3$change_case_rate, 
                      bins = bins)
      
      labels<-paste("<p>",
                    us_latest3$state,
                    "</p>",
                    "<p>",
                    "Cases Changing Rate:",
                    percent(us_latest3$change_case_rate),
                    "</p>",
                    "<p>",
                    "Cases Changing:",
                    format(us_latest3$change_case,
                           big.mark=",",
                           scientific=FALSE) ,
                    
                    "</p>",
                    
                    sep="")
      m<-leaflet()%>%
        setView(-96, 37.8, 5)%>%
        addMapPane("labels", zIndex = 430) %>% # Level 3: top
        addMapPane("polygons", zIndex = 420) %>%  # Level 2: middle
        addMapPane("background_map", zIndex = 410) %>% # Level 1: bottom    
        addProviderTiles(providers$CartoDB.PositronNoLabels,
                         options = pathOptions(pane = "background_map")) %>%
        addPolygons(data=states,
                    weight = 1,
                    smoothFactor = 0.5,
                    color = 'white',
                    fillOpacity = 0.8,
                    fillColor = pal(us_latest3$change_case_rate),
                    options = pathOptions(pane='polygons'),
                    highlight=highlightOptions(
                      weight = 5,
                      color = "#666"
                    ),
                    label = lapply(labels,HTML))%>%
        addProviderTiles(
          #providers$CartoDB.PositronOnlyLabels,
          providers$Stamen.TonerHybrid,
          options = pathOptions(pane = "labels")
        )
      m
    }
    else{
      bins <- c(0, 10000, 50000, 100000, 1000000, 2000000, 5000000, 10000000, Inf)
      pal <- colorBin("Blues", 
                      domain = us_latest3$cases, 
                      bins = bins)
      
      labels<-paste("<p>",
                    us_latest3$state,
                    "</p>",
                    "<p>",
                    "Total Cases:",
                    format(us_latest3$cases,
                           big.mark=",",
                           scientific=FALSE),
                    "</p>",
                    "<p>",
                    "Cases Rate: ",
                    round(us_latest3$case_rate,digits = 0) ,
                    " per 100,000 people",
                    "</p>",
                    sep="")
      m<-leaflet()%>%
        setView(-96, 37.8, 5)%>%
        addMapPane("labels", zIndex = 430) %>% # Level 3: top
        addMapPane("polygons", zIndex = 420) %>%  # Level 2: middle
        addMapPane("background_map", zIndex = 410) %>% # Level 1: bottom   
        addProviderTiles(providers$CartoDB.PositronNoLabels,
                         options = pathOptions(pane='background_map')) %>%
        addPolygons(data=states,
                    weight = 1,
                    smoothFactor = 0.5,
                    color = 'white',
                    fillOpacity = 0.8,
                    fillColor = pal(us_latest3$cases),
                    options = pathOptions(pane='polygons'),
                    highlight=highlightOptions(
                      weight = 5,
                      color = "#666"
                    ),
                    label = lapply(labels,HTML))%>%
        addProviderTiles(
          #providers$CartoDB.PositronOnlyLabels,
          providers$Stamen.TonerHybrid,
          options = pathOptions(pane = "labels")
        )
      m
    }
  }
  
  else{
    if(Change){
      bins <- c(0,0.001,0.005,0.015,0.02, Inf)
      pal <- colorBin("Purples", 
                      domain = us_latest3$change_death_rate, 
                      bins = bins)
      
      labels<-paste("<p>",
                    us_latest3$state,
                    "</p>",
                    "<p>",
                    "Deaths Changing Rate:",
                    percent(us_latest3$change_death_rate),
                    "</p>",
                    "<p>",
                    "Death Changing:",
                    format(us_latest3$change_death,
                           big.mark=",",
                           scientific=FALSE) ,
                    
                    "</p>",
                    sep="")
      m<-leaflet()%>%
        setView(-96, 37.8, 5)%>%
        addMapPane("labels", zIndex = 430) %>% # Level 3: top
        addMapPane("polygons", zIndex = 420) %>%  # Level 2: middle
        addMapPane("background_map", zIndex = 410) %>% # Level 1: bottom  
        addProviderTiles(providers$CartoDB.PositronNoLabels,
                         options = pathOptions(pane='background_map')) %>%
        addPolygons(data=states,
                    weight = 1,
                    smoothFactor = 0.5,
                    color = 'white',
                    fillOpacity = 0.8,
                    fillColor = pal(us_latest3$change_death_rate),
                    options = pathOptions(pane='polygons'),
                    highlight=highlightOptions(
                      weight = 5,
                      color = "#666"
                    ),
                    label = lapply(labels,HTML))%>%
        addProviderTiles(
          providers$Stamen.TonerHybrid,
          #providers$CartoDB.PositronOnlyLabels,
          options = pathOptions(pane='labels'))
      m
    }
    else{
      bins <- c(0, 100, 500, 1000, 5000,10000,50000, Inf)
      pal <- colorBin("Reds", 
                      domain = us_latest3$deaths, 
                      bins = bins)
      
      labels<-paste("<p>",
                    us_latest3$state,
                    "</p>",
                    "<p>",
                    "Total Deaths:",
                    format(us_latest3$deaths,
                           big.mark=",",
                           scientific=FALSE),
                    "</p>",
                    "<p>",
                    "Death Rate: ",
                    round(us_latest3$death_rate,digits = 0) ,
                    " per 100,000 people",
                    "</p>",
                    
                    sep="")
      m<-leaflet()%>%
        setView(-96, 37.8, 5)%>%
        addMapPane("labels", zIndex = 430) %>% # Level 3: top
        addMapPane("polygons", zIndex = 420) %>%  # Level 2: middle
        addMapPane("background_map", zIndex = 410) %>% # Level 1: bottom  
        addProviderTiles(providers$CartoDB.PositronNoLabels,
                         options = pathOptions(pane='background_map')) %>%
        addPolygons(data=states,
                    weight = 1,
                    smoothFactor = 0.5,
                    color = 'white',
                    fillOpacity = 0.8,
                    fillColor = pal(us_latest3$deaths),
                    options = pathOptions(pane='polygons'),
                    highlight=highlightOptions(
                      weight = 5,
                      color = "#666"
                    ),
                    label = lapply(labels,HTML))%>%
        addProviderTiles(
          providers$Stamen.TonerHybrid,
          #providers$CartoDB.PositronOnlyLabels,
          options = pathOptions(pane='labels'))
      m
    }
  }
}