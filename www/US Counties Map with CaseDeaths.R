##===================US Counties Map with Case/Deaths================##
##Sources:
## Map:      US Census Bureau
## Covid-19: https://raw.githubusercontent.com/COVID19Tracking/associated-data/master/us_census_data/us_census_2018_population_estimates_counties.csv
##Required Packages:
library(sf)
library(tigris)
library(htmltools)
library(dplyr)

# population<-fread('https://raw.githubusercontent.com/COVID19Tracking/associated-data/master/us_census_data/us_census_2018_population_estimates_counties.csv')
# population<-population%>%
#   select("state_name","county","population")
# names(population)[names(population) == "state_name"] <- "state"
# covid_county<-fread('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-2022.csv')
# Final<-left_join(covid_county,population,by=c('state','county'))
# COVID_Countymap(Final,'Ohio',T,F)

COVID_Countymap<-function(data,State,Case,Change){
  
  county_latest<-data%>%
    filter(state==State & county!="Unknown")%>%
    group_by(date,county)
  county_latest1<-county_latest%>%
    group_by(county) %>%
    mutate(lag.case = lag(cases, n = 1, default = NA),
           lag.death= lag(deaths,n=1, default = NA),
           case_rate=(cases/population)*100000,
           death_rate=(deaths/population)*100000,
           change_case=(cases-lag.case),
           change_death=(deaths-lag.death),
           change_case_rate=(cases-lag.case)/lag.case,
           change_death_rate=(deaths-lag.death)/lag.death)
  county_latest2<-county_latest1%>%
    group_by(county)%>%
    filter(date==last(date))

  shape <- counties(state = State,cb=T,resolution = '20m')
  county_latest3<-subset(county_latest2,is.element(county_latest2$county,shape$NAME))
  county_latest3<-county_latest3[order(match(county_latest3$county,
                                             shape$NAME)),]

  if(Case){
    if(Change){
      bins <- c(0,0.01,0.1,10,100,1000,10000, Inf)
      pal <- colorBin("Oranges", 
                      domain = county_latest3$change_case_rate,
                      # domain = data$change_case_rate,
                      bins = bins)
      
      labels<-paste("<p>",
                    county_latest3$county,
                    "</p>",
                    "<p>",
                    "Cases Changing Rate: ",
                    percent(county_latest3$change_case_rate),
                    # percent(data$change_case_rate),
                    "</p>",
                    "<p>",
                    "Cases Change: ",
                    format(
                          county_latest3$change_case,
                           # data$change_case_rate,
                           big.mark=",",
                           scientific=FALSE),
                    "</p>",
                    sep="")
      m<-leaflet()%>%
        addMapPane("labels", zIndex = 430) %>% # Level 3: top
        addMapPane("polygons", zIndex = 420) %>%  # Level 2: middle
        addMapPane("background_map", zIndex = 410) %>% # Level 1: bottom  
        addProviderTiles(providers$CartoDB.PositronNoLabels,
                         options = pathOptions(pane='background_map')) %>%
        addPolygons(data=shape,
                    weight = 1,
                    smoothFactor = 0.5,
                    color = 'white',
                    fillOpacity = 0.8,
                    fillColor = pal(county_latest3$change_case_rate),
                    # fillColor = pal(data$change_case_rate),
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
      
      m}
    else{
      bins <- c(0, 100, 500, 1000, 10000, 20000, 50000, 100000, Inf)
      pal <- colorBin("Blues", 
                      domain = county_latest3$cases, 
                      bins = bins)
      
      labels<-paste("<p>",
                    county_latest3$county,
                    "</p>",
                    "<p>",
                    "Total Cases: ",
                    format(county_latest3$cases,
                           big.mark=",",
                           scientific=FALSE),
                    "</p>",
                    "<p>",
                    "Cases Rate: ",
                    round(county_latest3$case_rate,digits = 0) ,
                    " per 100,000 people",
                    "</p>",
                    sep="")
      m<-leaflet()%>%
        addMapPane("labels", zIndex = 430) %>% # Level 3: top
        addMapPane("polygons", zIndex = 420) %>%  # Level 2: middle
        addMapPane("background_map", zIndex = 410) %>% # Level 1: bottom  
        addProviderTiles(providers$CartoDB.PositronNoLabels,
                         options = pathOptions(pane='background_map')) %>%
        addPolygons(data=shape,
                    weight = 1,
                    smoothFactor = 0.5,
                    color = 'white',
                    fillOpacity = 0.8,
                    fillColor = pal(county_latest3$cases),
                    options = pathOptions(pane='polygons'),
                    highlight=highlightOptions(
                      weight = 5,
                      color = "#666"
                    ),
                    label = lapply(labels,HTML))%>%
        addProviderTiles(
          # providers$CartoDB.Positron,
          providers$Stamen.TonerHybrid,
          options = pathOptions(pane = "labels")
        )
      
      m
    }
    
  }
  else{
    if(Change){
      bins <- c(0,0.01,0.02,0.03,0.04,0.05,0.1, Inf)
      pal <- colorBin("Reds", 
                      domain = county_latest3$change_death_rate, 
                      bins = bins)
      
      labels<-paste("<p>",
                    county_latest3$county,
                    "</p>",
                    "<p>",
                    "Deaths Changing Rate: ",
                    percent(county_latest3$change_death_rate),
                    "</p>",
                    "<p>",
                    "Death Changing: ",
                    format(county_latest3$change_death,
                           big.mark=",",
                           scientific=FALSE) ,
                    
                    "</p>",
                    sep="")
      m<-leaflet()%>%
        addMapPane("labels", zIndex = 430) %>% # Level 3: top
        addMapPane("polygons", zIndex = 420) %>%  # Level 2: middle
        addMapPane("background_map", zIndex = 410) %>% # Level 1: bottom  
        addProviderTiles(providers$CartoDB.PositronNoLabels,
                         options = pathOptions(pane='polygons')) %>%
        addPolygons(data=shape,
                    weight = 1,
                    smoothFactor = 0.5,
                    color = 'white',
                    fillOpacity = 0.8,
                    fillColor = pal(county_latest3$change_death_rate),
                    options =pathOptions(pane='polygons') ,
                    highlight=highlightOptions(
                      weight = 5,
                      color = "#666"
                    ),
                    label = lapply(labels,HTML))%>%
        addProviderTiles(
          # providers$CartoDB.PositronOnlyLabels,
          providers$Stamen.TonerHybrid,
          options = pathOptions(pane = "labels")
        )
      
      m
    }
    else{
      bins <- c(0, 100, 500, 1000, 5000,10000,50000, Inf)
      pal <- colorBin("Reds", 
                      domain =county_latest3$deaths, 
                      bins = bins)
      
      labels<-paste("<p>",
                    county_latest3$county,
                    "</p>",
                    "<p>",
                    "Total Deaths: ",
                    format(county_latest3$deaths,
                           big.mark=",",
                           scientific=FALSE),
                    "</p>",
                    "<p>",
                    "Death Rate: ",
                    round(county_latest3$death_rate,digits = 0) ,
                    " per 100,000 people",
                    "</p>",
                    sep="")
      m<-leaflet()%>%
        addMapPane("labels", zIndex = 430) %>% # Level 3: top
        addMapPane("polygons", zIndex = 420) %>%  # Level 2: middle
        addMapPane("background_map", zIndex = 410) %>% # Level 1: bottom  
        addProviderTiles(providers$CartoDB.PositronNoLabels,
                         options = pathOptions(pane='background_map')) %>%
        addPolygons(data=shape,
                    weight = 1,
                    smoothFactor = 0.5,
                    color = 'white',
                    fillOpacity = 0.8,
                    fillColor = pal(county_latest3$deaths),
                    options = pathOptions(pane="polygons"),
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
}
