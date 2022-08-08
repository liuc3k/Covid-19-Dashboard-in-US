##=============Covid-19 Tracking App(Version 2)============================##
##Reference:
# http://shinyapps.dreamrs.fr/shinyWidgets/
##working directory
# setwd('C:/Users/karin/Desktop/Kaggle')

library(shiny)
library(shinythemes)
library(DT)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggmap)
library(tidyverse)
library(scales)
library(shinyWidgets)
library(rsconnect)
library(maps)
#setwd('C:/Users/karin/Desktop/Kaggle')

##===============Cases/Death Report=================##
Report_us<-function(data,Cases,Change){
  
  
  us<-data%>%
    group_by(date)%>%
    summarise(Cases=sum(cases),
              Death=sum(deaths))%>%
    mutate(Diff_Case = (Cases - lag(Cases)),
           Diff_Death = (Death - lag(Death)))
  if(Cases){
    if(Change){
      fig <- plot_ly(us,
                     x = ~date, 
                     y = ~Diff_Case,
                     
                     name = paste('Cases'), 
                     type = 'bar',
                     hovertemplate = paste('Cases: %{y}',
                                           '<br>Date: %{x}'),
                     showlegend = T)%>%
        
        layout(title = paste("Daily confirmed new cases in US"),
               xaxis = list(title = "Dates",
                            tickangle=-90),
               yaxis = list (title = ""))
      fig
    }
    else{
      fig <- plot_ly(us,
                     x = ~date, 
                     y = ~Cases,
                     type = 'scatter', 
                     mode = 'lines',
                     name = paste('Cases'), 
                     hovertemplate = paste('Cases: %{y}',
                                           '<br>Date: %{x}'),
                     showlegend = T)%>%
        
        layout(title = paste("Cumulative Confirmed Cases in US"),
               xaxis = list(title = "Dates",
                            tickangle=-90),
               yaxis = list (title = ""))
      fig
    }
  }
  else {
    if(Change){
      fig <- plot_ly(us,
                     x = ~date, 
                     y = ~Diff_Death,
                     type = 'bar',
                     name = paste('Deaths'), 
                     hovertemplate = paste('Death: %{y}',
                                           '<br>Date: %{x}'),
                     marker=list(color='rgb(205, 12, 24)'),
                     
                     
                     showlegend = T)%>%
        
        layout(title = paste("Daily Confirmed New Deaths in US"),
               xaxis = list(title = "Dates",
                            tickangle=-90),
               yaxis = list (title = ""))
      fig
    }
    else{
      fig <- plot_ly(us,
                     x = ~date, 
                     y = ~Death,
                     
                     name = paste('Deaths'), 
                     type = 'scatter', 
                     mode = 'lines',
                     hovertemplate = paste('Cases: %{y}',
                                           '<br>Date: %{x}'),
                     line  = list(color='rgb(205, 12, 24)'),
                     showlegend = T)%>%
        
        layout(title = paste("Cumulative Confirmed Deaths in US"),
               xaxis = list(title = "Dates",
                            tickangle=-90
               ),
               yaxis = list (title = ""))
      fig
    }
    
  }
}
Report_county<-function(data,State,Cases,Change){
  us<-data%>%
    filter(state==State)%>%
    group_by(date)%>%
    summarise(Cases=sum(cases),
              Death=sum(deaths))%>%
    mutate(Diff_Case = (Cases - lag(Cases)),
           Diff_Death = (Death - lag(Death)))
  
  if(Cases){
    if(Change){
      fig <- plot_ly(us,
                     x = ~date, 
                     y = ~Diff_Case,
                     hovertemplate = paste('Cases: %{y}',
                                           '<br>Date: %{x}'),
                     name = paste('Cases'), 
                     type = 'bar',
                     
                     showlegend = T)%>%
        
        layout(title = paste("Daily Confirmed New Cases in",State),
               xaxis = list(title = "Dates",
                            tickangle=-90),
               yaxis = list (title = ""))
      fig
    }
    else{
      fig <- plot_ly(us,
                     x = ~date, 
                     y = ~Cases,
                     type = 'scatter', 
                     mode = 'lines',
                     name = paste('Cases'), 
                     hovertemplate = paste('Cases: %{y}',
                                           '<br>Date: %{x}'),
                     showlegend = T)%>%
        
        layout(title = paste("Cumulative Confirmed Cases in", State),
               xaxis = list(title = "Dates",
                            tickangle=-90),
               yaxis = list (title = ""))
      fig
    }
    
  }
  else{
    if(Change){
      fig <- plot_ly(us,
                     x = ~date, 
                     y = ~Diff_Death,
                     hovertemplate = paste('Deaths: %{y}',
                                           '<br>Date: %{x}'),
                     name = paste('Deaths'), 
                     type = 'bar',
                     marker=list(color='rgb(205, 12, 24)'),
                     showlegend = T)%>%
        
        layout(title = paste("Daily Confirmed New Deaths in",State),
               xaxis = list(title = "Dates",
                            tickangle=-90),
               yaxis = list (title = ""))
      fig
    }
    else{
      fig <- plot_ly(us,
                     x = ~date, 
                     y = ~Death,
                     
                     name = paste('Deaths'), 
                     type = 'scatter', 
                     mode = 'lines',
                     hovertemplate = paste('Deaths: %{y}',
                                           '<br>Date: %{x}'),
                     line  = list(color='rgb(205, 12, 24)'),
                     showlegend = T)%>%
        
        layout(title = paste("Cumulative Confirmed Deaths in" ,State),
               xaxis = list(title = "Dates",
                            tickangle=-90
               ),
               yaxis = list (title = ""))
      fig
    }
    
    
  }  
  
}


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
      bins <- c(0,0.01,0.02,0.03,0.04,0.05,0.1, Inf)
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
      bins <- c(0, 100, 500, 1000, 10000, 20000, 50000, 100000, Inf)
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



##===================US Counties Map with Case/Deaths================##
##Sources:
## Map:      US Census Bureau
## Covid-19: https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv
##Required Packages:
library(sf)
library(tigris)
library(htmltools)
COVID_Countymap<-function(data,State,Case,Change){
  
  county_latest<-data%>%
    filter(state==State & county!="Unknown")%>%
    group_by(date,county)
  county_latest1<-county_latest%>%
    group_by(county) %>%
    mutate(lag.case = lag(cases, n = 1, default = NA),
           lag.death= lag(deaths,n=1, default = NA),
           change_case=(cases-lag.case)/lag.case,
           change_death=(deaths-lag.death)/lag.death)
  county_latest2<-county_latest1%>%
    group_by(county)%>%
    filter(date==last(date))
  
  shape <- counties(state = State,cb=T,resolution = '20m')  
  county_latest3<-subset(county_latest2,is.element(county_latest2$county,shape$NAME))
  county_latest3<-county_latest3[order(match(county_latest3$county,
                                             shape$NAME)),]
  
  if(Case){
    if(Change){
      bins <- c(0,0.01,0.02,0.03,0.04,0.05,0.1, Inf)
      pal <- colorBin("Oranges", 
                      domain = county_latest3$change_case, 
                      bins = bins)
      
      labels<-paste("<p>",
                    county_latest3$county,
                    "</p>",
                    "<p>",
                    "Cases:",
                    percent(county_latest3$change_case),
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
                    fillColor = pal(county_latest3$change_case),
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
                    "Cases:",
                    format(county_latest3$cases,
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
                    fillColor = pal(county_latest3$cases),
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
      bins <- c(0,0.01,0.02,0.03,0.04,0.05,0.1, Inf)
      pal <- colorBin("Reds", 
                      domain = county_latest3$change_death, 
                      bins = bins)
      
      labels<-paste("<p>",
                    county_latest3$county,
                    "</p>",
                    "<p>",
                    "Deaths:",
                    percent(county_latest3$change_death),
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
                    fillColor = pal(county_latest3$change_death),
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
                    "Deaths:",
                    format(county_latest3$deaths,
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

###=============================Summary Table for state-level=======##
##Required Packages:
library(DT)
###Example:
# us_his<-fread('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv')
# 
# us_latest<-data
# us_latest1<-us_latest%>%
#   arrange(state,date)%>%
#   group_by(state) %>%
#   mutate(lag.case = lag(cases, n = 1, default = NA),
#          lag.death= lag(deaths,n=1, default = NA),
#          case_rate=(cases/Population)*100000,
#          death_rate=(deaths/Population)*100000,
#          change_case=(cases-lag.case),
#          change_death=(deaths-lag.death),
#          change_case_rate=(cases-lag.case)/lag.case,
#          change_death_rate=(deaths-lag.death)/lag.death)
# us_latest2<-us_latest1%>%
#   group_by(state)%>%
#   filter(date==last(date))
# states <- geojson_read("https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", 
#                        what = "sp")
# 
# us_latest3<-subset(us_latest2,is.element(us_latest2$state,states$name))
# us_latest3<-us_latest3[order(match(us_latest3$state,
#                                    states$name)),]
#Summary_table(us_latest3,T,F)

Summary_table<-function(data,change,case){
  if(change){
    if(case){
      DT<-datatable(data%>%
                      select('state',"change_case")%>%
                      arrange(desc(change_case)),
                    rownames = FALSE,
                    colnames = c('State'='state',
                                 "Changing Cases"="change_case"), 
                    options = list(dom = 'tp'))
      DT}
    else{
      DT<-datatable(data%>%
                      select('state',"change_death")%>%
                      arrange(desc(change_death)),
                    rownames = FALSE,
                    colnames = c('State'='state',
                                 "Deaths"="change_death"), 
                    options = list(dom = 'tp'))
      DT
    }
  }
  else{
    if(case){
      DT<-datatable(data%>%
                      select('state',"cases")%>%
                      arrange(desc(cases)),
                    rownames = FALSE,
                    colnames = c('State'='state',
                                 "Cases"="cases"), 
                    options = list(dom = 'tp'))
      DT}
    else{
      DT<-datatable(data%>%
                      select('state',"deaths")%>%
                      arrange(desc(deaths)),
                    rownames = FALSE,
                    colnames = c('State'='state',
                                 "Deaths"="deaths"), 
                    options = list(dom = 'tp'))
      DT
    }
  }
  
}
##Read Data
mydata<-fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
counties<-map_data("county")%>%
  mutate(region=str_to_title(region))%>%
  mutate(region=ifelse(region=='District Of Columbia','District of Columbia',region))

#############=======UI========#####
ui<-navbarPage("Covid-19 Tracking App in US",
               navbarMenu("Interactive Map",
                          tabPanel("State-Level",
                                   div(class="outer",
                                       
                                       tags$head(
                                         # Include our custom CSS
                                         includeCSS("www/styles.css")
                                         
                                       ),
                                       
                                       # If not using custom CSS, set height of leafletOutput to a number instead of percent
                                       leafletOutput("us_leaf", width="100%", height="100%"),
                                       
                                       # Shiny versions prior to 0.11 should use class = "modal" instead.
                                       absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                                     draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                                     width = 330, height = "auto",
                                                     
                                                     h2("Covid-19 Explorer"),
                                                     br(),
                                                     h3(textOutput("Cases Total")),
                                                     h3(textOutput("Cases Change")),
                                                     h4(textOutput("Death Total")),
                                                     
                                                     br(),
                                                     radioGroupButtons(
                                                       inputId = "Case_Death_state",
                                                       label = "Death/Cases",
                                                       c('Cases'=T,
                                                         'Deaths'=F),
                                                       justified = TRUE
                                                     ),
                                                     
                                                     br(),
                                                     radioGroupButtons(
                                                       inputId = "Change_state",
                                                       label = "Cumulative/Daily Changing",
                                                       choices = c("Cumulative"=F, 
                                                                   "Daily Changing"=T),
                                                       justified = TRUE
                                                     ),
                                                     conditionalPanel(condition = "input.Case_Death_state==T",
                                                                      
                                                                      DTOutput("state_table")),
                                                     # DTOutput("state_table_cum"),
                                                     h5(textOutput("Today_state"))
                                                     
                                                     
                                                     
                                       )
                                       
                                       
                                   )),
                          tabPanel("County-Level",
                                   div(class="outerct",
                                       
                                       tags$head(
                                         # Include our custom CSS
                                         includeCSS("www/stylesct.css")
                                         
                                       ),
                                       
                                       # If not using custom CSS, set height of leafletOutput to a number instead of percent
                                       leafletOutput("county_leaf", width="100%", height="100%"),
                                       
                                       # Shiny versions prior to 0.11 should use class = "modal" instead.
                                       absolutePanel(id = "controlsct", class = "panel panel-default", fixed = TRUE,
                                                     draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                                     width = 330, height = "auto",
                                                     
                                                     h2("Covid-19 Explorer"),
                                                     br(),
                                                     h3(textOutput("Cases Total County")),
                                                     h4(textOutput("Death Total County")),
                                                     
                                                     br(),
                                                     selectInput('States',
                                                                 'States:',
                                                                 width = "auto",
                                                                 c(unique(as.character(counties$region)))
                                                     ),
                                                     br(),
                                                     radioGroupButtons(
                                                       inputId = "Case_Death_county",
                                                       label = "Death/Cases",
                                                       c('Cases'=T,
                                                         'Deaths'=F),
                                                       justified = TRUE
                                                     ),
                                                     
                                                     br(),
                                                     radioGroupButtons(
                                                       inputId = "Change_county",
                                                       label = "Cumulative/Daily Changing",
                                                       choices = c("Cumulative"=F, 
                                                                   "Daily Changing"=T),
                                                       justified = TRUE),
                                                     h5(textOutput("Today_county")),
                                                     
                                                     
                                                     
                                       )
                                       
                                       
                                   )
                          )
               ),
               tabPanel("Trends",
                        sidebarLayout(
                          sidebarPanel(
                            width = 4,
                            selectInput('Level',
                                        'Levels:',
                                        width = "150",
                                        c('United States',
                                          'County')),
                            conditionalPanel(condition = "input.Level=='County'",
                                             selectInput("county_trends_case",
                                                         'Region:',
                                                         width = "150",
                                                         c(unique(as.character(counties$region)))
                                             )),
                            
                            
                            
                            selectInput('Outcome',
                                        'Outcome:',
                                        width = "150",
                                        c('Cases(Total)'=T,
                                          'Deaths(Total)'=F))
                            
                            
                          ),
                          mainPanel(
                            width =8,
                            tabsetPanel(
                              type='tabs',
                              tabPanel("Cumualtive",
                                       
                                       # conditionalPanel(condition = "input.Level=='County'",
                                       #                  plotlyOutput("cum_county_report")),
                                       
                                       # conditionalPanel(condition = "input.Level=='United States'",
                                       #                  plotlyOutput('cum_us_report'))
                              ),
                              
                              
                              
                              tabPanel("New",
                                       # conditionalPanel(condition = "input.Level=='County'",
                                       #                  plotlyOutput("day_county_report")),
                                       # conditionalPanel(condition = "input.Level=='United States'",
                                       #                  plotlyOutput('day_us_report'))
                              )
                            )
                            
                          )
                        )),
               
               
               tabPanel("Demographic Characteristics"),
               
               tabPanel("Forecasting")
)

###=====SERVER===========#####
server <- function(input, output) {
  
  
  mydata<-fread('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv')
  ##Set Time Format
  Sys.setlocale("LC_TIME", "C")
  ###Import COVID DATA from NY time.
  us_his<-fread('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv')
  us_total<-fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv")
  #census_api_key("4b9473175cd5c1c8673b2e7c40375e0f47d64589",
                 #install = TRUE,
                 #overwrite=TRUE)
  census_api_key("4b9473175cd5c1c8673b2e7c40375e0f47d64589")
  population <- get_acs(geography = "state",
                        variables = "B01003_001"
  )%>%
    select('NAME',"estimate")
  names(population)[names(population) == "NAME"] <- "state"
  names(population)[names(population) == "estimate"] <- "Population"
  data<-left_join(us_his,population,by="state")%>%
    mutate(Population=ifelse(state=='Virgin Islands',106977,
                             ifelse(state=='Guam',165768,
                                    ifelse(state=="Northern Mariana Islands",56882,Population))))
  
  ###Display US map
  output$us_leaf <- renderLeaflet({
    
    COVID_USmap(data,
                input$Case_Death_state,
                input$Change_state)
    
  })
  ###Display County Map
  output$county_leaf<-renderLeaflet(
    {
      COVID_Countymap(mydata,
                      input$States,
                      input$Case_Death_county,
                      input$Change_county)
    }
  )
  ###Display Current date in state-level &COUNTY LEVEL
  output$Today_state<-renderText(
    {
      paste("Last Updated at",max(us_his$date))
    }
  )
  
  ###Display Current date in county-level
  output$Today_county<-renderText(
    {paste("Last Updated at",max(us_his$date))}
  )
  ###Display Total Number for Case/Death in US
  output$`Cases Total`<-renderText({ 
    state_latest<-mydata%>%
      filter(date==last(date))
    
    paste(format(sum(state_latest$cases),
                 big.mark=",",
                 scientific=FALSE),"Confirmed Cases")
  })
  output$`Death Total`<-renderText({ 
    state_latest<-mydata%>%
      filter(date==last(date))
    
    paste(format(sum(state_latest$deaths),
                 big.mark=",",
                 scientific=FALSE),"Confirmed Deaths")
  })
  output$`Cases Change`<-renderText({ 
    state_change<-us_total%>%
      mutate(Change=cases-lag(cases))%>%
      filter(date==last(date))
    
    paste(format(sum(state_change$Change),
                 big.mark=",",
                 scientific=FALSE),"Daily Confirmed Cases")
  })
  ##Display Total Number for Case/Death Per STATES
  output$`Cases Total County` <- renderText({ 
    state_latest<-mydata%>%
      filter(date==last(date) & state==input$States)
    paste(format(sum(state_latest$cases),
                 big.mark=",",
                 scientific=FALSE),"Confirmed Cases")
  })
  
  output$`Death Total County` <- renderText({ 
    state_latest<-mydata%>%
      filter(date==last(date) & state==input$States)
    paste(format(sum(state_latest$deaths),
                 big.mark=",",
                 scientific=FALSE),"Confirmed Deaths")
  })
  ##Report Cases/Deaths in US
  output$cum_us_report<-renderPlotly(Report_us(mydata,
                                               Cases=input$Outcome,
                                               Change=F))
  output$day_us_report<-renderPlotly(Report_us(mydata,
                                               Cases=input$Outcome,
                                               Change=T))
  # ##Report Cases/Deaths by county
  output$cum_county_report<-renderPlotly(Report_county(mydata,
                                                       State=input$county_trends_case,
                                                       Cases=input$Outcome,
                                                       Change=F))
  output$day_county_report<-renderPlotly(Report_county(mydata,
                                                       State=input$county_trends_case,
                                                       Cases=input$Outcome,
                                                       Change=T))
  
  ##Dispaly state case table
  us_latest1<-data%>%
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
  output$state_table<-renderDT(
    Summary_table(us_latest3,
                  input$Change_state,
                  input$Case_Death_state
    )
  )
  
  
}

shinyApp(ui, server)
