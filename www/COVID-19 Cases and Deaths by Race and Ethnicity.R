##===========COVID-19 Cases and Deaths by Race/Ethnicity======##
##Source: https://data.cdc.gov/api/views/pj7m-y5uh/rows.csv?accessType=DOWNLOAD
##Required Packages
library(dplyr)
library(plotly)
library(data.table)
library(tidyr)

# Data<-fread('https://data.cdc.gov/api/views/pj7m-y5uh/rows.csv?accessType=DOWNLOAD')
# Race_plot(Data,"Arkansas")
Race_plot<-function(Data,Region){
  
  race_latest<-Data%>%
    filter(`Data as of`==max(`Data as of`) & Indicator %in% c('Distribution of COVID-19 deaths (%)', "Count of COVID-19 deaths"))
  
  
  race_latest_us<-race_latest%>%
    filter(State==Region & Indicator=="Count of COVID-19 deaths" )%>%
    select(-c("Data as of",
              'State',
              "Indicator",
              "Footnote"))
  gather<-c("Non-Hispanic White",
            "Non-Hispanic Black or African American",
            "Non-Hispanic American Indian or Alaska Native",
            "Non-Hispanic Asian",
            "Hispanic or Latino",
            "Other")
  race_latest_us<-gather(race_latest_us,Race,Percent,gather)
  fig <- plot_ly(race_latest_us, 
                 x = ~as.numeric(Percent),
                 y = ~Race,
                 type = 'bar',
                 orientation = 'h',
                 hovertemplate = paste('Deaths: %{x}',
                                       '<br>Race: %{y}',
                                       "<extra></extra>"),
                 
                 #showlegend = F,
                 marker = list(color = 'rgb(0,128,0)'))%>%
    layout(title = "All Age Groups" ,
           xaxis = list(title = "Deaths"),
           yaxis = list(title = ""))%>% 
    config(displayModeBar = F)
  
  fig
  
  
}