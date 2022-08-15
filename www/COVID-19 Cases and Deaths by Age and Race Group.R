###================COVID-19 Cases and Deaths by Age and Race Group===============##
##Source: https://data.ct.gov/Health-and-Human-Services/COVID-19-Cases-and-Deaths-by-Age-Group/ypz6-8qyf
##Required Packages
library(dplyr)
library(data.table)
library(plotly)
#age_race<-fread("https://data.cdc.gov/api/views/ks3g-spdg/rows.csv?accessType=DOWNLOAD")

# unique(age_race$`Age group`)

# Age_race_plot(age_race,"United States","All Ages")

Age_race_plot<-function(Data,Region,Age){
  age_race_latest<-Data%>%
    filter(`Data as of`== max(`Data as of`) & `Race and Hispanic Origin Group`!="Total Deaths" & `Age group`==Age & State==Region)
  
  fig <- plot_ly(age_race_latest, 
                 x = ~as.numeric(`COVID-19 Deaths`),
                 y = ~`Race and Hispanic Origin Group`,
                 type = 'bar',
                 hovertemplate = paste('Deaths: %{x}',
                                       '<br>Race: %{y}',
                                       "<extra></extra>"),
                 # width = 1000, 
                 # height = 1000,
                 orientation = 'h',
                 #showlegend = F,
                 marker = list(color = 'rgb(134, 154, 244)'))%>%
    layout(title = "By Age Group" ,
           xaxis = list(title = "Deaths"),
           yaxis = list(title = ""))%>% config(displayModeBar = F)
  
  fig
  
  
}