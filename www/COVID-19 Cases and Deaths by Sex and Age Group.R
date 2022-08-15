###================COVID-19 Cases and Deaths by Sex and Age Group===============##
##Source: https://data.cdc.gov/api/views/9bhg-hcku/rows.csv?accessType=DOWNLOAD
##Required Packages
library(dplyr)
library(data.table)
library(plotly)
#age_sex<-fread("https://data.cdc.gov/api/views/9bhg-hcku/rows.csv?accessType=DOWNLOAD")
# unique(age_sex$`Age group`)
# Age_sex_plot(age_sex,"United States","Under 1 year")

Age_sex_plot<-function(Data,Region,Age){
  age_sex_latest<-Data%>%
    filter(`Data as of`== max(`Data as of`)  & `Age group`==Age & State==Region  )%>%
    filter(!(`Age group` %in% c("All Ages","All ages")) & Sex!="All")
  
  fig <- plot_ly(age_sex_latest, 
                 x = ~as.numeric(`COVID-19 Deaths`),
                 y = ~`Sex`,
                 type = 'bar',
                 orientation = 'h',
                 hovertemplate = paste('Deaths: %{y}',
                                       '<br>Sex: %{x}',
                                       "<extra></extra>"),
                 
                 #showlegend = F,
                 marker = list(color = 'rgb(201, 230, 244)'))%>%
    layout(title = "" ,
           xaxis = list(title = "Deaths"),
           yaxis = list(title = ""))%>% config(displayModeBar = F)
  
  fig
  
  
}  