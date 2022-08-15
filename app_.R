##=============Covid-19 Tracking App(Version 2)============================##
##Reference:
# http://shinyapps.dreamrs.fr/shinyWidgets/
##working directory

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
library(leaflet)
Sys.setlocale("LC_TIME", "C")
source('CasesDeath Report.R')
source('US Map with Changing CaseDeaths.R')
source('US Counties Map with CaseDeaths.R')
source('Summary Table for state-level.R')
source('Summary Table for county-level.R')
source('COVID-19 Cases and Deaths by Age and Race Group.R')
source('COVID-19 Cases and Deaths by Sex and Age Group.R')
source('COVID-19 Cases and Deaths by Race and Ethnicity.R')
####Global R Object
##Read Data()
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
                                                     # br(),
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
                                                     
                                                     h5(textOutput("Today_state")),
                                                     actionButton("usmap_plot", "An action button")
                                                     
                                                     
                                                     
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
                                                     h3(textOutput("Cases Change County")),
                                                     h4(textOutput("Death Total County")),
                                                     
                                                     br(),
                                                     
                                                     htmlOutput('States_select'),
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
                                                     conditionalPanel(condition = "input.Case_Death_county==T",
                                                                      
                                                                      DTOutput("county_table")),
                                                     h5(textOutput("Today_county")),
                                                     actionButton("countymap_plot", "An action button")
                                                     
                                                     
                                                     
                                       )
                                       
                                       
                                   )
                          )
               ),
               tabPanel("Trends",
                        h2("Trends in Number of COVID-19 Cases/Deaths in the US by State"),
                        hr(),
                        sidebarLayout(
                          sidebarPanel(
                            width = 4,
                            
                            htmlOutput('Level_select'),
                      
                          
                                             
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
                                       plotlyOutput("cum_report")

                                       ),
                                       
                           
                              
                              tabPanel("New",
                                       plotlyOutput("day_report")

                                       )
                            )
                            
                          )
                        )),
               
               
               tabPanel("Demographic Characteristics",
                        h2("Demographic Trends of COVID-19 Deaths in the US"),
                        hr(),
                        div(h3("Deaths involving coronavirus disease 2019 (COVID-19) by race and Hispanic origin group and age (United States)"),align="center"),
                        fluidRow(
                          column(11),
                          column(1,
                            align = "right",
                            div(
                            
                            selectInput("Age1",
                                        "",
                                        width = "200",
                                        c("Under 1 year",
                                          "1-4 years",
                                          "5-14 years",
                                          "15-24 years",
                                          "25-34 years",
                                          "35-44 years",
                                          "45-54 years",
                                          "55-64 years",
                                          "65-74 years",
                                          "75-84 years",
                                          "85 years and over")
                                        
                            ),
                            style="text-align:left;"))
                          ),
                        splitLayout(div(plotlyOutput("Age_race_us",height = "400",width = "100%"),align="center"),
                                    div(plotlyOutput("Age_race",height = "400",width = "100%"),align="center")),
                        

                        div(h3("Deaths involving coronavirus disease 2019 (COVID-19) by race and Hispanic origin group and age (State Level)"),align="center"),
                        fluidRow(
                        column(11),
                        column(1,
                               align='right',
                               div( style="text-align:left;",
                                    
                                    htmlOutput('age_race_stat'),
                               selectInput("Age2",
                                           "Age Group:",
                                           width = "200",
                                           c("Under 1 year",
                                             "1-4 years",
                                             "5-14 years",
                                             "15-24 years",
                                             "25-34 years",
                                             "35-44 years",
                                             "45-54 years",
                                             "55-64 years",
                                             "65-74 years",
                                             "75-84 years",
                                             "85 years and over")
                                                       )
                        ))

                        ),
                        
                        splitLayout(div(plotlyOutput("Age_race_state",height = "400",width = "100%"),align="center"),
                                    div(plotlyOutput("Age_race2",height = "400",width = "100%"),align="center")),
                        ),
               
               tabPanel("Data",
                        h2("Sources:"),
                        hr(),
                        selectInput('Tables',
                                    '',
                                    width = "200",
                                    c('U.S. National-Level Data'="U.S. National-Level Data",
                                      'State-Level Data'="State-Level Data",
                                      "County-Level Data"="County-Level Data")),
                        DTOutput("Data_table"))
)

#####SERVER======
server <- function(input, output) {
  County<-reactive({fread('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-2022.csv')})
  State<-reactive({fread('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv')})
  US<-reactive({fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv")})
  ###COVID_USmap_data Data
  ##Exeucte button
  a<-reactiveValues(dousmap=F)
  
  observeEvent(input$usmap_plot,
               {
                 a$dousmap<-input$usmap_plot
               })
  COVID_USmap_data<-reactive({
    if(a$dousmap==F)return()
    isolate({
    population_state<-fread("https://raw.githubusercontent.com/COVID19Tracking/associated-data/master/us_census_data/us_census_2018_population_estimates_states.csv")
    population_state<-population_state%>%
      select("state_name","population")
    names(population_state)[names(population_state) == "state_name"] <- "state"
    names(population_state)[names(population_state) == "population"] <- "Population"
    COVID_USmap_data<-left_join(State(),
                                population_state,
                                by="state")
  })
  })
  ###COVID_Countymap_data
  ##Exeucte button
  b<-reactiveValues(docountymap=F)
  
  observeEvent(input$countymap_plot,
               {
                 b$docountymap<-input$countymap_plot
               })
  
  COVID_Countymap_data<-reactive({
    if(b$docountymap==F)return()
    isolate({
      population<-fread('https://raw.githubusercontent.com/COVID19Tracking/associated-data/master/us_census_data/us_census_2018_population_estimates_counties.csv')
      population<-population%>%
        select("state_name","county","population")
      names(population)[names(population) == "state_name"] <- "state"
      
      COVID_Countymap_data<-left_join(County(),
                                      population,
                                      by=c('state','county'))
    })

  })

  ###Covid DM data by state and county level  
  age_race<-reactive({fread("https://data.cdc.gov/api/views/ks3g-spdg/rows.csv?accessType=DOWNLOAD")})
  age_sex<-reactive({fread("https://data.cdc.gov/api/views/9bhg-hcku/rows.csv?accessType=DOWNLOAD")})
  Race<-reactive({fread('https://data.cdc.gov/api/views/pj7m-y5uh/rows.csv?accessType=DOWNLOAD')})
  output$States_select<-renderUI({
    selectInput('States',
                'States:',
                width = "auto",
                c(unique(as.character(counties$region)))
    )
  })
  output$Level_select<-renderUI({
    selectInput('Level',
                'State/Regions:',
                width = "150",
                c(`United States`='US',
                  c(unique(as.character(counties$region)))))
  })
  ###Display US map
  output$us_leaf <- renderLeaflet({
    if(a$dousmap==F)return()
    isolate({
    COVID_USmap(COVID_USmap_data(),
                input$Case_Death_state,
                input$Change_state)
    
  })
  })
  ###Display county map
  
  
  output$county_leaf<-renderLeaflet(
    {if(b$docountymap==F)return()
      isolate({
      COVID_Countymap(COVID_Countymap_data(),
                      input$States,
                      input$Case_Death_county,
                      input$Change_county)
      }
      )
    }
  )
  ###Display Current date in state-level 
  output$Today_state<-renderText({
    if(a$dousmap==F)return()
    isolate({
    paste("Last Updated at",max(State()$date))
    })
  })
  ###Display Current date in county-level
  output$Today_county<-renderText({
    if(b$docountymap==F)return()
    isolate({
    paste("Last Updated at",max(State()$date))
    })})
  
  ###Display Total Number for Case/Death in US
  output$`Cases Total`<-renderText({
    if(a$dousmap==F)return()
    isolate({
    state_latest<-County()%>%
      filter(date==last(date))
    
    paste(format(sum(state_latest$cases,na.rm =T),
                 big.mark=",",
                 scientific=FALSE),"Confirmed Cases")
  })})
  output$`Death Total`<-renderText({
    if(a$dousmap==F)return()
    isolate({
    state_latest<-County()%>%
      filter(date==last(date))
    
    paste(format(sum(state_latest$deaths,na.rm =T),
                 big.mark=",",
                 scientific=FALSE),"Confirmed Deaths")
  }) })
  output$`Cases Change`<-renderText({
    if(a$dousmap==F)return()
    isolate({
    state_change<-US()%>%
      mutate(Change=cases-lag(cases))%>%
      filter(date==last(date))
    
    paste(format(sum(state_change$Change,na.rm =T),
                 big.mark=",",
                 scientific=FALSE),"Daily Confirmed Cases")
  }) })
  output$`Cases Change County`<-renderText({
    if(b$docountymap==F)return()
    isolate({
    county_change<-Summary_county()%>%
      
      filter(state==input$States)
    
    paste(format(sum(county_change$change_case,na.rm =T),
                 big.mark=",",
                 scientific=FALSE),"Daily Confirmed Cases")
  })})
  ##Display Total Number for Case/Death Per STATES
  output$`Cases Total County` <- renderText({
    if(b$docountymap==F)return()
    isolate({
    state_latest<-County()%>%
      filter(date==last(date) & state==input$States)
    paste(format(sum(state_latest$cases,na.rm =T),
                 big.mark=",",
                 scientific=FALSE),"Confirmed Cases")
  })
  })
  output$`Death Total County` <- renderText({
    if(b$docountymap==F)return()
    isolate({
    state_latest<-County()%>%
      filter(date==last(date) & state==input$States)
    paste(format(sum(state_latest$deaths,na.rm =T),
                 big.mark=",",
                 scientific=FALSE),"Confirmed Deaths")
  }) })
  
  ###Report Cases/Deaths(Cumulative)
  output$cum_report<-renderPlotly(
    if(input$Level!='US'){    
      Report_county(County(),
                    State=input$Level,
                    Cases=input$Outcome,
                    Change=F)}
    else{
      Report_us(County(),
                Cases=input$Outcome,
                Change=F)
    }
    
  )
  ###Report Cases/Deaths(Daily)
  output$day_report<-renderPlotly(
    if(input$Level!='US'){    
      Report_county(County(),
                    State=input$Level,
                    Cases=input$Outcome,
                    Change=T)}
    else{
      Report_us(County(),
                Cases=input$Outcome,
                Change=T)
    }
    
  )
  ##Dispaly state case/death table
  Summary_state<-reactive({
    if(a$dousmap==F)return()
    isolate({
    us_latest1<-COVID_USmap_data()%>%
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
    
  }) })
  
  
  
  output$state_table<-renderDT({    
    
    if(a$dousmap==F)return()
    isolate({
    Summary_table(Summary_state(),
                  input$Change_state,
                  input$Case_Death_state)
    }
    
    )
    }

  )
  ###Dispaly county_table
  Summary_county <- reactive({
    if(b$docountymap==F)return()
    isolate({
    population<-fread('https://raw.githubusercontent.com/COVID19Tracking/associated-data/master/us_census_data/us_census_2018_population_estimates_counties.csv')
    population<-population%>%
      select("state_name","county","population")
    names(population)[names(population) == "state_name"] <- "state"
    covid_county<-fread('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-2022.csv')
    Final<-left_join(covid_county,population,by=c('state','county'))
    county_latest<-Final%>%
      filter(state==input$States & county!="Unknown")%>%
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
    states <- geojson_read("https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", 
                           what = "sp")
    shape <- counties(state = input$States,cb=T,resolution = '20m')  
    county_latest3<-subset(county_latest2,is.element(county_latest2$county,shape$NAME))
    county_latest3<-county_latest3[order(match(county_latest3$county,
                                               shape$NAME)),]
  })
  })
  output$county_table<-renderDT({
    if(b$docountymap==F)return()
    isolate({
    Summary_table_county( Summary_county() ,input$Change_county,input$Case_Death_county)
  })
  })
  output$Data_table<-renderDT(
    
    
    if(input$Tables=='U.S. National-Level Data'){
      datatable(US(), 
                class = "compact",
                options = list(
                  pageLength = 25, 
                  autoWidth = TRUE
                ),
                colnames = c('Date', 'Cases',"Deaths"))
    }
    else if (input$Tables=="State-Level Data"){
      datatable(State(), 
                class = "compact",
                options = list(
                  pageLength = 25, 
                  autoWidth = TRUE
                ),
                colnames = c('Date', 'State', 'FIPS', 'Cases',"Deaths"))
    }
    else{datatable(County(), 
                   class = "compact",
                   options = list(
                     pageLength = 25, 
                     autoWidth = TRUE
                   ),
                   colnames = c('Date', 'County', 'State', 'FIPS', 'Cases',"Deaths"))}
  )
  
  output$Age_race<-renderPlotly(
    Age_race_plot(age_race(),"United States",input$Age1)
  )
  
  age_race_us<-reactive({
    age_race()%>%
      filter(`Data as of`== max(`Data as of`) & `Race and Hispanic Origin Group`!="Total Deaths" & `Age group`=="All Ages" & State=="United States")
  })  
  
  output$Age_race_us<-renderPlotly(

    
    fig <- plot_ly(age_race_us(), 
                   x = ~as.numeric(`COVID-19 Deaths`),
                   y = ~`Race and Hispanic Origin Group`,
                   type = 'bar',
                   hovertemplate = paste('Deaths: %{x}',
                                         '<br>Race: %{y}',
                                         "<extra></extra>"),
                   
                   orientation = 'h',
                   
                   marker = list(color = 'rgb(0,128,0)'))%>%
      layout(title = "All Age Group" ,
             xaxis = list(title = "Deaths"),
             yaxis = list(title = ""))%>% 
      config(displayModeBar = F)
    
   
  )
  output$age_race_stat<-renderUI({    selectInput("Age_race_stat",
                                                  "States:",
                                                  width = "200",
                                                  c(unique(age_race()$State)))})

  output$Age_race2<-renderPlotly(
    Age_race_plot(age_race(),input$Age_race_stat,input$Age2)
  )
  
  output$Age_race_state<-renderPlotly(
    Race_plot(Race(),input$Age_race_stat)
    
  )
  # output$Age_sex<-renderPlotly(
  #   Age_sex_plot(age_sex,input$Level22,input$Age2)
  # )
}
shinyApp(ui, server)



