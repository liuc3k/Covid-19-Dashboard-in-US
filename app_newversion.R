##Ref:
##https://plotly.com/r/choropleth-maps/
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(plotly)
library(data.table)
library(dplyr)
library(shinycssloaders)
County<-fread('www/County.csv')
State<-fread('www/State.csv')
US<-fread('www/US.csv')
State_change<-fread('www/State_change.csv')
County_change<-fread('www/County_change.csv')
states_code<-fread('www/us-state-ansi-fips.csv')
# US<-fread("www/US.csv")
ui <- navbarPage("Covid-19 Tracking App in US",
                navbarMenu("Interactive Map",
                           tabPanel("State-Level",
                                    sidebarLayout(
                                      sidebarPanel(width =3,
                                        h2(strong("Covid-19 Explorer")),
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
                                        actionButton("usmap_plot", "Exeucte"),
                                        br(),
                                        DTOutput("state_table"),
                                        h5(textOutput("Today_state"))
                                        
                                      ),
                                      
                                      # Show a plot 
                                      mainPanel(width =9,
                                                plotlyOutput("usmap",
                                                             width = "1500px", 
                                                             height = "1500px")%>%withSpinner()
                                      )
                                    )),

                                        
                                        
                                    
                           tabPanel("County-Level",
                                    sidebarLayout(
                                      sidebarPanel(width =3,
                                                   h2(strong("Covid-19 Explorer by County")),
                                                   h3(textOutput("Cases Total County")),
                                                   h3(textOutput("Cases Change County")),
                                                   h4(textOutput("Death Total County")),
                                                   br(),
                                                   
                                                   selectInput('States',
                                                               'States:',
                                                               width = "auto",
                                                               c(unique(as.character(states_code$state)))
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
                                                   actionButton("countymap_plot", "Exeucte"),
                                                   br(),
                                                   DTOutput("county_table"),
                                                   h5(textOutput("Today_county"))
                                                   
                                      ),
                                      
                                      # Show a plot 
                                      mainPanel(width =9,
                                                plotlyOutput("countymap",
                                                             width = "1500px",
                                                             height = "1500px")%>%withSpinner()
                                      )
                                    ))
                                        
                                        
                                    
                           )
                
                  
                  
)

server <- function(input, output, session) {

  ###Display Current date in state-level 
  output$Today_state<-renderText({
    
      paste("Last Updated at",max(State$date))
    
  })
  ###Display Total Number for Case/Death in US
  output$`Cases Total`<-renderText({
    state_latest<-US%>%filter(date==last(date))
      paste(format(sum(state_latest$cases,na.rm =T),
                   big.mark=",",
                   scientific=FALSE),"Confirmed Cases")
    
  })
  output$`Death Total`<-renderText({
    state_latest<-US%>%filter(date==last(date))

      paste(format(sum(state_latest$deaths,na.rm =T),
                   big.mark=",",
                   scientific=FALSE),"Confirmed Deaths")
    
  })
  output$`Cases Change`<-renderText({

      state_change<-US%>%
        mutate(Change=cases-lag(cases))%>%
        filter(date==last(date))

      paste(format(sum(state_change$Change,na.rm =T),
                   big.mark=",",
                   scientific=FALSE),"Daily Confirmed Cases")

  })
  ##Exeucte button for US Map
  a<-reactiveValues(dousmap=F)
  
  observeEvent(input$usmap_plot,{a$dousmap<-input$usmap_plot })
               
                 
  state_data<-reactive({
    if(a$dousmap==F)return()
    isolate({

    if(input$Case_Death_state==T){
      if(input$Change_state==T){
        us_his<-State_change%>%
          filter(date==last(date))
        us_his$hover<-with(us_his, paste("States:",state, "<br>","Cases Change:", change_case, "<br>","Date:", date))
      }
      else{
        us_his<-State%>%
          filter(date==last(date))
        us_his$hover<-with(us_his, paste("States:",state, "<br>","Cases:", cases, "<br>","Date:", date))
      }
      
      
      }
    else{
      if(input$Change_state==T){
        us_his<-State_change%>%filter(date==last(date))
        
        us_his$hover<-with(us_his, paste("States:",state, "<br>","Deaths Change:", change_death, "<br>","Date:", date))}
      else{us_his<-State%>%
        filter(date==last(date))
      us_his$hover<-with(us_his, paste("States:",state, "<br>","Deaths:", deaths, "<br>","Date:", date))}
      
      }
    
    us_his<-left_join(us_his,states_code,by=c('fips','state'))%>%
      filter(!(is.na(code)))
  })
  })
  output$state_table<-renderDT({ 
    if(a$dousmap==F)return()
    isolate({
    if(input$Case_Death_state==T){
      if(input$Change_state==T){datatable(state_data()%>%select(c(state,change_case)) , options = list(dom = 't'))}
      else{datatable(state_data()%>%select(c(state,cases)) , options = list(dom = 't'))}
      
      }
    else{
      if(input$Change_state==T){datatable(state_data()%>%select(c(state,change_death)) , options = list(dom = 't'))}
      else{datatable(state_data()%>%select(c(state,deaths)) , options = list(dom = 't'))}
      
      }
      
                  
    })
  })
    
  
  output$usmap <- renderPlotly({
    if(a$dousmap==F)return()
    isolate({
    # give state boundaries a white border
    l <- list(color = toRGB("white"), width = 1)
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    
    if(input$Case_Death_state==T){
      if(input$Change_state==T){
        fig <- plot_geo(state_data(), 
                        locationmode = 'USA-states')%>% 
          add_trace(
            z = ~change_case, 
            text = ~hover, 
            locations = ~code,
            color = ~change_case, 
            colors = 'Greens')%>% 
          
          colorbar(title = "Daily Cases Change")%>% 
          layout(
            title = paste("Covid-19 Daily Cases Change",'(',format(Sys.time(), "%Y"),')'),
            geo = g
          )%>%
          hide_colorbar()
        fig
        
      }
      else{###Cum Cases 
        fig <- plot_geo(state_data(), 
                        locationmode = 'USA-states')%>%
          add_trace(
            z = ~cases, 
            text = ~hover, 
            locations = ~code,
            color = ~cases, 
            colors = 'Blues')%>% 
          
          colorbar(title = "Cases")%>% 
          layout(
            title = paste("Covid-19 Cases",'(',format(Sys.time(), "%Y"),')'),
            geo = g)%>%
          hide_colorbar()
        fig}
      
      }
    else{
      if(input$Change_state==T){
        ###Cum Deaths
        fig <- plot_geo(state_data(), 
                        locationmode = 'USA-states')%>%
          add_trace(
            z = ~change_death, 
            text = ~hover, 
            locations = ~code,
            color = ~change_death, 
            colors = 'Oranges')%>% 
          
          colorbar(title = "Daily Deaths")%>% 
          layout(
            title = paste("Covid-19 Daily Deaths",'(',format(Sys.time(), "%Y"),')'),
            geo = g)%>%
          hide_colorbar()
        fig
      }
      else{###Cum Deaths
        fig <- plot_geo(state_data(), 
                        locationmode = 'USA-states')%>%
          add_trace(
            z = ~deaths, 
            text = ~hover, 
            locations = ~code,
            color = ~deaths, 
            colors = 'Reds')%>% 
          
          colorbar(title = "Deaths")%>% 
          layout(
            title = paste("Covid-19 Deaths",'(',format(Sys.time(), "%Y"),')'),
            geo = g)%>%
          hide_colorbar()
        fig}
      
    }
      
 
  })
  })
  
  ##Exeucte button for County Map
  b<-reactiveValues(docountymap=F)
  observeEvent(input$countymap_plot,{b$docountymap<-input$countymap_plot })
  county_code<-reactive({
    county_code <- map_data("county") %>%
      filter(region ==tolower(input$States))
  })
  county_data<-reactive({
    if(b$docountymap==F)return()
    isolate({
      if(input$Case_Death_county==T){
        if(input$Change_county==T){
          county_his<-County_change%>%
            filter(date==last(date),
                   state ==input$States)
          county_his$county <- tolower(county_his$county) # matching string
          # county_his$hover<-with(county_his, paste("States:",state, "<br>","Cases Change:", change_case, "<br>","Date:", date))
        }
        else{
          county_his<-County_change%>%
            filter(date==last(date),
                   state ==input$States)
          county_his$county <- tolower(county_his$county) # matching string
          
          # county_his$hover<-with(county_his, paste("States:",state, "<br>","Cases:", cases, "<br>","Date:", date))
        }
        
        
      }
      else{
        if(input$Change_county==T){
          county_his<-County_change%>%filter(date==last(date),
                                             state ==input$States)
          county_his$county <- tolower(county_his$county) # matching string
          
          # county_his$hover<-with(county_his, paste("States:",state, "<br>","Deaths Change:", change_death, "<br>","Date:", date))
          }
        else{
          county_his<-County_change%>%
          filter(date==last(date),
                 state ==input$States)
          county_his$county <- tolower(county_his$county) # matching string
          
          # county_his$hover<-with(county_his, paste("States:",state, "<br>","Deaths:", deaths, "<br>","Date:", date))
          }
        
      }
      
      df <- merge(county_code(), county_his, by.x = "subregion", by.y = "county")
  })
  })
  output$county_table<-renderDT({ 
    if(b$docountymap==F)return()
    isolate({
      if(input$Case_Death_county==T){
        if(input$Change_county==T){datatable(county_data()%>%
                                               select(c(subregion,change_case))%>%
                                               group_by(subregion)%>%
                                               distinct()
                                             , options = list(dom = 't'))}
        else{datatable(county_data()%>%
                         select(c(subregion,cases))%>%
                         group_by(subregion)%>%
                         distinct() , options = list(dom = 't'))}
        
      }
      else{
        if(input$Change_county==T){datatable(county_data()%>%select(c(subregion,change_death))%>%
                                               group_by(subregion)%>%
                                               distinct() , options = list(dom = 't'))}
        else{datatable(county_data()%>%select(c(subregion,deaths))%>%
                         group_by(subregion)%>%
                         distinct() , options = list(dom = 't'))}
        
      }
      
      
    })
  })

output$countymap <- renderPlotly({
  if(b$docountymap==F)return()
  isolate({
    # specify some map projection/options
    geo <- list(
      scope = 'usa',
      showland = TRUE,
      landcolor = toRGB("gray95"),
      countrycolor = toRGB("gray80")
    )
    
    
    if(input$Case_Death_county==T){
      if(input$Change_county==T){
        p <- county_data() %>%
          group_by(group) %>%
          plot_geo(
            x = ~long, y = ~lat, color = ~change_case, colors = c('#ffeda0','#f03b20'),
            text = ~paste(county_data()$subregion, "<br />", county_data()$change_case), hoverinfo = 'text') %>%
          add_polygons(line = list(width = 0.4)) %>%
          add_polygons(
            fillcolor = 'transparent',
            line = list(color = 'black', width = 0.5),
            showlegend = FALSE
          ) %>%
          layout(
            title = paste("Covid-19 Cases","in",input$States,"by County"),
            geo = geo)%>%
          hide_colorbar()
        
        p
        
      }
      else{###Cum Cases 
        }
      
    }
    else{
      if(input$Change_county==T){
        ###Cum Deaths
        
      }
      else{###Cum Deaths
        }
      
    }
    
    
  })
})



}
##################################
##### Call shinyApp function #####
##################################
shinyApp(ui = ui, server = server)
