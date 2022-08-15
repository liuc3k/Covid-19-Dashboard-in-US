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