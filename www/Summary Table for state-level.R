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