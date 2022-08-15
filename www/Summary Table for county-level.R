###=============================Summary Table for county-level=======##
##Required Packages:
library(DT)
###Example:
# population<-fread('https://raw.githubusercontent.com/COVID19Tracking/associated-data/master/us_census_data/us_census_2018_population_estimates_counties.csv')
# population<-population%>%
#   select("state_name","county","population")
# names(population)[names(population) == "state_name"] <- "state"
# covid_county<-fread('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv')
# Final<-left_join(covid_county,population,by=c('state','county'))
# county_latest<-Final%>%
#   filter(state=='Connecticut' & county!="Unknown")%>%
#   group_by(date,county)
# county_latest1<-county_latest%>%
#   group_by(county) %>%
#   mutate(lag.case = lag(cases, n = 1, default = NA),
#          lag.death= lag(deaths,n=1, default = NA),
#          case_rate=(cases/population)*100000,
#          death_rate=(deaths/population)*100000,
#          change_case=(cases-lag.case),
#          change_death=(deaths-lag.death),
#          change_case_rate=(cases-lag.case)/lag.case,
#          change_death_rate=(deaths-lag.death)/lag.death)
# county_latest2<-county_latest1%>%
#   group_by(county)%>%
#   filter(date==last(date))
# states <- geojson_read("https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", 
#                        what = "sp")
# shape <- counties(state = 'Connecticut',cb=T,resolution = '20m')  
# county_latest3<-subset(county_latest2,is.element(county_latest2$county,shape$NAME))
# county_latest3<-county_latest3[order(match(county_latest3$county,
#                                            shape$NAME)),]
# # Summary_table_county(county_latest3,F,F)

Summary_table_county<-function(data,change,case){
  if(change){
    if(case){
      DT<-datatable(data%>%
                      select('county',"change_case")%>%
                      arrange(desc(change_case)),
                    rownames = FALSE,
                    colnames = c('County'='county',
                                 "Changing Cases"="change_case"), 
                    options = list(dom = 'tp'))
      DT}
    else{
      DT<-datatable(data%>%
                      select('county',"change_death")%>%
                      arrange(desc(change_death)),
                    rownames = FALSE,
                    colnames = c('County'='county',
                                 "Changing Deaths"="change_death"), 
                    options = list(dom = 'tp'))
      DT
    }
  }
  else{
    if(case){
      DT<-datatable(data%>%
                      select('county',"cases")%>%
                      arrange(desc(cases)),
                    rownames = FALSE,
                    colnames = c('County'='county',
                                 "Cases"="cases"), 
                    options = list(dom = 'tp'))
      DT}
    else{
      DT<-datatable(data%>%
                      select('county',"deaths")%>%
                      arrange(desc(deaths)),
                    rownames = FALSE,
                    colnames = c('County'='county',
                                 "Deaths"="deaths"), 
                    options = list(dom = 'tp'))
      DT
    }
  }
  
}