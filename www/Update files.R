##Update files
County<-fread('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-2022.csv')%>%
  filter(date%in%c(last(date),last(date)-1))
State<-fread('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv')%>%
  filter(date%in%c(last(date),last(date)-1))
US<-fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv")%>%
  filter(date%in%c(last(date),last(date)-1))
State_change<-fread('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv')%>%
  filter(date%in%c(last(date),last(date)-1))%>%
  arrange(state,date)%>%
  group_by(state) %>%
  mutate(lag.case = lag(cases, n = 1, default = NA),
         lag.death= lag(deaths,n=1, default = NA),
         change_case=(cases-lag.case),
         change_death=(deaths-lag.death))%>%
  filter(date==last(date))
County_change<-fread('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-2022.csv')%>%
  filter(date%in%c(last(date),last(date)-1))%>%
  arrange(county,date)%>%
  group_by(state) %>%
  mutate(lag.case = lag(cases, n = 1, default = NA),
         lag.death= lag(deaths,n=1, default = NA),
         change_case=(cases-lag.case),
         change_death=(deaths-lag.death))%>%
  filter(date==last(date))
fwrite(County,'C:/Users/lokar/Desktop/covid19dashboard/www/County.csv')
fwrite(State,'C:/Users/lokar/Desktop/covid19dashboard/www/State.csv')
fwrite(US,'C:/Users/lokar/Desktop/covid19dashboard/www/US.csv')
fwrite(State_change,'C:/Users/lokar/Desktop/covid19dashboard/www/State_change.csv')
fwrite(County_change,'C:/Users/lokar/Desktop/covid19dashboard/www/County_change.csv')
