library(rvest)
library(magrittr)
source('wd.R')
setwd(wd)



# Spot names function ---------

source('spot_names.R')

# end month

end_month = function(string){
  if(grepl("Jan", x = string, fixed = T)==1){
    return_number = 1
  }
  if(grepl("Feb", x = string, fixed = T)==1){
    return_number = 2
  }
  if(grepl("Mar", x = string, fixed = T)==1){
    return_number = 3
  }
  if(grepl("Apr", x = string, fixed = T)==1){
    return_number = 4
  }
  if(grepl("May", x = string, fixed = T)==1){
    return_number = 5
  }
  if(grepl("Jun", x = string, fixed = T)==1){
    return_number = 6
  }
  if(grepl("Jul", x = string, fixed = T)==1){
    return_number = 7
  }
  if(grepl("Aug", x = string, fixed = T)==1){
    return_number = 8
  }
  if(grepl("Sep", x = string, fixed = T)==1){
    return_number = 9
  }
  if(grepl("Oct", x = string, fixed = T)==1){
    return_number = 10
  }
  if(grepl("Nov", x = string, fixed = T)==1){
    return_number = 11
  }
  if(grepl("Dec", x = string, fixed = T)==1){
    return_number = 12
  }
  return(return_number)
}


# Mens --------------


year = as.numeric(format(as.Date(Sys.time(), format="%d/%m/%Y"),"%Y"))

month = as.numeric(format(as.Date(Sys.time(), format="%d/%m/%Y"),"%m"))

day = as.numeric(format(as.Date(Sys.time(), format="%d/%m/%Y"),"%d"))

event_url = paste('http://www.worldsurfleague.com/events/',paste(year,'/mct', sep = ''),sep = '')

events = read_html(event_url)

events2 = html_nodes(events,'.tour-event-range')

events3 = html_text(events2) 

events4 = c()

for(i in 1:length(events3)){
  if(grepl("Complete",x = events3[i], fixed = T)==1){
    events4[i] = 1
  }
  if(grepl("Upcoming",x = events3[i], fixed = T)==1){
    events4[i] = 0 
  }
  if(grepl("Standby",x = events3[i], fixed = T)==1){
    events4[i] = 0
  }
}

events4 = as.numeric(events4)

finished_index = which.max(which(events4==1))

spots = html_nodes(events,'.tour-event-name')

spots = html_text(spots) 

finished = spot_names(spots)[finished_index]

finished_date = events3[finished_index]

finished_month = end_month(finished_date)

finished_day = unique(na.omit(as.numeric(unlist(strsplit(unlist(finished_date), "[^0-9]+")))))[2]

mens_analyse_time = 0

if(finished_month == month & finished_day + 10 >= day  & finished_day < day){
  mens_analyse_time = 1
}




