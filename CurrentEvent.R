library(rvest)
remove(list=ls(all=TRUE))

# Spot names function ---------

source('spot_names.R')


# Mens --------------

year = as.numeric(format(as.Date(Sys.time(), format="%d/%m/%Y"),"%Y"))

event_url = paste('http://www.worldsurfleague.com/events/',paste(year,'/mct', sep = ''),sep = '')

events = read_html(event_url)

events2 = html_nodes(events,'.tour-event-range')

events3 = html_text(events2) 

for(i in 1:length(events3)){
  if(grepl("Complete",x = events3[i], fixed = T)==1){
    events3[i] = 0
  }
  if(grepl("Upcoming",x = events3[i], fixed = T)==1){
    events3[i] = 1 
  }
  if(grepl("Standby",x = events3[i], fixed = T)==1){
    events3[i] = 0 
  }
}

events3 = as.numeric(events3)

upcoming_index = which(events3==1)[1]

names = events2 = html_nodes(events,'.tour-event-name')

names = html_text(names) 

upcoming = spot_names(names)[upcoming_index]


# Mens --------------

year = as.numeric(format(as.Date(Sys.time(), format="%d/%m/%Y"),"%Y"))

event_url = paste('http://www.worldsurfleague.com/events/',paste(year,'/mct', sep = ''),sep = '')

events = read_html(event_url)

events2 = html_nodes(events,'.tour-event-range')

events3 = html_text(events2) 

for(i in 1:length(events3)){
  if(grepl("Complete",x = events3[i], fixed = T)==1){
    events3[i] = 0
  }
  if(grepl("Upcoming",x = events3[i], fixed = T)==1){
    events3[i] = 1 
  }
}

events3 = as.numeric(events3)

upcoming_index = which(events3==1)[1]

names = events2 = html_nodes(events,'.tour-event-name')

names = html_text(names) 

upcoming_mens = spot_names(names)[upcoming_index]


# Womens --------------

year = as.numeric(format(as.Date(Sys.time(), format="%d/%m/%Y"),"%Y"))

event_url = paste('http://www.worldsurfleague.com/events/',paste(year,'/wct', sep = ''),sep = '')

events = read_html(event_url)

events2 = html_nodes(events,'.tour-event-range')

events3 = html_text(events2) 

for(i in 1:length(events3)){
  if(grepl("Complete",x = events3[i], fixed = T)==1){
    events3[i] = 0
  }
  if(grepl("Upcoming",x = events3[i], fixed = T)==1){
    events3[i] = 1 
  }
  if(grepl("Standby",x = events3[i], fixed = T)==1){
    events3[i] = 0 
  }
}

events3 = as.numeric(events3)

upcoming_index = which(events3==1)[1]

names = events2 = html_nodes(events,'.tour-event-name')

names = html_text(names) 

upcoming_womens = spot_names(names)[upcoming_index]

