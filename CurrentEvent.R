library(rvest)
remove(list=ls(all=TRUE))

# Spot neames function ---------

spot_names = function(vec){
  for(i in 1:length(vec)){
    if(grepl("Gold Coast",x = vec[i], fixed = T)==1){
      vec[i] = "GoldCoast"
    }
    if(grepl("Margaret River",x = vec[i], fixed = T)==1){
      vec[i] = "MargaretRiver"
    }
    if(grepl("Bells Beach",x = vec[i], fixed = T)==1){
      vec[i] = "BellsBeach"
    }
    if(grepl("Rio",x = vec[i], fixed = T)==1){
      vec[i] = "Rio"
    }
    if(grepl("Fiji",x = vec[i], fixed = T)==1){
      vec[i] = "Fiji"
    }
    if(grepl("J-Bay",x = vec[i], fixed = T)==1){
      vec[i] = "Jbay"
    }
    if(grepl("Tahiti",x = vec[i], fixed = T)==1){
      vec[i] = "Tahiti"
    }
    if(grepl("Teahupoo",x = vec[i], fixed = T)==1){
      vec[i] = "Tahiti"
    }
    if(grepl("France",x = vec[i], fixed = T)==1){
      vec[i] = "France"
    }
    if(grepl("Portugal",x = vec[i], fixed = T)==1){
      vec[i] = "Portugal"
    }
    if(grepl("Pipe",x = vec[i], fixed = T)==1){
      vec[i] = "Pipeline"
    }
    if(grepl("Bali",x = vec[i], fixed = T)==1){
      vec[i] = "Bali"
    }
    if(grepl("Trestles",x = vec[i], fixed = T)==1){
      vec[i] = "Trestles"
    }
    if(grepl("Catarina",x = vec[i], fixed = T)==1){
      vec[i] = "SantaCatarina"
    }
    if(grepl("New York",x = vec[i], fixed = T)==1){
      vec[i] = "NewYork"
    }
    if(grepl("San Fran",x = vec[i], fixed = T)==1){
      vec[i] = "SanFrancisco"
    }
    if(grepl("Cruz",x = vec[i], fixed = T)==1){
      vec[i] = "SantaCruz"
    }
    if(grepl("US Open",x = vec[i], fixed = T)==1){
      vec[i] = "USOpen"
    }
    if(grepl("Cascais",x = vec[i], fixed = T)==1){
      vec[i] = "Cascais"
    }
    if(grepl("Maui",x = vec[i], fixed = T)==1){
      vec[i] = "Maui"
    }
    if(grepl("Swatch",x = vec[i], fixed = T)==1){
      vec[i] = "Trestles"
    }
    if(grepl("NZ",x = vec[i], fixed = T)==1){
      vec[i] = "NewZealand"
    }
    if(grepl("Beachley",x = vec[i], fixed = T)==1){
      vec[i] = "Beachley"
    }
    if(grepl("Peru",x = vec[i], fixed = T)==1){
      vec[i] = "Peru"
    }
    if(grepl("World Cup",x = vec[i], fixed = T)==1){
      vec[i] = "Oahu"
    }
  }
  return(vec)
} 



# Mens --------------

year = 2017

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

