library(rvest)
library(magrittr)
remove(list=ls(all=TRUE))
wd = getwd()



# Spot names function ---------

source('spot_names.R')

# which month function -------

which_month = function(string){
  if(grepl("Dec", x = string, fixed = T)==1){
    return_number = 12
  }
  if(grepl("Nov", x = string, fixed = T)==1){
    return_number = 11
  }
  if(grepl("Oct", x = string, fixed = T)==1){
    return_number = 10
  }
  if(grepl("Sep", x = string, fixed = T)==1){
    return_number = 9
  }
  if(grepl("Aug", x = string, fixed = T)==1){
    return_number = 8
  }
  if(grepl("Jul", x = string, fixed = T)==1){
    return_number = 7
  }
  if(grepl("Jun", x = string, fixed = T)==1){
    return_number = 6
  }
  if(grepl("May", x = string, fixed = T)==1){
    return_number = 5
  }
  if(grepl("Apr", x = string, fixed = T)==1){
    return_number = 4
  }
  if(grepl("Mar", x = string, fixed = T)==1){
    return_number = 3
  }
  if(grepl("Feb", x = string, fixed = T)==1){
    return_number = 2
  }
  if(grepl("Jan", x = string, fixed = T)==1){
    return_number = 1
  }
  return(return_number)
}



# Mens --------------
setwd(paste(wd,'/Mens',sep=''))

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
    events4[i] = 0
  }
  if(grepl("Upcoming",x = events3[i], fixed = T)==1){
    events4[i] = 1 
  }
  if(grepl("Standby",x = events3[i], fixed = T)==1){
    events4[i] = 0 
  }
}

events4 = as.numeric(events4)

upcoming_index = which(events4==1)[1]

spots = html_nodes(events,'.tour-event-name')

spots = html_text(spots) 

upcoming_mens = spot_names(spots)[upcoming_index]

upcoming_date = events3[upcoming_index]

start_month = which_month(upcoming_date)

start_day = unique(na.omit(as.numeric(unlist(strsplit(unlist(upcoming_date), "[^0-9]+")))))[1]

mens_go_time = 0

if(start_month == month & start_day - 1 == day){
  mens_go_time = 1
}

if(mens_go_time == 1){
  # competitors list ------
  
  names2010 = read.csv('whole2010.csv',stringsAsFactors = F)[,1]
  names2011 = read.csv('whole2011.csv',stringsAsFactors = F)[,1]
  names2012 = read.csv('whole2012.csv',stringsAsFactors = F)[,1]
  names2013 = read.csv('whole2013.csv',stringsAsFactors = F)[,1]
  names2014 = read.csv('whole2014.csv',stringsAsFactors = F)[,1]
  names2015 = read.csv('whole2015.csv',stringsAsFactors = F)[,1]
  names2016 = read.csv('whole2016.csv',stringsAsFactors = F)[,1]
  names2017 = read.csv('whole2017.csv',stringsAsFactors = F)[,1]
  
  names = unique(c(names2010,names2011,names2012,names2013,names2014,names2015,names2016,names2017))
  names2 = matrix(nrow = length(names),ncol=2)
  for(i in 1:length(names)){
    names2[i,1] = head(strsplit(names[i],split="")[[1]],1)
    names2[i,2] = tail(strsplit(names[i],split=" ")[[1]],1)
  }
  
  # convert to full name -----------
  
  convert = function(string){
    first_initial = head(strsplit(string,split="")[[1]],1)
    surname = tail(strsplit(string,split=" ")[[1]],1)
    if(first_initial %in% names2[,1] & surname %in% names2[,2]){
      fi_indexes = which(names2[,1] == first_initial)
      surname_indexes = which(names2[,2] == surname)
      if(sum(fi_indexes %in% surname_indexes)){
        return(names[fi_indexes[fi_indexes %in% surname_indexes]])
      }
      else(return('DELETE'))
    }
    else(return('DELETE'))
  }
  
  
  s = html_session(event_url)
  
  s = s %>% follow_link(spots[upcoming_index]) %>% follow_link("Results")
  
  comp = html_nodes(s,'.athlete-name')
  
  comp = html_text(comp)
  
  complete = c()
  for(i in 1:length(comp)){
    complete[i] = convert(comp[i])
  }
  
  
  
  complete = complete[! complete == 'DELETE']
  
  complete = unique(complete)
  
  mens_competitors = complete
}

setwd(wd)



# Womens --------------
setwd(paste(wd,'/Womens',sep=''))

year = as.numeric(format(as.Date(Sys.time(), format="%d/%m/%Y"),"%Y"))

month = as.numeric(format(as.Date(Sys.time(), format="%d/%m/%Y"),"%m"))

day = as.numeric(format(as.Date(Sys.time(), format="%d/%m/%Y"),"%d"))

event_url = paste('http://www.worldsurfleague.com/events/',paste(year,'/wct', sep = ''),sep = '')

events = read_html(event_url)

events2 = html_nodes(events,'.tour-event-range')

events3 = html_text(events2) 

events4 = c()

for(i in 1:length(events3)){
  if(grepl("Complete",x = events3[i], fixed = T)==1){
    events4[i] = 0
  }
  if(grepl("Upcoming",x = events3[i], fixed = T)==1){
    events4[i] = 1 
  }
  if(grepl("Standby",x = events3[i], fixed = T)==1){
    events4[i] = 0 
  }
}

events4 = as.numeric(events4)

upcoming_index = which(events4==1)[1]

spots = html_nodes(events,'.tour-event-name')

spots = html_text(spots) 

upcoming_womens = spot_names(spots)[upcoming_index]

upcoming_date = events3[upcoming_index]

start_month = which_month(upcoming_date)

start_day = unique(na.omit(as.numeric(unlist(strsplit(unlist(upcoming_date), "[^0-9]+")))))[1]

womens_go_time = 0

if(start_month == month & start_day - 1 == day){
  womens_go_time = 1
}

if(womens_go_time == 1){
  
  # competitors list ------
  
  names2010 = read.csv('whole2010.csv',stringsAsFactors = F)[,1]
  names2011 = read.csv('whole2011.csv',stringsAsFactors = F)[,1]
  names2012 = read.csv('whole2012.csv',stringsAsFactors = F)[,1]
  names2013 = read.csv('whole2013.csv',stringsAsFactors = F)[,1]
  names2014 = read.csv('whole2014.csv',stringsAsFactors = F)[,1]
  names2015 = read.csv('whole2015.csv',stringsAsFactors = F)[,1]
  names2016 = read.csv('whole2016.csv',stringsAsFactors = F)[,1]
  names2017 = read.csv('whole2017.csv',stringsAsFactors = F)[,1]
  
  names = unique(c(names2010,names2011,names2012,names2013,names2014,names2015,names2016,names2017))
  names2 = matrix(nrow = length(names),ncol=2)
  for(i in 1:length(names)){
    names2[i,1] = head(strsplit(names[i],split="")[[1]],1)
    names2[i,2] = tail(strsplit(names[i],split=" ")[[1]],1)
  }
  
  # convert to full name -----------
  
  convert = function(string){
    first_initial = head(strsplit(string,split="")[[1]],1)
    surname = tail(strsplit(string,split=" ")[[1]],1)
    if(first_initial %in% names2[,1] & surname %in% names2[,2]){
      fi_indexes = which(names2[,1] == first_initial)
      surname_indexes = which(names2[,2] == surname)
      if(sum(fi_indexes %in% surname_indexes)){
        return(names[fi_indexes[fi_indexes %in% surname_indexes]])
      }
      else(return('DELETE'))
    }
    else(return('DELETE'))
  }
  
  
  s = html_session(event_url)
  
  s = s %>% follow_link(spots[upcoming_index]) %>% follow_link("Results")
  
  comp = html_nodes(s,'.athlete-name')
  
  comp = html_text(comp)
  
  complete = c()
  for(i in 1:length(comp)){
    complete[i] = convert(comp[i])
  }
  
  complete = complete[! complete == 'DELETE']
  
  complete = unique(complete)
  
  womens_competitors = complete

}
setwd(wd)










