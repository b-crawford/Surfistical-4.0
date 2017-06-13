remove(list=ls(all=TRUE))

library(rvest)
library(magrittr)

source('wd.R')

# Mens --------

# all surfers

names2010 = read.csv('M_whole2010',stringsAsFactors = F)[,1]
names2011 = read.csv('M_whole2011',stringsAsFactors = F)[,1]
names2012 = read.csv('M_whole2012',stringsAsFactors = F)[,1]
names2013 = read.csv('M_whole2013',stringsAsFactors = F)[,1]
names2014 = read.csv('M_whole2014',stringsAsFactors = F)[,1]
names2015 = read.csv('M_whole2015',stringsAsFactors = F)[,1]
names2016 = read.csv('M_whole2016',stringsAsFactors = F)[,1]
names2017 = read.csv('M_whole2017',stringsAsFactors = F)[,1]

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

# spot names ----------

source('spot_names.R')

# Collect data:

for(year in 2010:2017){

  url = paste(paste('http://www.worldsurfleague.com/events/', year, sep=''),'/mct',sep='')
  
  page = read_html(url)
  
  nodes = html_nodes(page,'.tour-event-name')
  
  links = html_text(nodes)
  
  titles = spot_names(links)
  
  
  
  for(j in 1:length(links)){
    
    s = html_session(url)
  
    s  = s %>%  follow_link(links[j]) %>%  follow_link('Results') 
    
    surfer_nodes = html_nodes(s,'.athlete-name')
    
    surfers = html_text(surfer_nodes)
    
    surfers = unique(surfers)
    
    complete = c()
    for(i in 1:length(surfers)){
      complete[i] = convert(surfers[i])
    }
    
    complete = complete[! complete == 'DELETE']
    
    complete = unique(complete)
    
    competitor_vector_name = paste(paste(titles[j],year, sep=''), '.csv' , sep='')
    
    setwd(paste(wd,'/Competitors/Mens',sep=''))
    
    write.csv(complete, file = competitor_vector_name)
    
    setwd(wd)

  }
}


# Womens--------

# all surfers

names2010 = read.csv('F_whole2010',stringsAsFactors = F)[,1]
names2011 = read.csv('F_whole2011',stringsAsFactors = F)[,1]
names2012 = read.csv('F_whole2012',stringsAsFactors = F)[,1]
names2013 = read.csv('F_whole2013',stringsAsFactors = F)[,1]
names2014 = read.csv('F_whole2014',stringsAsFactors = F)[,1]
names2015 = read.csv('F_whole2015',stringsAsFactors = F)[,1]
names2016 = read.csv('F_whole2016',stringsAsFactors = F)[,1]
names2017 = read.csv('F_whole2017',stringsAsFactors = F)[,1]

names = unique(c(names2010,names2011,names2012,names2013,names2014,names2015,names2016,names2017))
names2 = matrix(nrow = length(names),ncol=2)
for(i in 1:length(names)){
  names2[i,1] = head(strsplit(names[i],split="")[[1]],1)
  names2[i,2] = tail(strsplit(names[i],split=" ")[[1]],1)
}



# Collect data:

for(year in 2011:2011){
  
  url = paste(paste('http://www.worldsurfleague.com/events/', year, sep=''),'/wct',sep='')
  
  page = read_html(url)
  
  nodes = html_nodes(page,'.tour-event-name')
  
  links = html_text(nodes)
  
  titles = spot_names(links)
  
  
  
  for(j in 1:length(links)){
    
    s = html_session(url)
    
    s  = s %>%  follow_link(links[j]) %>%  follow_link('Results') 
    
    surfer_nodes = html_nodes(s,'.athlete-name')
    
    surfers = html_text(surfer_nodes)
    
    surfers = unique(surfers)
    
    complete = c()
    for(i in 1:length(surfers)){
      complete[i] = convert(surfers[i])
    }
    
    complete = complete[! complete == 'DELETE']
    
    complete = unique(complete)
    
    competitor_vector_name = paste(paste(titles[j],year, sep=''), '.csv' , sep='')
    
    setwd(paste(wd,'/Competitors/Womens',sep=''))
    
    write.csv(complete, file = competitor_vector_name)
    
    setwd(wd)
    
  }
}





