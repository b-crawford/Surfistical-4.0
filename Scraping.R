
library(rvest)

# Spot function --------

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
} # not currently in use


# test change




# Mens ------
remove(list=ls(all=TRUE))
# 2017 results --------------------

year = 2017

rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/mct?year=', year, sep='')

rankings = read_html(rankings_url)



# Surfer names:
name_indexes = html_nodes(rankings,'.athlete-name')

names = html_text(name_indexes) 

# table

results = html_table(rankings,header = T)

results = data.frame(results[1])

end = dim(results)[2]

name_col = which(results[1,] == 'Name')

results2 = results[-1,-c(1:(name_col+1),end)]

row.names(results2) = names

for(i in 1:dim(results2)[1]){
  for(j in 1:dim(results2)[2]){
    results2[i,j] = gsub('[PointsThrowaway]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[Points]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[-]','NA', results2[i,j])
    results2[i,j] = gsub('[,]','', results2[i,j])
    results2[i,j] = gsub("[[:space:]]", "", results2[i,j])
    results2[i,j] = gsub("[INJ]", "NA", results2[i,j])
    if(results2[i,j] == '131750'){results2[i,j] = 13}
    else if(results2[i,j] == '25500'){results2[i,j] = 25}
    else if(results2[i,j] == '171750'){results2[i,j] = 17}
    else if(results2[i,j] == 'NA'){}
    else(results2[i,j] = as.numeric(substr(results2[i,j],1,1)))
  }
}

results3 = data.frame(data.matrix(results2))

#  Spot names:

spot_indexes = html_nodes(rankings,'.athlete-event-place strong')

spots = html_text(spot_indexes) 

spots = spots[1:dim(results3)[2]]

for(i in 1:length(spots)){
  if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
    spots[i] = "GoldCoast"
  }
  if(grepl("Margaret River",x = spots[i], fixed = T)==1){
    spots[i] = "MargaretRiver"
  }
  if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
    spots[i] = "BellsBeach"
  }
  if(grepl("Rio",x = spots[i], fixed = T)==1){
    spots[i] = "Rio"
  }
  if(grepl("Fiji",x = spots[i], fixed = T)==1){
    spots[i] = "Fiji"
  }
  if(grepl("J-Bay",x = spots[i], fixed = T)==1){
    spots[i] = "Jbay"
  }
  if(grepl("Tahiti",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("France",x = spots[i], fixed = T)==1){
    spots[i] = "France"
  }
  if(grepl("Portugal",x = spots[i], fixed = T)==1){
    spots[i] = "Portugal"
  }
  if(grepl("Pipe",x = spots[i], fixed = T)==1){
    spots[i] = "Pipeline"
  }
  if(grepl("Bali",x = spots[i], fixed = T)==1){
    spots[i] = "Bali"
  }
  if(grepl("Trestles",x = spots[i], fixed = T)==1){
    spots[i] = "Trestles"
  }
  if(grepl("Catarina",x = spots[i], fixed = T)==1){
    spots[i] = "SantaCatarina"
  }
  if(grepl("New York",x = spots[i], fixed = T)==1){
    spots[i] = "NewYork"
  }
  if(grepl("Search",x = spots[i], fixed = T)==1){
    spots[i] = "SanFrancisco"
  }
}

colnames(results3) = spots

# next exits?

next__ = html_nodes(rankings,'.next a')
if(grepl("Next",html_text(next__))){
  rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/mct?offset=51&sort=rank&year=', year, sep='')
  rankings = read_html(rankings_url)
  # Surfer names:
  name_indexes = html_nodes(rankings,'.athlete-name')
  
  names = html_text(name_indexes) 
  
  # table
  
  res = html_table(rankings,header = T)
  
  res = data.frame(res[1])
  
  dim(res)
  
  end = dim(res)[2]
  
  name_col = which(res[1,] == 'Name')
  
  res2 = res[-1,-c(1:(name_col+1),end)]
  
  res = data.frame(res[1])
  
  row.names(res2) = names
  
  for(i in 1:dim(res2)[1]){
    for(j in 1:dim(res2)[2]){
      res2[i,j] = gsub('[PointsThrowaway]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[Points]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[-]','NA', res2[i,j])
      res2[i,j] = gsub('[,]','', res2[i,j])
      res2[i,j] = gsub("[[:space:]]", "", res2[i,j])
      res2[i,j] = gsub("[INJ]", "NA", res2[i,j])
      if(res2[i,j] == '131750'){res2[i,j] = 13}
      else if(res2[i,j] == '25500'){res2[i,j] = 25}
      else if(res2[i,j] == '171750'){res2[i,j] = 17}
      else if(res2[i,j] == 'NA'){}
      else(res2[i,j] = as.numeric(substr(res2[i,j],1,1)))
    }
  }
  
  res3 = data.frame(data.matrix(res2))
  
  #  Spot names:
  
  spot_indexes = html_nodes(rankings,'.athlete-event-place strong')
  
  spots = html_text(spot_indexes) 
  
  spots = spots[1:dim(res3)[2]]
  
  for(i in 1:length(spots)){
    if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
      spots[i] = "GoldCoast"
    }
    if(grepl("Margaret River",x = spots[i], fixed = T)==1){
      spots[i] = "MargaretRiver"
    }
    if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
      spots[i] = "BellsBeach"
    }
    if(grepl("Rio",x = spots[i], fixed = T)==1){
      spots[i] = "Rio"
    }
    if(grepl("Fiji",x = spots[i], fixed = T)==1){
      spots[i] = "Fiji"
    }
    if(grepl("J-Bay",x = spots[i], fixed = T)==1){
      spots[i] = "Jbay"
    }
    if(grepl("Tahiti",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("France",x = spots[i], fixed = T)==1){
      spots[i] = "France"
    }
    if(grepl("Portugal",x = spots[i], fixed = T)==1){
      spots[i] = "Portugal"
    }
    if(grepl("Pipe",x = spots[i], fixed = T)==1){
      spots[i] = "Pipeline"
    }
    if(grepl("Bali",x = spots[i], fixed = T)==1){
      spots[i] = "Bali"
    }
    if(grepl("Trestles",x = spots[i], fixed = T)==1){
      spots[i] = "Trestles"
    }
    if(grepl("Catarina",x = spots[i], fixed = T)==1){
      spots[i] = "SantaCatarina"
    }
    if(grepl("New York",x = spots[i], fixed = T)==1){
      spots[i] = "NewYork"
    }
    if(grepl("Search",x = spots[i], fixed = T)==1){
      spots[i] = "SanFrancisco"
    }
  }
  
  colnames(res3) = spots
  res4 = res3
  whole2017 <<- rbind(results3, res4)
}
if(!exists('whole2017')){
  whole2017<<-results3
}

# 2016 results --------------------

year = 2016

rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/mct?year=', year, sep='')

rankings = read_html(rankings_url)



# Surfer names:
name_indexes = html_nodes(rankings,'.athlete-name')

names = html_text(name_indexes) 

# table

results = html_table(rankings,header = T)

results = data.frame(results[1])

end = dim(results)[2]

name_col = which(results[1,] == 'Name')

results2 = results[-1,-c(1:(name_col+1),end)]

row.names(results2) = names

for(i in 1:dim(results2)[1]){
  for(j in 1:dim(results2)[2]){
    results2[i,j] = gsub('[PointsThrowaway]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[Points]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[-]','NA', results2[i,j])
    results2[i,j] = gsub('[,]','', results2[i,j])
    results2[i,j] = gsub("[[:space:]]", "", results2[i,j])
    results2[i,j] = gsub("[INJ]", "NA", results2[i,j])
    if(results2[i,j] == '131750'){results2[i,j] = 13}
    else if(results2[i,j] == '25500'){results2[i,j] = 25}
    else if(results2[i,j] == '171750'){results2[i,j] = 17}
    else if(results2[i,j] == 'NA'){}
    else(results2[i,j] = as.numeric(substr(results2[i,j],1,1)))
  }
}

results3 = data.frame(data.matrix(results2))

#  Spot names:

spot_indexes = html_nodes(rankings,'.athlete-event-place strong')

spots = html_text(spot_indexes) 

spots = spots[1:dim(results3)[2]]

for(i in 1:length(spots)){
  if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
    spots[i] = "GoldCoast"
  }
  if(grepl("Margaret River",x = spots[i], fixed = T)==1){
    spots[i] = "MargaretRiver"
  }
  if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
    spots[i] = "BellsBeach"
  }
  if(grepl("Rio",x = spots[i], fixed = T)==1){
    spots[i] = "Rio"
  }
  if(grepl("Fiji",x = spots[i], fixed = T)==1){
    spots[i] = "Fiji"
  }
  if(grepl("J-Bay",x = spots[i], fixed = T)==1){
    spots[i] = "Jbay"
  }
  if(grepl("Tahiti",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("France",x = spots[i], fixed = T)==1){
    spots[i] = "France"
  }
  if(grepl("Portugal",x = spots[i], fixed = T)==1){
    spots[i] = "Portugal"
  }
  if(grepl("Pipe",x = spots[i], fixed = T)==1){
    spots[i] = "Pipeline"
  }
  if(grepl("Bali",x = spots[i], fixed = T)==1){
    spots[i] = "Bali"
  }
  if(grepl("Trestles",x = spots[i], fixed = T)==1){
    spots[i] = "Trestles"
  }
  if(grepl("Catarina",x = spots[i], fixed = T)==1){
    spots[i] = "SantaCatarina"
  }
  if(grepl("New York",x = spots[i], fixed = T)==1){
    spots[i] = "NewYork"
  }
  if(grepl("Search",x = spots[i], fixed = T)==1){
    spots[i] = "SanFrancisco"
  }
}

colnames(results3) = spots

# next exits?

next__ = html_nodes(rankings,'.next a')
if(grepl("Next",html_text(next__))){
  rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/mct?offset=51&sort=rank&year=', year, sep='')
  rankings = read_html(rankings_url)
  # Surfer names:
  name_indexes = html_nodes(rankings,'.athlete-name')
  
  names = html_text(name_indexes) 
  
  # table
  
  res = html_table(rankings,header = T)
  
  res = data.frame(res[1])
  
  dim(res)
  
  end = dim(res)[2]
  
  name_col = which(res[1,] == 'Name')
  
  res2 = res[-1,-c(1:(name_col+1),end)]
  
  res = data.frame(res[1])
  
  row.names(res2) = names
  
  for(i in 1:dim(res2)[1]){
    for(j in 1:dim(res2)[2]){
      res2[i,j] = gsub('[PointsThrowaway]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[Points]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[-]','NA', res2[i,j])
      res2[i,j] = gsub('[,]','', res2[i,j])
      res2[i,j] = gsub("[[:space:]]", "", res2[i,j])
      res2[i,j] = gsub("[INJ]", "NA", res2[i,j])
      if(res2[i,j] == '131750'){res2[i,j] = 13}
      else if(res2[i,j] == '25500'){res2[i,j] = 25}
      else if(res2[i,j] == '171750'){res2[i,j] = 17}
      else if(res2[i,j] == 'NA'){}
      else(res2[i,j] = as.numeric(substr(res2[i,j],1,1)))
    }
  }
  
  res3 = data.frame(data.matrix(res2))
  
  #  Spot names:
  
  spot_indexes = html_nodes(rankings,'.athlete-event-place strong')
  
  spots = html_text(spot_indexes) 
  
  spots = spots[1:dim(res3)[2]]
  
  for(i in 1:length(spots)){
    if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
      spots[i] = "GoldCoast"
    }
    if(grepl("Margaret River",x = spots[i], fixed = T)==1){
      spots[i] = "MargaretRiver"
    }
    if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
      spots[i] = "BellsBeach"
    }
    if(grepl("Rio",x = spots[i], fixed = T)==1){
      spots[i] = "Rio"
    }
    if(grepl("Fiji",x = spots[i], fixed = T)==1){
      spots[i] = "Fiji"
    }
    if(grepl("J-Bay",x = spots[i], fixed = T)==1){
      spots[i] = "Jbay"
    }
    if(grepl("Tahiti",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("France",x = spots[i], fixed = T)==1){
      spots[i] = "France"
    }
    if(grepl("Portugal",x = spots[i], fixed = T)==1){
      spots[i] = "Portugal"
    }
    if(grepl("Pipe",x = spots[i], fixed = T)==1){
      spots[i] = "Pipeline"
    }
    if(grepl("Bali",x = spots[i], fixed = T)==1){
      spots[i] = "Bali"
    }
    if(grepl("Trestles",x = spots[i], fixed = T)==1){
      spots[i] = "Trestles"
    }
    if(grepl("Catarina",x = spots[i], fixed = T)==1){
      spots[i] = "SantaCatarina"
    }
    if(grepl("New York",x = spots[i], fixed = T)==1){
      spots[i] = "NewYork"
    }
    if(grepl("Search",x = spots[i], fixed = T)==1){
      spots[i] = "SanFrancisco"
    }
  }
  
  colnames(res3) = spots
  res4 = res3
  whole2016 <<- rbind(results3, res4)
}
if(!exists('whole2016')){
  whole2016<<-results3
}




head(whole2016)

# 2015 results --------------------

year = 2015

rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/mct?year=', year, sep='')

rankings = read_html(rankings_url)



# Surfer names:
name_indexes = html_nodes(rankings,'.athlete-name')

names = html_text(name_indexes) 

# table

results = html_table(rankings,header = T)

results = data.frame(results[1])

end = dim(results)[2]

name_col = which(results[1,] == 'Name')

results2 = results[-1,-c(1:(name_col+1),end)]

row.names(results2) = names

for(i in 1:dim(results2)[1]){
  for(j in 1:dim(results2)[2]){
    results2[i,j] = gsub('[PointsThrowaway]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[Points]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[-]','NA', results2[i,j])
    results2[i,j] = gsub('[,]','', results2[i,j])
    results2[i,j] = gsub("[[:space:]]", "", results2[i,j])
    results2[i,j] = gsub("[INJ]", "NA", results2[i,j])
    if(results2[i,j] == '131750'){results2[i,j] = 13}
    else if(results2[i,j] == '25500'){results2[i,j] = 25}
    else if(results2[i,j] == '171750'){results2[i,j] = 17}
    else if(results2[i,j] == 'NA'){}
    else(results2[i,j] = as.numeric(substr(results2[i,j],1,1)))
  }
}

results3 = data.frame(data.matrix(results2))

#  Spot names:

spot_indexes = html_nodes(rankings,'.athlete-event-place strong')

spots = html_text(spot_indexes) 

spots = spots[1:dim(results3)[2]]

for(i in 1:length(spots)){
  if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
    spots[i] = "GoldCoast"
  }
  if(grepl("Margaret River",x = spots[i], fixed = T)==1){
    spots[i] = "MargaretRiver"
  }
  if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
    spots[i] = "BellsBeach"
  }
  if(grepl("Rio",x = spots[i], fixed = T)==1){
    spots[i] = "Rio"
  }
  if(grepl("Fiji",x = spots[i], fixed = T)==1){
    spots[i] = "Fiji"
  }
  if(grepl("J-Bay",x = spots[i], fixed = T)==1){
    spots[i] = "Jbay"
  }
  if(grepl("Tahiti",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("France",x = spots[i], fixed = T)==1){
    spots[i] = "France"
  }
  if(grepl("Portugal",x = spots[i], fixed = T)==1){
    spots[i] = "Portugal"
  }
  if(grepl("Pipe",x = spots[i], fixed = T)==1){
    spots[i] = "Pipeline"
  }
  if(grepl("Bali",x = spots[i], fixed = T)==1){
    spots[i] = "Bali"
  }
  if(grepl("Trestles",x = spots[i], fixed = T)==1){
    spots[i] = "Trestles"
  }
  if(grepl("Catarina",x = spots[i], fixed = T)==1){
    spots[i] = "SantaCatarina"
  }
  if(grepl("New York",x = spots[i], fixed = T)==1){
    spots[i] = "NewYork"
  }
  if(grepl("Search",x = spots[i], fixed = T)==1){
    spots[i] = "SanFrancisco"
  }
}

colnames(results3) = spots

# next exits?

next__ = html_nodes(rankings,'.next a')
if(grepl("Next",html_text(next__))){
  rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/mct?offset=51&sort=rank&year=', year, sep='')
  rankings = read_html(rankings_url)
  # Surfer names:
  name_indexes = html_nodes(rankings,'.athlete-name')
  
  names = html_text(name_indexes) 
  
  # table
  
  res = html_table(rankings,header = T)
  
  res = data.frame(res[1])
  
  dim(res)
  
  end = dim(res)[2]
  
  name_col = which(res[1,] == 'Name')
  
  res2 = res[-1,-c(1:(name_col+1),end)]
  
  res = data.frame(res[1])
  
  row.names(res2) = names
  
  for(i in 1:dim(res2)[1]){
    for(j in 1:dim(res2)[2]){
      res2[i,j] = gsub('[PointsThrowaway]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[Points]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[-]','NA', res2[i,j])
      res2[i,j] = gsub('[,]','', res2[i,j])
      res2[i,j] = gsub("[[:space:]]", "", res2[i,j])
      res2[i,j] = gsub("[INJ]", "NA", res2[i,j])
      if(res2[i,j] == '131750'){res2[i,j] = 13}
      else if(res2[i,j] == '25500'){res2[i,j] = 25}
      else if(res2[i,j] == '171750'){res2[i,j] = 17}
      else if(res2[i,j] == 'NA'){}
      else(res2[i,j] = as.numeric(substr(res2[i,j],1,1)))
    }
  }
  
  res3 = data.frame(data.matrix(res2))
  
  #  Spot names:
  
  spot_indexes = html_nodes(rankings,'.athlete-event-place strong')
  
  spots = html_text(spot_indexes) 
  
  spots = spots[1:dim(res3)[2]]
  
  for(i in 1:length(spots)){
    if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
      spots[i] = "GoldCoast"
    }
    if(grepl("Margaret River",x = spots[i], fixed = T)==1){
      spots[i] = "MargaretRiver"
    }
    if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
      spots[i] = "BellsBeach"
    }
    if(grepl("Rio",x = spots[i], fixed = T)==1){
      spots[i] = "Rio"
    }
    if(grepl("Fiji",x = spots[i], fixed = T)==1){
      spots[i] = "Fiji"
    }
    if(grepl("J-Bay",x = spots[i], fixed = T)==1){
      spots[i] = "Jbay"
    }
    if(grepl("Tahiti",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("France",x = spots[i], fixed = T)==1){
      spots[i] = "France"
    }
    if(grepl("Portugal",x = spots[i], fixed = T)==1){
      spots[i] = "Portugal"
    }
    if(grepl("Pipe",x = spots[i], fixed = T)==1){
      spots[i] = "Pipeline"
    }
    if(grepl("Bali",x = spots[i], fixed = T)==1){
      spots[i] = "Bali"
    }
    if(grepl("Trestles",x = spots[i], fixed = T)==1){
      spots[i] = "Trestles"
    }
    if(grepl("Catarina",x = spots[i], fixed = T)==1){
      spots[i] = "SantaCatarina"
    }
    if(grepl("New York",x = spots[i], fixed = T)==1){
      spots[i] = "NewYork"
    }
    if(grepl("Search",x = spots[i], fixed = T)==1){
      spots[i] = "SanFrancisco"
    }
  }
  
  colnames(res3) = spots
  res4 = res3
  whole2015 <<- rbind(results3, res4)
}
if(!exists('whole2015')){
  whole2015<<-results3
}

# 2014 results --------------------

year = 2014

rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/mct?year=', year, sep='')

rankings = read_html(rankings_url)



# Surfer names:
name_indexes = html_nodes(rankings,'.athlete-name')

names = html_text(name_indexes) 

# table

results = html_table(rankings,header = T)

results = data.frame(results[1])

end = dim(results)[2]

name_col = which(results[1,] == 'Name')

results2 = results[-1,-c(1:(name_col+1),end)]

row.names(results2) = names

for(i in 1:dim(results2)[1]){
  for(j in 1:dim(results2)[2]){
    results2[i,j] = gsub('[PointsThrowaway]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[Points]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[-]','NA', results2[i,j])
    results2[i,j] = gsub('[,]','', results2[i,j])
    results2[i,j] = gsub("[[:space:]]", "", results2[i,j])
    results2[i,j] = gsub("[INJ]", "NA", results2[i,j])
    if(results2[i,j] == '131750'){results2[i,j] = 13}
    else if(results2[i,j] == '25500'){results2[i,j] = 25}
    else if(results2[i,j] == '171750'){results2[i,j] = 17}
    else if(results2[i,j] == 'NA'){}
    else(results2[i,j] = as.numeric(substr(results2[i,j],1,1)))
  }
}

results3 = data.frame(data.matrix(results2))

#  Spot names:

spot_indexes = html_nodes(rankings,'.athlete-event-place strong')

spots = html_text(spot_indexes) 

spots = spots[1:dim(results3)[2]]

for(i in 1:length(spots)){
  if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
    spots[i] = "GoldCoast"
  }
  if(grepl("Margaret River",x = spots[i], fixed = T)==1){
    spots[i] = "MargaretRiver"
  }
  if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
    spots[i] = "BellsBeach"
  }
  if(grepl("Rio",x = spots[i], fixed = T)==1){
    spots[i] = "Rio"
  }
  if(grepl("Fiji",x = spots[i], fixed = T)==1){
    spots[i] = "Fiji"
  }
  if(grepl("J-Bay",x = spots[i], fixed = T)==1){
    spots[i] = "Jbay"
  }
  if(grepl("Tahiti",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("France",x = spots[i], fixed = T)==1){
    spots[i] = "France"
  }
  if(grepl("Portugal",x = spots[i], fixed = T)==1){
    spots[i] = "Portugal"
  }
  if(grepl("Pipe",x = spots[i], fixed = T)==1){
    spots[i] = "Pipeline"
  }
  if(grepl("Bali",x = spots[i], fixed = T)==1){
    spots[i] = "Bali"
  }
  if(grepl("Trestles",x = spots[i], fixed = T)==1){
    spots[i] = "Trestles"
  }
  if(grepl("Catarina",x = spots[i], fixed = T)==1){
    spots[i] = "SantaCatarina"
  }
  if(grepl("New York",x = spots[i], fixed = T)==1){
    spots[i] = "NewYork"
  }
  if(grepl("Search",x = spots[i], fixed = T)==1){
    spots[i] = "SanFrancisco"
  }
}

colnames(results3) = spots

# next exits?

next__ = html_nodes(rankings,'.next a')
if(grepl("Next",html_text(next__))){
  rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/mct?offset=51&sort=rank&year=', year, sep='')
  rankings = read_html(rankings_url)
  # Surfer names:
  name_indexes = html_nodes(rankings,'.athlete-name')
  
  names = html_text(name_indexes) 
  
  # table
  
  res = html_table(rankings,header = T)
  
  res = data.frame(res[1])
  
  dim(res)
  
  end = dim(res)[2]
  
  name_col = which(res[1,] == 'Name')
  
  res2 = res[-1,-c(1:(name_col+1),end)]
  
  res = data.frame(res[1])
  
  row.names(res2) = names
  
  for(i in 1:dim(res2)[1]){
    for(j in 1:dim(res2)[2]){
      res2[i,j] = gsub('[PointsThrowaway]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[Points]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[-]','NA', res2[i,j])
      res2[i,j] = gsub('[,]','', res2[i,j])
      res2[i,j] = gsub("[[:space:]]", "", res2[i,j])
      res2[i,j] = gsub("[INJ]", "NA", res2[i,j])
      if(res2[i,j] == '131750'){res2[i,j] = 13}
      else if(res2[i,j] == '25500'){res2[i,j] = 25}
      else if(res2[i,j] == '171750'){res2[i,j] = 17}
      else if(res2[i,j] == 'NA'){}
      else(res2[i,j] = as.numeric(substr(res2[i,j],1,1)))
    }
  }
  
  res3 = data.frame(data.matrix(res2))
  
  #  Spot names:
  
  spot_indexes = html_nodes(rankings,'.athlete-event-place strong')
  
  spots = html_text(spot_indexes) 
  
  spots = spots[1:dim(res3)[2]]
  
  for(i in 1:length(spots)){
    if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
      spots[i] = "GoldCoast"
    }
    if(grepl("Margaret River",x = spots[i], fixed = T)==1){
      spots[i] = "MargaretRiver"
    }
    if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
      spots[i] = "BellsBeach"
    }
    if(grepl("Rio",x = spots[i], fixed = T)==1){
      spots[i] = "Rio"
    }
    if(grepl("Fiji",x = spots[i], fixed = T)==1){
      spots[i] = "Fiji"
    }
    if(grepl("J-Bay",x = spots[i], fixed = T)==1){
      spots[i] = "Jbay"
    }
    if(grepl("Tahiti",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("France",x = spots[i], fixed = T)==1){
      spots[i] = "France"
    }
    if(grepl("Portugal",x = spots[i], fixed = T)==1){
      spots[i] = "Portugal"
    }
    if(grepl("Pipe",x = spots[i], fixed = T)==1){
      spots[i] = "Pipeline"
    }
    if(grepl("Bali",x = spots[i], fixed = T)==1){
      spots[i] = "Bali"
    }
    if(grepl("Trestles",x = spots[i], fixed = T)==1){
      spots[i] = "Trestles"
    }
    if(grepl("Catarina",x = spots[i], fixed = T)==1){
      spots[i] = "SantaCatarina"
    }
    if(grepl("New York",x = spots[i], fixed = T)==1){
      spots[i] = "NewYork"
    }
    if(grepl("Search",x = spots[i], fixed = T)==1){
      spots[i] = "SanFrancisco"
    }
  }
  
  colnames(res3) = spots
  res4 = res3
  whole2014 <<- rbind(results3, res4)
}
if(!exists('whole2014')){
  whole2014<<-results3
}





# 2013 results --------------------

year = 2013

rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/mct?year=', year, sep='')

rankings = read_html(rankings_url)



# Surfer names:
name_indexes = html_nodes(rankings,'.athlete-name')

names = html_text(name_indexes) 

# table

results = html_table(rankings,header = T)

results = data.frame(results[1])

end = dim(results)[2]

name_col = which(results[1,] == 'Name')

results2 = results[-1,-c(1:(name_col+1),end)]

row.names(results2) = names

for(i in 1:dim(results2)[1]){
  for(j in 1:dim(results2)[2]){
    results2[i,j] = gsub('[PointsThrowaway]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[Points]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[-]','NA', results2[i,j])
    results2[i,j] = gsub('[,]','', results2[i,j])
    results2[i,j] = gsub("[[:space:]]", "", results2[i,j])
    results2[i,j] = gsub("[INJ]", "NA", results2[i,j])
    if(results2[i,j] == '131750'){results2[i,j] = 13}
    else if(results2[i,j] == '25500'){results2[i,j] = 25}
    else if(results2[i,j] == '171750'){results2[i,j] = 17}
    else if(results2[i,j] == 'NA'){}
    else(results2[i,j] = as.numeric(substr(results2[i,j],1,1)))
  }
}

results3 = data.frame(data.matrix(results2))

#  Spot names:

spot_indexes = html_nodes(rankings,'.athlete-event-place strong')

spots = html_text(spot_indexes) 

spots = spots[1:dim(results3)[2]]

for(i in 1:length(spots)){
  if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
    spots[i] = "GoldCoast"
  }
  if(grepl("Margaret River",x = spots[i], fixed = T)==1){
    spots[i] = "MargaretRiver"
  }
  if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
    spots[i] = "BellsBeach"
  }
  if(grepl("Rio",x = spots[i], fixed = T)==1){
    spots[i] = "Rio"
  }
  if(grepl("Fiji",x = spots[i], fixed = T)==1){
    spots[i] = "Fiji"
  }
  if(grepl("J-Bay",x = spots[i], fixed = T)==1){
    spots[i] = "Jbay"
  }
  if(grepl("Tahiti",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("France",x = spots[i], fixed = T)==1){
    spots[i] = "France"
  }
  if(grepl("Portugal",x = spots[i], fixed = T)==1){
    spots[i] = "Portugal"
  }
  if(grepl("Pipe",x = spots[i], fixed = T)==1){
    spots[i] = "Pipeline"
  }
  if(grepl("Bali",x = spots[i], fixed = T)==1){
    spots[i] = "Bali"
  }
  if(grepl("Trestles",x = spots[i], fixed = T)==1){
    spots[i] = "Trestles"
  }
  if(grepl("Catarina",x = spots[i], fixed = T)==1){
    spots[i] = "SantaCatarina"
  }
  if(grepl("New York",x = spots[i], fixed = T)==1){
    spots[i] = "NewYork"
  }
  if(grepl("Search",x = spots[i], fixed = T)==1){
    spots[i] = "SanFrancisco"
  }
}

colnames(results3) = spots

# next exits?

next__ = html_nodes(rankings,'.next a')
if(grepl("Next",html_text(next__))){
  rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/mct?offset=51&sort=rank&year=', year, sep='')
  rankings = read_html(rankings_url)
  # Surfer names:
  name_indexes = html_nodes(rankings,'.athlete-name')
  
  names = html_text(name_indexes) 
  
  # table
  
  res = html_table(rankings,header = T)
  
  res = data.frame(res[1])
  
  dim(res)
  
  end = dim(res)[2]
  
  name_col = which(res[1,] == 'Name')
  
  res2 = res[-1,-c(1:(name_col+1),end)]
  
  res = data.frame(res[1])
  
  row.names(res2) = names
  
  for(i in 1:dim(res2)[1]){
    for(j in 1:dim(res2)[2]){
      res2[i,j] = gsub('[PointsThrowaway]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[Points]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[-]','NA', res2[i,j])
      res2[i,j] = gsub('[,]','', res2[i,j])
      res2[i,j] = gsub("[[:space:]]", "", res2[i,j])
      res2[i,j] = gsub("[INJ]", "NA", res2[i,j])
      if(res2[i,j] == '131750'){res2[i,j] = 13}
      else if(res2[i,j] == '25500'){res2[i,j] = 25}
      else if(res2[i,j] == '171750'){res2[i,j] = 17}
      else if(res2[i,j] == 'NA'){}
      else(res2[i,j] = as.numeric(substr(res2[i,j],1,1)))
    }
  }
  
  res3 = data.frame(data.matrix(res2))
  
  #  Spot names:
  
  spot_indexes = html_nodes(rankings,'.athlete-event-place strong')
  
  spots = html_text(spot_indexes) 
  
  spots = spots[1:dim(res3)[2]]
  
  for(i in 1:length(spots)){
    if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
      spots[i] = "GoldCoast"
    }
    if(grepl("Margaret River",x = spots[i], fixed = T)==1){
      spots[i] = "MargaretRiver"
    }
    if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
      spots[i] = "BellsBeach"
    }
    if(grepl("Rio",x = spots[i], fixed = T)==1){
      spots[i] = "Rio"
    }
    if(grepl("Fiji",x = spots[i], fixed = T)==1){
      spots[i] = "Fiji"
    }
    if(grepl("J-Bay",x = spots[i], fixed = T)==1){
      spots[i] = "Jbay"
    }
    if(grepl("Tahiti",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("France",x = spots[i], fixed = T)==1){
      spots[i] = "France"
    }
    if(grepl("Portugal",x = spots[i], fixed = T)==1){
      spots[i] = "Portugal"
    }
    if(grepl("Pipe",x = spots[i], fixed = T)==1){
      spots[i] = "Pipeline"
    }
    if(grepl("Bali",x = spots[i], fixed = T)==1){
      spots[i] = "Bali"
    }
    if(grepl("Trestles",x = spots[i], fixed = T)==1){
      spots[i] = "Trestles"
    }
    if(grepl("Catarina",x = spots[i], fixed = T)==1){
      spots[i] = "SantaCatarina"
    }
    if(grepl("New York",x = spots[i], fixed = T)==1){
      spots[i] = "NewYork"
    }
    if(grepl("Search",x = spots[i], fixed = T)==1){
      spots[i] = "SanFrancisco"
    }
  }
  
  colnames(res3) = spots
  res4 = res3
  whole2013 <<- rbind(results3, res4)
}
if(!exists('whole2013')){
  whole2013<<-results3
}

# 2012 results --------------------

year = 2012

rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/mct?year=', year, sep='')

rankings = read_html(rankings_url)



# Surfer names:
name_indexes = html_nodes(rankings,'.athlete-name')

names = html_text(name_indexes) 

# table

results = html_table(rankings,header = T)

results = data.frame(results[1])

end = dim(results)[2]

name_col = which(results[1,] == 'Name')

results2 = results[-1,-c(1:(name_col+1),end)]

row.names(results2) = names

for(i in 1:dim(results2)[1]){
  for(j in 1:dim(results2)[2]){
    results2[i,j] = gsub('[PointsThrowaway]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[Points]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[-]','NA', results2[i,j])
    results2[i,j] = gsub('[,]','', results2[i,j])
    results2[i,j] = gsub("[[:space:]]", "", results2[i,j])
    results2[i,j] = gsub("[INJ]", "NA", results2[i,j])
    if(results2[i,j] == '131750'){results2[i,j] = 13}
    else if(results2[i,j] == '25500'){results2[i,j] = 25}
    else if(results2[i,j] == '171750'){results2[i,j] = 17}
    else if(results2[i,j] == 'NA'){}
    else(results2[i,j] = as.numeric(substr(results2[i,j],1,1)))
  }
}

results3 = data.frame(data.matrix(results2))

#  Spot names:

spot_indexes = html_nodes(rankings,'.athlete-event-place strong')

spots = html_text(spot_indexes) 

spots = spots[1:dim(results3)[2]]

for(i in 1:length(spots)){
  if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
    spots[i] = "GoldCoast"
  }
  if(grepl("Margaret River",x = spots[i], fixed = T)==1){
    spots[i] = "MargaretRiver"
  }
  if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
    spots[i] = "BellsBeach"
  }
  if(grepl("Rio",x = spots[i], fixed = T)==1){
    spots[i] = "Rio"
  }
  if(grepl("Fiji",x = spots[i], fixed = T)==1){
    spots[i] = "Fiji"
  }
  if(grepl("J-Bay",x = spots[i], fixed = T)==1){
    spots[i] = "Jbay"
  }
  if(grepl("Tahiti",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("France",x = spots[i], fixed = T)==1){
    spots[i] = "France"
  }
  if(grepl("Portugal",x = spots[i], fixed = T)==1){
    spots[i] = "Portugal"
  }
  if(grepl("Pipe",x = spots[i], fixed = T)==1){
    spots[i] = "Pipeline"
  }
  if(grepl("Bali",x = spots[i], fixed = T)==1){
    spots[i] = "Bali"
  }
  if(grepl("Trestles",x = spots[i], fixed = T)==1){
    spots[i] = "Trestles"
  }
  if(grepl("Catarina",x = spots[i], fixed = T)==1){
    spots[i] = "SantaCatarina"
  }
  if(grepl("New York",x = spots[i], fixed = T)==1){
    spots[i] = "NewYork"
  }
  if(grepl("Search",x = spots[i], fixed = T)==1){
    spots[i] = "SanFrancisco"
  }
  if(grepl("Santa Cruz",x = spots[i], fixed = T)==1){
    spots[i] = "SantaCruz"
  }
}

colnames(results3) = spots

# next exits?

next__ = html_nodes(rankings,'.next a')
if(grepl("Next",html_text(next__))){
  rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/mct?offset=51&sort=rank&year=', year, sep='')
  rankings = read_html(rankings_url)
  # Surfer names:
  name_indexes = html_nodes(rankings,'.athlete-name')
  
  names = html_text(name_indexes) 
  
  # table
  
  res = html_table(rankings,header = T)
  
  res = data.frame(res[1])
  
  dim(res)
  
  end = dim(res)[2]
  
  name_col = which(res[1,] == 'Name')
  
  res2 = res[-1,-c(1:(name_col+1),end)]
  
  res = data.frame(res[1])
  
  row.names(res2) = names
  
  for(i in 1:dim(res2)[1]){
    for(j in 1:dim(res2)[2]){
      res2[i,j] = gsub('[PointsThrowaway]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[Points]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[-]','NA', res2[i,j])
      res2[i,j] = gsub('[,]','', res2[i,j])
      res2[i,j] = gsub("[[:space:]]", "", res2[i,j])
      res2[i,j] = gsub("[INJ]", "NA", res2[i,j])
      if(res2[i,j] == '131750'){res2[i,j] = 13}
      else if(res2[i,j] == '25500'){res2[i,j] = 25}
      else if(res2[i,j] == '171750'){res2[i,j] = 17}
      else if(res2[i,j] == 'NA'){}
      else(res2[i,j] = as.numeric(substr(res2[i,j],1,1)))
    }
  }
  
  res3 = data.frame(data.matrix(res2))
  
  #  Spot names:
  
  spot_indexes = html_nodes(rankings,'.athlete-event-place strong')
  
  spots = html_text(spot_indexes) 
  
  spots = spots[1:dim(res3)[2]]
  
  for(i in 1:length(spots)){
    if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
      spots[i] = "GoldCoast"
    }
    if(grepl("Margaret River",x = spots[i], fixed = T)==1){
      spots[i] = "MargaretRiver"
    }
    if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
      spots[i] = "BellsBeach"
    }
    if(grepl("Rio",x = spots[i], fixed = T)==1){
      spots[i] = "Rio"
    }
    if(grepl("Fiji",x = spots[i], fixed = T)==1){
      spots[i] = "Fiji"
    }
    if(grepl("J-Bay",x = spots[i], fixed = T)==1){
      spots[i] = "Jbay"
    }
    if(grepl("Tahiti",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("France",x = spots[i], fixed = T)==1){
      spots[i] = "France"
    }
    if(grepl("Portugal",x = spots[i], fixed = T)==1){
      spots[i] = "Portugal"
    }
    if(grepl("Pipe",x = spots[i], fixed = T)==1){
      spots[i] = "Pipeline"
    }
    if(grepl("Bali",x = spots[i], fixed = T)==1){
      spots[i] = "Bali"
    }
    if(grepl("Trestles",x = spots[i], fixed = T)==1){
      spots[i] = "Trestles"
    }
    if(grepl("Catarina",x = spots[i], fixed = T)==1){
      spots[i] = "SantaCatarina"
    }
    if(grepl("New York",x = spots[i], fixed = T)==1){
      spots[i] = "NewYork"
    }
    if(grepl("Search",x = spots[i], fixed = T)==1){
      spots[i] = "SanFrancisco"
    }
    if(grepl("Santa Cruz",x = spots[i], fixed = T)==1){
      spots[i] = "SantaCruz"
    }
  }
  
  colnames(res3) = spots
  res4 = res3
  whole2012 <<- rbind(results3, res4)
}
if(!exists('whole2012')){
  whole2012<<-results3
}




head(whole2012)
# 2011 results --------------------

year = 2011

rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/mct?year=', year, sep='')

rankings = read_html(rankings_url)



# Surfer names:
name_indexes = html_nodes(rankings,'.athlete-name')

names = html_text(name_indexes) 

# table

results = html_table(rankings,header = T)

results = data.frame(results[1])

end = dim(results)[2]

name_col = which(results[1,] == 'Name')

results2 = results[-1,-c(1:(name_col+1),end)]

row.names(results2) = names

for(i in 1:dim(results2)[1]){
  for(j in 1:dim(results2)[2]){
    results2[i,j] = gsub('[PointsThrowaway]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[Points]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[-]','NA', results2[i,j])
    results2[i,j] = gsub('[,]','', results2[i,j])
    results2[i,j] = gsub("[[:space:]]", "", results2[i,j])
    results2[i,j] = gsub("[INJ]", "NA", results2[i,j])
    if(results2[i,j] == '131750'){results2[i,j] = 13}
    else if(results2[i,j] == '25500'){results2[i,j] = 25}
    else if(results2[i,j] == '171750'){results2[i,j] = 17}
    else if(results2[i,j] == 'NA'){}
    else(results2[i,j] = as.numeric(substr(results2[i,j],1,1)))
  }
}

results3 = data.frame(data.matrix(results2))

#  Spot names:

spot_indexes = html_nodes(rankings,'.athlete-event-place strong')

spots = html_text(spot_indexes) 

spots = spots[1:dim(results3)[2]]

for(i in 1:length(spots)){
  if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
    spots[i] = "GoldCoast"
  }
  if(grepl("Margaret River",x = spots[i], fixed = T)==1){
    spots[i] = "MargaretRiver"
  }
  if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
    spots[i] = "BellsBeach"
  }
  if(grepl("Rio",x = spots[i], fixed = T)==1){
    spots[i] = "Rio"
  }
  if(grepl("Fiji",x = spots[i], fixed = T)==1){
    spots[i] = "Fiji"
  }
  if(grepl("J-Bay",x = spots[i], fixed = T)==1){
    spots[i] = "Jbay"
  }
  if(grepl("Tahiti",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("France",x = spots[i], fixed = T)==1){
    spots[i] = "France"
  }
  if(grepl("Portugal",x = spots[i], fixed = T)==1){
    spots[i] = "Portugal"
  }
  if(grepl("Pipe",x = spots[i], fixed = T)==1){
    spots[i] = "Pipeline"
  }
  if(grepl("Bali",x = spots[i], fixed = T)==1){
    spots[i] = "Bali"
  }
  if(grepl("Trestles",x = spots[i], fixed = T)==1){
    spots[i] = "Trestles"
  }
  if(grepl("Catarina",x = spots[i], fixed = T)==1){
    spots[i] = "SantaCatarina"
  }
  if(grepl("New York",x = spots[i], fixed = T)==1){
    spots[i] = "NewYork"
  }
  if(grepl("Search",x = spots[i], fixed = T)==1){
    spots[i] = "SanFrancisco"
  }
  if(grepl("Cruz",x = spots[i], fixed = T)==1){
    spots[i] = "SantaCruz"
  }
}

colnames(results3) = spots

# next exits?

next__ = html_nodes(rankings,'.next a')
if(grepl("Next",html_text(next__))){
  rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/mct?offset=51&sort=rank&year=', year, sep='')
  rankings = read_html(rankings_url)
  # Surfer names:
  name_indexes = html_nodes(rankings,'.athlete-name')
  
  names = html_text(name_indexes) 
  
  # table
  
  res = html_table(rankings,header = T)
  
  res = data.frame(res[1])
  
  dim(res)
  
  end = dim(res)[2]
  
  name_col = which(res[1,] == 'Name')
  
  res2 = res[-1,-c(1:(name_col+1),end)]
  
  res = data.frame(res[1])
  
  row.names(res2) = names
  
  for(i in 1:dim(res2)[1]){
    for(j in 1:dim(res2)[2]){
      res2[i,j] = gsub('[PointsThrowaway]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[Points]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[-]','NA', res2[i,j])
      res2[i,j] = gsub('[,]','', res2[i,j])
      res2[i,j] = gsub("[[:space:]]", "", res2[i,j])
      res2[i,j] = gsub("[INJ]", "NA", res2[i,j])
      if(res2[i,j] == '131750'){res2[i,j] = 13}
      else if(res2[i,j] == '25500'){res2[i,j] = 25}
      else if(res2[i,j] == '171750'){res2[i,j] = 17}
      else if(res2[i,j] == 'NA'){}
      else(res2[i,j] = as.numeric(substr(res2[i,j],1,1)))
    }
  }
  
  res3 = data.frame(data.matrix(res2))
  
  #  Spot names:
  
  spot_indexes = html_nodes(rankings,'.athlete-event-place strong')
  
  spots = html_text(spot_indexes) 
  
  spots = spots[1:dim(res3)[2]]
  
  for(i in 1:length(spots)){
    if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
      spots[i] = "GoldCoast"
    }
    if(grepl("Margaret River",x = spots[i], fixed = T)==1){
      spots[i] = "MargaretRiver"
    }
    if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
      spots[i] = "BellsBeach"
    }
    if(grepl("Rio",x = spots[i], fixed = T)==1){
      spots[i] = "Rio"
    }
    if(grepl("Fiji",x = spots[i], fixed = T)==1){
      spots[i] = "Fiji"
    }
    if(grepl("J-Bay",x = spots[i], fixed = T)==1){
      spots[i] = "Jbay"
    }
    if(grepl("Tahiti",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("France",x = spots[i], fixed = T)==1){
      spots[i] = "France"
    }
    if(grepl("Portugal",x = spots[i], fixed = T)==1){
      spots[i] = "Portugal"
    }
    if(grepl("Pipe",x = spots[i], fixed = T)==1){
      spots[i] = "Pipeline"
    }
    if(grepl("Bali",x = spots[i], fixed = T)==1){
      spots[i] = "Bali"
    }
    if(grepl("Trestles",x = spots[i], fixed = T)==1){
      spots[i] = "Trestles"
    }
    if(grepl("Catarina",x = spots[i], fixed = T)==1){
      spots[i] = "SantaCatarina"
    }
    if(grepl("New York",x = spots[i], fixed = T)==1){
      spots[i] = "NewYork"
    }
    if(grepl("Search",x = spots[i], fixed = T)==1){
      spots[i] = "SanFrancisco"
    }
    if(grepl("Cruz",x = spots[i], fixed = T)==1){
      spots[i] = "SantaCruz"
    }
  }
  
  colnames(res3) = spots
  res4 = res3
  whole2011 <<- rbind(results3, res4)
}
if(!exists('whole2011')){
  whole2011<<-results3
}

# 2010 results --------------------

year = 2010

rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/mct?year=', year, sep='')

rankings = read_html(rankings_url)



# Surfer names:
name_indexes = html_nodes(rankings,'.athlete-name')

names = html_text(name_indexes) 

# table

results = html_table(rankings,header = T)

results = data.frame(results[1])

end = dim(results)[2]

name_col = which(results[1,] == 'Name')

results2 = results[-1,-c(1:(name_col+1),end)]

row.names(results2) = names

for(i in 1:dim(results2)[1]){
  for(j in 1:dim(results2)[2]){
    results2[i,j] = gsub('[PointsThrowaway]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[Points]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[-]','NA', results2[i,j])
    results2[i,j] = gsub('[,]','', results2[i,j])
    results2[i,j] = gsub("[[:space:]]", "", results2[i,j])
    results2[i,j] = gsub("[INJ]", "NA", results2[i,j])
    if(results2[i,j] == '131750'){results2[i,j] = 13}
    else if(results2[i,j] == '25500'){results2[i,j] = 25}
    else if(results2[i,j] == '171750'){results2[i,j] = 17}
    else if(results2[i,j] == 'NA'){}
    else(results2[i,j] = as.numeric(substr(results2[i,j],1,1)))
  }
}

results3 = data.frame(data.matrix(results2))

#  Spot names:

spot_indexes = html_nodes(rankings,'.athlete-event-place strong')

spots = html_text(spot_indexes) 

spots = spots[1:dim(results3)[2]]

for(i in 1:length(spots)){
  if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
    spots[i] = "GoldCoast"
  }
  if(grepl("Margaret River",x = spots[i], fixed = T)==1){
    spots[i] = "MargaretRiver"
  }
  if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
    spots[i] = "BellsBeach"
  }
  if(grepl("Rio",x = spots[i], fixed = T)==1){
    spots[i] = "Rio"
  }
  if(grepl("Fiji",x = spots[i], fixed = T)==1){
    spots[i] = "Fiji"
  }
  if(grepl("J-Bay",x = spots[i], fixed = T)==1){
    spots[i] = "Jbay"
  }
  if(grepl("Tahiti",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("France",x = spots[i], fixed = T)==1){
    spots[i] = "France"
  }
  if(grepl("Portugal",x = spots[i], fixed = T)==1){
    spots[i] = "Portugal"
  }
  if(grepl("Pipe",x = spots[i], fixed = T)==1){
    spots[i] = "Pipeline"
  }
  if(grepl("Bali",x = spots[i], fixed = T)==1){
    spots[i] = "Bali"
  }
  if(grepl("Trestles",x = spots[i], fixed = T)==1){
    spots[i] = "Trestles"
  }
  if(grepl("Catarina",x = spots[i], fixed = T)==1){
    spots[i] = "SantaCatarina"
  }
  if(grepl("New York",x = spots[i], fixed = T)==1){
    spots[i] = "NewYork"
  }
  if(grepl("Search",x = spots[i], fixed = T)==1){
    spots[i] = "SanFrancisco"
  }
}

colnames(results3) = spots

# next exits?

next__ = html_nodes(rankings,'.next a')
if(grepl("Next",html_text(next__))){
  rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/mct?offset=51&sort=rank&year=', year, sep='')
  rankings = read_html(rankings_url)
  # Surfer names:
  name_indexes = html_nodes(rankings,'.athlete-name')
  
  names = html_text(name_indexes) 
  
  # table
  
  res = html_table(rankings,header = T)
  
  res = data.frame(res[1])
  
  dim(res)
  
  end = dim(res)[2]
  
  name_col = which(res[1,] == 'Name')
  
  res2 = res[-1,-c(1:(name_col+1),end)]
  
  res = data.frame(res[1])
  
  row.names(res2) = names
  
  for(i in 1:dim(res2)[1]){
    for(j in 1:dim(res2)[2]){
      res2[i,j] = gsub('[PointsThrowaway]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[Points]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[-]','NA', res2[i,j])
      res2[i,j] = gsub('[,]','', res2[i,j])
      res2[i,j] = gsub("[[:space:]]", "", res2[i,j])
      res2[i,j] = gsub("[INJ]", "NA", res2[i,j])
      if(res2[i,j] == '131750'){res2[i,j] = 13}
      else if(res2[i,j] == '25500'){res2[i,j] = 25}
      else if(res2[i,j] == '171750'){res2[i,j] = 17}
      else if(res2[i,j] == 'NA'){}
      else(res2[i,j] = as.numeric(substr(res2[i,j],1,1)))
    }
  }
  
  res3 = data.frame(data.matrix(res2))
  
  #  Spot names:
  
  spot_indexes = html_nodes(rankings,'.athlete-event-place strong')
  
  spots = html_text(spot_indexes) 
  
  spots = spots[1:dim(res3)[2]]
  
  for(i in 1:length(spots)){
    if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
      spots[i] = "GoldCoast"
    }
    if(grepl("Margaret River",x = spots[i], fixed = T)==1){
      spots[i] = "MargaretRiver"
    }
    if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
      spots[i] = "BellsBeach"
    }
    if(grepl("Rio",x = spots[i], fixed = T)==1){
      spots[i] = "Rio"
    }
    if(grepl("Fiji",x = spots[i], fixed = T)==1){
      spots[i] = "Fiji"
    }
    if(grepl("J-Bay",x = spots[i], fixed = T)==1){
      spots[i] = "Jbay"
    }
    if(grepl("Tahiti",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("France",x = spots[i], fixed = T)==1){
      spots[i] = "France"
    }
    if(grepl("Portugal",x = spots[i], fixed = T)==1){
      spots[i] = "Portugal"
    }
    if(grepl("Pipe",x = spots[i], fixed = T)==1){
      spots[i] = "Pipeline"
    }
    if(grepl("Bali",x = spots[i], fixed = T)==1){
      spots[i] = "Bali"
    }
    if(grepl("Trestles",x = spots[i], fixed = T)==1){
      spots[i] = "Trestles"
    }
    if(grepl("Catarina",x = spots[i], fixed = T)==1){
      spots[i] = "SantaCatarina"
    }
    if(grepl("New York",x = spots[i], fixed = T)==1){
      spots[i] = "NewYork"
    }
    if(grepl("Search",x = spots[i], fixed = T)==1){
      spots[i] = "SanFrancisco"
    }
  }
  
  colnames(res3) = spots
  res4 = res3
  whole2010 <<- rbind(results3, res4)
}
if(!exists('whole2010')){
  whole2010<<-results3
}




head(whole2010)

# Write Files -------------

for(i in 0:7){
  write.csv(x = get(paste('whole201',i,sep='')),file = paste('M_whole201',i,sep=''))
}



# Womens ----------
remove(list=ls(all=TRUE))
# 2017 results --------------------

year = 2017

rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/wct?year=', year, sep='')

rankings = read_html(rankings_url)



# Surfer names:
name_indexes = html_nodes(rankings,'.athlete-name')

names = html_text(name_indexes) 

# table

results = html_table(rankings,header = T)

results = data.frame(results[1])

end = dim(results)[2]

name_col = which(results[1,] == 'Name')

results2 = results[-1,-c(1:(name_col+1),end)]

row.names(results2) = names

for(i in 1:dim(results2)[1]){
  for(j in 1:dim(results2)[2]){
    results2[i,j] = gsub('[PointsThrowaway]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[Points]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[-]','NA', results2[i,j])
    results2[i,j] = gsub('[,]','', results2[i,j])
    results2[i,j] = gsub("[[:space:]]", "", results2[i,j])
    results2[i,j] = gsub("[INJ]", "NA", results2[i,j])
    if(results2[i,j] == '131750'){results2[i,j] = 13}
    else if(results2[i,j] == '25500'){results2[i,j] = 25}
    else if(results2[i,j] == '171750'){results2[i,j] = 17}
    else if(results2[i,j] == 'NA'){}
    else(results2[i,j] = as.numeric(substr(results2[i,j],1,1)))
  }
}

results3 = data.frame(data.matrix(results2))

#  Spot names:

spot_indexes = html_nodes(rankings,'.athlete-event-place strong')

spots = html_text(spot_indexes) 

spots = spots[1:dim(results3)[2]]

for(i in 1:length(spots)){
  if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
    spots[i] = "GoldCoast"
  }
  if(grepl("Margaret River",x = spots[i], fixed = T)==1){
    spots[i] = "MargaretRiver"
  }
  if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
    spots[i] = "BellsBeach"
  }
  if(grepl("Rio",x = spots[i], fixed = T)==1){
    spots[i] = "Rio"
  }
  if(grepl("Fiji",x = spots[i], fixed = T)==1){
    spots[i] = "Fiji"
  }
  if(grepl("J-Bay",x = spots[i], fixed = T)==1){
    spots[i] = "Jbay"
  }
  if(grepl("Tahiti",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("France",x = spots[i], fixed = T)==1){
    spots[i] = "France"
  }
  if(grepl("Portugal",x = spots[i], fixed = T)==1){
    spots[i] = "Portugal"
  }
  if(grepl("Pipe",x = spots[i], fixed = T)==1){
    spots[i] = "Pipeline"
  }
  if(grepl("Bali",x = spots[i], fixed = T)==1){
    spots[i] = "Bali"
  }
  if(grepl("Trestles",x = spots[i], fixed = T)==1){
    spots[i] = "Trestles"
  }
  if(grepl("Catarina",x = spots[i], fixed = T)==1){
    spots[i] = "SantaCatarina"
  }
  if(grepl("New York",x = spots[i], fixed = T)==1){
    spots[i] = "NewYork"
  }
  if(grepl("Search",x = spots[i], fixed = T)==1){
    spots[i] = "SanFrancisco"
  }
  if(grepl("US Open",x = spots[i], fixed = T)==1){
    spots[i] = "USOpen"
  }
  if(grepl("Cascais",x = spots[i], fixed = T)==1){
    spots[i] = "Cascais"
  }
  if(grepl("Maui",x = spots[i], fixed = T)==1){
    spots[i] = "Maui"
  }
  if(grepl("Swatch",x = spots[i], fixed = T)==1){
    spots[i] = "Trestles"
  }
  if(grepl("TSB",x = spots[i], fixed = T)==1){
    spots[i] = "NewZealand"
  }
  if(grepl("Beachley",x = spots[i], fixed = T)==1){
    spots[i] = "Beachley"
  }
  if(grepl("Peru",x = spots[i], fixed = T)==1){
    spots[i] = "Peru"
  }
  if(grepl("World Cup",x = spots[i], fixed = T)==1){
    spots[i] = "Oahu"
  }
}

colnames(results3) = spots

# next exits?

next__ = html_nodes(rankings,'.next a')
if(grepl("Next",html_text(next__))){
  rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/mct?offset=51&sort=rank&year=', year, sep='')
  rankings = read_html(rankings_url)
  # Surfer names:
  name_indexes = html_nodes(rankings,'.athlete-name')
  
  names = html_text(name_indexes) 
  
  # table
  
  res = html_table(rankings,header = T)
  
  res = data.frame(res[1])
  
  dim(res)
  
  end = dim(res)[2]
  
  name_col = which(res[1,] == 'Name')
  
  res2 = res[-1,-c(1:(name_col+1),end)]
  
  res = data.frame(res[1])
  
  row.names(res2) = names
  
  for(i in 1:dim(res2)[1]){
    for(j in 1:dim(res2)[2]){
      res2[i,j] = gsub('[PointsThrowaway]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[Points]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[-]','NA', res2[i,j])
      res2[i,j] = gsub('[,]','', res2[i,j])
      res2[i,j] = gsub("[[:space:]]", "", res2[i,j])
      res2[i,j] = gsub("[INJ]", "NA", res2[i,j])
      if(res2[i,j] == '131750'){res2[i,j] = 13}
      else if(res2[i,j] == '25500'){res2[i,j] = 25}
      else if(res2[i,j] == '171750'){res2[i,j] = 17}
      else if(res2[i,j] == 'NA'){}
      else(res2[i,j] = as.numeric(substr(res2[i,j],1,1)))
    }
  }
  
  res3 = data.frame(data.matrix(res2))
  
  #  Spot names:
  
  spot_indexes = html_nodes(rankings,'.athlete-event-place strong')
  
  spots = html_text(spot_indexes) 
  
  spots = spots[1:dim(res3)[2]]
  
  for(i in 1:length(spots)){
    if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
      spots[i] = "GoldCoast"
    }
    if(grepl("Margaret River",x = spots[i], fixed = T)==1){
      spots[i] = "MargaretRiver"
    }
    if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
      spots[i] = "BellsBeach"
    }
    if(grepl("Rio",x = spots[i], fixed = T)==1){
      spots[i] = "Rio"
    }
    if(grepl("Fiji",x = spots[i], fixed = T)==1){
      spots[i] = "Fiji"
    }
    if(grepl("J-Bay",x = spots[i], fixed = T)==1){
      spots[i] = "Jbay"
    }
    if(grepl("Tahiti",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("France",x = spots[i], fixed = T)==1){
      spots[i] = "France"
    }
    if(grepl("Portugal",x = spots[i], fixed = T)==1){
      spots[i] = "Portugal"
    }
    if(grepl("Pipe",x = spots[i], fixed = T)==1){
      spots[i] = "Pipeline"
    }
    if(grepl("Bali",x = spots[i], fixed = T)==1){
      spots[i] = "Bali"
    }
    if(grepl("Trestles",x = spots[i], fixed = T)==1){
      spots[i] = "Trestles"
    }
    if(grepl("Catarina",x = spots[i], fixed = T)==1){
      spots[i] = "SantaCatarina"
    }
    if(grepl("New York",x = spots[i], fixed = T)==1){
      spots[i] = "NewYork"
    }
    if(grepl("Search",x = spots[i], fixed = T)==1){
      spots[i] = "SanFrancisco"
    }
    if(grepl("US Open",x = spots[i], fixed = T)==1){
      spots[i] = "USOpen"
    }
    if(grepl("Cascais",x = spots[i], fixed = T)==1){
      spots[i] = "Cascais"
    }
    if(grepl("Maui",x = spots[i], fixed = T)==1){
      spots[i] = "Maui"
    }
    if(grepl("Swatch",x = spots[i], fixed = T)==1){
      spots[i] = "Trestles"
    }
    if(grepl("TSB",x = spots[i], fixed = T)==1){
      spots[i] = "NewZealand"
    }
    if(grepl("Beachley",x = spots[i], fixed = T)==1){
      spots[i] = "Beachley"
    }
    if(grepl("Peru",x = spots[i], fixed = T)==1){
      spots[i] = "Peru"
    }
    if(grepl("World Cup",x = spots[i], fixed = T)==1){
      spots[i] = "Oahu"
    }
  }
  
  colnames(res3) = spots
  res4 = res3
  whole2017 <<- rbind(results3, res4)
}
if(!exists('whole2017')){
  whole2017<<-results3
}

# 2016 results --------------------

year = 2016

rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/wct?year=', year, sep='')

rankings = read_html(rankings_url)



# Surfer names:
name_indexes = html_nodes(rankings,'.athlete-name')

names = html_text(name_indexes) 

# table

results = html_table(rankings,header = T)

results = data.frame(results[1])

end = dim(results)[2]

name_col = which(results[1,] == 'Name')

results2 = results[-1,-c(1:(name_col+1),end)]

row.names(results2) = names

for(i in 1:dim(results2)[1]){
  for(j in 1:dim(results2)[2]){
    results2[i,j] = gsub('[PointsThrowaway]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[Points]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[-]','NA', results2[i,j])
    results2[i,j] = gsub('[,]','', results2[i,j])
    results2[i,j] = gsub("[[:space:]]", "", results2[i,j])
    results2[i,j] = gsub("[INJ]", "NA", results2[i,j])
    if(results2[i,j] == '131750'){results2[i,j] = 13}
    else if(results2[i,j] == '25500'){results2[i,j] = 25}
    else if(results2[i,j] == '171750'){results2[i,j] = 17}
    else if(results2[i,j] == 'NA'){}
    else(results2[i,j] = as.numeric(substr(results2[i,j],1,1)))
  }
}

results3 = data.frame(data.matrix(results2))

#  Spot names:

spot_indexes = html_nodes(rankings,'.athlete-event-place strong')

spots = html_text(spot_indexes) 

spots = spots[1:dim(results3)[2]]

for(i in 1:length(spots)){
  if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
    spots[i] = "GoldCoast"
  }
  if(grepl("Margaret River",x = spots[i], fixed = T)==1){
    spots[i] = "MargaretRiver"
  }
  if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
    spots[i] = "BellsBeach"
  }
  if(grepl("Rio",x = spots[i], fixed = T)==1){
    spots[i] = "Rio"
  }
  if(grepl("Fiji",x = spots[i], fixed = T)==1){
    spots[i] = "Fiji"
  }
  if(grepl("J-Bay",x = spots[i], fixed = T)==1){
    spots[i] = "Jbay"
  }
  if(grepl("Tahiti",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("France",x = spots[i], fixed = T)==1){
    spots[i] = "France"
  }
  if(grepl("Portugal",x = spots[i], fixed = T)==1){
    spots[i] = "Portugal"
  }
  if(grepl("Pipe",x = spots[i], fixed = T)==1){
    spots[i] = "Pipeline"
  }
  if(grepl("Bali",x = spots[i], fixed = T)==1){
    spots[i] = "Bali"
  }
  if(grepl("Trestles",x = spots[i], fixed = T)==1){
    spots[i] = "Trestles"
  }
  if(grepl("Catarina",x = spots[i], fixed = T)==1){
    spots[i] = "SantaCatarina"
  }
  if(grepl("New York",x = spots[i], fixed = T)==1){
    spots[i] = "NewYork"
  }
  if(grepl("Search",x = spots[i], fixed = T)==1){
    spots[i] = "SanFrancisco"
  }
  if(grepl("US Open",x = spots[i], fixed = T)==1){
    spots[i] = "USOpen"
  }
  if(grepl("Cascais",x = spots[i], fixed = T)==1){
    spots[i] = "Cascais"
  }
  if(grepl("Maui",x = spots[i], fixed = T)==1){
    spots[i] = "Maui"
  }
  if(grepl("Swatch",x = spots[i], fixed = T)==1){
    spots[i] = "Trestles"
  }
  if(grepl("TSB",x = spots[i], fixed = T)==1){
    spots[i] = "NewZealand"
  }
  if(grepl("Beachley",x = spots[i], fixed = T)==1){
    spots[i] = "Beachley"
  }
  if(grepl("Peru",x = spots[i], fixed = T)==1){
    spots[i] = "Peru"
  }
  if(grepl("World Cup",x = spots[i], fixed = T)==1){
    spots[i] = "Oahu"
  }
}

colnames(results3) = spots

# next exits?

next__ = html_nodes(rankings,'.next a')
if(grepl("Next",html_text(next__))){
  rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/mct?offset=51&sort=rank&year=', year, sep='')
  rankings = read_html(rankings_url)
  # Surfer names:
  name_indexes = html_nodes(rankings,'.athlete-name')
  
  names = html_text(name_indexes) 
  
  # table
  
  res = html_table(rankings,header = T)
  
  res = data.frame(res[1])
  
  dim(res)
  
  end = dim(res)[2]
  
  name_col = which(res[1,] == 'Name')
  
  res2 = res[-1,-c(1:(name_col+1),end)]
  
  res = data.frame(res[1])
  
  row.names(res2) = names
  
  for(i in 1:dim(res2)[1]){
    for(j in 1:dim(res2)[2]){
      res2[i,j] = gsub('[PointsThrowaway]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[Points]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[-]','NA', res2[i,j])
      res2[i,j] = gsub('[,]','', res2[i,j])
      res2[i,j] = gsub("[[:space:]]", "", res2[i,j])
      res2[i,j] = gsub("[INJ]", "NA", res2[i,j])
      if(res2[i,j] == '131750'){res2[i,j] = 13}
      else if(res2[i,j] == '25500'){res2[i,j] = 25}
      else if(res2[i,j] == '171750'){res2[i,j] = 17}
      else if(res2[i,j] == 'NA'){}
      else(res2[i,j] = as.numeric(substr(res2[i,j],1,1)))
    }
  }
  
  res3 = data.frame(data.matrix(res2))
  
  #  Spot names:
  
  spot_indexes = html_nodes(rankings,'.athlete-event-place strong')
  
  spots = html_text(spot_indexes) 
  
  spots = spots[1:dim(res3)[2]]
  
  for(i in 1:length(spots)){
    if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
      spots[i] = "GoldCoast"
    }
    if(grepl("Margaret River",x = spots[i], fixed = T)==1){
      spots[i] = "MargaretRiver"
    }
    if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
      spots[i] = "BellsBeach"
    }
    if(grepl("Rio",x = spots[i], fixed = T)==1){
      spots[i] = "Rio"
    }
    if(grepl("Fiji",x = spots[i], fixed = T)==1){
      spots[i] = "Fiji"
    }
    if(grepl("J-Bay",x = spots[i], fixed = T)==1){
      spots[i] = "Jbay"
    }
    if(grepl("Tahiti",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("France",x = spots[i], fixed = T)==1){
      spots[i] = "France"
    }
    if(grepl("Portugal",x = spots[i], fixed = T)==1){
      spots[i] = "Portugal"
    }
    if(grepl("Pipe",x = spots[i], fixed = T)==1){
      spots[i] = "Pipeline"
    }
    if(grepl("Bali",x = spots[i], fixed = T)==1){
      spots[i] = "Bali"
    }
    if(grepl("Trestles",x = spots[i], fixed = T)==1){
      spots[i] = "Trestles"
    }
    if(grepl("Catarina",x = spots[i], fixed = T)==1){
      spots[i] = "SantaCatarina"
    }
    if(grepl("New York",x = spots[i], fixed = T)==1){
      spots[i] = "NewYork"
    }
    if(grepl("Search",x = spots[i], fixed = T)==1){
      spots[i] = "SanFrancisco"
    }
    if(grepl("US Open",x = spots[i], fixed = T)==1){
      spots[i] = "USOpen"
    }
    if(grepl("Cascais",x = spots[i], fixed = T)==1){
      spots[i] = "Cascais"
    }
    if(grepl("Maui",x = spots[i], fixed = T)==1){
      spots[i] = "Maui"
    }
    if(grepl("Swatch",x = spots[i], fixed = T)==1){
      spots[i] = "Trestles"
    }
    if(grepl("TSB",x = spots[i], fixed = T)==1){
      spots[i] = "NewZealand"
    }
    if(grepl("Beachley",x = spots[i], fixed = T)==1){
      spots[i] = "Beachley"
    }
    if(grepl("Peru",x = spots[i], fixed = T)==1){
      spots[i] = "Peru"
    }
    if(grepl("World Cup",x = spots[i], fixed = T)==1){
      spots[i] = "Oahu"
    }
  }
  
  colnames(res3) = spots
  res4 = res3
  whole2016 <<- rbind(results3, res4)
}
if(!exists('whole2016')){
  whole2016<<-results3
}




head(whole2016)

# 2015 results --------------------

year = 2015

rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/wct?year=', year, sep='')

rankings = read_html(rankings_url)



# Surfer names:
name_indexes = html_nodes(rankings,'.athlete-name')

names = html_text(name_indexes) 

# table

results = html_table(rankings,header = T)

results = data.frame(results[1])

end = dim(results)[2]

name_col = which(results[1,] == 'Name')

results2 = results[-1,-c(1:(name_col+1),end)]

row.names(results2) = names

for(i in 1:dim(results2)[1]){
  for(j in 1:dim(results2)[2]){
    results2[i,j] = gsub('[PointsThrowaway]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[Points]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[-]','NA', results2[i,j])
    results2[i,j] = gsub('[,]','', results2[i,j])
    results2[i,j] = gsub("[[:space:]]", "", results2[i,j])
    results2[i,j] = gsub("[INJ]", "NA", results2[i,j])
    if(results2[i,j] == '131750'){results2[i,j] = 13}
    else if(results2[i,j] == '25500'){results2[i,j] = 25}
    else if(results2[i,j] == '171750'){results2[i,j] = 17}
    else if(results2[i,j] == 'NA'){}
    else(results2[i,j] = as.numeric(substr(results2[i,j],1,1)))
  }
}

results3 = data.frame(data.matrix(results2))

#  Spot names:

spot_indexes = html_nodes(rankings,'.athlete-event-place strong')

spots = html_text(spot_indexes) 

spots = spots[1:dim(results3)[2]]

for(i in 1:length(spots)){
  if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
    spots[i] = "GoldCoast"
  }
  if(grepl("Margaret River",x = spots[i], fixed = T)==1){
    spots[i] = "MargaretRiver"
  }
  if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
    spots[i] = "BellsBeach"
  }
  if(grepl("Rio",x = spots[i], fixed = T)==1){
    spots[i] = "Rio"
  }
  if(grepl("Fiji",x = spots[i], fixed = T)==1){
    spots[i] = "Fiji"
  }
  if(grepl("J-Bay",x = spots[i], fixed = T)==1){
    spots[i] = "Jbay"
  }
  if(grepl("Tahiti",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("France",x = spots[i], fixed = T)==1){
    spots[i] = "France"
  }
  if(grepl("Portugal",x = spots[i], fixed = T)==1){
    spots[i] = "Portugal"
  }
  if(grepl("Pipe",x = spots[i], fixed = T)==1){
    spots[i] = "Pipeline"
  }
  if(grepl("Bali",x = spots[i], fixed = T)==1){
    spots[i] = "Bali"
  }
  if(grepl("Trestles",x = spots[i], fixed = T)==1){
    spots[i] = "Trestles"
  }
  if(grepl("Catarina",x = spots[i], fixed = T)==1){
    spots[i] = "SantaCatarina"
  }
  if(grepl("New York",x = spots[i], fixed = T)==1){
    spots[i] = "NewYork"
  }
  if(grepl("Search",x = spots[i], fixed = T)==1){
    spots[i] = "SanFrancisco"
  }
  if(grepl("US Open",x = spots[i], fixed = T)==1){
    spots[i] = "USOpen"
  }
  if(grepl("Cascais",x = spots[i], fixed = T)==1){
    spots[i] = "Cascais"
  }
  if(grepl("Maui",x = spots[i], fixed = T)==1){
    spots[i] = "Maui"
  }
  if(grepl("Swatch",x = spots[i], fixed = T)==1){
    spots[i] = "Trestles"
  }
  if(grepl("TSB",x = spots[i], fixed = T)==1){
    spots[i] = "NewZealand"
  }
  if(grepl("Beachley",x = spots[i], fixed = T)==1){
    spots[i] = "Beachley"
  }
  if(grepl("Peru",x = spots[i], fixed = T)==1){
    spots[i] = "Peru"
  }
  if(grepl("World Cup",x = spots[i], fixed = T)==1){
    spots[i] = "Oahu"
  }
}

colnames(results3) = spots

# next exits?

next__ = html_nodes(rankings,'.next a')
if(grepl("Next",html_text(next__))){
  rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/mct?offset=51&sort=rank&year=', year, sep='')
  rankings = read_html(rankings_url)
  # Surfer names:
  name_indexes = html_nodes(rankings,'.athlete-name')
  
  names = html_text(name_indexes) 
  
  # table
  
  res = html_table(rankings,header = T)
  
  res = data.frame(res[1])
  
  dim(res)
  
  end = dim(res)[2]
  
  name_col = which(res[1,] == 'Name')
  
  res2 = res[-1,-c(1:(name_col+1),end)]
  
  res = data.frame(res[1])
  
  row.names(res2) = names
  
  for(i in 1:dim(res2)[1]){
    for(j in 1:dim(res2)[2]){
      res2[i,j] = gsub('[PointsThrowaway]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[Points]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[-]','NA', res2[i,j])
      res2[i,j] = gsub('[,]','', res2[i,j])
      res2[i,j] = gsub("[[:space:]]", "", res2[i,j])
      res2[i,j] = gsub("[INJ]", "NA", res2[i,j])
      if(res2[i,j] == '131750'){res2[i,j] = 13}
      else if(res2[i,j] == '25500'){res2[i,j] = 25}
      else if(res2[i,j] == '171750'){res2[i,j] = 17}
      else if(res2[i,j] == 'NA'){}
      else(res2[i,j] = as.numeric(substr(res2[i,j],1,1)))
    }
  }
  
  res3 = data.frame(data.matrix(res2))
  
  #  Spot names:
  
  spot_indexes = html_nodes(rankings,'.athlete-event-place strong')
  
  spots = html_text(spot_indexes) 
  
  spots = spots[1:dim(res3)[2]]
  
  for(i in 1:length(spots)){
    if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
      spots[i] = "GoldCoast"
    }
    if(grepl("Margaret River",x = spots[i], fixed = T)==1){
      spots[i] = "MargaretRiver"
    }
    if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
      spots[i] = "BellsBeach"
    }
    if(grepl("Rio",x = spots[i], fixed = T)==1){
      spots[i] = "Rio"
    }
    if(grepl("Fiji",x = spots[i], fixed = T)==1){
      spots[i] = "Fiji"
    }
    if(grepl("J-Bay",x = spots[i], fixed = T)==1){
      spots[i] = "Jbay"
    }
    if(grepl("Tahiti",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("France",x = spots[i], fixed = T)==1){
      spots[i] = "France"
    }
    if(grepl("Portugal",x = spots[i], fixed = T)==1){
      spots[i] = "Portugal"
    }
    if(grepl("Pipe",x = spots[i], fixed = T)==1){
      spots[i] = "Pipeline"
    }
    if(grepl("Bali",x = spots[i], fixed = T)==1){
      spots[i] = "Bali"
    }
    if(grepl("Trestles",x = spots[i], fixed = T)==1){
      spots[i] = "Trestles"
    }
    if(grepl("Catarina",x = spots[i], fixed = T)==1){
      spots[i] = "SantaCatarina"
    }
    if(grepl("New York",x = spots[i], fixed = T)==1){
      spots[i] = "NewYork"
    }
    if(grepl("Search",x = spots[i], fixed = T)==1){
      spots[i] = "SanFrancisco"
    }
    if(grepl("US Open",x = spots[i], fixed = T)==1){
      spots[i] = "USOpen"
    }
    if(grepl("Cascais",x = spots[i], fixed = T)==1){
      spots[i] = "Cascais"
    }
    if(grepl("Maui",x = spots[i], fixed = T)==1){
      spots[i] = "Maui"
    }
    if(grepl("Swatch",x = spots[i], fixed = T)==1){
      spots[i] = "Trestles"
    }
    if(grepl("TSB",x = spots[i], fixed = T)==1){
      spots[i] = "NewZealand"
    }
    if(grepl("Beachley",x = spots[i], fixed = T)==1){
      spots[i] = "Beachley"
    }
    if(grepl("Peru",x = spots[i], fixed = T)==1){
      spots[i] = "Peru"
    }
    if(grepl("World Cup",x = spots[i], fixed = T)==1){
      spots[i] = "Oahu"
    }
  }
  
  colnames(res3) = spots
  res4 = res3
  whole2015 <<- rbind(results3, res4)
}
if(!exists('whole2015')){
  whole2015<<-results3
}

# 2014 results --------------------

year = 2014

rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/wct?year=', year, sep='')

rankings = read_html(rankings_url)



# Surfer names:
name_indexes = html_nodes(rankings,'.athlete-name')

names = html_text(name_indexes) 

# table

results = html_table(rankings,header = T)

results = data.frame(results[1])

end = dim(results)[2]

name_col = which(results[1,] == 'Name')

results2 = results[-1,-c(1:(name_col+1),end)]

row.names(results2) = names

for(i in 1:dim(results2)[1]){
  for(j in 1:dim(results2)[2]){
    results2[i,j] = gsub('[PointsThrowaway]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[Points]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[-]','NA', results2[i,j])
    results2[i,j] = gsub('[,]','', results2[i,j])
    results2[i,j] = gsub("[[:space:]]", "", results2[i,j])
    results2[i,j] = gsub("[INJ]", "NA", results2[i,j])
    if(results2[i,j] == '131750'){results2[i,j] = 13}
    else if(results2[i,j] == '25500'){results2[i,j] = 25}
    else if(results2[i,j] == '171750'){results2[i,j] = 17}
    else if(results2[i,j] == 'NA'){}
    else(results2[i,j] = as.numeric(substr(results2[i,j],1,1)))
  }
}

results3 = data.frame(data.matrix(results2))

#  Spot names:

spot_indexes = html_nodes(rankings,'.athlete-event-place strong')

spots = html_text(spot_indexes) 

spots = spots[1:dim(results3)[2]]

for(i in 1:length(spots)){
  if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
    spots[i] = "GoldCoast"
  }
  if(grepl("Margaret River",x = spots[i], fixed = T)==1){
    spots[i] = "MargaretRiver"
  }
  if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
    spots[i] = "BellsBeach"
  }
  if(grepl("Rio",x = spots[i], fixed = T)==1){
    spots[i] = "Rio"
  }
  if(grepl("Fiji",x = spots[i], fixed = T)==1){
    spots[i] = "Fiji"
  }
  if(grepl("J-Bay",x = spots[i], fixed = T)==1){
    spots[i] = "Jbay"
  }
  if(grepl("Tahiti",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("France",x = spots[i], fixed = T)==1){
    spots[i] = "France"
  }
  if(grepl("Portugal",x = spots[i], fixed = T)==1){
    spots[i] = "Portugal"
  }
  if(grepl("Pipe",x = spots[i], fixed = T)==1){
    spots[i] = "Pipeline"
  }
  if(grepl("Bali",x = spots[i], fixed = T)==1){
    spots[i] = "Bali"
  }
  if(grepl("Trestles",x = spots[i], fixed = T)==1){
    spots[i] = "Trestles"
  }
  if(grepl("Catarina",x = spots[i], fixed = T)==1){
    spots[i] = "SantaCatarina"
  }
  if(grepl("New York",x = spots[i], fixed = T)==1){
    spots[i] = "NewYork"
  }
  if(grepl("Search",x = spots[i], fixed = T)==1){
    spots[i] = "SanFrancisco"
  }
  if(grepl("US Open",x = spots[i], fixed = T)==1){
    spots[i] = "USOpen"
  }
  if(grepl("Cascais",x = spots[i], fixed = T)==1){
    spots[i] = "Cascais"
  }
  if(grepl("Maui",x = spots[i], fixed = T)==1){
    spots[i] = "Maui"
  }
  if(grepl("Swatch",x = spots[i], fixed = T)==1){
    spots[i] = "Trestles"
  }
  if(grepl("TSB",x = spots[i], fixed = T)==1){
    spots[i] = "NewZealand"
  }
  if(grepl("Beachley",x = spots[i], fixed = T)==1){
    spots[i] = "Beachley"
  }
  if(grepl("Peru",x = spots[i], fixed = T)==1){
    spots[i] = "Peru"
  }
  if(grepl("World Cup",x = spots[i], fixed = T)==1){
    spots[i] = "Oahu"
  }
}

colnames(results3) = spots

# next exits?

next__ = html_nodes(rankings,'.next a')
if(grepl("Next",html_text(next__))){
  rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/mct?offset=51&sort=rank&year=', year, sep='')
  rankings = read_html(rankings_url)
  # Surfer names:
  name_indexes = html_nodes(rankings,'.athlete-name')
  
  names = html_text(name_indexes) 
  
  # table
  
  res = html_table(rankings,header = T)
  
  res = data.frame(res[1])
  
  dim(res)
  
  end = dim(res)[2]
  
  name_col = which(res[1,] == 'Name')
  
  res2 = res[-1,-c(1:(name_col+1),end)]
  
  res = data.frame(res[1])
  
  row.names(res2) = names
  
  for(i in 1:dim(res2)[1]){
    for(j in 1:dim(res2)[2]){
      res2[i,j] = gsub('[PointsThrowaway]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[Points]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[-]','NA', res2[i,j])
      res2[i,j] = gsub('[,]','', res2[i,j])
      res2[i,j] = gsub("[[:space:]]", "", res2[i,j])
      res2[i,j] = gsub("[INJ]", "NA", res2[i,j])
      if(res2[i,j] == '131750'){res2[i,j] = 13}
      else if(res2[i,j] == '25500'){res2[i,j] = 25}
      else if(res2[i,j] == '171750'){res2[i,j] = 17}
      else if(res2[i,j] == 'NA'){}
      else(res2[i,j] = as.numeric(substr(res2[i,j],1,1)))
    }
  }
  
  res3 = data.frame(data.matrix(res2))
  
  #  Spot names:
  
  spot_indexes = html_nodes(rankings,'.athlete-event-place strong')
  
  spots = html_text(spot_indexes) 
  
  spots = spots[1:dim(res3)[2]]
  
  for(i in 1:length(spots)){
    if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
      spots[i] = "GoldCoast"
    }
    if(grepl("Margaret River",x = spots[i], fixed = T)==1){
      spots[i] = "MargaretRiver"
    }
    if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
      spots[i] = "BellsBeach"
    }
    if(grepl("Rio",x = spots[i], fixed = T)==1){
      spots[i] = "Rio"
    }
    if(grepl("Fiji",x = spots[i], fixed = T)==1){
      spots[i] = "Fiji"
    }
    if(grepl("J-Bay",x = spots[i], fixed = T)==1){
      spots[i] = "Jbay"
    }
    if(grepl("Tahiti",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("France",x = spots[i], fixed = T)==1){
      spots[i] = "France"
    }
    if(grepl("Portugal",x = spots[i], fixed = T)==1){
      spots[i] = "Portugal"
    }
    if(grepl("Pipe",x = spots[i], fixed = T)==1){
      spots[i] = "Pipeline"
    }
    if(grepl("Bali",x = spots[i], fixed = T)==1){
      spots[i] = "Bali"
    }
    if(grepl("Trestles",x = spots[i], fixed = T)==1){
      spots[i] = "Trestles"
    }
    if(grepl("Catarina",x = spots[i], fixed = T)==1){
      spots[i] = "SantaCatarina"
    }
    if(grepl("New York",x = spots[i], fixed = T)==1){
      spots[i] = "NewYork"
    }
    if(grepl("Search",x = spots[i], fixed = T)==1){
      spots[i] = "SanFrancisco"
    }
    if(grepl("US Open",x = spots[i], fixed = T)==1){
      spots[i] = "USOpen"
    }
    if(grepl("Cascais",x = spots[i], fixed = T)==1){
      spots[i] = "Cascais"
    }
    if(grepl("Maui",x = spots[i], fixed = T)==1){
      spots[i] = "Maui"
    }
    if(grepl("Swatch",x = spots[i], fixed = T)==1){
      spots[i] = "Trestles"
    }
    if(grepl("TSB",x = spots[i], fixed = T)==1){
      spots[i] = "NewZealand"
    }
    if(grepl("Beachley",x = spots[i], fixed = T)==1){
      spots[i] = "Beachley"
    }
    if(grepl("Peru",x = spots[i], fixed = T)==1){
      spots[i] = "Peru"
    }
    if(grepl("World Cup",x = spots[i], fixed = T)==1){
      spots[i] = "Oahu"
    }
  }
  
  colnames(res3) = spots
  res4 = res3
  whole2014 <<- rbind(results3, res4)
}
if(!exists('whole2014')){
  whole2014<<-results3
}





# 2013 results --------------------

year = 2013

rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/wct?year=', year, sep='')

rankings = read_html(rankings_url)



# Surfer names:
name_indexes = html_nodes(rankings,'.athlete-name')

names = html_text(name_indexes) 

# table

results = html_table(rankings,header = T)

results = data.frame(results[1])

end = dim(results)[2]

name_col = which(results[1,] == 'Name')

results2 = results[-1,-c(1:(name_col+1),end)]

row.names(results2) = names

for(i in 1:dim(results2)[1]){
  for(j in 1:dim(results2)[2]){
    results2[i,j] = gsub('[PointsThrowaway]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[Points]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[-]','NA', results2[i,j])
    results2[i,j] = gsub('[,]','', results2[i,j])
    results2[i,j] = gsub("[[:space:]]", "", results2[i,j])
    results2[i,j] = gsub("[INJ]", "NA", results2[i,j])
    if(results2[i,j] == '131750'){results2[i,j] = 13}
    else if(results2[i,j] == '25500'){results2[i,j] = 25}
    else if(results2[i,j] == '171750'){results2[i,j] = 17}
    else if(results2[i,j] == 'NA'){}
    else(results2[i,j] = as.numeric(substr(results2[i,j],1,1)))
  }
}

results3 = data.frame(data.matrix(results2))

#  Spot names:

spot_indexes = html_nodes(rankings,'.athlete-event-place strong')

spots = html_text(spot_indexes) 

spots = spots[1:dim(results3)[2]]

for(i in 1:length(spots)){
  if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
    spots[i] = "GoldCoast"
  }
  if(grepl("Margaret River",x = spots[i], fixed = T)==1){
    spots[i] = "MargaretRiver"
  }
  if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
    spots[i] = "BellsBeach"
  }
  if(grepl("Rio",x = spots[i], fixed = T)==1){
    spots[i] = "Rio"
  }
  if(grepl("Fiji",x = spots[i], fixed = T)==1){
    spots[i] = "Fiji"
  }
  if(grepl("J-Bay",x = spots[i], fixed = T)==1){
    spots[i] = "Jbay"
  }
  if(grepl("Tahiti",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("France",x = spots[i], fixed = T)==1){
    spots[i] = "France"
  }
  if(grepl("Portugal",x = spots[i], fixed = T)==1){
    spots[i] = "Portugal"
  }
  if(grepl("Pipe",x = spots[i], fixed = T)==1){
    spots[i] = "Pipeline"
  }
  if(grepl("Bali",x = spots[i], fixed = T)==1){
    spots[i] = "Bali"
  }
  if(grepl("Trestles",x = spots[i], fixed = T)==1){
    spots[i] = "Trestles"
  }
  if(grepl("Catarina",x = spots[i], fixed = T)==1){
    spots[i] = "SantaCatarina"
  }
  if(grepl("New York",x = spots[i], fixed = T)==1){
    spots[i] = "NewYork"
  }
  if(grepl("Search",x = spots[i], fixed = T)==1){
    spots[i] = "SanFrancisco"
  }
  if(grepl("US Open",x = spots[i], fixed = T)==1){
    spots[i] = "USOpen"
  }
  if(grepl("Cascais",x = spots[i], fixed = T)==1){
    spots[i] = "Cascais"
  }
  if(grepl("Maui",x = spots[i], fixed = T)==1){
    spots[i] = "Maui"
  }
  if(grepl("Swatch",x = spots[i], fixed = T)==1){
    spots[i] = "Trestles"
  }
  if(grepl("TSB",x = spots[i], fixed = T)==1){
    spots[i] = "NewZealand"
  }
  if(grepl("Beachley",x = spots[i], fixed = T)==1){
    spots[i] = "Beachley"
  }
  if(grepl("Peru",x = spots[i], fixed = T)==1){
    spots[i] = "Peru"
  }
  if(grepl("World Cup",x = spots[i], fixed = T)==1){
    spots[i] = "Oahu"
  }
}

colnames(results3) = spots

# next exits?

next__ = html_nodes(rankings,'.next a')
if(grepl("Next",html_text(next__))){
  rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/mct?offset=51&sort=rank&year=', year, sep='')
  rankings = read_html(rankings_url)
  # Surfer names:
  name_indexes = html_nodes(rankings,'.athlete-name')
  
  names = html_text(name_indexes) 
  
  # table
  
  res = html_table(rankings,header = T)
  
  res = data.frame(res[1])
  
  dim(res)
  
  end = dim(res)[2]
  
  name_col = which(res[1,] == 'Name')
  
  res2 = res[-1,-c(1:(name_col+1),end)]
  
  res = data.frame(res[1])
  
  row.names(res2) = names
  
  for(i in 1:dim(res2)[1]){
    for(j in 1:dim(res2)[2]){
      res2[i,j] = gsub('[PointsThrowaway]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[Points]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[-]','NA', res2[i,j])
      res2[i,j] = gsub('[,]','', res2[i,j])
      res2[i,j] = gsub("[[:space:]]", "", res2[i,j])
      res2[i,j] = gsub("[INJ]", "NA", res2[i,j])
      if(res2[i,j] == '131750'){res2[i,j] = 13}
      else if(res2[i,j] == '25500'){res2[i,j] = 25}
      else if(res2[i,j] == '171750'){res2[i,j] = 17}
      else if(res2[i,j] == 'NA'){}
      else(res2[i,j] = as.numeric(substr(res2[i,j],1,1)))
    }
  }
  
  res3 = data.frame(data.matrix(res2))
  
  #  Spot names:
  
  spot_indexes = html_nodes(rankings,'.athlete-event-place strong')
  
  spots = html_text(spot_indexes) 
  
  spots = spots[1:dim(res3)[2]]
  
  for(i in 1:length(spots)){
    if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
      spots[i] = "GoldCoast"
    }
    if(grepl("Margaret River",x = spots[i], fixed = T)==1){
      spots[i] = "MargaretRiver"
    }
    if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
      spots[i] = "BellsBeach"
    }
    if(grepl("Rio",x = spots[i], fixed = T)==1){
      spots[i] = "Rio"
    }
    if(grepl("Fiji",x = spots[i], fixed = T)==1){
      spots[i] = "Fiji"
    }
    if(grepl("J-Bay",x = spots[i], fixed = T)==1){
      spots[i] = "Jbay"
    }
    if(grepl("Tahiti",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("France",x = spots[i], fixed = T)==1){
      spots[i] = "France"
    }
    if(grepl("Portugal",x = spots[i], fixed = T)==1){
      spots[i] = "Portugal"
    }
    if(grepl("Pipe",x = spots[i], fixed = T)==1){
      spots[i] = "Pipeline"
    }
    if(grepl("Bali",x = spots[i], fixed = T)==1){
      spots[i] = "Bali"
    }
    if(grepl("Trestles",x = spots[i], fixed = T)==1){
      spots[i] = "Trestles"
    }
    if(grepl("Catarina",x = spots[i], fixed = T)==1){
      spots[i] = "SantaCatarina"
    }
    if(grepl("New York",x = spots[i], fixed = T)==1){
      spots[i] = "NewYork"
    }
    if(grepl("Search",x = spots[i], fixed = T)==1){
      spots[i] = "SanFrancisco"
    }
    if(grepl("US Open",x = spots[i], fixed = T)==1){
      spots[i] = "USOpen"
    }
    if(grepl("Cascais",x = spots[i], fixed = T)==1){
      spots[i] = "Cascais"
    }
    if(grepl("Maui",x = spots[i], fixed = T)==1){
      spots[i] = "Maui"
    }
    if(grepl("Swatch",x = spots[i], fixed = T)==1){
      spots[i] = "Trestles"
    }
    if(grepl("TSB",x = spots[i], fixed = T)==1){
      spots[i] = "NewZealand"
    }
    if(grepl("Beachley",x = spots[i], fixed = T)==1){
      spots[i] = "Beachley"
    }
    if(grepl("Peru",x = spots[i], fixed = T)==1){
      spots[i] = "Peru"
    }
    if(grepl("World Cup",x = spots[i], fixed = T)==1){
      spots[i] = "Oahu"
    }
  }
  
  colnames(res3) = spots
  res4 = res3
  whole2013 <<- rbind(results3, res4)
}
if(!exists('whole2013')){
  whole2013<<-results3
}

# 2012 results --------------------

year = 2012

rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/wct?year=', year, sep='')

rankings = read_html(rankings_url)



# Surfer names:
name_indexes = html_nodes(rankings,'.athlete-name')

names = html_text(name_indexes) 

# table

results = html_table(rankings,header = T)

results = data.frame(results[1])

end = dim(results)[2]

name_col = which(results[1,] == 'Name')

results2 = results[-1,-c(1:(name_col+1),end)]

row.names(results2) = names

for(i in 1:dim(results2)[1]){
  for(j in 1:dim(results2)[2]){
    results2[i,j] = gsub('[PointsThrowaway]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[Points]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[-]','NA', results2[i,j])
    results2[i,j] = gsub('[,]','', results2[i,j])
    results2[i,j] = gsub("[[:space:]]", "", results2[i,j])
    results2[i,j] = gsub("[INJ]", "NA", results2[i,j])
    if(results2[i,j] == '131750'){results2[i,j] = 13}
    else if(results2[i,j] == '25500'){results2[i,j] = 25}
    else if(results2[i,j] == '171750'){results2[i,j] = 17}
    else if(results2[i,j] == 'NA'){}
    else(results2[i,j] = as.numeric(substr(results2[i,j],1,1)))
  }
}

results3 = data.frame(data.matrix(results2))

#  Spot names:

spot_indexes = html_nodes(rankings,'.athlete-event-place strong')

spots = html_text(spot_indexes) 

spots = spots[1:dim(results3)[2]]

for(i in 1:length(spots)){
  if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
    spots[i] = "GoldCoast"
  }
  if(grepl("Margaret River",x = spots[i], fixed = T)==1){
    spots[i] = "MargaretRiver"
  }
  if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
    spots[i] = "BellsBeach"
  }
  if(grepl("Rio",x = spots[i], fixed = T)==1){
    spots[i] = "Rio"
  }
  if(grepl("Fiji",x = spots[i], fixed = T)==1){
    spots[i] = "Fiji"
  }
  if(grepl("J-Bay",x = spots[i], fixed = T)==1){
    spots[i] = "Jbay"
  }
  if(grepl("Tahiti",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("France",x = spots[i], fixed = T)==1){
    spots[i] = "France"
  }
  if(grepl("Portugal",x = spots[i], fixed = T)==1){
    spots[i] = "Portugal"
  }
  if(grepl("Pipe",x = spots[i], fixed = T)==1){
    spots[i] = "Pipeline"
  }
  if(grepl("Bali",x = spots[i], fixed = T)==1){
    spots[i] = "Bali"
  }
  if(grepl("Trestles",x = spots[i], fixed = T)==1){
    spots[i] = "Trestles"
  }
  if(grepl("Catarina",x = spots[i], fixed = T)==1){
    spots[i] = "SantaCatarina"
  }
  if(grepl("New York",x = spots[i], fixed = T)==1){
    spots[i] = "NewYork"
  }
  if(grepl("Search",x = spots[i], fixed = T)==1){
    spots[i] = "SanFrancisco"
  }
  if(grepl("Santa Cruz",x = spots[i], fixed = T)==1){
    spots[i] = "SantaCruz"
  }
  if(grepl("US Open",x = spots[i], fixed = T)==1){
    spots[i] = "USOpen"
  }
  if(grepl("Cascais",x = spots[i], fixed = T)==1){
    spots[i] = "Cascais"
  }
  if(grepl("Maui",x = spots[i], fixed = T)==1){
    spots[i] = "Maui"
  }
  if(grepl("Swatch",x = spots[i], fixed = T)==1){
    spots[i] = "Trestles"
  }
  if(grepl("TSB",x = spots[i], fixed = T)==1){
    spots[i] = "NewZealand"
  }
  if(grepl("Beachley",x = spots[i], fixed = T)==1){
    spots[i] = "Beachley"
  }
  if(grepl("Peru",x = spots[i], fixed = T)==1){
    spots[i] = "Peru"
  }
  if(grepl("World Cup",x = spots[i], fixed = T)==1){
    spots[i] = "Oahu"
  }
}

colnames(results3) = spots

# next exits?

next__ = html_nodes(rankings,'.next a')
if(grepl("Next",html_text(next__))){
  rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/mct?offset=51&sort=rank&year=', year, sep='')
  rankings = read_html(rankings_url)
  # Surfer names:
  name_indexes = html_nodes(rankings,'.athlete-name')
  
  names = html_text(name_indexes) 
  
  # table
  
  res = html_table(rankings,header = T)
  
  res = data.frame(res[1])
  
  dim(res)
  
  end = dim(res)[2]
  
  name_col = which(res[1,] == 'Name')
  
  res2 = res[-1,-c(1:(name_col+1),end)]
  
  res = data.frame(res[1])
  
  row.names(res2) = names
  
  for(i in 1:dim(res2)[1]){
    for(j in 1:dim(res2)[2]){
      res2[i,j] = gsub('[PointsThrowaway]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[Points]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[-]','NA', res2[i,j])
      res2[i,j] = gsub('[,]','', res2[i,j])
      res2[i,j] = gsub("[[:space:]]", "", res2[i,j])
      res2[i,j] = gsub("[INJ]", "NA", res2[i,j])
      if(res2[i,j] == '131750'){res2[i,j] = 13}
      else if(res2[i,j] == '25500'){res2[i,j] = 25}
      else if(res2[i,j] == '171750'){res2[i,j] = 17}
      else if(res2[i,j] == 'NA'){}
      else(res2[i,j] = as.numeric(substr(res2[i,j],1,1)))
    }
  }
  
  res3 = data.frame(data.matrix(res2))
  
  #  Spot names:
  
  spot_indexes = html_nodes(rankings,'.athlete-event-place strong')
  
  spots = html_text(spot_indexes) 
  
  spots = spots[1:dim(res3)[2]]
  
  for(i in 1:length(spots)){
    if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
      spots[i] = "GoldCoast"
    }
    if(grepl("Margaret River",x = spots[i], fixed = T)==1){
      spots[i] = "MargaretRiver"
    }
    if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
      spots[i] = "BellsBeach"
    }
    if(grepl("Rio",x = spots[i], fixed = T)==1){
      spots[i] = "Rio"
    }
    if(grepl("Fiji",x = spots[i], fixed = T)==1){
      spots[i] = "Fiji"
    }
    if(grepl("J-Bay",x = spots[i], fixed = T)==1){
      spots[i] = "Jbay"
    }
    if(grepl("Tahiti",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("France",x = spots[i], fixed = T)==1){
      spots[i] = "France"
    }
    if(grepl("Portugal",x = spots[i], fixed = T)==1){
      spots[i] = "Portugal"
    }
    if(grepl("Pipe",x = spots[i], fixed = T)==1){
      spots[i] = "Pipeline"
    }
    if(grepl("Bali",x = spots[i], fixed = T)==1){
      spots[i] = "Bali"
    }
    if(grepl("Trestles",x = spots[i], fixed = T)==1){
      spots[i] = "Trestles"
    }
    if(grepl("Catarina",x = spots[i], fixed = T)==1){
      spots[i] = "SantaCatarina"
    }
    if(grepl("New York",x = spots[i], fixed = T)==1){
      spots[i] = "NewYork"
    }
    if(grepl("Search",x = spots[i], fixed = T)==1){
      spots[i] = "SanFrancisco"
    }
    if(grepl("Santa Cruz",x = spots[i], fixed = T)==1){
      spots[i] = "SantaCruz"
    }
    if(grepl("US Open",x = spots[i], fixed = T)==1){
      spots[i] = "USOpen"
    }
    if(grepl("Cascais",x = spots[i], fixed = T)==1){
      spots[i] = "Cascais"
    }
    if(grepl("Maui",x = spots[i], fixed = T)==1){
      spots[i] = "Maui"
    }
    if(grepl("Swatch",x = spots[i], fixed = T)==1){
      spots[i] = "Trestles"
    }
    if(grepl("TSB",x = spots[i], fixed = T)==1){
      spots[i] = "NewZealand"
    }
    if(grepl("Beachley",x = spots[i], fixed = T)==1){
      spots[i] = "Beachley"
    }
    if(grepl("Peru",x = spots[i], fixed = T)==1){
      spots[i] = "Peru"
    }
    if(grepl("World Cup",x = spots[i], fixed = T)==1){
      spots[i] = "Oahu"
    }
  }
  
  colnames(res3) = spots
  res4 = res3
  whole2012 <<- rbind(results3, res4)
}
if(!exists('whole2012')){
  whole2012<<-results3
}




head(whole2012)
# 2011 results --------------------

year = 2011

rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/wct?year=', year, sep='')

rankings = read_html(rankings_url)



# Surfer names:
name_indexes = html_nodes(rankings,'.athlete-name')

names = html_text(name_indexes) 

# table

results = html_table(rankings,header = T)

results = data.frame(results[1])

end = dim(results)[2]

name_col = which(results[1,] == 'Name')

results2 = results[-1,-c(1:(name_col+1),end)]

row.names(results2) = names

for(i in 1:dim(results2)[1]){
  for(j in 1:dim(results2)[2]){
    results2[i,j] = gsub('[PointsThrowaway]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[Points]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[-]','NA', results2[i,j])
    results2[i,j] = gsub('[,]','', results2[i,j])
    results2[i,j] = gsub("[[:space:]]", "", results2[i,j])
    results2[i,j] = gsub("[INJ]", "NA", results2[i,j])
    if(results2[i,j] == '131750'){results2[i,j] = 13}
    else if(results2[i,j] == '25500'){results2[i,j] = 25}
    else if(results2[i,j] == '171750'){results2[i,j] = 17}
    else if(results2[i,j] == 'NA'){}
    else(results2[i,j] = as.numeric(substr(results2[i,j],1,1)))
  }
}

results3 = data.frame(data.matrix(results2))

#  Spot names:

spot_indexes = html_nodes(rankings,'.athlete-event-place strong')

spots = html_text(spot_indexes) 

spots = spots[1:dim(results3)[2]]

for(i in 1:length(spots)){
  if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
    spots[i] = "GoldCoast"
  }
  if(grepl("Margaret River",x = spots[i], fixed = T)==1){
    spots[i] = "MargaretRiver"
  }
  if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
    spots[i] = "BellsBeach"
  }
  if(grepl("Rio",x = spots[i], fixed = T)==1){
    spots[i] = "Rio"
  }
  if(grepl("Fiji",x = spots[i], fixed = T)==1){
    spots[i] = "Fiji"
  }
  if(grepl("J-Bay",x = spots[i], fixed = T)==1){
    spots[i] = "Jbay"
  }
  if(grepl("Tahiti",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("France",x = spots[i], fixed = T)==1){
    spots[i] = "France"
  }
  if(grepl("Portugal",x = spots[i], fixed = T)==1){
    spots[i] = "Portugal"
  }
  if(grepl("Pipe",x = spots[i], fixed = T)==1){
    spots[i] = "Pipeline"
  }
  if(grepl("Bali",x = spots[i], fixed = T)==1){
    spots[i] = "Bali"
  }
  if(grepl("Trestles",x = spots[i], fixed = T)==1){
    spots[i] = "Trestles"
  }
  if(grepl("Catarina",x = spots[i], fixed = T)==1){
    spots[i] = "SantaCatarina"
  }
  if(grepl("New York",x = spots[i], fixed = T)==1){
    spots[i] = "NewYork"
  }
  if(grepl("Search",x = spots[i], fixed = T)==1){
    spots[i] = "SanFrancisco"
  }
  if(grepl("Cruz",x = spots[i], fixed = T)==1){
    spots[i] = "SantaCruz"
  }
  if(grepl("US Open",x = spots[i], fixed = T)==1){
    spots[i] = "USOpen"
  }
  if(grepl("Cascais",x = spots[i], fixed = T)==1){
    spots[i] = "Cascais"
  }
  if(grepl("Maui",x = spots[i], fixed = T)==1){
    spots[i] = "Maui"
  }
  if(grepl("Swatch",x = spots[i], fixed = T)==1){
    spots[i] = "Trestles"
  }
  if(grepl("TSB",x = spots[i], fixed = T)==1){
    spots[i] = "NewZealand"
  }
  if(grepl("Beachley",x = spots[i], fixed = T)==1){
    spots[i] = "Beachley"
  }
  if(grepl("Peru",x = spots[i], fixed = T)==1){
    spots[i] = "Peru"
  }
  if(grepl("World Cup",x = spots[i], fixed = T)==1){
    spots[i] = "Oahu"
  }
}

colnames(results3) = spots

# next exits?

next__ = html_nodes(rankings,'.next a')
if(grepl("Next",html_text(next__))){
  rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/mct?offset=51&sort=rank&year=', year, sep='')
  rankings = read_html(rankings_url)
  # Surfer names:
  name_indexes = html_nodes(rankings,'.athlete-name')
  
  names = html_text(name_indexes) 
  
  # table
  
  res = html_table(rankings,header = T)
  
  res = data.frame(res[1])
  
  dim(res)
  
  end = dim(res)[2]
  
  name_col = which(res[1,] == 'Name')
  
  res2 = res[-1,-c(1:(name_col+1),end)]
  
  res = data.frame(res[1])
  
  row.names(res2) = names
  
  for(i in 1:dim(res2)[1]){
    for(j in 1:dim(res2)[2]){
      res2[i,j] = gsub('[PointsThrowaway]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[Points]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[-]','NA', res2[i,j])
      res2[i,j] = gsub('[,]','', res2[i,j])
      res2[i,j] = gsub("[[:space:]]", "", res2[i,j])
      res2[i,j] = gsub("[INJ]", "NA", res2[i,j])
      if(res2[i,j] == '131750'){res2[i,j] = 13}
      else if(res2[i,j] == '25500'){res2[i,j] = 25}
      else if(res2[i,j] == '171750'){res2[i,j] = 17}
      else if(res2[i,j] == 'NA'){}
      else(res2[i,j] = as.numeric(substr(res2[i,j],1,1)))
    }
  }
  
  res3 = data.frame(data.matrix(res2))
  
  #  Spot names:
  
  spot_indexes = html_nodes(rankings,'.athlete-event-place strong')
  
  spots = html_text(spot_indexes) 
  
  spots = spots[1:dim(res3)[2]]
  
  for(i in 1:length(spots)){
    if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
      spots[i] = "GoldCoast"
    }
    if(grepl("Margaret River",x = spots[i], fixed = T)==1){
      spots[i] = "MargaretRiver"
    }
    if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
      spots[i] = "BellsBeach"
    }
    if(grepl("Rio",x = spots[i], fixed = T)==1){
      spots[i] = "Rio"
    }
    if(grepl("Fiji",x = spots[i], fixed = T)==1){
      spots[i] = "Fiji"
    }
    if(grepl("J-Bay",x = spots[i], fixed = T)==1){
      spots[i] = "Jbay"
    }
    if(grepl("Tahiti",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("France",x = spots[i], fixed = T)==1){
      spots[i] = "France"
    }
    if(grepl("Portugal",x = spots[i], fixed = T)==1){
      spots[i] = "Portugal"
    }
    if(grepl("Pipe",x = spots[i], fixed = T)==1){
      spots[i] = "Pipeline"
    }
    if(grepl("Bali",x = spots[i], fixed = T)==1){
      spots[i] = "Bali"
    }
    if(grepl("Trestles",x = spots[i], fixed = T)==1){
      spots[i] = "Trestles"
    }
    if(grepl("Catarina",x = spots[i], fixed = T)==1){
      spots[i] = "SantaCatarina"
    }
    if(grepl("New York",x = spots[i], fixed = T)==1){
      spots[i] = "NewYork"
    }
    if(grepl("Search",x = spots[i], fixed = T)==1){
      spots[i] = "SanFrancisco"
    }
    if(grepl("Cruz",x = spots[i], fixed = T)==1){
      spots[i] = "SantaCruz"
    }
    if(grepl("US Open",x = spots[i], fixed = T)==1){
      spots[i] = "USOpen"
    }
    if(grepl("Cascais",x = spots[i], fixed = T)==1){
      spots[i] = "Cascais"
    }
    if(grepl("Maui",x = spots[i], fixed = T)==1){
      spots[i] = "Maui"
    }
    if(grepl("Swatch",x = spots[i], fixed = T)==1){
      spots[i] = "Trestles"
    }
    if(grepl("TSB",x = spots[i], fixed = T)==1){
      spots[i] = "NewZealand"
    }
    if(grepl("Beachley",x = spots[i], fixed = T)==1){
      spots[i] = "Beachley"
    }
    if(grepl("Peru",x = spots[i], fixed = T)==1){
      spots[i] = "Peru"
    }
    if(grepl("World Cup",x = spots[i], fixed = T)==1){
      spots[i] = "Oahu"
    }
  }
  
  colnames(res3) = spots
  res4 = res3
  whole2011 <<- rbind(results3, res4)
}
if(!exists('whole2011')){
  whole2011<<-results3
}

# 2010 results --------------------

year = 2010

rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/wct?year=', year, sep='')

rankings = read_html(rankings_url)



# Surfer names:
name_indexes = html_nodes(rankings,'.athlete-name')

names = html_text(name_indexes) 

# table

results = html_table(rankings,header = T)

results = data.frame(results[1])

end = dim(results)[2]

name_col = which(results[1,] == 'Name')

results2 = results[-1,-c(1:(name_col+1),end)]

row.names(results2) = names

for(i in 1:dim(results2)[1]){
  for(j in 1:dim(results2)[2]){
    results2[i,j] = gsub('[PointsThrowaway]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[Points]','', results2[i,j],fixed = F)
    results2[i,j] = gsub('[-]','NA', results2[i,j])
    results2[i,j] = gsub('[,]','', results2[i,j])
    results2[i,j] = gsub("[[:space:]]", "", results2[i,j])
    results2[i,j] = gsub("[INJ]", "NA", results2[i,j])
    if(results2[i,j] == '131750'){results2[i,j] = 13}
    else if(results2[i,j] == '25500'){results2[i,j] = 25}
    else if(results2[i,j] == '171750'){results2[i,j] = 17}
    else if(results2[i,j] == 'NA'){}
    else(results2[i,j] = as.numeric(substr(results2[i,j],1,1)))
  }
}

results3 = data.frame(data.matrix(results2))

#  Spot names:

spot_indexes = html_nodes(rankings,'.athlete-event-place strong')

spots = html_text(spot_indexes) 

spots = spots[1:dim(results3)[2]]

for(i in 1:length(spots)){
  if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
    spots[i] = "GoldCoast"
  }
  if(grepl("Margaret River",x = spots[i], fixed = T)==1){
    spots[i] = "MargaretRiver"
  }
  if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
    spots[i] = "BellsBeach"
  }
  if(grepl("Rio",x = spots[i], fixed = T)==1){
    spots[i] = "Rio"
  }
  if(grepl("Fiji",x = spots[i], fixed = T)==1){
    spots[i] = "Fiji"
  }
  if(grepl("J-Bay",x = spots[i], fixed = T)==1){
    spots[i] = "Jbay"
  }
  if(grepl("Tahiti",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
    spots[i] = "Tahiti"
  }
  if(grepl("France",x = spots[i], fixed = T)==1){
    spots[i] = "France"
  }
  if(grepl("Portugal",x = spots[i], fixed = T)==1){
    spots[i] = "Portugal"
  }
  if(grepl("Pipe",x = spots[i], fixed = T)==1){
    spots[i] = "Pipeline"
  }
  if(grepl("Bali",x = spots[i], fixed = T)==1){
    spots[i] = "Bali"
  }
  if(grepl("Trestles",x = spots[i], fixed = T)==1){
    spots[i] = "Trestles"
  }
  if(grepl("Catarina",x = spots[i], fixed = T)==1){
    spots[i] = "SantaCatarina"
  }
  if(grepl("New York",x = spots[i], fixed = T)==1){
    spots[i] = "NewYork"
  }
  if(grepl("Search",x = spots[i], fixed = T)==1){
    spots[i] = "SanFrancisco"
  }
  if(grepl("US Open",x = spots[i], fixed = T)==1){
    spots[i] = "USOpen"
  }
  if(grepl("Cascais",x = spots[i], fixed = T)==1){
    spots[i] = "Cascais"
  }
  if(grepl("Maui",x = spots[i], fixed = T)==1){
    spots[i] = "Maui"
  }
  if(grepl("Swatch",x = spots[i], fixed = T)==1){
    spots[i] = "Trestles"
  }
  if(grepl("TSB",x = spots[i], fixed = T)==1){
    spots[i] = "NewZealand"
  }
  if(grepl("Beachley",x = spots[i], fixed = T)==1){
    spots[i] = "Beachley"
  }
  if(grepl("Peru",x = spots[i], fixed = T)==1){
    spots[i] = "Peru"
  }
  if(grepl("World Cup",x = spots[i], fixed = T)==1){
    spots[i] = "Oahu"
  }
}

colnames(results3) = spots

# next exits?

next__ = html_nodes(rankings,'.next a')
if(grepl("Next",html_text(next__))){
  rankings_url = paste('http://www.worldsurfleague.com/athletes/tour/mct?offset=51&sort=rank&year=', year, sep='')
  rankings = read_html(rankings_url)
  # Surfer names:
  name_indexes = html_nodes(rankings,'.athlete-name')
  
  names = html_text(name_indexes) 
  
  # table
  
  res = html_table(rankings,header = T)
  
  res = data.frame(res[1])
  
  dim(res)
  
  end = dim(res)[2]
  
  name_col = which(res[1,] == 'Name')
  
  res2 = res[-1,-c(1:(name_col+1),end)]
  
  res = data.frame(res[1])
  
  row.names(res2) = names
  
  for(i in 1:dim(res2)[1]){
    for(j in 1:dim(res2)[2]){
      res2[i,j] = gsub('[PointsThrowaway]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[Points]','', res2[i,j],fixed = F)
      res2[i,j] = gsub('[-]','NA', res2[i,j])
      res2[i,j] = gsub('[,]','', res2[i,j])
      res2[i,j] = gsub("[[:space:]]", "", res2[i,j])
      res2[i,j] = gsub("[INJ]", "NA", res2[i,j])
      if(res2[i,j] == '131750'){res2[i,j] = 13}
      else if(res2[i,j] == '25500'){res2[i,j] = 25}
      else if(res2[i,j] == '171750'){res2[i,j] = 17}
      else if(res2[i,j] == 'NA'){}
      else(res2[i,j] = as.numeric(substr(res2[i,j],1,1)))
    }
  }
  
  res3 = data.frame(data.matrix(res2))
  
  #  Spot names:
  
  spot_indexes = html_nodes(rankings,'.athlete-event-place strong')
  
  spots = html_text(spot_indexes) 
  
  spots = spots[1:dim(res3)[2]]
  
  for(i in 1:length(spots)){
    if(grepl("Gold Coast",x = spots[i], fixed = T)==1){
      spots[i] = "GoldCoast"
    }
    if(grepl("Margaret River",x = spots[i], fixed = T)==1){
      spots[i] = "MargaretRiver"
    }
    if(grepl("Bells Beach",x = spots[i], fixed = T)==1){
      spots[i] = "BellsBeach"
    }
    if(grepl("Rio",x = spots[i], fixed = T)==1){
      spots[i] = "Rio"
    }
    if(grepl("Fiji",x = spots[i], fixed = T)==1){
      spots[i] = "Fiji"
    }
    if(grepl("J-Bay",x = spots[i], fixed = T)==1){
      spots[i] = "Jbay"
    }
    if(grepl("Tahiti",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("Teahupoo",x = spots[i], fixed = T)==1){
      spots[i] = "Tahiti"
    }
    if(grepl("France",x = spots[i], fixed = T)==1){
      spots[i] = "France"
    }
    if(grepl("Portugal",x = spots[i], fixed = T)==1){
      spots[i] = "Portugal"
    }
    if(grepl("Pipe",x = spots[i], fixed = T)==1){
      spots[i] = "Pipeline"
    }
    if(grepl("Bali",x = spots[i], fixed = T)==1){
      spots[i] = "Bali"
    }
    if(grepl("Trestles",x = spots[i], fixed = T)==1){
      spots[i] = "Trestles"
    }
    if(grepl("Catarina",x = spots[i], fixed = T)==1){
      spots[i] = "SantaCatarina"
    }
    if(grepl("New York",x = spots[i], fixed = T)==1){
      spots[i] = "NewYork"
    }
    if(grepl("Search",x = spots[i], fixed = T)==1){
      spots[i] = "SanFrancisco"
    }
    if(grepl("US Open",x = spots[i], fixed = T)==1){
      spots[i] = "USOpen"
    }
    if(grepl("Cascais",x = spots[i], fixed = T)==1){
      spots[i] = "Cascais"
    }
    if(grepl("Maui",x = spots[i], fixed = T)==1){
      spots[i] = "Maui"
    }
    if(grepl("Swatch",x = spots[i], fixed = T)==1){
      spots[i] = "Trestles"
    }
    if(grepl("TSB",x = spots[i], fixed = T)==1){
      spots[i] = "NewZealand"
    }
    if(grepl("Beachley",x = spots[i], fixed = T)==1){
      spots[i] = "Beachley"
    }
    if(grepl("Peru",x = spots[i], fixed = T)==1){
      spots[i] = "Peru"
    }
    if(grepl("World Cup",x = spots[i], fixed = T)==1){
      spots[i] = "Oahu"
    }
  }
  
  colnames(res3) = spots
  res4 = res3
  whole2010 <<- rbind(results3, res4)
}
if(!exists('whole2010')){
  whole2010<<-results3
}






# Write Files -------------

for(i in 0:7){
  write.csv(x = get(paste('whole201',i,sep='')),file = paste('F_whole201',i,sep=''))
}



