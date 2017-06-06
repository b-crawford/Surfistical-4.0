
library(rvest)



# Mens ------

  remove(list=ls(all=TRUE))

  wd = getwd()
  
  source('spot_names.R')

  for(year in 2010:2017){
    
      if(exists('complete')){
        remove(complete)
      }
    
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
  
      
      for(j in 1:dim(results2)[2]){
        for(i in 1:dim(results2)[1]){
          results2[i,j] = gsub('[PointsThrowaway]','', results2[i,j],fixed = F)
          results2[i,j] = gsub('[Points]','', results2[i,j],fixed = F)
          results2[i,j] = gsub('[-]','NA', results2[i,j])
          results2[i,j] = gsub('[-0]','', results2[i,j], fixed =T)
          results2[i,j] = gsub('[,]','', results2[i,j])
          results2[i,j] = gsub("[[:space:]]", "", results2[i,j])
          if(grepl("INJ",results2[i,j], fixed = T)==1){results2[i,j] = 'NA'}
          else if(results2[i,j] == '131750'){results2[i,j] = 13}
          else if(results2[i,j] == '25500'){results2[i,j] = 25}
          else if(results2[i,j] == '171750'){results2[i,j] = 17}
          else if(results2[i,j] == 'NA'){}
          else{results2[i,j] = as.numeric(substr(results2[i,j],1,1))}
        }
      }
      
      
      results3 = data.frame(results2)
      
      #  Spot names:
      
      spot_indexes = html_nodes(rankings,'.athlete-event-place strong')
      
      spots = html_text(spot_indexes) 
      
      spots = spots[1:dim(results3)[2]]
      
      spots = spot_names(spots)
      
      colnames(results3) = spots
    
      # next exits?
      
      next_1 = html_nodes(rankings,'.next a')
      next_2 = html_text(next_1)
      
      if(length(next_2)){
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
        
        for(j in 1:dim(res2)[2]){ 
          for(i in 1:dim(res2)[1]){
            res2[i,j] = gsub('[PointsThrowaway]','', res2[i,j],fixed = F)
            res2[i,j] = gsub('[Points]','', res2[i,j],fixed = F)
            res2[i,j] = gsub('[-]','NA', res2[i,j])
            res2[i,j] = gsub('[-0]','', res2[i,j], fixed = T)
            res2[i,j] = gsub('[,]','', res2[i,j])
            res2[i,j] = gsub("[[:space:]]", "", res2[i,j])
            if(grepl("INJ",res2[i,j], fixed = T)==1){res2[i,j] = 'NA'}
            else if(res2[i,j] == '131750'){res2[i,j] = 13}
            else if(res2[i,j] == '25500'){res2[i,j] = 25}
            else if(res2[i,j] == '171750'){res2[i,j] = 17}
            else if(res2[i,j] == 'NA'){}
            else{res2[i,j] = as.numeric(substr(res2[i,j],1,1))}
          }
        }
        
        res3 = data.frame(res2)
        
        #  Spot names:
        
        spot_indexes = html_nodes(rankings,'.athlete-event-place strong')
        
        spots = html_text(spot_indexes) 
        
        spots = spots[1:dim(res3)[2]]
        
        spots = spot_names(spots)
        
        colnames(res3) = spots
        res4 = res3
        complete =  rbind(results3, res4)
        
        if(year == 2010){
          colnames(complete)[9] = 'SanFrancisco'
        }
        if(year == 2011){
          colnames(complete)[10] = 'Isabela'
        }

        setwd(paste(wd,'/Mens',sep=''))
        write.csv(complete,file = paste(paste('whole',year,sep=''),'.csv', sep = ''))
        setwd(wd)
      }
      if(!exists('complete')){
        
        complete = results3
        
        if(year == 2010){
          colnames(complete)[9] = 'SanFrancisco'
        }
        if(year == 2011){
          colnames(complete)[10] = 'Isabela'
        }
        
        setwd(paste(wd,'/Mens',sep=''))
        write.csv(complete,file = paste(paste('whole',year,sep=''),'.csv', sep = ''))
        setwd(wd)
      }
      print(paste(year, 'Mens completed.'))

}


# Womens ------

remove(list=ls(all=TRUE))
  
wd = getwd()

source('spot_names.R')

for(year in 2010:2017){
  
  if(exists('complete')){
    remove(complete)
  }
  
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
  
  results2[40,]
  
  for(j in 1:dim(results2)[2]){
    for(i in 1:dim(results2)[1]){
      results2[i,j] = gsub('[PointsThrowaway]','', results2[i,j],fixed = F)
      results2[i,j] = gsub('[Points]','', results2[i,j],fixed = F)
      results2[i,j] = gsub('[-]','NA', results2[i,j])
      results2[i,j] = gsub('[-0]','', results2[i,j], fixed =T)
      results2[i,j] = gsub('[,]','', results2[i,j])
      results2[i,j] = gsub("[[:space:]]", "", results2[i,j])
      if(grepl("INJ",results2[i,j], fixed = T)==1){results2[i,j] = 'NA'}
      else if(results2[i,j] == '131750'){results2[i,j] = 13}
      else if(results2[i,j] == '25500'){results2[i,j] = 25}
      else if(results2[i,j] == '171750'){results2[i,j] = 17}
      else if(results2[i,j] == 'NA'){}
      else{results2[i,j] = as.numeric(substr(results2[i,j],1,1))}
    }
  }
  
  
  results3 = data.frame(results2)
  
  #  Spot names:
  
  spot_indexes = html_nodes(rankings,'.athlete-event-place strong')
  
  spots = html_text(spot_indexes) 
  
  spots = spots[1:dim(results3)[2]]
  
  spots = spot_names(spots)
  
  colnames(results3) = spots
  
  # next exits?
  
  next_1 = html_nodes(rankings,'.next a')
  next_2 = html_text(next_1)
  
  if(length(next_2)){
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
    
    for(j in 1:dim(res2)[2]){ 
      for(i in 1:dim(res2)[1]){
        res2[i,j] = gsub('[PointsThrowaway]','', res2[i,j],fixed = F)
        res2[i,j] = gsub('[Points]','', res2[i,j],fixed = F)
        res2[i,j] = gsub('[-]','NA', res2[i,j])
        res2[i,j] = gsub('[-0]','', res2[i,j], fixed = T)
        res2[i,j] = gsub('[,]','', res2[i,j])
        res2[i,j] = gsub("[[:space:]]", "", res2[i,j])
        if(grepl("INJ",res2[i,j], fixed = T)==1){res2[i,j] = 'NA'}
        else if(res2[i,j] == '131750'){res2[i,j] = 13}
        else if(res2[i,j] == '25500'){res2[i,j] = 25}
        else if(res2[i,j] == '171750'){res2[i,j] = 17}
        else if(res2[i,j] == 'NA'){}
        else{res2[i,j] = as.numeric(substr(res2[i,j],1,1))}
      }
    }
    
    res3 = data.frame(res2)
    
    #  Spot names:
    
    spot_indexes = html_nodes(rankings,'.athlete-event-place strong')
    
    spots = html_text(spot_indexes) 
    
    spots = spots[1:dim(res3)[2]]
    
    spots = spot_names(spots)
    
    colnames(res3) = spots
    res4 = res3
    complete =  rbind(results3, res4)
    
    setwd(paste(wd,'/Womens',sep=''))
    write.csv(complete,file = paste(paste('whole',year,sep=''),'.csv', sep = ''))
    setwd(wd)
  }
  if(!exists('complete')){
    
    complete = results3

    setwd(paste(wd,'/Womens',sep=''))
    write.csv(complete,file = paste(paste('whole',year,sep=''),'.csv', sep = ''))
    setwd(wd)
  }
  print(paste(year, 'Womens completed.'))
  
}

