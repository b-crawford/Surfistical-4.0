table2 

name = colnames(table2)[1]
strsplit(name, ' ')[[1]][1] == 'USOpen'


change_title = function(name){
  if(strsplit(name, ' ')[[1]][1] == 'USOpen'){
    return(paste('US Open',strsplit(name, ' ')[[1]][2], sep = ' '))
  }
  else(return(name))
}

tab = table2
# change_format = function(tab){
  colnames(tab)[1] = change_title(colnames(tab)[1])
  tab[,1:2] = tab[,2:1]
  colnames(tab)[1] = paste(colnames(tab)[1:2], collapse = ' - ')
  colnames(tab)[2] = ''
  
  one = c(tab[tab[,1] == '1',2 ],'','','')
  two = c(tab[tab[,1] == '2',2 ],'','','')
  three = c(tab[tab[,1] == '3',2 ],'','')
  five = tab[tab[,1] == '5',2 ]
  nine = tab[tab[,1] == '9',2 ]
  
  thirteen1 = tab[tab[,1] == '13',2 ]
  thirteen = replicate(12,'')
  
  for(i in 1:length(thirteen1)){
    thirteen[i] = thirteen1[i]
  }
  
  twenty_five1 = tab[tab[,1] == '25',2 ]
  if(length(twenty_five1)){
    twenty_five = replicate(12,'')
    for(i in 1:length(twenty_five1)){
      twenty_five[i] = twenty_five1[i]
    }
  }
  
  
  
  
  

