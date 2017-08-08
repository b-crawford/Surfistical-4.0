change_title = function(name){
  if(strsplit(name, ' ')[[1]][1] == 'USOpen'){
    return('U.S. Open')
  }
  else(return(gsub('([[:upper:]])', ' \\1', name)))
}