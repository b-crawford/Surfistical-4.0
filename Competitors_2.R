remove(list=ls(all=TRUE))

# Mens -------

source('wd.R')
setwd(paste(wd,'/Mens',sep=''))
whole2010 = read.csv('whole2010.csv', row.names = 1)
whole2011 = read.csv('whole2011.csv', row.names = 1)
whole2012 = read.csv('whole2012.csv', row.names = 1)
whole2013 = read.csv('whole2013.csv', row.names = 1)
whole2014 = read.csv('whole2014.csv', row.names = 1)
whole2015 = read.csv('whole2015.csv', row.names = 1)
whole2016 = read.csv('whole2016.csv', row.names = 1)
whole2017 = read.csv('whole2017.csv', row.names = 1)
setwd(wd)

for(year in 2010:2017){

  data_set = get(paste('whole', year, sep=''))
  
for(i in 1:dim(data_set)[2]){
    col = matrix(data_set[,i], nrow= dim(data_set)[1],ncol=1)
    rownames(col)=rownames(data_set)
    col = data.frame(col)
    col1= col[! is.na(col[,1]), 1, drop = F]
    setwd(paste(wd,'/Competitors_2/Mens',sep=''))
    write.csv(col1,paste(paste(colnames(data_set)[i],year,sep=''),'.csv',sep= ''))
    setwd(wd)
  }
  
}


# Womens -----------
remove(list=ls(all=TRUE))


source('wd.R')
setwd(paste(wd,'/Womens',sep=''))
whole2010 = read.csv('whole2010.csv', row.names = 1)
whole2011 = read.csv('whole2011.csv', row.names = 1)
whole2012 = read.csv('whole2012.csv', row.names = 1)
whole2013 = read.csv('whole2013.csv', row.names = 1)
whole2014 = read.csv('whole2014.csv', row.names = 1)
whole2015 = read.csv('whole2015.csv', row.names = 1)
whole2016 = read.csv('whole2016.csv', row.names = 1)
whole2017 = read.csv('whole2017.csv', row.names = 1)
setwd(wd)

for(year in 2010:2017){
  
  data_set = get(paste('whole', year, sep=''))
  
  for(i in 1:dim(data_set)[2]){
    col = matrix(data_set[,i], nrow= dim(data_set)[1],ncol=1)
    rownames(col)=rownames(data_set)
    col = data.frame(col)
    col1= col[! is.na(col[,1]), 1, drop = F]
    setwd(paste(wd,'/Competitors_2/Womens',sep=''))
    write.csv(col1,paste(paste(colnames(data_set)[i],year,sep=''),'.csv',sep= ''))
    setwd(wd)
  }
}

