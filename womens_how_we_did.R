library(stats)
library(stats)
library(ggplot2)
remove(list=ls(all=TRUE))
source('wd.R')
source('Competitors_2.R')
setwd(wd)


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


# just ended event --------

source('recently_ended_F.R')

year = as.numeric(format(as.Date(Sys.time(), format="%d/%m/%Y"),"%Y"))



# Spot characterstic lists  ----------
spots = c()
for(i in 0:7){
  data_set = get(paste('whole201',i,sep = ''))
  for(j in 1:dim(data_set)[2]){
    spots[100*i + j] = colnames(data_set)[j]
  }
}
spots = spots[! is.na(spots)]
spots = unique(spots[! spots %in% c('X')])
spots

left = c('Fiji')
right = c('GoldCoast','BellsBeach','MargaretRiver','Trestles','Maui')
both = c('Rio','France','Portugal','NewZealand','Peru','Oahu','USOpen','Cascais')
unknown_direction = c('Beachley','Search')
length(spots) - length(left) - length(right) - length(both) - length(unknown_direction)
reef = c('MargaretRiver','Fiji')
point = c('GoldCoast','BellsBeach','Trestles','Maui')
beach = c('Rio','Trestles','France','Portugal','Beachley','Peru','Oahu','USOpen','Cascais')
unknown_type = c('NewZealand','Search')
length(spots) - length(reef) - length(point) - length(beach) - length(unknown_type) # trestles in two




# parameter vector ------

source('find_top_parameters_W.R')
p1


# processing ----



how_we_did = function(year, spot, parameters1, density_v1){
  
  
# year = 2017
# spot = 'BellsBeach'
# parameters1 = p1[1:10]
# density_v1 = p1[11:12]
  
  
  setwd(paste(wd,'/Competitors_2/Womens',sep=''))
  competitors_vector = rownames(read.csv(paste(paste(spot,year,sep=''),'.csv',sep=''), row.names = 1, stringsAsFactors = F))
  actual = read.csv(paste(paste(spot,year,sep=''),'.csv',sep=''), row.names = 1, stringsAsFactors = F)
  setwd(wd)
  
  # data -------
  
  year_0 = get(paste('whole',year, sep=''))
  year_1 = get(paste('whole',year-1, sep=''))
  year_2 = get(paste('whole',year-2, sep=''))
  year_3 = get(paste('whole',year-3, sep=''))
  year_4 = get(paste('whole',year-4, sep=''))
  
  which_cols = c(0,0,0,0,1,1,1,1,0,0)
  
  
  
  
  # only keep competitors
  
  year_0 = year_0[rownames(year_0) %in% competitors_vector,]
  year_1 = year_1[rownames(year_1) %in% competitors_vector,]
  year_2 = year_2[rownames(year_2) %in% competitors_vector,]
  year_3 = year_3[rownames(year_3) %in% competitors_vector,]
  year_4 = year_1[rownames(year_4) %in% competitors_vector,]
  
  # previous years
  
  if(spot %in% colnames(year_1)){
    col_1 = matrix(year_1[,spot]/25, nrow = dim(year_1)[1], ncol = 1)
    rownames(col_1) = rownames(year_1)
    which_cols[1] = 1
  }
  
  if(spot %in% colnames(year_2)){
    col_2 = matrix(year_2[,spot]/25, nrow = dim(year_2)[1], ncol = 1)
    rownames(col_2) = rownames(year_2)
    which_cols[2] = 1
  }
  
  if(spot %in% colnames(year_3)){
    col_3 = matrix(year_3[,spot]/25, nrow = dim(year_3)[1], ncol = 1)
    rownames(col_3) = rownames(year_3)
    which_cols[3] = 1
  }
  
  if(spot %in% colnames(year_4)){
    col_4 = matrix(year_4[,spot]/25, nrow = dim(year_4)[1], ncol = 1)
    rownames(col_4) = rownames(year_4)
    which_cols[4] = 1
  }
  
  
  # Characteristic
  
  spots1= c()
  spots2= c()
  spots3= c()
  spots4= c()
  spots5= c()
  spots6= c()
  
  if(spot %in% left){
    spots1 = left
  }
  if(spot %in% right){
    spots2 = right
  }
  if(spot %in% both){
    spots3 = both
  }
  if(spot %in% reef){
    spots4 = reef
  }
  if(spot %in% point){
    spots5 = point
  }
  if(spot %in% beach){
    spots6 = beach
  }
  
  relevant_spots = c(spots1,spots2,spots3,spots4,spots5,spots6)
  relevant_spots2 = data.frame(ftable(relevant_spots))
  relevant_spots3 = unique(relevant_spots)
  
  new_year_1 = year_1[relevant_spots3[relevant_spots3 %in% colnames(year_1)]]
  new_year_2 = year_2[relevant_spots3[relevant_spots3 %in% colnames(year_2)]]
  new_year_3 = year_3[relevant_spots3[relevant_spots3 %in% colnames(year_3)]]
  new_year_4 = year_4[relevant_spots3[relevant_spots3 %in% colnames(year_4)]]
  
  
  for(i in 1:dim(relevant_spots2)[1]){
    if(paste(relevant_spots2[i,1]) %in% colnames(new_year_1)){
      new_year_1[,paste(relevant_spots2[i,1])] = new_year_1[,paste(relevant_spots2[i,1])] * relevant_spots2[i,2]
    }
    if(paste(relevant_spots2[i,1]) %in% colnames(new_year_2)){
      new_year_2[,paste(relevant_spots2[i,1])] = new_year_2[,paste(relevant_spots2[i,1])] * relevant_spots2[i,2]
    }
    if(paste(relevant_spots2[i,1]) %in% colnames(new_year_3)){
      new_year_3[,paste(relevant_spots2[i,1])] = new_year_3[,paste(relevant_spots2[i,1])] * relevant_spots2[i,2]
    }
    if(paste(relevant_spots2[i,1]) %in% colnames(new_year_4)){
      new_year_4[,paste(relevant_spots2[i,1])] = new_year_4[,paste(relevant_spots2[i,1])] * relevant_spots2[i,2]
    }
  }
  
  col_5 = matrix(rowMeans(new_year_1, na.rm = T)/50, nrow = dim(new_year_1)[1], ncol = 1)
  rownames(col_5) = rownames(new_year_1)
  
  col_6 = matrix(rowMeans(new_year_2, na.rm = T)/50, nrow = dim(new_year_2)[1], ncol = 1)
  rownames(col_6) = rownames(new_year_2)
  
  col_7 = matrix(rowMeans(new_year_3, na.rm = T)/50, nrow = dim(new_year_3)[1], ncol = 1)
  rownames(col_7) = rownames(new_year_3)
  
  col_8 = matrix(rowMeans(new_year_4, na.rm = T)/50, nrow = dim(new_year_4)[1], ncol = 1)
  rownames(col_8) = rownames(new_year_4)
  
  
  # Form Scores:
  
  spot_index = grep(spot, colnames(year_0))
  
  if(spot_index == 2){
    col_9 = matrix(year_0[,1]/25, nrow = dim(year_0)[1], ncol=1)
    rownames(col_9) = rownames(year_0)
    which_cols[9] = 1
  }
  
  if(spot_index > 2){
    col_9 = matrix(year_0[,spot_index-1]/25, nrow = dim(year_0)[1], ncol=1)
    rownames(col_9) = rownames(year_0)
    which_cols[9] = 1
    col_10 = matrix(year_0[,spot_index-2]/25, nrow = dim(year_0)[1], ncol=1)
    rownames(col_10) = rownames(year_0)
    which_cols[10] = 1
  }
  
  # combining -------
  
  merge_cols = matrix(nrow = 1,ncol=1)
  rownames(merge_cols) = c('empty')
  
  
  for(i in 1:10){
    if(which_cols[i] == 1){
      col_use = get(paste('col_',i,sep=''))
      colnames(col_use) = paste('col_',i,sep='')
      merge_cols = merge(merge_cols,col_use,by = 'row.names',all.x = T, all.y = T)
      rownames(merge_cols) = merge_cols[,1]
      merge_cols = merge_cols[,-1]
    }
  }
  
  merge_cols = merge_cols[,-1]
  merge_cols = merge_cols[-which(rownames(merge_cols) == 'empty'),]
  
  
  parameter_vector1 = parameters1 * which_cols
  parameter_vector1 = parameter_vector1[! parameter_vector1 %in% c(0)]
  parameter_vector1 = parameter_vector1/ sum(parameter_vector1)
  
  data_density = matrix(nrow = dim(merge_cols)[1], ncol = 1)
  rownames(data_density) = rownames(merge_cols)
  
  for(i in 1:dim(merge_cols)[1]){
    merge_cols[i,] = merge_cols[i,] * parameter_vector1
    data_density[i,] = sum(! is.na(merge_cols[i,]))
  }
  
  predict = matrix(rowMeans(merge_cols, na.rm = T), nrow = dim(merge_cols)[1], ncol = 1)
  rownames(predict) = rownames(merge_cols)
  
  predict = predict + (density_v1[1] <= 4)*density_v1[2]
  predict = data.frame(predict)
  predict = predict[! rownames(predict) == 'NA' , , drop = F]
  
  compare = merge(actual,predict,by = 'row.names',all.x = T)
  rownames(compare) = compare[,1]
  compare = compare[,-1]
  
  compare = compare[order(compare[,2]), ,drop = F]
  compare
  
  compare[1,2] = 1
  compare[2,2] = 2
  compare[3:4,2] = 3
  compare[5:8,2] = 5
  compare[9:12,2] = 9
  compare[13:24,2] = 13
  compare[25:dim(compare)[1],2] = 25
  
  title = paste(gsub('([[:upper:]])', ' \\1', spot), year)
  
  
  example = data.frame(x = compare[,2],y = compare[,1])
  
  setwd(paste(webwd,'/Womens-past',sep=''))
  png(filename = paste(spot,year,sep=''),width = 1400,height = 700, res = 200 )
  ggplot(example, aes(x, y)) +
    geom_count(col = hcl(h = 180, l = 65, c = 100))+ 
    scale_size_continuous(range = c(1, 10),  breaks= c(1,2,5,10), name="Number of \nSurfers")+
    geom_smooth(method='lm',formula=y~x, se  = F,col = hcl(h = 200 , l = 65, c = 100))+
    geom_segment(aes(x = 1, y = 1, xend = 25, yend = 25), data = NULL, col = "darkgrey",lty=2)+
    labs(x = "Predicted Result", y = "Actual Result", title =title)+
    scale_y_continuous(breaks = c(0,5,15,20,25),limits = c(0,26))
  dev.off()
  
  setwd(paste(wd,'/Womens-old-predictions',sep=''))
  colnames(example) = c('Prediction', 'Actual')
  write.csv(example,paste(paste(spot,year,sep=''),'.csv',sep = ''))
  
  setwd(wd)
  
}


if(womens_analyse_time == 1 ){
  how_we_did(year,finished,parameters1 = p1[1:10],density_v1 = p1[11:12])
}






  
