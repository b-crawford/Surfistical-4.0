library(stats)
remove(list=ls(all=TRUE))
source('wd.R')
source('Competitors_2.R')
setwd(wd)


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


# just ended event --------

source('recently_ended_M.R')

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

left = c('Fiji', 'Tahiti','Pipeline')
right = c('GoldCoast','BellsBeach','MargaretRiver','Jbay','Trestles','Bali')
both = c('Rio','France','Portugal','SantaCruz','SanFrancisco')
unknown_direction = c('SantaCatarina', 'NewYork', 'Isabela')
length(spots) - length(left) - length(right) - length(both) - length(unknown_direction)
reef = c('MargaretRiver','Fiji','Tahiti','Pipeline','Bali')
point = c('GoldCoast','BellsBeach','Jbay','Trestles')
beach = c('SanFrancisco','Rio','Trestles','France','Portugal','SantaCruz')
unknown_type = c('SantaCatarina', 'NewYork', 'Isabela')
length(spots) - length(reef) - length(point) - length(beach) - length(unknown_type) # trestles in two

heights = read.csv('Mean-Height.csv', row.names = 1)


# parameter vector ------

source('find_top_parameters.R')
p1


# processing ----



how_we_did = function(year, spot, parameters1, density_v1){
  
  
# year = 2017
# spot = 'Fiji'
# parameters1 = p1[1:10]
# density_v1 = p1[11:12]
  
  
  setwd(paste(wd,'/Competitors_2/Mens',sep=''))
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
  
  setwd(paste(webwd,'/Mens-past',sep=''))
  png(filename = paste(spot,year,sep=''),width = 1200,height = 600 )
  par(mar=c(6,6,6,6))
  plot(compare[,2],compare[,1], bty = 'l' , pch = 20, xlab = 'Prediction', ylab = 'Actual Result'
       , main = title,cex.lab =3, cex = 2,cex.main =3, cex.axis = 2)
  abline(lm(compare[,1]~compare[,2]), lty = 1, col = 'black', lwd =2)
  abline(0,1, lty = 2, col = 'red', lwd = 2)
  dev.off()
  setwd(wd)
  
}


if(mens_analyse_time == 1 ){
  how_we_did(year,finished,parameters1 = p1[1:10],density_v1 = p1[11:12])
}






  

eg1 = c()
eg1[1] = 1
eg1[2] = 2
eg1[3:4] = 3
eg1[5:8] = 5
eg1[9:12] = 9
eg1[13:24] = 13
eg1[25:36] = 25
eg2 = eg1
eg2[1] = 2
eg2[2] = 1
eg2[5] = 9
eg2[8] = 25
eg2[5] = 13
eg2[30] = 9
eg2[31] = 13
eg2[14] = 5
eg2[15] = 25




setwd(paste(webwd,'/Mens-past',sep=''))
png(filename = 'Example',width = 1200,height = 600 )
par(mar=c(6,6,6,6))
plot(eg1,eg2, bty = 'l' , pch = 20, xlab = 'Prediction', ylab = 'Actual Result'
     , main = 'Example',cex.lab =3, cex = 2,cex.main =3, cex.axis = 2)
abline(lm(eg2~eg1), lty = 1, col = 'black', lwd =2)
abline(0,1, lty = 2, col = 'red', lwd = 2)
dev.off()
setwd(wd)
  

