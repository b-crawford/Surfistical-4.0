remove(list=ls(all=TRUE))

M_whole2010 = read.csv('M_whole2010', row.names = 1)
M_whole2011 = read.csv('M_whole2011', row.names = 1)
M_whole2012 = read.csv('M_whole2012', row.names = 1)
M_whole2013 = read.csv('M_whole2013', row.names = 1)
M_whole2014 = read.csv('M_whole2014', row.names = 1)
M_whole2015 = read.csv('M_whole2015', row.names = 1)
M_whole2016 = read.csv('M_whole2016', row.names = 1)
M_whole2017 = read.csv('M_whole2017', row.names = 1)


# Spot characterstic lists  ----------
spots = c()
for(i in 0:7){
  data_set = get(paste('M_whole201',i,sep = ''))
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
unknown_direction = c('SantaCatarina', 'NewYork')
length(spots) - length(left) - length(right) - length(both) - length(unknown_direction)
reef = c('MargaretRiver','Fiji','Tahiti','Pipeline','Bali')
point = c('GoldCoast','BellsBeach','Jbay','Trestles','France','Portugal','SantaCruz')
beach = c('SanFrancisco','Rio','Trestles')
unknown_type = c('SantaCatarina', 'NewYork')
length(spots) - length(reef) - length(point) - length(beach) - length(unknown_type) # trestles in two

heights = read.csv('Mean-Height.csv', row.names = 1)


# add function for year and spot --------

year = 2017

spot = 'Fiji'

# previous years -------

year_1 = get(paste('M_whole',year-1, sep=''))
year_2 = get(paste('M_whole',year-2, sep=''))
year_3 = get(paste('M_whole',year-3, sep=''))
year_4 = get(paste('M_whole',year-4, sep=''))

which_years = c(0,0,0,0)

if(spot %in% colnames(year_1)){
  col_1 = matrix(year_1[,spot]/25, nrow = dim(year_1)[1], ncol = 1)
  rownames(col_1) = rownames(year_1)
  which_years[1] = 1
}

if(spot %in% colnames(year_2)){
  col_2 = matrix(year_2[,spot]/25, nrow = dim(year_2)[1], ncol = 1)
  rownames(col_2) = rownames(year_2)
  which_years[2] = 1
}

if(spot %in% colnames(year_3)){
  col_3 = matrix(year_3[,spot]/25, nrow = dim(year_3)[1], ncol = 1)
  rownames(col_3) = rownames(year_3)
  which_years[3] = 1
}

if(spot %in% colnames(year_4)){
  col_4 = matrix(year_4[,spot]/25, nrow = dim(year_4)[1], ncol = 1)
  rownames(col_4) = rownames(year_4)
  which_years[4] = 1
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




