find_top = read.csv('results.csv', header=T)[,-1]

find_top = find_top[! is.na(find_top[,13]),]

find_top = find_top[order(find_top[,13]), ]

head(find_top)

tail(find_top)

p1 = c()

for(i in 1:12){
  p1[i] = mean(find_top[1:10,i], na.rm=T)
}
