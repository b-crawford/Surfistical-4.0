setwd('/Users/Billy/Surfistical4.0/Mens-old-predictions')
files = list.files(pattern="*.csv")
results1 = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))[,-1]

setwd('/Users/Billy/Surfistical4.0/Womens-old-predictions')
files = list.files(pattern="*.csv")
results2 = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))[,-1]


results = rbind(results1,results2)

View(results)

summary(lm(results[,2]~results[,1]))
        