xx <- readLines("BH2K.log")

emptylines <- which(xx=="")
header <- emptylines[c(diff(emptylines)==2,F)] + 1

start1 <- emptylines[2] - 1
end1 <- emptylines[3] - 1

indices1 <- setdiff(start1:end1,emptylines)

start2 <- emptylines[5] - 1
indices2 <- setdiff(start2:length(xx),emptylines)

d1 <- read.table(text = xx[indices1],fill = T,header=T)
d2 <- read.table(text = xx[indices2],fill = T,header=T)
