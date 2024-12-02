
data <- read.csv("otu_taxon.csv", header = TRUE, row.names = 1)
mdata <- t(data)

#也可以过滤一下数据，如将asv丰度低于0.01的过滤掉
nasv <- mdata[,colSums(mdata)/sum(mdata)>=(0.01/100)]

write.csv(nasv, file = "otu_R_5.csv", row.names = TRUE)

