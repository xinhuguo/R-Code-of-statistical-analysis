##install.packages("UpSetR")
library(UpSetR)#R 3.5
otu_RA <- read.table('otu_RA.txt', header = TRUE, row.names = 1, sep = '\t')
otu_RA[otu_RA > 0] <- 1
p <- upset(otu_RA, nset = 7, nintersects = 10, order.by = c('degree','freq'), decreasing = c(TRUE,TRUE),
           mb.ratio = c(0.7, 0.3),
           point.size = 2,
           line.size = 1, 
           mainbar.y.label = "Intersection size", 
           sets.x.label = "Set Size", 
           main.bar.color = "#2a83a2", sets.bar.color = "#3b7960",
           queries = list(list(query = intersects, 
                               params = list("BS","RS","RE","VE","SE","LE","P"), 
                               active = T,color="#d66a35", 
                               query.name = "BS vs RS vs RE vs VE vs SE vs LE vs P")))
p
select_otu <- rownames(otu_RA[rowSums(otu_RA[1:7]) == 7, ])
otu_select <- otu_RA[select_otu, ]
write.table(as.matrix(otu_select),"otu_overlap.txt",sep = '\t',quote = FALSE,col.names = NA)
dev.off()
