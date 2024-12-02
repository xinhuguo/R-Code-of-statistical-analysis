rm(list = ls())#清楚环境变量
#install.packages(circlize)
library(circlize)#引用包
library(openxlsx)#引用包
phylum <- read.xlsx("phylum.xlsx",1,rowNames = TRUE)
#计算整理前十菌门相对丰度表
phylum$rowsum <- apply(phylum,1,sum)
phylum <- phylum[order (phylum$rowsum,decreasing=TRUE),]#对菌门进行降序排序
phylum = phylum[,-13]#删除求和列
#求物种相对丰度
df <- data.frame(apply(phylum,2,function(x) x/sum(x)),stringsAsFactors = FALSE)
#按行求指定列平均值，并且把算好的平均值添加data1数据框
df$A <- apply(df[,1:3], 1, mean) 
df$B <- apply(df[,4:6], 1, mean)
df$C <- apply(df[,7:9], 1, mean)
df$D <- apply(df[,10:12], 1, mean)
df = df[,-(1:12)]
#之前已经按照每行的和进行过升序排列，所以可以直接取前10行
df1<- df[1:10,]/apply(df,2,sum)
df2 <- 1-apply(df1, 2, sum) #计算剩下物种的总丰度
#合并数据
df3 <- rbind(df1,df2)
rownames(df3)[11] = "Others"
data1 = as.matrix(df3)
#进行和弦图基本绘制
?chordDiagram
chordDiagram(data1)
circos.clear()#清理当前对和弦图的参数设置
rownames(data1)
##预设links配色
col = c("#8DD3C7" ,"#BEBADA" ,'#009933',"#FF7F00",'#c5c5f2','#CC3366',
       "#FB8072","#F781BF" ,"#80B1D3","#A65628" ,'#660099')
grid.col = c(A = '#EF9A9A', B = '#90CAF9',C ='#F3D32C', D ="#FCCDE5",
            Acidobacteriota ="#BEBADA", Actinobacteriota ="#8DD3C7" , Proteobacteria= '#009933',
            Chloroflexi ="#FF7F00" ,Bacteroidota= '#c5c5f2',Firmicutes= '#CC3366',
            Gemmatimonadota = "#FB8072",Verrucomicrobiota ="#F781BF", Patescibacteria = "#80B1D3",
             Planctomycetota ="#A65628" ,Others= '#660099')##预设置外圈颜色


chordDiagram(data1, grid.col = grid.col, 
             column.col = col, directional = -1)##directional=1或-1 可在vector与links之间再加一层扇形 

legend.col = c(Acidobacteriota ="#BEBADA", Actinobacteriota ="#8DD3C7" , Proteobacteria= '#009933',
               Chloroflexi ="#FF7F00" ,Bacteroidota= '#c5c5f2',Firmicutes= '#CC3366',
               Gemmatimonadota = "#FB8072",Verrucomicrobiota ="#F781BF", Patescibacteria = "#80B1D3",
               Planctomycetota ="#A65628" ,Others= '#660099')
legend <- legend(x=1,y=1,###位置 
                 title="Phylum ",title.adj=0 ,  ##标题名字与位置，0为左对齐1为右对齐
                 bty='n',    ###n为不加外边框，o为添加外边框
                 c("Acidobacteriota","Actinobacteriota","Proteobacteria", 
                   "Chloroflexi","Bacteroidota","Firmicutes",  
                   "Gemmatimonadota","Verrucomicrobiota",
                   "Patescibacteria","Planctomycetota","Others"),   ###图例子名称
                 pch=c(16),    ###点形状
                 col=legend.col,   ##点配色
                 cex=1.2,pt.cex=1.2,    ##子名称大小、点大小
                 ncol = 1,xpd=T) ###一列，xpd表示图例是否可以超过图本身边界绘制，与画布par(mar=c())同用。

