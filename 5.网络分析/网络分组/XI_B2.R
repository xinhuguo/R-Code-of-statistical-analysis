#install.packages("WGCNA")
library(WGCNA)
library(igraph)
library(openxlsx)
library(ggplot2)
#install.packages("BiocManager")
#BiocManager::install(c("GO.db", "GOstats", "AnnotationDbi"))
#BiocManager::install('impute')
#BiocManager::install('preprocessCore')

library(readxl)
# 读取数据文件
OTU <- read_excel("otu_zj.xlsx",sheet="B2")
# 提取"straw"列中值为"S"的行
#straw_data <- OTU[OTU$tillage == "NT", ]
# 选择前四列作为样本处理信息
sample_info <- OTU[, 1:2]
# 选择后面的列作为测定数据
data <- OTU[, 3:ncol(OTU)]
MM_cor <- corAndPvalue(data,method ="spearman")

#阈值筛选
r <- MM_cor$cor
p <- MM_cor$p
#选取显著性 p 值小于 0.05 的相关系数，即 p<0.05
p <- p.adjust(p, method = 'fdr')    #可选 p 值校正，这里使用 BH 法校正 p 值
r[p>0.05|abs(r)<0.65] = 0
diag(r) <- 0    #将相关矩阵中对角线中的值（代表了自相关）转为 0
#构建含权的无向网络，权重代表了asv间的 spearman 相关系数
g <- graph.adjacency(r, weighted = TRUE, mode = 'undirected')
#去除自相关
g <- simplify(g)
#删除的孤立节点（即度为 0 的节点）
g <- delete.vertices(g, names(degree(g)[degree(g) == 0]))

#将相关系数复制一列并取其绝对值为权重
E(g)$correlation <- E(g)$weight
E(g)$weight <- abs(E(g)$weight)

#简单绘制网络图
plot(g)

#导出成graphml 格式，使用 gephi 软件打开并进行可视化编辑
#write.graph(g, 'net.graphml', format = 'graphml')
#install.packages("microeco")
library(microeco)

#利用microeco包识别网络枢纽节点和模块枢纽节点
data(dataset)#构建一个microeco网络
t1 <- trans_network$new(dataset = dataset, cal_cor = "WGCNA",
                        filter_thres = 0.01, cor_method = "spearman")#过滤数据并计算R

t1$res_network <- g#将网络映射到microeco中
#先要计算网络属性和网络模块
t1$cal_network_attr()
t1$cal_module()

#提取各节点的拓扑属性，包括 zi 和 pi 值等
t1$get_node_table(node_roles = TRUE)
net_top<- t1$res_node_table
net_top#这里可以查看枢纽节点数据
#可输出节点枢纽数据，以备在Gephi中进一步绘制图形
#write.xlsx(net_top, 'net_top.xlsx')
#可以绘制一个图看看
zipi <- t1$plot_taxa_roles(use_type = 1)
zipi <- zipi +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.ticks.x = element_line(color = "black"),
        axis.ticks.y = element_line(color = "black"),
        axis.ticks.length = unit(-0.15, "cm"),
        axis.text.x = element_text(face = "bold", color = "black", size = rel(1.7)),
        axis.text.y = element_text(face = "bold", color = "black", size = rel(1.7)),
        axis.title.x = element_blank(),  # 删除X轴标题
        axis.title.y = element_blank(),  # 删除Y轴标题
        axis.title = element_text(face = "bold", size = rel(1.5)),
        legend.title = element_text(face = "bold", size = rel(1.5)),
        legend.text = element_text(face = "bold", size = rel(1.5)),
        legend.position = "none",
        plot.margin = margin(10, 10, 10, 10),
        text = element_text(family = "serif", size = 14)) +
  geom_vline(xintercept = 0.62, linetype = "dashed", color = "black", alpha = 0.8) +
  geom_hline(yintercept = 2.5, linetype = "dashed", color = "black", alpha = 0.8)+
  coord_cartesian(xlim = c(0.00, 1), ylim = c(-2.0, 4))
zipi
ggsave("NS_zipi.pdf", zipi, width = 4.75, height = 4.75, dpi = 300)



