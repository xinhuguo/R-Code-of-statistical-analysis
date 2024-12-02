#install.packages("devtools")
#library(devtools)
#install_github("microbiota/amplicon")
library(ggplot2)
library(amplicon)
library(readxl)
library(openxlsx)
node_design<- read_excel("degree.xlsx",sheet="Degree")
Treat=factor(node_design$Treatment,levels = c("B2","B3"))#设置组
#制作Degree箱线图
p0 <- ggplot(node_design, aes(x=Treat, y=Degree, fill=Treat,color=Treat)) + 
  geom_boxplot(width=0.5,outlier.size = 0.01,alpha=0.5)+#基础图
  theme_minimal() +
  scale_color_manual(values = c("red","blue")) +
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
        text = element_text(family = "serif", size = 14))
p1 <- p0+geom_jitter(width = 0.2,aes(color=Treat),size=0.5,alpha=0.5)+#加上散点
  scale_fill_manual(values=c("white","white"))#箱体填充白色
p1
ggsave("degree_XJ.pdf", p1, width = 4.75, height = 4.75, dpi = 300)
#=================================秸秆
node_design<- read_excel("degree.xlsx",sheet="Between")
Treat=factor(node_design$Treatment,levels = c("B2","B3"))#设置组
#制作Degree箱线图
p2 <- ggplot(node_design, aes(x=Treat, y=betweenesscentrality, fill=Treat,color=Treat)) + 
  geom_boxplot(width=0.5,outlier.size = 0.01,alpha=0.5)+#基础图
  theme_minimal() +
  scale_color_manual(values = c("red","blue")) +
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
        text = element_text(family = "serif", size = 14))
p3 <- p2+geom_jitter(width = 0.2,aes(color=Treat),size=0.5,alpha=0.5)+#加上散点
  scale_fill_manual(values=c("white","white"))#箱体填充白色
p3
ggsave("Between.pdf", p3, width = 4.75, height = 4.75, dpi = 300)

