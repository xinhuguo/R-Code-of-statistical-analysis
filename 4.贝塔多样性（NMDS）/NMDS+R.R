#=========================2024.05.28=======B1===================
library(showtext) # 加载包
#serif代表Times New Roman字体，sans代表Arial字体，mono代表Courier New字体。这种映射关系在基础绘图系统和ggplot2系统中均可使用。
library(sysfonts) # 加载
library(vegan)
library(readxl)
library(ggplot2)
library(openxlsx)
library(ggpubr)
# 读取Excel文件
data <- read_excel("OTU-MPs.xlsx", sheet = "NMDS")

# 提取分组因子列
groups <- data$Treatment

# 提取数值型数据列#
nmds_data <- as.matrix(data[, -(1:2)])

dist_matrix <- vegdist(nmds_data, method = "bray")

# 进行NMDS分析
nmds <- metaMDS(dist_matrix, k = 2)

# 计算stress值
stress_value <- nmds$stress

# 输出stress值
print(paste("Stress value:", stress_value))

# 进行ANOSIM分析
anosim_result <- anosim(dist_matrix, groups, permutations = 999)
summary(anosim_result)

# 提取NMDS坐标
nmds_coordinates <- scores(nmds, display = "sites", choices = c(1, 2))

# 创建包含分组因子的数据框
nmds_df <- data.frame(nmds_coordinates, groups)
merged_data <- cbind(data[, 1:2], nmds_coordinates)
#write.xlsx(merged_data,file = "nmds.xlsx")
nmds_df$groups <- factor(nmds_df$groups, levels = c("Control", "PP", "PLA"))
# 绘制NMDS图形
p1 <- plot <- ggplot(nmds_df, aes(x = NMDS1, y = NMDS2, color = groups)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.8) +
  geom_point(shape = 16, size = 3.5, stroke = 1) +
  #stat_ellipse(aes(fill = groups), geom = 'polygon', level = 0.95, alpha = 0.1, show.legend = FALSE) +
  stat_chull(aes(color = groups,fill = groups), geom = "polygon",size=0.7, alpha = 0.1, show.legend = FALSE)+
  labs(x = "NMDS1", y = "NMDS2", color = "Treatment") +
  theme_minimal() +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_color_manual(values = c("#EECA40", "#FD7541", "#23B9C7")) +
  scale_fill_manual(values = c("#EECA40", "#FD7541", "#23B9C7")) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.ticks.x = element_line(color = "black"),
        axis.ticks.y = element_line(color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.text.x = element_text(face = "bold", color = "black", size = rel(1.7)),
        axis.text.y = element_text(face = "bold", color = "black", size = rel(1.7)),
        axis.title = element_text(face = "bold", size = rel(1.5)),
        legend.title = element_text(face = "bold", size = rel(1.5)),
        legend.text = element_text(face = "bold", size = rel(1.5)),
        plot.margin = margin(10, 10, 10, 10),
        text = element_text(family = "serif", size = 14)) +#serif代表Times New Roman字体，sans代表Arial字体，mono代表Courier New字体
  coord_cartesian(xlim = c(-0.35, 0.35), ylim = c(-0.35, 0.35))+ 
  annotate("text", x =0.2, y = -0.2, label="Stress = 0.07", cex = 5, col = "black", fontface = 2,family = "serif")+
  annotate("text", x =0.2, y = -0.25, label="P < 0.05", cex = 5, col = "black", fontface = 2,family = "serif")+
  annotate("text", x =0.2, y = -0.3, label="Anosim R = 0.18", cex = 5, col = "black", fontface = 2,family = "serif")
p1

# 保存图形（不包括图例）
#ggsave("0715.tiff", plot, width = 6, height = 4.75, dpi = 300, units = "in", device = "tiff")


#=================================导出图形===============================
ggsave("nmsd.pdf", p1, width = 6.5, height = 4.75, dpi = 300)

