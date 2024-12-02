library(readxl)
library(ggplot2)
library(ggpattern)
library(ggbreak)
library(patchwork)
library(ggpubr) # 需要安装这个包

# 读取数据
data <- read_excel("土壤理化性质.xlsx", sheet = "Sheet2")

# 定义自定义顺序
custom_order <- c("NT-S-R", "RT-S-R", "PT-S-R", "NT-NS-R", "RT-NS-R", "PT-NS-R",  
                  "NT-S-NR", "RT-S-NR", "PT-S-NR", "NT-NS-NR", "RT-NS-NR", "PT-NS-NR")

# 定义图案和颜色
pattern_values <- c("PT-S-R" = "stripe", "NT-S-R" = "stripe", "RT-S-R" = "stripe",
                    "PT-NS-R" = "none", "NT-NS-R" = "none", "RT-NS-R" = "none",
                    "PT-S-NR" = "stripe", "NT-S-NR" = "stripe", "RT-S-NR" = "stripe",
                    "PT-NS-NR" = "none", "NT-NS-NR" = "none", "RT-NS-NR" = "none")

fill_values <- c("NT-S-R" = "#66c2a5", "NT-NS-R" = "#66c2a5", "NT-S-NR" ="#66c2a5", "NT-NS-NR" = "#66c2a5",
                 "RT-S-R" = "#fc8d62", "RT-NS-R" = "#fc8d62", "RT-S-NR" = "#fc8d62", "RT-NS-NR" = "#fc8d62",
                 "PT-S-R" = "#8da0cb", "PT-NS-R" = "#8da0cb", "PT-S-NR" = "#8da0cb", "PT-NS-NR" = "#8da0cb")

# 提取所有的 Index 值
index_list <- unique(data$Index)

# 创建空列表来存储图形
plots <- list()

# 使用 for 循环自动处理每个 Index
for (index in index_list) {
  data_M1 <- subset(data, Index == index)
  
  # 将 Treatment 列转换为 factor 类型，并指定自定义顺序
  data_M1$Treatment <- factor(data_M1$Treatment, levels = custom_order)
  
  # 生成图形
  p <- ggplot(data_M1, aes(x = Treatment, y = AVER, fill = Treatment, pattern = group)) +
    geom_bar_pattern(stat = "identity", position = position_dodge(width = 0.6), pattern = pattern_values,
                     color = "black", pattern_color = "black", pattern_density = 0.03, width = 0.6, linewidth = 0.5) +
    geom_errorbar(aes(ymin = AVER - STDEV, ymax = AVER + STDEV), 
                  width = 0.25, position = position_dodge(0.9), linewidth = 0.5) +
    geom_text(aes(label = P, y = AVER + STDEV), position = position_dodge(width = 0.6),
              vjust = -0.5, size = 5, family = "serif", fontface = "bold") +
    labs(x = "", y = "") +
    scale_fill_manual(values = fill_values, guide = "none") +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
          axis.ticks.y = element_line(color = "black"),
          axis.ticks.length = unit(-0.15, "cm"),
          axis.text.x = element_text(face = "bold", color = "black", size = rel(1.5), angle = 45, hjust = 1),
          axis.text.y = element_text(face = "bold", color = "black", size = rel(1.2)),
          legend.title = element_text(face = "bold", size = rel(1.5)),
          legend.text = element_text(face = "bold", size = rel(1.5)),
          text = element_text(family = "serif", size = 14),
          plot.margin = margin(10, 10, 10, 10)) +
    geom_vline(xintercept = 6.5, linetype = "dashed", color = "black", alpha = 0.8, linewidth = 0.7)
  
  # 将绘制的图形添加到列表中
  plots[[index]] <- p
  
  # 保存每个图形到PDF文件
  ggsave(filename = paste0("plot_", index, ".pdf"), plot = p, width = 3.9, height = 2.53, units = "in", dpi = 300)
}

# 将所有的ggplot对象组合成一个图形
final_plot <- ggarrange(plotlist = plots, nrow = 3, ncol = 3)

# 打印和保存组合图形
pdf("violin.pdf")
print(final_plot)
dev.off()
