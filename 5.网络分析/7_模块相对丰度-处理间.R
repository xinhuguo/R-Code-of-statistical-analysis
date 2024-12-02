library(readxl)
library(ggplot2)
library(ggpattern)
# 读取数据
data <- read_excel("Module.xlsx", sheet = "Fig")
# 将数据中的 "M1" 模块筛选出来
data_M1 <- subset(data, module == "M1")
custom_order <- c("T4","T1", "T6", "T5","T7", "T8")
# 将 Treatment 列转换为 factor 类型，并指定自定义顺序
data_M1$Treatment <- factor(data_M1$Treatment, levels = custom_order)
# 定义不同 group 值对应的图案类型
p <- ggplot(data_M1, aes(x = Treatment, y = AVER)) +
  geom_bar_pattern(stat = "identity", position = position_dodge(width = 0.6),pattern= "stripe",
                   color = "black",fill = "#264653", width = 0.6, size = 0.6) +
  geom_errorbar(aes(ymin = AVER - STDEV, ymax = AVER + STDEV), 
                width = 0.25, position = position_dodge(0.9), size = 0.6) +  
  geom_text(aes(label = P, y = AVER + STDEV), position = position_dodge(width = 0.6),
            vjust = -0.5, size = 5, family = "serif", fontface = "bold") +
  labs(x = "Treatment", y = "AVER") + 
  #scale_fill_manual(values = "#e66f51", guide = FALSE) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        #axis.ticks.x = element_line(color = "black"),
        axis.ticks.y = element_line(color = "black"),
        axis.ticks.length = unit(-0.15, "cm"),
        axis.text.x = element_text(face = "bold", color = "black", size = rel(1.5)),
        axis.text.y = element_text(face = "bold", color = "black", size = rel(1.5)),
        axis.title = element_text(face = "bold", size = rel(1.5)),
        legend.title = element_text(face = "bold", size = rel(1.5)),
        legend.text = element_text(face = "bold", size = rel(1.5)),
        plot.margin = margin(10, 10, 10, 10),
        text = element_text(family = "serif", size = 14)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.5)) +  # Set Y-axis limits
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p)
#========================================m2    
data_M1 <- subset(data, module == "M2")
custom_order <- c("T4","T1", "T6", "T5","T7", "T8")
# 将 Treatment 列转换为 factor 类型，并指定自定义顺序
data_M1$Treatment <- factor(data_M1$Treatment, levels = custom_order)
# 定义不同 group 值对应的图案类型
p1 <- ggplot(data_M1, aes(x = Treatment, y = AVER)) +
  geom_bar_pattern(stat = "identity", position = position_dodge(width = 0.6),pattern= "stripe",
                   color = "black",fill = "#299d90", width = 0.6, size = 0.6) +
  geom_errorbar(aes(ymin = AVER - STDEV, ymax = AVER + STDEV), 
                width = 0.25, position = position_dodge(0.9), size = 0.6) +  
  geom_text(aes(label = P, y = AVER + STDEV), position = position_dodge(width = 0.6),
            vjust = -0.5, size = 5, family = "serif", fontface = "bold") +
  labs(x = "Treatment", y = "AVER") + 
  #scale_fill_manual(values = "#e66f51", guide = FALSE) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        #axis.ticks.x = element_line(color = "black"),
        axis.ticks.y = element_line(color = "black"),
        axis.ticks.length = unit(-0.15, "cm"),
        axis.text.x = element_text(face = "bold", color = "black", size = rel(1.5)),
        axis.text.y = element_text(face = "bold", color = "black", size = rel(1.5)),
        axis.title = element_text(face = "bold", size = rel(1.5)),
        legend.title = element_text(face = "bold", size = rel(1.5)),
        legend.text = element_text(face = "bold", size = rel(1.5)),
        plot.margin = margin(10, 10, 10, 10),
        text = element_text(family = "serif", size = 14)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.5)) +  # Set Y-axis limits
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p1)
#================================M3
data_M1 <- subset(data, module == "M3")
custom_order <- c("T4","T1", "T6", "T5","T7", "T8")
# 将 Treatment 列转换为 factor 类型，并指定自定义顺序
data_M1$Treatment <- factor(data_M1$Treatment, levels = custom_order)
# 定义不同 group 值对应的图案类型
p2 <- ggplot(data_M1, aes(x = Treatment, y = AVER)) +
  geom_bar_pattern(stat = "identity", position = position_dodge(width = 0.6),pattern= "stripe",
                   color = "black",fill = "#e8c56b", width = 0.6, size = 0.6) +
  geom_errorbar(aes(ymin = AVER - STDEV, ymax = AVER + STDEV), 
                width = 0.25, position = position_dodge(0.9), size = 0.6) +  
  geom_text(aes(label = P, y = AVER + STDEV), position = position_dodge(width = 0.6),
            vjust = -0.5, size = 5, family = "serif", fontface = "bold") +
  labs(x = "Treatment", y = "AVER") + 
  #scale_fill_manual(values = "#e66f51", guide = FALSE) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        #axis.ticks.x = element_line(color = "black"),
        axis.ticks.y = element_line(color = "black"),
        axis.ticks.length = unit(-0.15, "cm"),
        axis.text.x = element_text(face = "bold", color = "black", size = rel(1.5)),
        axis.text.y = element_text(face = "bold", color = "black", size = rel(1.5)),
        axis.title = element_text(face = "bold", size = rel(1.5)),
        legend.title = element_text(face = "bold", size = rel(1.5)),
        legend.text = element_text(face = "bold", size = rel(1.5)),
        plot.margin = margin(10, 10, 10, 10),
        text = element_text(family = "serif", size = 14)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.5)) +  # Set Y-axis limits
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p2)
#=============================================M4
data_M1 <- subset(data, module == "M4")
custom_order <- c("T4","T1", "T6", "T5","T7", "T8")
# 将 Treatment 列转换为 factor 类型，并指定自定义顺序
data_M1$Treatment <- factor(data_M1$Treatment, levels = custom_order)
# 定义不同 group 值对应的图案类型
p3 <- ggplot(data_M1, aes(x = Treatment, y = AVER)) +
  geom_bar_pattern(stat = "identity", position = position_dodge(width = 0.6),pattern= "stripe",
                   color = "black",fill = "#e66f51", width = 0.6, size = 0.6) +
  geom_errorbar(aes(ymin = AVER - STDEV, ymax = AVER + STDEV), 
                width = 0.25, position = position_dodge(0.9), size = 0.6) +  
  geom_text(aes(label = P, y = AVER + STDEV), position = position_dodge(width = 0.6),
            vjust = -0.5, size = 5, family = "serif", fontface = "bold") +
  labs(x = "Treatment", y = "AVER") + 
  #scale_fill_manual(values = "#e66f51", guide = FALSE) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        #axis.ticks.x = element_line(color = "black"),
        axis.ticks.y = element_line(color = "black"),
        axis.ticks.length = unit(-0.15, "cm"),
        axis.text.x = element_text(face = "bold", color = "black", size = rel(1.5)),
        axis.text.y = element_text(face = "bold", color = "black", size = rel(1.5)),
        axis.title = element_text(face = "bold", size = rel(1.5)),
        legend.title = element_text(face = "bold", size = rel(1.5)),
        legend.text = element_text(face = "bold", size = rel(1.5)),
        plot.margin = margin(10, 10, 10, 10),
        text = element_text(family = "serif", size = 14)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.5)) +  # Set Y-axis limits
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p3)

ggsave("M1_XJ.pdf",p, width =6 , height = 3.2, units = "in", dpi = 300)
ggsave("M2_XJ.pdf",p1, width =6 , height = 3.2, units = "in", dpi = 300)
ggsave("M3_XJ.pdf",p2, width =6 , height = 3.2, units = "in", dpi = 300)
ggsave("M4_XJ.pdf",p3, width =6 , height = 3.2, units = "in", dpi = 300)

