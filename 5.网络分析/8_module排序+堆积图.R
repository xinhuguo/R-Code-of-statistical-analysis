# 加载所需的库
library(tidyverse)
library(ggplot2)
library(openxlsx)
library(readxl)

# 读取数据文件
data <- read_excel("Module.xlsx", sheet = "堆积图")

# 按属分类（Genus）和样本分组（module）进行分类，并计算每个“Genus”在各分组中的OTU数量总和
Genus <- data %>%
  group_by(module, Genus) %>%
  summarise(TotalAbundance = sum(`OTU Numeber`)) %>%
  ungroup()

# 计算每个分组中的总OTU数量
module_total <- Genus %>%
  group_by(module) %>%
  summarise(TotalAbundance = sum(TotalAbundance))

# 将Genus表和module_total表合并，并计算每个“Genus”在各分组中的占比
Genus_percentage <- Genus %>%
  left_join(module_total, by = "module") %>%
  mutate(Percentage = TotalAbundance.x / TotalAbundance.y) %>%
  select(-TotalAbundance.x, -TotalAbundance.y)

top6_per_module <- Genus_percentage %>%
  group_by(module) %>%
  top_n(6, Percentage) %>%
  ungroup()

# 计算剩余"Genus"的百分比之和，合并为"Other"
other_per_module <- Genus_percentage %>%
  anti_join(top6_per_module, by = c("module", "Genus")) %>%
  group_by(module) %>%
  summarise(Genus = "Other", Percentage = sum(Percentage)) %>%
  ungroup()

# 合并top6和other数据框
top10_per_module <- bind_rows(top6_per_module, other_per_module)

#write.xlsx(top10_XJ_module, file = "top10_per_module2.xlsx")
# 自定义顺序
custom_order <- c("Other","Methylomirabilota","unclassified_k__norank_d__Bacteria","Nitrospirota",
                  "Firmicutes","Gemmatimonadota", "Acidobacteriota","Chloroflexi", "Actinobacteriota", "Proteobacteria")

# 将"Genus"列转换为有序因子，并按自定义顺序排序
top10_per_module$Genus <- factor(top10_per_module$Genus, levels = custom_order, ordered = TRUE)

# 设置新的颜色
new_colors <- rev(c("#223e9c", "#edae11", "#aebea6", "#b12b23", "#0f6657", "#8d4bbb", "#f2ccac", "#fa6e01","#0eb0c8","#adacae"))

# 绘制百分比堆积柱状图
abun_plot <- ggplot(top10_per_module, aes(x = module, y = Percentage, fill = Genus)) +
  geom_bar(stat = "identity", position = "fill",color="black") +
  scale_y_continuous(labels = scales::percent_format(scale = 100), expand = c(0, 0)) +
  scale_fill_manual(values = new_colors) +  # 设置新的颜色
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +  # 旋转x轴标签45度
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.ticks.x = element_line(color = "black"),
        axis.ticks.y = element_line(color = "black"),
        axis.ticks.length = unit(-0.15, "cm"),
        axis.text.x = element_text(face = "bold", color = "black", size = rel(1.1)),
        axis.text.y = element_text(face = "bold", color = "black", size = rel(1)),
        axis.title = element_text(face = "bold", size = rel(1)),
        legend.title = element_text(face = "bold", size = rel(0.7)),
        legend.text = element_text(face = "bold", size = rel(0.7)),
        plot.margin = margin(10, 10, 10, 10),
        text = element_text(family = "serif", size = 14))
abun_plot <- abun_plot + labs(fill = "Phylum")

print(abun_plot)
#ggsave("module_abun2.tiff", abun_plot, width = 6, height = 4.75, dpi = 300)
ggsave("module_abun_XJ.pdf", abun_plot, width = 7, height = 4.75, dpi = 300)

