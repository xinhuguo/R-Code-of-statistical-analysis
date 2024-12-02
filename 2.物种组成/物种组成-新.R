rm(list = ls())
#install.packages("ggalluvial")

#install.packages("ggh4x")

library(MicrobiotaProcess) 
library(ggplot2) 
library(ggalluvial) 
library(ggh4x) 
library(phyloseq) 
library(openxlsx)
library(readxl)
library(tibble)
####加载数据，包括OTU特征表、样本信息以及物种分类表
OTU <- read_excel("OTU-MPs.xlsx", sheet = "otu")
Tax <- read_excel("OTU-MPs.xlsx", sheet = "tax")
sample <- read_excel("OTU-MPs.xlsx", sheet = "sample")
sample <- column_to_rownames(sample, var = colnames(sample)[1])

## 构造microtable
ps <- phyloseq(sample_data(sample),
               otu_table(as.matrix(OTU), taxa_are_rows=TRUE), 
               tax_table(as.matrix(Tax)))
#转换数据格式
df<-ps%>%as.MPSE()
df
df %<>%
  mp_cal_abundance( # for each samples
    .abundance = RareAbundance
    ) %>%
  mp_cal_abundance( # for each groups
    .abundance=RareAbundance,
    .group=group
    )

p1 <- df %>%  mp_plot_abundance(
  .abundance=RareAbundance,
  .group=group, #指定分组以分面
  taxa.class = Phylum, #指定分类水平    
  topn = 10,#可视化物种的数量    
  relative = TRUE#相对丰度  
  )+
  theme(legend.position = "none")
p1


p3 <- df %>%
  mp_plot_abundance(
    .abundance = RareAbundance,
    .group = group,
    taxa.class = Phylum,
    topn = 10,
    plot.group = TRUE
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    text = element_text(family = "serif", size = 14, face = "bold"), # 字体加粗
    panel.border = element_rect(color = "black", fill = NA, size = 1) # 外边框
  ) +
  scale_fill_manual(values = c("#E3F2FD", "#A0D8F1", "#6BCDF2", "#87CEEB", "#5AB3F0", "#FFD97D", "#FFDC70", "#F9D29B", "#F8C58C", "#F6B680", "#F4A261")) +
  scale_x_discrete(limits = c("Control", "PP", "PLA"))

p3

ggsave("Phylum_堆积.pdf", p3, width = 5, height = 7, dpi = 300)




#=====================属
Genus <- trans_abund$new(dataset = df, taxrank = "Genus", ntaxa = 10)
Genus$plot_bar(others_color = "grey70",#剩余分类的填充色
                facet = "group", #根据组进行分面
                xtext_keep = T, #是否显示样本名称
                legend_text_italic = F)

Genus_abundance <- Genus$data_abund
openxlsx::write.xlsx(Genus_abundance, file = "Genus_abundance.xlsx")

Genus2 <- trans_abund$new(dataset = df, taxrank = "Genus", ntaxa = 10, groupmean = "group")

p3 <-Genus2$plot_bar(others_color = "grey70",legend_text_italic = FALSE)
p4<-p3 + geom_bar(stat = "identity", position = "stack", color = "grey50") +
  scale_fill_manual(values = c("#E3F2FD",  "#A0D8F1", "#6BCDF2", "#87CEEB","#5AB3F0", "#FFD97D", "#FFDC70", "#F9D29B", "#F8C58C", "#F6B680", "#F4A261"))+
  scale_x_discrete(limits = c("Control", "PP", "PLA")) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), # 黑色边框
        axis.ticks.x = element_line(color = "black"),
        axis.ticks.y = element_line(color = "black"),
        axis.ticks.length = unit(0.15, "cm"), # 调整刻度线的长度
        axis.text.x = element_text(face = "bold", color = "black", size = rel(1.2), angle = 45, vjust = 1, hjust = 1), # 斜角X轴标签
        axis.text.y = element_text(face = "bold", color = "black", size = rel(1.2)),
        axis.title = element_text(face = "bold", size = rel(1)),
        legend.title = element_text(face = "bold", size = rel(0.8)),
        legend.text = element_text(face = "bold", size = rel(0.8)),
        plot.margin = margin(10, 10, 10, 10), # 图形四周的空白
        text = element_text(family = "serif", size = 14)) + # 字体设置
  scale_y_continuous(expand = c(0, 0)) # 消除Y轴的间隙
p4  
ggsave("Genus_堆积.pdf", p4, width = 5, height = 7, dpi = 300)



