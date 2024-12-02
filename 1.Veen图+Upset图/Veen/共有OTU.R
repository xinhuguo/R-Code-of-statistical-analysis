library(microeco)
library(ggplot2)
library(readxl)
library(openxlsx)
library(tibble)
####加载数据，包括OTU特征表、样本信息以及物种分类表
otu <- read_excel("OTU-MPs.xlsx", sheet = "otu")
tax <- read_excel("OTU-MPs.xlsx", sheet = "tax")
sample <- read_excel("OTU-MPs.xlsx", sheet = "sample")
sample <- column_to_rownames(sample, var = colnames(sample)[1])

##构造microtable
df<-microtable$new(sample_table=sample,
                     otu_table=otu,
                     tax_table=tax,
                     auto_tidy=F)
df
df1 <- df$merge_samples(use_group = "group")

Venn <- trans_venn$new(df1, ratio = "seqratio")
p1<-Venn$plot_venn()
p1
ggsave("Ven.pdf", p1, width = 5, height = 4.8, dpi = 300)




