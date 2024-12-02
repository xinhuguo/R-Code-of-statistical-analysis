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

alpha<-trans_alpha$new(dataset=df,group="group")
alpha<-trans_alpha$new(dataset=df)
alpha$data_alpha
openxlsx::write.xlsx(alpha$data_alpha, file = "a1.xlsx")