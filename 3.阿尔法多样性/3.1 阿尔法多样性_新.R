library(microeco)
library(ggplot2)
library(readxl)
library(openxlsx)
library(tibble)

#### 加载数据
otu <- read_excel("OTU-MPs.xlsx", sheet = "otu") |> as.data.frame()
tax <- read_excel("OTU-MPs.xlsx", sheet = "tax") |> as.data.frame()
sample <- read_excel("OTU-MPs.xlsx", sheet = "sample")

## 将 sample 第一列设置为行名
sample <- column_to_rownames(sample, var = colnames(sample)[1])

## 构造 microtable
df <- microtable$new(sample_table = sample,
                     otu_table = otu,
                     tax_table = tax,
                     auto_tidy = FALSE)

df

## 计算 alpha 多样性
alpha <- trans_alpha$new(dataset = df, group = "group")

## 提取 alpha 多样性数据
alpha_data <- alpha$data_alpha

## 导出数据
write.xlsx(alpha_data, file = "a2.xlsx")
