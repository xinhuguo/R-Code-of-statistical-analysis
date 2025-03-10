#========================================显示×===========
#install.packages("tidyverse")
library(tidyverse)
raw_data<-read_csv("env.csv") %>% 
  select(-c(1))
corrmatrix <- cor(raw_data, method = "spearman")
corrmatrix
res1 <-corrplot::cor.mtest(corrmatrix, conf.level= .95)

res1$p
res1$lowCI
res1$uppCI
col3 <- grDevices::colorRampPalette(c("#025c03", "white", "#cebd9c")) 
col3

corrplot::corrplot.mixed(corr=corrmatrix,
                         lower="number",
                         upper="circle",
                         diag="u",
                         upper.col =col3(20),
                         lower.col = col3(20),
                         number.cex=0.9,
                         p.mat= res1$p,
                         sig.level= 0.05,
                         igsig="blank",
                         bg = "white",
                         is.corr = TRUE, 
                         outline = FALSE, 
                         mar = c(0,0,3,0),
                         addCoef.col = NULL, 
                         addCoefasPercent = FALSE, 
                         order = c("original"),
                         rect.col = "black", 
                         rect.lwd = 1, 
                         tl.cex = 1.2,
                         tl.col = "black", 
                         tl.offset = 0.4, 
                         tl.srt = 90,
                         cl.cex = 1.1, 
                         cl.ratio = 0.2,
                         tl.pos="lt",
                         cl.offset = 0.5 )
title(main=" ",cex.main=2.1)
#========================================不显示×===========
library(tidyverse)
#install.packages("corrplot")
library(corrplot)
raw_data <- read_csv("env.csv") %>% select(-c(1))

corrmatrix <- cor(raw_data, method = "spearman")
res1 <- corrplot::cor.mtest(raw_data, conf.level = .95)
col3 <- grDevices::colorRampPalette(c("#cebd9c", "white", "#025c03"))

pdf(file = "heatmap0.pdf",width = 5,height = 5,family="Times")
corrplot::corrplot.mixed(corr = corrmatrix,
                         lower = "number",
                         upper = "circle",
                         diag = "u",
                         upper.col = col3(10),
                         lower.col = col3(10),
                         number.cex = 0.7,
                         p.mat = res1$p,
                         sig.level = 0.05,
                         insig = "blank", # 这里设置为“blank”以便不显著的不标记
                         bg = "white",
                         is.corr = TRUE, 
                         outline = FALSE, 
                         mar = c(0, 0, 3, 0),
                         addCoef.col = NULL, 
                         addCoefasPercent = FALSE, 
                         order = "original",
                         rect.col = "black", 
                         rect.lwd = 1, 
                         tl.cex = 1.2,
                         tl.col = "black", 
                         tl.offset = 0.4, 
                         tl.srt = 90,
                         cl.cex = 1.1, 
                         cl.ratio = 0.2,
                         tl.pos = "lt",
                         cl.offset = 0.2)
dev.off()

#========================================显示*****===========
library(tidyverse)
library(corrplot)

# 读取数据并选择所需列
raw_data <- read_csv("env.csv") %>% select(-c(1))

# 计算Spearman相关矩阵
corrmatrix <- cor(raw_data, method = "spearman")

# 计算p值矩阵
res1 <- corrplot::cor.mtest(raw_data, conf.level = .95)

# 定义颜色调色板
col3 <- grDevices::colorRampPalette(c("#025c03", "white", "#cebd9c"))

pdf(file = "heatmap.pdf",width = 5,height = 5,family="Times")
# 绘制上三角部分（圆圈表示相关系数，星号表示显著性水平）
corrplot(corr = corrmatrix,
         method = "circle",
         type = "upper",
         tl.pos = "lt",
         insig = "label_sig",
         sig.level = c(.001, .01, .05),
         pch.cex = 1.0, # 设置显著性标签的字符大小
         p.mat = res1$p,
         col = col3(10), 
        tl.col = "black")

# 绘制下三角部分（数字表示相关系数）
corrplot(corr = corrmatrix,
         method = "number",
         type = "lower",
         add = TRUE,
         tl.pos = "n",
         cl.pos = "n",
         diag = FALSE,
         col = col3(10),
         number.cex = 0.7)
dev.off()










