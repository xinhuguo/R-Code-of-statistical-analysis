# 安装和加载必要的R包
#install.packages("devtools")
#devtools::install_github("Hy4m/linKEt", force = TRUE)
#packageVersion("linkET")
#install.packages("FD")
library(dplyr)
library(vegan)
library(linkET)
library(ggplot2)
library(readxl)
library(FD)
# 用于读取Excel文件的包

# 读取相关性矩阵的热图数据
wenv_data <- read_excel("chem.xlsx")

# 读取另一个数据
wspe_data <- read_excel("spec.xlsx", sheet = "Sheet1")

# 进行Mantel相关性分析
mantel <- mantel_test(wspe_data, wenv_data,
                      spec_select = list(M1 = 1:1,
                                         M2 = 2:2,
                                         M3 = 3:3,
                                         M4 = 4:4)) %>%
  mutate(rd = cut(r, breaks = c(-Inf, 0.2, 0.4, Inf),
                  labels = c("< 0.2", "0.2 - 0.4", ">= 0.4")),
         pd = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf),
                  labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")))

#write.csv(mantel, file = "mantel_result_占比.csv", row.names = FALSE)

# 绘制相关性矩阵的热图和Mantel相关性图
R <- qcorrplot(correlate(wenv_data), type = "lower", diag = FALSE) +
  geom_square() +
  geom_couple(aes(colour = pd, size = rd),
              data = mantel,
              curvature = nice_curvature()) +
  scale_fill_gradient2(low = "#0000FF", high = "#FF0000", limits = c(-1, 1)) +
  scale_size_manual(values = c(0.5, 1, 2)) +
  scale_colour_manual(values = color_pal(3)) +
  guides(size = guide_legend(title = "Mantel's r",
                             override.aes = list(colour = "grey35"),
                             order = 2),
         colour = guide_legend(title = "Mantel's p",
                               override.aes = list(size = 3),
                               order = 1),
         fill = guide_colorbar(title = "Pearson's r", order = 3))
ggsave("模块-mantel_占比.tiff", R, width = 8, height = 4.75, dpi = 300)
ggsave("模块-mantel_占比.pdf", R,  dpi = 300)
