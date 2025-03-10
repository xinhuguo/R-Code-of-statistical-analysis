# 加载必要的库
#install.packages("GGally")
library(GGally)
library(ggplot2)

# 读取数据文件
d <- read.table("env.txt", header = TRUE, row.names = 1, sep = "")  # 替换 sep 参数以匹配实际分隔符
d1 <- as.data.frame(d)  # 确保数据为数据框格式

# 1. 生成相关性矩阵
corr_matrix <- cor(d1)

# 2. 使用 ggpairs 绘制图形
p1 <- ggpairs(
  d1,
  # 上三角：相关系数
  upper = list(continuous = wrap("cor", alpha = 0.6, colour = "black")),
  # 下三角：散点图
  lower = list(continuous = wrap("points", alpha = 0.6, colour = "#ff5e00")),
  # 对角线：密度图
  diag = list(continuous = wrap("densityDiag", alpha = 0.5, fill = "#5cb3ba"))
)

# 显示图形
print(p1)

# 3. 输出图形为 PDF 文件
ggsave(
  filename = "ggpairs_plot.pdf",  # 输出文件名
  plot = p1,            # 图形对象
  dpi = 1200,                     # 分辨率
  height = 6,                     # 高度
  width = 8                       # 宽度
)
