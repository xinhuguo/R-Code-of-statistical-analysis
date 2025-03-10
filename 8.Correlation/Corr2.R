# 加载必要的包
library(GGally)
library(ggplot2)
library(scales)

# 读取 txt 数据
d <- read.table("env.txt", header = TRUE, row.names = 1, sep = "")
d1 <- as.data.frame(d)  # 转换为数据框

# 1. 生成相关性矩阵
corr_matrix <- cor(d1)

# 2. 自定义拟合函数，用于更改拟合线颜色
custom_smooth <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_smooth(method = "lm", color = "#5cb3ba", alpha = 0.4, size = 1, ...) +  # 拟合线
    geom_point(color = "#ff5e00", alpha = 0.6) +  # 数据点
    theme_minimal()
}

# 3. 使用 ggpairs 绘制下三角和对角线
ggpairs_plot <- ggpairs(
  d1,
  upper = "blank",  # 上三角留空
  lower = list(continuous = wrap(custom_smooth)),  # 下三角线性拟合图
  diag = list(continuous = wrap("densityDiag", alpha = 0.5, fill = "#5cb3ba"))  # 对角线密度图
)

# 4. 定义红蓝颜色映射函数
color_mapping <- function(value) {
  scales::col_numeric(
    palette = c("#5cb3ba", "white", "#ff5e00"),  # 蓝-白-红渐变
    domain = c(-1, 1)  # 相关系数范围
  )(value)
}

# 5. 替换 ggpairs 上三角内容
for (i in seq_along(d1)) {
  for (j in seq_along(d1)) {
    if (i < j) {  # 仅处理上三角部分
      corr_value <- corr_matrix[i, j]  # 提取相关系数
      corr_color <- color_mapping(corr_value)  # 映射到颜色
      corr_size <- abs(corr_value) * 25  # 动态调整矩形大小比例
      
      # 创建矩形单元格
      rect_cell <- ggplot() +
        geom_point(
          aes(x = 0.5, y = 0.5),
          shape = 21, size = corr_size, fill = corr_color, color = "black", alpha = 0.8
        ) +  # 动态矩形大小
        annotate("text", x = 0.5, y = 0.5, label = sprintf("%.2f", corr_value), size = 4, color = "black") +
        theme_void() +  # 去除多余背景
        theme(aspect.ratio = 1)  # 保持单元格正方形
      
      # 替换 ggpairs 的上三角单元格
      ggpairs_plot[i, j] <- rect_cell
    }
  }
}

# 6. 显示最终图表
print(ggpairs_plot)

# 7. 输出图片为 PDF 文件
ggsave("p2.pdf", plot = ggpairs_plot, dpi = 1200, height = 6, width = 8)
