# 生成模拟数据
set.seed(123)  # 保证结果可重复

# 假设有 50 个样本
n <- 50

# 生成观测变量的随机数据
data <- data.frame(
  YC1 = rnorm(n, mean = 50, sd = 1.1),
  YC2 = rnorm(n, mean = 55, sd = 1.2),
  SP1 = rnorm(n, mean = 30, sd = 1.5),
  SP2 = rnorm(n, mean = 32, sd = 1),
  DMA1 = rnorm(n, mean = 40, sd = 2),
  DMA2 = rnorm(n, mean = 42, sd = 3),
  DMA3 = rnorm(n, mean = 41, sd = 2.1),
  DMA4 = rnorm(n, mean = 43, sd = 2.3),
  DMA5 = rnorm(n, mean = 45, sd = 1),
  DMA6 = rnorm(n, mean = 46, sd = 3),
  GP1 = rnorm(n, mean = 60, sd = 2.7),
  GP2 = rnorm(n, mean = 61, sd = 2.6),
  GP3 = rnorm(n, mean = 59, sd = 2),
  Y1 = rnorm(n, mean = 70, sd = 2.5)
)

# 将数据保存为文本文件
write.table(data, file = "PLS_1.txt", sep = "\t", row.names = FALSE, col.names = TRUE)

head(data)
