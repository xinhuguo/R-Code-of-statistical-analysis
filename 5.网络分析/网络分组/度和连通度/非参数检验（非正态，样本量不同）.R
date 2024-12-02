library(openxlsx)

#=====================非参数检验
data <- read.xlsx("degree.xlsx", sheet = "Degree")
# 将数据分成两组
group1 <- data$Degree[data$Treatment == "B2"]
group2 <- data$Degree[data$Treatment == "B3"]
# 执行Mann-Whitney U 检验（样本量不同，不确定是否符合正太分布时使用）
result <- wilcox.test(group1, group2)
# 打印检验结果
print(result)
#=====================秸秆
data <- read.xlsx("degree.xlsx", sheet = "Between")
# 将数据分成两组
group1 <- data$betweenesscentrality[data$Treatment == "B2"]
group2 <- data$betweenesscentrality[data$Treatment == "B3"]
# 执行Mann-Whitney U 检验
result <- wilcox.test(group1, group2)
# 打印检验结果
print(result)
#=====================根系
data <- read.xlsx("度.xlsx", sheet = "Tillage")
# 将数据分成两组
group_RT <- data$Degree[data$Treatment == "RT"]
group_PT <- data$Degree[data$Treatment == "PT"]
group_NT <- data$Degree[data$Treatment == "NT"]
# 执行 Mann-Whitney U 检验比较 RT 和 PT 组
result_RT_PT <- wilcox.test(group_RT, group_PT)

# 执行 Mann-Whitney U 检验比较 RT 和 NT 组
result_RT_NT <- wilcox.test(group_RT, group_NT)

# 执行 Mann-Whitney U 检验比较 PT 和 NT 组
result_PT_NT <- wilcox.test(group_PT, group_NT)

# 设置显著性水平（通常为0.05）
alpha <- 0.05
# 计算Bonferroni校正后的显著性水平
alpha_adjusted <- alpha / 3  # 3是比较的总数

# 打印校正后的显著性水平
cat("校正后的显著性水平 (Bonferroni校正) =", alpha_adjusted, "\n")

# 打印比较结果并进行校正
print(result_RT_PT)


if (result_RT_PT$p.value < alpha_adjusted) {
  cat("RT 和 PT 组之间存在显著差异 (校正后)\n")
} else {
  cat("RT 和 PT 组之间没有显著差异 (校正后)\n")
}

print(result_RT_NT)
if (result_RT_NT$p.value < alpha_adjusted) {
  cat("RT 和 NT 组之间存在显著差异 (校正后)\n")
} else {
  cat("RT 和 NT 组之间没有显著差异 (校正后)\n")
}

print(result_PT_NT)
if (result_PT_NT$p.value < alpha_adjusted) {
  cat("PT 和 NT 组之间存在显著差异 (校正后)\n")
} else {
  cat("PT 和 NT 组之间没有显著差异 (校正后)\n")
}

