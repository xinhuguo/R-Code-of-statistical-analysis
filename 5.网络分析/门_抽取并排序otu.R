# 载入所需的库
library(readxl)
library(openxlsx)
# 读取“taxon_W.xlsx”文件中的Sheet1和Sheet2数据
sheet1_data <- read_excel("Module.xlsx", sheet = "Sheet3")
sheet2_data <- read_excel("Module.xlsx", sheet = "Sheet1")

# 提取“taxon_W.xlsx，Sheet1”中的“OTU ID”列的数据
otu_ids <- sheet1_data$`name`

# 根据“taxon_W.xlsx，Sheet2”中的“OTU ID”列的名称和顺序重新排序数据
ordered_otu_ids <- otu_ids[match(sheet2_data$`name`, otu_ids)]

# 提取“taxon_W.xlsx，Sheet1”中与排序后的“OTU ID”对应的所有列数据
extracted_data <- sheet1_data[match(ordered_otu_ids, otu_ids), ]

# 将提取的数据保存为新的Excel文件
write.xlsx(extracted_data, file = "modes.xlsx", sheetName = "Extracted new", rowNames = FALSE)
