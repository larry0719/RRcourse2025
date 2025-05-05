# 加载包（确保已安装 rmarkdown）
if (!require("rmarkdown")) install.packages("rmarkdown")
library(rmarkdown)

# 设置工作目录（以下二选一）
## 方法1：硬编码路径（替换为你的实际路径）
setwd("C:/Users/MI/Desktop/RR25/RRcourse2025/10. MD and Quarto 3")

# 批量生成报告
for (season in 1:8) {
  output_file <- paste0("GOT_Season_", season, "_Report.pdf")
  
  tryCatch({
    rmarkdown::render(
      input = "Assignment.Qmd",  # 确保文件名一致（区分大小写！）
      output_file = output_file,
      params = list(season = season),
      encoding = "UTF-8"
    )
    message("成功生成：", output_file)
  }, error = function(e) {
    message("生成失败：", output_file, "\n错误信息：", e$message)
  })
}

