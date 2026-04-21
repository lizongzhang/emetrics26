# 安装必要的包（如果尚未安装）
install.packages(c("modelsummary", "flextable"))

data("mtcars")

library(modelsummary)
library(flextable)
library(tidyverse)

# modelsummary ------------------------------------------------------------
# 1. 准备数据
data_subset <- mtcars[, c("mpg", "cyl", "disp", "hp", "drat", "wt")]

# 2. 生成表格并应用 APA 三线表格式
# 注意：我们将 output 设为 'flextable' 以便进行格式美化
tab <- datasummary(mpg + cyl + disp + hp + drat + wt ~ 
                     N + Mean + SD + Median + Min + Max, 
                   data = data_subset, 
                   fmt = 3, 
                   output = 'flextable') %>%
  theme_apa() %>%              # 应用标准 APA 格式（完美的三线表）
  autofit() %>%                # 自动调整列宽
  font(fontname = "Times New Roman", part = "all")

# 3. 导出到 Word
save_as_docx(tab, path = "Table1_APA_Style.docx")

# regression table --------------------------------------------------------

# 估计模型
m1 <- lm(mpg ~ wt, data = mtcars)
m2 <- lm(mpg ~ wt + hp, data = mtcars)
m3 <- lm(mpg ~ wt + hp + cyl, data = mtcars)

# 将模型放入一个列表
models <- list(
  "Model 1" = m1,
  "Model 2" = m2,
  "Model 3" = m3
)

# 一行代码生成回归表
modelsummary(models, 
             fmt = 3,                 # 保留三位小数
             stars = TRUE,            # 自动添加显著性星号 (* p<0.1, ** p<0.05, *** p<0.01)
             output = "flextable") %>% # 转换为可美化的格式
  theme_apa() %>%                     # 应用 APA 三线表风格
  autofit() %>% 
  save_as_docx(path = "Table2_Regression_Results.docx")
