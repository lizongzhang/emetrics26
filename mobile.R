
lm(laptop ~ brand_preference, df)

table(df$brand_preference)

library(readxl)
df <- read_excel("data/mobile.xlsx")

lm(log(laptop) ~ log(cost) + log(mobile) + usage_month + gender + 
     gender:log(mobile), data = df) %>% 
  summary()


