# ---------------------------
# PART III: ACS 数据抓取与分析 (Illinois counties)
# ---------------------------

# 安装（如需要）并加载包
if (!requireNamespace("tidycensus", quietly = TRUE)) install.packages("tidycensus")
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("scales", quietly = TRUE)) install.packages("scales")

library(tidycensus)
library(tidyverse)
library(scales)

# ----- 设置 Census API key（只需运行一次，或把它保存在 .Renviron 中） -----
# census_api_key("<YOUR_CENSUS_KEY>", install = TRUE, overwrite = FALSE)
# readRenviron("~/.Renviron")  # 如果你刚安装 key，取消注释以加载

# 我们要用的变量（2023 ACS 5-year）
vars <- c(
  median_income = "B19013_001E",      # median household income
  broadband = "B28002_004E",          # With an Internet subscription!!Broadband of any type
  total_households = "B28002_001E"    # Total households (as given)
)

# ----- Step 2: 用 get_acs() 获取所有 Illinois 县的数据 -----
# geography = "county", state = "IL", year = 2023 (acs5)
raw <- get_acs(
  geography = "county",
  variables = vars,
  state = "IL",
  year = 2023,
  survey = "acs5",
  output = "wide"  # 直接把 estimate 放宽成宽格式，方便后续处理
)

# raw 是宽表，变量名称类似 median_incomeE, median_incomeM (margin) 等
# 展示前几行
glimpse(raw)
head(raw)

# ----- Step 3: 清洗与变换 -----
clean <- raw %>%
  # 选择我们需要的 estimate 列（_E 后缀为 estimate）
  select(GEOID, NAME,
         median_income = median_incomeE,
         broadband = broadbandE,
         total_households = total_householdsE) %>%
  # 计算 broadband rate (%)：broadband / total_households * 100
  mutate(
    broadband_rate = 100 * (broadband / total_households)
  ) %>%
  # 如果想要把 NAME 拆成 County 和 State（Bonus）
  separate(NAME, into = c("County", "State"), sep = ", ", extra = "merge", fill = "right") %>%
  # 将县名去掉尾部 " County"（可选）
  mutate(County = str_remove(County, " County$")) %>%
  # 按 broadband_rate 从高到低排序
  arrange(desc(broadband_rate))

# 查看清洗后前几行
head(clean)

# 保存清洗后的 CSV（方便提交）
write_csv(clean, "IL_county_broadband_income_2023.csv")

# ----- Step 4: 分析模式 -----
# a) mean 和 median broadband rate（全州所有县）
mean_broadband_rate <- mean(clean$broadband_rate, na.rm = TRUE)
median_broadband_rate <- median(clean$broadband_rate, na.rm = TRUE)

mean_broadband_rate
median_broadband_rate

# b) top 5 counties 和 bottom 5 counties
top5 <- clean %>% slice_head(n = 5)
bottom5 <- clean %>% slice_tail(n = 5)

top5
bottom5

# ----- Step 5: 可视化 -----
# 1) 散点图：收入 (x) vs. 宽带率 (y)，加回归线
p_scatter <- ggplot(clean, aes(x = median_income, y = broadband_rate)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Illinois Counties: Median Household Income vs Broadband Adoption Rate (2023 ACS 5-yr)",
    x = "Median household income (USD, 2023 inflation-adjusted)",
    y = "Broadband adoption rate (%)",
    caption = "Source: ACS 2019-2023 5-year via tidycensus"
  ) +
  theme_minimal()

print(p_scatter)

# 保存图片
ggsave("scatter_income_vs_broadband.png", p_scatter, width = 8, height = 5, dpi = 300)

# 2) 条形图：Top 10 和 Bottom 10 counties by broadband_rate
top10 <- clean %>% slice_head(n = 10) %>% arrange(broadband_rate)
bottom10 <- clean %>% slice_tail(n = 10) %>% arrange(desc(broadband_rate))  # reverse for plotting

p_top10 <- ggplot(top10, aes(x = reorder(County, broadband_rate), y = broadband_rate)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 10 Illinois Counties by Broadband Rate",
       x = "County", y = "Broadband rate (%)") +
  theme_minimal()

p_bottom10 <- ggplot(bottom10, aes(x = reorder(County, -broadband_rate), y = broadband_rate)) +
  geom_col() +
  coord_flip() +
  labs(title = "Bottom 10 Illinois Counties by Broadband Rate",
       x = "County", y = "Broadband rate (%)") +
  theme_minimal()

print(p_top10)
print(p_bottom10)

ggsave("top10_broadband.png", p_top10, width = 7, height = 5, dpi = 300)
ggsave("bottom10_broadband.png", p_bottom10, width = 7, height = 5, dpi = 300)

# 3) BONUS: 单图显示所有县按 broadband_rate 排序（有序条形图）
p_all <- ggplot(clean, aes(x = reorder(County, broadband_rate), y = broadband_rate)) +
  geom_col() +
  coord_flip() +
  labs(title = "All Illinois Counties Ordered by Broadband Rate",
       x = "County", y = "Broadband rate (%)") +
  theme_minimal()

print(p_all)
ggsave("allcounties_broadband_ordered.png", p_all, width = 8, height = 12, dpi = 300)

# ----- 输出汇总：把关键结果保存到文件 -----
summary_list <- list(
  mean_broadband_rate = mean_broadband_rate,
  median_broadband_rate = median_broadband_rate,
  top5 = top5,
  bottom5 = bottom5
)

# Optional: 保存为 RDS 供后续使用
saveRDS(summary_list, "IL_broadband_summary_2023.rds")

# 完成
message("Done. CSV and plots saved in working directory.")
