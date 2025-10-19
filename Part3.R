
library(tidycensus)
library(tidyverse)
library(scales)


census_api_key("2686e1465fe3ea7adc6f212a4d1a37facebe4ba3", install = TRUE, overwrite=TRUE)
readRenviron("~/.Renviron")

# Step 1: 
vars <- c(
  median_income = "B19013_001E",      # median household income
  broadband = "B28002_004E",          # With an Internet subscription!!Broadband of any type
  total_households = "B28002_001E"    # Total households (as given)
)

# Step 2: 
raw <- get_acs(
  geography = "county",
  variables = vars,
  state = "IL",
  year = 2023,
  survey = "acs5",
  output = "wide"  
)

glimpse(raw)
head(raw)

# Step 3:
clean <- raw %>%
  select(GEOID, NAME,
         median_income,
         broadband,
         total_households) %>%
  mutate(
    broadband_rate = 100 * (broadband / total_households)
  ) %>%
  separate(NAME, into = c("County", "State"), sep = ", ", extra = "merge", fill = "right") %>%
  mutate(County = str_remove(County, " County$")) %>%
  arrange(desc(broadband_rate))

head(clean)

write_csv(clean, "IL_county_broadband_income_2023.csv")

# Step 4:
mean_broadband_rate <- mean(clean$broadband_rate, na.rm = TRUE)
median_broadband_rate <- median(clean$broadband_rate, na.rm = TRUE)

mean_broadband_rate
median_broadband_rate

top5 <- clean %>% slice_head(n = 5)
bottom5 <- clean %>% slice_tail(n = 5)

top5
bottom5

# Step 5:
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

ggsave("scatter_income_vs_broadband.png", p_scatter, width = 8, height = 5, dpi = 300)

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

p_all <- ggplot(clean, aes(x = reorder(County, broadband_rate), y = broadband_rate)) +
  geom_col() +
  coord_flip() +
  labs(title = "All Illinois Counties Ordered by Broadband Rate",
       x = "County", y = "Broadband rate (%)") +
  theme_minimal()

print(p_all)
ggsave("allcounties_broadband_ordered.png", p_all, width = 8, height = 12, dpi = 300)

summary_list <- list(
  mean_broadband_rate = mean_broadband_rate,
  median_broadband_rate = median_broadband_rate,
  top5 = top5,
  bottom5 = bottom5
)

saveRDS(summary_list, "IL_broadband_summary_2023.rds")

message("Done. CSV and plots saved in working directory.")





Step 6: Reflection

Income and Broadband Patterns
Counties with higher household incomes usually have higher broadband access. Poorer counties often show lower access rates. It seems income level is linked with better internet availability.

Reasons for Variation
Some counties are more rural and have less internet infrastructure. Service can also be expensive or unavailable in small towns. Urban areas simply have more options and better coverage.

Use for Policy
Public administrators can use this data to find where broadband service is weak. They could focus on rural or low-income areas when giving funding or building new networks. This helps reduce the digital gap.

Limitations of ACS Data
ACS data are only estimates and not always up to date. Some small counties may have missing or uncertain numbers. Also, it shows access, not actual usage or speed, so the picture is incomplete.
