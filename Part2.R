
install.packages(c("rvest", "dplyr"))


library(rvest)
library(dplyr)


url <- "https://www.scrapethissite.com/pages/simple/"


page <- read_html(url)


countries <- page %>%
  html_nodes("div.col-md-4.country")


country_data <- data.frame(
  Country = countries %>% html_node("h3.country-name") %>% html_text(trim = TRUE),
  Capital = countries %>% html_node("span.country-capital") %>% html_text(trim = TRUE),
  Population = countries %>% html_node("span.country-population") %>% html_text(trim = TRUE),
  Area = countries %>% html_node("span.country-area") %>% html_text(trim = TRUE)
)


country_data <- country_data %>%
  mutate(
    Population = as.numeric(gsub(",", "", Population)),
    Area = as.numeric(gsub(",", "", Area))
  )


head(country_data)
nrow(country_data)


