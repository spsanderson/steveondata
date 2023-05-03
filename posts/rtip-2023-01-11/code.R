library(dplyr)
library(readxl)
library(lubridate)
library(timetk)
library(tidyr)
library(janitor)
library(stringr)
library(ggplot2)
library(healthyR.ts)

fp <- "C:/Users/steve/Downloads/linkedin_content.xlsx"
engagement_tbl <- read_excel(fp, sheet = "ENGAGEMENT") %>%
  clean_names() %>%
  mutate(date = mdy(date))
top_posts_tbl <- read_excel(fp, sheet = "TOP POSTS", skip = 1) %>%
  clean_names()
followers_tbl <- read_excel(fp, sheet = "FOLLOWERS", skip = 2) %>%
  clean_names() %>%
  mutate(date = mdy(date))
demographics_tbl <- read_excel(fp, sheet = "DEMOGRAPHICS") %>%
  clean_names()

engagement_tbl %>%
  mutate(`Engagement Rate` = (engagements / impressions) * 100) %>%
  filter_by_time(
    .date_var = date,
    .end_date = "2022-12-31"
  ) %>%
  pivot_longer(cols = -date) %>%
  mutate(name = str_to_title(name)) %>%
  plot_time_series(
    .facet_vars = name,
    .value = value,
    .date_var = date,
    .interactive = FALSE,
    .smooth = FALSE,
    .title = "LinkedIn Stats Time Series Plot"
  ) +
  geom_vline(xintercept = as.Date("2022-07-07"),
             color = "blue", linetype = "dashed") +
  geom_vline(xintercept = as.Date("2022-10-05"), 
             color = "red", linetype = "dashed") +
  labs(
    subtitle = "Blue Line - Start Telegram Channel, Red Line - Start Blog"
  ) +
  theme_minimal()

engagement_tbl %>%
  summarise_by_time(
    .date_var = date,
    .by = "month",
    `Cumulative Impressions` = sum(impressions),
    `Cumulative Engagements` = sum(engagements)
  ) %>%
  mutate(
    `Cumulative Impressions` = cumsum(`Cumulative Impressions`),
    `Cumulative Engagements` = cumsum(`Cumulative Engagements`)
  ) %>%
  slice(1:12) %>%
  pivot_longer(cols = -date) %>%
  mutate(name = str_to_title(name)) %>%
  ggplot(aes(x = date, y = value)) +
  geom_col() +
  facet_wrap(~ name, scales = "free") +
  geom_vline(xintercept = as.Date("2022-07-07"),
             color = "blue", linetype = "dashed") +
  geom_vline(xintercept = as.Date("2022-10-05"), 
             color = "red", linetype = "dashed") +
  labs(
    x = "Date",
    y = "Value",
    title = "LinkedIn Stats Time Series Plot",
    subtitle = "Blue Line - Start Telegram Channel, Red Line - Start Blog"
  ) +
  theme_minimal() 

df <- engagement_tbl %>%
  mutate(`Engagement Rate` = (engagements / impressions) * 100) %>%
  filter_by_time(
    .date_var = date,
    .end_date = "2022-12-31"
  )

ts_vva_plot(df, date, engagements)$plots$static_plot +
  geom_vline(xintercept = as.Date("2022-07-07"),
             color = "blue", linetype = "dashed") +
  geom_vline(xintercept = as.Date("2022-10-05"), 
             color = "red", linetype = "dashed") +
  labs(caption = "Blue Line - Start Telegram Channel, Red Line - Start Blog")

ts_vva_plot(df, date, impressions)$plots$static_plot +
  geom_vline(xintercept = as.Date("2022-07-07"),
             color = "blue", linetype = "dashed") +
  geom_vline(xintercept = as.Date("2022-10-05"), 
             color = "red", linetype = "dashed") +
  labs(caption = "Blue Line - Start Telegram Channel, Red Line - Start Blog")

ts_vva_plot(df, date, `Engagement Rate`)$plots$static_plot +
  geom_vline(xintercept = as.Date("2022-07-07"),
             color = "blue", linetype = "dashed") +
  geom_vline(xintercept = as.Date("2022-10-05"), 
             color = "red", linetype = "dashed") +
  labs(caption = "Blue Line - Start Telegram Channel, Red Line - Start Blog")

followers_tbl %>%
  filter_by_time(
    .date_var = date,
    .end_date = "2022-12-31"
  ) %>%
  plot_time_series(
    .value = new_followers,
    .date_var = date,
    .interactive = FALSE,
    .smooth = FALSE,
    .title = "LinkedIn Stats Time Series Plot - New Followers"
  ) +
  geom_vline(xintercept = as.Date("2022-07-07"),
             color = "blue", linetype = "dashed") +
  geom_vline(xintercept = as.Date("2022-10-05"), 
             color = "red", linetype = "dashed") +
  labs(
    subtitle = "Blue Line - Start Telegram Channel, Red Line - Start Blog"
  ) +
  theme_minimal()

followers_tbl %>%
  summarise_by_time(
    .date_var = date,
    .by = "month",
    `Cumulative Followers` = sum(new_followers)
  ) %>%
  mutate(
    `Cumulative Followers` = cumsum(`Cumulative Followers`)
  ) %>%
  slice(1:12) %>%
  ggplot(aes(x = date, y = `Cumulative Followers`)) +
  geom_col() +
  geom_vline(xintercept = as.Date("2022-07-07"),
             color = "blue", linetype = "dashed") +
  geom_vline(xintercept = as.Date("2022-10-05"), 
             color = "red", linetype = "dashed") +
  labs(
    x = "Date",
    y = "Value",
    title = "LinkedIn Stats Time Series Plot - New Followers",
    subtitle = "Blue Line - Start Telegram Channel, Red Line - Start Blog"
  ) +
  theme_minimal() 

ts_sma_plot(df, date, impressions, .sma_order = c(7, 14, 21, 30))$plots$static_plot +
  theme(axis.text.x = element_blank()) +
  geom_vline(xintercept = as.Date("2022-07-07"),
             color = "blue", linetype = "dashed") +
  geom_vline(xintercept = as.Date("2022-10-05"), 
             color = "red", linetype = "dashed") +
  labs(caption = "Blue Line - Start Telegram Channel, Red Line - Start Blog")

ts_sma_plot(df, date, engagements, .sma_order = c(7, 14, 21, 30))$plots$static_plot +
  theme(axis.text.x = element_blank()) +
  geom_vline(xintercept = as.Date("2022-07-07"),
             color = "blue", linetype = "dashed") +
  geom_vline(xintercept = as.Date("2022-10-05"), 
             color = "red", linetype = "dashed") +
  labs(caption = "Blue Line - Start Telegram Channel, Red Line - Start Blog")

ts_sma_plot(df, date, `Engagement Rate`, .sma_order = c(7, 14, 21, 30))$plots$static_plot +
  theme(axis.text.x = element_blank()) +
  geom_vline(xintercept = as.Date("2022-07-07"),
             color = "blue", linetype = "dashed") +
  geom_vline(xintercept = as.Date("2022-10-05"), 
             color = "red", linetype = "dashed") +
  labs(caption = "Blue Line - Start Telegram Channel, Red Line - Start Blog")

ts_calendar_heatmap_plot(df, date, impressions, .interactive = FALSE) +
  labs(title = "Calendar Heatmap - Impressions")

ts_calendar_heatmap_plot(df, date, engagements, .interactive = FALSE) +
  labs(title = "Calendar Heatmap - Engagemets")

ts_calendar_heatmap_plot(df, date, `Engagement Rate`, .interactive = FALSE) +
  labs(title = "Calendar Heatmap - Engagement Rate")


plot_seasonal_diagnostics(df, date, engagements, .interactive = FALSE,
                          .feature_set = c("wday.lbl", "month.lbl"),
                          .title = "Seasonal Diagnostics - Engagements") +
  theme_minimal()

plot_seasonal_diagnostics(df, date, impressions, .interactive = FALSE,
                          .feature_set = c("wday.lbl", "month.lbl"),
                          .title = "Seasonal Diagnostics - Impressions") +
  theme_minimal()

plot_seasonal_diagnostics(df, date, `Engagement Rate`, .interactive = FALSE,
                          .feature_set = c("wday.lbl", "month.lbl"),
                          .title = "Seasonal Diagnostics - Engagement Rate") +
  theme_minimal()

ts_lag_correlation(df, date, engagements, .lags = c(7, 14, 21, 28))$plots$lag_plot +
  labs(title = "Lag Correlation Plot - Engagements") +
  geom_smooth(se = FALSE, method = "lm", color = "black", linetype = "dashed")

ts_lag_correlation(df, date, impressions, .lags = c(7, 14, 21, 28))$plots$lag_plot +
  labs(title = "Lag Correlation Plot - Impressions") +
  geom_smooth(se = FALSE, method = "lm", color = "black", linetype = "dashed")

ts_lag_correlation(df, date, `Engagement Rate`, .lags = c(7, 14, 21, 28))$plots$lag_plot +
  labs(title = "Lag Correlation Plot - Engagement Rate") +
  geom_smooth(se = FALSE, method = "lm", color = "black", linetype = "dashed")
