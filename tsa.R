library(tidyverse)
library(rvest)
library(httr)
library(extrafont)
library(ggtext)



df1 <- rvest::html_table(
  content(
    GET("https://www.tsa.gov/coronavirus/passenger-throughput")
    )
  )

#write_rds(df1,paste0("travel",Sys.Date(),".rds"))

df2 <- df1[[1]] %>%
  janitor::clean_names() %>%
  mutate(date = lubridate::mdy(date)) %>%
  mutate_if(is.character,  ~ gsub(",", "", .)) %>%
  mutate_if(is.character, as.numeric) %>%
  pivot_longer(starts_with("x"), names_to = "year", values_to = "pax") %>%
  mutate(year = str_remove_all(year, "_traveler_throughput"), year = str_remove(year, "x")) %>%
  mutate(date_real = as.Date(paste(year,lubridate::month(date),lubridate::day(date),sep = "-"))) %>%
  filter(is.na(pax) == FALSE) %>%
  arrange(date_real) %>%
  mutate(pax_ra = zoo::rollmean(x = pax,k = 7, na.pad = TRUE, align = "right")) %>%
  mutate(date_jul = as.numeric(format(date, "%j")))


ggplot(df2, aes(x = date_real, y = pax_ra, color = year, group = year)) +
  geom_path(size = 1.25) +
  theme_minimal()

ggplot(df2, aes(x = date_jul, y = pax_ra, color = year, group = year)) +
  geom_path(size = 1.25) +
  theme_minimal()


df3 <- df2 %>%
  select(date, year, pax_ra) %>%
  pivot_wider(names_from = "year", values_from = "pax_ra") %>%
  mutate(baseline20 = (`2020`-`2019`)/`2019`, baseline21 = (`2021`-`2019`)/`2019`)



df4 <- data.frame(val = c(df3$baseline20, df3$baseline21)) %>%
  mutate(day = row_number(), date = as.Date("2020-01-01") + (lubridate::days(day-1)))


ggplot(df4, aes(x = date, y = val)) +
  geom_path() +
  geom_smooth(data = filter(df4, date >= as.Date("2021-01-01")), method = "lm")


df5 <- df4 %>%
  filter(date >= as.Date("2021-01-01"))

model1 <- lm(df5$val ~ df5$date)

summary(model1)

preds <- predict(model1,as.Date("2021-12-30"))

df6 <- df5 %>%
  bind_cols(pred = preds) %>%
  mutate(we_back = case_when(
    pred >= 0 & lag(pred) < 0 ~ TRUE,
    TRUE ~ FALSE
  ))


col1 <- ggsci::pal_aaas()(2)[1]
col2 <- ggsci::pal_aaas()(3)[2]

ggplot(df6, aes(x = date)) +
  geom_path(aes(y = val), color = col1, size = 1.25) +
  geom_path(aes(y = pred), color = col2,size = 1) +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(data = filter(df6,we_back == TRUE),
             aes(xintercept = date), linetype = 3) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(), limits = c(min(df6$val),0.05)) +
  labs(
    y = "% Travelers From 2019 Baseline",
    x = "Date",
    title = "TSA Checkpoint Entries as Percent of Baseline",
    subtitle = "Linear Regression Prediction of 2021 Data"
  ) +
  theme(
    text = element_text(size = 16, family = "Bahnschrift"),
    axis.title.x = element_blank(),
    plot.title = element_textbox(color = col1),
    plot.subtitle = element_textbox(color = col2)
  )

ggsave("tsa_pred.png", width = 8, height = 6, dpi = 320)