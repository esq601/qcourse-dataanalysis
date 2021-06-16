library(tidyverse)

source("airports.R")

restr <- read_csv("https://raw.githubusercontent.com/OxCGRT/USA-covid-policy/master/data/OxCGRT_US_latest.csv")


rest_lg <- restr %>%
  janitor::clean_names() %>%
  mutate(date = lubridate::ymd(date))

states <- rest_lg %>%
  filter(region_name %in% airport_state$name)


ggplot(states, aes(x= date, y = stringency_index_for_display)) +
  geom_step(aes(color = region_name), size = 1.25) +
  theme_minimal() +
  ggsci::scale_color_jama() +
  theme(
    text = element_text(family = "Bahnschrift",size = 16),
    strip.background = element_rect(color = "black"),
    panel.background = element_rect(color = "black"),
    legend.position = "top",
    plot.caption = element_text(color = "grey40",size = 6)
  ) +
  scale_x_date(limits = c(as.Date("2021-01-01"),Sys.Date()), labels = scales::date_format(format = "%b-%y")) +
  labs(
    title = "State Restriction Stringency",
    #subtitle = "Comparing April 2019 to April 2021",
    y = "Stringency Index",
    x = "Date",
    color = "State",
    caption = "Source: https://github.com/OxCGRT/USA-covid-policy"
  )

#ggsave("restriction.png", width = 8, height = 6, dpi = 320)


states_sum <- states %>%
  group_by(region_name) %>%
  summarise(ind_mean = mean(stringency_index_for_display, na.rm = TRUE))


rest_sum <- airport_sum %>%
  left_join(states_sum, by =c("name" = "region_name")) 

ggplot(rest_sum, aes(x = ind_mean, y = pct)) +
  geom_point(alpha = 1,aes(size = `2019`, color = "2019")) +
  geom_point(alpha = 1,aes(size = `2021`, color = "2021")) +
  geom_text(aes(label = facility), hjust = 0, nudge_x = .25) +
  geom_smooth(method = "lm",alpha = .1) +
  scale_size_continuous(range = c(1,10)) +
  ggsci::scale_color_jama() +
  scale_y_continuous(labels = scales::percent_format(1)) +
  theme_minimal() +
  labs(
    size = "Arrivals",
    color = "Year",
    title = "Relation between 2021 Stringency and April Flights",
    subtitle = "Linear Regression Line with R^2 of 0.46",
    x = "Mean Stringency Index",
    y = "% Change Between April '19 and '21"
  ) +
  theme(
    text = element_text(family = "Bahnschrift",size = 16),
    strip.background = element_rect(color = "black"),
    panel.background = element_rect(color = "black"),
    legend.position = "top",
    plot.caption = element_text(color = "grey40",size = 6)
  )

ggsave("lin_model.png", width = 8, height = 6, dpi = 320)

model1 <- lm(rest_sum$pct ~ rest_sum$ind_mean)

summary(model1)
