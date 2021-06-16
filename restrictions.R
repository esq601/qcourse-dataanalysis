library(tidyverse)


restr <- read_csv("https://raw.githubusercontent.com/OxCGRT/USA-covid-policy/master/data/OxCGRT_US_latest.csv")


rest_lg <- restr %>%
  janitor::clean_names() %>%
  mutate(date = lubridate::ymd(date))

states <- rest_lg %>%
  filter(region_name %in% c("California","New York","Texas","Florida","Michigan","Illinois"))


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

ggsave("restriction.png", width = 8, height = 6, dpi = 320)


