library(tidyverse)
library(extrafont)


apm_report <- read_csv("apm_report.csv") %>%
  janitor::clean_names() %>%
  mutate(date_fmt = lubridate::dmy(paste("01",date, sep = "-"))) %>%
  filter(is.na(date_fmt) == FALSE)

largest <- apm_report %>%
  group_by(facility) %>%
  filter(date_fmt >= as.Date("2019-01-01")) %>%
  summarise(flights = sum(actual_departures)) %>%
  arrange(desc(flights))

apt_use <- top_n(largest,25)[,1]

aus_report <- apm_report %>%
  filter(facility %in% apt_use$facility) %>%
  filter(date_fmt >= as.Date("2019-01-01")) %>%
  mutate(open = case_when(
    facility %in% c("LAX","SFO","LGA","JFK") ~ "NY/CA",
    T ~ "Other"
  ))

aus_comp <- aus_report %>%
  filter(lubridate::month(date_fmt) == 4 &
           lubridate::year(date_fmt) %in% c(2019,2021))

aus_pct <- aus_comp %>%
  group_by(facility) %>%
  mutate(year = lubridate::year(date_fmt),pct = (actual_arrivals-lag(actual_arrivals))/ lag(actual_arrivals)) %>%
  select(facility,year, pct,actual_arrivals) 

aus_pct1 <- aus_pct %>%
  select(facility, year, actual_arrivals) %>%
  pivot_wider(values_from = actual_arrivals, names_from = year) 

aus_pct <- aus_pct %>%
  filter(is.na(pct) == FALSE) %>%
  left_join(aus_pct1, by = "facility")

  


aus_report$facility <- fct_relevel(aus_report$facility, levels = aus_pct$facility)

ggplot(aus_report, aes(x = date_fmt)) +
  geom_path(aes(y = actual_arrivals, color = open),size = 1.25) +
  geom_path(data = aus_comp, aes(y = actual_arrivals)) +
  geom_label(data = aus_pct, aes( y= 1.1 * actual_arrivals,
                                  x = as.Date("2021-03-01"),
                                  label = scales::percent(pct,1)),
             size = 3) +
  theme_minimal() +
  ggsci::scale_color_aaas() +
  facet_wrap(vars(facility), scales= "free_y") +
  scale_x_date(labels = scales::date_format(format = "%m-%y")) +
  theme(
    text = element_text(family = "Bahnschrift",size = 16),
    strip.background = element_rect(color = "black"),
    panel.background = element_rect(color = "black"),
    axis.text = element_blank(),
    legend.position = "top"
  ) +
  labs(
    title = "Airports Responding to State Policy",
    subtitle = "Comparing April 2019 to April 2021",
    y = "Actual Arrivals",
    x = "Monthly Aggregates",
    color = "State"
  )

#ggsave("airports.png", width = 8, height = 6, dpi = 320)


state_ind <- tibble(
  name = state.name,
  abb = state.abb
)

airport_state <- tibble(
  airport = apt_use$facility,
  state = c("GA","IL","TX","CO","NC","CA","WA","AZ","MI","TX","NV","MN","CA",
            "FL","PA","NJ","NY","UT","MA","VA","NY","MD","FL","FL","CA")
) %>%
  left_join(state_ind, by = c("state" = "abb"))

airport_sum <- aus_pct %>%
  left_join(airport_state, by = c("facility" = "airport"))
