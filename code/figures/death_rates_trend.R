gc()
rm(list = ls())


plot <- read_csv(here("data", "raw",
                      "NCHS_-_Drug_Poisoning_Mortality_by_State__United_States.csv"),
                 show_col_types = F) %>% 
  clean_names() %>% 
  select(state,
         year,
         cdr = crude_death_rate) %>% 
  filter(year >= 2000, year <= 2018) %>% 
  group_by(year) %>% 
  mutate(us_average_cdr = cdr %>% mean()) %>% 
  ungroup() %>% 
  pivot_longer(cols = cdr:us_average_cdr,
               names_to = "type") %>% 
  filter(state == "Colorado") %>% 
  ggplot() +
  geom_line(aes(x = year, y = value, color = type),
            size = 1) +
  labs(x = "Year",
       y = "Drug Poisoning Death Rate",
       color = "") +
  scale_color_discrete(labels = c("Colorado", "U.S. Average")) +
  theme_minimal()

ggsave(here("figures/death_rates_trend.jpg"),
       plot,
       width = 8, height = 6, units = "in")
