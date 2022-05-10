gc()
rm(list = ls())

df <- read_csv(here("data", "clean", "clean_2000.csv"),
               show_col_types = F)

#### Summary of Marijuana Legalization by State

df %>%
  select(state, year_legalized) %>% 
  filter(year_legalized != 0) %>% 
  distinct() %>% 
  arrange(year_legalized) %>% 
  kbl(caption = "Legalization of Recreational Cannabis by State",
      col.names = c("State", "Year"),
      booktabs = T,
      format = "latex",
      label = "tab:rollout") %>% 
  kable_styling(latex_option = c("striped", "HOLD_position")) %>% 
  write_lines(here("tables", "rollout.tex"))
