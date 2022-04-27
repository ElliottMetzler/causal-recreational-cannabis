gc()
rm(list = ls())

df <- read_csv(here("data", "clean", "clean_2000.csv"),
               show_col_types = F) %>% as.data.frame

#### Variable Summary (Means and Standard Deviations) ####

df %>% 
  select(deaths:crude_death_rate,
         male_prop, female_prop,
         under_30_prop, under_50_prop, over_50_prop,
         american_indian_prop:not_married_prop,
         hispanic_prop, not_hispanic_prop,
         less_than_hs_prop, hs_prop, some_college_prop, college_prop, more_than_college_prop,
         employed_prop, 
         not_employed_prop, 
         mean_hrs_worked, 
         median_income,
         mean_children,
         mean_children_u5) %>% 
  summarise(across(everything(),
                   .fns = list(mean = mean,
                               sd = sd),
                   .names = "{.col}-{.fn}")) %>% 
  pivot_longer(cols = everything(),
               names_to = c("Variable", "Statistic"),
               names_sep = "-") %>% 
  pivot_wider(id_cols = Variable,
              names_from = Statistic,
              values_from = value) %>%
  mutate(across(mean:sd, ~round(.x,3)),
         Variable = Variable %>% 
           str_replace_all("_", " ") %>%
           str_to_title() %>% 
           str_replace("Prop", "") %>% 
           str_replace("U5", "Under 5 years old") %>% 
           str_replace("Hs", "High School")) %>% 
  kbl(caption = "Variable Summary",
      col.names = c("Variable", "Mean", "Std. Dev"),
      booktabs = T,
      format = "latex",
      label = "var_summary") %>% 
  kable_styling(latex_option = c("striped", "HOLD_position")) %>%
  pack_rows("Drug Poisonings", 1,3) %>%
  pack_rows("Gender Population Proportions", 4,5) %>% 
  pack_rows("Age Population Proportions", 6,8) %>% 
  pack_rows("Race Population Proportions", 9,13) %>%
  pack_rows("Hispanic Population Proportions", 14,15) %>% 
  pack_rows("Marital Status Population Proportions", 16, 17) %>% 
  pack_rows("Education Population Proportions", 18, 22) %>% 
  pack_rows("Employment Population Proportions and Summary Statistics", 23, 26) %>% 
  pack_rows("Children Summary Statistics", 27,28) %>% 
  write_lines(here("tables", "var_summary.tex"))
