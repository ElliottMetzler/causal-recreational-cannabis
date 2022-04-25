gc()
rm(list = ls())

df <- read_csv(here("data", "clean", "clean.csv"),
               show_col_types = F) %>% as.data.frame

out <- df %>%
  # Filter out states that legalized pot at some point
  filter(state == "Colorado" | ever_legalized == 0) %>% 
  
  # Generate synthetic control models
  synthetic_control(
    outcome = crude_death_rate,
    unit = state,
    time = year,
    i_unit = "Colorado",
    i_time = 2012,
    generate_placebos = T) %>% 
  
  # Include all the potential predictors as means
  generate_predictor(time_window = 2006:2012,
                     across(female_prop:mean_hrs_worked, mean)) %>%
  
  # Add specific predictors for a couple things
  # generate_predictor(time_window = 2008,
  #                    under_30_prop_2008 = under_30_prop) %>%
  # generate_predictor(time_window = 2010,
  #                    under_30_prop_2010 = under_30_prop) %>%
  # 
  # generate_predictor(time_window = 2008,
  #                    black_prop_2008 = black_prop) %>%
  # generate_predictor(time_window = 2010,
  #                    black_prop_2010 = black_prop) %>%

  # Add lagged predictors
  # generate_predictor(time_window = 2008,
  #                    cdr_2008 = crude_death_rate) %>% 
  # generate_predictor(time_window = 2009,
  #                    cdr_2009 = crude_death_rate) %>% 
  # generate_predictor(time_window = 2010,
  #                    cdr_2010 = crude_death_rate) %>%
  
  # Generate Weights
  generate_weights(optimization_window = 2006:2012,
                   margin_ipop = 0.02,
                   sigf_ipop = 7,
                   bound_ipop = 6) %>% 
  
  generate_control()

out %>% plot_trends()
out %>% plot_differences()
out %>% plot_weights()

out %>% grab_balance_table()

out %>% plot_placebos()
out %>% plot_mspe_ratio()

out %>% grab_signficance()

out %>% grab_synthetic_control()
