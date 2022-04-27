gc()
rm(list = ls())

df <- read_csv(here("data", "clean", "clean_2000.csv"),
               show_col_types = F) %>% as.data.frame

out_col <- df %>%
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
  generate_predictor(time_window = 2000:2012,
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
  generate_weights(optimization_window = 2000:2012,
                   margin_ipop = 0.02,
                   sigf_ipop = 7,
                   bound_ipop = 6) %>% 
  
  generate_control()

out_col %>% plot_trends()
out_col %>% plot_differences()
out_col %>% plot_weights()

out_col %>% grab_balance_table()

out_col %>% plot_placebos()
out_col %>% plot_mspe_ratio()

out_col %>% grab_signficance()


#Same synthetic control code using Washington as the treated state
out_wash <- df %>%
  # Filter out states that legalized pot at some point
  filter(state == "Washington" | ever_legalized == 0) %>% 
  
  # Generate synthetic control models
  synthetic_control(
    outcome = crude_death_rate,
    unit = state,
    time = year,
    i_unit = "Washington",
    i_time = 2012,
    generate_placebos = T) %>% 
  
  # Include all the potential predictors as means
  generate_predictor(time_window = 2000:2012,
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
generate_weights(optimization_window = 2000:2012,
                 margin_ipop = 0.02,
                 sigf_ipop = 7,
                 bound_ipop = 6) %>% 
  
  generate_control()

out_wash %>% plot_trends()
out_wash %>% plot_differences()
out_wash %>% plot_weights()

out_wash %>% grab_balance_table()

out_wash %>% plot_placebos()
out_wash %>% plot_mspe_ratio()

out_wash %>% grab_signficance()

# Code does same thing as plot_placebos
out %>% grab_synthetic_control(placebo = T) %>% 
  mutate(delta =synth_y - real_y) %>% 
  ggplot +
  geom_line(aes(x = time_unit, y = delta, color = .id))
