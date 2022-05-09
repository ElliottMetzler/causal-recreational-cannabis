gc()
rm(list = ls())

df <- read_csv(here("data", "clean", "clean_2000.csv"),
               show_col_types = F) %>% as.data.frame

#### Functions ####

# Model Runner
run_synth_model <- function(state_of_interest) {
  df %>% 
    filter(state == state_of_interest | ever_legalized == 0) %>% 
    synthetic_control(
      outcome = crude_death_rate,
      unit = state,
      time = year,
      i_unit = state_of_interest,
      i_time = 2012,
      generate_placebos = T) %>% 
    generate_predictor(time_window =2000:2012,
                       across(female_prop:median_income, mean)) %>% 
    generate_weights(optimization_window = 2000:2012,
                     margin_ipop = 0.02,
                     sigf_ipop = 7,
                     bound_ipop = 6) %>% 
    generate_control()
}


# Create Unit Weight Table
unit_weight_table <- function(synth_model_object,
                              state_of_interest) {
  
  labels_ <- paste0("unit_weight_table_", tolower(state_of_interest))
  output_ <- paste0("tables/", labels_, ".tex")
    
  synth_model_object %>% 
    grab_unit_weights() %>% 
    kbl(caption = "Synthetic Weights",
        col.names = c("Unit", "Weight"),
        booktabs = T,
        format = "latex",
        label = labels_) %>% 
    kable_styling(latex_option = c("striped", "HOLD_position")) %>% 
    write_lines(here(output_))
}

# Create Balance Table
balance_table <- function(synth_model_object,
                          state_of_interest) {
  
  labels_ <- paste0("balance_table_", tolower(state_of_interest))
  output_ <- paste0("tables/", labels_, ".tex")
  
  synth_model_object %>% 
    grab_balance_table() %>% 
    kbl(caption = "Balance Table",
        col.names = c("Variable",
                      state_of_interest, 
                      paste0("Synthetic ", state_of_interest),
                      "Donor Sample"),
        booktabs = T,
        format = "latex",
        label = labels_) %>% 
    kable_styling(latex_option = c("striped", "HOLD_position")) %>% 
    write_lines(here(output_))
}


# Plot Trends

plot_trends_gen_fig <- function(synth_model_object,
                                state_of_interest) {
  plot <- synth_model_object %>% 
    grab_synthetic_control() %>% 
    pivot_longer(cols = c("real_y", "synth_y")) %>% 
    mutate(name = if_else(name == "real_y", 
                          state_of_interest,
                          paste0("Synthetic ", state_of_interest))) %>% 
    ggplot() + 
    geom_line(aes(x = time_unit, y = value, color = name),
              size = 1.5) +
    theme_minimal() +
    labs(x = "Year", 
         y = "Crude Death Rate",
         color = "") +
    geom_vline(xintercept = 2012,
               linetype = "longdash",
               size = 0.75)
  
  output <- paste0("figures/trends_plot_", tolower(state_of_interest), ".jpg")
  ggsave(here(output), plot)
}

# Plot Differences

plot_diffs_gen_fig <- function(synth_model_object, state_of_interest) {
  plot <- synth_model_object %>%
    grab_synthetic_control() %>% 
    mutate(diff = real_y - synth_y) %>% 
    ggplot() + 
    geom_line(aes(x = time_unit, y = diff),
              color = "#F8766D",
              size = 1.5) +
    theme_minimal() +
    labs(x = "Year",
         y = "Difference of Actual and Synthetic") +
    geom_hline(yintercept = 0,
               size = 0.75) +
    geom_vline(xintercept = 2012,
               linetype = "longdash",
               size = 0.75) + 
    annotate("text",
             x = 2009,
             y = -10,
             label = "Legalization occurs in 2012")
  
  output <- paste0("figures/diffs_plot_", tolower(state_of_interest), ".jpg")
  ggsave(here(output), plot)
  
}

# Last one to add is the Ratio Plot Then this is done and I can write it up

# Overall runner function

run_model_create_figures <- function(some_state) {
  
  model_object <- run_synth_model(some_state)
  unit_weight_table(model_object, some_state)
  balance_table(model_object, some_state)
  plot_trends_gen_fig(model_object,some_state)
  plot_diffs_gen_fig(model_object, some_state)

}


#### Generate Outputs ####

run_model_create_figures("Colorado")
run_model_create_figures("Washington")
