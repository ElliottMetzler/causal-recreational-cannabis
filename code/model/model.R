gc()
rm(list = ls())

df <- read_csv(here("data", "clean", "clean_2000.csv"),
               show_col_types = F) %>% as.data.frame

#### Functions ####

# Model
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
    arrange(weight %>% desc) %>% 
    mutate(weight = weight %>% round(3)) %>% 
    filter(weight > 0.001) %>%
    kbl(caption = paste0("Synthetic ",
                         state_of_interest,
                         " Donor State Weights"),
        col.names = c("Unit", "Weight"),
        booktabs = T,
        format = "latex",
        label = labels_) %>%
    add_footnote("Table excludes donor states with less than 0.1 percent model weight.") %>% 
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
    mutate(variable = variable %>% 
             str_replace_all("_", " ") %>% 
             str_replace_all("prop", "Proportion") %>%
             str_to_title(),
           variable = variable %>% str_replace("Hs", "High School"),
           across(-variable, ~round(.x,3))) %>%
  kbl(caption = paste0("Synthetic ",
                       state_of_interest,
                       " Predictor Variable Balance Summary"),
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
  ggsave(here(output), plot, width = 8, height = 5, units = "in")
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
  ggsave(here(output), plot, width = 8, height = 5, units = "in")
  
}

# MSPE Figure
plot_mspe_gen_fig <- function(synth_model_object, state_of_interest) {
  plot <- synth_model_object %>% 
    grab_signficance() %>%
    ggplot() +
    geom_col(aes(x = mspe_ratio,
                 y = fct_reorder(unit_name, mspe_ratio),
                 fill = type)) +
    scale_fill_manual(values = c("grey", "#F8766D")) +
    labs(x = "Postperiod MSPE / Preperiod MSPE",
         y = "State") +
    theme_minimal() +
    theme(legend.position="none")

  output <- paste0("figures/mspe_plot_", tolower(state_of_interest), ".jpg")
  ggsave(here(output), plot, width = 8, height = 6, units = "in")
  
}

# Generate a list of states where pre-period RMSPE is less than or equal to 
# 2 times the unit's pre-period RMSPE
get_pruned_placebos_list <- function(model_object) {
  
  prune_threshold <- model_object %>% 
    grab_signficance() %>% 
    filter(type == "Treated") %>% 
    pull(pre_mspe) %>% sqrt() * 2
  
  model_object %>% 
    grab_signficance() %>% 
    mutate(pre_rmspe = pre_mspe %>% sqrt()) %>% 
    filter(pre_rmspe <= prune_threshold) %>% 
    pull(unit_name)
}

# Plot Placebos Function
plot_placebos_gen_fig <- function(synth_model_object, state_of_interest) {
  
  prune_list <- synth_model_object %>% get_pruned_placebos_list()
  
  plot_data <- synth_model_object %>% 
    grab_synthetic_control(placebo = T) %>% 
    filter(.id %in% prune_list) %>% 
    mutate(diff = real_y - synth_y)
  
  others <- plot_data %>% filter(.placebo == 1)
  main <- plot_data %>% filter(.placebo == 0)

  color_manual <- rep("grey", length(prune_list))

  plot <- ggplot() +
    geom_line(data = others,
              mapping = aes(x = time_unit, y = diff, color = .id),
              size = 0.75,
              alpha = 0.5) +
    scale_color_manual(values = color_manual) +
    geom_line(data = main, mapping = aes(x = time_unit, y = diff),
              color = "#F8766D",
              size = 1.5) +
    labs(x = "Year",
         y = "Difference of Actual and Synthetic") +
    geom_hline(yintercept = 0,
               size = 0.75) +
    geom_vline(xintercept = 2012,
               linetype = "longdash",
               size = 0.75) +
    theme_minimal() +
    theme(legend.position="none")

  output <- paste0("figures/placebos_plot_", tolower(state_of_interest), ".jpg")
  ggsave(here(output), plot, width = 8, height = 6, units = "in")
}

# Causal Estimate Table
causal_est_table <- function(synth_model_object, state_of_interest) {
  
  labels_ <- paste0("causal_est_table_", tolower(state_of_interest))
  output_ <- paste0("tables/", labels_, ".tex")
  
  
  synth_model_object %>% 
    grab_synthetic_control() %>% 
    filter(time_unit > 2012,
           time_unit %% 2 == 0) %>% 
    mutate(diff = real_y - synth_y,
           time_unit = time_unit - 2012,
           across(real_y:diff, ~round(.x,3))) %>%
    kable(caption = paste0("Estimated Impact of Legalization on 
                         Drug Poisoning Death Rate: ",
                         state_of_interest),
        col.names = c("Years After Legalization",
                      "Actual",
                      "Synthetic",
                      "Estimated Causal Effect"),
        booktabs = T,
        format = "latex",
        label = labels_) %>%
    column_spec(1:4, width = "1in") %>% 
    add_header_above(c("",
                       "Drug Poisoning Death Rate" = 3)) %>% 
    kable_styling(latex_option = c("striped", "HOLD_position")) %>% 
    write_lines(here(output_))
  
}

mean_causal_estimate <- function(synth_model_object, state_of_interest) {
  val <- synth_model_object %>% 
    grab_synthetic_control() %>% 
    filter(time_unit > 2012) %>% 
    mutate(diff = real_y - synth_y) %>% 
    pull(diff) %>% 
    mean() %>% 
    round(2)
  
  print(paste0(state_of_interest," Mean Causal Estimate: ", val))
}

c %>% mean_causal_estimate("Colorado")
w %>% mean_causal_estimate("Washington")
  
  
# Overall runner function

run_model_create_figures <- function(some_state) {
  
  # Instantiate Model
  model_object <- run_synth_model(some_state)
  
  # Tables
  unit_weight_table(model_object, some_state)
  balance_table(model_object, some_state)
  
  # Figures
  plot_trends_gen_fig(model_object,some_state)
  plot_diffs_gen_fig(model_object, some_state)
  plot_mspe_gen_fig(model_object, some_state)
  plot_placebos_gen_fig(model_object, some_state)
  
  # Causal Table
  causal_est_table(model_object, some_state)
  
  # Mean Estimates
  mean_causal_estimate(model_object, some_state)
}

  
#### Generate Outputs ####

run_model_create_figures("Colorado")
run_model_create_figures("Washington")
