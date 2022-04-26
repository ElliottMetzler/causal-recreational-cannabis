gc()
rm(list = ls())

df <- read_csv(here("data", "clean", "clean.csv"),
               show_col_types = F) %>% as.data.frame

#### Variable Summary (Means and Standard Deviations) ####

summary_groups <- list()

summary_groups[["main_variables"]] <- c("deaths", "population", "crude_death_rate")
summary_groups[["Gender"]] <- c("male_prop", "female_prop")
summary_groups[["Age"]] <- c("under_30_prop", "under_50_prop", "over_50_prop")
summary_groups[["Race"]] <- df %>% select(american_indian_prop:white_prop) %>% colnames()
summary_groups[["marital_status"]] <- c("married_prop", "not_married_prop")
summary_groups[["Education_and_Work"]] <- df %>% 
  select(college_prop:not_employed_prop, 
         mean_hrs_worked, 
         median_income) %>% 
  colnames()
summary_groups[["Children"]] <- c("mean_children", "mean_children_u5")

# Need to do: Fix the variable naming in the tables so it looks pretty
#             Fix the rounding for the variables so it looks pretty

create_var_summary <- function(df, group) {
  
  group_title_case <- group %>% str_replace_all("_"," ") %>% str_to_title()
  table_caption <- paste0("Variable Summary - ", group_title_case)
  
  group_label_case <- group %>% str_to_lower()
  table_label <- paste0("tab:", group_label_case)
  
  table_file <- paste0(group_label_case, ".tex")
  
  columns <- summary_groups[[group]]
  
  df %>% 
    select(all_of(columns)) %>% 
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
    kbl(caption = table_caption,
        col.names = c("Variable", "Mean", "Std. Dev"),
        booktabs = T,
        format = "latex",
        label = table_label) %>% 
    kable_styling(latex_option = c("striped", "HOLD_position")) %>% 
    write_lines(here("tables", table_file))
}


for (group in names(summary_groups)) {
  
  create_var_summary(df = df, group = group)
  
}
