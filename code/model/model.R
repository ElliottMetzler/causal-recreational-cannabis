gc()
rm(list = ls())

df <- read_csv(here("data", "clean", "clean.csv"),
               show_col_types = F) %>% as.data.frame
state_mapping <- read_csv(here("data", "state_mapping.csv"),
               show_col_types = F) %>% as.data.frame %>% select(-State, -State_Abbr)
df <- inner_join(df,state_mapping, by = c("statefip" ="STATEFIP"))

treatment_fip <- 8
treatment_year <- 2012
last_pretreat_year <- treatment_year - 1
data_start <- min(df$year)
data_end <- max(df$year)

predictors_list <- df %>% 
  select(female_prop:mean_hrs_worked) %>% 
  colnames()

controls_list <- df %>% 
  select(statefip, Ever_Legalized) %>%
  filter(statefip != treatment_fip & Ever_Legalized != 1) %>% select(-Ever_Legalized) %>% distinct() %>% pull()

# Need to double check a few things here
# Thing 1 - year cut offs throughout (last year before treatment or at treatment?)
# Thing 2 - how to decide between normal predictors and special predictors
# Thing 3 - what is the mean summary thing doing?

data_prep_out <- dataprep(
  foo = df,
  predictors = predictors_list,
  predictors.op = "mean",
  time.predictors.prior = data_start:last_pretreat_year,
  # special.predictors = ,
  dependent = "crude_death_rate",
  unit.variable = "statefip",
  unit.names.variable = "state",
  time.variable = "year",
  treatment.identifier = treatment_fip,
  controls.identifier = controls_list,
  time.optimize.ssr = data_start:last_pretreat_year,
  time.plot = data_start:data_end
)

synth_out <- synth(data.prep.obj = data_prep_out)

path.plot(synth_out, data_prep_out)
gaps.plot(synth_out, data_prep_out)
