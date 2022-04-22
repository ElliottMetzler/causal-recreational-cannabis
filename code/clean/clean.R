gc()
rm(list = ls())

#### Read and Clean Ipums Data ####
data <- read_csv(here("data/raw/usa_00005.csv"),
                  show_col_types = F) %>% 
  clean_names() %>% 
  select(-sample,
         -serial,
         -cbserial,
         -hhwt,
         -cluster,
         -strata,
         -gq,
         -pernum,
         -perwt,
         -raced,
         -hispand,
         -educd,
         -empstatd) %>%
  mutate(sex = if_else(sex == 1, "male_prop", "female_prop"),
         age = case_when(age < 30 ~ "under_30_prop",
                         age < 50 ~ "under_50_prop",
                         T ~ "over_50_prop"),
         race = case_when(race == 1 ~ "white_prop", 
                          race == 2 ~ "black_prop",
                          race == 3 ~ "american_indian_prop",
                          race == 4 ~ "asian_prop",
                          race == 5 ~ "asian_prop",
                          race == 6 ~ "asian_prop",
                          T ~ "other_race_prop"),
         marst = 
           case_when(marst == 1 ~ "married_prop",
                     marst == 2 ~ "married_prop",
                     marst == 3 ~ "not_married_prop",
                     marst == 4 ~ "not_married_prop",
                     marst == 5 ~ "not_married_prop",
                     marst == 6 ~ "not_married_prop"),
         educ = 
           case_when(educ < 6 ~ "less_hs_prop",
                     educ == 6 ~ "hs_prop",
                     educ < 10 ~ "some_college_prop",
                     educ == 10 ~ "college_prop",
                     educ > 10 ~ "higher_college_prop"))

# Function to calculate demographic proportions and organize wide
calculate_demographic_proportions <- function(df, type_var) {
  type_var <- enquo(type_var)
  df %>% 
    count(year, statefip, !!type_var) %>%
    group_by(year, statefip) %>% 
    mutate(total = sum(n)) %>%
    ungroup() %>% 
    mutate(prop = n / total) %>% 
    select(-n, -total) %>% 
    pivot_wider(id_cols = c("year", "statefip"),
                names_from = !!type_var,
                values_from = prop)
}

# Combo demo props data
clean_data <- list()
clean_data[["sex"]] <- calculate_demographic_proportions(data, sex)
clean_data[["age"]] <- calculate_demographic_proportions(data, age)
clean_data[["race"]] <- calculate_demographic_proportions(data, race)
clean_data[["marst"]] <- calculate_demographic_proportions(data, marst)
clean_data[["educ"]] <- calculate_demographic_proportions(data, educ)

# Calculate Means Data
mean_cols <- c("nchild", "nchlt5", "uhrswork")
means <- data %>% 
  group_by(year, statefip) %>% 
  summarise(across(mean_cols, mean)) %>% 
  ungroup()

colnames(means) <- c("year",
                     "statefip",
                     "mean_children", 
                     "mean_children_u5", 
                     "mean_hrs_worked")

clean_data[["means"]] <- means

rm(data, mean_cols, means)

#### Read and clean Drug Death Data ####

# Fips Mapping
fips_map <- read_csv(here("data", "raw", "state_fips_map.csv"),
                     show_col_types = F) %>% 
  clean_names() %>% 
  rename(state = name,
         statefip = fips) %>% 
  select(-postal_code)

# Read, Merge, select
data <- read_csv(here("data", "raw",
                      "NCHS_-_Drug_Poisoning_Mortality_by_State__United_States.csv"),
                 show_col_types = F) %>% 
  clean_names() %>% 
  select(state:upper_confidence_limit_for_crude_rate) %>% 
  left_join(fips_map, by = "state") %>% 
  mutate(statefip = if_else(state == "District of Columbia", 
                            11,
                            statefip))

#### Combine All and Export ####

clean_data %>% 
  reduce(left_join, by = c("year", "statefip")) %>% 
  left_join(data, by = c("year", "statefip")) %>% 
  mutate(pot = if_else(year >= 2012 & statefip == 8, 1, 0)) %>% 
  select(year, 
         statefip, 
         pot,
         state:upper_confidence_limit_for_crude_rate,
         everything()) %>% 
  write_csv(here("data/clean/clean.csv"))

rm(clean_data, data, fips_map, calculate_demographic_proportions)
