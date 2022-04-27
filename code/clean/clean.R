gc()
rm(list = ls())

#### Read and Clean Ipums Data ####

data <- read_csv(here("data/raw/usa_00006.csv.gz"),
                  show_col_types = F) %>%
  clean_names() %>% as.data.frame()

#### Get Dimensions of the raw data set ####

print("Dimensions of the Raw IPUMs Data")
print(data %>% dim())
print("Dimensions after filtering out 0 for empstat")
print(data %>% filter(empstat != 0) %>% dim())
print("Dimensions after filtering out 999999 for incwage")
print(data %>% filter(empstat != 0, incwage != 999999) %>% dim())

# Thus, we only lose a few entries that are probably weird data. we can drop these

#### Mutate and Summarize IPUMs Data ####

data %<>% 
  filter(empstat != 0,
         incwage != 999999) %>% 
  select(-sample,
         -serial,
         -hhwt,
         -cluster,
         -strata,
         -gq,
         -pernum,
         -perwt,
         -raced,
         -hispand,
         -educd,
         -empstatd,
         -ftotinc,
         -classwkr,
         -classwkrd,
         -occ,
         -citizen,
         -school) 

data %<>%  
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
         hispan = case_when(hispan == 0 ~ "not_hispanic_prop",
                            hispan > 0 ~ "hispanic_prop"),
         marst =
           case_when(marst == 1 ~ "married_prop",
                     marst == 2 ~ "married_prop",
                     marst == 3 ~ "not_married_prop",
                     marst == 4 ~ "not_married_prop",
                     marst == 5 ~ "not_married_prop",
                     marst == 6 ~ "not_married_prop"),
         educ =
           case_when(educ < 6 ~ "less_than_hs_prop",
                     educ == 6 ~ "hs_prop",
                     educ < 10 ~ "some_college_prop",
                     educ == 10 ~ "college_prop",
                     educ > 10 ~ "more_than_college_prop"),
         empstat =
           case_when(empstat == 1 ~ "employed_prop",
                     empstat > 1 ~ "not_employed_prop"))

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
clean_data[["hispan"]] <- calculate_demographic_proportions(data, hispan)
clean_data[["marst"]] <- calculate_demographic_proportions(data, marst)
clean_data[["educ"]] <- calculate_demographic_proportions(data, educ)
clean_data[["employment_status"]] <- calculate_demographic_proportions(data, empstat)

# Calculate

# Calculate Means Data
mean_cols <- c("nchild", "nchlt5", "uhrswork")
means <- data %>% 
  group_by(year, statefip) %>% 
  summarise(across(all_of(mean_cols), mean),
            incwage = median(incwage)) %>% 
  ungroup()

colnames(means) <- c("year",
                     "statefip",
                     "mean_children", 
                     "mean_children_u5", 
                     "mean_hrs_worked",
                     "median_income")

clean_data[["means"]] <- means

data2 <- data

rm(data, mean_cols, means)


#### Read and clean Drug Death Data ####

# Fips Mapping
fips_map <- read_csv(here("data", "mapping", "state_fips_map.csv"),
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
  select(state:crude_death_rate) %>% 
  left_join(fips_map, by = "state")

#### Combine All and Export ####

clean_data %>% 
  reduce(left_join, by = c("year", "statefip")) %>% 
  left_join(data, by = c("year", "statefip")) %>% 
  mutate(pot = if_else(year >= year_legalized & year_legalized !=0, 1,0)) %>% 
  select(year,
         statefip,
         state,
         year_legalized,
         pot,
         ever_legalized,
         deaths:crude_death_rate,
         everything()) %>% 
  arrange(statefip)  %>%
  write_csv(here("data", "clean", "clean_2000.csv"))

rm(clean_data, data, fips_map, calculate_demographic_proportions)
