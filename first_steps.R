library(tidyverse)
library(TidyABS)
library(testthat)
library(lubridate)
library(readxl)
library(janitor)

cpi_compt <- read_excel(path = './Data/consumer price index  - 2018 weighting pattern.xls', sheet = 'Table 4', range = 'A8:C139', col_names = c('cat', 'sub_cat', 'exp_cl')) %>% map_df(tolower) %>% 
  mutate_if(is_character, str_trim) %>%
  mutate_if(is_character, str_replace_all, " ", "_") %>%
  mutate_if(is_character, str_replace_all, "-", "_") %>% 
  mutate_if(is_character, str_replace_all, ",", "") 

seperate_cpi_decomp <- read_excel(path = './Data/cpi_category_break_down.xlsx', sheet = 'sub_cat')

# cpi_cats <- tibble(
#   category     = list(distinct(cpi_compt, cat) %>% filter(complete.cases(.))),
#   sub_category = list(distinct(cpi_compt, sub_cat) %>% filter(complete.cases(.))),
#   exp_class    = list(distinct(cpi_compt, exp_cl) %>% filter(complete.cases(.)))
#   )

category <- distinct(cpi_compt, cat) %>% filter(complete.cases(.)) %>% map_df(tolower) %>% 
  mutate_if(is_character, str_trim) %>%
  mutate_if(is_character, str_replace_all, " ", "_") %>%
  mutate_if(is_character, str_replace_all, "-", "_") %>% 
  mutate_if(is_character, str_replace_all, ",", "") 

sub_category <- distinct(cpi_compt, sub_cat) %>% filter(complete.cases(.)) %>% map_df(tolower) %>% map_df(tolower) %>% 
  mutate_if(is_character, str_trim) %>%
  mutate_if(is_character, str_replace_all, " ", "_") %>%
  mutate_if(is_character, str_replace_all, "-", "_") %>% 
  mutate_if(is_character, str_replace_all, ",", "")

exp_class <- distinct(cpi_compt, exp_cl) %>% filter(complete.cases(.)) %>% map_df(tolower) %>% 
  map_df(tolower) %>% 
  mutate_if(is_character, str_trim) %>%
  mutate_if(is_character, str_replace_all, " ", "_") %>%
  mutate_if(is_character, str_replace_all, "-", "_") %>% 
  mutate_if(is_character, str_replace_all, ",", "")


df <- ABS_narrative_augment_funct('./Data/640105.xls') %>% 
  group_by(level_1, level_2, level_3, unit) %>% 
  nest() %>% 
    mutate_if(is_character, str_trim) %>%
    mutate_if(is_character, str_replace_all, " ", "_") %>%
    mutate_if(is_character, str_replace_all, "-", "_") %>% 
    mutate_if(is_character, str_replace_all, ",", "") %>%  
  distinct(level_1, level_2, level_3, unit, .keep_all = T) %>% 
  unnest()

# Initialising date variables for variable_text function - minus 4 and 10 years respecitvely
date_minus_4_years <- max(df$period)
year(date_minus_4_years) <- year(date_minus_4_years) - 4
date_minus_10_years <- max(df$period)
year(date_minus_10_years) <- year(date_minus_10_years) - 10

current_period <- max(df$period)

# By removing level_2 from this filter operation you get a list of the most recent percentage changes from the previous period.
pct_change_prev_period <- df %>% 
  filter(level_1 == 'percentage_change_from_previous_period', period == current_period) %>% 
  select(level_2, obs)

pct_change_prev_year <- df %>% 
  filter(level_1 == 'percentage_change_from_corresponding_quarter_of_previous_year', period == current_period) %>% 
  select(level_2, obs)

table_1 <- left_join(category, pct_change_prev_period, by = c('cat' = 'level_2')) %>% 
  arrange(desc(obs)) %>% 
  rename(pct_prev_period = obs,
         category = cat) %>% 
  distinct(category, pct_prev_period)
  left_join(pct_change_prev_year, by = c('category' = 'level_2')) %>% 
  rename(pct_prev_year = obs)

# putting growth figures next to sub_cat's
sub_cat <- read_excel(path = './Data/cpi_category_break_down.xlsx', sheet = 'sub_cat') %>% 
  mutate_if(is_character, str_trim) %>%
  mutate_if(is_character, str_replace_all, " ", "_") %>%
  mutate_if(is_character, str_replace_all, "-", "_") %>% 
  mutate_if(is_character, str_replace_all, ",", "") %>% 
  mutate_if(is_character, tolower)

exp_cl <- seperate_cpi_decomp <- read_excel(path = './Data/cpi_category_break_down.xlsx', sheet = 'exp_cl') %>% 
  mutate_if(is_character, str_trim) %>%
  mutate_if(is_character, str_replace_all, " ", "_") %>%
  mutate_if(is_character, str_replace_all, "-", "_") %>% 
  mutate_if(is_character, str_replace_all, ",", "")  %>% 
  mutate_if(is_character, tolower)


# create a partial and map category names over to generate a dataframe - map_dfr(unique(cpi_compt$cat), cat_df)

```{r}
Alcohol and tobacco prices rose by 3.2 per cent in the quarter to be 6.8 per cent higher through the year. 
The main contributor to the rise was tobacco, which rose by 9.4 per cent.
```

#This function tests to see that the dataframe being passed to it contains a single, non-null variable.
variable_text <- function(single_tidy_variable) {
  context('Data Quality --- variable_text')
  test_df <- distinct(single_tidy_variable, level_1, level_2, level_3, obs)
  test_that("Data quality test for df passed to variable_text function", {
    expect_equal(nrow(distinct(single_tidy_variable, level_1, level_2, level_3)), 1)
    expect_true(all(is.na(distinct(pp_delta, level_1, level_2, level_3)) == FALSE))
    
  })
  
  
  
  
  if (current_per_obs > percentile_95) {
  'This is a really high number'
  }

}



gghighlight::gghighlight_point(dg, aes(x = period[as.Date(period) > '1990-01-01'], y = obs), predicate = obs > quantile(dg$obs, na.rm = T, probs = .95), use_direct_label = F)
