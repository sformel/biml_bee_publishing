# Load required libraries -------------------------------------------------

library(tidyverse)
library(httr2)
library(lubridate)

# Define regex for valid scientific names ---------------------------------

sciname_regex <- '^([A-Z][a-zA-Z-]+) ([a-z][a-zA-Z-]+)( (subsp\\. |var\\. |f\\. |sp\\. )([a-z][a-zA-Z-]+))?$'

# Read input data ---------------------------------------------------------

d <- readr::read_delim(file = "data/USGS_DRO_flat.txt.gz", delim = '$')

#write out auto-detected problems to csv
problems(d) %>% 
  mutate(colname = names(d)[col],
         ID = d$ID.[row]) %>% 
  select(row, ID, col, colname, expected, actual) %>% 
  write_csv('output/BIML_problems_identified_during_read-in.csv')

# Identify and summarize invalid scientific names -------------------------

d %>% 
  filter(!str_detect(name, pattern = sciname_regex)) %>% 
  count(name) %>% 
  arrange(desc(n)) %>% 
  readr::write_csv('output/BIML_flag_summary_binomial_names.csv')

# Begin flagging invalid or suspicious data -------------------------------

FLAGGED <- d %>%
  mutate(
    flags = NA,  # initialize flag column
    
    # Parse incomplete ISO date-time strings after removing 'x' placeholders
    time1 = str_remove_all(time1, 'x') %>% parse_date_time(., c("Y", "Ym", "Ymd", "YmdH", "YmdHM", "YmdHMs")),
    time2 = str_remove_all(time2, 'x') %>% parse_date_time(., c("Y", "Ym", "Ymd", "YmdH", "YmdHM", "YmdHMs")),
    
    # Simplify dates where time1 is improperly after time2
    time1 = case_when(
      time1 > time2 ~ strftime(time1, format = "%Y-%m-%d"),
      TRUE ~ as.character(time1)
    ),
    
    # Format dates into ISO8601 with 'Z' suffix when appropriate
    time1final = case_when(
      time1 != 'NA' & !str_detect(time1, '\\s') ~ time1,
      str_detect(time1, '\\s') ~ paste0(time1, 'Z')
    ),
    time2final = case_when(
      as.character(time2) != 'NA' & !str_detect(as.character(time2), '\\s') ~ as.character(time2),
      str_detect(as.character(time2), '\\s') ~ paste0(time2, 'Z')
    ),
    
    # Construct ISO8601 time interval if both dates are valid
    eventDate = case_when(
      !is.na(time1final) & !is.na(time2final) ~ paste0(time1final, '/', time2final) %>%
        str_replace_all('\\s', 'T')
    ),
    
    # Extract year, month, day components from time1
    year = year(time1),
    month = month(time1),
    day = day(time1),
    
    # Flagging various types of invalid data -------------------------------
    
    invalid_COLLECTION.db = case_when(COLLECTION.db == 'USA' ~ 1, .default = 0),
    
    invalid_latitude = case_when(
      abs(as.numeric(latitude)) > 90 | abs(as.numeric(latitude)) < 0 ~ 1, .default = 0),
    
    invalid_longitude = case_when(
      abs(as.numeric(longitude)) > 180 | abs(as.numeric(longitude)) < 0 ~ 1, .default = 0),
    
    invalid_longitude_double_negative = case_when(stringr::str_detect(string = longitude, pattern = '--') ~ 1, .default = 0),
    
    invalid_time_range = case_when(
      (day > day(time2) & month >= month(time2) & year >= year(time2)) |
        (month > month(time2) & year >= year(time2)) |
        year > year(time2) ~ 1, .default = 0),
    
    invalid_time_in_other_field = case_when(
      str_detect(elevation, 'x') ~ 1, .default = 0),
    
    invalid_time_not_recorded = case_when(
      is.na(time1) & is.na(time2) ~ 1, .default = 0),
    
    invalid_time1_not_recorded = case_when(
      invalid_time_not_recorded == 0 & is.na(time1) ~ 1, .default = 0),
    
    invalid_time2_not_recorded = case_when(
      invalid_time_not_recorded == 0 & is.na(time2) ~ 1, .default = 0),
    
    invalid_date_unlikely = case_when(
      DeterminedWhen < '1900-01-01' ~ 1, .default = 0),
    
    # Convert full country names to ISO2 codes
    countryCode = countrycode::countrycode(
      country,
      origin = 'country.name',
      destination = 'iso2c',
      warn = TRUE,
      nomatch = NA
    ),
    
    invalid_country = case_when(
      is.na(countryCode) & !is.na(country) ~ 1, .default = 0),
    
    # Validate that DeterminedWhen is a proper date
    invalid_DeterminedWhen = case_when(
      !is.Date(DeterminedWhen) ~ 1, .default = 0)
  ) %>%
  
  # Reshape and summarize flags per record --------------------------------

select(ID., starts_with('invalid')) %>%
  pivot_longer(cols = starts_with('invalid'), names_to = 'flag', values_to = 'value') %>%
  filter(value > 0) %>%
  select(-value) %>%
  group_by(ID.) %>%
  summarise(values = paste(flag, collapse = " | ")) %>%
  glimpse()

# Export flag summary tables ----------------------------------------------

# Summary of unique flag combinations
FLAGGED %>%
  count(values) %>%
  arrange(desc(n)) %>%
  rename(flags = values, `record count` = n) %>%
  readr::write_csv('output/BIML_flag_summary_table.csv')

# Full list of flagged records with flag details
FLAGGED %>%
  mutate(length = nchar(values)) %>%
  arrange(desc(length)) %>%
  mutate(ID = ID., flags = values, .keep = 'none') %>%
  readr::write_csv('output/BIML_flag_table.csv')