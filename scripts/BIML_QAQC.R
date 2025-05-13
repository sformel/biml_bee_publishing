library(tidyverse)
library(httr2)
library(lubridate)

# read in data ------------------------------------------------------------

sciname_regex <- '^([A-Z][a-zA-Z-]+) ([a-z][a-zA-Z-]+)( (subsp\\. |var\\. |f\\. |sp\\. )([a-z][a-zA-Z-]+))?$'                      

d <- readr::read_csv(file = "data/species_projects_joined_new_13Feb2025.csv", quote = '"')

d %>% 
  filter(!str_detect(name, pattern = sciname_regex)) %>% 
  count(name) %>% 
  arrange(desc(n)) %>% 
  readr::write_csv(., 'output/flag_summary_binomial_names.csv')

FLAGGED <- d %>%
  mutate(
    flags = NA,

    time1 = str_remove_all(time1, pattern = 'x') %>% parse_date_time(., c("Y", "Ym", "Ymd", "YmdH", "YmdHM", "YmdHMs")),
    time2 = str_remove_all(time2, pattern = 'x') %>% parse_date_time(., c("Y", "Ym", "Ymd", "YmdH", "YmdHM", "YmdHMs")),

    #generalize bad data where time1 > time2
    time1 = case_when(time1 > time2 ~ strftime(time1, format = "%Y-%m-%d"),
      TRUE ~ time1 %>% as.character()),
    
    time1final = case_when(
      time1 != 'NA' & str_detect(time1, pattern = '\\s', negate = TRUE) ~ time1,
      str_detect(time1, pattern = '\\s') ~ paste0(time1, 'Z')
    ),
    time2final = case_when(
      as.character(time2) != 'NA' & str_detect(time2, pattern = '\\s', negate = TRUE) ~ time2 %>% as.character(),
      str_detect(time2, pattern = '\\s') ~ paste0(time2, 'Z')
    ),
    
    eventDate = case_when(
      !is.na(time1final) &
        !is.na(time2final) ~ paste0(time1final, '/', time2final) %>%
        str_replace_all(., pattern = '\\s', replacement = 'T')
    ),
    year = year(time1),
    month = month(time1),
    day = day(time1),
    
    invalid_COLLECTION.db = case_when(COLLECTION.db == 'USA' ~ 1, .default = 0),
    invalid_latitude = case_when(abs(as.numeric(latitude)) > 90 |
                                   abs(as.numeric(latitude)) < 0 ~ 1, .default = 0),
    invalid_longitude = case_when(abs(as.numeric(longitude)) > 180 |
                                    abs(as.numeric(longitude)) < 0 ~ 1, .default = 0),
    invalid_time_range = case_when((day > day(time2) & month >= month(time2) & year >= year(time2)) |
                                     (month > month(time2) &
                                     year >= year(time2)) | 
                                     year > year(time2) ~ 1, .default = 0),
    invalid_time_in_other_field = case_when(str_detect(elevation, 'x') ~ 1, .default = 0),
    invalid_time_not_recorded = case_when(is.na(time1) & is.na(time2) ~ 1, .default = 0),
    invalid_time1_not_recorded = case_when(invalid_time_not_recorded == 0 & is.na(time1) ~ 1, .default = 0),
    invalid_time2_not_recorded = case_when(invalid_time_not_recorded == 0 & is.na(time2) ~ 1, .default = 0),
    invalid_date_unlikely = case_when(DeterminedWhen < '1900-01-01' ~ 1, .default = 0),
    countryCode = countrycode::countrycode(country, 
                             origin = 'country.name', 
                             destination = 'iso2c', 
                             warn = TRUE, 
                             nomatch = NA), #converted to two-letter code
    invalid_country = case_when(is.na(countryCode) & !is.na(country) ~ 1, .default = 0),
    
    #other checks
    invalid_DeterminedWhen = case_when(!is.Date(DeterminedWhen) ~ 1, .default = 0),
    
    ) %>% 
  select(ID., starts_with('invalid')) %>% 
  pivot_longer(cols = starts_with('invalid'), names_to = 'flag', values_to = 'value') %>% 
  filter(value > 0) %>%
  select(-value) %>% 
  group_by(ID.) %>%
  summarise(values = paste(flag, collapse = " | ")) %>% 
  glimpse()


FLAGGED %>% count(values) %>% arrange(desc(n)) %>% 
  rename(flags = values, 'record count' = n) %>% 
  readr::write_csv(., 'output/flag_summary_table.csv')

FLAGGED %>%
  mutate(length = nchar(values)) %>%
  arrange(desc(length)) %>% 
  mutate(ID = ID., flags = values, .keep = 'none') %>%
  readr::write_csv(., 'output/flag_table.csv')
