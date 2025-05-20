library(tidyverse)
library(httr2)
library(lubridate)
library(sf)
library(readr)
library(stringr)

# read in data ------------------------------------------------------------

d <- species_projects_joined_new %>%
  filter(datasetID != 'USFWS GLRI') %>%
  filter(COLLECTION.db != 'USA') %>% #filter out records that need serious QAQC
  mutate(
    verbatimLatitude = latitude,
    verbatimLongitude = longitude,
    
    #clean up double negatives from longitude
    longitude = str_replace(string = longitude, pattern = '--', replacement = '-'),
    
    #make invalid coordinates NA in DwC terms
    decimalLatitude = case_when(abs(as.numeric(latitude)) > 90 ~ NA_real_, TRUE ~ as.numeric(latitude)),
    decimalLongitude = case_when(abs(as.numeric(longitude)) > 180 ~ NA_real_, TRUE ~ as.numeric(longitude)),
    tz = 'UTC', #This is a placeholder that will be corrected later. It's purpose is to get around a non-vectorized function
    
    #clean up sex according to https://doi.org/10.5281/zenodo.14187862
    sex = case_when(
      sex == 'm' ~ 'Male',
      sex == 'f' ~ 'Female',
      sex == 'q' ~ 'Female',
      sex == 'd' ~ 'Female',
      TRUE ~ 'Indeterminate'
    ),
    verbatimEventDate = paste0('time1:', time1, ';', 'time2:', time2)
  )

# Run some checks from errors past
d %>% filter(abs(decimalLatitude) > 90)

# time wrangling ----------------------------------------------------------

# Vectorized ISO 8601 builder
# function written with the help of chatGPT

make_iso8601 <- function(year, month, day, hour, minute, second) {
  case_when(
    is.na(year) ~ NA_character_,
    is.na(month) ~ year,
    is.na(day) ~ str_c(year, month, sep = "-"),
    is.na(hour) ~ str_c(year, month, day, sep = "-"),
    is.na(minute) ~ str_c(year, month, day, "T", hour),
    is.na(second) ~ str_c(year, month, day, "T", hour, ":", minute),
    TRUE ~ str_c(year, month, day, "T", hour, ":", minute, ":", second)
  )
}


#get time zone when there is spatial information, otherwise, assume UTC
with_coords <- d %>% 
  filter(!is.na(decimalLongitude) & !is.na(decimalLatitude))

d$tz[!is.na(d$decimalLongitude) & !is.na(d$decimalLatitude)] <- lutz::tz_lookup_coords(lat = with_coords$decimalLatitude, 
                                                                                       lon = with_coords$decimalLongitude, 
                                                                                       method = "accurate")


d_cleaned <- d %>% 

  #filter(!is.na(time1)&!is.na(time2)) %>% 
  mutate(
    
    #split time strings to help with acrobatics later
    year1 = str_sub(time1, start = 1, end = 4),
    year2 = str_sub(time2, start = 1, end = 4),
    month1 = str_sub(time1, start = 5, end = 6),
    month2 = str_sub(time2, start = 5, end = 6),
    day1 = str_sub(time1, start = 7, end = 8),
    day2 = str_sub(time2, start = 7, end = 8),
    hour1 = str_sub(time1, start = 9, end = 10),
    hour2 = str_sub(time2, start = 9, end = 10),
    minute1 = str_sub(time1, start = 11, end = 12),
    minute2 = str_sub(time2, start = 11, end = 12),
    sec1 = str_sub(time1, start = 13, end = 14),
    sec2 = str_sub(time2, start = 13, end = 14)) %>%
  
  #start building ISO8601 dates
  mutate(across(year1:sec2, ~ na_if(.x, "xx")))  %>% # Replace "xx" with NA and ensure proper formatting
  mutate(
    start_iso8601 = make_iso8601(year1, month1, day1, hour1, minute1, sec1),
    end_iso8601   = make_iso8601(year2, month2, day2, hour2, minute2, sec2)
  )

glimpse(d_cleaned)

# convert times to UTC --------------------------------------------------------------

d_cleaned <- d_cleaned %>% 
  mutate(
    #parse to date
    t1final = parse_date_time(start_iso8601, c("Y","Ym", "Ymd", "YmdH", "YmdHM", "YmdHMs")),
    
    #annotate invalid dates
    bad_start_dates = case_when(is.na(t1final) ~ start_iso8601),
    
    #add time zone
    t1final = force_tz(t1final, tzone = tz),
    
    #parse to date
    t2final = parse_date_time(end_iso8601, c("Y","Ym", "Ymd", "YmdH", "YmdHM", "YmdHMs")),
    
    #annotate invalid dates
    bad_end_dates = case_when(is.na(t1final) ~ end_iso8601),
    
    #add time zone
    t2final = force_tz(t2final, tzone = tz),
    
    # convert to UTC
    t1final = with_tz(time = t1final, tzone = 'UTC'),
    t2final = with_tz(time = t2final, tzone = 'UTC'),
    
    #side trip to make variables for correct interpretation of time in GBIF
    year = year(t1final),
    month = month(t1final),
    day = day(t1final),
    
    #catch bad timestamps
    badTS = case_when(day > day(t2final) & month >= month(t2final) & year >= year(t2final) ~ ID.,
                      month > month(t2final) & year >= year(t2final) ~ ID.,
                      year > year(t2final) ~ ID.),
    
    # #generalize bad data where time1 > time2 by removing time2
    # t1final = case_when(t1final > end_iso8601 ~ strftime(t1final, format="%Y-%m-%d"),
    #                      TRUE ~ t1final %>% as.character()
    #                      ),
    
    #make into character to allow the following parsing
    t1final = as.character(t1final),
    t2final = as.character(t2final),
    
    t1final = case_when(
      t1final != 'NA' & str_detect(t1final, pattern = '\\s', negate = TRUE) ~ t1final,
      str_detect(t1final, pattern = '\\s') ~ paste0(t1final, 'Z')
    ), t2final = case_when(
      t2final != 'NA' & str_detect(t2final, pattern = '\\s', negate = TRUE) ~ t2final %>% as.character(),
      str_detect(t2final, pattern = '\\s') ~ paste0(t2final, 'Z')
    ),

    #replace unknown tz case with ISO8601 format for local time zone
    t1final = case_when(
      is.na(decimalLatitude) | is.na(decimalLongitude) ~ as.character(start_iso8601),
      TRUE ~ t1final),
    
    t2final = case_when(
      is.na(decimalLatitude) | is.na(decimalLongitude) ~ as.character(end_iso8601),
      TRUE ~ t2final),
    
    # If t2 and t2 aren't NA, and don't equal each other, make a range. Otherwise return t1
    eventDate = case_when(!is.na(t1final) & !is.na(t2final) & (t1final != t2final) ~ paste0(t1final, '/', t2final) %>%
                            str_replace_all(., pattern = '\\s', replacement = 'T'),
                          t1final == t2final ~ t1final
                          ),
    
    #revise bad time stamps to t1 dates only for eventDate
    eventDate = case_when(!is.na(badTS) ~ t1final,
                          TRUE ~ eventDate),
    
    
    #revise time zone to NA when spatial data is NA
    tz = case_when(
      is.na(decimalLatitude) | is.na(decimalLongitude) ~ NA_character_,
      TRUE ~ tz)  # keep original tz otherwise
    
  ) %>% glimpse()


#sanity check
glimpse(d_cleaned %>% filter(is.na(tz) & !is.na(start_iso8601)))

# create lookup table for scientificName ----------------------------------

#filter names based on special characters
# / == certain up to genus, uncertainty of species due to evolving taxonomic classification
# ? == certainty up to genus, a little doubt about species

patterns_to_trim_to_genus <- paste(
  c(
    '[^a-zA-Z\\s]',
    '\\sspecies',
    '\\sweird',
    '\\sinteresting',
    '\\simmature',
    '\\sAA',
    '\\sWYCALUT',
    '\\sWY CAL SD',
    '\\sMRL'
  ),
  collapse = '|'
)

patterns_to_filter_out <- c("Deleted",
                            "Destroyed",
                            "Destroyed ",
                            "Destoryed",
                            "Destroy",
                            "destroyed",
                            "Destroyed",
                            "no ID",
                            "Unknown")

# specs_to_check <- c("cf_Villa",
#                     "Specimen_Number_Logged_by_Logan_Name_missing",
#                     "Pompilinae_gen.",
#                     "Parancistrocerus/Stenodynerus",
#                     "Ichneumonidae/Ichneunominae",
#                     "Ichneumonidae/Phygadeuontinae",
#                     "Ichneumonidae/Tryphoninae",
#                     "Figitidae/Eucoilinae",
#                     "Andrena_Trachandrena")                        

d_cleaned <-d_cleaned %>% filter(!name %in% patterns_to_filter_out) %>% glimpse()

d_cleaned %>% 
  filter(str_detect(string = name, pattern = '\\?')) %>% 
  glimpse()

d_cleaned_tax <- d_cleaned %>% 
  mutate(trim2genus = str_remove(string = name, pattern = '\\s.*$'),
         query_names = case_when(str_detect(string = name, pattern = patterns_to_trim_to_genus) ~ trim2genus, #any flags for unusual names
                                 TRUE ~ name))

d_cleaned_tax %>% glimpse()

# No_Bees_Found == bees were searched for, may have caught insects, but no bees were found. So the event should be valid, but 'no bees found' 
# What to do with these?

#filter out above list until checked
query_names_vector <- d_cleaned_tax %>% filter(!str_detect(string = query_names, pattern = paste(patterns_to_filter_out, collapse = '|'))) %>% pull(query_names) %>% unique()

# repair query_names based on above list
query_names_vector[query_names_vector=='Bee'] <- 'Hymenoptera'
query_names_vector[query_names_vector=='Nonbee'] <- 'Insecta'

query_names_vector <- unique(query_names_vector)

# Query GBIF backbone - assuming everything is at least Insecta.
tax_table <- rgbif::name_backbone_checklist(name_data = query_names_vector, 
                                            kingdom = 'Animalia', 
                                            class = 'Insecta')

#not hitting in GBIF
no_GBIF_hit <- tax_table %>% filter(is.na(kingdom)) %>% pull(verbatim_name)

no_GBIF_hit %>% sort()


#check any unaccepted names. 20 are 'Doubtful'
tax_table %>% 
  filter(!status %in% c('ACCEPTED', 'SYNONYM')) %>% 
  select(status, scientificName, verbatim_name) %>% 
  print(n = 1e3)

#per documentation: https://gbif.github.io/gbif-api/apidocs/org/gbif/api/vocabulary/TaxonomicStatus.html
# DOUBTFUL: Treated as accepted, but doubtful whether this is correct.

#check alignment
tax_table %>% select(verbatim_name, scientificName) %>% 
  distinct() %>% 
  print(n = 1e2)

# create lookup table to collectionID
collectionID_table <- lapply(unique(d$datasetID), function(x) {
  
  r <- request(base_url = 'https://api.gbif.org/v1/grscicoll/collection/suggest') %>%
    req_url_query(q = x) %>%
    req_perform() %>%
    resp_body_json()
  
  if(length(r) == 0){
    data.frame(key = NA,
               code = x,
               name = NA)
  } else {
    r[[1]] %>% as_tibble()
  }
}) %>%
  bind_rows()

# wrangle -----------------------------------------------------------------

#create list of valid DwC terms
DwC_terms <- read.csv("https://raw.githubusercontent.com/tdwg/dwc/refs/heads/master/vocabulary/term_versions.csv") %>% 
  filter(status == "recommended") %>% 
  pull(term_localName)

event <- d_cleaned_tax %>% 
  
  mutate(
    
    institutionCode,
    institutionID,
    
    #event terms
    datasetName,
    datasetID = datasetID,
    eventID = COLLECTION.db, #Sam prefix + identifier
    eventRemarks = case_when(is.na(decimalLongitude) ~ paste('Timezone uncertain due to lack of spatial coordinates. Can be approximated as local time based on any other included spatial information (e.g. stateProvince)', 
                                                             field_note, sep = ' | '), # best solution for now, until we have time to parse the variables
                             TRUE ~ field_note),
    samplingProtocol = how0, #pan trap or hand net
    samplingProtocol = case_when(samplingProtocol == 'pan trap' ~ 'bowl trap'),
    sampleSizeValue = case_when(samplingProtocol == 'bowl trap' ~ how1), #number of bowls successful (data exists)
    sampleSizeUnit = case_when(samplingProtocol == 'bowl trap' ~ 'bowl traps collected'),
    samplingEffort = case_when(samplingProtocol == 'bowl trap' ~ '24 hours'),
    
    #time
    verbatimEventDate,
    eventDate,
    year,
    month,
    day,
    
    #location terms
    locationID = site,
    locality = city,
    county,
    stateProvince = state,
    countryCode = countrycode::countrycode(country, 
                                           origin = 'country.name', 
                                           destination = 'iso2c', 
                                           warn = TRUE, 
                                           nomatch = country), #converted to two-letter code
    .keep = 'none') %>%
  
  select(any_of(DwC_terms)) %>% 
  distinct()


occ <- d_cleaned_tax %>% 
  
  left_join(., tax_table, by = c("query_names" = "verbatim_name")) %>% 
  
  mutate(
    
    eventID = COLLECTION.db, #Sam prefix + identifier
    
    #occurrence terms
    occurrenceID = paste0("https://www.discoverlife.org/mp/20l?id=", ID.),
    catalogNumber = ID.,
    occurrenceStatus = "Present",
    basisOfRecord = 'PreservedSpecimen', #could be PreservedSpecimen or MaterialEntity, HumanObservation (identified in field)
    collectedBy = str_replace_all(who, pattern = ',', replacement = '|'), #could also add orcid as collectedByID
    verbatimIdentification = name, #makes sense, not tied to any database, we'll align to GBIF Backbone (until xCOL is ready)
    taxonRank = rank,
    recordedBy = str_replace(who, pattern = ", ", replacement = "|"),
    identifiedBy = DeterminedBy,
    dateIdentified = case_when(DeterminedWhen > '1900-01-01' ~ DeterminedWhen), #remove one date in 1615,
    sex, #see here: https://registry.gbif.org/vocabulary/Sex/concepts, blanks can be filled in, u is only if it's not a bee. may filter out u.
    associatedTaxa = str_replace(note, pattern = ",", replacement = "|"), #every flowering plant species present at site during sampling, maybe add as additional occurrences
    individualCount = 1, #or organismQuantity
    #disposition = NA, #destroyed, missing, on loan, etc.
    
    #collection info
    datasetID,
    collectionCode = 'BIML',
    collectionID = 'https://scientific-collections.gbif.org/collection/2338a0da-9fd4-42e5-9a61-2607a0b339aa',
    
    #time
    verbatimEventDate,
    eventDate,
    year,
    month,
    day,
    
    #Location Terms
    decimalLatitude = decimalLatitude,
    decimalLongitude = decimalLongitude,
    geodeticDatum = "WGS84", #double check with Maria
    coordinatePrecision = case_when(accuracy == 1 ~ 0.1,
                                    accuracy == 2 ~ 0.01,
                                    accuracy == 3 ~ 0.001,
                                    accuracy == 4 ~ 0.0001, #This is actually number of decimal points, so recoding according to the DwC examples.
                                    accuracy == 5 ~ 0.00001),
    scientificName,
    taxonRank = rank,
    kingdom,
    phylum,
    class,
    order,
    genus,
    species,
    .keep = 'none') %>%
  select(any_of(DwC_terms)) %>% 
  filter(verbatimIdentification != 'Destroyed')

occ %>% filter(is.na(scientificName)) %>% glimpse()

emof <- d_cleaned_tax %>% 
  mutate(
    datasetID,
    occurrenceID = paste0("https://www.discoverlife.org/mp/20l?id=", ID.),
    eventID = COLLECTION.db,
    occurrenceStatus = "Present",,
    'TrapVolume' = how2, #size of bowl
    'TrapLiquid' = how4, # they all use soap dawn
    .keep = 'none') %>% 
  distinct() %>% 
  pivot_longer(cols = `TrapVolume`: TrapLiquid, 
               names_to = 'measurementType', 
               values_to = 'measurementValue')

#sanity check for DwC names
d_cleaned_tax %>% 
  select(!any_of(DwC_terms)) %>% 
  glimpse()

# write out to csv --------------------------------------------------------

#for now, ignore datasetID

  event %>%
    #filter(datasetID == x) %>% 
    write_csv(file = here::here('output',
                                paste0('BIML_DwC_event', Sys.Date(), '.gz')
    ), na = ""
    )
  
  occ %>%
    #filter(datasetID == x) %>% 
    write_csv(file = here::here('output',
                                paste0('BIML_DwC_occ', Sys.Date(), '.gz')
    ), na = ""
    )
  
  emof %>%
    #filter(datasetID == x) %>% 
    select(-datasetID) %>% 
    write_csv(file = here::here('output',
                                paste0('BIML_DwC_EMoF', Sys.Date(), '.gz')
    ), na = ""
    )