library(tidyverse)
library(httr2)
library(lubridate)

# read in data ------------------------------------------------------------

d <- readr::read_csv(file = "data/species_projects_joined_new_13Feb2025.csv", quote = '"') %>%
  filter(datasetID != 'USFWS GLRI') %>%
  filter(COLLECTION.db != 'USA') %>% #filter out records that need serious QAQC
  mutate(
    verbatimLatitude = latitude,
    verbatimLongitude = longitude,
    
    #make invalid coordinates NA in DwC terms
    decimalLatitude = case_when(abs(as.numeric(latitude)) > 90 ~ NA_real_, TRUE ~ as.numeric(latitude)),
    decimalLongitude = case_when(abs(as.numeric(longitude)) > 180 ~ NA_real_, TRUE ~ as.numeric(longitude)),
    tz = 'UTC',
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

d %>% filter(abs(decimalLatitude) > 90)

d %>% filter(ID. == 'USGS_DRO664541') %>% glimpse()

#get time zone when there is spatial information
with_coords <- d %>% 
  filter(!is.na(decimalLongitude) & !is.na(decimalLatitude))

d$tz[!is.na(d$decimalLongitude) & !is.na(d$decimalLatitude)] <- lutz::tz_lookup_coords(lat = with_coords$decimalLatitude, lon = with_coords$decimalLongitude, method = "accurate")

#d[is.na(parse_date_time(d$time1, c("Y","Ym", "Ymd", "YmdH", "YmdHM", "YmdHMs"))),] %>% pull(time1) %>% unique()

#clean up times, some of which are NA
d <- d %>%
  
  #filter(!is.na(time1)&!is.na(time2)) %>% 
  mutate(
    
    #convert time to ISO 8601 UTC
    #strip x from times and parse into date-times
    time1 = str_remove_all(time1, pattern = 'x') %>% parse_date_time(., c("Y","Ym", "Ymd", "YmdH", "YmdHM", "YmdHMs")),
    time1 = force_tz(time1, tzone = tz),
    time2 = str_remove_all(time2, pattern = 'x') %>% parse_date_time(., c("Y","Ym", "Ymd", "YmdH", "YmdHM", "YmdHMs")),
    time2 = force_tz(time2, tzone = tz),
    
    #wrangle into ISO8601, UTC
    time1UTC = with_tz(time = time1, tzone = 'UTC'),
    time2UTC = with_tz(time = time2, tzone = 'UTC'),
    
    #generalize bad data where time1 > time2
    time1UTC_corrected = case_when(time1UTC > time2UTC ~ strftime(time1UTC, format="%Y-%m-%d"),
                         TRUE ~ time1UTC %>% as.character()
                         ),
    
    time1final = case_when(time1UTC_corrected != 'NA' & str_detect(time1UTC_corrected, 
                                                                   pattern = '\\s', 
                                                                   negate = TRUE) ~ time1UTC_corrected,
                           str_detect(time1UTC_corrected, pattern = '\\s') ~ paste0(time1UTC_corrected, 'Z')),
    time2final = case_when(as.character(time2UTC) != 'NA' & str_detect(time2UTC, 
                                                                       pattern = '\\s', 
                                                                       negate = TRUE) ~ time2UTC%>% as.character(),
                           str_detect(time2UTC, pattern = '\\s') ~ paste0(time2UTC, 'Z')
                           ),

    eventDate = case_when(!is.na(time1final) & !is.na(time2final) ~ paste0(time1final, '/', time2final) %>%
                            str_replace_all(., pattern = '\\s', replacement = 'T')
                          ),
    year = year(time1UTC),
    month = month(time1UTC),
    day = day(time1UTC),
    
    #catch bad timestamps
    badTS = case_when(day > day(time2UTC) & month >= month(time2UTC) & year >= year(time2UTC) ~ ID.,
                      month > month(time2UTC) & year >= year(time2UTC) ~ ID.,
                      year > year(time2UTC) ~ ID.),
    
    #reduce those to time1 dates only
    eventDate = case_when(!is.na(badTS) ~ time1final,
                          TRUE ~ eventDate)
    
  ) %>% glimpse()

d %>% filter(ID. == 'USGS_DRO691777') %>% glimpse()

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
                            "Destoryed",
                            "Destroy",
                            "destroyed",
                            "Destroyed",
                            "no ID",
                            "Unknown")

d <- d %>% 
  mutate(trim2genus = str_remove(string = name, pattern = '\\s.*$'),
         query_names = case_when(str_detect(string = name, pattern = patterns_to_trim_to_genus) ~ trim2genus, #any flags for unusual names
                                 TRUE ~ name))

# No_Bees_Found == bees were searched for, may have caught insects, but no bees were found. So the event should be valid, but 'no bees found' 
# What to do with these?

specs_to_check <- c("cf_Villa",
                    "Specimen_Number_Logged_by_Logan_Name_missing",
                    "Pompilinae_gen.",
                    "Parancistrocerus/Stenodynerus",
                    "Ichneumonidae/Ichneunominae",
                    "Ichneumonidae/Phygadeuontinae",
                    "Ichneumonidae/Tryphoninae",
                    "Figitidae/Eucoilinae",
                    "Andrena_Trachandrena")                        

#create list of Ids to double check
d %>% filter(name %in% specs_to_check) %>% select(ID., COLLECTION.db) %>% distinct() %>% 
  write_csv(file = paste0('specs_to_check_', Sys.Date(), '.csv'))

#filter out above list until checked
query_names <- d %>% filter(!str_detect(string = query_names, pattern = paste(specs_to_check, patterns_to_filter_out, collapse = '|'))) %>% pull(query_names) %>% unique()

# Query GBIF backbone - assuming everything is at least Insecta.

tax_table <- rgbif::name_backbone_checklist(name_data = query_names, 
                                            kingdom = 'Animalia', 
                                            class = 'Insecta')


#not hitting in GBIF
no_GBIF_hit <- tax_table %>% filter(is.na(kingdom)) %>% pull(verbatim_name)

no_GBIF_hit %>% sort()

# repair query_names based on above list
query_names[query_names=='Bee'] <- 'Hymenoptera'
query_names[query_names=='Nonbee'] <- 'Insecta'

#check any unaccepted names. Only two synonyms, will be interpreted by GBIF.
tax_table %>% filter(status != 'ACCEPTED') %>% glimpse()

#check alignment
A <- tax_table %>% select(verbatim_name, scientificName)

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

event <- d %>% 
  
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


occ <- d %>% 
  
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

emof <- d %>% 
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
d %>% 
  select(!any_of(DwC_terms)) %>% 
  glimpse()

# write out to csv --------------------------------------------------------

#for now, ignore datasetID

  event %>%
    #filter(datasetID == x) %>% 
    write_csv(file = here::here('output',
                                paste0('BIML_DwC_event', '.gz')
    ), na = ""
    )
  
  occ %>%
    #filter(datasetID == x) %>% 
    write_csv(file = here::here('output',
                                paste0('BIML_DwC_occ', '.gz')
    ), na = ""
    )
  
  emof %>%
    #filter(datasetID == x) %>% 
    select(-datasetID) %>% 
    write_csv(file = here::here('output',
                                paste0('BIML_DwC_EMoF', '.gz')
    ), na = ""
    )

#check if there are any GLRI occurrenceIDs in BIML
GLRI <- readr::read_csv(file = 'output/USFWS GLRIDwC_occ_2025-02-18.gz')

GLRI %>% 
  filter(occurrenceID %in% occ$occurrenceID)
