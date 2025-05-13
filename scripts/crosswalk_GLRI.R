library(tidyverse)
library(httr2)
library(lubridate)

# read in data ------------------------------------------------------------

d <- readr::read_csv(file = "data/species_projects_joined_new_13Feb2025.csv", quote = '"') %>%
  filter(datasetID == 'USFWS GLRI') %>%
  #QAQC
  mutate(
    
    #clean up sex according to https://doi.org/10.5281/zenodo.14187862
    sex = case_when(
      sex == 'm' ~ 'Male',
      sex == 'f' ~ 'Female',
      sex == 'q' ~ 'Female',
      sex == 'd' ~ 'Female',
      TRUE ~ 'Indeterminate'
    ),
    #convert time to ISO 8601 UTC
    decimalLatitude = as.numeric(latitude),
    decimalLongitude = as.numeric(longitude),
    tz = lutz::tz_lookup_coords(lat = decimalLatitude, lon = decimalLongitude, method = "accurate"),
    
    verbatimEventDate = paste0('time1:', time1, ';', 'time2:', time2),
    
    #strip x from times and parse into date-times
    time1 = str_remove_all(time1, pattern = 'x') %>% parse_date_time(., c("Ymd", "YmdHM", "YmdHMs")),
    time1 = case_when(tz == "America/Detroit" ~ force_tz(time1, tzone = "America/Detroit"),
                tz == "America/New_York" ~ force_tz(time1, tzone = "America/New_York"),
                tz == "America/Chicago" ~ force_tz(time1, tzone = "America/Chicago")),
    time2 = str_remove_all(time2, pattern = 'x') %>% 
      parse_date_time(., c("Ymd", "YmdHM", "YmdHMs")),
    time2 = case_when(tz == "America/Detroit" ~ force_tz(time2, tzone = "America/Detroit"),
                tz == "America/New_York" ~ force_tz(time2, tzone = "America/New_York"),
                tz == "America/Chicago" ~ force_tz(time2, tzone = "America/Chicago")),
    
    #wrangle into ISO8601, UTC
    time1UTC = with_tz(time = time1, tzone = 'UTC'),
    time2UTC = with_tz(time = time2, tzone = 'UTC'),
    
    #generalize bad data where time1 > time2
    time1UTC = case_when(time1UTC > time2UTC ~ strftime(time1UTC, format="%Y-%m-%d"),
                         TRUE ~ time1UTC %>% as.character() %>% paste0(., 'Z')),
    
    eventDate = paste0(time1UTC, '/', time2UTC, 'Z') %>%
      str_replace_all(., pattern = '\\s', replacement = 'T'),
      year = year(time1UTC),
      month = month(time1UTC),
      day = day(time1UTC)
      
    ) 


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
query_names <- d$query_names[!d$query_names %in% c(specs_to_check, patterns_to_filter_out)] %>% unique()

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
    collectionID_table[[1]] %>% as_data_frame()
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
    eventRemarks = field_note, # best solution for now, until we have time to parse the variables
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
    countryCode = case_when(country == 'USA' ~ 'US'), #converted to two-letter code
    .keep = 'none') %>%
  
  select(any_of(DwC_terms)) %>% 
  distinct()

occ <- d %>% 
  
  left_join(., tax_table, by = c("query_names" = "verbatim_name")) %>% 
  
  mutate(
    
    eventID = COLLECTION.db, #Sam prefix + identifier
    
    #occurrence terms
    occurrenceID = paste0("https://www.discoverlife.org/mp/20l?id=", ID.),
    occurrenceStatus = "Present",
    basisOfRecord = 'PreservedSpecimen', #could be PreservedSpecimen or MaterialEntity, HumanObservation (identified in field)
    collectedBy = str_replace_all(who, pattern = ',', replacement = '|'), #could also add orcid as collectedByID
    verbatimIdentification = name, #makes sense, not tied to any database, we'll align to GBIF Backbone (until xCOL is ready)
    taxonRank = rank,
    recordedBy = str_replace(who, pattern = ", ", replacement = "|"),
    identifiedBy = DeterminedBy,
    dateIdentified = DeterminedWhen,
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
    decimalLatitude = latitude,
    decimalLongitude = longitude,
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

lapply(d$datasetID %>% unique(), function(x){
  
  event %>%
    filter(datasetID == x) %>% 
    write_csv(file = here::here('output',
                                paste0(x, 'DwC_event_', Sys.Date(), '.gz')
    ), na = ""
    )
  
  occ %>%
    filter(datasetID == x) %>% 
    write_csv(file = here::here('output',
                                paste0(x, 'DwC_occ_', Sys.Date(), '.gz')
    ), na = ""
    )
  
  emof %>%
    filter(datasetID == x) %>% 
    select(-datasetID) %>% 
    write_csv(file = here::here('output',
                                paste0(x, 'DwC_EMoF_', Sys.Date(), '.gz')
    ), na = ""
    )
}
)

occ %>% 
  select(occurrenceID) %>% 
  readr::write_csv(file = 'output/occurrenceIDs_to_migrate.csv')
