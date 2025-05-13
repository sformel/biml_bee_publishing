library(tidyverse)
library(httr2)

# read in data ------------------------------------------------------------

d <- readr::read_csv(file = "data/species_projects_joined_new.csv", quote = '"') %>% 
  #QAQC
  mutate(sex = case_when(sex == 'm' ~ 'Male',
                         sex == 'f' ~ 'Female',
                         sex == 'q' ~ 'Female',
                         sex == 'd' ~ 'Female',
                         TRUE ~ 'Indeterminate')
         ) #clean up sex according to https://doi.org/10.5281/zenodo.14187862

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
                            "Destoryed",
                            "Destroy",
                            "destroyed",
                            "Destroyed",
                            "no ID",
                            "Unknown")

query_names <- d %>% 
  mutate(trim2genus = str_remove(string = name, pattern = '\\s.*$'),
         name = case_when(str_detect(string = name, pattern = patterns_to_trim_to_genus) ~ trim2genus, #any flags for unusual names
                          TRUE ~ name)) %>% 
  pull(name) %>% unique()

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
  write_csv(file = paste0('output/specs_to_check_', Sys.Date(), '.csv'))

#filter out above list until checked
query_names <- query_names[!query_names %in% c(specs_to_check, patterns_to_filter_out)]

# Query GBIF backbone
tax_table <- rgbif::name_backbone_checklist(name_data = query_names)


#not hitting in GBIF
no_GBIF_hit <- tax_table %>% filter(is.na(kingdom)) %>% pull(verbatim_name)

no_GBIF_hit %>% sort()


# create lookup table to collectionID

collectionID_table <- lapply(unique(d$collectionID), function(x) {

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
  
  #collection info
  collectionCode = 'BIML',
  collectionID = 'https://scientific-collections.gbif.org/collection/2338a0da-9fd4-42e5-9a61-2607a0b339aa',
  institutionCode,
  institutionID,

  #event terms
  datasetName,
  datasetID = datasetID,
  eventID = COLLECTION.db, #Sam prefix + identifier
  eventRemarks = field_note, # best solution for now, until we have time to parse the variables
  samplingProtocol = how0, #pan trap or hand net
  sampleSizeValue = case_when(samplingProtocol == 'pan trap' ~ how1), #number of bowls successful (data exists)
  sampleSizeUnit = 'successful traps',
  samplingEffort = case_when(samplingProtocol == 'pan trap' ~ paste(days, 'Day(s)')), #What does this mean? Only values of 1 (left out for one day), blank (netting)
  
  #strip x from times
  time1 = str_remove_all(time1, pattern = 'x'),
  time2 = str_remove_all(time2, pattern = 'x'),
  
  #wrangle into ISO8601
  verbatimEventDate = paste0('time1:', time1,';', 'time2:', time2),
  eventDate = paste(lubridate::parse_date_time(time1, c("Ymd", "YmdHM", "YmdHMs")), '/', lubridate::parse_date_time(time2, c("Ymd", "YmdHM", "YmdHMs"))), # collectionStart/Stop YMDtime, XX = seconds
  year = str_extract(time1, pattern = "^[0-9]{4}"),
  
  #location terms
  locationID = site,
  locality = city,
  county,
  stateProvince = state,
  country = case_when(country == 'USA' ~ 'US') #converted to two-letter code
  ) %>%

  select(any_of(DwC_terms)) %>% 
  glimpse()

occ <- d %>% 
  
  left_join(., tax_table, by = c("name" = "verbatim_name")) %>% 
  
  mutate(
    
    #occurrence terms
    occurrenceID = paste0("https://www.discoverlife.org/mp/20l?id=", ID.),
    occurrenceStatus = "Present",
    basisOfRecord = 'MaterialEntity', #could be PreservedSpecimen or MaterialEntity, HumanObservation (identified in field)
    collectedBy = str_replace_all(who, pattern = ',', replacement = '|'), #could also add orcid as collectedByID
    verbatimIdentification = name, #makes sense, not tied to any database, we'll align to GBIF Backbone (until xCOL is ready)
    taxonRank = rank,
    identifiedBy = DeterminedBy,
    dateIdentified = DeterminedWhen,
    sex, #see here: https://registry.gbif.org/vocabulary/Sex/concepts, blanks can be filled in, u is only if it's not a bee. may filter out u.
    associatedTaxa = note, #every flowering plant species present at site during sampling, maybe add as additional occurrences
    individualCount = 1, #or organismQuantity
    #disposition = NA, #destroyed, missing, on loan, etc.
    
    #Location Terms
    decimalLatitude = latitude,
    decimalLongitude = longitude,
    geodeticDatum = "WGS84", #double check with Maria
    coordinatePrecision = case_when(accuracy == 1 ~ 0.1,
                                    accuracy == 2 ~ 0.01,
                                    accuracy == 3 ~ 0.001,
                                    accuracy == 4 ~ 0.0001, #This is actually number of decimal points, so recoding according to the DwC examples.
                                    accuracy == 5 ~ 0.00001),
  ) %>%
  select(any_of(DwC_terms)) %>% 
  
  glimpse()
  

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

# write out to csv --------------------------------------------------------

lapply(d$datasetID %>% unique(), function(x){

  event %>%
    filter(datasetID == x) %>% 
    write_csv(file = here::here('output',
                                paste(x, 'DwC_event', Sys.Date(), '.gzip', sep = "_")
    )
    )
  
  occ %>%
    filter(datasetID == x) %>% 
    write_csv(file = here::here('output',
                                paste(x, 'DwC_occ', Sys.Date(), '.gzip', sep = "_")
    )
    )
  
  emof %>%
    filter(datasetID == x) %>% 
    select(-datasetID) %>% 
    write_csv(file = here::here('output',
                                paste(x, 'DwC_EMoF', Sys.Date(), '.gzip', sep = "_")
                                )
              )
  }
  )


# List of occurrenceIDs for GBIF to migrate -------------------------------

GLRI <- read_csv('output/USFWS GLRI_DwC_occ_2025-01-29_.gzip')

GLRI %>% select(occurrenceID) %>% write_csv('output/occurrenceID_to_migrate.csv')
