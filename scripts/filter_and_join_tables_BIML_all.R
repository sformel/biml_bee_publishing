library(tidyverse)

#import BIML Flat Exporta
biml_flat <- read_delim(file = 'data/USGS_DRO_flat.txt.gz', delim = '$') %>%
  rename(datasetID = email) #rename email field to datasetID

#import Project_Identifiers_Table.csv
projID_table <- read_csv(file = 'data/Project_Identifiers_Table.csv') %>%
  rename(collectionCode = collectionID) %>%  #rename CollectionID field to Darwin Core term, collectionCode
  bind_rows(
    tibble(
      ID = 4,
      datasetID = 'BIML',
      collectionCode = 'BIML',
      datasetName = 'Insect Species Occurrence Data from Multiple Projects Worldwide with Focus on Bees and Wasps in North America',
      institutionCode = 'USGS',
      institutionID = 'https://ror.org/035a68863',
      ownerInstitutionCode = 'USGS',
      publisher = 'USGS'
    ) #add row for BIML
  )

str(projID_table)

#remove all rows of data that do not have species-level IDs
biml_flat %>%
  count(!is.na(name))

biml_flat <- biml_flat %>%
    filter(!is.na(name))

# Update the email field, replace all email values that do not match datasetID with "BIML"
filteredData <- biml_flat %>%
  mutate(datasetID = ifelse(!(datasetID %in% projID_table$datasetID), "BIML", datasetID))

glimpse(filteredData)

#Join of filteredData and project identifiers table based on matching datasetID fields
species_projects_joined_new <- filteredData %>%
  left_join(x = ., y = projID_table, by = 'datasetID')
