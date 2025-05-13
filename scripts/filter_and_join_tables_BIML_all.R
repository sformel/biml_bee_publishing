library(dplyr)
library(readr)

#import USGS_DRO_Flat.txt file
#import Project_Identifiers_Table.csv

#remove all rows of data that do not have species-level IDs
filteredData <- USGS_DRO_flat %>%
    filter(!is.na(name))


# head(USGS_DRO_flat)


# Update the email field in filteredData - replace all email values that do not match datasetID with "BIML"
filteredData <- filteredData %>%
  mutate(email = ifelse(!(email %in% Project_Identifiers_Table$datasetID), "BIML", email))

#rename CollectionID field to collectionCode
Project_Identifiers_Table <- Project_Identifiers_Table %>%
    rename(collectionCode = collectionID)

# unique(filteredData$email)

#transform filteredData to dataframe for join
data.frame(filteredData)

#Inner join of filteredData and project identifiers table based on matching email and datasetID fields
species_projects_joined_new <- filteredData %>%
    inner_join(Project_Identifiers_Table, by = c("email"="datasetID"))

#rename email field to datasetID
species_projects_joined_new<- species_projects_joined_new %>%
  rename(datasetID=email)

#export table as .csv file
write.csv(species_projects_joined_new, file='<filepath>/species_projects_joined_new.csv') #replace <filepath> with desired output location


#after.csv is exported, upload this file into GitHub project Repo folder titled "data"