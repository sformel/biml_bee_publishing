
# Run filter_and_join_tables_BIML_all.R -----------------------------------

# 1. Remove all records that do not have confirmed species-level identifications present,
# 2. Reassign the value in the email field to “BIML” if the value does not match any of the other datasetID values in the PIT table,
# 3. Join the details of the PIT table to the records (1:N) based on matching values in the email field of the flat file and the datasetID field in the PIT table, and
# 4. Export the new .csv file, titled “species_project_joined_new.csv” for use in the second R script.

source('scripts/filter_and_join_tables_BIML_all.R')

# crosswalk data and write out for publication ---------------------------

source('scripts/crosswalk_BIML.R')