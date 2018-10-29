### This script creates lists that will help prioritize which ECSO ontology terms should be checked for completeness and should be
### added to the ontology. It uses counts to indicate which terms / concepts occur most frequently in the annotation spreadsheet.
### CSV files containing the term IDs, labels and counts are output for existing terms in the ontology and suggested terms and
### counts are output for proposed terms.

library(dplyr)

#Read annotations spreadsheet
annotation_spreadsheet <- list.files(full.names = TRUE, pattern = "annotation_spreadsheet")
annotation_df <- read.csv(annotation_spreadsheet, stringsAsFactors = FALSE)

#Read spreadsheet containing ECSO IDs and labels
ECSO_spreadsheet <- list.files(full.names = TRUE, pattern = "ECSO_terms.csv")
ECSO_df <- read.csv(ECSO_spreadsheet, stringsAsFactors = FALSE)


### Counts of carbon measurement attributes

#Count ECSO term occurrences in annotation spreadsheet (count of ECSO IDs)
ECSO_occurrences <- count(annotation_df, EC_Carbon_Measurements_Present )


# Add labels to existing ECSO terms in the annotation spreadsheet
ECSO_occurrences$label <- "NA"

counter <- 1 #to iterate through the annotation spreadsheet rows

#Iterate through rows in annotation spreadsheet
for (id_to_check in trimws(ECSO_occurrences$EC_Carbon_Measurements_Present) ){ 
	
	ECSO_id_counter <- 1 #to iterate through the ECSO ontology fields spreadsheet
	
	#Check if term from annotation spreadsheet was blank
	if (id_to_check != ""){
		
		#Iterate through the spreadsheet containing the ontology fields
		for (ECSO_id in trimws(ECSO_df$Class.ID )){

			if (grepl(id_to_check, ECSO_id )){
				ECSO_occurrences$label[counter] <- ECSO_df$Preferred.Label[ECSO_id_counter]
				
			}
			
			ECSO_id_counter <- ECSO_id_counter + 1
		}
	} else {
		ECSO_occurrences$label[counter] <- ""
	}	
	
	counter <- counter + 1 

}

# Sort count of ECSO occurrences and rearrange the column order
existing_ECSO_terms <- ECSO_occurrences %>% 
	arrange(desc(n)) %>%
	rename( ECSO_ID = EC_Carbon_Measurements_Present) %>%
	select( ECSO_ID, label, n)


#Count terms to add to ontology
terms_to_add_df <- count(annotation_df, Term_to_Add_to_ECSO ) %>%
	arrange(desc(n))


### Create CSVs
write.csv(existing_ECSO_terms, file = "existing_ECSO_terms_in_annotations_to_check.csv", row.names = FALSE)
write.csv(terms_to_add_df, file = "terms_to_add_to_ECSO.csv", row.names = FALSE)


### Non-carbon measurements
non_carbon_to_add_df <- annotation_df %>%
	select (attributeName, attributeDefinition, Carbon_Measurements_Present_original_column) %>%
#	select (attributeName, attributeDefinition, attributeUnit, Carbon_Measurements_Present_original_column) %>%
	filter (Carbon_Measurements_Present_original_column != 'yes', Carbon_Measurements_Present_original_column != 'Yes' ) %>%
	filter (Carbon_Measurements_Present_original_column != '', Carbon_Measurements_Present_original_column != 'yes (all missing values)') %>%
	select (attributeName, attributeDefinition)

# Count the number of non-carbon attributes
non_carbon_occurrences_df <- non_carbon_to_add_df %>%
	group_by_all() %>%
	count %>% #count of unique rows
	arrange(desc(n)) #arrange counts by descending order


write.csv(non_carbon_occurrences_df, file = "non-carbon_terms_to_add_to_ECSO.csv", row.names = FALSE)

