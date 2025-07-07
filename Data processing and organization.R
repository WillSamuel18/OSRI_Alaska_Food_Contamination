################################################################################

#OSRI Project - Subsistence Foods Baseline Contaminants Report
#Started on 4/21/2025
#Primary Author: William Samuel
#Project PI: Dr. Morgan Powers

#A meta-analysis project to understand the the impacts of contaminants on 
#Alaskan marine foods. These data include contaminant information from 
#fish, invertebrates, and marine mammals. 

#This script is for data organizing and manipulation. See "Analysis and Figures"
#for the data analysis and visualization. 

################################################################################

library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)
library(dplyr)
library(tibble)
library(writexl)

# Function to compare column names between NCCOS and another dataset
compare_column_names <- function(other_df) {
  nccos_cols <- names(NCCOS_data_processed)
  other_cols <- names(other_df)
  other_name <- deparse(substitute(other_df))
  
  only_in_nccos <- setdiff(nccos_cols, other_cols)
  only_in_other <- setdiff(other_cols, nccos_cols)
  
  list(
    only_in_NCCOS = only_in_nccos,
    only_in_other_dataset = only_in_other,
    summary = paste0(
      "Non-matched columns only in NCCOS: ", length(only_in_nccos), 
      "; Non-matched columns only in ", other_name, ": ", length(only_in_other)
    )
  )
}




# #Target dataframe structure: --------------------------------------------

#Data_source        - This is the source of the data
#Study_name         - This is the study that collected the data
#Source_siteID      - This is the Site ID as given by the original study/dataset
#Source_sampleID    - This is the Sample ID as given by the original study/dataset
  #OSRI_siteID        - This is our combined Site ID, to make them consistent
  #OSRI_sampleID      - This is our combined Sample ID, to make them consistent ***
#Sample_motivation  - Whether the sample was tested for basic biomonitoring,or in response to an oil spill
#General_location   - The general region where the samples were collected, which are mostly available in the NCCOS and Diver datasets
#Specific_location  - A description-based naming convention of the specific location, available in the NCCOS and Diver Datasets (e.g., STADESC)
#Lat                - Latitude of the sample location, in decimal degrees. May need to further refine this depending on the datum/projected coordinate systems. 
#Long               - Longitude of the sample location, in decimal degrees. May need to further refine this depending on the datum/projected coordinate systems. 
#Year               - Year the sample was collected
#Month              - Month the sample was collected
#Collection_date    - Date the sample was collected
#DOY                - Day of Year the sample was collected
#Collection_time    - The time the sample was collected, if possible
#Collection_method  - How the sample was obtained (e.g., harvest, found dead, etc.)
  #Species_complex    - The functional group that the species belongs to (e.g., fish, bottom fish, whale, mollusk). This is just for analysis purposes, and my not align with taxonomic classifications. It may need to be refined or dropped in the future.
#Common_name        - Common Name of the species
#Scientific_name    - Scientific name of the species
#Genus_latin        - Genus (from scientific name)
#Species_latin      - Species (from scientific name)
#Tissue_type        - The type of tissue which the sample was from
#Sample_composition - E.g., a single individual vs a group of pooled individuals (factor)
#Number_in_composite- This is the number of animals included in the composite

#Sex                - The sex of the animal that was sampled. May not be any data.
#Analysis_method    - The method that was used to test the sample (e.g., method code)
#Chem_code          - Potentially universal identifiers of these tests? 

#Parameter          - The chemical that was tested for, we will need to work to make sure these naming conventions are consistent across studies. In the Diver data it's "Analysis_Type"
#Value              - Measured value of the parameter that was sampled
#Units              - Units of measurements, may need to transform some of these so they are consistent
#Value_standardized - The standardized value, according to the standardized units
#Units_standardized - In case we need to transform the units
#Detection_limit    - The value for which the test can detect the target parameter
#Reporting_limit    - ...
#Basis              - Weight weight vs dry weight
#Lab_replicate      - Lab replicates of individual samples
#Qualifier_code     - A lab flag for potential errors, or describing how the value was calculated (if estimated)
#Lipid_pct          - Percent of lipids within the sample, used to standardize and compare samples properly
#Moisture_pct       - Percent of moisture
#Total_PAHs         - Total amount of Polycyclic Aromatic Hydrocarbons (PAHs) 
#Total_LMWAHs       - The total low molecular-weight aromatic hydrocarbons
#Total_HMWAHs       - The total high molecular-weight aromatic hydrocarbons
#Lab_ID             - The analytical lab where the samples were tested



###Might need to add unit basis, e.g., wet weight vs dry weight


df <- data.frame(Data_source = character(),
                  Study_name = character(),
                  Source_siteID = character(),
                  Source_sampleID = character(), 
                  OSRI_siteID = character(),
                  OSRI_sampleID = character(),   
                  Sample_motivation = character(),    
                  General_location = character(),
                  Specific_location = character(),
                  Lat = numeric(),
                  Long = numeric(),
                  Year = integer(),
                  Month = character(),
                  Collection_date = as.Date(character()),	
                  DOY = character(),
                  Collection_time = as.POSIXct(character()),
                  Collection_method = character(),
                  Species_complex =character(),
                  Common_name = character(),
                  Scientific_name = character(),
                  Genus_latin = character(),
                  Species_latin = character(),
                  Tissue_type = character(),
                  Sample_composition = character(),
                  Number_in_composite = numeric(),
                  Sex = character(),
                  Analysis_method = character(),
                  Chem_code = character(),
                  Parameter = character(),
                  Value = numeric(),
                  Units = character(),
                  Value_standardized = numeric(),
                  Units_standardized = character(),
                  Detection_limit = numeric(),
                  Reporting_limit = numeric(),
                  Basis = character(),
                  Lab_replicate = numeric(),
                  Qualifier_code = character(),
                  Lipid_pct = numeric(),
                  Moisture_pct = numeric(),
                  Total_PAHs = numeric(),
                  Total_LMWAHs = numeric(),
                  Total_HMWAHs = numeric(),
                  Lab_ID = character(),
                  Notes = character(),
                 stringsAsFactors = FALSE)



# NCCOS Data # --------------------------------------------------------------

NCCOS_clam <- read.delim("Input Data/NCCOS PAH Data/nccos_chem_data_clam.txt")      
NCCOS_cockles <- read.delim("Input Data/NCCOS PAH Data/nccos_chem_data_cockles.txt")      
NCCOS_fish <- read.delim("Input Data/NCCOS PAH Data/nccos_chem_data_fish.txt")      
NCCOS_flatfish <- read.delim("Input Data/NCCOS PAH Data/nccos_chem_data_flatfish.txt")      
NCCOS_mussel <- read.delim("Input Data/NCCOS PAH Data/nccos_chem_data_mussel.txt")      
NCCOS_shrimp <- read.delim("Input Data/NCCOS PAH Data/nccos_chem_data_shrimp.txt")      
NCCOS_starfish <- read.delim("Input Data/NCCOS PAH Data/nccos_chem_data_starfish.txt")      

View(NCCOS_clam)
str(NCCOS_clam)
#'data.frame':	707 obs. of  17 variables:
#$ Study            : chr  "North Pacific Research Board Alaska Study" "North Pacific Research Board Alaska Study" "North Pacific Research Board Alaska Study" "North Pacific Research Board Alaska Study" ...
#$ NST_Site         : chr  "NPR_NHA" "NPR_NHA" "NPR_NHA" "NPR_NHA" ...
#$ General_Location : chr  "Nanwalek" "Nanwalek" "Nanwalek" "Nanwalek" ...
#$ Specific_Location: chr  "Nanwalek" "Nanwalek" "Nanwalek" "Nanwalek" ...
#$ Sample_ID        : chr  "BA2010NPR_NHA" "BA2010NPR_NHA" "BA2010NPR_NHA" "BA2010NPR_NHA" ...
#$ Latitude         : num  59.3 59.3 59.3 59.3 59.3 59.3 59.3 59.3 59.3 59.3 ...
#$ Longitude        : num  -152 -152 -152 -152 -152 ...
#$ Fiscal_Year      : int  2010 2010 2010 2010 2010 2010 2010 2010 2010 2010 ...
#$ Collection_Date  : chr  "5/28/2010 12:00:00 AM" "5/28/2010 12:00:00 AM" "5/28/2010 12:00:00 AM" "5/28/2010 12:00:00 AM" ...
#$ Matrix           : chr  "Clam" "Clam" "Clam" "Clam" ...
#$ Scientific_Name  : chr  "Mya Arenaria" "Mya Arenaria" "Mya Arenaria" "Mya Arenaria" ...
#$ Method           : chr  "PAH-2010" "PAH-2010" "PAH-2010" "PAH-2010" ...
#$ Parameter        : chr  "1,6,7-Trimethylnaphthalene" "1-Methylnaphthalene" "1-Methylphenanthrene" "18a-Oleanane" ...
#$ Value            : num  1.3 8.4 0.8 0 4.2 13.6 2.8 0.8 5.6 0.4 ...
#$ Qualifier        : chr  "" "" "Below the MDL" "Not detected" ...
#$ Unit             : chr  "ng/dry g" "ng/dry g" "ng/dry g" "ng/dry g" ...
#$ X                : logi  NA NA NA NA NA NA ...



#### NCCOS clam data ------------------------------------------------------
NCCOS_clam_processed <- data.frame(
  Data_source = rep("NCCOS", nrow(NCCOS_clam)),
  Study_name = NCCOS_clam$Study,
  Source_siteID = NCCOS_clam$NST_Site,
  Source_sampleID = NCCOS_clam$Sample_ID,
  OSRI_siteID = NA,
  OSRI_sampleID = NA,
  Sample_motivation = NA,
  General_location = NCCOS_clam$General_Location,
  Specific_location = NCCOS_clam$Specific_Location,
  Lat = NCCOS_clam$Latitude,
  Long = NCCOS_clam$Longitude,
  Year = NCCOS_clam$Fiscal_Year,
  Month = NA,
  Collection_date =	NCCOS_clam$Collection_Date,
  DOY = NA,
  Collection_time = NA,
  Collection_method = NA,
  Species_complex = NCCOS_clam$Matrix,
  Common_name = NA,
  Scientific_name = NCCOS_clam$Scientific_Name,
  Genus_latin = NA,
  Species_latin = NA,
  Tissue_type = NA,
  Sample_composition = NA,
  Number_in_composite = NA,
  Sex = NA,
  Analysis_method = NCCOS_clam$Method,
  Chem_code = NA,
  Parameter = NCCOS_clam$Parameter,
  Value = NCCOS_clam$Value,
  Units = NCCOS_clam$Unit,
  Value_standardized = NA,
  Units_standardized = NA,
  Detection_limit = NA,
  Reporting_limit = NA,
  Basis = NA,
  Lab_replicate = NA,
  Qualifier_code = NCCOS_clam$Qualifier,
  Lipid_pct = NA,
  Moisture_pct = NA,
  Total_PAHs = NA,
  Total_LMWAHs = NA,
  Total_HMWAHs = NA,
  Lab_ID = NA,
  Notes = NA
  )

  

NCCOS_clam_processed <- NCCOS_clam_processed %>% 
  mutate(Collection_date = as.Date(Collection_date, format = "%Y-%m-%d"),  
         Month = month(Collection_date),
         DOY = yday(Collection_date),
         
         Basis = "Dry",
           
         Scientific_name = case_when(
           Scientific_name == "Mya Arenaria" ~ "Mya arenaria",
           Scientific_name == "Siliqua Patula" ~ "Siliqua patula",
           TRUE ~ NA_character_  # Default case if no match is found
         ),
         
         Common_name = case_when(
           Scientific_name == "Mya arenaria" ~ "Soft-shell clam",
           Scientific_name == "Siliqua patula" ~ "Pacific razor clam",
           Scientific_name == "Protothaca staminea" ~ "Pacific littleneck clam",
           TRUE ~ NA_character_  # Default case if no match is found
           ),
         
         Genus_latin = case_when(
           Scientific_name == "Mya arenaria" ~ "Mya",
           Scientific_name == "Siliqua patula" ~ "Siliqua",
           Scientific_name == "Protothaca staminea" ~ "Protothaca",
           TRUE ~ NA_character_  # Default case if no match is found
         ),
         
         Species_latin = case_when(
           Scientific_name == "Mya arenaria" ~ "arenaria",
           Scientific_name == "Siliqua arenaria" ~ "arenaria",
           Scientific_name == "Protothaca staminea" ~ "staminea",
           TRUE ~ NA_character_  # Default case if no match is found
         ))

str(NCCOS_clam_processed)



#### NCCOS cockles data -----------------------------------------------------

str(NCCOS_cockles)
identical(names(NCCOS_clam), names(NCCOS_cockles)) #The columns are identical as clams


NCCOS_cockles_processed <- data.frame(
  Data_source = rep("NCCOS", nrow(NCCOS_cockles)),
  Study_name = NCCOS_cockles$Study,
  Source_siteID = NCCOS_cockles$NST_Site,
  Source_sampleID = NCCOS_cockles$Sample_ID,
  OSRI_siteID = NA,
  OSRI_sampleID = NA,
  Sample_motivation = NA,
  General_location = NCCOS_cockles$General_Location,
  Specific_location = NCCOS_cockles$Specific_Location,
  Lat = NCCOS_cockles$Latitude,
  Long = NCCOS_cockles$Longitude,
  Year = NCCOS_cockles$Fiscal_Year,
  Month = NA,
  Collection_date =	NCCOS_cockles$Collection_Date,
  DOY = NA,
  Collection_time = NA,
  Collection_method = NA,
  Species_complex = NCCOS_cockles$Matrix,
  Common_name = NA,
  Scientific_name = NCCOS_cockles$Scientific_Name,
  Genus_latin = NA,
  Species_latin = NA,
  Tissue_type = NA,
  Sample_composition = NA,
  Number_in_composite = NA,
  Sex = NA,
  Analysis_method = NCCOS_cockles$Method,
  Chem_code = NA,
  Parameter = NCCOS_cockles$Parameter,
  Value = NCCOS_cockles$Value,
  Units = NCCOS_cockles$Unit,
  Value_standardized = NA,
  Units_standardized = NA,
  Detection_limit = NA,
  Reporting_limit = NA,
  Basis = NA,
  Lab_replicate = NA,
  Qualifier_code = NCCOS_cockles$Qualifier,
  Lipid_pct = NA,
  Moisture_pct = NA,
  Total_PAHs = NA,
  Total_LMWAHs = NA,
  Total_HMWAHs = NA,
  Lab_ID = NA,
  Notes = NA
)



unique(NCCOS_cockles_processed$Scientific_name)

NCCOS_cockles_processed <- NCCOS_cockles_processed %>% 
  mutate(Collection_date = as.Date(Collection_date, format = "%Y-%m-%d"),  
         Month = month(Collection_date),
         DOY = yday(Collection_date),
         
         Basis = "Dry",
         
         Common_name = case_when(
           Scientific_name == "Clinocardium nuttallii" ~ "cockle",
           TRUE ~ NA_character_  # Default case if no match is found
         ),
         
         Genus_latin = case_when(
           Scientific_name == "Clinocardium nuttallii" ~ "Clinocardium",
           TRUE ~ NA_character_  # Default case if no match is found
         ),
         
         Species_latin = case_when(
           Scientific_name == "Clinocardium nuttallii" ~ "nuttallii",
           TRUE ~ NA_character_  # Default case if no match is found
         ))

str(NCCOS_cockles_processed)



#### NCCOS fish data -----------------------------------------------------

str(NCCOS_fish)
identical(names(NCCOS_clam), names(NCCOS_fish)) #The columns are identical as clams


NCCOS_fish_processed <- data.frame(
  Data_source = rep("NCCOS", nrow(NCCOS_fish)),
  Study_name = NCCOS_fish$Study,
  Source_siteID = NCCOS_fish$NST_Site,
  Source_sampleID = NCCOS_fish$Sample_ID,
  OSRI_siteID = NA,
  OSRI_sampleID = NA,
  Sample_motivation = NA,
  General_location = NCCOS_fish$General_Location,
  Specific_location = NCCOS_fish$Specific_Location,
  Lat = NCCOS_fish$Latitude,
  Long = NCCOS_fish$Longitude,
  Year = NCCOS_fish$Fiscal_Year,
  Month = NA,
  Collection_date =	NCCOS_fish$Collection_Date,
  DOY = NA,
  Collection_time = NA,
  Collection_method = NA,
  Species_complex = NCCOS_fish$Matrix,
  Common_name = NA,
  Scientific_name = NCCOS_fish$Scientific_Name,
  Genus_latin = NA,
  Species_latin = NA,
  Tissue_type = NA,
  Sample_composition = NA,
  Number_in_composite = NA,
  Sex = NA,
  Analysis_method = NCCOS_fish$Method,
  Chem_code = NA,
  Parameter = NCCOS_fish$Parameter,
  Value = NCCOS_fish$Value,
  Units = NCCOS_fish$Unit,
  Value_standardized = NA,
  Units_standardized = NA,
  Detection_limit = NA,
  Reporting_limit = NA,
  Basis = NA,
  Lab_replicate = NA,
  Qualifier_code = NCCOS_fish$Qualifier,
  Lipid_pct = NA,
  Moisture_pct = NA,
  Total_PAHs = NA,
  Total_LMWAHs = NA,
  Total_HMWAHs = NA,
  Lab_ID = NA,
  Notes = NA
)


unique(NCCOS_fish_processed$Scientific_name)


NCCOS_fish_processed <- NCCOS_fish_processed %>% 
  mutate(Collection_date = as.Date(Collection_date, format = "%Y-%m-%d"),  
         Month = month(Collection_date),
         DOY = yday(Collection_date),
         
         Basis = "Dry",
         
         Common_name = case_when(
           Scientific_name == "Pleuronectes glacialis" ~ "Arctic flounder",
           Scientific_name == "Boreogadus saida" ~ "Artic cod",
           Scientific_name == "Osmerus mordax" ~ "Rainbow smelt",
           Scientific_name == "Platichthys stellatus" ~ "Starry flounder",
           TRUE ~ NA_character_  # Default case if no match is found
         ),
         
         Genus_latin = case_when(
           Scientific_name == "Pleuronectes glacialis" ~ "Pleuronectes",
           Scientific_name == "Boreogadus saida" ~ "Boreogadus",
           Scientific_name == "Osmerus mordax" ~ "Osmerus",
           Scientific_name == "Platichthys stellatus" ~ "Platichthys",
           TRUE ~ NA_character_  # Default case if no match is found
         ),
         
         Species_latin = case_when(
           Scientific_name == "Pleuronectes glacialis" ~ "glacialis",
           Scientific_name == "Boreogadus saida" ~ "saida",
           Scientific_name == "Osmerus mordax" ~ "mordax",
           Scientific_name == "Platichthys stellatus" ~ "stellatus",
           TRUE ~ NA_character_  # Default case if no match is found
         ))

str(NCCOS_fish_processed)


#### NCCOS flatfish data -----------------------------------------------------

str(NCCOS_flatfish)
identical(names(NCCOS_clam), names(NCCOS_flatfish)) #The columns are identical as clams


NCCOS_flatfish_processed <- data.frame(
  Data_source = rep("NCCOS", nrow(NCCOS_flatfish)),
  Study_name = NCCOS_flatfish$Study,
  Source_siteID = NCCOS_flatfish$NST_Site,
  Source_sampleID = NCCOS_flatfish$Sample_ID,
  OSRI_siteID = NA,
  OSRI_sampleID = NA,
  Sample_motivation = NA,
  General_location = NCCOS_flatfish$General_Location,
  Specific_location = NCCOS_flatfish$Specific_Location,
  Lat = NCCOS_flatfish$Latitude,
  Long = NCCOS_flatfish$Longitude,
  Year = NCCOS_flatfish$Fiscal_Year,
  Month = NA,
  Collection_date =	NCCOS_flatfish$Collection_Date,
  DOY = NA,
  Collection_time = NA,
  Collection_method = NA,
  Species_complex = NCCOS_flatfish$Matrix,
  Common_name = NA,
  Scientific_name = NCCOS_flatfish$Scientific_Name,
  Genus_latin = NA,
  Species_latin = NA,
  Tissue_type = NA,
  Sample_composition = NA,
  Number_in_composite = NA,
  Sex = NA,
  Analysis_method = NCCOS_flatfish$Method,
  Chem_code = NA,
  Parameter = NCCOS_flatfish$Parameter,
  Value = NCCOS_flatfish$Value,
  Units = NCCOS_flatfish$Unit,
  Value_standardized = NA,
  Units_standardized = NA,
  Detection_limit = NA,
  Reporting_limit = NA,
  Basis = NA,
  Lab_replicate = NA,
  Qualifier_code = NCCOS_flatfish$Qualifier,
  Lipid_pct = NA,
  Moisture_pct = NA,
  Total_PAHs = NA,
  Total_LMWAHs = NA,
  Total_HMWAHs = NA,
  Lab_ID = NA,
  Notes = NA
)



unique(NCCOS_flatfish_processed$Scientific_name)
unique(NCCOS_flatfish_processed$Common_name)


NCCOS_flatfish_processed <- NCCOS_flatfish_processed %>% 
  mutate(Collection_date = as.Date(Collection_date, format = "%Y-%m-%d"),  
         Month = month(Collection_date),
         DOY = yday(Collection_date),
        
         Basis = "Dry"
         
         #There are no scientific names available in this dataset
         
         )

str(NCCOS_flatfish_processed)


#### NCCOS mussel data -----------------------------------------------------

str(NCCOS_mussel)
identical(names(NCCOS_clam), names(NCCOS_mussel)) #The columns are identical as clams


NCCOS_mussel_processed <- data.frame(
  Data_source = rep("NCCOS", nrow(NCCOS_mussel)),
  Study_name = NCCOS_mussel$Study,
  Source_siteID = NCCOS_mussel$NST_Site,
  Source_sampleID = NCCOS_mussel$Sample_ID,
  OSRI_siteID = NA,
  OSRI_sampleID = NA,
  Sample_motivation = NA,
  General_location = NCCOS_mussel$General_Location,
  Specific_location = NCCOS_mussel$Specific_Location,
  Lat = NCCOS_mussel$Latitude,
  Long = NCCOS_mussel$Longitude,
  Year = NCCOS_mussel$Fiscal_Year,
  Month = NA,
  Collection_date =	NCCOS_mussel$Collection_Date,
  DOY = NA,
  Collection_time = NA,
  Collection_method = NA,
  Species_complex = NCCOS_mussel$Matrix,
  Common_name = NA,
  Scientific_name = NCCOS_mussel$Scientific_Name,
  Genus_latin = NA,
  Species_latin = NA,
  Tissue_type = NA,
  Sample_composition = NA,
  Number_in_composite = NA,
  Sex = NA,
  Analysis_method = NCCOS_mussel$Method,
  Chem_code = NA,
  Parameter = NCCOS_mussel$Parameter,
  Value = NCCOS_mussel$Value,
  Units = NCCOS_mussel$Unit,
  Value_standardized = NA,
  Units_standardized = NA,
  Detection_limit = NA,
  Reporting_limit = NA,
  Basis = NA,
  Lab_replicate = NA,
  Qualifier_code = NCCOS_mussel$Qualifier,
  Lipid_pct = NA,
  Moisture_pct = NA,
  Total_PAHs = NA,
  Total_LMWAHs = NA,
  Total_HMWAHs = NA,
  Lab_ID = NA,
  Notes = NA
)



unique(NCCOS_mussel_processed$Scientific_name)


NCCOS_mussel_processed <- NCCOS_mussel_processed %>% 
  mutate(Collection_date = as.Date(Collection_date, format = "%Y-%m-%d"),  
         Month = month(Collection_date),
         DOY = yday(Collection_date),
         
         Basis = "Dry",
         
         Common_name = case_when(
           Scientific_name == "Mytilus edulis" ~ "Blue mussel",
           Scientific_name == "Mytilus" ~ "Mussel spp.",
           TRUE ~ NA_character_  # Default case if no match is found
         ),
         
         Genus_latin = case_when(
           Scientific_name == "Mytilus edulis" ~ "Mytilus",
           Scientific_name == "Mytilus" ~ "Mytilus",
           TRUE ~ NA_character_  # Default case if no match is found
         ),
         
         Species_latin = case_when(
           Scientific_name == "Mytilus edulis" ~ "edulis",
           Scientific_name == "Mytilus" ~ NA,
           TRUE ~ NA_character_  # Default case if no match is found
         ))

str(NCCOS_mussel_processed)

#### NCCOS shrimp data -----------------------------------------------------

str(NCCOS_shrimp)
identical(names(NCCOS_clam), names(NCCOS_shrimp)) #The columns are identical as clams


NCCOS_shrimp_processed <- data.frame(
  Data_source = rep("NCCOS", nrow(NCCOS_shrimp)),
  Study_name = NCCOS_shrimp$Study,
  Source_siteID = NCCOS_shrimp$NST_Site,
  Source_sampleID = NCCOS_shrimp$Sample_ID,
  OSRI_siteID = NA,
  OSRI_sampleID = NA,
  Sample_motivation = NA,
  General_location = NCCOS_shrimp$General_Location,
  Specific_location = NCCOS_shrimp$Specific_Location,
  Lat = NCCOS_shrimp$Latitude,
  Long = NCCOS_shrimp$Longitude,
  Year = NCCOS_shrimp$Fiscal_Year,
  Month = NA,
  Collection_date =	NCCOS_shrimp$Collection_Date,
  DOY = NA,
  Collection_time = NA,
  Collection_method = NA,
  Species_complex = NCCOS_shrimp$Matrix,
  Common_name = NA,
  Scientific_name = NCCOS_shrimp$Scientific_Name,
  Genus_latin = NA,
  Species_latin = NA,
  Tissue_type = NA,
  Sample_composition = NA,
  Number_in_composite = NA,
  Sex = NA,
  Analysis_method = NCCOS_shrimp$Method,
  Chem_code = NA,
  Parameter = NCCOS_shrimp$Parameter,
  Value = NCCOS_shrimp$Value,
  Units = NCCOS_shrimp$Unit,
  Value_standardized = NA,
  Units_standardized = NA,
  Detection_limit = NA,
  Reporting_limit = NA,
  Basis = NA,
  Lab_replicate = NA,
  Qualifier_code = NCCOS_shrimp$Qualifier,
  Lipid_pct = NA,
  Moisture_pct = NA,
  Total_PAHs = NA,
  Total_LMWAHs = NA,
  Total_HMWAHs = NA,
  Lab_ID = NA,
  Notes = NA
)



unique(NCCOS_shrimp_processed$Scientific_name)



NCCOS_shrimp_processed <- NCCOS_shrimp_processed %>% 
  mutate(Collection_date = as.Date(Collection_date, format = "%Y-%m-%d"),  
         Month = month(Collection_date),
         DOY = yday(Collection_date),
         
         Basis = "Dry"
         
         #No species information available here
         )

str(NCCOS_shrimp_processed)


#### NCCOS starfish data -----------------------------------------------------


str(NCCOS_starfish)
identical(names(NCCOS_clam), names(NCCOS_starfish)) #The columns are identical as clams


NCCOS_starfish_processed <- data.frame(
  Data_source = rep("NCCOS", nrow(NCCOS_starfish)),
  Study_name = NCCOS_starfish$Study,
  Source_siteID = NCCOS_starfish$NST_Site,
  Source_sampleID = NCCOS_starfish$Sample_ID,
  OSRI_siteID = NA,
  OSRI_sampleID = NA,
  Sample_motivation = NA,
  General_location = NCCOS_starfish$General_Location,
  Specific_location = NCCOS_starfish$Specific_Location,
  Lat = NCCOS_starfish$Latitude,
  Long = NCCOS_starfish$Longitude,
  Year = NCCOS_starfish$Fiscal_Year,
  Month = NA,
  Collection_date =	NCCOS_starfish$Collection_Date,
  DOY = NA,
  Collection_time = NA,
  Collection_method = NA,
  Species_complex = NCCOS_starfish$Matrix,
  Common_name = NA,
  Scientific_name = NCCOS_starfish$Scientific_Name,
  Genus_latin = NA,
  Species_latin = NA,
  Tissue_type = NA,
  Sample_composition = NA,
  Number_in_composite = NA,
  Sex = NA,
  Analysis_method = NCCOS_starfish$Method,
  Chem_code = NA,
  Parameter = NCCOS_starfish$Parameter,
  Value = NCCOS_starfish$Value,
  Units = NCCOS_starfish$Unit,
  Value_standardized = NA,
  Units_standardized = NA,
  Detection_limit = NA,
  Reporting_limit = NA,
  Basis = NA,
  Lab_replicate = NA,
  Qualifier_code = NCCOS_starfish$Qualifier,
  Lipid_pct = NA,
  Moisture_pct = NA,
  Total_PAHs = NA,
  Total_LMWAHs = NA,
  Total_HMWAHs = NA,
  Lab_ID = NA,
  Notes = NA
)



unique(NCCOS_starfish_processed$Scientific_name)



NCCOS_starfish_processed <- NCCOS_starfish_processed %>% 
  mutate(Collection_date = as.Date(Collection_date, format = "%Y-%m-%d"),  
         Month = month(Collection_date),
         DOY = yday(Collection_date),
         
         Basis = "Dry"
         
         #No scientific names here
         )



str(NCCOS_starfish_processed)



#### Combine the NCCOS data ----------------------------------------------

NCCOS_data_processed <- rbind(NCCOS_clam_processed, NCCOS_cockles_processed, NCCOS_fish_processed,
                    NCCOS_flatfish_processed, NCCOS_mussel_processed, NCCOS_shrimp_processed, 
                    NCCOS_starfish_processed) 

#Figure out which one of these Morgan wants done!!!!!!!!!!!!!!!!!!!!!!!!
NCCOS_data_processed <- NCCOS_data_processed %>% 
  select(Study_name != "Mussel Watch")

#NCCOS_data_processed <- NCCOS_data_processed %>% 
#  filter(str_detect(tolower(Study_name), "mussel watch"))


View(NCCOS_data_processed) #22,428 data points!!



# Diver Data # --------------------------------------------------------------
#The DIVER_Explorer_2025_03_19_points_ExportTable is only from the geospatial data, it is not the complete dataset
#Diver_data_points <- read.csv("Input Data/DIVER_Explorer_2025_03_19_points/DIVER_Explorer_2025_03_19_points_ExportTable.csv")      
Diver_data <- read.csv("Input Data/DIVER_Explorer_2025_03_19_points/DIVER_Alaska_Tissue_PAH_2025_03_20_Samples.csv")      
#Diver_names <- read.csv("Input Data/DIVER_Explorer_2025_03_19_points/DIVER_Explorer_2025_03_19_points_field_names.csv")      

View(Diver_data_points)
str(Diver_data_points)
#'data.frame':	1694 obs. of  25 variables:
#  $ ï..Case_Act: chr  "Alaska (Mussel Watch)" "Alaska (Mussel Watch)" "Alaska (Mussel Watch)" "Alaska (Mussel Watch)" ...
#$ QMSITEID   : chr  "MWAK" "MWAK" "MWAK" "MWAK" ...
#$ StudyName  : chr  "Mussel Watch:Alaska 1990" "Mussel Watch:Alaska 1990" "Mussel Watch:Alaska 1991" "Mussel Watch:Alaska 1991" ...
#$ StudyID    : chr  "05" "05" "06" "06" ...
#$ AREADESC   : chr  "Port Valdez" "Unakwit Inlet" "Port Valdez" "Unakwit Inlet" ...
#$ sta_site   : chr  "PVMC" "UISB" "PVMC" "UISB" ...
#$ STADESC    : chr  "Mineral Creek Flats" "Siwash Bay" "Mineral Creek Flats" "Siwash Bay" ...
#$ STATGRPLST : chr  "Alaska/Arctic; National Program" "Alaska/Arctic; National Program" "Alaska/Arctic; National Program" "Alaska/Arctic; National Program" ...
#$ CollMatrix : chr  "Tissue" "Tissue" "Tissue" "Tissue" ...
#$ CollForm   : chr  "NOAA Posted Database" "NOAA Posted Database" "NOAA Posted Database" "NOAA Posted Database" ...
#$ WKSPCNAME  : chr  "Not Defined" "Not Defined" "Not Defined" "Not Defined" ...
#$ DataClass  : chr  "Not Defined" "Not Defined" "Not Defined" "Not Defined" ...
#$ DataSource : chr  "Mussel Watch" "Mussel Watch" "Mussel Watch" "Mussel Watch" ...
#$ srctype    : chr  "NOAA Posted Database" "NOAA Posted Database" "NOAA Posted Database" "NOAA Posted Database" ...
#$ sharestat  : chr  "Publicly Available" "Publicly Available" "Publicly Available" "Publicly Available" ...
#$ Workgroup  : chr  "Not Defined" "Not Defined" "Not Defined" "Not Defined" ...
#$ Workplan   : chr  "Not Defined" "Not Defined" "Not Defined" "Not Defined" ...
#$ source     : chr  "Samples" "Samples" "Samples" "Samples" ...
#$ loc_geom   : chr  "POINT(-146.461 61.13283333)" "POINT(-147.646 60.96083333)" "POINT(-146.461 61.13283333)" "POINT(-147.646 60.96083333)" ...
#$ legend_tex : chr  "Tissue" "Tissue" "Tissue" "Tissue" ...
#$ map_index  : int  1 1 1 1 1 1 1 1 1 1 ...
#$ startlat   : num  61.1 61 61.1 61 61.1 ...
#$ startlong  : num  -146 -148 -146 -148 -146 ...
#$ endlat     : logi  NA NA NA NA NA NA ...
#$ endlong    : logi  NA NA NA NA NA NA ...



#View(Diver_data)
str(Diver_data)
#'data.frame':	115046 obs. of  76 variables:
#$ Date                    : chr  "1989-04-29" "1989-04-29" "1989-04-29" "1989-04-29" ...
#$ Case_Activity           : chr  "Arctic/Alaska" "Arctic/Alaska" "Arctic/Alaska" "Arctic/Alaska" ...
#$ Site_ID                 : chr  "1096" "1096" "1096" "1096" ...
#$ Study_Name              : chr  "NOAA EVOS Subsistence Marine Mammal  1989" "NOAA EVOS Subsistence Marine Mammal  1989" "NOAA EVOS Subsistence Marine Mammal  1989" "NOAA EVOS Subsistence Marine Mammal  1989" ...
#$ Study_ID                : chr  "A3" "A3" "A3" "A3" ...
#$ Area_Description        : chr  "Not Defined" "Not Defined" "Not Defined" "Not Defined" ...
#$ Station                 : chr  "TS-HS-1" "TS-HS-1" "TS-HS-1" "TS-HS-1" ...
#$ Station_Description     : chr  "Herring Bay" "Herring Bay" "Herring Bay" "Herring Bay" ...
#$ Related_Sample_Group    : chr  "Not Defined" "Not Defined" "Not Defined" "Not Defined" ...
#$ Station_Group_List      : chr  "Alaska/Arctic" "Alaska/Arctic" "Alaska/Arctic" "Alaska/Arctic" ...
#$ DB_Sample_ID            : chr  "T001B" "T001B" "T001B" "T001B" ...
#$ Sample_ID               : chr  "1096-A3-60-1325" "1096-A3-60-1325" "1096-A3-60-1325" "1096-A3-60-1325" ...
#$ Collection_Matrix       : chr  "Tissue" "Tissue" "Tissue" "Tissue" ...
#$ Species_Group           : chr  "Mammals" "Mammals" "Mammals" "Mammals" ...
#$ Reported_Taxon          : chr  "Harbor Seal" "Harbor Seal" "Harbor Seal" "Harbor Seal" ...
#$ Lowest_Taxa_Level       : chr  "Not Defined" "Not Defined" "Not Defined" "Not Defined" ...
#$ Common_Name_Species     : chr  "Harbor Seal" "Harbor Seal" "Harbor Seal" "Harbor Seal" ...
#$ Latin_Name_Genus        : chr  "Not Standardized" "Not Standardized" "Not Standardized" "Not Standardized" ...
#$ Latin_Name_Species      : chr  "Phoca vitulina" "Phoca vitulina" "Phoca vitulina" "Phoca vitulina" ...
#$ Collection_Form         : chr  "NOAA Posted Database" "NOAA Posted Database" "NOAA Posted Database" "NOAA Posted Database" ...
#$ Workspace_Name          : chr  "Not Defined" "Not Defined" "Not Defined" "Not Defined" ...
#$ Data_Classification     : chr  "Not Defined" "Not Defined" "Not Defined" "Not Defined" ...
#$ Data_Source             : chr  "NOAA" "NOAA" "NOAA" "NOAA" ...
#$ Source_Type             : chr  "NOAA Posted Database" "NOAA Posted Database" "NOAA Posted Database" "NOAA Posted Database" ...
#$ Sharing_Status          : chr  "Publicly Available" "Publicly Available" "Publicly Available" "Publicly Available" ...
#$ Workgroup               : chr  "Not Defined" "Not Defined" "Not Defined" "Not Defined" ...
#$ Collection_Workplan     : chr  "Not Defined" "Not Defined" "Not Defined" "Not Defined" ...
#$ Habitat_Type            : chr  "Not Defined" "Not Defined" "Not Defined" "Not Defined" ...
#$ Organism_ID             : chr  "Not Defined" "Not Defined" "Not Defined" "Not Defined" ...
#$ Results_Status          : chr  "Results Reported" "Results Reported" "Results Reported" "Results Reported" ...
#$ DIVER_Dataset           : chr  "Samples - Northwest" "Samples - Northwest" "Samples - Northwest" "Samples - Northwest" ...
#$ File_Collection_ID      : chr  "Not Defined" "Not Defined" "Not Defined" "Not Defined" ...
#$ Analysis_Category       : chr  "Contaminant Chemistry" "Contaminant Chemistry" "Contaminant Chemistry" "Contaminant Chemistry" ...
#$ Analysis_Type           : chr  "Polycyclic aromatic hydrocarbons" "Polycyclic aromatic hydrocarbons" "Polycyclic aromatic hydrocarbons" "Polycyclic aromatic hydrocarbons" ...
#$ Analysis                : chr  "Acenaphthene" "Acenaphthylene" "Benzo(a)anthracene" "Benzo(a)pyrene" ...
#$ ChemCode                : chr  "ACENAPTHEN" "ACENAPTYLE" "BAA" "BAP" ...
#$ Lab_Replicate           : int  1 1 1 1 1 1 1 1 1 1 ...
#$ Analysis_Result         : chr  "0.50000000" "-9.00000000" "0.30000000" "0.30000000" ...
#$ Analysis_Result_Unit    : chr  "PPB" "PPB" "PPB" "PPB" ...
#$ Measurement_Basis       : chr  "WW" "WW" "WW" "WW" ...     I THINK THIS IS WET WEIGHT VS DRY WEIGHT
#$ Matrix_Group            : chr  "Tissue" "Tissue" "Tissue" "Tissue" ...
#$ Analysis_Matrix         : chr  "TS" "TS" "TS" "TS" ...
#$ Analysis_Matrix_Detailed: chr  "Fat" "Fat" "Fat" "Fat" ...
#$ Analysis_Method         : chr  "NR" "NR" "NR" "NR" ...
#$ Qualifier_Code          : chr  "U" "Not Defined" "U" "U" ...
#$ Detection_Extent        : chr  "Not Defined" "Not Defined" "Not Defined" "Not Defined" ...
#$ Review_Status           : chr  "Not Defined" "Not Defined" "Not Defined" "Not Defined" ...
#$ Analysis_Detail         : chr  "1096-A3-60-1325 | 1 | Acenaphthene" "1096-A3-60-1325 | 1 | Acenaphthylene" "1096-A3-60-1325 | 1 | Benzo(a)anthracene" "1096-A3-60-1325 | 1 | Benzo(a)pyrene" ...
#$ Detection_Limit         : num  -9 -9 -9 -9 -9 -9 -9 -9 -9 -9 ...
#$ Reporting_Limit         : int  -9 -9 -9 -9 -9 -9 -9 -9 -9 -9 ...
#$ Validation_Level        : chr  "UNK" "UNK" "UNK" "UNK" ...
#$ Collection_Method       : chr  "UNK" "UNK" "UNK" "UNK" ...
#$ Depth_Category          : chr  "Not Applicable" "Not Applicable" "Not Applicable" "Not Applicable" ...
#$ Sample_Upper_Depth      : int  -9 -9 -9 -9 -9 -9 -9 -9 -9 -9 ...
#$ Sample_Lower_Depth      : int  -9 -9 -9 -9 -9 -9 -9 -9 -9 -9 ...
#$ Sample_Depth_Unit       : chr  "cm" "cm" "cm" "cm" ...
#$ Lab_ID                  : chr  "UNK" "UNK" "UNK" "UNK" ...
#$ Sample_Type             : chr  "Sample" "Sample" "Sample" "Sample" ...
#$ Result_Type             : chr  "Result" "Result" "Result" "Result" ...
#$ Lab_Name                : chr  "Unknown" "Unknown" "Unknown" "Unknown" ...
#$ Sample_Delivery_Group   : chr  "NR" "NR" "NR" "NR" ...
#$ PDB_Sample_Details      : chr  "1096-A3-TS-HS-1-T001B" "1096-A3-TS-HS-1-T001B" "1096-A3-TS-HS-1-T001B" "1096-A3-TS-HS-1-T001B" ...
#$ TOC_pct                 : int  -9 -9 -9 -9 -9 -9 -9 -9 -9 -9 ...
#$ Lipid_pct               : num  -9 -9 -9 -9 -9 -9 -9 -9 -9 -9 ...
#$ Number_in_Composite     : int  1 1 1 1 1 1 1 1 1 1 ...
#$ Length_cm               : int  -9 -9 -9 -9 -9 -9 -9 -9 -9 -9 ...
#$ Weight_g                : int  -9 -9 -9 -9 -9 -9 -9 -9 -9 -9 ...
#$ Age_yrs                 : int  -9 -9 -9 -9 -9 -9 -9 -9 -9 -9 ...
#$ Tissue_Type             : chr  "Fat" "Fat" "Fat" "Fat" ...
#$ Sex                     : chr  "U" "U" "U" "U" ...
#$ Data_Category           : chr  "Samples" "Samples" "Samples" "Samples" ...
#$ Location_Geom           : chr  "POINT(-147.415204 60.431821)" "POINT(-147.415204 60.431821)" "POINT(-147.415204 60.431821)" "POINT(-147.415204 60.431821)" ...
#$ Start_Latitude          : num  60.4 60.4 60.4 60.4 60.4 ...
#$ Start_Longitude         : num  -147 -147 -147 -147 -147 ...
#$ End_Latitude            : logi  NA NA NA NA NA NA ...
#$ End_Longitude           : logi  NA NA NA NA NA NA ...



Diver_processed <- data.frame(
  Data_source = rep("Diver", nrow(Diver_data)),
  Study_name = Diver_data$Study_Name,
  Source_siteID = Diver_data$Site_ID,
  Source_sampleID = Diver_data$Sample_ID,
  OSRI_siteID = NA,
  OSRI_sampleID = NA,
  Sample_motivation = NA,
  General_location = Diver_data$Station_Description,
  Specific_location = Diver_data$Station,   #might need to adjust this one.... 
  Lat = Diver_data$Start_Latitude,
  Long = Diver_data$Start_Longitude,
  Year = NA,                                #Need to generate this info
  Month = NA,                               #Need to generate this info
  Collection_date =	Diver_data$Date,
  DOY = NA,                                 #Need to generate this info
  Collection_time = NA,
  Collection_method = Diver_data$Collection_Method, #Replace UNK with NA
  Species_complex = Diver_data$Species_Group,
  Common_name = Diver_data$Common_Name_Species,
  Scientific_name = Diver_data$Latin_Name_Species,
  Genus_latin = NA,                        #Need to calculate these below
  Species_latin = NA,                        #Need to calculate these below
  Tissue_type = Diver_data$Tissue_Type,
  Sample_composition = NA,                   #Need to calculate this from the column below
  Number_in_composite = Diver_data$Number_in_Composite,
  Sex = NA,                                  #No data in this dataset
  Analysis_method = Diver_data$Analysis_Method, 
  Chem_code = NA,                           
  Parameter = Diver_data$Analysis,
  Value = Diver_data$Analysis_Result,
  Units = Diver_data$Analysis_Result_Unit,
  Value_standardized = NA,
  Units_standardized = NA,
  Detection_limit = Diver_data$Detection_Limit,
  Reporting_limit = Diver_data$Reporting_Limit,
  Basis = Diver_data$Measurement_Basis,
  Lab_replicate = Diver_data$Lab_Replicate,
  Qualifier_code = Diver_data$Qualifier_Code,
  Lipid_pct = Diver_data$Lipid_pct,
  Moisture_pct = NA,
  Total_PAHs = NA,
  Total_LMWAHs = NA,
  Total_HMWAHs = NA,
  Lab_ID = NA,
  Notes = as.character(NA))


#Need to extract lat long from Location_Geom


Diver_processed <- Diver_processed %>% select(-Genus_latin, -Species_latin)


Diver_processed <- Diver_processed %>%
  mutate(
    Collection_date = as.Date(Collection_date, format = "%Y-%m-%d"),  
    Year = year(Collection_date),
    Month = month(Collection_date),
    DOY = yday(Collection_date),
    Collection_method = ifelse(Collection_method == "UNK", NA, Collection_method),
    
    Sample_composition = ifelse(Number_in_composite < 1, "composite", "individual"),
    Sample_composition = ifelse(Number_in_composite %in% c(-999, -9), NA, Sample_composition),
    
    Scientific_name = str_trim(Scientific_name),
    Scientific_name = ifelse(Scientific_name == "Clupea pallasii pallasiiÂ", "Clupea pallasii", Scientific_name),
    Scientific_name = ifelse(Scientific_name == "Octopus", "Octopoda", Scientific_name)
  ) %>%
  separate_wider_delim(
    Scientific_name,
    delim = " ",
    names = c("Genus_latin", "Species_latin"),
    too_few = "align_start",
    too_many = "drop",
    cols_remove = FALSE
  )




latlong <- data.frame(Diver_data$Location_Geom) 
latlong <- latlong %>%   
  mutate(Diver_data.Location_Geom = str_remove_all(Diver_data.Location_Geom, "POINT\\(|\\)")) %>% 
  separate(Diver_data.Location_Geom, into = c("Long", "Lat"), sep = " ", convert = TRUE)
head(latlong)

Diver_processed <- Diver_processed %>% 
  mutate(
    Lat = latlong$Lat,
    Long = latlong$Long
    )



str(Diver_processed)
head(Diver_processed) #115,046 data points!



compare_column_names(Diver_processed)




# Nationwide PAHs (supplement to NCCOS and Diver) -------------------------




#PAH Data 
Nationwide_data <- read.csv("Input Data/Nationwide Tissue PAHs.csv")      
#PAH sum data
Nationwide_sum <- read.csv("Input Data/Nationwide Tissue PAHs Sums.csv")      

#Filter just to Alaska samples
Nationwide_data <- Nationwide_data %>% filter(State == "Alaska")
Nationwide_sum <- Nationwide_sum %>% filter(State == "Alaska")


str(Nationwide_data)
#'data.frame':	3328 obs. of  23 variables:
#  $ Site_ID                         : chr  "UISB" "KTMP" "CIHS" "PVMC" ...
#$ Study                           : chr  "Mussel Watch" "Mussel Watch" "Mussel Watch" "Mussel Watch" ...
#$ General_Location                : chr  "Unakwit Inlet" "Ketchikan" "Cook Inlet" "Port Valdez" ...
#$ Specific_Location               : chr  "Siwash Bay" "Mountain Point" "Homer Spit" "Mineral Creek Flats" ...
#$ State                           : chr  "Alaska" "Alaska" "Alaska" "Alaska" ...
#$ Region                          : chr  "Pacific Coast" "Pacific Coast" "Pacific Coast" "Pacific Coast" ...
#$ Latitude                        : num  61 55.3 59.6 61.1 61 ...
#$ Longitude                       : num  -148 -132 -151 -146 -148 ...
#$ Collaborator                    : chr  "" "" "" "" ...
#$ Matrix                          : chr  "Mussel" "Mussel" "Mussel" "Mussel" ...
#$ Scientific_Name                 : chr  "Mytilus species" "Mytilus edulis" "Mytilus species" "Mytilus species" ...
#$ Calendar_Year                   : int  2003 2009 2007 2007 2001 1995 1999 1999 1992 1995 ...
#$ Parameter_Code                  : chr  "ACENTHE" "ACENTHE" "ACENTHE" "ACENTHE" ...
#$ Parameter                       : chr  "Acenaphthene" "Acenaphthene" "Acenaphthene" "Acenaphthene" ...
#$ Unit                            : chr  "ng/dry g" "ng/dry g" "ng/dry g" "ng/dry g" ...
#$ Method                          : chr  "PAH-2002" "PAH-2002" "PAH-2002" "PAH-2002" ...
#$ Laboratory                      : chr  "" "" "" "" ...
#$ Previous_MWP_Group              : chr  "PAHlmw" "PAHlmw" "PAHlmw" "PAHlmw" ...
#$ TechMemo_Group                  : chr  "Total PAHs" "Total PAHs" "Total PAHs" "Total PAHs" ...
#$ TechMemo_Group_withPPCPSubgroups: chr  "Total PAHs" "Total PAHs" "Total PAHs" "Total PAHs" ...
#$ Value                           : num  6.1 17.8 0 0 0.7 ...
#$ Detection_Limit                 : num  NA NA NA NA NA NA NA NA NA NA ...
#$ PresAbs                         : int  1 1 0 0 1 0 0 1 0 1 ...

str(Nationwide_sum)
#'data.frame':	65 obs. of  17 variables:
#  $ Site_ID          : chr  "UISB" "KTMP" "CIHS" "PVMC" ...
#$ Study            : chr  "Mussel Watch" "Mussel Watch" "Mussel Watch" "Mussel Watch" ...
#$ General_Location : chr  "Unakwit Inlet" "Ketchikan" "Cook Inlet" "Port Valdez" ...
#$ Specific_Location: chr  "Siwash Bay" "Mountain Point" "Homer Spit" "Mineral Creek Flats" ...
#$ State            : chr  "Alaska" "Alaska" "Alaska" "Alaska" ...
#$ Region           : chr  "Pacific Coast" "Pacific Coast" "Pacific Coast" "Pacific Coast" ...
#$ Latitude         : num  61 55.3 59.6 61.1 61 ...
#$ Longitude        : num  -148 -132 -151 -146 -148 ...
#$ Collaborator     : chr  "" "" "" "" ...
#$ Matrix           : chr  "Mussel" "Mussel" "Mussel" "Mussel" ...
#$ Scientific_Name  : chr  "Mytilus species" "Mytilus edulis" "Mytilus species" "Mytilus species" ...
#$ Calendar_Year    : int  2003 2009 2007 2007 2001 1995 1999 1999 1995 1995 ...
#$ Unit             : chr  "ng/dry g" "ng/dry g" "ng/dry g" "ng/dry g" ...
#$ Laboratory       : chr  "" "" "" "" ...
#$ TechMemo_Group   : chr  "Total PAHs" "Total PAHs" "Total PAHs" "Total PAHs" ...
#$ Sum              : num  436.8 127.1 224 194.7 24.2 ...
#$ Count            : int  39 39 39 39 39 39 39 39 39 39 ...




#Pick identifying columns (adjust as needed)
id_cols <- c("Site_ID", "Scientific_Name", "Calendar_Year", "Specific_Location")

# Check overlap
overlap <- inner_join(Nationwide_data, Nationwide_sum, by = id_cols)

# Show how many are duplicated
nrow(overlap)

# Alternatively: show just the overlapping IDs
overlapping_ids <- semi_join(Nationwide_data, Nationwide_sum, by = id_cols)
distinct(overlapping_ids, across(all_of(id_cols)))
#All of the PAH sum columns match the full dataset, so I'll join them

# Join the non-redundant columns from Nationwide_sum into Nationwide_data
Nationwide_data <- Nationwide_data %>%
  left_join(
    Nationwide_sum %>% select(all_of(id_cols), Sum, Count),
    by = id_cols
  )





#Now process it
str(Nationwide_data)


Nationwide_data_processed <- data.frame(
  Data_source = rep("Nationwide", nrow(Nationwide_data)),
  Study_name = Nationwide_data$Study,
  Source_siteID = Nationwide_data$Site_ID,
  Source_sampleID = Nationwide_data$Site_ID,
  OSRI_siteID = NA,
  OSRI_sampleID = NA,
  Sample_motivation = NA,
  General_location = Nationwide_data$General_Location,
  Specific_location = Nationwide_data$Specific_Location,   
  Lat = Nationwide_data$Latitude,
  Long = Nationwide_data$Longitude,
  Year = Nationwide_data$Calendar_Year,
  Month = NA,                               
  Collection_date =	NA,
  DOY = NA,                                 
  Collection_time = NA,
  Collection_method = NA, 
  Species_complex = NA,                 #Need to calculate
  Common_name = NA,                     #Need to calculate
  Scientific_name = Nationwide_data$Scientific_Name,
  Genus_latin = NA,                        #Need to calculate these below
  Species_latin = NA,                        #Need to calculate these below
  Tissue_type = Nationwide_data$Matrix,
  Sample_composition = NA,                   
  Number_in_composite = NA,
  Sex = NA,                                  
  Analysis_method = Nationwide_data$Method, 
  Chem_code = Nationwide_data$Parameter_Code,
  Parameter = Nationwide_data$Parameter,
  Value = Nationwide_data$Value,
  Units = Nationwide_data$Unit,
  Value_standardized = NA,
  Units_standardized = NA,
  Detection_limit = Nationwide_data$Detection_Limit,
  Reporting_limit = NA,
  Basis = NA,
  Lab_replicate = NA,
  Qualifier_code = NA,
  Lipid_pct = NA,
  Moisture_pct = NA,
  Total_PAHs = Nationwide_data$Sum,
  Total_LMWAHs = NA,
  Total_HMWAHs = NA,
  Lab_ID = Nationwide_data$Laboratory,
  Notes = Nationwide_data$Method
)






Nationwide_data_processed <- Nationwide_data_processed %>%
  mutate(
    Scientific_name = case_when(
      Scientific_name == "Mytilus species" ~ "Mytilus spp.",
      Scientific_name == "Siliqua Patula" ~ "Siliqua patula",
    ),
    
    Common_name = case_when(
      Scientific_name == "Mytilus edulis" ~ "Blue mussel",
      Scientific_name == "Siliqua patula" ~ "Pacific razor clam",
      TRUE ~ NA_character_  # Default case if no match is found
    ),
    
    Genus_latin = case_when(
      Scientific_name == "Mytilus species" ~ "Mytilus",
      Scientific_name == "Mytilus edulis" ~ "Mytilus",
      Scientific_name == "Siliqua patula" ~ "Siliqua",
      TRUE ~ NA_character_  # Default case if no match is found
    ),
    
    Species_latin = case_when(
      Scientific_name == "Mytilus species" ~ NA,
      Scientific_name == "Mytilus edulis" ~ "edulis",
      Scientific_name == "Siliqua patula" ~ "patula",
      TRUE ~ NA_character_  # Default case if no match is found
    ),
      
      Species_complex = case_when(
      Scientific_name == "Mytilus species" ~ "Mussel",
      Scientific_name == "Mytilus edulis" ~ "Mussel",
      Scientific_name == "Siliqua patula" ~ "Clam",
      TRUE ~ NA_character_  # Default case if no match is found
    ))
    
    
#Remove duplicate samples, selected by Morgan 
Nationwide_data_processed <- Nationwide_data_processed %>% 
  filter(Year == 2012)


str(Nationwide_data_processed)
head(Nationwide_data_processed) 



### Check for redundancy between NCCOS and Diver datasets -------------------



unique_motivations_MP <- read_xlsx("Output Data/unique_motivations_MP.xlsx")      
unique_motivations_MP











# Define identifying columns for overlap
id_cols <- c("Source_sampleID", "Source_siteID", "Scientific_name", "Year", "Parameter", "Value", "Units")

# Ensure matching column types
NCCOS_data_processed <- NCCOS_data_processed %>%
  mutate(across(all_of(id_cols), as.character))

Nationwide_data_processed <- Nationwide_data_processed %>%
  mutate(across(all_of(id_cols), as.character))

# Identify overlapping rows
overlap <- inner_join(NCCOS_data_processed, Nationwide_data_processed, by = id_cols)

# Filter out overlapping rows from NCCOS
NCCOS_unique <- NCCOS_data_processed %>%
  anti_join(Nationwide_data_processed, by = id_cols)

# Optional: print how many rows overlap and how many remain
cat("Overlapping rows:", nrow(overlap), "\n")
cat("Remaining unique NCCOS rows:", nrow(NCCOS_unique), "\n")




#Check the diver data
# Step 1: Define only existing, shared identifying columns
id_cols <- c("Source_sampleID", "Source_siteID", "Scientific_name", "Year", "Value", "Units")

# Step 2: Standardize column types
Nationwide_data_processed <- Nationwide_data_processed %>%
  mutate(across(all_of(id_cols), as.character))

Diver_processed <- Diver_processed %>%
  mutate(across(all_of(id_cols), as.character))

# Step 3: Find overlapping rows (identical samples)
overlap <- inner_join(Nationwide_data_processed, Diver_processed, by = id_cols)

# Step 4: Remove overlapping rows from Nationwide
Nationwide_unique <- Nationwide_data_processed %>%
  anti_join(Diver_processed, by = id_cols)

# Step 5: Output summary
cat("Overlapping rows removed from Nationwide:", nrow(overlap), "\n")
cat("Remaining unique Nationwide rows:", nrow(Nationwide_unique), "\n")



#No overlapping data in either of them -- NEED TO REVISIT THIS


#Removing one duplicate study
OSRI_data <- OSRI_data %>%
  filter(Study_name != "NOAA Prince William Sound RCAC 1993-2007")



# Wetzel Data # -------------------------------------------------------------

#Looks like I will need to assign better location data to this data
Wetzel_fish <- read_excel("Input Data/Wetzel Lab_PAHinAKTissues_3_25_25 copy.xlsx", sheet = "Fish")      
Wetzel_crustaceans <- read_excel("Input Data/Wetzel Lab_PAHinAKTissues_3_25_25 copy.xlsx", sheet = "Crustaceans")      
Wetzel_pinnipeds <- read_excel("Input Data/Wetzel Lab_PAHinAKTissues_3_25_25 copy.xlsx", sheet = "Pinnipeds")      
Wetzel_whale <- read_excel("Input Data/Wetzel Lab_PAHinAKTissues_3_25_25 copy.xlsx", sheet = "Whale")      

View(Wetzel_fish)
str(Wetzel_fish)
#tibble [552 x 7] (S3: tbl_df/tbl/data.frame)
#$ Sample   : chr [1:552] "182" "183" "184" "185" ...
#$ Year     : chr [1:552] "2024" "2024" "2024" "2024" ...
#$ Species  : chr [1:552] "Arctic Cisco" "Arctic Cisco" "Arctic Cisco" "Arctic Cisco" ...
#$ Matrix   : chr [1:552] "Liver" "Liver" "Liver" "Liver" ...
#$ Site     : chr [1:552] "Nigliq Channel, AK" "Nigliq Channel, AK" "Nigliq Channel, AK" "Nigliq Channel, AK" ...
#$ SPAH ug/g: chr [1:552] "1.4091111434282744" "1.1987870109138596" "0.81645259532922787" "2.4742391987837746" ...
#$ SPAH ng/g: chr [1:552] NA NA NA NA ...



### Wetzel fish data ----------------------------------------------------

Wetzel_fish_processed <- data.frame(
  Data_source = rep("Wetzel", nrow(Wetzel_fish)),
  Study_name = NA,                         #Is there actually no study info?
  Source_siteID = Wetzel_fish$Site,
  Source_sampleID = Wetzel_fish$Sample,
  OSRI_siteID = NA,
  OSRI_sampleID = NA,
  Sample_motivation = NA,
  General_location = Wetzel_fish$Site,
  Specific_location = NA,   
  Lat = NA,
  Long = NA,
  Year = Wetzel_fish$Year,                                
  Month = NA,                               
  Collection_date =	NA,
  DOY = NA,                                
  Collection_time = NA,
  Collection_method = NA, 
  Species_complex = rep("fish", nrow(Wetzel_fish)),
  Common_name = Wetzel_fish$Species,         
  Scientific_name = NA,    #Need to generate this
  Genus_latin = NA,                        #Need to calculate these below
  Species_latin = NA,                        #Need to calculate these below
  Tissue_type = Wetzel_fish$Matrix,
  Sample_composition = NA,                   #Need to calculate this from the column below
  Number_in_composite = NA,
  Sex = NA,                                  #No data in this dataset
  Analysis_method = NA, #ORR this might be the analysis column
  Chem_code = NA,                           #might be able to get this from Analysis_method??
  Parameter = NA,
  Value = Wetzel_fish$"SPAH ug/g",
  Units = rep("ug/g", nrow(Wetzel_fish)),
  Value_standardized = NA,
  Units_standardized = NA,
  Detection_limit = NA,
  Reporting_limit = NA,
  Basis = NA,
  Lab_replicate = NA,
  Qualifier_code = ifelse(Wetzel_fish$"SPAH ug/g" == "BDL", "BDL", NA),
  Lipid_pct = NA,
  Moisture_pct = NA,
  Total_PAHs = NA,
  Total_LMWAHs = NA,
  Total_HMWAHs = NA,
  Lab_ID = NA,
  Notes = NA
)



unique(Wetzel_fish_processed$Common_name)




# Create a lookup table
fish_lookup <- tibble(
  Common_name = c(
    "Arctic Cisco", "Broad Whitefish", "Burbot", "Butter Sole", "Dover Sole",
    "Eulachon (hooligan)", "Flathead Sole", "Grayling", "King Salmon", "Lake Trout",
    "Least Cisco", "Pacific Cod", "Pacific Sandfish", "Pacific Tom Cod", "Pink Salmon",
    "Pink Salmon?", "Round Whitefish", "Sandfish", "Sculpin", "Silver Salmon",
    "Longfin Smelt", "Snailfish", "SnailFish", "Sockeye Salmon", "Starry Flounder",
    "Walleye Pollock"
  ),
  Scientific_name = c(
    "Coregonus autumnalis", "Coregonus nasus", "Lota lota", "Isopsetta isolepis", "Microstomus pacificus",
    "Thaleichthys pacificus", "Hippoglossoides elassodon", "Thymallus arcticus", "Oncorhynchus tshawytscha", "Salvelinus namaycush",
    "Coregonus sardinella", "Gadus macrocephalus", "Trichodon trichodon", "Microgadus proximus", "Oncorhynchus gorbuscha",
    "Oncorhynchus gorbuscha", "Prosopium cylindraceum", "Trichodon trichodon", "Cottidae", "Oncorhynchus kisutch",
    "Spirinchus thaleichthys", "Liparidae", "Liparidae", "Oncorhynchus nerka", "Platichthys stellatus",
    "Gadus chalcogrammus"
  )
) %>%
  mutate(
    Genus_latin = sub(" .*", "", Scientific_name),
    Species_latin = sub(".* ", "", Scientific_name)
  )



# Join to your main data and replace NA in the matching columns
Wetzel_fish_processed <- Wetzel_fish_processed %>%
  left_join(fish_lookup, by = "Common_name") #%>%

Wetzel_fish_processed <- Wetzel_fish_processed %>%
  mutate(
    Scientific_name.x = Scientific_name.y,
    Genus_latin.x = Genus_latin.y,
    Species_latin.x = Species_latin.y
  ) %>%
  select(-Scientific_name.y, -Genus_latin.y, -Species_latin.y) %>% #
  rename(Scientific_name = Scientific_name.x,
         Genus_latin = Genus_latin.x,
         Species_latin = Species_latin.x)


Wetzel_fish_processed <- Wetzel_fish_processed %>%
  mutate(Common_name = ifelse(Common_name == "SnailFish", "Snailfish", Common_name))


str(Wetzel_fish_processed)


### Wetzel crustaceans data ----------------------------------------------------

identical(names(Wetzel_fish), names(Wetzel_crustaceans)) #The columns are NOT identical as fish
str(Wetzel_crustaceans)

Wetzel_crustaceans_processed <- data.frame(
  Data_source = rep("Wetzel", nrow(Wetzel_crustaceans)),
  Study_name = NA,                         #Is there actually no study info?
  Source_siteID = Wetzel_crustaceans$Site,
  Source_sampleID = Wetzel_crustaceans$Sample,
  OSRI_siteID = NA,
  OSRI_sampleID = NA,
  Sample_motivation = NA,
  General_location = Wetzel_crustaceans$Site,
  Specific_location = NA,   
  Lat = NA,
  Long = NA,
  Year = Wetzel_crustaceans$Year,                                
  Month = NA,                               
  Collection_date =	NA,
  DOY = NA,                                
  Collection_time = NA,
  Collection_method = NA, 
  Species_complex = rep("crustaceans", nrow(Wetzel_crustaceans)),
  Common_name = Wetzel_crustaceans$Species,         
  Scientific_name = NA,    #Need to generate this
  Genus_latin = NA,                        #Need to calculate these below
  Species_latin = NA,                        #Need to calculate these below
  Tissue_type = Wetzel_crustaceans$Matrix,
  Sample_composition = NA,                   #Need to calculate this from the column below
  Number_in_composite = NA,
  Sex = NA,                                  #No data in this dataset
  Analysis_method = NA, #ORR this might be the analysis column
  Chem_code = NA,                           #might be able to get this from Analysis_method??
  Parameter = NA,
  Value = Wetzel_crustaceans$"SPAH ug/g",
  Units = rep("ug/g", nrow(Wetzel_crustaceans)),
  Value_standardized = NA,
  Units_standardized = NA,
  Detection_limit = NA,
  Reporting_limit = NA,
  Basis = NA,
  Lab_replicate = NA,
  Qualifier_code = ifelse(Wetzel_crustaceans$"SPAH ug/g" == "BDL", "BDL", NA),
  Lipid_pct = NA,
  Moisture_pct = NA,
  Total_PAHs = NA,
  Total_LMWAHs = NA,
  Total_HMWAHs = NA,
  Lab_ID = NA,
  Notes = NA
)



unique(Wetzel_crustaceans_processed$Common_name)



Wetzel_crustaceans_processed <- Wetzel_crustaceans_processed %>%
  mutate(Scientific_name = "Crangon septemspinosa",
         Genus_latin = "septemspinosa",
         Species_latin = "septemspinosa")
           
           
          

str(Wetzel_crustaceans_processed)


### Wetzel pinnipeds data ----------------------------------------------------

identical(names(Wetzel_fish), names(Wetzel_pinnipeds)) #The columns are identical as fish
str(Wetzel_pinnipeds)

Wetzel_pinnipeds_processed <- data.frame(
  Data_source = rep("Wetzel", nrow(Wetzel_pinnipeds)),
  Study_name = NA,                         #Is there actually no study info?
  Source_siteID = Wetzel_pinnipeds$Site,
  Source_sampleID = Wetzel_pinnipeds$Sample,
  OSRI_siteID = NA,
  OSRI_sampleID = NA,
  Sample_motivation = NA,
  General_location = Wetzel_pinnipeds$Site,
  Specific_location = NA,   
  Lat = NA,
  Long = NA,
  Year = Wetzel_pinnipeds$Year,                                
  Month = NA,                               
  Collection_date =	NA,
  DOY = NA,                                
  Collection_time = NA,
  Collection_method = NA, 
  Species_complex = rep("pinnipeds", nrow(Wetzel_pinnipeds)),
  Common_name = Wetzel_pinnipeds$Species,         
  Scientific_name = NA,                     #Need to generate this
  Genus_latin = NA,                        #Need to calculate these below
  Species_latin = NA,                        #Need to calculate these below
  Tissue_type = Wetzel_pinnipeds$Matrix,
  Sample_composition = NA,                   #Need to calculate this from the column below
  Number_in_composite = NA,
  Sex = NA,                                  #No data in this dataset
  Analysis_method = NA, #ORR this might be the analysis column
  Chem_code = NA,                           #might be able to get this from Analysis_method??
  Parameter = NA,
  Value = Wetzel_pinnipeds$"SPAH ug/g",
  Units = rep("ug/g", nrow(Wetzel_pinnipeds)),
  Value_standardized = NA,
  Units_standardized = NA,
  Detection_limit = NA,
  Reporting_limit = NA,
  Basis = NA,
  Lab_replicate = NA,
  Qualifier_code = ifelse(Wetzel_pinnipeds$"SPAH ug/g" == "BDL", "BDL", NA),
  Lipid_pct = NA,
  Moisture_pct = NA,
  Total_PAHs = NA,
  Total_LMWAHs = NA,
  Total_HMWAHs = NA,
  Lab_ID = NA,
  Notes = NA
)



unique(Wetzel_pinnipeds_processed$Common_name)



# Create a lookup table
pinniped_lookup <- tibble(
  Common_name = c(
    "Walrus", "Unknown Seal", "Spotted Seal", "Ringed Seal", "Bearded Seal"),
  Scientific_name = c(
    "Odobenus rosmarus", "Phocidae spp.", "Phoca largha", "Pusa hispida", "Erignathus barbatus")
) %>%
  mutate(
    Genus_latin = sub(" .*", "", Scientific_name),
    Species_latin = sub(".* ", "", Scientific_name),
    Species_latin = ifelse(Species_latin == "spp.", NA, Species_latin)
  )



# Join to your main data and replace NA in the matching columns
Wetzel_pinnipeds_processed <- Wetzel_pinnipeds_processed %>%
  left_join(pinniped_lookup, by = "Common_name") #%>%

Wetzel_pinnipeds_processed <- Wetzel_pinnipeds_processed %>%
  mutate(
    Scientific_name.x = Scientific_name.y,
    Genus_latin.x = Genus_latin.y,
    Species_latin.x = Species_latin.y
  ) %>%
  select(-Scientific_name.y, -Genus_latin.y, -Species_latin.y) %>% #
  rename(Scientific_name = Scientific_name.x,
         Genus_latin = Genus_latin.x,
         Species_latin = Species_latin.x)




str(Wetzel_pinnipeds_processed)





### Wetzel whale data ----------------------------------------------------

identical(names(Wetzel_fish), names(Wetzel_whale)) #The columns are NOT identical as fish
str(Wetzel_whale)

Wetzel_whale_processed <- data.frame(
  Data_source = rep("Wetzel", nrow(Wetzel_whale)),
  Study_name = NA,                         #Is there actually no study info?
  Source_siteID = Wetzel_whale$Site,
  Source_sampleID = Wetzel_whale$Sample,
  OSRI_siteID = NA,
  OSRI_sampleID = NA,
  Sample_motivation = NA,
  General_location = Wetzel_whale$Site,
  Specific_location = NA,   
  Lat = NA,
  Long = NA,
  Year = Wetzel_whale$Year,                                
  Month = NA,                               
  Collection_date =	NA,
  DOY = NA,                                
  Collection_time = NA,
  Collection_method = NA, 
  Species_complex = rep("whale", nrow(Wetzel_whale)),
  Common_name = Wetzel_whale$Species,         
  Scientific_name = NA,                     #Need to generate this
  Genus_latin = NA,                        #Need to calculate these below
  Species_latin = NA,                        #Need to calculate these below
  Tissue_type = Wetzel_whale$Matrix,
  Sample_composition = NA,                   #Need to calculate this from the column below
  Number_in_composite = NA,
  Sex = NA,                                  #No data in this dataset
  Analysis_method = NA, #ORR this might be the analysis column
  Chem_code = NA,                           #might be able to get this from Analysis_method??
  Parameter = NA,
  Value = Wetzel_whale$"SPAH ug/g",
  Units = rep("ug/g", nrow(Wetzel_whale)),
  Value_standardized = NA,
  Units_standardized = NA,
  Detection_limit = NA,
  Reporting_limit = NA,
  Basis = NA,
  Lab_replicate = NA,
  Qualifier_code = ifelse(Wetzel_whale$"SPAH ug/g" == "BDL", "BDL", NA),
  Moisture_pct = NA,
  Lipid_pct = NA,
  Total_PAHs = NA,
  Total_LMWAHs = NA,
  Total_HMWAHs = NA,
  Lab_ID = NA,
  Notes = Wetzel_whale$MML
)



unique(Wetzel_whale_processed$Common_name)



# Create a lookup table
whale_lookup <- tibble(
  Common_name = c(
    "Beluga", "Bowhead", "Grey"),
  Scientific_name = c(
    "Delphinapterus leucas", "Balaena mysticetus", "Eschrichtius robustus")
) %>%
  mutate(
    Genus_latin = sub(" .*", "", Scientific_name),
    Species_latin = sub(".* ", "", Scientific_name)
  )



# Join to your main data and replace NA in the matching columns
Wetzel_whale_processed <- Wetzel_whale_processed %>%
  left_join(whale_lookup, by = "Common_name") #%>%

Wetzel_whale_processed <- Wetzel_whale_processed %>%
  mutate(
    Scientific_name.x = Scientific_name.y,
    Genus_latin.x = Genus_latin.y,
    Species_latin.x = Species_latin.y
  ) %>%
  select(-Scientific_name.y, -Genus_latin.y, -Species_latin.y) %>% #
  rename(Scientific_name = Scientific_name.x,
         Genus_latin = Genus_latin.x,
         Species_latin = Species_latin.x)




str(Wetzel_whale_processed)





### Combine the Wetzel data ----------------------------------------------


Wetzel_data_processed <- rbind(Wetzel_fish_processed, Wetzel_crustaceans_processed, Wetzel_pinnipeds_processed,
                              Wetzel_whale_processed) 

#View(Wetzel_data_processed) #887 data points




#Add lat long
unique(Wetzel_data_processed$General_location)


#I cant find any other infomation for these other sites. 
wetzel_coords <- tibble::tribble(
  ~General_location,           ~Lat,       ~Long,
  "Nigliq Channel, AK",         70.4333,   -150.4333,
  "Meade River JN Site",        70.4958,   -157.393,
  "Meade River FN Site",        70.4958,   -157.393,
  "Nuiqsut, AK",                70.2163,   -151.0057,
  "Puvisuk",                    70.09154,  -151.09264,
  "Uyagagvik Nigliq",           70.3300,   -150.7500,
  "Wood's Camp",                70.4333,   -150.4333,
  "CD2 loc 1",                  70.3421,   -150.9304,
  "TL003",                      70.48743,  -153.79658,
  "Ikpikpuk DL Camp",           70.8236,   -154.3025,
  "TL005",                      70.49447,  -153.85656,
  "TLSB2",                      70.44987,  -153.53802,
  "Ruth Nukapigak Site",        70.33376,  -150.77203,
  "S.Ikpikpuk",                 69.7669,   -154.661,
  "Trib 3",                     70.65972,  -154.34734,
  "Teshekpuk Lake Site 1",      70.5714,   -153.5142,
  "Joe Station",                69.0500,   -140.4500,
  "Joe Creek",                  69.0500,   -140.4500,
  "Moses Nayakik Site",         70.12184,  -159.63497,
  "DANLE",                      70.7028,   -154.52689,
  "Cook Inlet, AK",             60.3378,   -151.8750,
  "Big Su River/East Fork",     61.2706,   -150.5758,
  "Little Su River",            61.2508,   -150.2880,
  "Kuk Site",                   70.6081,   -160.1111,
  "Mouth of Eagle River",       61.3461,   -149.7317,
  "Eagle River",                61.3293,   -149.5681,
  "Little Su",                  61.2508,   -150.2880,
  "Eagle",                      61.3461,   -149.7317,
  "POA",                        61.2403,   -149.8861,
  "Ship Cr",                    61.2261,   -149.8925,
  "Ship Creek",                 61.2261,   -149.8925,
  "Tesh Offshore Camp",         70.48743,  -153.79658,
  "T602",                       70.55245,  -153.82244,
  "T600",                       70.72253,  -153.76562,
  "T611",                       70.72253,  -153.76562,
  "Ship",                       61.2261,   -149.8925,
  "Wai Inlet",                  70.6000,   -160.1300,
  "Ship Cr or N. Port Exp",     61.2261,   -149.8925,
  "Port",                       61.2403,   -149.8861,
  "Nome, AK",                   64.5039,   -165.3994,
  "Gambell, AK",                63.7761,   -171.7008,
  "Nome",                       64.5039,   -165.3994,
  "Nome, Cape Nome",            64.4370,   -165.0100,
  "Norton Sound, AK",           64.5000,   -162.5000,
  "Barrow",                     71.2906,   -156.7886,
  "Sledge Island, Nome",        64.4850,   -166.2033,
  "Barrow, AK",                 71.2906,   -156.7886,
  "Norton Bay/Norton Sound",    64.6981,   -161.4331,
  "Pt. Lay, AK",                69.7411,   -162.8656,
  "Tyonek, AK",                 61.0681,   -151.1370
)


Wetzel_data_processed <- Wetzel_data_processed %>%
  left_join(wetzel_coords, by = "General_location") %>% 
  mutate(
    Lat = Lat.y,
    Long = Long.y
  ) %>%
  select(-Lat.x, -Lat.y, -Long.x, -Long.y)




str(Wetzel_data_processed) #887 data points






# Stimmelmayr Data # --------------------------------------------------------

Stimmelmayr_data <- read_excel("Input Data/Stimmelmayr et al 2018_Seal_MinedTable4.xlsx")      

#Need to read the dates in properly
Stimmelmayr_data <- Stimmelmayr_data %>%
  mutate(
    `Collection date` = case_when(
      str_detect(`Collection date`, "^[0-9]+$") ~ as.character(as.Date(as.numeric(`Collection date`), origin = "1899-12-30")),
      TRUE ~ `Collection date`
    )
  )

Stimmelmayr_data <- Stimmelmayr_data[-c(31:33),] #remove useless rows


str(Stimmelmayr_data)


#tibble [33 x 20] (S3: tbl_df/tbl/data.frame)
#$ Tissue                   : chr [1:33] "Blubber" "Blubber" "Blubber" "Blubber" ...
#$ Species                  : chr [1:33] "Spotted seal" "Spotted seal" "Ringed seal" "Ringed seal" ...
#$ Field identication number: chr [1:33] "N52-2012" "2012-166" "N55-2012" "N55-2012" ...
#$ Collection site          : chr [1:33] "Shishmaref" "Gambell" "Gambell" "Gambell" ...
#$ Collection date          : chr [1:33] "2012-09-05" "2012-10-16" "2012-11-12" "2012-11-12" ...
#$ Collection Method        : chr [1:33] "Subsistence harvest" "Subsistence harvest" "Subsistence harvest" "Subsistence harvest" ...
#$ Sample Motivation        : chr [1:33] "oiling observed" "oiling observed" "oiling observed" "oiling observed" ...
#$ Sample n                 : num [1:33] 1 1 1 1 3 1 1 1 1 1 ...
#$ Percent lipid            : chr [1:33] "89" "85" "94" "92" ...
#$ LOQ                      : chr [1:33] "6.4 ± 7.8" "6.4 ± 7.8" "6.4 ± 7.8" "6.4 ± 7.8" ...
#$ SUM LMWAHs               : chr [1:33] "35" "48" "12" "25" ...
#$ SUMHMWAHS                : chr [1:33] "0.6" "0.4" "< LOQ" "< LOQ" ...
#$ SumPAHs                  : chr [1:33] "36" "48" "12" "25" ...
#$ Unit                     : chr [1:33] "ng/g" "ng/g" "ng/g" "ng/g" ...
#$ Basis                    : chr [1:33] "wet weight" "wet weight" "wet weight" "wet weight" ...
#$ Comments                 : chr [1:33] NA NA "Sample collected from a non-visibly oiled area of seal carcass." "Sample collected from a visibly oiled area of seal carcass." ...
#$ Analysis method          : chr [1:33] "GC/MS" "GC/MS" "GC/MS" "GC/MS" ...
#$ QA/QC measures           : chr [1:33] "met" "met" "met" "met" ...
#$ Lab                      : chr [1:33] "National Marine Fisheries\r\nServices's Northwest Fisheries Science Center in Seattle, Washington" "National Marine Fisheries\r\nServices's Northwest Fisheries Science Center in Seattle, Washington" "National Marine Fisheries\r\nServices's Northwest Fisheries Science Center in Seattle, Washington" "National Marine Fisheries\r\nServices's Northwest Fisheries Science Center in Seattle, Washington" ...
#$ Source                   : chr [1:33] "Stimmelmayr et al 2018" "Stimmelmayr et al 2018" "Stimmelmayr et al 2018" "Stimmelmayr et al 2018" ...


Stimmelmayr_data_processed <- data.frame(
  Data_source = rep("Stimmelmayr", nrow(Stimmelmayr_data)),
  Study_name = Stimmelmayr_data$Source,                         
  Source_siteID = Stimmelmayr_data$"Collection site",
  Source_sampleID = Stimmelmayr_data$"Field identication number",
  OSRI_siteID = NA,
  OSRI_sampleID = NA,
  Sample_motivation = Stimmelmayr_data$"Sample Motivation",
  General_location = Stimmelmayr_data$"Collection site",
  Specific_location = NA,   
  Lat = NA,
  Long = NA,
  Year = NA,                                #Need to calculate
  Month = NA,                               #Need to calculate
  Collection_date =	Stimmelmayr_data$"Collection date",          #THIS MIGHT BE IN A FAILED FORMAT FROM EXCEL
  DOY = NA,                                 #Need to calculate
  Collection_time = NA,
  Collection_method = Stimmelmayr_data$"Collection Method", 
  Species_complex = rep("pinnipeds", nrow(Stimmelmayr_data)),
  Common_name = Stimmelmayr_data$Species,         
  Scientific_name = NA,                     #Need to generate this
  Genus_latin = NA,                         #Need to calculate these below
  Species_latin = NA,                        #Need to calculate these below
  Tissue_type = Stimmelmayr_data$Tissue,
  Sample_composition = NA,                   #Need to calculate this from the column below
  Number_in_composite = Stimmelmayr_data$"Sample n",
  Sex = NA,                                  #No data in this dataset
  Analysis_method = Stimmelmayr_data$"Analysis method", 
  Chem_code = NA,                           
  Parameter = NA,
  Value = Stimmelmayr_data$SumPAHs,
  Units = Stimmelmayr_data$Unit,
  Value_standardized = NA,
  Units_standardized = NA,
  Detection_limit = Stimmelmayr_data$LOQ,
  Reporting_limit = NA,
  Basis = Stimmelmayr_data$Basis,
  Lab_replicate = NA,
  Qualifier_code = Stimmelmayr_data$"QA/QC measures",
  Lipid_pct = Stimmelmayr_data$"Percent lipid",
  Moisture_pct = NA,
  Total_PAHs = Stimmelmayr_data$SumPAHs,
  Total_LMWAHs = Stimmelmayr_data$"SUM LMWAHs",
  Total_HMWAHs = Stimmelmayr_data$SUMHMWAHS,
  Lab_ID = Stimmelmayr_data$Lab,
  Notes = Stimmelmayr_data$Comments
)



unique(Stimmelmayr_data_processed$Common_name)

unique(Stimmelmayr_data_processed$General_location)




#Add scientific Names
# Create a lookup table
Stimmelmayr_lookup <- tibble(
  Common_name = c(
    "Spotted seal", "Ringed seal", "Unoiled ringed seals"),
  Scientific_name = c(
    "Phoca largha", "Pusa hispida", "Pusa hispida")
) %>%
  mutate(
    Genus_latin = sub(" .*", "", Scientific_name),
    Species_latin = sub(".* ", "", Scientific_name),
    Species_latin = ifelse(Species_latin == "spp.", NA, Species_latin)
  )

# Join to your main data and replace NA in the matching columns
Stimmelmayr_data_processed <- Stimmelmayr_data_processed %>%
  left_join(Stimmelmayr_lookup, by = "Common_name") #%>%

Stimmelmayr_data_processed <- Stimmelmayr_data_processed %>%
  mutate(
    Scientific_name.x = Scientific_name.y,
    Genus_latin.x = Genus_latin.y,
    Species_latin.x = Species_latin.y
  ) %>%
  select(-Scientific_name.y, -Genus_latin.y, -Species_latin.y) %>% #
  rename(Scientific_name = Scientific_name.x,
         Genus_latin = Genus_latin.x,
         Species_latin = Species_latin.x)




Stimmelmayr_data_processed <- Stimmelmayr_data_processed %>%
  mutate(
    Year = case_when(
      # Year range (e.g., "2012–2013" or "2012-2014")
      str_detect(Collection_date, "^\\d{4}\\s*[–-]\\s*\\d{4}$") ~ str_replace_all(Collection_date, "\\s*", ""),
      
      # Standard date format (YYYY-MM-DD)
      str_detect(Collection_date, "^\\d{4}-\\d{2}-\\d{2}$") ~ str_sub(Collection_date, 1, 4),
      
      TRUE ~ NA_character_
    ),

    Lat = case_when(
      str_detect(General_location, "Shishmaref") ~ 66.25674,
      str_detect(General_location, "Gambell") ~ 63.77836,
      str_detect(General_location, "Utqiagvik|Utqiaħvik") ~ 71.29088
    ),
    
    # Same for Long
    Long = case_when(
      str_detect(General_location, "Shishmaref") ~ -166.06247,
      str_detect(General_location, "Gambell") ~ -171.72921,
      str_detect(General_location, "Utqiagvik|Utqiaħvik") ~ -156.78864
    )
    ) %>%
  mutate(
    # Only convert to Date where it's in standard format
    Collection_date = suppressWarnings(as.Date(Collection_date, format = "%Y-%m-%d")),
    Month = month(Collection_date),
    DOY = yday(Collection_date),
    
    Sample_composition = ifelse(Number_in_composite < 1, "composite", "individual"),
    Value = ifelse(Value == "< LOQ", "BLD", Value)
  )


str(Stimmelmayr_data_processed)





# Arnold Data -------------------------------------------------------------


Arnold_data <- read_excel("Input Data/Arnold 2006_SelendangAYU_AKDeptHealth_MinedTable4.xlsx")      

#calculate sample composites
Arnold_data <- Arnold_data %>% 
  mutate("Number_in_composite" = ifelse("Sample Composition" == "individual", 1, "10-20"),
         "Sample_composition" = ifelse("Sample Composition" == "individual", "individual", "composite"))


str(Arnold_data)
#tibble [67 x 15] (S3: tbl_df/tbl/data.frame)
#$ Collection Date   : POSIXct[1:67], format: "2005-06-25" "2005-06-23" "2005-06-23" "2005-06-23" ...
#$ Species           : chr [1:67] "black chitons" "black chitons" "black chitons" "black chitons" ...
#$ Location ID       : chr [1:67] "Captains Bay" "Humpy Cove 1" "Humpy Cove 1" "Humpy Cove 1" ...
#$ Sample ID         : chr [1:67] "CH-CBW01-06-25-05-01" "CH-SMB7-062305-01-rep" "CH-SMB7-062305-01-rep" "CH-SMB7-062305-01-rep" ...
#$ Replicate         : num [1:67] 0 1 2 3 0 0 0 0 0 0 ...
#$ Sample Motivation : chr [1:67] "Post Seledang AYU spill assessment" "Post Seledang AYU spill assessment" "Post Seledang AYU spill assessment" "Post Seledang AYU spill assessment" ...
#$ Sample Composition: chr [1:67] "10-20 individuals pooled" "10-20 individuals pooled" "10-20 individuals pooled" "10-20 individuals pooled" ...
#$ Total PAHs        : num [1:67] 36 836 791 1882 13.8 ...
#$ Unit              : chr [1:67] "µg/kg" "µg/kg" "µg/kg" "µg/kg" ...
#$ Basis             : logi [1:67] NA NA NA NA NA NA ...
#$ LOQ               : logi [1:67] NA NA NA NA NA NA ...
#$ Lab               : chr [1:67] "Woods Hole Group Analytical Laboratory in Raynham, Massachusetts" "Woods Hole Group Analytical Laboratory in Raynham, Massachusetts" "Woods Hole Group Analytical Laboratory in Raynham, Massachusetts" "Woods Hole Group Analytical Laboratory in Raynham, Massachusetts" ...
#$ Analysis Method   : chr [1:67] "USEPA8270c" "USEPA8270c" "USEPA8270c" "USEPA8270c" ...
#$ Comments          : chr [1:67] "Samples from Unalaska Bay" "Samples from Unalaska Bay" "Samples from Unalaska Bay" "Samples from Unalaska Bay" ...
#$ Source            : chr [1:67] "Arnold 2006- Public Health Evaluation of Subsistence\r\nResources Collected During 2005\r\n" "Arnold 2006- Public Health Evaluation of Subsistence\r\nResources Collected During 2005\r\n" "Arnold 2006- Public Health Evaluation of Subsistence\r\nResources Collected During 2005\r\n" "Arnold 2006- Public Health Evaluation of Subsistence\r\nResources Collected During 2005\r\n" ...
#$ Number_in_composite: chr [1:67] "10-20" "10-20" "10-20" "10-20" ...
#$ Sample_composition : chr [1:67] "composite" "composite" "composite" "composite" ...


Arnold_data_processed <- data.frame(
  Data_source = rep("Arnold", nrow(Arnold_data)),
  Study_name = Arnold_data$Source,                         
  Source_siteID = Arnold_data$"Location ID",
  Source_sampleID = Arnold_data$"Sample ID",
  OSRI_siteID = NA,
  OSRI_sampleID = NA,
  Sample_motivation = Arnold_data$"Sample Motivation",
  General_location = Arnold_data$"Location ID",
  Specific_location = NA,   
  Lat = NA,
  Long = NA,
  Year = NA,                                #Need to calculate
  Month = NA,                               #Need to calculate
  Collection_date =	Arnold_data$"Collection Date",          
  DOY = NA,                                 #Need to calculate
  Collection_time = NA,
  Collection_method = NA, 
  Species_complex = NA,                     #Need to calculate
  Common_name = Arnold_data$Species,         
  Scientific_name = NA,                     #Need to generate this
  Genus_latin = NA,                         #Need to calculate these below
  Species_latin = NA,                        #Need to calculate these below
  Tissue_type = NA,                          #can calculate this with a few samples
  Sample_composition = Arnold_data$Sample_composition,                   #Calculated above
  Number_in_composite = Arnold_data$Number_in_composite,
  Sex = NA,                                  #No data in this dataset
  Analysis_method = Arnold_data$"Analysis Method", 
  Chem_code = NA,                           
  Parameter = NA,
  Value = Arnold_data$"Total PAHs",
  Units = Arnold_data$Unit,
  Value_standardized = NA,
  Units_standardized = NA,
  Detection_limit = Arnold_data$LOQ,
  Reporting_limit = NA,
  Basis = Arnold_data$Basis,
  Lab_replicate = Arnold_data$Replicate,
  Qualifier_code = NA,
  Lipid_pct = NA,
  Moisture_pct = NA,
  Total_PAHs = Arnold_data$"Total PAHs",
  Total_LMWAHs = NA,
  Total_HMWAHs = NA,
  Lab_ID = Arnold_data$Lab,
  Notes = Arnold_data$Comments
)



unique(Arnold_data_processed$Common_name)


unique(Arnold_data_processed$General_location)


#Add lat long
Arnold_coords <- tibble::tribble(
  ~General_location,        ~Lat,        ~Long,
  "Captains Bay",     53.8602778, -166.5780556,
  "Humpy Cove 1",     53.919583,  -166.434158,
  "Humpy Cove 2",     53.919583,  -166.434158,
  "Iliuliuk Bay 1",   53.8927778, -166.5102778,
  "Iliuliuk Bay 2",   53.8927778, -166.5102778,
  "Morris Cove",      53.9138889, -166.4347222,
  "Summer Bay 1",     53.9119444, -166.4547222,
  "Summer Bay 2",     53.9119444, -166.4547222,
  "Wide Bay",         53.9494444, -166.6158333,
  "Humpy Cove 3",     53.919583,  -166.434158,
  "Anderson Bay 2",   53.6838889, -166.8469444,
  "Kismaliuk Bay 2",  53.4566667, -167.3066667,
  "Kismaliuk Bay 3",  53.4566667, -167.3066667,
  "Skan Bay S. 1",    53.6100,    -167.0450,
  "Skan Bay S. 3",    53.6100,    -167.0450,
  "Anderson Bay 1",   53.6838889, -166.8469444,
  "Cannery Bay",      53.7002778, -166.7852778,
  "Kashega Bay",      53.4667,    -167.1667,
  "Kismaliuk Bay 1",  53.4566667, -167.3066667,
  "Makushin Bay",     53.7358333, -166.9611111,
  "Skan Bay N.",      53.6458333, -167.0561111,
  "Skan Bay N. 2",    53.6458333, -167.0561111,
  "Skan Bay S.",      53.6100,    -167.0450,
  "Skan Bay S. 4",    53.6100,    -167.0450,
  "Skan Bay S. 2",    53.6100,    -167.0450,
  "Naginak Cove",     53.6511111, -166.8497222,
  "Skan Bay N. 1",    53.6458333, -167.0561111
)


Arnold_data_processed <- Arnold_data_processed %>%
  select(-any_of(c("Lat", "Long"))) %>%  # remove existing if they exist
  left_join(Arnold_coords, by = "General_location")




Arnold_locations <- Arnold_data_processed %>%
  distinct(General_location, Specific_location, Lat, Long)
Arnold_locations




#Add scientific Names
# Create a lookup table
Arnold_lookup <- tibble(
  Common_name = c(
    "black chitons", "blue mussels", "green sea urchin roe", 
    "harbor seal blubber", "pink salmon","Pacific cod"),
  Scientific_name = c(
    "Katharina tunicata", "Mytilus edulis", "Strongylocentrotus droebachiensis", 
    "Phoca vitulina", "Oncorhynchus gorbuscha", "Gadus macrocephalus")
) %>%
  mutate(
    Genus_latin = sub(" .*", "", Scientific_name),
    Species_latin = sub(".* ", "", Scientific_name),
    Species_latin = ifelse(Species_latin == "spp.", NA, Species_latin)
  )

# Join to your main data and replace NA in the matching columns
Arnold_data_processed <- Arnold_data_processed %>%
  left_join(Arnold_lookup, by = "Common_name") #%>%

Arnold_data_processed <- Arnold_data_processed %>%
  mutate(
    Scientific_name.x = Scientific_name.y,
    Genus_latin.x = Genus_latin.y,
    Species_latin.x = Species_latin.y
  ) %>%
  select(-Scientific_name.y, -Genus_latin.y, -Species_latin.y) %>% #
  rename(Scientific_name = Scientific_name.x,
         Genus_latin = Genus_latin.x,
         Species_latin = Species_latin.x)




Arnold_data_processed <- Arnold_data_processed %>%
  mutate(
    Collection_date = suppressWarnings(as.Date(Collection_date, format = "%Y-%m-%d")),
    Year = year(Collection_date),
    Month = month(Collection_date),
    DOY = yday(Collection_date),
    
    Tissue_type = ifelse(Common_name == "green sea urchin roe", "roe", Tissue_type), #Get the tissue types
    Tissue_type = ifelse(Common_name == "harbor seal blubber", "blubber", Tissue_type),
    Common_name = ifelse(Common_name == "green sea urchin roe", "green sea urchin", Common_name), #and them remove them from the species names
    Common_name = ifelse(Common_name == "harbor seal blubber", "harbor seal", Common_name),
    
    Species_complex = ifelse(Common_name == "black chitons", "tunicate", Species_complex),
    Species_complex = ifelse(Common_name == "blue mussels", "mussel", Species_complex),
    Species_complex = ifelse(Common_name == "green sea urchin", "urchin", Species_complex),
    Species_complex = ifelse(Common_name == "harbor seal", "pinnipeds", Species_complex),
    Species_complex = ifelse(Common_name == "pink salmon", "fish", Species_complex),
    Species_complex = ifelse(Common_name == "Pacific cod", "fish", Species_complex),
    
  )


str(Arnold_data_processed)







# LTEMP data # ------------------------------------------------------------
LTEMP_data <- read.csv("Input Data/LTEMPDataForOSRI.csv")      

str(LTEMP_data)

#'data.frame':	109209 obs. of  23 variables:
#  $ X            : int  1 2 3 4 5 6 7 8 9 10 ...
#$ DOMAIN       : chr  "LTEMP2002" "LTEMP2002" "LTEMP2002" "LTEMP2002" ...
#$ Coll_date    : chr  "2002-07-13" "2002-07-13" "2002-07-13" "2002-07-13" ...
#$ MATRIX_Lab   : chr  "Tissue" "Tissue" "Tissue" "Tissue" ...
#$ LAB          : chr  "ABL" "ABL" "ABL" "ABL" ...
#$ SAMPID       : chr  "AIB-B-02-2-1" "AIB-B-02-2-1" "AIB-B-02-2-1" "AIB-B-02-2-1" ...
#$ Location     : chr  "AIB" "AIB" "AIB" "AIB" ...
#$ Replicate    : int  1 1 1 1 1 1 1 1 1 1 ...
#$ PCT_MOIST    : num  NA NA NA NA NA NA NA NA NA NA ...
#$ ANALYTE      : chr  "Wet Weight" "Dry weight" "d26-Dodecane" "d34-Hexadecane" ...
#$ ANALMETH     : chr  NA NA NA NA ...
#$ RESULT_Lab   : num  6.06 0.59 58.09 68.23 79.28 ...
#$ LAB_FLAG     : chr  NA NA NA NA ...
#$ Detection    : chr  NA NA NA NA ...
#$ RESULT_UNITS : chr  "g" "g" "%" "%" ...
#$ UnitBasis    : chr  "wet" "DRY" "DRY" "DRY" ...
#$ RDL          : num  NA NA NA NA NA NA NA NA NA NA ...
#$ MDL          : num  NA NA NA NA NA NA NA NA NA NA ...
#$ YEAR         : int  2002 2002 2002 2002 2002 2002 2002 2002 2002 2002 ...
#$ Month        : int  7 7 7 7 7 7 7 7 7 7 ...
#$ Location_Full: chr  "Aialik Bay" "Aialik Bay" "Aialik Bay" "Aialik Bay" ...
#$ Lat_DD       : chr  "59.87917" "59.87917" "59.87917" "59.87917" ...
#$ Long_DD      : chr  "-149.6569" "-149.6569" "-149.6569" "-149.6569" ...





LTEMP_data_processed <- data.frame(
  Data_source = rep("LTEMP", nrow(LTEMP_data)),
  Study_name = LTEMP_data$DOMAIN,                         
  Source_siteID = LTEMP_data$Location_Full,
  Source_sampleID = LTEMP_data$SAMPID,
  OSRI_siteID = NA,
  OSRI_sampleID = NA,
  Sample_motivation = NA,
  General_location = LTEMP_data$Location,
  Specific_location = LTEMP_data$Location_Full,   
  Lat = LTEMP_data$Lat_DD,
  Long = LTEMP_data$Long_DD,
  Year = LTEMP_data$YEAR,                                
  Month = LTEMP_data$Month,                               
  Collection_date =	LTEMP_data$Coll_date,          
  DOY = NA,                                 #Need to calculate
  Collection_time = NA,
  Collection_method = NA, 
  Species_complex = NA,                      
  Common_name = NA,         
  Scientific_name = NA,                     #No info available
  Genus_latin = NA,                         #No info available
  Species_latin = NA,                        #No info available
  Tissue_type = LTEMP_data$MATRIX_Lab,                          
  Sample_composition = NA,                   
  Number_in_composite = NA,
  Sex = NA,                                 
  Analysis_method = LTEMP_data$ANALYTE, 
  Chem_code = NA,                           
  Parameter = LTEMP_data$ANALYTE,
  Value = LTEMP_data$RESULT_Lab,
  Units = LTEMP_data$RESULT_UNITS,
  Value_standardized = NA,
  Units_standardized = NA,
  Detection_limit = LTEMP_data$MDL,
  Reporting_limit = LTEMP_data$RDL,
  Basis = LTEMP_data$UnitBasis,
  Lab_replicate = LTEMP_data$Replicate,
  Qualifier_code = LTEMP_data$LAB_FLAG,
  Lipid_pct = NA,
  Moisture_pct = LTEMP_data$PCT_MOIST,
  Total_PAHs = NA,                                  #might be able to calculate this later
  Total_LMWAHs = NA,
  Total_HMWAHs = NA,
  Lab_ID = LTEMP_data$LAB,
  Notes = LTEMP_data$UnitBasis
)




LTEMP_data_processed <- tibble::tribble(
  ~General_location,                                 ~Lat,       ~Long,
  "GAL",                                             61.1292,   -146.3556,
  "SMB",                                             61.1292,   -146.3556,
  "TLP Alyeska Terminal Loading Platform",           61.1200,   -146.3500,
  "ZAB2",                                            61.1292,   -146.3556,
  NA,                                                61.1292,   -146.3556,
  )


#Hmmm this data is formatted weird:
LTEMP_data_processed %>%
  filter(tolower(trimws(Units)) %in% c("%", "percent")) %>%
  select(Data_source, Parameter, Value, Units)





LTEMP_data_processed <- LTEMP_data_processed %>%
  mutate(
    Collection_date = as.Date(Collection_date, format = "%Y-%m-%d"),
 
    DOY = yday(Collection_date),
    
    Common_name = "Blue mussels",
    Scientific_name = "Mytilus trossulus",
    Genus_latin = "mytilus",
    Species_latin = "trossulus",
    Species_complex = "mussels"
  )


str(LTEMP_data_processed)





# Ma data # ---------------------------------------------------------------

Ma_data <- read_excel("Input Data/MA 2020 MinedDataTable.xlsx", sheet = "Data")      
#We will need to estimate the lat long from the figure in the "Station map" tab..... 

str(Ma_data)

#tibble [228 x 12] (S3: tbl_df/tbl/data.frame)
#$ Sample ID   : chr [1:228] "Whelk" "Crab" "Starfish" "Fish-adult" ...
#$ Station ID  : chr [1:228] "NB02" "NB02" "NB02" "CDOI" ...
#$ Region      : chr [1:228] "Northern Bering Sea" "Northern Bering Sea" "Northern Bering Sea" "Northern Bering Sea" ...
#$ Species Name: chr [1:228] "Neptunea heros" "Hyas sp." "Ctenodiscus crispatus" "Boreogadus saida" ...
#$ Analyte     : chr [1:228] "naphthalene" "naphthalene" "naphthalene" "naphthalene" ...
#$ Result      : chr [1:228] "1.76" "10.81" "8.07" "8.0299999999999994" ...
#$ Unite       : chr [1:228] "ng/g" "ng/g" "ng/g" "ng/g" ...
#$ Basis       : chr [1:228] "Dry" "Dry" "Dry" "Dry" ...
#$ MDL         : num [1:228] 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 ...
#$ Study       : chr [1:228] "CHINARE 2014" "CHINARE 2014" "CHINARE 2014" "CHINARE 2014" ...
#$ LAT         : chr [1:228] "Mine from Station Map on next Sheet" NA NA NA ...
#$ LONG        : logi [1:228] NA NA NA NA NA NA ...




Ma_data_processed <- data.frame(
  Data_source = rep("Ma", nrow(Ma_data)),
  Study_name = Ma_data$Study,                         
  Source_siteID = Ma_data$'Station ID',
  Source_sampleID = Ma_data$'Sample ID',
  OSRI_siteID = NA,
  OSRI_sampleID = NA,
  Sample_motivation = NA,
  General_location = Ma_data$Region,
  Specific_location = Ma_data$'Station ID',   
  Lat = NA,                                #Need to calculate from figure
  Long = NA,                               #Need to calculate from figure
  Year = NA,                                
  Month = NA,                               
  Collection_date =	NA,          
  DOY = NA,                                 
  Collection_time = NA,
  Collection_method = NA, 
  Species_complex = Ma_data$'Sample ID',                    #Need to calculate  
  Common_name = NA,                        #Need to calculate
  Scientific_name = Ma_data$"Species Name",                     
  Genus_latin = NA,                         #No info available
  Species_latin = NA,                        #No info available
  Tissue_type = NA,                          
  Sample_composition = NA,                   
  Number_in_composite = NA,
  Sex = NA,                                 
  Analysis_method = Ma_data$Analyte, 
  Chem_code = NA,                           
  Parameter = NA,
  Value = Ma_data$Result,
  Units = Ma_data$Unite,
  Value_standardized = NA,
  Units_standardized = NA,
  Detection_limit = Ma_data$MDL,
  Reporting_limit = NA,
  Basis = Ma_data$Basis,
  Lab_replicate = NA,
  Qualifier_code = NA,
  Lipid_pct = NA,
  Moisture_pct = NA,
  Total_PAHs = NA,                                  #might be able to calculate this later
  Total_LMWAHs = NA,
  Total_HMWAHs = NA,
  Lab_ID = NA,
  Notes = NA
)



unique(Ma_data_processed$Scientific_name)

# Create a lookup table (corrected: scientific names and common names swapped)
Ma_lookup <- tibble(
  Scientific_name = c(
    "Neptunea heros", "Hyas sp.", "Ctenodiscus crispatus",
    "Boreogadus saida", "Delectopecten randolphi", "Lepidepecreum sp."),
  Common_name = c(
    "Heros neptune", "Crab", "Cookie-cutter Sea Star", 
    "Arctic cod", "Randolph's Scallop", "Crustacean")
) %>%
  mutate(
    Genus_latin = sub(" .*", "", Scientific_name),
    Species_latin = sub(".* ", "", Scientific_name),
    Species_latin = ifelse(Species_latin == "sp." | Species_latin == "spp.", NA, Species_latin)
  )

# Join on Scientific_name and update missing fields
Ma_data_processed <- Ma_data_processed %>%
  left_join(Ma_lookup, by = "Scientific_name") %>%
  mutate(
    Common_name = coalesce(Common_name.x, Common_name.y),
    Genus_latin = coalesce(Genus_latin.x, Genus_latin.y),
    Species_latin = coalesce(Species_latin.x, Species_latin.y)
  ) %>%
  select(-Common_name.x, -Common_name.y,
         -Genus_latin.x, -Genus_latin.y,
         -Species_latin.x, -Species_latin.y)





unique(Ma_data_processed$Source_siteID)

Ma_lookup_lat <- tibble(
  Source_siteID = c(
    "NB02", "CDOI", "AF7",  "cc07", "COI",  "R07"),
  Lat = c(
    "61.000", "62.500", "64.000", "68.200", "69.200", "74.000")
)


Ma_lookup_long <- tibble(
  Source_siteID = c(
    "NB02", "CDOI", "AF7",  "cc07", "COI",  "R07"),
  Long = c(
    "-175.000", "-174.000", "-171.000", "-168.000", "-169.000", "-169.500")
)

Ma_data_processed <- Ma_data_processed %>%
  left_join(Ma_lookup_lat, by = "Source_siteID") %>%
  left_join(Ma_lookup_long, by = "Source_siteID") #%>%
  

Ma_data_processed <- Ma_data_processed %>%
  left_join(Ma_lookup_lat, by = "Source_siteID") %>%
  left_join(Ma_lookup_long, by = "Source_siteID") %>%
  mutate(
    Lat = as.numeric(Lat.y),
    Long = as.numeric(Long.y)
  ) %>%
  select(-Lat.x, -Long.x, -Lat.y, -Long.y)

str(Ma_data_processed)


# Harvey data # ---------------------------------------------------------------

Harvey_data <- read_excel("Input Data/Harvey 2014 Chukchi Whelk data.xlsx")      

str(Harvey_data)
#tibble [111 x 14] (S3: tbl_df/tbl/data.frame)
#$ Species               : chr [1:111] "Neptunea heros" "Neptunea heros" "Neptunea heros" "Neptunea heros" ...
#$ Organism              : chr [1:111] "Northern Neptune Whelk" "Northern Neptune Whelk" "Northern Neptune Whelk" "Northern Neptune Whelk" ...
#$ Analyte               : chr [1:111] "2-Methylnapthalene" "1 -Methylnapthalene" "Biphenyl" "2,7-Dimethylnapthalene" ...
#$ Collection Date       : chr [1:111] "Summer 2009" "Summer 2009" "Summer 2009" "Summer 2009" ...
#$ Region                : chr [1:111] "Chukchi Sea" "Chukchi Sea" "Chukchi Sea" "Chukchi Sea" ...
#$ Tissue                : chr [1:111] "muscle" "muscle" "muscle" "muscle" ...
#$ Size class (cm length): chr [1:111] "< 5" "< 5" "< 5" "< 5" ...
#$ Result                : num [1:111] 1.43 0.68 0.52 0 0 0.72 0.65 0.49 0 0.23 ...
#$ Unit                  : chr [1:111] "ng/g" "ng/g" "ng/g" "ng/g" ...
#$ Basis                 : chr [1:111] "wet" "wet" "wet" "wet" ...
#$ Latitude              : chr [1:111] "70°28.122'" "70°28.122'" "70°28.122'" "70°28.122'" ...
#$ Longitude             : chr [1:111] "166°05.168'" "166°05.168'" "166°05.168'" "166°05.168'" ...
#$ data quality          : chr [1:111] "Blank Corrected" "Blank Corrected" "Blank Corrected" "Blank Corrected" ...
#$ Study                 : chr [1:111] "Harvey et al 2014 - COMIDA" "Harvey et al 2014 - COMIDA" "Harvey et al 2014 - COMIDA" "Harvey et al 2014 - COMIDA" ...


Harvey_data_processed <- data.frame(
  Data_source = rep("Harvey", nrow(Harvey_data)),
  Study_name = Harvey_data$Study,                         
  Source_siteID = NA,
  Source_sampleID = NA,
  OSRI_siteID = NA,
  OSRI_sampleID = NA,
  Sample_motivation = NA,
  General_location = Harvey_data$Region,
  Specific_location = NA,   
  Lat = Harvey_data$Latitude,                                
  Long = Harvey_data$Longitude,                              
  Year = NA,                                #Need to calculate
  Month = NA,                               
  Collection_date =	Harvey_data$'Collection Date',          
  DOY = NA,                                 
  Collection_time = NA,
  Collection_method = NA, 
  Species_complex = NA,                    #Need to calculate         
  Common_name = Harvey_data$Organism,                        
  Scientific_name = Harvey_data$Species,                     
  Genus_latin = NA,                         #Need to calculate
  Species_latin = NA,                        #Need to calculate
  Tissue_type = Harvey_data$Tissue,                          
  Sample_composition = NA,                   
  Number_in_composite = NA,
  Sex = NA,                                 
  Analysis_method = Harvey_data$Analyte, 
  Chem_code = NA,                           
  Parameter = NA,
  Value = Harvey_data$Result,
  Units = Harvey_data$Unit,
  Value_standardized = NA,
  Units_standardized = NA,
  Detection_limit = NA,
  Reporting_limit = NA,
  Basis = Harvey_data$Basis,
  Lab_replicate = NA,
  Qualifier_code = Harvey_data$'data quality',
  Lipid_pct = NA,
  Moisture_pct = NA,
  Total_PAHs = NA,                                  #might be able to calculate this later
  Total_LMWAHs = NA,
  Total_HMWAHs = NA,
  Lab_ID = NA,
  Notes = Harvey_data$'Size class (cm length)'
)



unique(Harvey_data_processed$Scientific_name)
unique(Harvey_data_processed$Lat)
unique(Harvey_data_processed$Long)




Harvey_data_processed <- Harvey_data_processed %>%
  mutate(
    Year = 2009,
    
    Species_complex = "Mollusk",
    
    Genus_latin = sub(" .*", "", Scientific_name),
    Species_latin = sub(".* ", "", Scientific_name),
    
    Lat = 70.4687,
    Long = -166.0861
  )






# ERM data # ---------------------------------------------------------------

ERM_data <- read_excel("Input Data/ERM NorthSlope Fish PAH Data.xlsx")      

str(ERM_data)

#tibble [2,900 x 16] (S3: tbl_df/tbl/data.frame)
# $ Sample ID          : chr [1:2900] "BDWF-2014-01" "BDWF-2014-01" "BDWF-2014-01" "BDWF-2014-01" ...
# $ Sample Date        : POSIXct[1:2900], format: "2014-07-13" "2014-07-13" "2014-07-13" "2014-07-13" ...
# $ Region             : chr [1:2900] "North Slope" "North Slope" "North Slope" "North Slope" ...
# $ Location           : chr [1:2900] "Nigliq Channel" "Nigliq Channel" "Nigliq Channel" "Nigliq Channel" ...
# $ Species            : chr [1:2900] "Board Whitefish" "Board Whitefish" "Board Whitefish" "Board Whitefish" ...
# $ Tissue             : chr [1:2900] "Wholebody" "Wholebody" "Wholebody" "Wholebody" ...
# $ Method             : chr [1:2900] "SW8270D-SIM" "SW8270D-SIM" "SW8270D-SIM" "SW8270D-SIM" ...
# $ Compound           : chr [1:2900] "2-Methylnaphthalene" "Acenaphthene" "Acenaphthylene" "Anthracene" ...
# $ Units              : chr [1:2900] "µg/kg" "µg/kg" "µg/kg" "µg/kg" ...
# $ Result             : chr [1:2900] "ND" "ND" "ND" "ND" ...
# $ MDL                : num [1:2900] 1.2 0.45 0.53 0.36 0.36 0.94 0.63 0.9 0.54 0.53 ...
# $ MRL                : num [1:2900] 4.8 4.8 4.8 4.8 4.8 4.8 4.8 4.8 4.8 4.8 ...
# $ Basis              : chr [1:2900] "Wet" "Wet" "Wet" "Wet" ...
# $ Sampling Motivation: chr [1:2900] "Subsistence harvest, biomonitoring" "Subsistence harvest, biomonitoring" "Subsistence harvest, biomonitoring" "Subsistence harvest, biomonitoring" ...
# $ Lab                : chr [1:2900] "ALS Environmental, Inc in Kelso Washington" "ALS Environmental, Inc in Kelso Washington" "ALS Environmental, Inc in Kelso Washington" "ALS Environmental, Inc in Kelso Washington" ...
# $ Study              : chr [1:2900] "ERM NorthSlope Subsistence Study 2014 2015" "ERM NorthSlope Subsistence Study 2014 2015" "ERM NorthSlope Subsistence Study 2014 2015" "ERM NorthSlope Subsistence Study 2014 2015" ...



ERM_data_processed <- data.frame(
  Data_source = rep("ERM", nrow(ERM_data)),
  Study_name = ERM_data$Study,                         
  Source_siteID = NA,
  Source_sampleID = ERM_data$'Sample ID',
  OSRI_siteID = NA,
  OSRI_sampleID = NA,
  Sample_motivation = NA,                  #Need to calculate
  General_location = ERM_data$Region,
  Specific_location = ERM_data$Location,   
  Lat = NA,                                
  Long = NA,                              
  Year = NA,                                #Need to calculate
  Month = NA,                               #Need to calculate
  Collection_date =	ERM_data$'Sample Date',          
  DOY = NA,                                 #Need to calculate
  Collection_time = NA,
  Collection_method = NA,                  #Need to calculate
  Species_complex = NA,                    #Need to calculate         
  Common_name = ERM_data$Species,          #Need to correct misspellings              
  Scientific_name = ERM_data$Species,                     
  Genus_latin = NA,                         #Need to calculate
  Species_latin = NA,                        #Need to calculate
  Tissue_type = ERM_data$Tissue,                          
  Sample_composition = NA,                   
  Number_in_composite = NA,
  Sex = NA,                                 
  Analysis_method = ERM_data$Compound, 
  Chem_code = ERM_data$Method,                           
  Parameter = NA,
  Value = ERM_data$Result,
  Units = ERM_data$Units,
  Value_standardized = NA,
  Units_standardized = NA,
  Detection_limit = ERM_data$MDL,
  Reporting_limit = ERM_data$MRL,
  Basis = ERM_data$Basis,
  Lab_replicate = NA,
  Qualifier_code = NA,
  Lipid_pct = NA,
  Moisture_pct = NA,
  Total_PAHs = NA,                                  #might be able to calculate this later
  Total_LMWAHs = NA,
  Total_HMWAHs = NA,
  Lab_ID = ERM_data$Lab,
  Notes = NA
)



unique(ERM_data_processed$Scientific_name)
unique(ERM_data$`Sampling Motivation`)

unique(ERM_data_processed$General_location)
unique(ERM_data_processed$Specific_location)



ERM_data_processed <- ERM_data_processed %>%
  mutate(
    Collection_date = as.Date(Collection_date, format = "%Y-%m-%d"),
    Year = year(Collection_date),
    Month = month(Collection_date),
    DOY = yday(Collection_date),
    
    Collection_method = "Subsistence harvest",
    Sample_motivation = "Biomonitoring",
    
    Species_complex = "fish",
    
    Common_name = ifelse(Common_name == "Board Whitefish", "Broad whitefish", Common_name), #fix misspelling
    Scientific_name = ifelse(Common_name == "Broad whitefish", "Coregonus nasus", Scientific_name),
    Scientific_name = ifelse(Common_name == "Arctic Cisco", "Coregonus autumnalis", Scientific_name),
    
    Genus_latin = sub(" .*", "", Scientific_name),
    Species_latin = sub(".* ", "", Scientific_name),
    
    Lat = 70.47815,
    Long = -150.77848
    
    )




# Selendang data ----------------------------------------------------------

Selendang_data <- read_excel("Input Data/2005 Selendang AYU study.xlsx")      

Selendang_locations <- read_excel("Input Data/2005 Selendang AYU study.xlsx", sheet = "Samples")      


str(Selendang_data)
#tibble [1,922 x 17] (S3: tbl_df/tbl/data.frame)
#$ Client ID        : chr [1:1922] "ML-SMB10-7-20-05" "ML-SMB10-7-20-05" "ML-SMB10-7-20-05" "ML-SMB10-7-20-05" ...
#$ Lab ID           : chr [1:1922] "0507100-01" "0507100-01" "0507100-01" "0507100-01" ...
#$ Matrix           : chr [1:1922] "Tissue" "Tissue" "Tissue" "Tissue" ...
#$ Reference Method : chr [1:1922] "Modified 8270C" "Modified 8270C" "Modified 8270C" "Modified 8270C" ...
#$ Batch ID         : chr [1:1922] "ST072705B05" "ST072705B05" "ST072705B05" "ST072705B05" ...
#$ Date Collected   : POSIXct[1:1922], format: "2005-07-20" "2005-07-20" "2005-07-20" "2005-07-20" ...
#$ Date Received    : POSIXct[1:1922], format: "2005-07-25" "2005-07-25" "2005-07-25" "2005-07-25" ...
#$ Date Prepped     : POSIXct[1:1922], format: "2005-07-27" "2005-07-27" "2005-07-27" "2005-07-27" ...
#$ Date Analyzed    : POSIXct[1:1922], format: "2005-08-10" "2005-08-10" "2005-08-10" "2005-08-10" ...
#$ Sample Size (wet): num [1:1922] 15.3 15.3 15.3 15.3 15.3 ...
#$ % Solid          : num [1:1922] 100 100 100 100 100 100 100 100 100 100 ...    WE DROP THIS SINCE IT'S ALL THE SAME
#$ File ID          : chr [1:1922] "P14865.D" "P14865.D" "P14865.D" "P14865.D" ...
#$ Units            : chr [1:1922] "µg/Kg" "µg/Kg" "µg/Kg" "µg/Kg" ...
#$ Analytes         : chr [1:1922] "cis/trans-Decalin" "C1-Decalins" "C2-Decalins" "C3-Decalins" ...
#$ Result           : num [1:1922] 0.62 NA NA NA NA NA NA NA NA NA ...
#$ Lab Flag         : chr [1:1922] "J" "U" "U" "U" ...
#$ Reporting Limit  : num [1:1922] 1.3 1.3 1.3 1.3 1.3 1.3 1.3 1.3 1.3 1.3 ...

#Pull the location data
# Deduplicate location data
names(Selendang_locations) <- gsub('^"|"$', '', names(Selendang_locations))

names(Selendang_locations)


Selendang_locations <- Selendang_locations %>%
  distinct(`Sample ID`, `Location ID`, LAT, LONG)





# Join into Selendang_data using Client ID
Selendang_data <- Selendang_data %>%
  left_join(
    Selendang_locations %>%
      distinct(`Sample ID`, `Location ID`, LAT, LONG),
    by = c("Client ID" = "Sample ID")
  )


Selendang_data %>%
  select(`Location ID`, LAT, LONG) %>%
  summary()



Selendang_data_processed <- data.frame(
  Data_source = rep("Selendang", nrow(Selendang_data)),
  Study_name = rep("Selendang", nrow(Selendang_data)),                         
  Source_siteID = Selendang_data$'Location ID',
  Source_sampleID = Selendang_data$'Client ID',
  OSRI_siteID = NA,
  OSRI_sampleID = NA,
  Sample_motivation = NA,                  
  General_location = NA,
  Specific_location = Selendang_data$'Location ID',   
  Lat = Selendang_data$LAT,                                
  Long = Selendang_data$LONG,                              
  Year = NA,                                #Need to calculate 
  Month = NA,                               #Need to calculate
  Collection_date =	Selendang_data$'Date Collected',          
  DOY = NA,                                 #Need to calculate
  Collection_time = NA,
  Collection_method = NA,                 
  Species_complex = NA,                    #Need to calculate         
  Common_name = NA,                        #Need to calculate  
  Scientific_name = NA,                     #Need to calculate
  Genus_latin = NA,                         #Need to calculate
  Species_latin = NA,                       #Need to calculate
  Tissue_type = Selendang_data$Matrix,                          
  Sample_composition = NA,                   
  Number_in_composite = NA,
  Sex = NA,                                 
  Analysis_method = Selendang_data$Analytes, 
  Chem_code = NA,                           
  Parameter = NA,
  Value = Selendang_data$Result,
  Units = Selendang_data$Units,
  Value_standardized = NA,
  Units_standardized = NA,
  Detection_limit = NA,
  Reporting_limit = Selendang_data$'Reporting Limit',
  Basis = NA,
  Lab_replicate = NA,
  Qualifier_code = Selendang_data$'Lab Flag',
  Lipid_pct = NA,
  Moisture_pct = NA,
  Total_PAHs = NA,                                  #might be able to calculate this later
  Total_LMWAHs = NA,
  Total_HMWAHs = NA,
  Lab_ID = Selendang_data$'Lab ID',
  Notes = NA
)



#Add species names
Selendang_data_processed <- Selendang_data_processed %>%
  mutate(Common_name = case_when(
    str_detect(Source_sampleID, "CH") ~ "Black chiton",
    str_detect(Source_sampleID, "UR") ~ "Green sea urchin",
    str_detect(Source_sampleID, "MU") ~ "Blue mussel",
    str_detect(Source_sampleID, "ML") ~ "Blue mussel",
    str_detect(Source_sampleID, "PNK") ~ "Pink salmon",
    str_detect(Source_sampleID, "SL") ~ "Harbor seal",
    str_detect(Source_sampleID, "CD") ~ "Pacific cod",
    TRUE ~ NA_character_
  ))


#No location Data available


#Now add the rest
Selendang_data_processed <- Selendang_data_processed %>%
  mutate(
    Collection_date = as.Date(Collection_date, format = "%Y-%m-%d"),
    Year = year(Collection_date),
    Month = month(Collection_date),
    DOY = yday(Collection_date)
    
    #Collection_method = "Subsistence harvest",
    #Sample_motivation = "Biomonitoring",
    
    #Species_complex = "fish",
    
    #Common_name = _________, 
    #Scientific_name = ________________,

    #Genus_latin = sub(" .*", "", Scientific_name),
    #Species_latin = sub(".* ", "", Scientific_name)
    
  )





# Shigenaka Data ----------------------------------------------------------


Shigenaka_data <- read_excel("Input Data/Shigenaka Little Clam 2007 Data.xlsx")      


str(Shigenaka_data)
#tibble [968 x 15] (S3: tbl_df/tbl/data.frame)
#$ Laboratory ID   : chr [1:968] "2N7226-15" "2N7226-15" "2N7226-15" "2N7226-15" ...
#$ Field ID        : chr [1:968] "TC17" "TC17" "TC17" "TC17" ...
#$ Site ID         : chr [1:968] "Block QI" "Block QI" "Block QI" "Block QI" ...
#$ Moisture (%)    : num [1:968] 14.6 14.6 14.6 14.6 14.6 14.6 14.6 14.6 14.6 14.6 ...
#$ Analytes        : chr [1:968] "Naphthalene" "C-l Naphthalene" "C-2 Naphthalene" "C-3 Naphthalene" ...
#$ Result          : chr [1:968] "ND" "2.0999999999999999E-3" "1.6000000000000001E-3" "1.6000000000000001E-3" ...
#$ Unit Basis      : chr [1:968] "dry" "dry" "dry" "dry" ...
#$ Unit            : chr [1:968] "ng/mg" "ng/mg" "ng/mg" "ng/mg" ...
#$ Collection      : POSIXct[1:968], format: "2007-07-01" "2007-07-01" "2007-07-01" "2007-07-01" ...
#$ Study           : chr [1:968] "Shigenaka 2008, Exxon Valdez Intertidal monitoring" "Shigenaka 2008, Exxon Valdez Intertidal monitoring" "Shigenaka 2008, Exxon Valdez Intertidal monitoring" "Shigenaka 2008, Exxon Valdez Intertidal monitoring" ...
#$ Species         : chr [1:968] "Littleneck clam" "Littleneck clam" "Littleneck clam" "Littleneck clam" ...
#$ Tissue          : chr [1:968] "Wholebody" "Wholebody" "Wholebody" "Wholebody" ...
#$ Latin           : chr [1:968] "Leukoma staminea" "Leukoma staminea" "Leukoma staminea" "Leukoma staminea" ...
#$ Lab             : chr [1:968] "Louisana State University, Baton Rouge LA" "Louisana State University, Baton Rouge LA" "Louisana State University, Baton Rouge LA" "Louisana State University, Baton Rouge LA" ...
#$ Sampling purpose: chr [1:968] "Long term monitoring post spill" "Long term monitoring post spill" "Long term monitoring post spill" "Long term monitoring post spill" ...


Shigenaka_sites <- read_excel("Input Data/Shigenaka Little Clam 2007 Data.xlsx", sheet = "Sites")      
str(Shigenaka_sites)
#tibble [11 x 4] (S3: tbl_df/tbl/data.frame)
#$ Oiling Category: chr [1:11] NA "Unoiled" "Unoiled" "Unoiled" ...
#$ Site           : chr [1:11] NA "Bainbridge Bight" "Sheep Bay" "Outside Bay" ...
#$ Coordinates    : chr [1:11] "Latitude" "60°06'59\"N" "60°41 '06\"N" "60°38'17\"N" ...
#$ ...4           : chr [1:11] "Longitude" "148° 14' 48\"W" "145°56'22\"W" "147°27'02\"W" ...

Shigenaka_data_processed <- data.frame(
  Data_source = rep("Shigenaka", nrow(Shigenaka_data)),
  Study_name = Shigenaka_data$Study,                         
  Source_siteID = Shigenaka_data$'Site ID',
  Source_sampleID = Shigenaka_data$"Field ID",
  OSRI_siteID = NA,
  OSRI_sampleID = NA,
  Sample_motivation = NA,
  General_location = NA,                   
  Specific_location = NA,                 #Need to calculate
  Lat = NA,                                #Need to calculate
  Long = NA,                               #Need to calculate
  Year = NA,                               #Need to calculate
  Month = NA,                              #Need to calculate
  Collection_date =	Shigenaka_data$Collection,          
  DOY = NA,                                #Need to calculate
  Collection_time = NA,
  Collection_method = NA, 
  Species_complex = NA,                     #Need to calculate
  Common_name = Shigenaka_data$Species,         
  Scientific_name = NA,                     #Need to generate this
  Genus_latin = NA,                         #Need to calculate these below
  Species_latin = NA,                        #Need to calculate these below
  Tissue_type = Shigenaka_data$Tissue,                          
  Sample_composition = NA,                   
  Number_in_composite = NA,
  Sex = NA,                                  
  Analysis_method = Shigenaka_data$Analytes, 
  Chem_code = Shigenaka_data$Analytes,                           
  Parameter = Shigenaka_data$Analytes,
  Value = Shigenaka_data$Result,
  Units = Shigenaka_data$Unit,
  Value_standardized = NA,
  Units_standardized = NA,
  Detection_limit = NA,
  Reporting_limit = NA,
  Basis = Shigenaka_data$'Unit Basis',
  Lab_replicate = NA,
  Qualifier_code = NA,
  Lipid_pct = NA,
  Moisture_pct = Shigenaka_data$'Moisture (%)',
  Total_PAHs = NA,                              #might be able to calculate this later
  Total_LMWAHs = NA,
  Total_HMWAHs = NA,
  Lab_ID = Shigenaka_data$Lab,
  Notes = NA
)



unique(Shigenaka_data_processed$Common_name)

site_lookup1 <- tibble(
  `Site ID` = c(
    "Block QI", "Block Q2", "Block Q3", "Block Q4", "Block Q5 &Q6",
    "Mussel Q3", "Mussel Q4", "Mussel Q5 & Q6",
    "Snug",
    "Bainbridge",
    "Sheep QI", "Sheep Q2", "Sheep Q3", "Sheep Q5",
    "NW Bay"
  ),
  Specific_location = c(
    "Block Island", "Block Island", "Block Island", "Block Island", "Block Island",
    "Mussel Beach", "Mussel Beach", "Mussel Beach",
    "Snug Harbor",
    "Bainbridge Bight",
    "Sheep Bay", "Sheep Bay", "Sheep Bay", "Sheep Bay",
    "Northwest Bay West Arm"
  )
)


site_lookup2 <- tibble(
  `Site ID` = c(
    "Block QI", "Block Q2", "Block Q3", "Block Q4", "Block Q5 &Q6",
    "Mussel Q3", "Mussel Q4", "Mussel Q5 & Q6",
    "Snug",
    "Bainbridge",
    "Sheep QI", "Sheep Q2", "Sheep Q3", "Sheep Q5",
    "NW Bay"
  ),
  Lat = c(
    "60.53", "60.53", "60.53", "60.53", "60.53",
    "60.536111", "60.536111", "60.536111",
    "60.261944",
    "60.116389",
    "60.685", "60.685", "60.685", "60.685",
    "60.543889"
  )
)



site_lookup3 <- tibble(
  `Site ID` = c(
    "Block QI", "Block Q2", "Block Q3", "Block Q4", "Block Q5 &Q6",
    "Mussel Q3", "Mussel Q4", "Mussel Q5 & Q6",
    "Snug",
    "Bainbridge",
    "Sheep QI", "Sheep Q2", "Sheep Q3", "Sheep Q5",
    "NW Bay"
  ),
  Long = c(
    "-147.606667", "-147.606667", "-147.606667", "-147.606667", "-147.606667",
    "147.615556", "147.615556", "147.615556",
    "-147.765833",
    "-148.246667",
    "-145.939444", "-145.939444", "-145.939444", "-145.939444",
    "-147.6025"
  )
)

#Join to Shigenaka_data
Shigenaka_data_processed <- Shigenaka_data_processed %>%
  left_join(site_lookup1, by = c("Source_siteID" = "Site ID")) %>%
  left_join(site_lookup2, by = c("Source_siteID" = "Site ID")) %>%
  left_join(site_lookup3, by = c("Source_siteID" = "Site ID"))






Shigenaka_data_processed <- Shigenaka_data_processed %>%
  mutate(
    Specific_location = Specific_location.y,
    Lat = as.numeric(Lat.y),
    Long = as.numeric(Long.y)
  ) %>%
  select(-Specific_location.x, -Specific_location.y, -Lat.x, -Lat.y, -Long.x, -Long.y)





Shigenaka_data_processed <- Shigenaka_data_processed %>% 
  mutate(Sample_motivation = "Exxon Valdez Intertidal long term monitoring",
         Species_complex = "Clam",  #might just group this in the moullusca mussels later
         Scientific_name = "Leukoma staminea",
         Genus_latin = "Leukoma",
         Species_latin = "staminea",
         
         Collection_date = suppressWarnings(as.Date(Collection_date, format = "%Y-%m-%d")),
         Year = year(Collection_date),
         Month = month(Collection_date),
         DOY = yday(Collection_date)
          
         )





str(Shigenaka_data_processed)




# Merge all the dataframes ------------------------------------------------

NCCOS_data_processed
Diver_processed
Nationwide_data_processed
Wetzel_data_processed           #No lat long, need to add that --- Resolved
Stimmelmayr_data_processed      #No lat long, need to add that --- Resolved
Arnold_data_processed           #No lat long, need to add that --- Resolved
LTEMP_data_processed
Ma_data_processed
Harvey_data_processed
ERM_data_processed              #No lat long, need to add that --- Resolved
Selendang_data_processed        #No location data at all 
Shigenaka_data_processed


#Will need to clean the species and correct for capitalization and slight misspellings
#add lat long and make a consistent location
#make sure units are standard
#Maybe calculate total PAHs
#




#Check to make sure all column names match (I used this to QA/QC the code)
datasets <- list(
  NCCOS_data_processed,
  Diver_processed,
  Nationwide_data_processed, 
  Wetzel_data_processed, 
  Stimmelmayr_data_processed,
  Arnold_data_processed,
  LTEMP_data_processed, 
  Ma_data_processed,
  Harvey_data_processed,
  ERM_data_processed,
  Selendang_data_processed,
  Shigenaka_data_processed,
  Nationwide_data_processed
)


# Extract names for each
colnames_list <- lapply(datasets, names)

# Get all unique column names across all datasets
all_names <- unique(unlist(colnames_list))

# Create a presence/absence matrix
colname_matrix <- sapply(colnames_list, function(x) all_names %in% x)
rownames(colname_matrix) <- all_names

# Find inconsistent columns (not present in all datasets)
inconsistent_columns <- rownames(colname_matrix)[!apply(colname_matrix, 1, all)]

# View which columns are missing where
colname_matrix[!apply(colname_matrix, 1, all), , drop = FALSE] %>%
  as.data.frame() %>%
  rownames_to_column("Column_Name")
#They all look good



#Fix different column formats
# Add all known numeric columns
columns_to_numeric <- c(
  "Detection_limit", "Reporting_limit", "DOY", "Lipid_pct", "Moisture_pct", "Value",
  "Total_PAHs", "Total_LMWAHs", "Total_HMWAHs", "Year", "Month", "Number_in_composite",
  "Lat", "Long"
)

datasets_fixed <- lapply(seq_along(datasets), function(i) {
  df <- datasets[[i]]
  dataset_name <- paste0("Dataset ", i)
  
  # Fix Collection_date
  if ("Collection_date" %in% names(df)) {
    if (!is.character(df$Collection_date)) {
      df$Collection_date <- as.character(df$Collection_date)
    }
    
    df$Collection_date <- na_if(df$Collection_date, "")
    df$Collection_date <- na_if(df$Collection_date, "Not Available")
    df$Collection_date <- na_if(df$Collection_date, "NA")
    
    looks_like_ymd <- grepl("^\\d{4}-\\d{2}-\\d{2}$", df$Collection_date)
    parsed_date <- rep(NA, length(df$Collection_date))
    
    parsed_date[looks_like_ymd] <- as.Date(df$Collection_date[looks_like_ymd])
    
    needs_fallback <- !looks_like_ymd & !is.na(df$Collection_date)
    if (any(needs_fallback)) {
      message(dataset_name, ": Using fallback parser for some Collection_date values")
      fallback <- suppressWarnings(
        parse_date_time(df$Collection_date[needs_fallback], orders = c("ymd", "mdy", "dmy", "Ymd"))
      )
      parsed_date[needs_fallback] <- as.Date(fallback)
    }
    
    df$Collection_date <- parsed_date
  }
  
  # Harmonize numeric columns
  for (col in columns_to_numeric) {
    if (col %in% names(df)) {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    }
  }
  
  return(df)
})

# Combine all datasets safely
OSRI_data <- bind_rows(datasets_fixed)





#Now I need to go through each of the columns and make sure they have consistent values 
str(OSRI_data)
attach(OSRI_data)

unique(Data_source)       #looks good
unique(Study_name)        #looks good
unique(OSRI_siteID)       #Need to calculate
unique(OSRI_sampleID)     #Need to calculate
unique(Sample_motivation) #looks good, maybe we can add to it
#ADD unique(Region)      #Maybe make this after we start mapping
unique(General_location) #Hmmm, this might need some work.... 
unique(Specific_location) #Hmmm, this might need some work.... 
unique(Lat)               #I added all these so they should be the primary way to map data. 
#We are missing some locations, from the Wetzel, LTEMP, and Selendang datasets, but we have exhausted our search on those sites.
unique(Long)              #I added all these so they should be the primary way to map data
unique(Year)              #Need to fix certain years
unique(Month)             #Looks good
unique(Collection_date)   #Need to reformat as a date
unique(DOY)               #Looks good
unique(Collection_time)   #No data here, we should drop it
unique(Collection_method) #This is not that useful, we could probably drop it?
unique(Species_complex)   #Refine this from common and scientific name. Maybe add a taxonomic version and one that's layman's terms
#ADD Laymans_group
#ADD Family_Latin
unique(Common_name)       #Need to standardize formatting in all these names
unique(Scientific_name)   #Need to double check these and standardize them where necessary
unique(Genus_latin)       #These are probably okay
unique(Species_latin)     #These are probably okay
unique(Tissue_type)
#ADD unique(Tissue_type_standardized)    #Need to remove duplicates and standardize these 
#ADD unique(Tissue_type_standardized_grouped)  #Larger groups, muscle, organ, fat, whole body
unique(Sample_composition) #Looks good
unique(Number_in_composite) #remove -999.0 and -9 and replace with NA
unique(Sex)               #empty, we can drop it
unique(Analysis_method)   #This is mostly redundant with Parameter
unique(Chem_code)         #This seemed redundant and confusing so lets drop it
unique(Parameter)         #Send to Morgan to make sure they are all consistent
unique(Value)   
unique(Units)
unique(Value_standardized)  #Need to standardize all of these
unique(Units_standardized)  #Need to standardize all of these
unique(Basis)   #Might need to pull some of these from the Units column
unique(Detection_limit)
unique(Reporting_limit)
#ADD unique(Detection_limit_standardized)
#ADD unique(Reporting_limit_standardized)
unique(Lab_replicate)    #Is this really that many replicates?
unique(Qualifier_code)   #Most of these can be removed I think
unique(Lipid_pct)       #NEED to go back and retrieve this from each original dataset
unique(Moisture_pct)    #NEED to go back and retrieve this from each original dataset
unique(Total_PAHs)     #Can we calculate this? From the parameter column?
unique(Total_LMWAHs)  #Probably not going to use
unique(Total_HMWAHs)  #Probably not going to use
unique(Lab_ID)        #Remove the lab sample ID's, keep the lab information in there
unique(Notes)        #Need to transfer over to other columns


detach(OSRI_data)

#Add a standardized sample ID
OSRI_data <- OSRI_data %>% 
  mutate(
    OSRI_sampleID = 1:nrow(OSRI_data))
    #I might come back to site ID, or do that in ArcGIS


#Adjust sample motivation
unique(OSRI_data$Notes)
unique(OSRI_data$Data_source)
unique(OSRI_data$Study_name)

unique_motivations <- OSRI_data %>%
  filter(!is.na(Study_name) & Study_name != "") %>%
  distinct(Study_name, Data_source, Sample_motivation)
unique_motivations

write_xlsx(unique_motivations, "Output Data/unique_motivations.xlsx")


unique_motivations <- read_xlsx("Output Data/unique_motivations_MP.xlsx")
unique_motivations


unique_motivations <- unique_motivations %>%
  group_by(Study_name) %>%
  summarise(Sample_motivation = first(Sample_motivation), .groups = "drop")


OSRI_data <- OSRI_data %>%
  select(-Sample_motivation) %>%
  left_join(unique_motivations, by = "Study_name")





OSRI_data <- OSRI_data %>%
  select(-Sample_motivation) %>%  # Remove any existing Sample_motivation column to avoid .x/.y
  left_join(
    unique_motivations %>%
      select(Study_name, Sample_motivation) %>%
      distinct(),
    by = "Study_name"
  )


unique_motivations %>%
  count(Study_name) %>%
  filter(n > 1)



#Location data
OSRI_data <- OSRI_data %>%
  mutate(
    Lat = case_when(
      str_detect(General_location, "Utqiagvik") ~ 71.29088,
      TRUE ~ Lat  # Preserve existing values
    ),
    Long = case_when(
      str_detect(General_location, "Utqiagvik") ~ -156.78864,
      TRUE ~ Long  # Preserve existing values
    )
  )





locations <- OSRI_data %>%
  filter(is.na(Lat) | is.na(Long)) %>%
  distinct(Study_name, Data_source, General_location, Specific_location, Lat, Long)
locations

write_xlsx(locations, "Output Data/locations.xlsx")




#Reformat dates to get rid of Excel format
OSRI_data <- OSRI_data %>%
  mutate(
    Year_clean = case_when(
      Year > 30000 ~ year(as.Date(Year, origin = "1899-12-30")),  # Excel numeric date fix
      TRUE ~ as.integer(Year)
    ),
    Collection_date = as.Date(Collection_date, origin = "1899-12-30")
    
  )


#Add taxanomic and laymans groups
OSRI_data <- OSRI_data %>%
  mutate(
    Species_standard = str_to_lower(str_trim(Species_complex)),
    
    Taxonomic_group = case_when(
      Species_standard %in% c("clam", "cockles", "mussel", "mussels", "scallop-i", "scallop-2", "whelk", "mollusk", "molluscs") ~ "Mollusca",
      Species_standard %in% c("crab", "shrimp", "crustaceans") ~ "Arthropoda (Crustacea)",
      Species_standard %in% c("urchin", "starfish", "echinoderms") ~ "Echinodermata",
      Species_standard %in% c("fish", "finfish", "flatfish", "fish-adult", "fish-larval", "fish-adult/pregnant") ~ "Chordata (Actinopterygii)",
      Species_standard %in% c("cephalapods") ~ "Mollusca (Cephalopoda)",
      Species_standard %in% c("pinnipeds", "mammals", "whale") ~ "Chordata (Mammalia)",
      Species_standard %in% c("tunicate") ~ "Mollusca",
      Species_standard %in% c("vegetation") ~ "Plantae or Algae",
      Species_standard %in% c("other") ~ "Other/Unclassified",
      TRUE ~ NA_character_
    ),
    
    Layman_group = case_when(
      Species_standard %in% c("clam", "cockles", "mussel", "mussels", "scallop-i", "scallop-2", "whelk", "mollusk", "molluscs", "cephalapods") ~ "Shellfish",
      Species_standard %in% c("crab", "shrimp", "crustaceans") ~ "Crab/Shrimp",
      Species_standard %in% c("urchin", "starfish", "echinoderms") ~ "Urchins/Starfish",
      Species_standard %in% c("fish", "finfish", "flatfish", "fish-adult", "fish-larval", "fish-adult/pregnant") ~ "Fish",
      Species_standard %in% c("pinnipeds", "mammals", "whale") ~ "Marine Mammals",
      Species_standard %in% c("tunicate") ~ "Shellfish",
      Species_standard %in% c("vegetation") ~ "Plants/Algae",
      Species_standard %in% c("other") ~ "Other",
      TRUE ~ NA_character_
    )
  )
    

    
#Clean up common and scientific names:
OSRI_data <- OSRI_data %>%
  mutate(
    # Standardize common names
    Common_name = Common_name %>%
      str_trim() %>%
      str_to_lower() %>%
      str_replace_all("artic cod", "arctic cod") %>%
      str_replace_all("pink salmon\\?", "Pink Salmon") %>%
      str_replace_all("mussels?, mytilus sp\\.", "Mussel") %>%
      str_replace_all("blue mussels", "Blue Mussel") %>%
      str_replace_all("black chitons", "Black Chiton") %>%
      str_replace_all("ringed seal", "Ringed Seal") %>%
      str_replace_all("unoiled ringed seals", "Ringed Seal") %>%
      str_replace_all("spotted seal", "Spotted Seal") %>%
      str_replace_all("unknown seal", "Seal") %>%
      str_to_title(),
    
    # Clean scientific names and remove "sp.", "spp.", and similar
    Scientific_name = Scientific_name %>%
      str_trim() %>%
      str_to_lower() %>%
      str_replace_all("\\bsp\\.?\\b", "") %>%
      str_replace_all("\\bspp\\.?\\b", "") %>%
      str_replace_all("\\bspecies\\b", "") %>%
      str_replace_all("\\.+$", "") %>%       # remove trailing periods
      str_replace_all("\\s{2,}", " ") %>%
      str_trim() %>%
      str_to_sentence(),
    
    # Use if_else() inside mutate to replace values conditionally
    Scientific_name = if_else(Scientific_name == "Not defined", NA_character_, Scientific_name),
    Scientific_name = if_else(Common_name == "Cockle", "Clinocardium nuttallii", Scientific_name)
  ) %>% 
  mutate(
    # Remove any trailing/extra spaces again
    Scientific_name = str_trim(Scientific_name)
  ) %>%
  separate(
    col = Scientific_name,
    into = c("Genus_latin", "Species_latin"),
    sep = " ",
    remove = FALSE,
    extra = "drop",
    fill = "right"
  )



unique_names <- OSRI_data %>%
  select(any_of(c("Common_name", "Scientific_name", "Taxonomic_group", "Layman_group"))) %>%
  pivot_longer(cols = everything(), names_to = "Name_Type", values_to = "Name") %>%
  filter(!is.na(Name) & Name != "") %>%
  distinct(Name)

unique_names <- OSRI_data %>%
  filter(!is.na(Scientific_name) & Scientific_name != "") %>%
  distinct(Scientific_name, Common_name, Taxonomic_group, Layman_group)
unique_names

write_xlsx(unique_names, "Output Data/unique_names.xlsx")





#Standardize tissue types
OSRI_data <- OSRI_data %>%
  mutate(
    # Standardize spelling, capitalization, and naming
    Tissue_type_standardized = Tissue_type %>%
      str_trim() %>%
      str_to_lower() %>%
      str_replace_all("whole bodies composited", "whole body") %>%
      str_replace_all("wholebody", "whole body") %>%
      str_replace_all("whole organism", "whole body") %>%
      str_replace_all("whole fish", "whole body") %>%
      str_replace_all("whole head", "head") %>%
      str_replace_all("smoked fillet", "muscle") %>%
      str_replace_all("edible parts only", "muscle") %>%
      str_replace_all("roe", "eggs") %>%
      str_replace_all("blubber", "fat") %>%
      str_replace_all("heart", "heart") %>%
      str_replace_all("kidney", "kidney") %>%
      str_replace_all("muscle", "muscle") %>%
      str_replace_all("epidermis", "skin") %>%
      str_replace_all("neocortex", "brain") %>%
      str_replace_all("gut content", "stomach contents") %>%
      str_replace_all("stomach contents", "stomach contents") %>%
      str_replace_all("trachea contents", "stomach contents") %>%
      str_replace_all("^et$", "other") %>%
      str_replace_all("^tissue$", "unspecified tissue") %>%
      str_to_title(),  # Capitalize final standardized labels
    
    # Group into larger categories
    Tissue_type_standardized_grouped = case_when(
      Tissue_type_standardized %in% c("Muscle", "Smoked Fillet") ~ "Muscle",
      Tissue_type_standardized %in% c("Fat", "Blubber") ~ "Fat",
      Tissue_type_standardized %in% c("Liver", "Kidney", "Heart", "Pancreas", "Lung", "Ovary", "Thyroid", "Brain") ~ "Organ",
      Tissue_type_standardized %in% c("Skin", "Hair", "Epidermis") ~ "Skin",
      Tissue_type_standardized %in% c("Eggs", "Roe") ~ "Eggs",
      Tissue_type_standardized %in% c("Whole Body", "Whole Organism") ~ "Whole Body",
      Tissue_type_standardized %in% c("Digestive Content", "Feces") ~ "Digestive",
      TRUE ~ "Other"
    )
  )
    








#Add composite data
composite_values_from_notes <- c("< 5", "5-8", "> 8")

OSRI_data <- OSRI_data %>%
  mutate(
    Number_in_composite = if_else(
      is.na(Number_in_composite) | Notes %in% composite_values_from_notes,
      as.character(Notes),
      as.character(Number_in_composite)),
    Sample_composition = ifelse(Number_in_composite > 1, "composite", "individual")
  )





#Fix composite data issues
OSRI_data <- OSRI_data %>%
  mutate(Number_in_composite = ifelse(Number_in_composite == -999.0, NA, Number_in_composite), 
         Number_in_composite = ifelse(Number_in_composite == -9.0, NA, Number_in_composite))



#Create  a new dataframe with the Parameter information for Morgan to review:
unique(OSRI_data$Parameter)


OSRI_data <- OSRI_data %>%
  mutate(
    Parameter = Parameter %>%
      str_trim() %>%
      str_replace_all("\\s+", " ") %>%  # collapse multiple spaces
      str_replace_all("\\(a\\)", "[a]") %>%  # unify bracket styles
      str_replace_all("\\(b\\)", "[b]") %>%
      str_replace_all("\\(k\\)", "[k]") %>%
      str_replace_all("\\(g,h,i\\)", "[g,h,i]") %>%
      str_replace_all("\\(1,2,3 - cd\\)", "[1,2,3-c,d]") %>%
      str_replace_all("Benzo \\(", "Benzo(") %>%  # fix escaped parenthesis
      str_replace_all("\\) ", ")") %>%
      str_replace_all(" +", " ") %>%
      str_to_title()
  )


Parameter_data <- data.frame(unique(OSRI_data$Parameter))
Parameter_data





Parameter_data <- data.frame(unique(OSRI_data$Parameter))
Parameter_data

write_xlsx(Parameter_data, "Output Data/Parameter_data.xlsx")

Parameter_data <- read_xlsx("Output Data/Parameter_data_MP.xlsx")
Parameter_data


OSRI_data <- OSRI_data %>%
  left_join(
    Parameter_data %>%
      distinct(Parameter_original, Parameter_standardized),
    by = c("Parameter" = "Parameter_original")
  )












#Filter out fat content in rows and assign it to the correct samples:
OSRI_data %>%
  filter(tolower(trimws(Units)) %in% c("%", "percent")) %>%
  select(Data_source, Parameter, Value)


#Convert the units and values to standardized format:
convert_to_ng_per_g <- function(value, unit) {
  unit <- tolower(trimws(unit))
  
  if (unit %in% c("ppb", "ug/kg", "µg/kg", "μg/kg", "µg/kg")) {
    return(value)  # µg/kg = 1 ng/g
  } else if (unit %in% c("ug/g", "µg/g")) {
    return(value * 1000)  # µg/g = 1000 ng/g
  } else if (unit %in% c("mg/kg")) {
    return(value * 1000)  # mg/kg = 1000 ng/g
  } else if (unit %in% c("ng/g", "ng/g dry", "ng/g wet", "ng/dry g")) {
    return(value)
  } else if (unit %in% c("ng/mg")) {
    return(value * 1000)
  } else if (unit %in% c("g")) {
    return(value * 1e9)
  } else if (unit %in% c("%", "percent")) {
    return(value * 1e7)
  } else if (unit == "ng") {
    return(NA_real_)
  } else {
    return(NA_real_)
  }
}


OSRI_data <- OSRI_data %>%
  mutate(
    Units_standardized = "ng/g",
    Value_standardized = mapply(convert_to_ng_per_g, Value, Units), 
    Value_standardized = ifelse(Value_standardized == -9, NA, Value_standardized),
    Detection_limit_standardized = mapply(convert_to_ng_per_g, Detection_limit, Units),  #Also adjust the MDL and MRL
    Detection_limit_standardized = ifelse(Detection_limit_standardized == -9, NA, Detection_limit_standardized),
    Reporting_limit_standardized = mapply(convert_to_ng_per_g, Reporting_limit, Units),    
    Reporting_limit_standardized = ifelse(Reporting_limit_standardized == -9, NA, Reporting_limit_standardized),
  
  )


unique(OSRI_data$Units)
unique(OSRI_data$Value)
unique(OSRI_data$Units_standardized)
unique(OSRI_data$Value_standardized)
summary(OSRI_data$Value_standardized)

OSRI_data %>%
  filter(Value_standardized < 0 | Value_standardized > 1e6) %>%
  select(Data_source, Parameter, Value, Units, Value_standardized)


summary(OSRI_data$Detection_limit_standardized)

OSRI_data %>%
  filter(Detection_limit_standardized < 0 | Detection_limit_standardized > 1e6) %>%
  select(Data_source, Parameter, Value, Units, Value_standardized)


summary(OSRI_data$Reporting_limit_standardized)

OSRI_data %>%
  filter(Reporting_limit_standardized < 0 | Reporting_limit_standardized > 1e6) %>%
  select(Data_source, Parameter, Value, Units, Value_standardized)

#^^All of these look good except for the LTEMP data that I need to revisit


#Standardize the basis:
basis_values_from_notes <- c("wet", "DRY", "dry", "dry wt", "Wet", "WET", "Dry")

OSRI_data <- OSRI_data %>%
  mutate(
    Basis = if_else(is.na(Basis) | Notes %in% basis_values_from_notes, Notes, Basis),
    
    Basis_standardized = case_when(
      is.na(Basis) ~ NA_character_,
      tolower(trimws(Basis)) %in% c("dry", "dw", "dry wt", "dwt", "dry weight", "dry", "DRY", "Dry") ~ "dry",
      tolower(trimws(Basis)) %in% c("wet", "ww", "wet weight", "wet", "Wet", "WET") ~ "wet",
      TRUE ~ NA_character_
    )
  )



#Remove NA values in lipid pct
OSRI_data <- OSRI_data %>% 
  mutate(Lipid_pct = ifelse(Lipid_pct == -9, NA, Lipid_pct))


#Clean up the Lab_ID column
values_to_na <- c(
  "","0507100-01", "0507100-02", "0507100-03", "0507100-03X", "0507100-04", "0507100-05",
  "0507100-06", "0507100-07", "0507100-08", "0507100-09", "0507100-10", "0507100-11",
  "0507100-12", "0507100-13", "0507100-13X", "0507100-14", "0507100-14X", "0507100-15",
  "0507100-16", "0507100-17", "0507008-01", "0507008-02", "0507007-01", "0507007-02",
  "0507006-01", "0507006-02", "0507006-03", "0507006-04", "0507006-05", "0507006-06",
  "0507006-07"
)

OSRI_data <- OSRI_data %>%
  mutate(Lab_ID = if_else(Lab_ID %in% values_to_na, NA_character_, Lab_ID))







#Calculate the total PAHs in a sample
##############Need to figure everything else out before we come back to this


  
#Remove rows we don't use  
OSRI_data <- OSRI_data %>%
  select(-c(Collection_time, Collection_method, Sex, Analysis_method, Chem_code))




write_xlsx(OSRI_data, "Output Data/OSRI_data.xlsx")


write.csv("Output Data/OSRI_data.xlsx")







unique_motivations_MP <- read_xlsx("Output Data/unique_motivations_MP.xlsx")      
unique_motivations_MP


str(OSRI_data)


mussel_watch_data <- OSRI_data %>%
  filter(str_detect(tolower(Study_name), "mussel watch"))

mussel_watch_data







