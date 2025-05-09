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



View(Diver_data)
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
  Analysis_method = Diver_data$Analysis_Detail, #ORR this might be the analysis column
  Chem_code = NA,                           #might be able to get this from Analysis_method??
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
  Moisture_pct = LTEMP_data$PCT_MOIST,
  Total_PAHs = NA,
  Total_LMWAHs = NA,
  Total_HMWAHs = NA,
  Lab_ID = NA,
  Notes = NA
)


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
  separate(Diver_data.Location_Geom, into = c("Lat", "Long"), sep = " ", convert = TRUE)
head(latlong)

Diver_processed <- Diver_processed %>% 
  mutate(
    Lat = latlong$Lat,
    Long = latlong$Long
    )



str(Diver_processed)
head(Diver_processed) #115,046 data points!




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

View(Wetzel_data_processed) #887 data points



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
      # Year range (e.g., "2012-2013" or "2012-2014")
      str_detect(Collection_date, "^\\d{4}\\s*[--]\\s*\\d{4}$") ~ str_replace_all(Collection_date, "\\s*", ""),
      
      # Standard date format (YYYY-MM-DD)
      str_detect(Collection_date, "^\\d{4}-\\d{2}-\\d{2}$") ~ str_sub(Collection_date, 1, 4),
      
      TRUE ~ NA_character_
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
    "175.000", "174.000", "171.000", "168.000", "169.000", "169.500")
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
  select(-Lat.y, -Long.y)



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







# Merge all the dataframes ------------------------------------------------

NCCOS_data_processed
Diver_processed
Wetzel_data_processed
Stimmelmayr_data_processed
Arnold_data_processed
LTEMP_data_processed




#Will need to clean the species and correct for capitalization and slight misspellings
#add lat long and make a consistent location
#make sure units are standard
#Maybe calculate total PAHs
#



# Storage -----------------------------------------------------------------

NCCOS_clam_processed <- df %>% rename ( 
  Data_source = 
    Study_name = 
    Source_siteID = 
    Source_sampleID = 
    OSRI_siteID = 
    OSRI_sampleID = 
    Sample_motivation =    
    General_location = 
    Specific_location = 
    Lat = 
    Long = 
    Year = 
    Month = 
    Collection_date =	
    DOY = 
    Collection_time = 
    Collection_method = 
    Species_complex =
    Species = 
    Scientific_name = 
    Genus_latin = 
    Species_latin = 
    Tissue_type = 
    Sample_composition = 
    Sex = 
    Analysis_method = 
    Chem_code = 
    Parameter = 
    Value = 
    Units = 
    Value_standardized = 
    Units_standardized = 
    Detection_limit = 
    Reporting_limit = 
    Lab_replicate = 
    Qualifier_code = 
    Lipid_pct = 
    Total_PAHs = 
    Total_LMWAHs = 
    Total_HMWAHs = 
    Lab_ID =
)







