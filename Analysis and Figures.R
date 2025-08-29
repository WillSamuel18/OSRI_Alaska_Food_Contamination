################################################################################

#OSRI Project - Subsistence Foods Baseline Contaminants Report
#Started on 4/21/2025
#Primary Author: William Samuel
#Project PI: Dr. Morgan Powers

#A meta-analysis project to understand the the impacts of contaminants on 
#Alaskan marine foods. These data include contaminant information from 
#fish, invertebrates, and marine mammals. 

#This script is for data analysis and visualization. 

################################################################################

library(tidyverse)
library(ggplot2)
library(readr)
library(tibble)
library(stringr)
library(writexl)



# Summarize subsistence harvest -------------------------------------------
#Group Subsistence dataset by Layman groups (Fish, Marine mammals, and marine inverts) (common in lbs or kg)
#Summarize relative importance/harvest for each community
#Compare that to data availability 


#Communities on map, 2008 NA Marine Atlas with southcentral and southeast separated

#Map 1 - Ecoregions with all points
#summary data with total PAHs for GIS

#Map series 2 - region specific  for all 5 regions

#with Map 2 use layman_group (6 groups)

#https://www.fisheries.noaa.gov/resource/map/alaska-subsistence-fishing-communities-interactive-map

#Compare subsistence harvest weight values (figure out how to weigh fish and lbs and marine mammals) to hydrocarbon data counts to say something about


subistence_data <- read.csv("Input Data/subsistence_communities.csv")      
subistence_data

#Add lat/long for communities missing those values
coords_manual <- tribble(
  ~COMMUNITY,                  ~LATITUDE,    ~LONGITUDE,
  "AFOGNAK",                   58.00778,     -152.76806,  # :contentReference[oaicite:0]{index=0}
  "ALEXANDER CREEK",           61.72550,     -150.86950,  # Alexander Creek on Susitna R. :contentReference[oaicite:1]{index=1}
  "ANNETTE ISLAND",            55.13500,     -131.45580,  # Island centroid (Metlakatla area) :contentReference[oaicite:2]{index=2}
  "BEAR LAKE",                 60.19053,     -149.36853,  # Kenai Pen. Bear Lake near Seward :contentReference[oaicite:3]{index=3}
  "BIRD CREEK",                60.97361,     -149.46583,  # community/stream mouth area :contentReference[oaicite:4]{index=4}
  "CANYON VILLAGE",            67.15484,     -142.08782,  # historical Gwich'in village site centroid :contentReference[oaicite:5]{index=5}
  "CHATANIKA",                 65.11222,     -147.47722,  # :contentReference[oaicite:6]{index=6}
  "CHATHAM",                   57.51528,     -134.94361,  # Chatham, on Admiralty Island :contentReference[oaicite:7]{index=7}
  "CHEKOK LAKE",               59.97779,     -154.26403,  # Lake & Peninsula Borough :contentReference[oaicite:8]{index=8}
  "CLEAR AFB",                 64.29095,     -149.17992,  # Clear Air Force Base :contentReference[oaicite:9]{index=9}
  "EKLUTNA",                   61.37226,     -149.03067,  # (Eklutna Lake area; community nearby) :contentReference[oaicite:10]{index=10}
  "ELLAMAR",                   60.89556,     -146.69750,  # Ellamar, PWS :contentReference[oaicite:11]{index=11}
  "KING ISLAND",               64.85170,     -166.11122,  # island locality (Nome area) :contentReference[oaicite:12]{index=12}
  "NUCHEK",                    60.33333,     -146.65528,  # Nuchek (historical), PWS :contentReference[oaicite:13]{index=13}
  "OLIKTOK LRRS AIRPORT",      70.49971,     -149.87953,  # POLI airport site :contentReference[oaicite:14]{index=14}
  "PETERS CREEK",              61.41111,     -149.44556,  # Anchorage muni CDP centroid :contentReference[oaicite:15]{index=15}
  "PORT WILLIAMS",             58.49222,     -152.58278,  # Shuyak Island hamlet/cannery area :contentReference[oaicite:16]{index=16}
  "ROWAN BAY",                 56.66836,     -134.26291,  # SE AK (Petersburg Borough) :contentReference[oaicite:17]{index=17}
  "SECURITY BAY",              56.83806,     -134.32750,  # Kuiu Island bay center :contentReference[oaicite:18]{index=18}
  "SOURDOUGH",                 62.52908,     -145.51700,  # Sourdough area on Richardson Hwy (roadhouse vicinity) :contentReference[oaicite:19]{index=19}
  "TELIDA",                    63.38304,     -153.28081,  # Telida village :contentReference[oaicite:20]{index=20}
  "UGANIK BAY",                57.67059,     -153.51453,  # South Arm Uganik Bay area (Kodiak) :contentReference[oaicite:21]{index=21}
  "VOZNESENKA",                59.79457,     -151.09864,  # Kenai Pen. (Old Believer community) :contentReference[oaicite:22]{index=22}
  "YES BAY",                   55.91333,     -131.79361   # Yes Bay (Ketchikan Gateway) :contentReference[oaicite:23]{index=23}
)


#"DENALI NATIONAL PARK"
#"KUSKOKWIM RIVER"
#"KWETHLUK & KIPNUK"
#"LAKE CREEK"
#"LOWER KUSKOKWIM"
#"MAY CREEK"
#"NEETS BAY"
#"SERENITY CREEK"
#"TUNUNAK + MEKORYUK"


# Fill only where LAT/LONG are currently NA
subistence_data <- subistence_data %>%
  mutate(
    LATITUDE  = if_else(is.na(LATITUDE),  NA_real_, LATITUDE),
    LONGITUDE = if_else(is.na(LONGITUDE), NA_real_, LONGITUDE)
  ) %>%
  # normalize COMMUNITY for a safe join (trim and uppercase to match our lookup)
  mutate(COMMUNITY_norm = str_to_upper(str_trim(COMMUNITY))) %>%
  left_join(
    coords_manual %>% mutate(COMMUNITY_norm = str_to_upper(COMMUNITY)) %>%
      select(COMMUNITY_norm, LATITUDE_fill = LATITUDE, LONGITUDE_fill = LONGITUDE),
    by = "COMMUNITY_norm"
  ) %>%
  mutate(
    LATITUDE  = if_else(is.na(LATITUDE),  LATITUDE_fill,  LATITUDE),
    LONGITUDE = if_else(is.na(LONGITUDE), LONGITUDE_fill, LONGITUDE)
  ) %>%
  select(-COMMUNITY_norm, -LATITUDE_fill, -LONGITUDE_fill)

#Quick QA: what's still missing (broad regions, unknowns, etc.)?
still_missing <- subistence_data %>%
  filter(is.na(LATITUDE) | is.na(LONGITUDE)) %>%
  distinct(COMMUNITY) %>%
  arrange(COMMUNITY)

print(still_missing)


#Remove remaining rows
subistence_data <- subistence_data %>% 
  filter(!is.na(LATITUDE))


#Convert to common units:
str(subistence_data)
#Average weights or edible meat --- These conversions should be in a supplemental table
#Pink Salmon 1.5 kg
#Chinook 10 kg
#Coho 3 kg
#Chum 2.5 kg
#Sockeye 2.25 kg
#Polar Bear 250 kg
#Sea Otter 14 kg
#Walrus 540 kg
#Beluga 550 kg

#Convert lbs to kg
subistence_data <- subistence_data %>% 
  mutate(Halibut_kg = SHARC_HALIBUT_LBS/2.205,
         Invert_kg = ADFG_MINVERT_LBS/2.205,
         NonSalmon_kg = ADFG_NONSALMFISH_LBS/2.205)

#Convert #of individuals to kg
subistence_data <- subistence_data %>% 
  mutate(Chinook_kg = ADFG_SALMON_CHINOOK_NUM*10,
         Coho_kg = ADFG_SALMON_COHO_NUM*3,
         Chum_kg = ADFG_SALMON_CHUM_NUM*2.5,
         Sockeye_kg = ADFG_SALMON_SOCK_NUM*2.25,
         Pink_kg = ADFG_SALMON_PINK_NUM*1.5,
         PolarBear_kg = USFWS_POLAR_BEAR_NUM*250,
         SeaOtter_kg = USFWS_SEA_OTTER_NUM*14,
         Walrus_kg = USFWS_WALRUS_NUM*540,
         Beluga_kg = ABWC_BELUGA_NUM*550)



write_csv(subistence_data, "Output data/subistence_data.csv")



str(subistence_data)


sub_one_coord <- subistence_data %>%
  arrange(COMMUNITY, desc(!is.na(LATITUDE)), desc(!is.na(LONGITUDE))) %>%
  group_by(COMMUNITY) %>%
  summarise(LATITUDE = first(na.omit(LATITUDE)),
            LONGITUDE = first(na.omit(LONGITUDE)),
            .groups = "drop")

subsistence_summary <- subistence_data %>%
  group_by(COMMUNITY) %>%
  summarise(
    Fish_kg = mean(rowSums(across(c(Chinook_kg, Coho_kg, Chum_kg, Sockeye_kg,
                                    Pink_kg, Halibut_kg, NonSalmon_kg)),
                           na.rm = TRUE), na.rm = TRUE),
    MarineMammals_kg = mean(rowSums(across(c(Walrus_kg, Beluga_kg, PolarBear_kg, SeaOtter_kg)),
                                    na.rm = TRUE), na.rm = TRUE),
    MarineInverts_kg = mean(Invert_kg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(sub_one_coord, by = "COMMUNITY") %>%
  mutate(across(c(Fish_kg, MarineMammals_kg, MarineInverts_kg), as.numeric)) %>%
  mutate(total_kg = rowSums(across(c(Fish_kg, MarineMammals_kg, MarineInverts_kg)), na.rm = TRUE)) %>% 
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))





subsistence_summary <- subsistence_summary %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))


View(subsistence_summary)

write_xlsx(subsistence_summary, "Output data/subsistence_summary_by_community.xlsx")



write_csv(subsistence_summary, "Output data/subsistence_summary_by_community2.csv")






##### NEED TO DO:
#Read in the the subsistence data with regions -> Create pie charts
#Assign regions to the Sum dataset and read back in -> Create pie charts
#Add pie charts to ArcGIS
#Create heatmaps by layman group using the tessalation
subsistence_summary_coastal <- read.csv("Input Data/subsistence_summary_by_community2_coastal.csv")      






# Import and organize Data ------------------------------------------------


OSRI_data <- read_xlsx("Output Data/OSRI_data.xlsx")      
str(OSRI_data)


OSRI_data <- read_xlsx("Output Data/OSRI_data.xlsx")      



#To do list:
#Need to assign samples to spatial regions using lat longs












# Exploratory analysis ----------------------------------------------------
#Summary statistics (mean, median, range, detection frequency) by:
  #Analyte and SumPAH
  #Taxon
  #Region (e.g., Arctic, Northwest, Southwest, Southcentral, Southeast)
  #Time period (decadal, seasonal)






# Data Gap Identification -------------------------------------------------
#Spatial Gaps: Map sampling locations to identify understudied areas.
#Temporal Gaps: Assess whether data coverage is consistent over time.
#Taxonomic Gaps: Compare representation across taxa groups and over space
#Subsistence diet Mismatch: Investigate data found against subsistence consumed items to find what is missing





# Time series analysis ----------------------------------------------------
#Mann-Kendall test for monotonic trends.
#If data allows - Seasonal decomposition (STL) to separate long-term trends from seasonal variation.
#If data allows - Breakpoint Detection: Identify significant shifts in contaminant levels (e.g., Pettitt test).




# Cross-Taxon & Cross-Compound Analysis -----------------------------------
#If Data Allows- Principal Component Analysis (PCA): Identify dominant PAH analyte patterns across taxa.
#If Data Allows- Regression Models: Test if lipid content, body size, or diet predict tissue concentrations.





# Gap identification summary ----------------------------------------------
#Tabulate underrepresented:
  #Geographic regions
  #Time periods
  #Species/taxa
  #Compound-specific high-resolution data






