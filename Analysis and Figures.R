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



# Import and organize Data ------------------------------------------------

OSRI_data <- read_xlsx("Output Data/OSRI_data.xlsx")      
str(OSRI_data)


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





