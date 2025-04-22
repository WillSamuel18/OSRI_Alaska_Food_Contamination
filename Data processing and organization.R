################################################################################

#OSRI Project - Subsistence Foods Baseline Contaminants Report
#Started on 4/21/2025
#Primary Author: William Samuel
#Project PI: Dr. Morgan Powers

#A meta-analysis project to understand the the impacts of contaminants on 
#Alaskan marine foods. These data include contaminant information from 
#fish, invertebrates, and marine mammals. 

#This script is for data organizing and manipulation. See "Analysis and figures"
#for the data analysis and visualization. 

################################################################################

library(tidyverse)



# NCCOS Data --------------------------------------------------------------

NCCOS_clam <- read.delim("Input Data/NCCOS PAH Data/nccos_chem_data_clam.txt")      
NCCOS_cockles <- read.delim("Input Data/NCCOS PAH Data/nccos_chem_data_cockles.txt")      
NCCOS_fish <- read.delim("Input Data/NCCOS PAH Data/nccos_chem_data_fish.txt")      
NCCOS_flatfsh <- read.delim("Input Data/NCCOS PAH Data/nccos_chem_data_flatfish.txt")      
NCCOS_mussel <- read.delim("Input Data/NCCOS PAH Data/nccos_chem_data_mussel.txt")      
NCCOS_shrimp <- read.delim("Input Data/NCCOS PAH Data/nccos_chem_data_shrimp.txt")      
NCCOS_starfish <- read.delim("Input Data/NCCOS PAH Data/nccos_chem_data_starfish.txt")      














