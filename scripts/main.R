####Project: Modex - Study 1: Cross Fit
####Authors: Sam K, Andrew Govus, Matt Driller, CVP
####Date: 30//08/2023
####Date file:
####Explanatory variables:
#1. Time; 2 levels: Visit 1 (Pre), 2 (Post)
#2. Condition: 2 levels: PYC, PLA
#3. Order: 2 levels: PYC/PLA, PLA/PYC
#4. Sex: 2 levels: Male/Female
#5. Age: Continuous covariate, integer, zero bound
####Response Variables/features: AGE:P_PEAKPRE
#Purpose: Analyse two period AB cross over trial

#clear everything in R session environment  
rm(list = ls())

#give command
cat("Starting analysis pipeline...\n")

# Run project in the following order
source(here::here("Scripts", "01_packages.R"))
source(here::here("Scripts", "02_data_import.R"))
source(here::here("Scripts", "03_data_cleaning.R"))
ksource(here::here("Scripts", "04_descriptive_stats.R"))
#view the below script to view parcimonious model selection                            
#source(here::here("Scripts", "05_mixed_model_selection.R"))
source(here::here("scripts", "06_mixed_model_final"))
source(here::here("Scripts", "07_figures.R"))
source(here::here("Scripts", "08_tables.R"))

#give command
cat("Analysis complete.\n")

# ==========================================
# End of pipeline
# ==========================================










