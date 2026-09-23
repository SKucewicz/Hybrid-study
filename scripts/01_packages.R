cat("Setting up project environment...\n")

# Install renv if needed
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# Restore packages from lockfile (only if it exists)
if (file.exists("renv.lock")) {
  renv::restore(prompt = FALSE)
}

#output message
cat("Loading packages...\n")

#load library
library(dplyr)
library(readr)
library(tidyverse)
library(gplots)
library(Hmisc)
library(corrplot)
library(skimr)
library(ggpubr)
library(extrafont)
library(ordinal)
library(nlme)
library(lme4)
library(ggResidpanel)
library(performance)
library(sjPlot)
library(car)
library(likert)
library(effectsize)
library(emmeans)
library(effects)

# Define output folders
fig_path <- here::here("outputs", "figures")
tab_path <- here::here("outputs", "tables")

# Create folders if they don't exist
dir.create(fig_path, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_path, recursive = TRUE, showWarnings = FALSE)

cat("Packages loaded and output folders ready.\n")