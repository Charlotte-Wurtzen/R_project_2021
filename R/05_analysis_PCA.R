# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
golub_clean_aug <- read_tsv(file = "data/03_golub_clean_aug.tsv.gz")


# Wrangle data ------------------------------------------------------------
golub_clean_aug %>% ...


# Model data
golub_clean_aug %>% ...


# Visualise data ----------------------------------------------------------
golub_clean_aug %>% ...


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)