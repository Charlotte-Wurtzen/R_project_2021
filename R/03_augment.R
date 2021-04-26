# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
golub_clean <- read_tsv(file = "data/02_golub_clean.tsv.gz")


# Wrangle data ------------------------------------------------------------
golub_clean_aug <- golub_clean %>% 
  mutate(type = case_when(value == "ALL" ~ 0,
                          value == "AML" ~ 1)) 
  relocate(type)


# Write data --------------------------------------------------------------
write_tsv(x = golub_clean_aug,
          file = "data/03_golub_clean_aug.tsv.gz")