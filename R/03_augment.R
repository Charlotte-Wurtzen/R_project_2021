# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
golub_clean <- read_tsv(file = "data/02_golub_clean.tsv.gz")


# Wrangle data ------------------------------------------------------------

# Separate by type and join again
ALL <- golub_clean %>% 
  filter(value == "ALL")

AML <- golub_clean %>% 
  filter(value == "AML")

golub_clean <- ALL %>% full_join(AML)

# Binarize type
golub_clean_aug <- golub_clean %>% 
  mutate(type = case_when(value == "ALL" ~ 0,
                          value == "AML" ~ 1)) %>% 
  mutate(id = row_number()) %>% 
  relocate(c(id, 
             type))


# Write data --------------------------------------------------------------
write_tsv(x = golub_clean_aug,
          file = "data/03_golub_clean_aug.tsv.gz")