# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")

# Load data ---------------------------------------------------------------
x <- read_tsv(file = "data/golub_x.tsv.gz")
y <- read_tsv(file = "data/golub_y.tsv.gz")


# Wrangle data ------------------------------------------------------------
golub_clean <- bind_cols(x,y)

# separate by type and join again
ALL <- golub_clean %>% 
  filter(value == "ALL")

AML <- golub_clean %>% 
  filter(value == "AML")

golub_clean <- ALL %>% full_join(AML)


# Write data --------------------------------------------------------------
write_tsv(x = golub_clean,
          path = "data/02_golub_clean.tsv.gz")

