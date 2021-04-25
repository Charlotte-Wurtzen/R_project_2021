# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")

# Load data ---------------------------------------------------------------
x <- read_tsv(file = "2021_group_7/data/golub_x.tsv.gz")
y <- read_tsv(file = "2021_group_7/data/golub_y.tsv.gz")


# Wrangle data ------------------------------------------------------------
gravier_clean <- bind_cols(x,y) 


# Write data --------------------------------------------------------------
write_tsv(x = gravier_clean,
          path = "2021_group_7/data/02_golub_clean.tsv.gz")
