# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Load data ---------------------------------------------------------------
load(file = "data/_raw/golub.RData")


# Wrangle data ------------------------------------------------------------
x_data <- golub %>% 
  pluck("x") %>% 
  as_tibble()

y_data <- golub %>% 
  pluck("y") %>% 
  as_tibble()


# Write data --------------------------------------------------------------
write_tsv(x = x_data,
          file = "data/01_golub_x.tsv.gz")

write_tsv(x = y_data,
          file = "data/01_golub_y.tsv.gz")