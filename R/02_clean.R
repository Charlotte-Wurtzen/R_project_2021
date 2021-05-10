# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Load data ---------------------------------------------------------------
x <- read_tsv(file = "data/golub_x.tsv.gz")
y <- read_tsv(file = "data/golub_y.tsv.gz")


# Wrangle data ------------------------------------------------------------
golub_clean <- bind_cols(x, y)


# Write data --------------------------------------------------------------
write_tsv(x = golub_clean,
          path = "data/02_golub_clean.tsv.gz")