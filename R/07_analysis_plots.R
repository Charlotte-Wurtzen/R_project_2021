# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")

# Load data ---------------------------------------------------------------
golub_clean_aug <- read_tsv(file = "data/03_golub_clean_aug.tsv.gz")

# Wrangle data ------------------------------------------------------------
golub_data_long <- golub_clean_aug %>% 
  pivot_longer(cols = c(-type,-value), 
               names_to = "gene", 
               values_to = "expr_level")
# Histogram ----------------------------------------------------------------
golub_data_long %>% 
  count(value) %>% 
  ggplot(aes(x= value, y=n))+
  geom_col()
