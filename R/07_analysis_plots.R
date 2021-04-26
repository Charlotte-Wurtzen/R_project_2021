# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")

# Load data ---------------------------------------------------------------
golub_clean_aug <- read_tsv(file = "data/03_golub_clean_aug.tsv.gz")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Wrangle data ------------------------------------------------------------
# pivot longer 
golub_data_long <- longer(golub_clean_aug)

# Histogram ----------------------------------------------------------------
golub_data_long %>% 
  count(type) %>% 
  ggplot(aes(x= type, y=n, fill= type))+
  geom_col()

# Scatter plot ------------------------------------------------------------
ggplot(data = golub_data_long, mapping = aes(x =gene, y = norm_expr_level, colour = norm_expr_level)) +
  geom_point() + 
  scale_color_gradient(low="blue", high="red")+
  theme_minimal()+
  ylab(label="Gene") +
  xlab(label="Normalized Expression Level") +
  labs(title = "?", caption = "Group 7")


