# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
golub_clean_aug <- read_tsv(file = "data/03_golub_clean_aug.tsv.gz")

# Wrangle data ------------------------------------------------------------

# pivot longer
golub_data_long <- longer(golub_clean_aug)

# sample 100 genes randomly
set.seed(12345)
golub_data_long <- 
  golub_data_long %>% 
  sample_n(100)


# Visualise data ----------------------------------------------------------
<<<<<<< HEAD
#pl1 <- golub_data_long %>% 
  
=======
golub_data_long %>% 
  ggplot(golub_data_long, 
         mapping = aes(x = type,y = gene, fill = norm_expr_level)) +
  geom_tile() +
  xlab(label = "Type") + 
  theme(axis.title.y = element_blank(), # Remove the y-axis title
        axis.text.x = element_text(angle = 45, vjust = 0.5)) # Rotate the x-axis labels

ggsave(filename = "results/04_heatmap.png", width = 16, height = 9, dpi = 72)

  
>>>>>>> af4cfd13dce15579345b1fb2776e4bcc8c39804c
