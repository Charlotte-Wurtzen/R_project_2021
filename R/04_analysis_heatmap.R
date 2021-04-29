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
golub_data_long %>% 
  arrange(desc(norm_expr_level)) %>% 
  
  ggplot(mapping = aes(x = type,y = gene, fill = norm_expr_level)) +
  geom_tile() +
  
  theme_classic(base_size = 12) +
  ggtitle("Normalized gene expression levels distinguishing ALL and AML") + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",midpoint = 2) +
  xlab(label = "Type") + 
  ylab(label = "Genes") + 
  labs(fill = "Normalized expression level") +
  theme(legend.position="bottom") +
  
  # facet_grid makes two panels, one for ALL, one for AML:
  facet_grid(~ type, switch = "x", scales = "free_x", space = "free_x") + 
  theme(axis.text.x = element_blank()) 

ggsave(filename = "results/04_heatmap.png", width = 16, height = 9, dpi = 72)