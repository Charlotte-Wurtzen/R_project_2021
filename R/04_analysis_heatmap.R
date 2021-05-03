# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
#golub_clean_aug <- read_tsv(file = "data/03_golub_clean_aug.tsv.gz")
golub_top_genes <- read_tsv(file = "data/06_top_genes.tsv.gz")

# Wrangle data ------------------------------------------------------------
golub_top_genes_wide <- golub_top_genes %>% 
  pivot_wider(id_cols = NULL,names_from = gene,values_from = norm_expr_level)

#range(1,71)
#rank(x, ties.method = "first")


# Visualise data ----------------------------------------------------------
golub_top_genes %>% 
  
  ggplot(mapping = aes(x = rownames(golub_top_genes), y = gene, fill = norm_expr_level)) +
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





