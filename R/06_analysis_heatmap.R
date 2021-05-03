# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
golub_top_genes <- read_tsv(file = "data/04_top_genes.tsv.gz")


# Wrangle data ------------------------------------------------------------

# pivot wider and add patient id
golub_top_genes_wide <- golub_top_genes %>% 
  pivot_wider(names_from = gene,values_from = norm_expr_level) %>% 
  unnest() %>% 
  mutate(id = row_number()) %>% 
  relocate(id)

# pivot longer
golub_top_genes_long <- golub_top_genes_wide %>% 
  pivot_longer(cols = -c(id,type),
               names_to = "gene", 
               values_to = "norm_expr_level") 
  


# Visualise data ----------------------------------------------------------
golub_top_genes_long %>% 
  
  ggplot(mapping = aes(x = id, y = gene, fill = norm_expr_level)) +
  geom_tile() +
  
  theme_classic(base_size = 12) +
  ggtitle("Normalized gene expression levels distinguishing ALL and AML") + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",midpoint = 2) +
  xlab(label = "Patient id") + 
  ylab(label = "Genes") + 
  labs(fill = "Normalized expression level") +
  theme(legend.position="bottom") +
  
  # facet_grid makes two panels, one for ALL, one for AML:
  facet_grid(~ type, switch = "x", scales = "free_x", space = "free_x")  
  #theme(axis.text.x = element_blank()) 


# save plot ----------------------------------------------------------
ggsave(filename = "results/06_heatmap.png", width = 16, height = 9, dpi = 72)





