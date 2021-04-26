# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")

# Load data ---------------------------------------------------------------
golub_clean_aug <- read_tsv(file = "data/03_golub_clean_aug.tsv.gz")

# Wrangle data ------------------------------------------------------------

# pivot longer
set.seed(12345)
golub_data_long <- golub_clean_aug %>% 
  pivot_longer(cols = c(-type,-value), 
               names_to = "gene", 
               values_to = "expr_level") %>% 
  mutate(norm_expr_level = (expr_level - mean(expr_level))/sd(expr_level)) +
  sample_n(100)

# nesting
set.seed(12345)
golub_data_long_nested <- golub_data_long %>% 
  group_by(gene) %>% 
  nest() %>% 
  ungroup() 

# sample 100 genes randomly
set.seed(12345)
golub_data_long_nested <- 
  golub_data_long_nested %>% 
  sample_n(100)

'''
# Making the dataset with 100 genes wide 
golub_data_wide = golub_clean_aug %>%
  select(type, pull(golub_data_long_nested, gene))
'''


# Visualise data ----------------------------------------------------------
golub_data_long %>% 
  ggplot(golub_data_long, 
         mapping = aes(x = value,y = gene, fill = norm_expr_level)) +
  geom_tile() +
  xlab(label = "Type") + 
  theme(axis.title.y = element_blank(), # Remove the y-axis title
        axis.text.x = element_text(angle = 45, vjust = 0.5)) # Rotate the x-axis labels



#ggsave(filename = "results/04_heatmap.png", width = 16, height = 9, dpi = 72)

  