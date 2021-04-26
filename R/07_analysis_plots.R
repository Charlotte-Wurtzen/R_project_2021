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
               values_to = "expr_level") %>% 
              mutate(norm_expr_level = (expr_level - mean(expr_level))/sd(expr_level) )
# Histogram ----------------------------------------------------------------
golub_data_long %>% 
  count(value) %>% 
  ggplot(aes(x= value, y=n, fill= value))+
  geom_col()

# Scatter plot ------------------------------------------------------------
ggplot(data = golub_data_long, mapping = aes(x = str_extract(gene,"5_at"), y = value, colour = norm_expr_level)) +
  geom_point() + 
  scale_color_gradient(low="blue", high="red")+
  theme_minimal()+
  ylab(label="?") +
  xlab(label="?") +
  labs(title = "?", caption = "Group 7")


