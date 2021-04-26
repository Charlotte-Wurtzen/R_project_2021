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

# Histograms ----------------------------------------------------------------
golub_data_long %>% 
  count(type) %>% 
  ggplot(aes(x= type, y=n, fill= type))+
  geom_col()

ALL <- 
  filter(ggolub_data_long, type=0 ) %>% 
  count(Allele_F_1_2) %>% 
  mutate(perc = n / 72 * 100)

g1 <- ALL %>% 
  ggplot(mapping = aes(x = perc, y = reorder(Allele_F_1_2, perc))) + 
  geom_bar(stat = "identity") + 
  theme_bw(base_size = 8) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(title = "HLA-A") + 
  xlab(NULL) + 
  ylab(label="Allele") +
  xlim(0, 33)


AML <- 
  filter(allele_data_long, type=1) %>% 
  count(type) %>% 
  mutate(perc = n / 72 * 100)

g2 <- AML %>% 
  ggplot(mapping = aes(x = perc, y = reorder(Allele_F_1_2, perc))) + 
  geom_bar(stat = "identity") + 
  theme_bw(base_size = 8) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(title = "HLA-B") + 
  xlab("Observed Allele Frequency (within each gene) [%]") + 
  ylab(NULL) + 
  xlim(0, 33)


g1 + g2 

# Scatter plot ------------------------------------------------------------
  # sample 100 random genes
set.seed(12345)
golub_data_long_sample <- 
  golub_data_long %>% 
  sample_n(100)

ggplot(data = golub_data_long_sample, mapping = aes(x =gene, y = norm_expr_level, colour = norm_expr_level)) +
  geom_point() + 
  scale_color_gradient(low="blue", high="red")+
  theme_minimal()+
  ylab(label="Normalized Expression Level") +
  xlab(label="Gene") +
  labs(title = "?", caption = "Group 7")+
theme(legend.position = 'bottom', axis.text.x = element_text(angle = 45, hjust=1))


