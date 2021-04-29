# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("patchwork")

# Load data ---------------------------------------------------------------
golub_clean_aug <- read_tsv(file = "data/03_golub_clean_aug.tsv.gz")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Wrangle data ------------------------------------------------------------
# pivot longer 
golub_data_long <- longer(golub_clean_aug)

# Histograms ----------------------------------------------------------------
histogram1 <- 
  golub_data_long %>% 
  count(type) %>% 
  ggplot(aes(x= type, y=n, fill= type))+
  geom_col()

# sample 50 random genes
set.seed(12345)
golub_data_long_sample <- 
  golub_data_long %>% 
  sample_n(50)

g1 <- golub_data_long_sample %>%
  ggplot(mapping = aes(x = norm_expr_level, y = gene)) + 
  geom_bar(stat = "identity") + 
  theme_bw(base_size = 8) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(title = "ALL") + 
  xlab(NULL) + 
  ylab(label="Genes") +
  xlim(0, 5)




g2 <- golub_data_long_sample %>%
  ggplot(mapping = aes(x = norm_expr_level, y = gene)) + 
  geom_bar(stat = "identity") + 
  theme_bw(base_size = 8) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(title = "AML") + 
  xlab("Gene expression level on Sample Data") + 
  ylab(NULL) + 
  xlim(0, 5)


histogram2 <- g1 + g2 

# Box plot ------------------------------------------------------------
golub_data_long %>%
  ggplot(mapping = aes(x = pluck(type==0), y = norm_expr_level , fill=type)) +
  geom_boxplot(alpha = 0.5) +
  theme_classic()


# Scatter plot ------------------------------------------------------------

scatter_plot <- 
  ggplot(data = golub_data_long_sample, mapping = aes(x =gene, y = norm_expr_level, colour = norm_expr_level)) +
  geom_point() + 
  scale_color_gradient(low="blue", high="red")+
  theme_minimal()+
  ylab(label="Normalized Expression Level on Sample Data") +
  xlab(label="Gene") +
  labs(title = "?", caption = "Group 7")+
theme(legend.position = 'bottom', axis.text.x = element_text(angle = 45, hjust=1))

# Write data --------------------------------------------------------------
ggsave("results/07_histogram1.png", plot = histogram1)
ggsave("results/07_histogram2.png", plot = histogram2)
ggsave("results/07_scatter_plot.png", plot = scatter_plot)

