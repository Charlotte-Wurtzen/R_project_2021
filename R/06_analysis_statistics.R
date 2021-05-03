# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")
library("dplyr")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
golub_clean_aug <- read_tsv(file = "data/03_golub_clean_aug.tsv.gz")


# Wrangle data ------------------------------------------------------------

# pivot longer 
golub_data_long <- longer(golub_clean_aug)

# grouping and nesting 
golub_data_long_nested <- groupnest(golub_data_long)


# separate data on cancer type
ALL <- golub_clean_aug %>% 
  filter(type == 0) %>% 
  select(-c(value,type)) 

avg_ALL <- ALL %>% 
  colMeans() %>% 
  as_tibble() %>% 
  mutate(gene_names = colnames(ALL)) %>% 
  arrange(desc(value))

AML <- golub_clean_aug %>% 
  filter(type == 1)

avg_AML <- AML %>% 
  colMeans() %>% 
  as_tibble() %>% 
  mutate(gene_names = colnames(AML)) %>% 
  arrange(desc(value))

'''
# sample 100 random genes
set.seed(928488)
golub_data_long_nested <- 
  golub_data_long_nested %>% 
  sample_n(100)
'''


# Statistics ------------------------------------------------------------

# fit logistic model on gene
golub_expr_data_long_nested = golub_data_long_nested %>%
  mutate(mdl = map(data, ~glm(type ~ norm_expr_level,
                              data = .x,
                              family = binomial(link = "logit"))))



# add model information
golub_expr_data_long_nested = golub_expr_data_long_nested %>%
  mutate(mdl_tidy = map(mdl, ~tidy(.x, conf.int = TRUE))) %>% 
  unnest(mdl_tidy)

# look at slope only (remove intercept row)
golub_expr_data_long_nested = golub_expr_data_long_nested %>% 
  filter(str_detect(term, "level"))

# significance of p < 0.05
golub_expr_data_long_nested = golub_expr_data_long_nested %>% 
  mutate(identified_as = case_when(p.value < 0.05 ~ "Significant",
                                   TRUE ~ "Non-significant"),
         gene_label = case_when(identified_as == "Significant" ~ gene,
                                identified_as == "Non-significant" ~ ""))

# Visualization ------------------------------------------------------------

# Confidence interval plot
CI_plot <- golub_expr_data_long_nested %>% 
  ggplot(aes(x = estimate,
             y = fct_reorder(gene, desc(estimate)),
             colour = identified_as,
             label = gene_label)) +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  geom_point(alpha = 0.5) +
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high,
                     height = 0.2)) +
  geom_text(aes(x = conf.high),
            size = 2,
            colour = "black",
            nudge_x = 0.5) +
  theme_classic(base_family = "Avenir",
                base_size = 8) +
  theme(axis.text.y = element_blank(),
        legend.position = "bottom") +
  labs(y = "") 

# CI plot for significant genes
significant_genes = golub_expr_data_long_nested %>% filter(identified_as == "Significant")

significant_plot <- significant_genes %>% 
  ggplot(aes(x = estimate,
             y = fct_reorder(gene, desc(estimate)),
             label = gene_label)) +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  geom_point(alpha = 0.5) +
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high,
                     height = 0.2)) +
  geom_text(aes(x = conf.high),
            size = 2,
            colour = "black",
            nudge_x = 0.5) +
  theme_classic(base_family = "Avenir",
                base_size = 8) +
  theme(axis.text.y = element_blank(),
        legend.position = "bottom") +
  labs(y = "") 


# Write data --------------------------------------------------------------
#write_tsv(x = golub_expr_data_long_nested, file = "data/06_golub_expr_data_long_nested.tsv.gz")

ggsave("results/06_CI_plot.png", plot = CI_plot)

ggsave("results/06_significant_plot.png", plot = significant_plot)

