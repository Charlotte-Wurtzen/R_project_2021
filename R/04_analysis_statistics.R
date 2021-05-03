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
  mutate(identified_as = case_when(p.value < 0.01 ~ "Significant",
                                   TRUE ~ "Non-significant"),
         gene_label = case_when(identified_as == "Significant" ~ gene,
                                identified_as == "Non-significant" ~ ""))

# Visualization ------------------------------------------------------------

# CI plot for significant genes
significant_genes = golub_expr_data_long_nested %>% 
  filter(identified_as == "Significant")

# top genes
top_genes = significant_genes %>% 
  arrange(p.value) %>% 
  head(n=71L)

top_genes = top_genes %>% 
  unnest(data) %>% 
  select(c(gene, type, norm_expr_level))

# separate top genes based on cancer type 
ALL <- top_genes %>% 
  pivot_wider(names_from = "gene", values_from = "norm_expr_level") %>% 
  unnest() %>% 
  filter(type == 0) %>% 
  select(-c(type)) 

avg_ALL <- ALL %>% 
  colMeans() %>% 
  as_tibble() %>% 
  mutate(gene_names = colnames(ALL)) %>% 
  arrange(desc(value))

AML <- top_genes %>% 
  pivot_wider(names_from = "gene", values_from = "norm_expr_level") %>% 
  unnest() %>% 
  filter(type == 1) %>% 
  select(-c(type))

avg_AML <- AML %>% 
  colMeans() %>% 
  as_tibble() %>% 
  mutate(gene_names = colnames(AML)) %>% 
  arrange(desc(value))


# Write data --------------------------------------------------------------
write_tsv(x = top_genes, file = "data/04_top_genes.tsv.gz")

write_tsv(x = avg_ALL, file = "data/04_avg_ALL.tsv.gz")

write_tsv(x = avg_AML, file = "data/04_avg_AML.tsv.gz")

#ggsave("results/06_CI_plot.png", plot = CI_plot)

#ggsave("results/06_significant_plot.png", plot = significant_plot)


#--------

# Confidence interval plot
#CI_plot <- golub_expr_data_long_nested %>% 
#  ggplot(aes(x = estimate,
#             y = fct_reorder(gene, desc(estimate)),
#             colour = identified_as,
#             label = gene_label)) +
#  geom_vline(xintercept = 0,
#             linetype = "dashed") +
#  geom_point(alpha = 0.5) +
#  geom_errorbarh(aes(xmin = conf.low,
#                     xmax = conf.high,
#                     height = 0.2)) +
#  geom_text(aes(x = conf.high),
#            size = 2,
#            colour = "black",
#            nudge_x = 0.5) +
#  theme_classic(base_family = "Avenir",
#                base_size = 8) +
#  theme(axis.text.y = element_blank(),
#        legend.position = "bottom") +
#  labs(y = "") 


#significant_plot <- significant_genes %>% 
#  ggplot(aes(x = estimate,
#             y = fct_reorder(gene, desc(estimate)),
#             label = gene_label)) +
#  geom_vline(xintercept = 0,
#             linetype = "dashed") +
#  geom_point(alpha = 0.5) +
#  geom_errorbarh(aes(xmin = conf.low,
#                     xmax = conf.high,
#                     height = 0.2)) +
#  geom_text(aes(x = conf.high),
#            size = 2,
#            colour = "black",
#            nudge_x = 0.5) +
#  theme_classic(base_family = "Avenir",
#                base_size = 8) +
#  theme(axis.text.y = element_blank(),
#        legend.position = "bottom") +
#  labs(y = "") 


