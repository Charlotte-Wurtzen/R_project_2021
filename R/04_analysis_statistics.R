# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
#library("broom")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
golub_clean_aug <- read_tsv(file = "data/03_golub_clean_aug.tsv.gz")


# Wrangle data ------------------------------------------------------------

# Pivot longer 
golub_long <- longer(golub_clean_aug)

# Grouping and nesting 
golub_long_nested <- groupnest(golub_long, gene)


# Modeling ------------------------------------------------------------

# Fit logistic model on gene
golub_model <- golub_long_nested %>%
  mutate(mdl = map(data, ~glm(type ~ norm_expr_level,
                              data = .x,
                              family = binomial(link = "logit"))))

# Add model information
golub_model <- golub_model %>%
  mutate(mdl_tidy = map(mdl, ~tidy(.x, conf.int = TRUE))) %>% 
  unnest(mdl_tidy)

# Look at slope only (remove intercept rows)
golub_model <- golub_model %>% 
  filter(str_detect(term, "level")) %>% 
  select(-term)

# Significance of p < alpha
alpha <- 0.01

golub_model <- golub_model %>% 
  mutate(identified_as = case_when(p.value < alpha ~ "Significant",
                                   p.value >= alpha ~ "Non-significant"))


# Statistics ------------------------------------------------------------

# Extract significant genes
significant_genes <- golub_model %>% 
  filter(identified_as == "Significant")

# Top genes
top_genes <- significant_genes %>% 
  arrange(p.value) %>% 
  head(n = 71L) %>% 
  unnest(data) %>% 
  select(c(id, gene, type, norm_expr_level))

# Separate top genes based on cancer type 
ALL <- top_genes %>% 
  pivot_wider(names_from = "gene", values_from = "norm_expr_level") %>% 
  filter(type == 0) %>% 
  select(-c(type, id)) 

avg_ALL <- ALL %>% 
  colMeans() %>% 
  as_tibble() %>% 
  mutate(gene_names = colnames(ALL)) %>% 
  arrange(desc(value))

AML <- top_genes %>% 
  pivot_wider(names_from = "gene", values_from = "norm_expr_level") %>% 
  filter(type == 1) %>% 
  select(-c(type, id))

avg_AML <- AML %>% 
  colMeans() %>% 
  as_tibble() %>% 
  mutate(gene_names = colnames(AML)) %>% 
  arrange(desc(value))


# Write data --------------------------------------------------------------
write_tsv(x = top_genes, file = "data/04_top_genes.tsv.gz")

write_tsv(x = avg_ALL, file = "data/04_avg_ALL.tsv.gz")

write_tsv(x = avg_AML, file = "data/04_avg_AML.tsv.gz")