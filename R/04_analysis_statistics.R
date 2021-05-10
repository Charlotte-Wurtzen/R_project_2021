# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
golub_clean_aug <- read_tsv(file = "data/03_golub_clean_aug.tsv.gz")


# Wrangle data ------------------------------------------------------------

# Pivot longer 
golub_long <- golub_clean_aug %>% 
  pivot_longer(cols = -c(type, 
                         value, 
                         id),
               names_to = "gene", 
               values_to = "expr_level") %>% 
  mutate(norm_expr_level = (expr_level - mean(expr_level))/sd(expr_level)) %>% 
  select(-c(value, 
            expr_level))

# Grouping and nesting 
golub_long_nested <- groupnest(golub_long, 
                               gene)


# Modeling ------------------------------------------------------------

# Fit logistic model on gene
golub_model <- golub_long_nested %>%
  mutate(mdl = map(data, ~glm(type ~ norm_expr_level,
                              data = .x,
                              family = binomial(link = "logit"))))

# Add model information
golub_model <- golub_model %>%
  mutate(mdl_tidy = map(mdl, ~tidy(.x,
                                   conf.int = TRUE))) %>% 
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
  select(c(id, 
           gene, 
           type, 
           norm_expr_level))


# Write data --------------------------------------------------------------
write_tsv(x = top_genes, 
          file = "data/04_top_genes.tsv.gz")