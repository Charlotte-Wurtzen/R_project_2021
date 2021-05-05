# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library("tidymodels")
library("patchwork")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------

# Wrangle data ------------------------------------------------------------

# Kmeans analysis ------------------------------------------------------------

pca_aug <- pca_fit %>% 
  augment(golub_clean_aug)

k_type <- pca_aug %>%
  select(contains("_at"),contains("_st")) %>%
  kmeans(centers = 2)
k_type

pca_aug_k_type <- k_type %>%
  augment(pca_aug) %>% 
  rename(cluster_type = .cluster)
pca_aug_k_type

k_pca <- pca_aug_k_type %>%
  select(.fittedPC1, .fittedPC2) %>%
  kmeans(centers = 2)
k_pca

pca_aug_k_type_pca <- k_pca %>%
  augment(pca_aug_k_type) %>% 
  rename(cluster_pca = .cluster)
pca_aug_k_type_pca

# plot type + clustering based on type and pca 
p1 <- pca_aug %>% 
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = factor(type))) +
  geom_point() +
  theme(legend.position = "bottom")

p2 <- pca_aug_k_type_pca %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = cluster_type)) +
  geom_point() +
  theme(legend.position = "bottom")

p3 <- pca_aug_k_type_pca %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = cluster_pca)) +
  geom_point() +
  theme(legend.position = "bottom")

p1 + p2 + p3
