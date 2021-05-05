# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("cowplot")
library("patchwork")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
golub_clean_aug <- read_tsv(file = "data/03_golub_clean_aug.tsv.gz")
golub_top_genes <- read_tsv(file = "data/04_top_genes.tsv.gz")


# Wrangle data ------------------------------------------------------------
# Model data
top_gene_names = golub_top_genes %>% 
  groupnest(gene) %>% 
  head(n = 15L)

pca_fit <- golub_clean_aug %>% 
  select(where(is.numeric),-type) %>% 
  prcomp(scale = TRUE)


# Visualise data ----------------------------------------------------------
plot1 <- pca_fit %>% 
  augment(golub_clean_aug) %>% 
  ggplot(aes(.fittedPC1, .fittedPC2, 
             color = factor(type))) + 
  geom_point(size = 1.5) +
  labs(color = "type") + 
  theme_half_open(12)

arrow_style <- arrow(
  angle = 20,
  ends = "first",
  type = "closed",
  length = grid::unit(8, "pt")
)

plot2 <- pca_fit %>% 
  tidy(matrix = "rotation") %>% 
  pivot_wider(names_from = "PC",
              names_prefix = "PC",
              values_from = "value") %>% 
  filter(column %in% pull(top_gene_names, gene)) %>% 
  ggplot(aes(PC1, PC2)) + 
  geom_segment(xend = 0, 
               yend = 0, 
               arrow = arrow_style) +
  geom_text(aes(label = column),
            hjust = 0.5,
            nudge_x = -0.0008,
            color = "#904C2F",
            size = 3) +
  theme_minimal_grid(12)


plot3 <- pca_fit %>% 
  tidy(matrix = "eigenvalues") %>% 
  head(.,10) %>% 
  ggplot(aes(PC, percent)) +
  geom_col(fill = "#56B4E9",
           alpha = 0.8) +
  scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01))) +
  theme_minimal_hgrid(12)

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

kmeans_plot <- p1 + p2 + p3

# Write data --------------------------------------------------------------
ggsave("results/05_PCA_plot1.png",
       plot = plot1)

ggsave("results/05_PCA_plot2.png",
       plot = plot2)

ggsave("results/05_kmeans_plot1.png",
       plot = kmeans_plot)

ggsave("results/05_PCA_plot3.png",
       plot = plot3)
