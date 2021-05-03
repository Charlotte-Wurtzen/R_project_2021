# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("cowplot")


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
  select(where(is.numeric)) %>% 
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
plot2

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


# Write data --------------------------------------------------------------
ggsave("results/05_PCA_plot1.png",
       plot = plot1)

ggsave("results/05_PCA_plot2.png",
       plot = plot2)

ggsave("results/05_PCA_plot3.png",
       plot = plot3)