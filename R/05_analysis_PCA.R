# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("cowplot")
library("patchwork")
library("broom")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
golub_clean_aug <- read_tsv(file = "data/03_golub_clean_aug.tsv.gz")
golub_top_genes <- read_tsv(file = "data/04_top_genes.tsv.gz")


# Wrangle data ------------------------------------------------------------

# Extract most significant genes
top_gene_names = golub_top_genes %>% 
  groupnest(gene) %>% 
  head(n = 10L) %>% 
  select(-data)

# Model data
pca_fit <- golub_clean_aug %>% 
  select(where(is.numeric),
         -c(type, id)) %>% 
  prcomp(scale = TRUE)

pca_aug <- pca_fit %>% 
  augment(golub_clean_aug)

# Visualise data ----------------------------------------------------------

# Scatter plot, variance for patients
plot1 <- pca_aug %>% 
  ggplot(aes(.fittedPC1, .fittedPC2, 
             color = factor(type))) + 
  geom_point(size = 1.5) +
  labs(color = "Type", 
       title = "Projection of patients unto PC1 and PC2",
       caption = "Data from Golub et al. (1999)") +
  xlab(label = "Principal Component 1") +
  ylab(label = "Principal Component 2") +
  theme_bw()

# Rotation matrix, variance for genes
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
  filter(column %in% pull(top_gene_names)) %>% 
  ggplot(aes(PC1, PC2)) + 
  geom_segment(xend = 0, 
               yend = 0, 
               arrow = arrow_style) +
  geom_text(aes(label = column),
            hjust = 0.5,
            nudge_x = -0.0009,
            color = "#904C2F",
            size = 3) +
  labs(color = "Type", 
       title = "Projection of gene expressions unto PC1 and PC2",
       caption = "Data from Golub et al. (1999)") +
  theme_bw() + 
  theme(plot.title = element_text(face = "bold"))

# Plot of variance explained
plot3 <- pca_fit %>% 
  tidy(matrix = "eigenvalues") %>% 
  head(.,10) %>% 
  ggplot(aes(PC, percent)) +
  geom_col(fill = "#56B4E9",
           alpha = 0.8) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01))) +
  labs(title = "Variance explained by the principal components",
       caption = "Data from Golub et al. (1999)") +
  ylab(label = "Percent") +
  theme_minimal_hgrid(12)


# Kmeans analysis ------------------------------------------------------------

# Kmeans clustering based on gene expressions
k_type <- golub_clean_aug %>%
  select(-c(type, 
            value, 
            id)) %>%
  kmeans(centers = 2)

pca_aug_k_type <- k_type %>%
  augment(pca_aug) %>% 
  rename(cluster_type = .cluster)


# Kmeans clustering based on principal components 
k_pca <- pca_aug_k_type %>%
  select(.fittedPC1, 
         .fittedPC2) %>%
  kmeans(centers = 2)

pca_aug_k_type_pca <- k_pca %>%
  augment(pca_aug_k_type) %>% 
  rename(cluster_pca = .cluster)


# Plot type + clustering based on type and pca 
p1 <- pca_aug %>% 
  ggplot(aes(x = .fittedPC1, 
             y = .fittedPC2, 
             colour = factor(type))) +
  geom_point() +
  ylab(label = "Principal Component 2") +
  xlab(label = "Principal Component 1") +
  labs(color = "Cancer type") + 
  theme(legend.position = "bottom")

p2 <- pca_aug_k_type_pca %>%
  ggplot(aes(x = .fittedPC1, 
             y = .fittedPC2, 
             colour = cluster_type)) +
  geom_point() +
  xlab(label = "Principal Component 1") +
  labs(color = "Gene cluster") + 
  theme(legend.position = "bottom", axis.title.y=element_blank())

p3 <- pca_aug_k_type_pca %>%
  ggplot(aes(x = .fittedPC1, 
             y = .fittedPC2, 
             colour = cluster_pca)) +
  geom_point() +
  xlab(label = "Principal Component 1") +
  labs(color = "PCA cluster") + 
  theme(legend.position = "bottom", 
        axis.title.y=element_blank())

kmeans_plot <- p1 + p2 + p3 + 
  plot_annotation(title = "K-means analysis",
                  caption = "Data from Golub et al. (1999)")

# Write data --------------------------------------------------------------
ggsave("results/05_PCA_plot1.png",
       plot = plot1,
       width = 6, 
       height = 4)

ggsave("results/05_PCA_plot2.png",
       plot = plot2,
       width = 6, 
       height = 4)

ggsave("results/05_PCA_plot3.png",
       plot = plot3,
       width = 6, 
       height = 4)

ggsave("results/05_kmeans_plot.png",
       plot = kmeans_plot,
       width = 12, 
       height = 4)