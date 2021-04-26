# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("cowplot")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
golub_clean_aug <- read_tsv(file = "data/03_golub_clean_aug.tsv.gz")


# Wrangle data ------------------------------------------------------------
golub_data_long <- golub_clean_aug %>% 
  pivot_longer(cols = -c(type, value), 
               names_to = "gene", 
               values_to = "expr_level") %>% 
  mutate(norm_expr_level = (expr_level - mean(expr_level))/sd(expr_level)) %>% 
  select(-c(type, expr_level))

# Model data
golub_data_long_nested <- golub_data_long %>% 
  group_by(gene) %>% 
  nest() %>% 
  ungroup()

set.seed(12345)
golub_data_long_nested <- golub_data_long_nested %>% 
  sample_n(100)

golub_data_long_nested <- golub_data_long_nested %>% 
  mutate(mdl = map(data, ~glm(type ~ norm_expr_level,
                              data = .x,
                              family = binomial(link = "logit"))))

# Visualise data ----------------------------------------------------------
golub_data_wide <- golub_clean_aug %>% 
  select(type, pull(golub_data_long_nested, gene))

pca_fit <- golub_data_wide %>% 
  select(where(is.numeric)) %>% 
  prcomp(scale = TRUE)

plot1 <- pca_fit %>% 
  augment(golub_data_wide) %>% 
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
  ggplot(aes(PC1, PC2)) + 
  geom_segment(xend = 0, 
               yend = 0, 
               arrow = arrow_style) +
  geom_text(aes(label = column),
            hjust = 1,
            nudge_x = -0.02,
            color = "#904C2F") +
  xlim(-0.3, 0.3) + ylim(-0.3, 0.2) + 
  coord_fixed() +
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

# Write data --------------------------------------------------------------
ggsave("results/05_plot1.png",
       plot = plot1)

ggsave("results/05_plot2.png",
       plot = plot2)

ggsave("results/05_plot3.png",
       plot = plot3)