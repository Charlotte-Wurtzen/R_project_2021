# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Load data ---------------------------------------------------------------
golub_clean_aug <- read_tsv(file = "data/03_golub_clean_aug.tsv.gz")
top_genes <- read_tsv(file = "data/04_top_genes.tsv.gz")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Wrangle data ------------------------------------------------------------

# Define top 25 significant genes
top_25_genes <- top_genes %>% 
  groupnest(gene) %>% 
  head(n = 25L) %>% 
  select(gene)
  
# Average of top gene data
top_genes_avg <- top_genes %>% 
  group_by(gene) %>% 
  summarise(avg_norm_expr_level = mean(norm_expr_level))

# Separate top gene data on cancer type and find average
avg_ALL_top <- top_genes %>% 
  filter(type == 0) %>% 
  select(-c(type, id)) %>% 
  group_by(gene) %>% 
  summarise(avg_norm_expr_level = mean(norm_expr_level)) 

avg_AML_top <- top_genes %>% 
  filter(type == 1) %>% 
  select(-c(type, id)) %>% 
  group_by(gene) %>% 
  summarise(avg_norm_expr_level = mean(norm_expr_level))

# Take out the gene name with the highest expression level
top1_avg_ALL <- avg_ALL_top %>% 
  arrange(desc(avg_norm_expr_level)) %>%
  head(n=1L) %>% 
  pull(gene)

top1_avg_AML <- avg_AML_top %>% 
  arrange(desc(avg_norm_expr_level)) %>%
  head(n=1L) %>% 
  pull(gene)


# Visualisations ----------------------------------------------------------------

# Histogram showing how many measurements of each type 
bar_count <- golub_clean_aug %>%
  mutate(type = case_when(type == 0 ~ "ALL",
                          type == 1 ~ "AML")) %>% 
  count(type) %>% 
  ggplot(aes(x = type, 
             y = n, 
             fill = factor(type))) +
  geom_col(alpha = 0.8) +
  labs(title = "Number of patients with each cancer type",
       caption = "Data from Golub et al. (1999)",
       fill = "Cancer type") +
  ylab(label= "Number of patients") +
  theme_minimal_grid(12) +
  theme(legend.position = "right",
        axis.title.x = element_blank()) +   
  scale_x_discrete(breaks = NULL)

# Bar plot showing expression of top genes divided into type
bar_plot <- avg_ALL_top %>% 
  right_join(avg_AML_top, 
             by = "gene") %>%
  rename(ALL = avg_norm_expr_level.x,
         AML = avg_norm_expr_level.y) %>% 
  filter(gene %in% pull(top_25_genes)) %>% 
  
  pivot_longer(cols = -gene,
               names_to = "Type",
               values_to = "avg_expr") %>% 
  
  ggplot(mapping = aes(x = avg_expr, 
                       y = gene, 
                       fill = Type)) +
  geom_col(position = "dodge") +
  theme_bw(base_size = 8) + 
  labs(title = "Average gene expression according to cancer type",
       subtitle = "25 most significant genes.",
       caption = "Data from Golub et al. (1999)") + 
  xlab(label = "Average gene expression") + 
  ylab(label = "Genes")


# Box plots of top average genes
boxplot_ALL_topgene <- top_genes %>% 
  filter(gene == top1_avg_ALL) %>% 
  mutate(type = case_when(type == 0 ~ "ALL",
                          type == 1 ~ "AML")) %>% 
  ggplot(mapping = aes(y = norm_expr_level, fill = factor(type))) +
  geom_boxplot(alpha=0.5) +
  facet_wrap(vars(type), 
             strip.position = "bottom", 
             scales = "free_x") +
  labs(title = str_c("Boxplots of expression levels of gene ", top1_avg_ALL),
       caption = "Data from Golub et al. (1999)",
       subtitle = str_c("The chosen gene ",
                        top1_avg_ALL, 
                        " has the highest average expression for ALL.")) +
  ylab(label = "Normalized expression level") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none")

boxplot_AML_topgene <- top_genes %>% 
  filter(gene == top1_avg_AML) %>% 
  mutate(type = case_when(type == 0 ~ "ALL",
                          type == 1 ~ "AML")) %>% 
  ggplot(mapping = aes(y = norm_expr_level, 
                       fill = factor(type))) +
  geom_boxplot(alpha=0.5) +
  facet_wrap(vars(type), 
             strip.position = "bottom", 
             scales = "free_x") +
  labs(title = str_c("Boxplots of expression levels of gene ", top1_avg_AML),
       caption = "Data from Golub et al. (1999)",
       subtitle = str_c("The chosen gene ", 
                        top1_avg_AML, 
                        " has the highest average expression for AML.")) +
  ylab(label = "Normalized expression level") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none")


# Write data --------------------------------------------------------------
ggsave("results/07_barcount.png", 
       plot = bar_count)

ggsave("results/07_barplot.png", 
       plot = bar_plot)

ggsave("results/07_boxplot_ALL.png", 
       plot = boxplot_ALL_topgene)

ggsave("results/07_boxplot_AML.png", 
       plot = boxplot_AML_topgene)