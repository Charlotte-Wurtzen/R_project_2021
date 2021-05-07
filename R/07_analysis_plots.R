# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
#library("patchwork")


# Load data ---------------------------------------------------------------
golub_clean_aug <- read_tsv(file = "data/03_golub_clean_aug.tsv.gz")
top_genes <- read_tsv(file = "data/04_top_genes.tsv.gz")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Wrangle data ------------------------------------------------------------

# Pivot longer 
golub_data_long <- longer(golub_clean_aug)

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


# Histograms ----------------------------------------------------------------

# Histogram showing how many measurements of each type 
histogram1 <- golub_clean_aug %>%
  mutate(type = case_when(type == 0 ~ "ALL",
                          type == 1 ~ "AML")) %>% 
  count(type) %>% 
  ggplot(aes(x = type, y = n, fill = factor(type))) +
  geom_col(alpha = 0.8) +
  labs(title = "Number of patients with each cancer type",
       caption = "Data from Golub et al. (1999)",
       fill = "Cancer type") +
  ylab(label= "Number of patients") +
  theme_minimal_grid(12) +
  theme(legend.position = "right",
        axis.title.x = element_blank()) +   
  scale_x_discrete(breaks = NULL)

# Histogram showing expression of top genes divided into type


avg_ALL_top %>% 
  right_join(avg_AML_top, by = "gene") %>%
  rename(ALL = avg_norm_expr_level.x,
         AML = avg_norm_expr_level.y) %>% 
  sample_n(25) %>% 
  pivot_longer(cols = -gene,
               names_to = "Type",
               values_to = "avg_expr") %>% 
  
  ggplot(mapping = aes(x = avg_expr, y = gene, fill = Type)) +
  geom_col(position = "dodge") +
  theme_bw(base_size = 8) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(title = "Average gene expression according to cancer type",
       subtitle = "25 random significant genes") + 
  xlab(label = "Average gene expression") + 
  ylab(label="Genes")


g1 <- avg_ALL_top %>%
  ggplot(mapping = aes(x =avg_norm_expr_level, y = gene)) + 
  geom_bar(stat = "identity") + 
  theme_bw(base_size = 8) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(title = "ALL") + 
  xlab(NULL) + 
  ylab(label="Genes") +
  theme_classic()
g1

g2 <- avg_AML_top %>%
  ggplot(mapping = aes(x = avg_norm_expr_level, y = gene)) + 
  geom_bar(stat = "identity") + 
  theme_bw(base_size = 8) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(title = "AML") + 
  xlab("Gene expression level shown for top genes") + 
  ylab(NULL) +
  theme_classic()
 

histogram2 <- g1 / g2 
histogram2

# Box plot ------------------------------------------------------------
# this need to be fixed
boxplot_ALL_topgene <- top_genes %>% 
  filter(gene == top1_avg_ALL) %>% 
  ggplot(mapping = aes(y = norm_expr_level, fill = factor(type))) +
  geom_boxplot(alpha=0.5) +
  facet_wrap(~type, strip.position = "bottom", scales = "free_x")+
  labs(title = "gene Y00433_at" ) +
  theme(axis.text.x=element_blank())
boxplot_ALL_topgene

boxplot_AML_topgene <- top_genes %>% 
  filter(gene == top1_avg_AML) %>% 
  ggplot(mapping = aes(y = norm_expr_level, fill = factor(type))) +
  geom_boxplot(alpha=0.5) +
  facet_wrap(~type, strip.position = "bottom", scales = "free_x") +
  labs(title = "gene M11147_at" ) +
  theme(axis.text.x=element_blank())
boxplot_AML_topgene


# Scatter plot ------------------------------------------------------------
scatter_plot <- 
  ggplot(data = top_genes_avg, mapping = aes(x =gene, y = avg_norm_expr_level, colour = avg_norm_expr_level)) +
  geom_point() + 
  scale_color_gradient(low="blue", high="red")+
  theme_minimal()+
  ylab(label="Normalized Expression Level") +
  xlab(label="Gene") +
  labs(title = "Expression value shown for each top gene", caption = "Group 7")+
theme(legend.position = 'bottom', axis.text.x = element_text(angle = 45, hjust=1))

# Write data --------------------------------------------------------------
ggsave("results/07_histogram1.png", plot = histogram1)
ggsave("results/07_histogram2.png", plot = histogram2)
ggsave("results/07_scatter_plot.png", plot = scatter_plot)

