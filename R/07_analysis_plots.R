# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("patchwork")

# Load data ---------------------------------------------------------------
golub_clean_aug <- read_tsv(file = "data/03_golub_clean_aug.tsv.gz")
top_genes <- read_tsv(file = "data/04_top_genes.tsv.gz")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Wrangle data ------------------------------------------------------------
# pivot longer 
golub_data_long <- longer(golub_clean_aug)

# average of top gene data
top_genes_avg <- top_genes %>% 
  group_by(gene) %>% 
  summarise(avg_norm_expr_level = mean(norm_expr_level))


# separate top gene data on cancer type and find average
avg_ALL_top <- top_genes%>% 
  filter(type == 0) %>% 
  select(-c(type)) %>% 
  group_by(gene) %>% 
  summarise(avg_norm_expr_level = mean(norm_expr_level)) 

avg_AML_top <- top_genes %>% 
  filter(type == 1) %>% 
  select(-c(type)) %>% 
  group_by(gene) %>% 
  summarise(avg_norm_expr_level = mean(norm_expr_level))


# take out the gene name with the highest expression level
top1_avg_ALL <- avg_ALL_top %>% 
  arrange(desc(avg_norm_expr_level)) %>%
  head(n=1L) %>% 
  pull(gene)

top1_avg_AML <- avg_AML_top %>% 
  arrange(desc(avg_norm_expr_level)) %>%
  head(n=1L) %>% 
  pull(gene)


# Histograms ----------------------------------------------------------------
# histogram showing how many measurements of each type 
# add some legends etc here
histogram1 <- 
  golub_data_long %>% 
  count(type) %>% 
  ggplot(aes(x= type, y=n, fill= type))+
  geom_col()+
  xlab(label="Types ( 0 = ALL, 1 = AML )") +
  ylab(label= "Number of genes measured")


# histogram showing expression of top genes divided into type
# add nice colours
g1 <- avg_ALL_top %>%
  ggplot(mapping = aes(x =avg_norm_expr_level, y = gene)) + 
  geom_bar(stat = "identity") + 
  theme_bw(base_size = 8) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(title = "ALL") + 
  xlab(NULL) + 
  ylab(label="Genes") +
  theme_classic()


g2 <- avg_AML_top %>%
  ggplot(mapping = aes(x = avg_norm_expr_level, y = gene)) + 
  geom_bar(stat = "identity") + 
  theme_bw(base_size = 8) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(title = "AML") + 
  xlab("Gene expression level shown for top genes") + 
  ylab(NULL) +
  theme_classic()
 

histogram2 <- g1 + g2 

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

