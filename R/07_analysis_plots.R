# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("patchwork")

# Load data ---------------------------------------------------------------
golub_clean_aug <- read_tsv(file = "data/03_golub_clean_aug.tsv.gz")
top_genes <- read_tsv(file = "data/06_top_genes.tsv.gz")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Wrangle data ------------------------------------------------------------
# pivot longer 
golub_data_long <- longer(golub_clean_aug)

# separate top gene data on cancer type in the 
ALL_top <- top_genes%>% 
  filter(type == 0) %>% 
  select(-c(type)) 

avg_ALL_top <- ALL_top %>% 
  group_by(gene) %>% 
  summarise(avg_norm_expr_level = mean(norm_expr_level))

AML_top <- top_genes %>% 
  filter(type == 1) %>% 
  select(-c(type))

avg_AML_top <- AML_top %>% 
  group_by(gene) %>% 
  summarise(avg_norm_expr_level = mean(norm_expr_level))


# Histograms ----------------------------------------------------------------
# histogram showing how many measurements of each type 
# add some legends etc here
histogram1 <- 
  golub_data_long %>% 
  count(type) %>% 
  ggplot(aes(x= type, y=n, fill= type))+
  geom_col()


# histogram showing expression of top genes divided into type
# add nice colours
g1 <- avg_ALL_top %>%
  ggplot(mapping = aes(x =avg_norm_expr_level, y = gene)) + 
  geom_bar(stat = "identity") + 
  theme_bw(base_size = 8) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(title = "ALL") + 
  xlab(NULL) + 
  ylab(label="Genes") 


g2 <- avg_AML_top %>%
  ggplot(mapping = aes(x = avg_norm_expr_level, y = gene)) + 
  geom_bar(stat = "identity") + 
  theme_bw(base_size = 8) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(title = "AML") + 
  xlab("Gene expression level on Sample Data") + 
  ylab(NULL) 
 

histogram2 <- g1 + g2 

# Box plot ------------------------------------------------------------
# this need to be fixed
boxplot_ALL <- golub_data_long %>%
  ggplot(mapping = aes(x = pluck(type==0), y = norm_expr_level , fill=type)) +
  geom_boxplot(alpha = 0.5) +
  theme_classic()

boxplot_AML <- golub_data_long %>%
  ggplot(mapping = aes(x = pluck(type==1), y = norm_expr_level , fill=type)) +
  geom_boxplot(alpha = 0.5) +
  theme_classic()

boxplot_ALL/boxplot_AML

# Scatter plot ------------------------------------------------------------
scatter_plot <- 
  ggplot(data = top_genes, mapping = aes(x =gene, y = norm_expr_level, colour = norm_expr_level)) +
  geom_point() + 
  scale_color_gradient(low="blue", high="red")+
  theme_minimal()+
  ylab(label="Normalized Expression Level on Sample Data") +
  xlab(label="Gene") +
  labs(title = "?", caption = "Group 7")+
theme(legend.position = 'bottom', axis.text.x = element_text(angle = 45, hjust=1))

# Write data --------------------------------------------------------------
ggsave("results/07_histogram1.png", plot = histogram1)
ggsave("results/07_histogram2.png", plot = histogram2)
ggsave("results/07_scatter_plot.png", plot = scatter_plot)

