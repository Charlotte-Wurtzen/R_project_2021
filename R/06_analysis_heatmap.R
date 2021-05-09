# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
golub_top_genes <- read_tsv(file = "data/04_top_genes.tsv.gz")


# Visualise data ----------------------------------------------------------
plot1 <- golub_top_genes %>% 
  mutate(type = case_when(type == 0 ~ "ALL",
            type == 1 ~ "AML")) %>% 
  ggplot(mapping = aes(x = id, 
                       y = gene, 
                       fill = norm_expr_level)) +
  geom_tile() +
  theme_classic(base_size = 12) +
  ggtitle("Normalized gene expression levels distinguishing ALL and AML") + 
  scale_fill_gradient2(low = "blue", 
                       high = "red", 
                       mid = "white",
                       midpoint = 2) +
  xlab(label = "Patient ID") + 
  ylab(label = "Genes") + 
  labs(fill = "Normalized expression level",
       caption = "Data from Golub et al. (1999)") +
  theme(legend.position="bottom",
        plot.title = element_text(size = rel(1.7)),
        axis.title.y = element_text(size = rel(1.4)),
        axis.title.x = element_text(size = rel(1.4))) +
  facet_grid(~type, 
             switch = "x", 
             scales = "free_x", 
             space = "free_x")  


# save plot ----------------------------------------------------------
ggsave(filename = "results/06_heatmap.png", width = 16, height = 9, dpi = 72, plot = plot1)





