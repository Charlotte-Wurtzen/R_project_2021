# Run all scripts
source(file = "R/01_load.R")
source(file = "R/02_clean.R")
source(file = "R/03_augment.R")
source(file = "R/04_analysis_statistics.R")
source(file = "R/05_analysis_PCA.R")
source(file = "R/06_analysis_heatmap.R")
source(file = "R/07_analysis_plots.R")

rmarkdown::render("/cloud/project/doc/presentation.Rmd")