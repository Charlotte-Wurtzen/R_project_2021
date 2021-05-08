# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library(shiny)


# Define functions --------------------------------------------------------
source(file = "/cloud/project/R/99_project_functions.R")


# Load data ---------------------------------------------------------------
top_genes <- read_tsv(file = "/cloud/project/data/04_top_genes.tsv.gz")


# Wrangle data ------------------------------------------------------------
# Extract most significant genes
top_gene_names = top_genes %>% 
    groupnest(gene) %>% 
    select(-data)


# APP: UI----- ------------------------------------------------------------
ui <- fluidPage(
    selectInput("gene", "What's your favourite gene?", top_gene_names)#,
    #radioButtons("animal", "What's your favourite animal?", animals)
)



# APP: Server ------------------------------------------------------------
server <- function(input, output) {
    
    input$gene

    output$plot <- renderPlot({

        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
