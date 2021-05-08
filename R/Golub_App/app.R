# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("shiny")
library("shinythemes")


# Define functions --------------------------------------------------------
source(file = "/cloud/project/R/99_project_functions.R")


# Load data ---------------------------------------------------------------
top_genes <- read_tsv(file = "/cloud/project/data/04_top_genes.tsv.gz")


# Wrangle data ------------------------------------------------------------
# Extract most significant genes
top_gene_names = top_genes %>% 
    groupnest(gene) %>% 
    select(-data)

plot_options = c("Boxplot", "Histogram", "Scatterplot")


# APP: UI----- ------------------------------------------------------------
ui <- fluidPage(theme = shinytheme("cerulean"),
    titlePanel("The Golub Data Set: Most Significant Leukemia Genes"),
    sidebarLayout(
        sidebarPanel(
            selectInput("gene", "Choose your desired gene", top_gene_names),
            radioButtons("plotChoice", "Choose your desired plot", plot_options)),
        mainPanel("Statistics of your desired gene",
            tableOutput("stat")
        )
    ),
    plotOutput("plot")
)


# APP: Server -------------------------------------------------------------
server <- function(input, output) {
    data <- reactive(top_genes %>% 
                         filter(gene == input$gene) %>% 
                         mutate(type = case_when(type == 0 ~ "ALL",
                                                 type == 1 ~ "AML")))
    
    plotChoice <- reactive(input$plotChoice)
    
    output$plot <- renderPlot({
        if (plotChoice() == "Boxplot"){
            ggplot(data(), mapping = aes(y = norm_expr_level, fill = factor(type))) +
                geom_boxplot(alpha=0.5) +
                facet_wrap(vars(type), 
                           strip.position = "bottom", 
                           scales = "free_x")+
                labs(title = str_c("Boxplots of expression levels of gene", input$gene, sep = " "),
                     caption = "Data from Golub et al. (1999)") +
                ylab(label = "Normalized expression level") +
                theme_bw() +
                theme(axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      legend.position = "none")
            
        } else if (plotChoice() == "Histogram"){
            ggplot(data(), mapping = aes(x = id, y = norm_expr_level, fill = type)) +
                geom_col(position = "dodge", width = 0.7) +
                theme_bw() +
                labs(title = str_c("Normalized gene expression of ", input$gene, " according to cancer type"),
                     caption = "Data from Golub et al. (1999)",
                     fill = "Cancer type") + 
                xlab(label = "Patient id") + 
                ylab(label = "Normalized expression level")
            
        } else if (plotChoice() == "Scatterplot"){
            ggplot(data(), mapping = aes(x = id, 
                                 y = norm_expr_level, 
                                 colour = norm_expr_level)) +
                geom_point() + 
                scale_color_gradient(low = "blue", high = "red") +
                theme_minimal() +
                ylab(label = "Normalized Expression Level") +
                xlab(label = "Patient id") +
                labs(title = str_c("Normalized expression level shown for ", input$gene), 
                     caption = "Data from Golub et al. (1999)",
                     color = "Expression") +
                theme_bw() +
                theme(legend.position = 'bottom', 
                      axis.text.x = element_text(angle = 45, hjust=1))
        }
    })
    
    output$stat <- renderTable({
        data() %>% 
            rename(Type = type) %>% 
            group_by(Type) %>% 
            summarise(Mean = mean(norm_expr_level),
                      Q25 = quantile(norm_expr_level, 0.25),
                      Median = median(norm_expr_level),
                      Q75 = quantile(norm_expr_level, 0.75),
                      SD = sd(norm_expr_level),
                      Min = min(norm_expr_level),
                      Max = max(norm_expr_level))
    })
}


# Run the Application -----------------------------------------------------
shinyApp(ui = ui, server = server)
