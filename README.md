# Identification of important genes in leukemia patients

## Data set
The data set used for this project is a Leukemia data set called Golub (1999) found in John Ramey’s datamicroarray Github repository. 
The data can be found in the following link:
 https://github.com/ramhiser/datamicroarray/blob/master/data/golub.RData.
In this data set, there are a total of 72 leukemia patients that can be divided into 2 classes based on the type of leukemia; acute lymphoblastic leukemia (ALL) and acute myeloid leukemia (AML). 
47 of the patients are diagnosed with ALL and 25 patients are diagnosed with AML. 
All patients were assayed for 7129 gene expression levels using the Affymetrix Hgu6800 chips. 
Each gene is an attribute in the data set making the dimensions 72x7129. 

The data set has no missing values.

## Motivation
Using the Golub (1999) data, we want to investigate the different genes expressed in leukemia patients. 
We also want to take a look at which genes are upregulated in leukemia patients and whether the same genes are involved in the different leukemia types. 
By statistical evaluation, we will examine whether there is a significant difference in gene expression of key genes between ALL and AML. 

## Code style
The language used in this project is R and the analysis is carried out using functions from the Tidyverse-package.

## Content
In the repository of this project you can find a 'data', 'R', 'results' and 'doc'-folder containing the materials, methods and results used and found from this project.

We have in the 'doc'-folder prepared a presentation of the project in a ioslide format.

## Analysis procedure
In the 'R'-folder you find the scripts used to carry out the analysis. 
To begin with, tidying and data wrangling is achieved.
We decided based on the large range of expression levels to standardize the gene expression by subtracting the mean and dividing by the standard deviation.
After data transformation, modeling in form of logistic regression, PCA and K-means clustering was attempted. PCA and K-means were however not appropriate for this data set. 
We have furthermore found the most significant genes chosen by lowest p-value. We decided to go with the top 1% equal to 71 genes.
To visualize our data, we have created plots of these most significant genes by making bar charts, heatmap and boxplots. 

The entire project analysis can be carried out by running the 00_doit.R script. 

## Dead-ends
Through this project we carried out the supervised learning techniques; principal component analysis (PCA) and K-means clustering analysis. 
These methods were however not suitable for this data set. 

## Authors
The people that contributed to this project are Charlotte Würtzen s174564, Emma Ahrensbach Rørbeck s173733, Julie Maria Johansen s174595, Simone Majken Stegenborg-Grathwohl s174596.
