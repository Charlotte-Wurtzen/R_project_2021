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
The analysis is carried out through the Tidyverse-package.

## Analysis procedure
In the 'R'-folder you find the scripts used to carry out the analysis. 
To begin with, tidying and data wrangling is achieved from the first 3 scripts. 
In the 01_load.R file, the data set is loaded and x and y is extracted as tibbles from this file. 
The 02_clean.R script merges the x and y into a single tibble. 
In the 03_augment.R script, a binarization of the cancer types is made following addition of a patient ID column.
Secondly, we did modeling on the analysis-ready data obtained from the first 3 scripts. 
In the 04_analysis_statistics.R file, we found 

Lastly, we have prepared a presentation on the project in the 'doc'-folder in a ioslide format.

You can run the entire analysis through the 00_doit.R script. 

## Dead-ends
Through this project we carried out the supervised learning techniques; principal component analysis (PCA) and K-means clustering analysis. 
These methods were however not suitable for this data set. 
