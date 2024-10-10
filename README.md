# Comorbidity Networks From Population-Wide Health Data: Aggregated Data of 8.9M Hospital Patients (1997-2014)
Population-Wide Medical Claims and Comorbidity Networks: Aggregated Data of 8.9M Patients (1997-2014)

This repository contains aggragated data, comoribity networks and scripts, as detailed in our paper published in [x](x). This data has the potential to enable researchers to address a wealth of research questions.

## Table of Contents

- [Overview](#overview)
- [Data](#overview)
- [Scripts](#scripts)
- [Notes](#notes)
- [Usage](#usage)
- [Contact](#contact)

## Overview

This project aims to provide a comprehensive dataset for a wide range of comorbidity networks to foster further research in this direction.
By utilizing medical claims databases from all Austrian hospitals, these networks reflect information about 45,000,000 hospital stays and their interrelations in the Austrian population (N=8,996,916). The underlying database, maintained by the Austrian Ministry of Health, includes data on patients' age and gender, primary and secondary diagnoses, entry and release dates, release type, hospital region, and patient's residential region. It covers the years 1997 through 2014. Level-3 ICD-10 codes are used to represent primary and secondary diagnoses. 

![Project Overview](https://github.com/elmadervic/Comorbidity-Networks-From-Population-Wide-Health-Data/blob/main/DataNet.png?raw=true)

Here, we used this vast dataset to construct different types of comorbidity networks, for instance, networks of ICD10 diagnoses, ICD blocks, and chronic conditions blocks for different sex, age groups, and time periods. All these networks are visualized via an online web application, which is publicly available [LINK](https://vis.csh.ac.at/comorbidity_networks/). 


![FlowChart](https://github.com/elmadervic/sComorbidity-Networks-From-Population-Wide-Health-Data/blob/main/Plots/FlowChart.png?raw=true)

## Data

Due to GitHub's storage limitations, larger files related to this project are available on FigShare for easy access. 

### 1.Prevalence
Info on prevalence of ICD10 3D codes per sex, age group, year [Data/Prevalence](Data/1.Prevalence). 

### 2.Contingency Tables

You can find info at the following link: [Data/ContingencyTables](Data/2.ContingencyTables). 

### 3.Adjacency Matrices
Info on the Adjacency Matrices are listed in the subfolder [Data/Adjacency Matrices](Data/3.AdjacencyMatrices). 

### 4.Graphs - gexf files
Info on the Gephi file, use in the [visualisation web application](https://vis.csh.ac.at/comorbidity_networks/) are listed in the subfolder [Data/Graphs](Data/4.Graphs-gexffiles). 
For instance:
![Example](https://github.com/elmadervic/BreadcrumbsComorbidity-Networks-From-Population-Wide-Health-Data/blob/main/Plots/Graphs/Graph_Female_ICD_Age_5.png?raw=true)



## Scripts

The most useful scripts for general purposes are:

1. `1_Make_AdjMatrix_Blocks.R`, `1_Make_AdjMatrix_Chronic.R`, `1_Make_AdjMatrix_ICD.R`, 
2. `2_Make_NET_Blocks.R`, `2_Make_NET_Cheonic.R`, `2_Make_NET_ICD.R`, 

### Purpose of the Scripts

- **1_Make_AdjMatrix_Blocks.R** and **1_Make_AdjMatrix_Chronic.R** and **1_Make_AdjMatrix_ICD.R**:
  - These scripts are used to make adjency matrix based on contigency tables (for example Data/3.AdjacencyMatrices/Adj_Matrix_Female_Blocks_age_1.csv or Data/3.AdjacencyMatrices/Adj_Matrix_Female_Blocks_age_1.csv.rds). They require files: Data/2.ContingencyTables/Blocks_ContingencyTables_Female_Final.rds and
    Data/2.ContingencyTables/Blocks_ContingencyTables_Male_Final.rds  as input.
  
- **2_Make_NET_Blocks.R**, **2_Make_NET_Chronic.R** and **2_Make_NET_ICD.R** :
  - These scripts generate GEXF files of the networks.
 
-  **Network_properties.R** :
  - check some network properties.

### Other Scripts

- ....

## Notes
Feel free to ask for additional inputs.


## Usage

1. Download Data from FigShare and add it to your local directory of this project.
2. Ensure you have the necessary R packages installed.
3. Before running the code, please ensure that your working directory is set to the root directory of this project.
   You can do this by executing the following command in your R script or console:

```r
setwd("path/to/this/project/directory")
```

For example
```r
setwd("/Users/ed/Documents/Github/Comorbidity-Networks-From-Population-Wide-Health-Data")
```

3. Run the scripts:

```r
# Running LMF_3_filtered_Male.R
source("Scritps/1_Make_AdjMatrix_Blocks.R")

# Running LMF_3_filtered_Female.R
source("Scripts/2_Make_NET_Blocks.R")
```



## Contact

For any questions, feedback, or further information regarding this project, please feel free to reach out:

- **Primary Contact:**
  - **Name:** Elma H Dervic
  - **Email:** [dervic@csh.ac.at](mailto:dervic@csh.ac.at)
  - [WEBPage](https://elmadervic.bio) 

- **Social Media:**
  - [Twitter](https://x.com/ElmaDervicMe) 
  - [LinkedIn](https://www.linkedin.com/in/elmahot/)



