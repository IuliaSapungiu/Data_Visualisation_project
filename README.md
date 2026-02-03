# Uneven Air Quality Change: A Visual Analysis of PM₂.₅ Trends Across UK Cities During and After COVID-19
###### This project is developed for academic and educational purposes as part of the MSc Data Science programme. Air quality data are sourced from the UK Air Quality Archive (DEFRA) and are subject to their terms of use.
###### All rights reserved © Iulia Sapungiu

## Table of contents
 - [Project Overview](#project-overview)
 - [Research Questions](#research-questions)
 - [Technologies](#technologies)
 - [Installation and usage](#installation-and-usage)
 - [Project Structure](#project-structure)

### Project Overview
This project examines how fine particulate matter (PM₂.₅) concentrations changed across four major UK cities (London, Edinburgh, Manchester, and Sheffield) during the COVID-19 pandemic (2020–2021) and the post-pandemic period (2022–2024). Using daily air quality measurements from urban background monitoring stations, the analysis focuses on identifying temporal patterns, between-city differences, and the consistency of PM₂.₅ changes over time.

A composite visualisation approach is employed to combine temporal trends, distributional comparisons, summary statistics, and pollutant relationships. While other pollutants are explored during data preparation, PM₂.₅ is prioritised due to its well-established public health relevance, with NO₂ used selectively to contextualise potential traffic-related influences. The project aims to build visual evidence on whether pandemic-related changes in PM₂.₅ were uniform across UK cities or shaped by local urban dynamics.

### Research Questions
How did daily concentrations of PM₂.₅, PM₁₀, NO₂, and O₃ vary across major UK cities during and after the COVID-19 pandemic (2020–2024)?

### Technologies
- R version 4.0.0 or higher
- tidyverse - Data Manipulation and visualisation
- lubridate - Date handling
- ggplot2 - Plotting
- patchwork - combining multiple plots
- scales – Axis and label formatting

### Installation and usage
#### Step 1: Setup
1. Clone or download this repository _(make sure to delete plots and processed_data folders so that you create them when running the code)_
2. Place your data file ```uk_air_quality_data.csv``` in the project root directory
3. Open R or RStudio and set your working directory to the project folder ```air_quality_analysis```
4. Install and load required packages:
   
   ```
    install.packages(c("tidyverse", "lubridate", "ggplot2", "patchwork", "scales"))
   
    library(tidyverse)    
    library(lubridate)    
    library(ggplot2)      
    library(patchwork)
    library(scales)
   ```
5. Run the file based on the step-level documentation block 

   ```
    source("air_quality_eda.R")
   ```
   - When the script is executed in RStudio, visual outputs are both displayed interactively and saved to file
     ```
     print(figure)
     ```
     Renders the composite visualisation directly in the RStudio Plots panel.
     
    What the script does:
     - Imports and cleans raw UK air quality data
     - Reshapes data into a long, analysis-ready format
     - Defines pandemic and post-pandemic periods
     - Computes descriptive and summary statistics for PM₂.₅
     - Generates four analytical visualisations
       - Monthly PM₂.₅ temporal trends (2020–2024)
       - Pandemic vs post-pandemic distribution comparison (boxplots + t-tests)
       - Mean PM₂.₅ comparison with uncertainty (bar charts + stabdard error of mean)
       - PM₂.₅ and NO₂ correlation analysis by city and period
    - Combines all charts into a single composite visualisation
    - Saves:
       -  Processed datasets to ```processed_data/```
       -  High-resolution plots to ```plots/```

### Project Structure

```
air_quality_analysis/
│
├── air_quality_eda.R
├── uk_air_quality_data.csv
│
├── processed_data/
│   ├── pm25_monthly.csv
│   └── pm25_period_summary.csv
│
├── plots/
│   ├── chart1_temporal_trends.png
│   ├── chart2_period_comparison.png
│   ├── chart3_mean_comparison.png
│   ├── chart4_pollutant_correlation_by_period.png
│   └── composite_visualization_all4.png
│
└── README.txt
```
