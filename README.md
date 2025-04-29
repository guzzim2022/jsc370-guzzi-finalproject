# JSC370 Final Project

Final Project for JSC370 (Winter 2025)

This repository contains the final project for the JSC370 course, where I analyze bicycle theft in Toronto by exploring relationships between theft incidents and neighborhood characteristics.

## Overview

The goal of this project is to understand the spatial patterns of bicycle theft across Toronto and explore the influence of various socioeconomic factors. The study combines data from three key datasets: Bicycle Thefts, Bicycle Shops, and Neighbourhood Profiles. By integrating these datasets, this analysis provides insights into how urban infrastructure and socio-economic factors relate to the rates of bicycle theft.

### Project Link

You can view the website for this project at:  
[Project Website](https://guzzim2022.github.io/jsc370-guzzi-finalproject/)



## Dataset Acquisition

The datasets used in this analysis are publicly available from the City of Toronto’s Open Data Portal. Below is a list of the key datasets and their respective sources:

1. Bicycle Thefts Dataset  
   This dataset provides information about bicycle theft incidents across Toronto, including the location, timing, and details about the bikes (e.g., make, model, and value). The data was used to analyze patterns of bicycle theft throughout the city.  
   [Bicycle Thefts Dataset](https://open.toronto.ca/dataset/bicycle-thefts/)

2. Bicycle Shops Dataset  
   This dataset includes information about registered bicycle shops and repair shops across Toronto. The presence of these shops is used in this project to study their relationship with bicycle theft rates in various neighborhoods.  
   [Bicycle Shops Dataset](https://open.toronto.ca/dataset/bicycle-shops/)

3. Neighbourhood Profiles Dataset  
   This dataset contains socio-economic data for 158 Toronto neighborhoods, including demographic, income, and housing data. The dataset allows the analysis of how socio-economic factors influence bicycle theft rates.  
   [Neighbourhood Profiles Dataset](https://open.toronto.ca/dataset/neighbourhood-profiles/)


All of these datasets were retrieved via the City of Toronto’s API using R’s `opendatatoronto` and `httr` packages. The data is in CSV format and was downloaded directly from the Open Data Portal using automated scripts.



