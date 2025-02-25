
# Maize Breeding Trial Dataset

This dataset provides comprehensive multi-environment trial (MET) results for maize hybrids evaluated under varying vapor pressure deficit (VPD) and water management conditions. Collected over an 11-year period (2009-2019) in Croatia and Türkiye, the data include detailed agronomic performance metrics—grain yield, phenological stages, and environmental descriptors—across a range of irrigation regimes and temperature-humidity combinations. The dataset comprises more than 2900 plot-level observations, capturing genetic variance, genotype-by-environment interaction, and yield responses to increasing VPD. These data enable researchers to investigate maize adaptability to emerging hot, managed environments, assess genetic gains over breeding cycles, and explore how transpiration traits influence yield potential. Fully documented environmental indices and methodological details facilitate re-analysis, meta-studies, and integration into broader crop modeling frameworks. Researchers, breeders, and agronomists can use this dataset to inform breeding strategies, optimize management practices, and better understand maize yield stability in the face of climate change.

## Data and Code Availability

* **Repository name:** Zenodo 
* **Data identification number:** 10.5281/zenodo.14445186 
* **Direct URL to data:** [https://zenodo.org/records/14678148](https://zenodo.org/records/14678148) 
* **Code files:** corrs_CO.R, corrs_C1.R, Visualizations.ipynb 

## Code Usage

### R Scripts

The R scripts (corrs_CO.R and corrs_C1.R) analyze the correlation between yield performance and environmental variables using mixed models.  They perform the following steps:

1. Fit mixed-effects models using environmental and genotype data. 
2. Extract Best Linear Unbiased Predictions (BLUPs) for genotype-environment interactions. 
3. Calculate environmental averages (e.g., vapor pressure deficit) for comparison. 
4. Plot trendlines to illustrate yield relationships with environmental conditions, annotating R² values for model fit. 

### iPython Notebook

The iPython notebook (Visualizations.ipynb) describes the dataset structure and performs Principal Component Analysis (PCA) to identify key variables influencing yield and environmental interactions.  It also visualizes PCA results. 

## Requirements

* R (version 3.x or 4.x) with the following packages: 
  * lme4 (v1.1.x) 
  * sommer (v3.x) 
  * metan (v1.x) 
  * ggplot2 
* Python (version 3.11.10) with the necessary libraries for PCA and visualization. 

## License

Data are publicly available under a Creative Commons Attribution 4.0 International license.  No access restrictions. 
