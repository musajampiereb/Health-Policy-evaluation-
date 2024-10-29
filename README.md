# MRC Data Science Program - Mathematical Modelling Projects

## Trends and Seasonal Variation Analysis of Childhood Illnesses in Rwanda

This script analyzes trends and seasonal variations in the availability and stockouts of malaria-related commodities (Coartem, Quinine Vials, and Quinine Tablets) for childhood illnesses in Rwanda from 2012 to 2016, focusing on children under five in the Integrated Management of Childhood Illnesses (IMCI) program.

### Dataset
The data for this analysis is contained in the `MALARIA_COMMODITIES.csv` file, which includes monthly records for malaria treatments and commodities in Rwanda.

### Objectives
1. Explore seasonal patterns and long-term trends in malaria treatments.
2. Analyze stockout periods using time series methods.
3. Model the impact of time, policy level, and trend changes on stockouts and distributions.

### Features and Analysis
- **Time Series Conversion**: Data columns are converted to time series objects for monthly analysis.
- **Exploratory Data Analysis**: Histograms, boxplots, and normality checks for stockout distributions.
- **Seasonal Decomposition**: Decomposition of time series data into trend, seasonal, and irregular components.
- **Trend Modeling**: Ordinary least squares (OLS) regression and generalized least squares (GLS) models are used to estimate trends.
- **Diagnostics**: Residual analysis, autocorrelation checks, and likelihood-ratio tests for model adequacy.

### Requirements
- R version 4.0 or above.
- R packages:
  - `MASS`
  - `rcompanion`
  - `lattice`
  - `nlme`
  - `car`
  - `foreign`
  - `tseries`

### Script Overview
1. **Data Loading**: Reads the CSV file into a dataframe.
2. **Time Series Creation**: Converts data columns into time series (monthly frequency) for 2012-2016.
3. **Exploratory Analysis**: Generates histograms, boxplots, and normality plots for each stockout variable.
4. **Seasonal Decomposition**: Breaks down the time series into trend, seasonal, and irregular components.
5. **Trend Modeling**: Applies regression models to understand stockout trends over time.
6. **Diagnostics**: Residual diagnostics and autocorrelation checks.
7. **Advanced Modeling**: Fits ARMA models to handle serial correlation in the data.

### Running the Script
Ensure all dependencies are installed. Run the script in R or RStudio. Example:
```R
source("path_to_script.R")
