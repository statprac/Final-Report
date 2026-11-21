# Final Report - US Election Polling Accuracy

### Project Overview

This project analyses the accuracy of 2012 and 2016 election state-level polls and whether undecided voter share and polling mode are associated with polling accuracy. The relationship between the two election periods is also investigated. These relationships will be uncovered using exploratory data analysis along with linear modelling.

### Research Question

How are survey mode and reported undecided voter share associated with state-level polling error, after accounting for time before Election Day, and do these associations differ between the 2012 and 2016 elections? 

### Data Sources

This project uses 3 main datasets:

- Polling data: Two datasets containing state-level polling data from the 2012 and 2016 US presidential elections were combined into one dataset which is used in this report. This data was sourced from the HuffPost Pollster collection.
- Election results: US Election results were obtained from the MIT Election Data and Science Lab (MEDSL). This dataset contains electoral results from 1976-2024.

### Repository Structure

- `project datasets/`: Stores raw data and cleaned data (cleaned in EDA report) along with analysis datasets created in this investigation.
- `report-references.bib`: Contains the bibliography of references used in the report.
- `Final_Report.qmd`: Quarto document used to write and produce the completed report.
- `README.md`: An overview of the project repository along with instructions for reproducing the analysis done in the report.
- `project datasets/data_preparation.R`: Prepares the analysis datasets and performs the sensitivity check.
- `model_analysis.R`: Fits and compares the models, checks diagnostics, and creates the prediction plot.

### Data Preparation

The combined polling datasets and the election results dataset were cleaned and then combined prior to the analysis. The main steps of this process included:

1. Election results for 2012 and 2016 were selected.
2. Votes for major party presidential candidates across different ballot lines were combined.
3. The Democratic-Republican vote margin was calculated for each state.
4. The poll observations were matched to their corresponding electoral results.
5. The absolute difference between poll margin and actual margin was calculated and added to the dataset as polling error.
6. Polling observations were cut off so that all of the observations were within 90 days of the election date.
7. Observations with invalid or missing sample sizes, missing polling error or missing undecided voter share were all removed from the dataset.
8. Polls that used Live Phone, Internet or IVR/Online survey modes were kept in the dataset for the main analysis. The rest were excluded.
9. Two unusual observations were removed from the main sample but retained for sensitivity testing.

### Reproducing the Analysis

To reproduce the analysis done in this investigation:

1. Clone the repository and open `Final-Report.Rproj` in RStudio.
2. Install R and the required packages listed below.
3. If needed, run `project datasets/data_preparation.R` to recreate the analysis datasets and sensitivity comparison.
4. If needed, run `model_analysis.R` to reproduce the models, diagnostics, and prediction plot.
5. Open `Final_Report.qmd` and render it to PDF using Quarto.

### Packages Used

The analysis and investigation were completed in R, and the main report was produced using Quarto. The packages used are:

- tidyverse - for data manipulation, cleaning, and visualisation
- readr - for importing and exporting datasets
- dplyr - for data wrangling and manipulation
- tidyr - for reshaping and organising data
- ggplot2 - for visualising data
- modelsummary - for displaying model results
- car - for checking generalised variance inflation factors
- knitr - for formatting tables in the report

### Authors

- Caitlin Knowles
- Isobel Cusack
- Linh Chi Nguyen
- Manjun Xu
