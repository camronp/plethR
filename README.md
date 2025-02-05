# plethR

**plethR** is an R package designed for the analysis and visualization of Whole Body Plethysmography (WBP) data in preclinical mouse studies. This package provides tools for data processing, normalization, statistical analysis, and generating heatmaps and plots to compare groups and parameters. It aims to assist researchers in analyzing respiratory function data in animal models, such as those used in pulmonary disease research.

## Features

- **Data Import**: Load and preprocess WBP data from CSV or Excel files.
- **Normalization and Standardization**: Easily normalize or standardize data to make it comparable across different groups or experiments.
- **Visualization**: Generate heatmaps and plots for group comparisons and parameter analysis.
- **Statistical Analysis**: Perform statistical tests to assess differences between groups and conditions.

## Installation

To install the `plethR` package from GitHub, you can use the following command in R:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install plethR from GitHub
devtools::install_github("camronp/plethR")
```
Updates pushed out monthly
- Update 1/16/2025 for some minor bug fixes, add custom_define_group function, and fix PCA plotting for consistency
- update 1/17/2025 to include export_as_clean_df and renamed_group_df functions
- update 2/5/2025 to include a string_function with the renamed_group_df function
