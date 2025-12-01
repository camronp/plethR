# plethR

**plethR** is an R package designed for analysis and visualization of Whole Body Plethysmography (WBP) data from DSI systems in preclinical respiratory studies. This package provides a complete workflow from data import through publication-quality figures, specifically designed for researchers studying respiratory function in mouse models of pulmonary disease, infection, and airway inflammation.

## Features

### Data Import & Processing
- **`sheets_into_list()`**: Import multi-sheet Excel files from DSI WBP systems
- **`organize_by_groups()`**: Organize subjects into experimental groups
- **`assign_groups_interactive()`**: Interactive group assignment with range support
- **`set_group_names()`**: Define custom group labels

### Data Analysis
- **`calculate_group_averages()`**: Compute group means across time with flexible summary statistics
- **`calculate_auc()`**: Calculate Area Under the Curve with baseline correction and normalization options
- **`plot_pca()`**: Principal Component Analysis with k-means clustering support

### Visualization
- **`plot_wbp_timeseries()`**: Time series plots with smoothing (rolling average or LOESS)
- **`plot_auc_bars()`**: Publication-quality bar plots with statistical annotations
- **`plot_auc_heatmap()`**: Hierarchical clustering heatmaps with multiple color schemes
- **`export_to_excel()`**: Export processed data to organized Excel workbooks

### Key Capabilities
- Handles time series respiratory data (frequency, tidal volume, Penh, etc.)
- Smart y-axis scaling for better visualization of small differences
- Multiple color palettes including colorblind-friendly options
- High-resolution output (300-600 DPI) for publications
- Statistical comparison indicators with customizable reference groups
- Flexible data aggregation and normalization

## Installation

### From GitHub (Recommended)
```r
# Install devtools if you haven't already
install.packages("devtools")

# Install plethR from GitHub
devtools::install_github("camronp/plethR")
```

### Dependencies
plethR requires the following packages, which will be installed automatically:
- **Data manipulation**: dplyr, tidyr, readxl, writexl, zoo
- **Visualization**: ggplot2, ggrepel, ggforce, RColorBrewer, pheatmap, viridisLite
- **Statistics**: stats (base R)

## Quick Start

### Basic Workflow
```r
library(plethR)

# 1. Import data from DSI Excel file
df_list <- sheets_into_list("experiment_data.xlsx", 
                             clean_time = TRUE,
                             remove_apnea = TRUE)

# 2. Define experimental groups
groups <- set_group_names("Control", "Low Dose", "High Dose")

# 3. Assign sheets to groups interactively
mapping <- assign_groups_interactive(groups, df_list = df_list)

# 4. Calculate group averages over time
group_avgs <- calculate_group_averages(df_list, mapping)

# 5. Plot time series
plots <- plot_wbp_timeseries(group_avgs,
                              parameters = c("f", "TVb", "Penh"),
                              smooth_method = "rolling",
                              smooth_window = 3)

# 6. Calculate AUC with normalization
auc_results <- calculate_auc(group_avgs,
                              normalize_to = "Control",
                              baseline_correct = TRUE)

# 7. Create publication-quality AUC plots
auc_plots <- plot_auc_bars(auc_results,
                            group_order = c("Control", "Low Dose", "High Dose"),
                            show_stats = TRUE,
                            reference_group = "Control",
                            save_plots = TRUE,
                            dpi = 600)

# 8. Generate heatmap
heatmap <- plot_auc_heatmap(auc_results,
                             color_scheme = "RdBu",
                             save_plot = TRUE)

# 9. PCA analysis
pca_result <- plot_pca(df_list,
                       group_mapping = mapping,
                       show_loadings = TRUE,
                       save_plot = TRUE)
```

## Example: Infection Study
```r
# Study with 4 groups: Uninfected WT, Uninfected KO, Infected WT, Infected KO

# Import data
df_list <- sheets_into_list("infection_study.xlsx", 
                             clean_time = TRUE,
                             date_average = TRUE)

# Define groups
groups <- set_group_names("Uninfected WT", "Uninfected KO", 
                         "Infected WT", "Infected KO")

# Assign subjects (e.g., sheets 1-4 are group 1, 5-8 are group 2, etc.)
mapping <- assign_groups_interactive(groups, df_list)

# Calculate group averages
averages <- calculate_group_averages(df_list, mapping)

# Calculate AUC normalized to Uninfected WT
auc <- calculate_auc(averages, normalize_to = "Uninfected WT")

# Create bar plots with custom order
plots <- plot_auc_bars(auc,
                       group_order = c("Uninfected WT", "Infected WT",
                                      "Uninfected KO", "Infected KO"),
                       parameters = c("f", "TVb", "MVb", "Penh"),
                       show_stats = TRUE,
                       reference_group = "Uninfected WT",
                       save_plots = TRUE,
                       output_dir = "figures",
                       dpi = 600)

# PCA to visualize overall differences
pca <- plot_pca(df_list,
                group_mapping = mapping,
                show_loadings = TRUE,
                n_loadings = 5)

# View variance explained
print(pca$variance)
```

## Key Parameters

### Respiratory Parameters Analyzed
- **f**: Breathing frequency (breaths/min)
- **TVb**: Tidal volume (mL)
- **MVb**: Minute ventilation (mL/min)
- **Penh**: Enhanced pause (airway resistance indicator)
- **PAU**: Pause
- **Ti, Te**: Inspiratory/expiratory time
- **PIF, PEF**: Peak inspiratory/expiratory flow
- **And more...**

## Tips for Publication-Quality Figures

1. **Use high DPI**: Set `dpi = 600` for journal submissions
2. **Smart y-axis scaling**: Use `y_axis_start = "smart"` to emphasize differences
3. **Custom group order**: Arrange groups logically with `group_order`
4. **Colorblind-friendly palettes**: Use `color_palette = "Set2"` or `"PRGn"`
5. **Statistical annotations**: Enable `show_stats = TRUE` with appropriate `reference_group`
6. **Consistent dimensions**: Use same `width` and `height` across figures

## Update History

- **v1.1.0** (November 2024): Major enhancement release
  - **Complete package overhaul**: All 12 core functions refactored with modern best practices
  - **Enhanced data pipeline**:
    - `sheets_into_list()`: Improved Excel import with better metadata handling and validation
    - `organize_by_groups()`: Streamlined group organization with automatic subject ID assignment
    - `assign_groups_interactive()`: Added range notation support (e.g., "1-4" instead of "1,2,3,4")
    - `calculate_group_averages()`: Flexible summary statistics with configurable time aggregation
  - **Advanced analysis**:
    - `calculate_auc()`: New AUC calculation with baseline correction, normalization, and fold-change output
    - `plot_pca()`: Complete PCA implementation with k-means clustering, loading vectors, and proper subject aggregation
  - **Publication-ready visualizations**:
    - `plot_wbp_timeseries()`: Time series plots with LOESS/rolling average smoothing and multiple layout options
    - `plot_auc_bars()`: Bar plots with intelligent y-axis scaling, statistical annotations, and customizable group ordering
    - `plot_auc_heatmap()`: Hierarchical clustering heatmaps with multiple color schemes including colorblind-friendly options
  - **Quality of life improvements**:
    - `export_to_excel()`: Flexible Excel export supporting both single data frames and lists
    - Smart y-axis scaling eliminates wasted white space while maintaining data integrity
    - Consistent parameter naming across all functions (snake_case)
    - Comprehensive error handling with informative messages
    - High-resolution output support (300-600 DPI) for journal submissions
    - All functions now use modern ggplot2 syntax (no deprecated functions)
  - **Documentation**: Complete roxygen2 documentation for all functions with detailed examples
  - **Deprecated functions**: Moved 8 old functions to archive, replaced with improved versions
  
- **v1.0.5** (November 2024): Major refactor with 12 improved functions
  - Complete rewrite of all core functions
  - Added publication-quality plotting with smart y-axis scaling
  - New `calculate_auc()` with baseline correction
  - Enhanced PCA analysis with loading vectors
  - Improved heatmaps with multiple color schemes
  - Better group organization and workflow
  - High-resolution output options (up to 600 DPI)
  - Comprehensive error handling and validation

- **v0.2.5** (February 2025): Compatibility updates
  - Adapted to new FinePointe software version
  - Added unnest functionality

- **v0.2.0** (February 2025): Feature additions
  - Added string mapping functionality
  - Enhanced group organization

- **v0.1.5** (January 2025): Initial improvements
  - Added `custom_define_group()` function
  - Fixed PCA plotting consistency
  - Added data export functions
  - Minor bug fixes

## Citation

If you use plethR in your research, please cite:

```
Camron Pearce (2025). plethR: Analysis and Visualization of Whole Body Plethysmography Data. 
R package version 1.1.0. https://github.com/camronp/plethR
```

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests on GitHub.

## Contact

For questions, suggestions, or bug reports, please open an issue on GitHub or contact camronpearce@gmail.com.

## Acknowledgments

Developed for preclinical respiratory research. Designed to work with DSI Buxco/FinePointe whole body plethysmography systems.
