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

## Shiny Application

plethR includes a guided Shiny application for the complete analysis, with no R code required.

**Toolbar** (always visible): open a data file or project (with a list of recent projects), **Save** (or Ctrl+S), download everything, choose the parameter and the control group, and open Analysis settings (summary metric and statistics) or Figures. Save writes the project (data, groups, design file, every setting, trained models) to a folder on your computer (default `Documents/plethR_projects`); the toolbar shows when there are unsaved changes, unsaved work is autosaved every 3 minutes, and the browser warns before closing with unsaved changes.

**Pages**

1. **Setup**: one page with three steps. *Data*: upload a FinePointe export; animals, sessions and parameters are detected automatically. *Groups*: groups are suggested from sheet names (`Infected WT1`, `Infected WT2` -> `Infected WT`); edit them, or load a **study design file** (template downloadable from the app) with groups, sex, exclusions, body weights and CFU. *Processing*: one value per animal per session (median recommended), optional Rinx filter, baseline adjustment, variability features and body weight, with a data-check table.
2. **Results**: every view in one list, grouped by question. *Overview*: key findings, all parameters, a methods paragraph. *Over time*: time course (mixed model with baseline, sex and weight covariates, or per-timepoint tests), every animal as a heatmap, animal trends, change from baseline, and bacterial burden (shown when the design file has CFU values). *Group differences*: group comparison, effect sizes with 95% CIs, % difference heatmap, factorial (e.g. genotype x infection) model. *Patterns*: PCA and More views (correlations, group profiles, two-parameter paths, breath-record distributions). *Data*: data tables.
3. **Prediction**: *Models for this study* predict infected vs uninfected, acute vs chronic infection, or disease severity from the parameters you choose. Validation holds out whole animals; models are saved with the project and can be applied to new files. *Models across studies* is the study library: add each analyzed study to a library folder, train models across studies with leave-one-study-out validation, and keep a history of saved models.
4. **Treatment**: a lung health score (or a machine learning disease score) comparing treated animals with untreated and healthy animals: verdict, % rescue, statistics, per-parameter breakdown and per-animal trends.

Every figure has image and PDF download buttons and every table has Copy/CSV/Excel buttons. **Download everything** produces one zip with the Excel workbook, every figure, a combined PDF, methods, settings and a README; the R script download reproduces the analysis with plethR functions.

All statistics use the animal, not the individual breath, as the experimental unit.

To launch it after installing the package:
```r
library(plethR)
run_plethR_app()
```

This opens the application in your default web browser. Set `run_plethR_app(launch_browser = FALSE)` to run it without opening a browser automatically.

The application requires the `shiny`, `bslib`, `DT`, and `shinycssloaders` packages, which can be installed with:
```r
install.packages(c("shiny", "bslib", "DT", "shinycssloaders"))
```

When developing plethR from a cloned copy of this repository, you can instead open `inst/shiny-app/app.R` in RStudio and click **Run App**. Run this way, the app loads the functions directly from `R/`, so changes take effect without reinstalling the package. There is no upload size limit, so large FinePointe Excel exports can be imported.

## Quick Start

### Recommended workflow (v1.2.0)

The animal-level pipeline used by the app is also available in R:

```r
library(plethR)

wbp <- read_wbp("experiment.xlsx")                 # keeps Phase, drops empty sheets and log lines
groups <- split(unique(wbp$subject), suggest_groups(unique(wbp$subject)))
sessions <- summarize_sessions(assign_groups(wbp, groups))   # one median per animal per session

group_means <- summarize_groups(sessions)          # mean, SD, SEM, n per group and timepoint
auc <- summarize_subjects(sessions, metric = "auc") # one AUC per animal and parameter
stats <- compare_groups(auc, reference = "Uninfected WT")    # Welch tests, Holm correction
tp_stats <- compare_timepoints(sessions, reference = "Uninfected WT")

colors <- plethr_colors(names(groups))
plot_timecourse(group_means, "Penh", sessions, colors, show_individuals = TRUE, stats = tp_stats)
plot_group_comparison(auc, "Penh", stats, colors)
plot_difference_heatmap(stats)
plot_subject_pca(auc, colors)$plot
```

### Prediction models

Requires the optional modeling packages (`ml_check_packages()` lists any that are missing).

```r
infected <- c("Infected WT", "Infected BENaC"); controls <- c("Uninfected WT", "Uninfected BENaC")

# Infected vs uninfected (sessions before "Week 1" are left out by default)
fit <- ml_fit(ml_prepare_infection(sessions, infected, controls, infection_timepoint = "Week 1"))
fit                                   # held-out performance per model
plot_ml_over_time(fit, "en")          # when does infection become detectable?

# Acute vs chronic, plus the time-only check on controls
phase <- ml_fit(ml_prepare_phase(sessions, infected, "Week 1", acute_days = 14))
check <- ml_fit(ml_prepare_phase(sessions, controls, "Week 1", acute_days = 14))

# Severity: a table with columns subject and value (and optionally timepoint)
sev <- ml_fit(ml_prepare_severity(sessions, cfu_table, outcome_name = "Lung CFU", log_outcome = TRUE))
plot_ml_observed(sev, "en", "animal")

ml_predict(fit, new_sessions, "en")   # apply to a new study
```

### Is a treatment working?

```r
scored <- lung_health_score(sessions, healthy = "Uninfected", disease = "Infected + vehicle",
                            parameters = c("Penh", "TVb", "EF50", "Rpef", "EEP"), onset_timepoint = "Day 3")
eff <- treatment_efficacy(scored, treated = "Infected + drug", from = "Day 7")
eff$verdict$text                      # plain-language verdict
eff$rescue                            # % of the disease effect removed
plot_treatment_parameters(eff)        # which parameters the treatment normalizes

# Alternative score: a model trained on healthy vs untreated animals (log-odds of disease)
ml_scored <- ml_disease_score(sessions, "Uninfected", "Infected + vehicle", onset_timepoint = "Day 3")
treatment_efficacy(ml_scored, treated = "Infected + drug", from = "Day 7")$verdict$text

# Direction of each animal over time (improving / worsening / no clear trend)
trends <- animal_trends(scored, "lung_score", from = "Day 7", higher_is = "worse")
plot_animal_trends(scored, "lung_score", trends, higher_is = "worse", reference_line = 0)
```

### Mixed-model time course and study planning

```r
mt <- mixed_timecourse(sessions, "Penh", reference = "Uninfected WT", covariates = c("baseline", "sex", "weight"),
                       sex = setNames(design$animals$sex, design$animals$animal), log_transform = TRUE)
mt$anova; mt$contrasts               # terms, and each group vs reference at every timepoint
plot_mixed_timecourse(mt, colors)

plan <- sample_size_plan(auc, "Uninfected WT", power = 0.8, effect_pct = 20, n_comparisons = 3)
plot_power_curve(plan, "Penh", colors)
```

### Study library: models across studies

```r
lib <- "~/plethR_library"
library_add_study(lib, sessions, "CP05", conditions = c("Uninfected WT" = "Uninfected", "Infected WT" = "Infected", ...),
                  infection_timepoint = "Week 1", offset = 3)
fit <- ml_fit(ml_prepare_library(library_load(lib), "infection"), validation = "study")  # leave one study out
ml_study_metrics(fit)                 # performance in each held-out study
library_save_model(lib, fit, "infection_v1")
plot_model_history(library_models(lib))
```

### Breathing variability features (optional)

How each parameter fluctuates within a session: smoothness (lag-1 autocorrelation), irregularity (sample entropy),
robust CV, slow/mid/fast fluctuation power and spectral slope. FinePointe records are ~2 s averages, so these describe
changes over seconds to minutes, not breath-to-breath variability.

```r
var <- session_variability(wbp, parameters = c("TVb", "MVb", "PIFb"), features = c("ac1", "sampen", "slow"))
sessions <- add_variability(sessions, var)   # adds columns such as TVb_ac1, analyzed like any parameter
```

### Original workflow
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

- **v1.4.0** (October 2026): Reproducibility, better statistics and a study library
  - Save and reopen projects; downloadable R script that reproduces the analysis
  - Reorganized app: a toolbar with file, save, export, analysis and figure options; Setup on one page; all results views in one list; Save/Ctrl+S to a projects folder with autosave, unsaved-changes indicator and recent projects
  - Automated tests (`tests/testthat`), including regression tests on CP05 when the file is present
  - Mixed-model time course with baseline, sex and body-weight covariates: `mixed_timecourse()`, `plot_mixed_timecourse()`
  - Sample-size planning in R (not in the app): `sample_size_plan()`, `plot_power_curve()`
  - Multi-study library with leave-one-study-out validation and model history: `library_add_study()`, `library_load()`, `ml_prepare_library()`, `ml_fit(validation = "study")`, `ml_study_metrics()`, `library_save_model()`, `plot_model_history()`

- **v1.3.0** (October 2026): Prediction goals and treatment efficacy
  - Machine learning: infected vs uninfected (with pre-infection sessions labeled uninfected), acute vs chronic (with a time-only check on controls), and disease severity regression on a measured value: `ml_prepare_infection()`, `ml_prepare_phase()`, `ml_prepare_severity()`, `add_dpi()`, `plot_ml_observed()`
  - Treatment efficacy from a lung health score or a machine learning disease score: `lung_health_score()`, `ml_disease_score()`, `treatment_efficacy()`, `plot_treatment_parameters()`
  - Per-animal trends with direction labels: `animal_trends()`, `plot_animal_trends()` (Results and Treatment steps)
  - Optional within-session breathing variability features: `session_variability()`, `add_variability()` (switch in the Process step)
  - New views: `plot_dashboard()`, `plot_animal_heatmap()`, `plot_correlation()`, `plot_group_profile()`, `plot_effect_forest()`, `plot_record_distribution()`, `plot_trajectory()`, `plot_waterfall()`; `compare_groups()` now returns 95% confidence intervals
  - Export: figure format/size/resolution settings, table export buttons, and a "Download everything" zip
  - Study design file: `write_design_template()`, `read_design()`, `design_groups()`, `add_body_weight()`, `design_dpi()`, `plot_cfu_overlay()`, `cfu_correlation()`, `plot_cfu_correlation()`
  - App: prediction goal selector, user-chosen predictor parameters, severity table upload, and a Treatment efficacy step
- **v1.2.0** (October 2026): Animal-level analysis pipeline and redesigned app
  - New functions: `read_wbp()`, `suggest_groups()`, `assign_groups()`, `summarize_sessions()`, `apply_baseline()`, `summarize_groups()`, `summarize_subjects()`, `compare_groups()`, `compare_timepoints()`
  - New plots: `plot_timecourse()`, `plot_group_comparison()`, `plot_difference_heatmap()`, `plot_subject_pca()`, with `theme_plethr()` and colorblind-safe `plethr_colors()`
  - Statistics treat the animal as the experimental unit, with Welch or rank-based tests and multiple comparison correction
  - Guided Shiny app with recommended settings, data checks, key findings, a parameter glossary, full export and a methods paragraph
  - Existing functions are unchanged

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

- **v1.0.6** (February 2025): Compatibility updates
  - Adapted to new FinePointe software version
  - Added unnest functionality

- **v1.0.7** (February 2025): Feature additions
  - Added string mapping functionality
  - Enhanced group organization

- **v1.0.8** (January 2025): Initial improvements
  - Added `custom_define_group()` function
  - Fixed PCA plotting consistency
  - Added data export functions
  - Minor bug fixes

- **v1.1.0** (July 2026)
  - Updated depracated functions
  - Re-worked calculate_group_averages
  - Validated package functionality

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
