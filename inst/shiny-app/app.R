library(shiny)
library(bslib)
library(DT)
library(shinycssloaders)
library(ggplot2)

# Remove Shiny's upload size limit (default 5 MB) so large FinePointe exports load.
options(shiny.maxRequestSize = -1)

# Running from the plethR-fresh source tree: use the source R/ files so edits show
# up without reinstalling. Otherwise (run_plethR_app()) use the installed package.
pkg_dir <- normalizePath(file.path(getwd(), "..", ".."), mustWork = FALSE)
r_files <- list.files(file.path(pkg_dir, "R"), pattern = "\\.R$", full.names = TRUE)
if (length(r_files) > 0 && file.exists(file.path(pkg_dir, "DESCRIPTION"))) {
  # Sourced files don't get the package's imports, so attach them first.
  imports <- read.dcf(file.path(pkg_dir, "DESCRIPTION"), fields = "Imports")[1, 1]
  imports <- trimws(sub("\\(.*", "", strsplit(imports, ",")[[1]]))
  for (pkg in imports) suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  invisible(lapply(r_files, source))
  app_version <- read.dcf(file.path(pkg_dir, "DESCRIPTION"), fields = "Version")[1, 1]
} else if (requireNamespace("plethR", quietly = TRUE)) {
  library(plethR)
  app_version <- as.character(utils::packageVersion("plethR"))
} else {
  stop("Could not find the plethR package. Install it with devtools::install() or run this app with plethR::run_plethR_app().")
}

`%||%` <- function(a, b) if (is.null(a)) b else a
`%>%` <- magrittr::`%>%`

# ---- Helpers --------------------------------------------------------------

rec <- function() span(class = "badge rounded-pill text-bg-success ms-1 rec-badge", "recommended")

# Labels with a tip are underlined with dots; hovering shows the explanation.
label_with <- function(text, tip = NULL, recommended = FALSE) {
  span(if (is.null(tip)) text else tooltip(span(class = "has-tip", text), tip), if (recommended) rec())
}

next_button <- function(id, label) {
  div(class = "d-flex justify-content-end mt-3",
      actionButton(id, label, class = "btn-primary"))
}

plot_downloads <- function(id) {
  div(class = "d-flex gap-2 mt-2",
      downloadButton(paste0(id, "_png"), "PNG (300 dpi)", class = "btn-sm btn-outline-secondary", icon = NULL),
      downloadButton(paste0(id, "_pdf"), "PDF (vector)", class = "btn-sm btn-outline-secondary", icon = NULL))
}

save_plot <- function(file, plot, width, height, type) {
  if (type == "pdf") {
    ggplot2::ggsave(file, plot, width = width, height = height,
                    device = if (capabilities("cairo")) grDevices::cairo_pdf else "pdf")
  } else {
    ggplot2::ggsave(file, plot, width = width, height = height, dpi = 300, bg = "white")
  }
}

safe_name <- function(x) gsub("[^A-Za-z0-9_-]+", "_", x)

metric_choices <- c(
  "Area under the curve (AUC)" = "auc",
  "Time-averaged value" = "mean",
  "Peak (maximum)" = "max",
  "Minimum" = "min",
  "Value at one timepoint" = "value"
)

plethR_ml_names <- function() ml_model_names()

test_names <- c(parametric = "Welch's t-test", nonparametric = "Mann-Whitney test")
omnibus_names <- c(parametric = "Welch's ANOVA", nonparametric = "Kruskal-Wallis test")
padj_names <- c(holm = "Holm", BH = "Benjamini-Hochberg (FDR)", bonferroni = "Bonferroni", none = "no")

# ---- UI -------------------------------------------------------------------

app_theme <- bs_theme(
  version = 5,
  primary = "#1F5F8B",
  secondary = "#6C7A89",
  success = "#2E8B57",
  base_font = font_collection("-apple-system", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif"),
  "border-radius" = "0.5rem"
)

app_css <- "
.step-intro { color: #495057; max-width: 70rem; margin-bottom: 1rem; }
.rec-badge { font-size: 0.65rem; font-weight: 600; vertical-align: middle; }
.card-header { font-weight: 600; }
.sidebar .form-group, .sidebar .shiny-input-container { margin-bottom: 0.9rem; }
.choice-help { font-size: 0.82rem; color: #6c757d; margin-top: -0.5rem; margin-bottom: 0.9rem; }
.methods-box { background: #f8f9fa; border-left: 4px solid #1F5F8B; padding: 1rem 1.25rem; font-size: 0.95rem; }
.status-pill { font-size: 0.8rem; }
table.small-table { font-size: 0.85rem; }
.has-tip { border-bottom: 1px dotted #1F5F8B; cursor: help; }
.glossary td { vertical-align: top; }
"

load_panel <- nav_panel(
  title = "1. Load data", value = "load",
  div(class = "container-fluid py-3",
    p(class = "step-intro",
      "Upload a DSI FinePointe Excel export. Each sheet is one animal. plethR keeps the session (Phase) labels, ",
      "skips empty sheets (such as .Apnea sheets) and FinePointe log lines, and reads every respiratory parameter."),
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Upload"),
        fileInput("excel_file", NULL, accept = ".xlsx", buttonLabel = "Choose .xlsx file", width = "100%"),
        p(class = "small text-muted", "There is no file size limit. Large files (around 100 MB) take about a minute to read."),
        uiOutput("load_message")
      ),
      card(
        card_header("What was found"),
        uiOutput("load_summary")
      )
    ),
    uiOutput("load_next")
  )
)

groups_panel <- nav_panel(
  title = "2. Groups", value = "groups",
  div(class = "container-fluid py-3",
    p(class = "step-intro",
      "Tell plethR which animals belong to which experimental group. Groups were suggested from the sheet names ",
      "(\"Infected WT1\", \"Infected WT2\" → \"Infected WT\"). Edit the list of group names, then check the animals in each group. ",
      "Animals that are not in any group are left out of the analysis."),
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Groups"),
        textAreaInput("group_names", label_with("Group names, one per line",
                      "The order here is the order groups appear in every figure and table. Rename a group by editing its line."),
                      rows = 5, width = "100%"),
        selectInput("reference", label_with("Control / reference group",
                    "Other groups are compared with this group in the statistics, heatmaps and significance markers. Usually your untreated, uninfected or wild-type group."),
                    choices = NULL, width = "100%"),
        uiOutput("group_warnings")
      ),
      card(
        card_header("Animals in each group"),
        uiOutput("group_assign_ui")
      )
    ),
    next_button("to_process", "Next: process data")
  )
)

process_panel <- nav_panel(
  title = "3. Process", value = "process",
  layout_sidebar(
    sidebar = sidebar(
      width = 360, title = "Processing settings",
      radioButtons("tp_def", label_with("Define sessions (timepoints) by"),
                   choices = c("FinePointe Phase label" = "phase", "Calendar date" = "date"), selected = "phase"),
      div(class = "choice-help", "Phase labels (e.g. \"Week 2\", \"7DPE\") are the session names set in FinePointe and are recommended. ",
          "Use calendar date if you did not set phases. Timepoints are ordered by when they were recorded."),
      radioButtons("session_stat", label_with("Summarize each animal's session with the"),
                   choices = c("Median" = "median", "Mean" = "mean"), selected = "median"),
      div(class = "choice-help", tags$b("Median is recommended."), " A 20-minute session has hundreds of breath records, ",
          "including sighs, sniffing and movement artifacts with extreme values (e.g. Penh far above its typical value). ",
          "The median is not pulled by these; the mean is."),
      input_switch("use_rinx", "Exclude records with a high rejection index (Rinx)", value = FALSE),
      conditionalPanel("input.use_rinx",
        sliderInput("rinx_max", "Maximum Rinx (%)", min = 10, max = 100, value = 80, step = 5)),
      div(class = "choice-help", "Rinx is the % of breaths FinePointe rejected in a record. Optional: in typical data the median Rinx is around 50%, ",
          "so strict cut-offs remove a lot of data. Check record counts in the table after changing this."),
      hr(),
      radioButtons("baseline_method", label_with("Express values as"),
                   choices = c("Measured values (no baseline adjustment)" = "none",
                               "% of each animal's baseline" = "percent",
                               "Change from each animal's baseline" = "difference"),
                   selected = "none"),
      conditionalPanel("input.baseline_method != 'none'",
        selectInput("baseline_tp", "Baseline timepoint", choices = NULL)),
      div(class = "choice-help", tags$b("Start with measured values."), " Baseline adjustment removes differences between animals ",
          "that existed before treatment; use it when baselines vary a lot or groups differ at baseline."),
      selectizeInput("tp_include", label_with("Timepoints to analyze",
                     "Remove a timepoint to exclude it from all figures and statistics, e.g. a failed session."),
                     choices = NULL, multiple = TRUE, options = list(plugins = list("remove_button")))
    ),
    p(class = "step-intro",
      "Breath records are summarized into one value per animal per session. All statistics use the animal, not the individual breath, ",
      "as the unit of analysis: thousands of breaths from one animal are not independent replicates."),
    card(
      card_header("Data check: records per animal and session"),
      p(class = "small text-muted mb-2",
        "Red cells are missing sessions; yellow cells have fewer than half the typical number of records. Consider excluding animals or timepoints with many flags."),
      withSpinner(DTOutput("check_table"), color = "#1F5F8B"),
      uiOutput("check_flags")
    ),
    next_button("to_results", "Next: results")
  )
)

results_sidebar <- sidebar(
  width = 340, title = "Analysis settings", open = "desktop",
  selectInput("param", "Parameter", choices = NULL),
  uiOutput("param_description"),
  accordion(
    open = c("Summary metric", "Statistics"),
    accordion_panel(
      "Summary metric",
      selectInput("metric", label_with("One value per animal",
                  "Used for group comparisons, the heatmap and PCA. AUC captures the cumulative effect over the whole time course."),
                  choices = metric_choices, selected = "auc"),
      div(class = "choice-help", tags$b("AUC is recommended"), " for overall effects. \"Time-averaged value\" is the same information in the parameter's own units (AUC divided by duration), which is easier to read."),
      uiOutput("window_ui")
    ),
    accordion_panel(
      "Statistics",
      radioButtons("test", label_with("Test"),
                   choices = c("Welch's t-test / Welch's ANOVA" = "parametric",
                               "Mann-Whitney / Kruskal-Wallis" = "nonparametric"),
                   selected = "parametric"),
      div(class = "choice-help", tags$b("Welch is recommended"), " for typical group sizes (4-10 animals). It does not assume equal variances. ",
          "Rank-based tests have very little power with small groups: with 4 vs 4 animals, the smallest possible p-value is 0.029."),
      radioButtons("comp_mode", "Compare",
                   choices = c("Each group with the reference group" = "reference", "All pairs of groups" = "all"),
                   selected = "reference"),
      div(class = "choice-help", "Comparing only with the reference group means fewer tests, so more power after correction."),
      selectInput("padj", label_with("Multiple comparison correction"),
                  choices = c("Holm" = "holm", "Benjamini-Hochberg (FDR)" = "BH", "Bonferroni" = "bonferroni", "None (exploratory only)" = "none"),
                  selected = "holm"),
      div(class = "choice-help", tags$b("Holm is recommended."), " It controls false positives like Bonferroni but has more power. ",
          "Benjamini-Hochberg is suited to screening many timepoints or parameters.")
    ),
    accordion_panel(
      "Appearance",
      selectInput("palette", "Group colors",
                  choices = c("Colorblind-safe (Okabe-Ito)" = "okabe-ito", "Set1" = "set1", "Dark2" = "dark2",
                              "Viridis" = "viridis", "Grayscale" = "grayscale"),
                  selected = "okabe-ito"),
      selectizeInput("params_multi", label_with("Parameters in heatmap, PCA and export",
                     "Remove parameters you do not want in the overview figures and the export."),
                     choices = NULL, multiple = TRUE, options = list(plugins = list("remove_button")))
    )
  )
)

results_panel <- nav_panel(
  title = "4. Results", value = "results",
  layout_sidebar(
    sidebar = results_sidebar,
    navset_card_underline(
      id = "results_tabs",
      nav_panel(
        "Key findings",
        uiOutput("summary_boxes"),
        uiOutput("power_note"),
        h5(class = "mt-3", "Group differences, strongest first"),
        p(class = "small text-muted", textOutput("findings_caption", inline = TRUE)),
        withSpinner(DTOutput("findings_table"), color = "#1F5F8B")
      ),
      nav_panel(
        "Time course",
        layout_columns(
          col_widths = c(9, 3),
          div(withSpinner(plotOutput("tc_plot", height = "520px"), color = "#1F5F8B"), plot_downloads("dl_tc")),
          div(
            radioButtons("tc_error", label_with("Error bars", "SEM shows how precisely the group mean is known; SD shows how much animals vary."),
                         choices = c("SEM" = "sem", "SD" = "sd", "None" = "none"), selected = "sem", inline = TRUE),
            radioButtons("tc_style", "Error style", choices = c("Bars" = "bars", "Shaded band" = "band"), selected = "bars", inline = TRUE),
            radioButtons("tc_x", label_with("X axis", "Session labels are evenly spaced. Study day shows the true time between sessions."),
                         choices = c("Session labels" = "timepoint", "Study day" = "day"), selected = "timepoint"),
            input_switch("tc_individuals", "Show each animal", value = FALSE),
            input_switch("tc_stats", "Mark significant timepoints", value = TRUE),
            p(class = "choice-help", "Stars: adjusted p < 0.05 vs the reference group at that timepoint, colored by group. ",
              "Corrected across all timepoints and groups for this parameter.")
          )
        ),
        h6(class = "mt-3", "Tests at each timepoint"),
        DTOutput("tc_table")
      ),
      nav_panel(
        "Group comparison",
        layout_columns(
          col_widths = c(8, 4),
          div(withSpinner(plotOutput("cmp_plot", height = "520px"), color = "#1F5F8B"), plot_downloads("dl_cmp")),
          div(
            radioButtons("cmp_style", "Plot style",
                         choices = c("Bars (mean ± SEM) with animals" = "bar",
                                     "Points with mean ± SEM" = "dot",
                                     "Box plot with animals" = "box"), selected = "bar"),
            input_switch("cmp_stats", "Show comparison brackets", value = TRUE),
            input_switch("cmp_ns", "Include non-significant (ns) brackets", value = FALSE),
            uiOutput("cmp_stats_text")
          )
        ),
        h6(class = "mt-3", "Pairwise comparisons"),
        DTOutput("cmp_table")
      ),
      nav_panel(
        "Heatmap",
        layout_columns(
          col_widths = c(9, 3),
          div(withSpinner(plotOutput("hm_plot", height = "640px"), color = "#1F5F8B"), plot_downloads("dl_hm")),
          div(
            radioButtons("hm_mode", "Show",
                         choices = c("Summary metric, all groups" = "metric", "Each timepoint, one group" = "time"),
                         selected = "metric"),
            conditionalPanel("input.hm_mode == 'time'", selectInput("hm_group", "Group", choices = NULL)),
            input_switch("hm_cluster", "Cluster similar parameters", value = TRUE),
            p(class = "choice-help", "Colors show the % difference between group means and the reference group. ",
              "Stars mark adjusted p < 0.05. Large differences are capped so small ones stay visible.")
          )
        )
      ),
      nav_panel(
        "PCA",
        layout_columns(
          col_widths = c(9, 3),
          div(withSpinner(plotOutput("pca_plot", height = "560px"), color = "#1F5F8B"), plot_downloads("dl_pca")),
          div(
            p(class = "choice-help mt-0", "Each point is one animal, positioned by its summary metric across the selected parameters ",
              "(centered and scaled). Animals with similar respiratory profiles are close together. PCA is descriptive: it shows patterns, not significance."),
            radioButtons("pca_shapes", "Group outlines",
                         choices = c("Outline (convex hull)" = "hull", "95% confidence ellipse" = "ellipse", "None" = "none"),
                         selected = "hull"),
            conditionalPanel("input.pca_shapes == 'ellipse'",
              p(class = "choice-help", "Ellipses need at least 3 animals per group. With few animals they are very uncertain ",
                "and often much larger than the points; treat them as a visual guide only.")),
            input_switch("pca_labels", "Label animals", value = FALSE),
            sliderInput("pca_loadings", "Parameter arrows", min = 0, max = 10, value = 5, step = 1),
            uiOutput("pca_note")
          )
        ),
        h6(class = "mt-3", "Variance explained"),
        DTOutput("pca_variance")
      ),
      nav_panel(
        "Factorial model",
        p(class = "step-intro",
          "For designs where every group combines two factors, such as genotype × infection. Instead of comparing groups two at a time, ",
          "this tests each factor using all animals (e.g. every WT vs every βENaC animal) and whether the effect of one factor depends on the other (the interaction). ",
          "It answers the question the experiment was designed to ask, with more power than pairwise tests."),
        layout_columns(
          col_widths = c(5, 7),
          div(
            card(
              card_header("Design"),
              layout_columns(
                col_widths = c(6, 6),
                textInput("fac_a_name", "Factor A name", value = "Factor A"),
                textInput("fac_b_name", "Factor B name", value = "Factor B")
              ),
              p(class = "choice-help mt-0", "Levels were suggested by splitting group names into the first word and the rest. Edit if needed."),
              uiOutput("fac_levels_ui"),
              uiOutput("fac_design_status")
            ),
            card(
              card_header("Model"),
              radioButtons("fac_model", label_with("Analysis"),
                           choices = c("Mixed model on every session" = "mixed",
                                       "Two-way ANOVA on the summary metric" = "anova"),
                           selected = "mixed"),
              div(class = "choice-help", tags$b("The mixed model is recommended."), " It uses every session with a random effect for each animal, ",
                  "so repeated measurements are handled correctly and time and its interactions are tested too. ",
                  "The two-way ANOVA uses one value per animal (the summary metric chosen in the sidebar)."),
              input_switch("fac_log", "Log-transform values", value = FALSE),
              div(class = "choice-help", "Useful for skewed ratio parameters such as Penh, where effects are proportional. ",
                  "Decide before looking at results, and apply the same choice to all parameters."),
              p(class = "choice-help", "p-values are corrected across the parameters in the table for each term, using the correction chosen under Statistics. ",
                "Correcting across many parameters is strict: if you decided on a few key parameters in advance, keep only those under Appearance."),
              input_switch("fac_raw", "Show uncorrected p-values", value = FALSE)
            )
          ),
          div(
            withSpinner(DTOutput("fac_table"), color = "#1F5F8B"),
            uiOutput("fac_summary"),
            h6(class = "mt-3", "Interaction plot"),
            withSpinner(plotOutput("fac_plot", height = "460px"), color = "#1F5F8B"),
            plot_downloads("dl_fac")
          )
        )
      ),
      nav_panel(
        "Data tables",
        radioButtons("table_choice", NULL, inline = TRUE,
                     choices = c("Per animal and session" = "sessions", "Group mean per timepoint" = "group",
                                 "Summary metric per animal" = "subjects", "All group comparisons" = "comparisons",
                                 "All timepoint tests" = "timepoints")),
        withSpinner(DTOutput("data_table"), color = "#1F5F8B")
      )
    )
  )
)

ml_panel <- nav_panel(
  title = "5. Machine learning", value = "ml",
  layout_sidebar(
    sidebar = sidebar(
      width = 360, title = "Model setup",
      selectizeInput("ml_pos", "Positive class: groups", choices = NULL, multiple = TRUE,
                     options = list(plugins = list("remove_button"))),
      textInput("ml_pos_label", "Positive class name", value = "Positive"),
      selectizeInput("ml_neg", "Negative class: groups", choices = NULL, multiple = TRUE,
                     options = list(plugins = list("remove_button"))),
      textInput("ml_neg_label", "Negative class name", value = "Negative"),
      selectizeInput("ml_exclude", label_with("Leave out timepoints",
                     "Sessions before treatment (e.g. Pre-Infection) look the same in both classes; labeling them positive teaches the model the wrong thing."),
                     choices = NULL, multiple = TRUE, options = list(plugins = list("remove_button"))),
      div(class = "choice-help", tags$b("Leaving out the baseline is recommended"), " when the positive class is defined by a treatment given after it."),
      input_switch("ml_day", "Use study day as a predictor", value = TRUE),
      uiOutput("ml_covariate_ui"),
      div(class = "choice-help", "Predictors are the parameters selected under Appearance (step 4), using the processing settings from step 3."),
      hr(),
      radioButtons("ml_validation", label_with("Validation"),
                   choices = c("Hold out whole animals" = "animal", "Random 80/20 split of sessions (original pipeline)" = "random"),
                   selected = "animal"),
      div(class = "choice-help", tags$b("Holding out whole animals is recommended."), " Each animal's sessions are kept together, so models are always tested on animals ",
          "they have never seen. With a random split, sessions of the same animal are in both training and test data and models can score well by recognizing individual animals."),
      checkboxGroupInput("ml_models", "Models", choices = stats::setNames(names(plethR_ml_names()), plethR_ml_names()),
                         selected = names(plethR_ml_names())),
      radioButtons("ml_tuning", "Tuning", choices = c("Quick" = "quick", "Thorough (original grids, slow)" = "thorough"),
                   selected = "quick", inline = TRUE),
      input_switch("ml_parallel", "Use several CPU cores", value = TRUE),
      numericInput("ml_seed", "Random seed", value = 10, min = 1, step = 1),
      actionButton("ml_run", "Train models", class = "btn-primary w-100")
    ),
    navset_card_underline(
      id = "ml_tabs",
      nav_panel(
        "Performance",
        uiOutput("ml_intro"),
        uiOutput("ml_summary"),
        layout_columns(
          col_widths = c(7, 5),
          div(withSpinner(plotOutput("ml_perf_plot", height = "420px"), color = "#1F5F8B"), plot_downloads("dl_ml_perf")),
          div(h6("Metrics"), DTOutput("ml_metrics_table"))
        )
      ),
      nav_panel(
        "ROC and confusion",
        radioButtons("ml_level", NULL, inline = TRUE,
                     choices = c("Each session" = "session", "Each animal (mean of its sessions)" = "animal")),
        layout_columns(
          col_widths = c(7, 5),
          div(withSpinner(plotOutput("ml_roc_plot", height = "460px"), color = "#1F5F8B"), plot_downloads("dl_ml_roc")),
          div(selectInput("ml_model_cm", "Model", choices = NULL),
              withSpinner(plotOutput("ml_cm_plot", height = "360px"), color = "#1F5F8B"), plot_downloads("dl_ml_cm"))
        )
      ),
      nav_panel(
        "Over time",
        p(class = "choice-help mt-2", "Held-out probability of the positive class for each group at each timepoint. ",
          "If the model detects the condition, positive groups rise above 50% after treatment and negative groups stay below."),
        selectInput("ml_model_time", "Model", choices = NULL),
        withSpinner(plotOutput("ml_time_plot", height = "480px"), color = "#1F5F8B"), plot_downloads("dl_ml_time")
      ),
      nav_panel(
        "Importance",
        p(class = "choice-help mt-2", "Which predictors the final model (trained on all data) relies on. Importance shows what a model uses, ",
          "not whether it generalizes: check the held-out performance first."),
        selectInput("ml_model_imp", "Model", choices = NULL),
        withSpinner(plotOutput("ml_imp_plot", height = "480px"), color = "#1F5F8B"), plot_downloads("dl_ml_imp")
      ),
      nav_panel(
        "Animals",
        p(class = "choice-help mt-2", "Held-out predictions averaged over each animal's sessions."),
        selectInput("ml_model_animals", "Model", choices = NULL),
        DTOutput("ml_animals_table")
      ),
      nav_panel(
        "Predict new data",
        p(class = "step-intro mt-2", "Apply the trained models to a new FinePointe file. The new data are processed with the same settings ",
          "(sessions, summary, Rinx filter, baseline adjustment) as the training data. You can also save trained models and load them later."),
        layout_columns(
          col_widths = c(4, 8),
          div(
            card(card_header("Models"),
                 downloadButton("ml_save", "Save trained models (.rds)", class = "btn-outline-secondary w-100 mb-2", icon = NULL),
                 fileInput("ml_load", "Or load saved models", accept = ".rds"),
                 uiOutput("ml_loaded_info")),
            card(card_header("New data"),
                 fileInput("ml_new_file", "FinePointe export (.xlsx)", accept = ".xlsx"),
                 selectInput("ml_model_new", "Model", choices = NULL),
                 uiOutput("ml_new_covariate_ui"))
          ),
          div(
            uiOutput("ml_new_status"),
            DTOutput("ml_new_table"),
            withSpinner(plotOutput("ml_new_plot", height = "420px"), color = "#1F5F8B"),
            downloadButton("ml_new_download", "Download predictions (.xlsx)", class = "btn-outline-secondary mt-2", icon = NULL)
          )
        )
      )
    )
  )
)

export_panel <- nav_panel(
  title = "6. Export", value = "export",
  div(class = "container-fluid py-3",
    p(class = "step-intro", "Download everything at once. Exports use the settings currently chosen in steps 3 to 5 ",
      "and the parameters selected under Appearance. Machine learning results are included once models are trained."),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(card_header("Excel workbook"),
           p("All tables: per-animal session values, group means, summary metrics, statistics, data check, settings and methods."),
           downloadButton("dl_xlsx", "Download workbook", class = "btn-primary", icon = NULL)),
      card(card_header("All figures (PDF)"),
           p("One page per figure: time course and group comparison for every selected parameter, plus heatmap and PCA. Vector graphics, ready for editing in Illustrator or Inkscape."),
           downloadButton("dl_pdf_all", "Download PDF", class = "btn-primary", icon = NULL)),
      card(card_header("All figures (PNG)"),
           p("The same figures as 300 dpi PNG files in a zip archive, for slides and documents."),
           uiOutput("zip_button"))
    ),
    card(
      card_header("Methods paragraph"),
      p(class = "small text-muted", "A description of the analysis with your current settings, for a methods section. Check and edit before use."),
      div(class = "methods-box", textOutput("methods_text"))
    )
  )
)

guide_panel <- nav_panel(
  title = "Guide", value = "guide",
  div(class = "container py-3", style = "max-width: 70rem;",
    h3("How plethR analyzes whole body plethysmography data"),
    p("FinePointe records many breath-by-breath measurements per animal per session. plethR follows the approach used in published WBP studies:"),
    tags$ol(
      tags$li(tags$b("Summarize each session per animal."), " Each animal gets one value per parameter per session (the median of its records), so outlier breaths have little effect."),
      tags$li(tags$b("Describe groups."), " Group means ± SEM at each timepoint show the time course. n is the number of animals."),
      tags$li(tags$b("Reduce each time course to one number per animal"), " (AUC, time-averaged value, peak, or a single timepoint) to compare overall effects."),
      tags$li(tags$b("Test with the animal as the unit,"), " correcting for multiple comparisons."),
      tags$li(tags$b("Explore patterns"), " across parameters with the heatmap and PCA.")
    ),
    h4(class = "mt-4", "Choosing settings"),
    tags$table(class = "table table-sm glossary",
      tags$thead(tags$tr(tags$th("Setting"), tags$th("Recommended"), tags$th("Why"))),
      tags$tbody(
        tags$tr(tags$td("Sessions"), tags$td("Phase label"), tags$td("Matches the session names you set in FinePointe; ordered by recording time.")),
        tags$tr(tags$td("Session summary"), tags$td("Median"), tags$td("Breath records contain artifacts (sighs, sniffing, movement) with extreme values.")),
        tags$tr(tags$td("Rinx filter"), tags$td("Off"), tags$td("Rinx is often high in normal data; filtering can remove much of it. Use only with a reason, and check the data check table.")),
        tags$tr(tags$td("Baseline"), tags$td("Measured values"), tags$td("Easiest to interpret. Use % of baseline when animals differ a lot before treatment.")),
        tags$tr(tags$td("Summary metric"), tags$td("AUC"), tags$td("Captures the whole time course in one number per animal; avoids testing every timepoint.")),
        tags$tr(tags$td("Test"), tags$td("Welch"), tags$td("Robust to unequal variances; rank tests cannot reach significance with very small groups.")),
        tags$tr(tags$td("Comparisons"), tags$td("vs reference group"), tags$td("Fewer tests, more power, and usually the question being asked.")),
        tags$tr(tags$td("Correction"), tags$td("Holm"), tags$td("Controls false positives with more power than Bonferroni.")),
        tags$tr(tags$td("Error bars"), tags$td("SEM, with animals shown"), tags$td("SEM for the precision of means; showing each animal reveals the spread."))
      )
    ),
    h4(class = "mt-4", "Interpreting Penh"),
    p("Penh (enhanced pause) is widely reported but is an empirical index of breathing pattern, not a direct measure of airway resistance. ",
      "Interpret it together with flow and timing parameters (EF50, Rpef, PEFb, Te) and avoid strong mechanistic claims from Penh alone."),
    h4(class = "mt-4", "Parameter glossary"),
    DTOutput("glossary")
  )
)

ui <- page_navbar(
  id = "steps",
  title = span(icon("lungs"), " plethR"),
  window_title = "plethR",
  theme = app_theme,
  navbar_options = navbar_options(bg = "#1F3A5F", theme = "dark"),
  header = tags$head(tags$style(HTML(app_css))),
  load_panel,
  groups_panel,
  process_panel,
  results_panel,
  ml_panel,
  export_panel,
  nav_spacer(),
  guide_panel,
  nav_item(uiOutput("status_pill"))
)

# ---- Server -----------------------------------------------------------------

server <- function(input, output, session) {

  # load_id makes group inputs unique per file, so selections never leak between files.
  rv <- reactiveValues(raw = NULL, file_name = NULL, saved_assign = list(), tp_levels = NULL,
                       load_id = 0, params_set = NULL)
  grp_id <- function(i) paste0("grp_", rv$load_id, "_", i)

  # -- Navigation ---------------------------------------------------------------
  observeEvent(input$to_groups, nav_select("steps", "groups"))
  observeEvent(input$to_process, nav_select("steps", "process"))
  observeEvent(input$to_results, nav_select("steps", "results"))

  # -- 1. Load ------------------------------------------------------------------
  observeEvent(input$excel_file, {
    f <- input$excel_file
    raw <- withProgress(message = "Reading Excel file", value = 0, {
      tryCatch(
        read_wbp(f$datapath, progress = function(i, n, sheet) {
          setProgress(i / n, detail = sprintf("sheet %d of %d (%s)", i, n, sheet))
        }),
        error = function(e) {
          showNotification(paste("Could not read this file:", conditionMessage(e)), type = "error", duration = NULL)
          NULL
        })
    })
    req(raw)
    subjects <- unique(raw$subject)
    sug <- suggest_groups(subjects)
    gn <- unique(stats::na.omit(sug))
    rv$saved_assign <- if (length(gn) >= 2) lapply(stats::setNames(gn, gn), function(g) subjects[!is.na(sug) & sug == g]) else list()
    if (length(gn) < 2) gn <- c("Group 1", "Group 2")
    rv$load_id <- rv$load_id + 1
    rv$raw <- raw
    rv$file_name <- f$name
    updateTextAreaInput(session, "group_names", value = paste(gn, collapse = "\n"))
    showNotification(sprintf("Loaded %d animals from %s.", length(subjects), f$name), type = "message")
  })

  params_available <- reactive({
    req(rv$raw)
    p <- attr(rv$raw, "parameters")
    p[vapply(p, function(x) any(!is.na(rv$raw[[x]])), logical(1))]
  })

  output$load_message <- renderUI({
    if (is.null(rv$raw)) return(NULL)
    div(class = "alert alert-success py-2 mb-0", " ", rv$file_name, " loaded.")
  })

  output$load_summary <- renderUI({
    if (is.null(rv$raw)) {
      return(p(class = "text-muted", "Upload a file to see the animals, sessions and parameters it contains."))
    }
    raw <- rv$raw
    phases <- unique(stats::na.omit(raw$Phase))
    order_time <- tapply(as.numeric(raw$Time), raw$Phase, stats::median)
    phases <- names(sort(order_time))
    tagList(
      layout_columns(
        fill = FALSE,
        value_box("Animals", length(unique(raw$subject)), theme = "primary"),
        value_box("Sessions", if (length(phases)) length(phases) else length(unique(as.Date(raw$Time))),
                theme = "info"),
        value_box("Parameters", length(params_available()), theme = "success"),
        value_box("Records", format(nrow(raw), big.mark = ","), theme = "secondary")
      ),
      p(tags$b("Sessions in recording order: "),
        if (length(phases)) paste(phases, collapse = " → ") else "no Phase labels found; sessions will be defined by calendar date."),
      p(tags$b("Animals: "), paste(unique(raw$subject), collapse = ", ")),
      p(tags$b("Parameters: "), paste(params_available(), collapse = ", "))
    )
  })

  output$load_next <- renderUI({
    req(rv$raw)
    next_button("to_groups", "Next: assign groups")
  })

  # -- 2. Groups ----------------------------------------------------------------
  group_names <- debounce(reactive({
    g <- trimws(strsplit(input$group_names %||% "", "\n", fixed = TRUE)[[1]])
    unique(g[nzchar(g)])
  }), 500)

  output$group_assign_ui <- renderUI({
    if (is.null(rv$raw)) return(p(class = "text-muted", "Load a file first."))
    g <- group_names()
    if (length(g) == 0) return(p(class = "text-muted", "Type at least one group name."))
    subjects <- unique(rv$raw$subject)
    sug <- suggest_groups(subjects)
    saved <- isolate(rv$saved_assign)
    inputs <- lapply(seq_along(g), function(i) {
      id <- grp_id(i)
      # Keep selections by group name; a renamed group keeps the animals at its position.
      sel <- saved[[g[i]]] %||% isolate(input[[id]]) %||% subjects[!is.na(sug) & sug == g[i]]
      selectizeInput(id, g[i], choices = subjects, selected = sel, multiple = TRUE, width = "100%",
                     options = list(plugins = list("remove_button"), placeholder = "Click to add animals"))
    })
    do.call(layout_column_wrap, c(list(width = "320px", fill = FALSE), inputs))
  })

  # Build the group inputs even if the user skips the Groups tab.
  outputOptions(output, "group_assign_ui", suspendWhenHidden = FALSE)

  assignment <- reactive({
    g <- group_names()
    req(length(g) > 0)
    stats::setNames(lapply(seq_along(g), function(i) input[[grp_id(i)]] %||% character(0)), g)
  })

  observe({
    g <- group_names()
    for (i in seq_along(g)) {
      v <- input[[grp_id(i)]]
      if (!is.null(v)) isolate(rv$saved_assign[[g[i]]] <- v)
    }
  })

  groups_ok <- reactive({
    req(rv$raw)
    a <- assignment()
    a <- a[lengths(a) > 0]
    validate(need(length(a) >= 1, "Assign animals to at least one group in step 2."))
    all_sel <- unlist(a, use.names = FALSE)
    dup <- unique(all_sel[duplicated(all_sel)])
    validate(need(length(dup) == 0, paste("These animals are in more than one group:", paste(dup, collapse = ", "))))
    a
  })

  observe({
    g <- names(groups_ok())
    current <- isolate(input$reference)
    updateSelectInput(session, "reference", choices = g, selected = if (!is.null(current) && current %in% g) current else g[1])
  })

  reference <- reactive({
    r <- input$reference
    if (is.null(r) || !r %in% names(groups_ok())) names(groups_ok())[1] else r
  })

  output$group_warnings <- renderUI({
    req(rv$raw)
    a <- assignment()
    all_sel <- unlist(a, use.names = FALSE)
    unassigned <- setdiff(unique(rv$raw$subject), all_sel)
    dup <- unique(all_sel[duplicated(all_sel)])
    small <- names(a)[lengths(a) > 0 & lengths(a) < 3]
    tagList(
      if (length(dup)) div(class = "alert alert-danger py-2",
                           " In more than one group: ", paste(dup, collapse = ", ")),
      if (length(unassigned)) div(class = "alert alert-warning py-2",
                                  sprintf(" Not in any group (excluded, %d): ", length(unassigned)), paste(unassigned, collapse = ", ")),
      if (length(small)) div(class = "alert alert-warning py-2",
                             " Fewer than 3 animals: ", paste(small, collapse = ", "), ". Statistics need at least 2 per group and are weak below 4."),
      if (!length(dup) && !length(unassigned)) div(class = "alert alert-success py-2", " Every animal is in exactly one group.")
    )
  })

  # -- 3. Process ---------------------------------------------------------------
  sessions_base <- reactive({
    req(rv$raw)
    summarize_sessions(rv$raw, timepoint = input$tp_def, stat = input$session_stat,
                       rinx_max = if (isTRUE(input$use_rinx)) input$rinx_max else NULL)
  })

  observeEvent(sessions_base(), {
    lv <- levels(sessions_base()$timepoint)
    if (!identical(lv, rv$tp_levels)) {
      rv$tp_levels <- lv
      updateSelectizeInput(session, "tp_include", choices = lv, selected = lv)
      updateSelectInput(session, "baseline_tp", choices = lv, selected = lv[1])
    }
  })

  params <- reactive(intersect(params_available(), attr(sessions_base(), "parameters")))

  sessions <- reactive({
    s <- assign_groups(sessions_base(), groups_ok())
    validate(need(nrow(s) > 0, "No data for the assigned animals."))
    tp <- input$tp_include
    validate(need(length(tp) > 0, "Select at least one timepoint in step 3."))
    if (input$baseline_method != "none") {
      req(input$baseline_tp %in% levels(s$timepoint))
      s <- apply_baseline(s, input$baseline_method, input$baseline_tp)
    }
    s <- s[s$timepoint %in% tp, , drop = FALSE]
    s$timepoint <- droplevels(s$timepoint)
    attr(s, "parameters") <- params()
    s
  })

  transform <- reactive(input$baseline_method)

  output$check_table <- renderDT({
    s <- assign_groups(sessions_base(), groups_ok())
    lv <- levels(s$timepoint)
    wide <- tidyr::pivot_wider(s[, c("group", "subject", "timepoint", "n_records")],
                               names_from = "timepoint", values_from = "n_records", values_fill = 0)
    wide <- wide[order(wide$group, wide$subject), c("group", "subject", intersect(lv, names(wide)))]
    names(wide)[1:2] <- c("Group", "Animal")
    typical <- stats::median(s$n_records)
    datatable(wide, rownames = FALSE, class = "compact stripe nowrap small-table",
              options = list(dom = "t", paging = FALSE, scrollX = TRUE, scrollY = "420px", ordering = FALSE)) %>%
      formatStyle(names(wide)[-(1:2)],
                  backgroundColor = styleInterval(c(0.5, typical / 2), c("#f8d7da", "#fff3cd", "white")))
  })

  output$check_flags <- renderUI({
    s <- assign_groups(sessions_base(), groups_ok())
    lv <- levels(s$timepoint)
    missing <- lapply(split(s, s$subject), function(d) setdiff(lv, as.character(d$timepoint)))
    missing <- missing[lengths(missing) > 0]
    typical <- stats::median(s$n_records)
    low <- s[s$n_records < typical / 2, , drop = FALSE]
    items <- c(
      vapply(names(missing), function(a) sprintf("%s has no data for: %s", a, paste(missing[[a]], collapse = ", ")), character(1)),
      if (nrow(low)) sprintf("%s at %s: only %d records (typical: %d)", low$subject, low$timepoint, low$n_records, round(typical))
    )
    if (length(items) == 0) {
      return(div(class = "alert alert-success py-2 mt-3 mb-0",
                 sprintf(" Every animal has data at every timepoint (typical session: %d records).", round(typical))))
    }
    div(class = "alert alert-warning py-2 mt-3 mb-0", " Flags:",
        tags$ul(class = "mb-0", lapply(items, tags$li)))
  })

  # -- 4. Results: shared computations -------------------------------------------
  observeEvent(params(), {
    p <- params()
    if (identical(p, rv$params_set)) return()
    rv$params_set <- p
    sel <- if ("Penh" %in% p) "Penh" else p[1]
    updateSelectInput(session, "param", choices = p, selected = if (isTRUE(input$param %in% p)) input$param else sel)
    updateSelectizeInput(session, "params_multi", choices = p, selected = p)
  })

  output$param_description <- renderUI({
    info <- wbp_parameter_info()
    i <- match(input$param, info$parameter)
    if (is.na(i)) return(NULL)
    div(class = "choice-help", HTML(paste0("<b>", info$name[i], "</b>",
                                           if (nzchar(info$unit[i])) paste0(" (", info$unit[i], ")"), ". ", info$description[i])))
  })

  output$window_ui <- renderUI({
    lv <- levels(sessions()$timepoint)
    if (identical(input$metric, "value")) {
      selectInput("win_from", "Timepoint", choices = lv, selected = isolate(input$win_from) %||% lv[length(lv)])
    } else {
      tagList(
        selectInput("win_from", "From", choices = lv, selected = if (isTRUE(isolate(input$win_from) %in% lv)) isolate(input$win_from) else lv[1]),
        selectInput("win_to", "To", choices = lv, selected = if (isTRUE(isolate(input$win_to) %in% lv)) isolate(input$win_to) else lv[length(lv)])
      )
    }
  })

  window <- reactive({
    lv <- levels(sessions()$timepoint)
    from <- if (isTRUE(input$win_from %in% lv)) input$win_from else lv[1]
    to <- if (identical(input$metric, "value")) from else if (isTRUE(input$win_to %in% lv)) input$win_to else lv[length(lv)]
    if (match(to, lv) < match(from, lv)) to <- from
    list(from = from, to = to)
  })

  colors <- reactive(plethr_colors(names(groups_ok()), input$palette))
  n_groups <- reactive(length(groups_ok()))
  has_comparisons <- reactive(n_groups() >= 2)

  group_sum <- reactive(summarize_groups(sessions(), params()))

  tp_stats <- reactive({
    if (!has_comparisons()) return(NULL)
    compare_timepoints(sessions(), params(), reference(), input$test, input$padj)
  })

  subj_vals <- reactive({
    w <- window()
    summarize_subjects(sessions(), params(), input$metric, w$from, w$to)
  })

  comps <- reactive({
    if (!has_comparisons()) return(NULL)
    compare_groups(subj_vals(), reference = if (input$comp_mode == "reference") reference() else NULL,
                   test = input$test, p_adjust = input$padj)
  })

  metric_label <- function(p) {
    lab <- wbp_axis_label(p, transform())
    w <- window()
    switch(input$metric,
      auc = paste0("AUC of ", lab, " × days"),
      mean = paste0(lab, ", time-averaged"),
      max = paste0("Peak ", lab),
      min = paste0("Minimum ", lab),
      value = paste0(lab, " at ", w$from))
  }

  metric_text <- reactive({
    w <- window()
    span_txt <- if (w$from == w$to) w$from else paste0(w$from, " to ", w$to)
    switch(input$metric,
      auc = paste0("area under the curve (", span_txt, ")"),
      mean = paste0("time-averaged value (", span_txt, ")"),
      max = paste0("peak value (", span_txt, ")"),
      min = paste0("minimum value (", span_txt, ")"),
      value = paste0("value at ", w$from))
  })

  # -- Plot builders ---------------------------------------------------------------
  make_tc <- function(p) {
    plot_timecourse(group_sum(), p, sessions(), colors(), error = input$tc_error, error_style = input$tc_style,
                    x_axis = input$tc_x, show_individuals = isTRUE(input$tc_individuals),
                    stats = if (isTRUE(input$tc_stats)) tp_stats() else NULL, transform = transform())
  }
  make_cmp <- function(p) {
    plot_group_comparison(subj_vals(), p, comparisons = if (isTRUE(input$cmp_stats)) comps() else NULL,
                          colors = colors(), style = input$cmp_style, show_ns = isTRUE(input$cmp_ns),
                          y_label = metric_label(p))
  }
  heatmap_data <- reactive({
    sel <- input$params_multi
    validate(need(length(sel) >= 1, "Select parameters under Appearance."))
    validate(need(has_comparisons(), "The heatmap compares groups, so it needs at least two groups."))
    if (input$hm_mode == "metric") {
      d <- comps()
      d <- d[d$parameter %in% sel, , drop = FALSE]
      d$column <- if (input$comp_mode == "reference") d$group2 else paste(d$group2, "vs", d$group1, recycle0 = TRUE)
      d$column <- factor(d$column, levels = unique(d$column))
      list(d = d, cols = "column", title = paste0("Difference from ", reference(), ": ", metric_text()))
    } else {
      g <- input$hm_group
      validate(need(isTRUE(g %in% setdiff(names(groups_ok()), reference())), "Choose a group to compare with the reference."))
      gs <- group_sum()
      gs <- gs[gs$parameter %in% sel, , drop = FALSE]
      a <- gs[gs$group == g, c("parameter", "timepoint", "mean")]
      b <- gs[gs$group == reference(), c("parameter", "timepoint", "mean")]
      d <- merge(a, b, by = c("parameter", "timepoint"), suffixes = c("", "_ref"))
      d$pct_difference <- ifelse(d$mean_ref == 0, NA_real_, (d$mean - d$mean_ref) / abs(d$mean_ref) * 100)
      st <- tp_stats()
      st <- st[st$group == g, c("parameter", "timepoint", "stars")]
      d <- merge(d, st, by = c("parameter", "timepoint"), all.x = TRUE)
      d$stars[is.na(d$stars)] <- ""
      d$timepoint <- factor(d$timepoint, levels = levels(sessions()$timepoint))
      list(d = d, cols = "timepoint", title = paste0(g, " vs ", reference(), " at each timepoint"))
    }
  })
  make_hm <- function() {
    h <- heatmap_data()
    plot_difference_heatmap(h$d, rows = "parameter", cols = h$cols, cluster_rows = isTRUE(input$hm_cluster), title = h$title)
  }
  pca_result <- reactive({
    sel <- input$params_multi
    validate(need(length(sel) >= 2, "PCA needs at least 2 parameters (see Appearance)."))
    v <- subj_vals()
    v <- v[v$parameter %in% sel, , drop = FALSE]
    tryCatch(plot_subject_pca(v, colors(), show_labels = isTRUE(input$pca_labels), group_shapes = input$pca_shapes,
                              n_loadings = input$pca_loadings, title = paste0("PCA of animals: ", metric_text())),
             error = function(e) validate(need(FALSE, conditionMessage(e))))
  })

  # -- Key findings ----------------------------------------------------------------
  output$summary_boxes <- renderUI({
    s <- sessions()
    n <- table(droplevels(s$group[!duplicated(s$subject)]))
    sig <- if (has_comparisons()) sum(comps()$p_adj < 0.05, na.rm = TRUE) else 0
    layout_columns(
      fill = FALSE,
      value_box("Animals analyzed", sum(n), p(paste(sprintf("%s: %d", names(n), as.integer(n)), collapse = " · ")),
                theme = "primary"),
      value_box("Timepoints", nlevels(s$timepoint), p(paste(levels(s$timepoint)[c(1, nlevels(s$timepoint))], collapse = " → ")),
                theme = "info"),
      value_box("Significant differences", sig, p("adjusted p < 0.05, ", metric_text()),
                theme = if (sig > 0) "success" else "secondary")
    )
  })

  output$power_note <- renderUI({
    req(has_comparisons())
    s <- sessions()
    n <- table(droplevels(s$group[!duplicated(s$subject)]))
    notes <- list()
    if (min(n) < 6) {
      notes[[length(notes) + 1]] <- sprintf("The smallest group has %d animals. With small groups, only large effects reach significance, and a non-significant result does not show the groups are the same. Report effect sizes (%% difference) alongside p-values.", min(n))
    }
    if (input$test == "nonparametric" && min(n) <= 4) {
      notes[[length(notes) + 1]] <- "Rank-based tests with 4 or fewer animals per group cannot reach adjusted p < 0.05 after correction. Welch's t-test is recommended here."
    }
    if (length(notes) == 0) return(NULL)
    div(class = "alert alert-info py-2 mt-2", " ", lapply(notes, function(x) span(x, " ")))
  })

  findings <- reactive({
    req(has_comparisons())
    info <- wbp_parameter_info()
    d <- comps()
    d <- d[!is.na(d$p_adj), , drop = FALSE]
    d <- d[order(d$p_adj, -abs(d$pct_difference)), , drop = FALSE]
    data.frame(
      Parameter = d$parameter,
      Description = ifelse(is.na(match(d$parameter, info$parameter)), "", info$name[match(d$parameter, info$parameter)]),
      Comparison = paste(d$group2, "vs", d$group1, recycle0 = TRUE),
      `Mean (reference)` = signif(d$mean1, 4),
      `Mean (group)` = signif(d$mean2, 4),
      `% difference` = round(d$pct_difference, 1),
      `Adjusted p` = signif(d$p_adj, 3),
      ` ` = d$stars,
      check.names = FALSE
    )
  })

  output$findings_caption <- renderText({
    req(has_comparisons())
    n_sig <- sum(findings()$`Adjusted p` < 0.05)
    sprintf("%sPer-animal %s compared with %s, %s correction within each parameter. Highlighted rows: adjusted p < 0.05. Click a row to open that parameter.",
            if (n_sig == 0) "No comparison reached adjusted p < 0.05; the strongest differences are listed first. " else "",
            metric_text(), test_names[[input$test]], padj_names[[input$padj]])
  })

  output$findings_table <- renderDT({
    validate(need(has_comparisons(), "Statistics need at least two groups."))
    f <- findings()
    validate(need(nrow(f) > 0, "No comparisons could be computed (each group needs at least 2 animals)."))
    datatable(f, rownames = FALSE, selection = "single", class = "compact hover",
              options = list(dom = "ftip", pageLength = 12)) %>%
      formatStyle("% difference", color = styleInterval(0, c("#2166AC", "#B2182B")), fontWeight = "bold") %>%
      formatStyle("Adjusted p", target = "row", backgroundColor = styleInterval(0.05, c("#e3f1e8", "white")))
  })

  observeEvent(input$findings_table_rows_selected, {
    f <- findings()
    p <- f$Parameter[input$findings_table_rows_selected]
    updateSelectInput(session, "param", selected = p)
    nav_select("results_tabs", "Group comparison")
  })

  # -- Time course -----------------------------------------------------------------
  output$tc_plot <- renderPlot(make_tc(req(input$param)), res = 96)

  output$tc_table <- renderDT({
    req(has_comparisons(), input$param)
    gs <- group_sum()
    gs <- gs[gs$parameter == input$param, , drop = FALSE]
    gs$cell <- sprintf("%s ± %s (%d)", signif(gs$mean, 3), ifelse(is.na(gs$sem), "–", signif(gs$sem, 2)), gs$n)
    wide <- tidyr::pivot_wider(gs[, c("timepoint", "group", "cell")], names_from = "group", values_from = "cell")
    st <- tp_stats()
    st <- st[st$parameter == input$param, , drop = FALSE]
    st$cell <- ifelse(is.na(st$p_adj), "", paste0(signif(st$p_adj, 2), ifelse(st$stars == "ns", "", paste0(" ", st$stars))))
    st <- tidyr::pivot_wider(st[, c("timepoint", "group", "cell")], names_from = "group", values_from = "cell",
                             names_glue = "Adj. p: {group}")
    out <- merge(wide, st, by = "timepoint", all.x = TRUE)
    out <- out[order(out$timepoint), , drop = FALSE]
    names(out)[1] <- "Timepoint"
    datatable(out, rownames = FALSE, class = "compact stripe", caption = sprintf(
                "Mean ± SEM (n animals) per group; adjusted p vs %s.", reference()),
              options = list(dom = "t", paging = FALSE, scrollX = TRUE, ordering = FALSE))
  })

  # -- Group comparison ------------------------------------------------------------
  output$cmp_plot <- renderPlot(make_cmp(req(input$param)), res = 96)

  output$cmp_stats_text <- renderUI({
    req(has_comparisons(), input$param)
    d <- comps()
    d <- d[d$parameter == input$param, , drop = FALSE]
    om <- d$p_omnibus[1]
    tagList(
      if (n_groups() >= 3 && !is.na(om)) p(class = "small", tags$b(omnibus_names[[input$test]], ": "),
                                          sprintf("p = %s", format.pval(om, digits = 3, eps = 0.001)),
                                          br(), span(class = "text-muted", "Tests whether any group differs. Pairwise tests show which ones.")),
      p(class = "small text-muted", sprintf("%s, %s correction.", test_names[[input$test]], padj_names[[input$padj]]))
    )
  })

  output$cmp_table <- renderDT({
    req(has_comparisons(), input$param)
    d <- comps()
    d <- d[d$parameter == input$param, , drop = FALSE]
    out <- data.frame(
      Comparison = paste(d$group2, "vs", d$group1, recycle0 = TRUE),
      n = paste(d$n2, d$n1, sep = " vs "),
      `Mean (group)` = d$mean2, `Mean (reference)` = d$mean1,
      Difference = d$difference, `% difference` = d$pct_difference,
      p = d$p, `Adjusted p` = d$p_adj, ` ` = d$stars, check.names = FALSE)
    datatable(out, rownames = FALSE, class = "compact", options = list(dom = "t", paging = FALSE)) %>%
      formatSignif(c("Mean (group)", "Mean (reference)", "Difference", "p", "Adjusted p"), 3) %>%
      formatRound("% difference", 1)
  })

  # -- Heatmap ---------------------------------------------------------------------
  observe({
    others <- setdiff(names(groups_ok()), reference())
    current <- isolate(input$hm_group)
    updateSelectInput(session, "hm_group", choices = others,
                      selected = if (isTRUE(current %in% others)) current else others[length(others)])
  })
  output$hm_plot <- renderPlot(make_hm(), res = 96)

  # -- PCA -------------------------------------------------------------------------
  output$pca_plot <- renderPlot(pca_result()$plot, res = 96)
  output$pca_variance <- renderDT({
    datatable(utils::head(pca_result()$variance, 6), rownames = FALSE, class = "compact",
              options = list(dom = "t")) %>% formatRound(c("Variance (%)", "Cumulative (%)"), 1)
  })
  output$pca_note <- renderUI({
    d <- pca_result()$dropped
    if (length(d) == 0) return(NULL)
    p(class = "small text-warning", "Left out (missing for some animals or constant): ", paste(d, collapse = ", "))
  })

  # -- Data tables -----------------------------------------------------------------
  table_data <- reactive({
    switch(input$table_choice,
      sessions = {
        s <- sessions()
        s$start <- format(s$start, "%Y-%m-%d %H:%M")
        s[, c("group", "subject", "timepoint", "day", "start", "n_records", params())]
      },
      group = group_sum(),
      subjects = subj_vals(),
      comparisons = comps(),
      timepoints = tp_stats())
  })
  output$data_table <- renderDT({
    d <- table_data()
    validate(need(!is.null(d) && nrow(d) > 0, "Nothing to show (statistics need at least two groups)."))
    num <- names(d)[vapply(d, is.double, logical(1)) & !names(d) %in% c("day")]
    datatable(d, rownames = FALSE, filter = "top", class = "compact stripe",
              extensions = "Buttons",
              options = list(pageLength = 25, scrollX = TRUE, dom = "Bfrtip", buttons = c("copy", "csv"))) %>%
      formatSignif(num, 4)
  })

  # -- Per-figure downloads ---------------------------------------------------------------
  register_download <- function(id, name, build, width, height) {
    lapply(c("png", "pdf"), function(type) {
      output[[paste0(id, "_", type)]] <- downloadHandler(
        filename = function() paste0(name(), ".", type),
        content = function(file) save_plot(file, build(), width(), height(), type)
      )
    })
  }
  register_download("dl_tc", function() paste0("timecourse_", safe_name(input$param)),
                    function() make_tc(input$param), function() 9, function() 5.5)
  register_download("dl_cmp", function() paste0("comparison_", input$metric, "_", safe_name(input$param)),
                    function() make_cmp(input$param), function() max(4.5, 1.2 * n_groups() + 2), function() 5.5)
  register_download("dl_hm", function() paste0("heatmap_", input$hm_mode),
                    make_hm, function() 8, function() max(4.5, 0.42 * length(input$params_multi) + 2.5))
  register_download("dl_pca", function() paste0("pca_", input$metric),
                    function() pca_result()$plot, function() 8.5, function() 6.5)

  # -- Factorial model -------------------------------------------------------------
  fac_id <- function(side, g) paste0("fac_", side, "_", safe_name(g))

  output$fac_levels_ui <- renderUI({
    g <- names(groups_ok())
    sug <- suggest_factorial_design(g)
    val <- function(side, i) {
      prev <- isolate(input[[fac_id(side, g[i])]])
      if (!is.null(prev)) return(prev)
      s <- sug[[toupper(side)]][i]
      if (is.na(s)) "" else s
    }
    rows <- lapply(seq_along(g), function(i) {
      layout_columns(
        col_widths = c(4, 4, 4), class = "align-items-center mb-n2",
        div(class = "small fw-semibold", g[i]),
        textInput(fac_id("a", g[i]), NULL, value = val("a", i)),
        textInput(fac_id("b", g[i]), NULL, value = val("b", i))
      )
    })
    tagList(
      layout_columns(col_widths = c(4, 4, 4), div(class = "small text-muted", "Group"),
                     div(class = "small text-muted", textOutput("fac_a_label", inline = TRUE)),
                     div(class = "small text-muted", textOutput("fac_b_label", inline = TRUE))),
      rows
    )
  })
  outputOptions(output, "fac_levels_ui", suspendWhenHidden = FALSE)
  output$fac_a_label <- renderText(fac_names()[1])
  output$fac_b_label <- renderText(fac_names()[2])

  fac_names <- reactive({
    a <- trimws(input$fac_a_name %||% "")
    b <- trimws(input$fac_b_name %||% "")
    c(if (nzchar(a)) a else "Factor A", if (nzchar(b)) b else "Factor B")
  })

  fac_design <- reactive({
    g <- names(groups_ok())
    data.frame(
      group = g,
      A = vapply(g, function(x) trimws(input[[fac_id("a", x)]] %||% ""), character(1), USE.NAMES = FALSE),
      B = vapply(g, function(x) trimws(input[[fac_id("b", x)]] %||% ""), character(1), USE.NAMES = FALSE),
      stringsAsFactors = FALSE
    )
  })

  fac_problem <- reactive({
    d <- fac_design()
    if (any(!nzchar(d$A) | !nzchar(d$B))) return("Give every group a level for both factors.")
    if (length(unique(d$A)) < 2 || length(unique(d$B)) < 2) return("Each factor needs at least 2 levels.")
    if (anyDuplicated(paste(d$A, d$B, sep = "\r"))) return("Two groups have the same combination of levels.")
    if (nrow(d) != length(unique(d$A)) * length(unique(d$B))) {
      return("Not every combination of levels has a group, so the interaction cannot be tested.")
    }
    NULL
  })

  output$fac_design_status <- renderUI({
    msg <- fac_problem()
    d <- fac_design()
    if (!is.null(msg)) return(div(class = "alert alert-warning py-2 mb-0", msg))
    div(class = "alert alert-success py-2 mb-0",
        sprintf("%d × %d design: %s (%s) × %s (%s).", length(unique(d$A)), length(unique(d$B)),
                fac_names()[1], paste(unique(d$A), collapse = ", "), fac_names()[2], paste(unique(d$B), collapse = ", ")))
  })

  fac_results <- reactive({
    validate(need(is.null(fac_problem()), fac_problem()))
    sel <- input$params_multi
    validate(need(length(sel) >= 1, "Select parameters under Appearance."))
    res <- withProgress(message = "Fitting models", value = 0.5, {
      if (input$fac_model == "mixed") {
        factorial_mixed_model(sessions(), fac_design(), sel, fac_names(), isTRUE(input$fac_log))
      } else {
        factorial_anova(subj_vals(), fac_design(), sel, fac_names(), isTRUE(input$fac_log))
      }
    })
    res$p_adj <- NA_real_
    ok <- !is.na(res$term)
    res$p_adj[ok] <- stats::ave(res$p[ok], res$term[ok], FUN = function(p) stats::p.adjust(p, method = input$padj))
    res
  })

  output$fac_table <- renderDT({
    r <- fac_results()
    r <- r[!is.na(r$term), , drop = FALSE]
    terms <- unique(r$term)
    raw <- isTRUE(input$fac_raw)
    r$shown <- if (raw) r$p else r$p_adj
    wide <- as.data.frame(tidyr::pivot_wider(r[, c("parameter", "term", "shown")], names_from = "term", values_from = "shown"))
    names(wide)[1] <- "Parameter"
    datatable(wide, rownames = FALSE, selection = "single", class = "compact hover",
              caption = sprintf("%s p-values for each term (%s). Green: p < 0.05. Click a row for details.",
                                if (input$fac_model == "mixed") "Mixed model" else "Two-way ANOVA",
                                if (raw) "uncorrected" else paste(padj_names[[input$padj]], "correction across parameters")),
              options = list(dom = "t", paging = FALSE, scrollX = TRUE, ordering = FALSE)) %>%
      formatSignif(terms, 3) %>%
      formatStyle(terms, backgroundColor = styleInterval(0.05, c("#d4edda", "white")),
                  fontWeight = styleInterval(0.05, c("bold", "normal")))
  })

  observeEvent(input$fac_table_rows_selected, {
    r <- fac_results()
    p <- unique(r$parameter)[input$fac_table_rows_selected]
    updateSelectInput(session, "param", selected = p)
  })

  output$fac_summary <- renderUI({
    r <- fac_results()
    prm <- input$param
    r <- r[r$parameter == prm, , drop = FALSE]
    if (nrow(r) == 0) return(p(class = "text-muted small mt-2", "Select a parameter included in the table (sidebar)."))
    if (all(is.na(r$term))) return(div(class = "alert alert-warning py-2 mt-2", "The model could not be fitted for ", prm, "."))
    fn <- fac_names()
    explain <- function(term) {
      has <- function(x) grepl(x, term, fixed = TRUE)
      n_parts <- length(strsplit(term, " × ", fixed = TRUE)[[1]])
      if (term == "Time") return(paste0(prm, " changes over time."))
      if (n_parts == 1) return(paste0(term, " has an overall effect on ", prm, ", averaged over the other factor",
                                      if (input$fac_model == "mixed") " and time" else "", "."))
      if (n_parts == 2 && has("Time")) return(paste0("The effect of ", sub(" × Time", "", term, fixed = TRUE), " on ", prm, " changes over time."))
      if (n_parts == 2) return(paste0("The effect of ", fn[1], " on ", prm, " depends on ", fn[2], " (interaction)."))
      paste0("The ", fn[1], " × ", fn[2], " interaction changes over time.")
    }
    sig <- r[!is.na(r$p_adj) & r$p_adj < 0.05, , drop = FALSE]
    div(class = "mt-2",
      p(tags$b(prm, ": "), if (nrow(sig) == 0) "no term reached adjusted p < 0.05." else "significant terms:"),
      if (nrow(sig) > 0) tags$ul(lapply(seq_len(nrow(sig)), function(i)
        tags$li(sprintf("%s (F(%g, %g) = %.2f, adjusted p = %s): %s", sig$term[i], sig$num_df[i], round(sig$den_df[i]),
                        sig$F[i], format.pval(sig$p_adj[i], digits = 2, eps = 1e-4), explain(sig$term[i]))))),
      p(class = "choice-help", "Main effects are hard to interpret when their interaction is significant: then describe each combination of groups instead.")
    )
  })

  make_fac <- function(p) {
    d <- fac_design()
    validate(need(is.null(fac_problem()), fac_problem()))
    plot_interaction(subj_vals(), d, p, fac_names(),
                     colors = plethr_colors(unique(d$B), input$palette), y_label = metric_label(p))
  }
  output$fac_plot <- renderPlot(make_fac(req(input$param)), res = 96)
  register_download("dl_fac", function() paste0("interaction_", safe_name(input$param)),
                    function() make_fac(input$param), function() 6.5, function() 5)

  fac_valid <- function() isTRUE(tryCatch(is.null(fac_problem()), error = function(e) FALSE))

  # -- 5. Machine learning ------------------------------------------------------------
  ml_res <- reactiveVal(NULL)
  ml_state <- reactiveValues(groups = NULL, tp = NULL)

  # Default classes: the factor level that differs from the reference group (e.g. Infected vs Uninfected).
  observe({
    g <- names(groups_ok())
    if (identical(g, ml_state$groups)) return()
    ml_state$groups <- g
    ref <- reference()
    d <- suggest_factorial_design(g)
    if (!all(is.na(d$A)) && length(unique(d$A)) == 2) {
      neg_lv <- d$A[d$group == ref]
      pos_lv <- setdiff(unique(d$A), neg_lv)
      pos <- d$group[d$A == pos_lv]; neg <- d$group[d$A == neg_lv]; labs <- c(pos_lv, neg_lv)
    } else {
      pos <- setdiff(g, ref); neg <- ref
      labs <- c(if (length(pos) == 1) pos else "Positive", ref)
    }
    updateSelectizeInput(session, "ml_pos", choices = g, selected = pos)
    updateSelectizeInput(session, "ml_neg", choices = g, selected = neg)
    updateTextInput(session, "ml_pos_label", value = labs[1])
    updateTextInput(session, "ml_neg_label", value = labs[2])
  })

  observe({
    lv <- levels(sessions()$timepoint)
    if (identical(lv, ml_state$tp)) return()
    ml_state$tp <- lv
    updateSelectizeInput(session, "ml_exclude", choices = lv, selected = lv[1])
  })

  # If the classes are the two levels of one factor, the other factor (e.g. genotype) can be a predictor.
  ml_cov <- reactive({
    if (!fac_valid()) return(NULL)
    d <- fac_design()
    pos <- input$ml_pos; neg <- input$ml_neg
    for (k in c("A", "B")) {
      other <- setdiff(c("A", "B"), k)
      lp <- unique(d[[k]][d$group %in% pos]); ln <- unique(d[[k]][d$group %in% neg])
      if (length(lp) == 1 && length(ln) == 1 && lp != ln && setequal(c(pos, neg), d$group)) {
        return(list(name = fac_names()[if (other == "A") 1 else 2], map = stats::setNames(d[[other]], d$group)))
      }
    }
    NULL
  })

  output$ml_covariate_ui <- renderUI({
    cv <- ml_cov()
    if (is.null(cv)) return(NULL)
    tagList(input_switch("ml_use_cov", sprintf("Use %s (%s) as a predictor", cv$name, paste(unique(cv$map), collapse = ", ")), value = TRUE),
            div(class = "choice-help", sprintf("%s (%s) is known for every animal, as in the original pipeline.",
                                               cv$name, paste(unique(cv$map), collapse = ", "))))
  })

  observeEvent(input$ml_run, {
    pos_label <- trimws(input$ml_pos_label); neg_label <- trimws(input$ml_neg_label)
    if (!nzchar(pos_label)) pos_label <- "Positive"
    if (!nzchar(neg_label)) neg_label <- "Negative"
    res <- tryCatch({
      ml_check_packages()
      if (length(input$ml_models) == 0) stop("Select at least one model.")
      if (length(input$params_multi) < 2) stop("Select at least 2 parameters under Appearance (step 4).")
      cv <- if (isTRUE(input$ml_use_cov)) ml_cov() else NULL
      d <- ml_prepare(sessions(), input$ml_pos, input$ml_neg, labels = make.names(c(pos_label, neg_label)),
                      parameters = input$params_multi, exclude_timepoints = input$ml_exclude,
                      include_day = isTRUE(input$ml_day), covariate = cv$map,
                      covariate_name = if (is.null(cv)) "covariate" else make.names(cv$name))
      withProgress(message = "Training models", value = 0, {
        ml_fit(d, models = input$ml_models, validation = input$ml_validation, tuning = input$ml_tuning,
               seed = input$ml_seed, parallel = isTRUE(input$ml_parallel),
               progress = function(i, n, name) setProgress((i - 1) / n, detail = sprintf("%s (%d of %d)", name, i, n)))
      })
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = NULL)
      NULL
    })
    if (is.null(res)) return()
    st <- attr(sessions_base(), "settings")
    res$processing <- list(timepoint = st$timepoint, stat = st$stat, rinx_max = st$rinx_max,
                           baseline = input$baseline_method, source = rv$file_name)
    ml_res(res)
    showNotification(sprintf("Trained %d model(s).", length(res$fits)), type = "message")
  })

  observeEvent(ml_res(), {
    obj <- ml_res()
    m <- names(obj$fits)
    nm <- stats::setNames(m, ml_model_names()[m])
    best <- obj$metrics$model[obj$metrics$level == "session"][which.max(obj$metrics$roc_auc[obj$metrics$level == "session"])]
    for (id in c("ml_model_cm", "ml_model_time", "ml_model_animals", "ml_model_new")) updateSelectInput(session, id, choices = nm, selected = best)
    imp_models <- intersect(m, unique(obj$importance$model))
    updateSelectInput(session, "ml_model_imp", choices = stats::setNames(imp_models, ml_model_names()[imp_models]),
                      selected = if (best %in% imp_models) best else imp_models[1])
  })

  ml_need <- function() validate(need(!is.null(ml_res()), "Choose the settings on the left and click \"Train models\"."))

  output$ml_intro <- renderUI({
    if (!is.null(ml_res())) return(NULL)
    div(class = "step-intro mt-2",
      p("Train classification models that predict which class an animal belongs to (for example infected vs uninfected) from its breathing parameters. ",
        "This is the CP05 infection-prediction pipeline: logistic regression, k-nearest neighbors, linear and quadratic discriminant analysis, elastic net, ",
        "random forest and gradient-boosted trees, with centered and scaled predictors, upsampling of the smaller class, and tuning by ROC AUC."),
      p("Each row is one animal at one session. Performance is reported for single sessions and for whole animals (averaging each animal's predicted probabilities)."),
      p(class = "text-muted", "Training takes about 1 minute with Quick tuning on several cores, and much longer with Thorough tuning."))
  })

  output$ml_summary <- renderUI({
    ml_need()
    obj <- ml_res()
    m <- obj$metrics
    ses <- m[m$level == "session", ]; ani <- m[m$level == "animal", ]
    best <- ses$model[which.max(ses$roc_auc)]
    b_s <- ses[ses$model == best, ]; b_a <- ani[ani$model == best, ]
    n_an <- length(unique(obj$data$subject))
    random <- obj$settings$validation == "random"
    verdict <- if (random) {
      div(class = "alert alert-warning py-2",
          tags$b("Random split: likely optimistic. "), "Sessions of the same animal are in both training and test data, so models can recognize ",
          "individual animals instead of the condition. Re-run with \"Hold out whole animals\" to see how well the models work on new animals.")
    } else if (b_a$roc_auc >= 0.8) {
      div(class = "alert alert-success py-2", sprintf("Good separation of %s and %s in animals the models had not seen (best animal-level ROC AUC %.2f). ",
                                                       obj$settings$labels[1], obj$settings$labels[2], b_a$roc_auc),
          sprintf("With %d animals the estimate is uncertain; confirm in an independent cohort.", n_an))
    } else if (b_a$roc_auc >= 0.65) {
      div(class = "alert alert-info py-2", sprintf("Moderate separation in held-out animals (best animal-level ROC AUC %.2f). ", b_a$roc_auc),
          "Treat as preliminary.")
    } else {
      div(class = "alert alert-secondary py-2", tags$b("The models did not reliably tell the classes apart in animals they had not seen "),
          sprintf("(best animal-level ROC AUC %.2f; 0.5 is chance). ", b_a$roc_auc),
          "Breathing parameters may not carry a consistent signal for this outcome, or more animals are needed.")
    }
    tagList(
      layout_columns(
        fill = FALSE,
        value_box("Best model", ml_model_names()[[best]], p(sprintf("highest session-level ROC AUC (%s)", if (random) "test sessions" else "held-out animals")), theme = "primary"),
        value_box("Session ROC AUC", sprintf("%.2f", b_s$roc_auc), p(sprintf("accuracy %.0f%%, %d sessions", 100 * b_s$accuracy, b_s$n)), theme = "info"),
        value_box("Animal ROC AUC", sprintf("%.2f", b_a$roc_auc), p(sprintf("accuracy %.0f%%, %d animals", 100 * b_a$accuracy, b_a$n)), theme = "secondary")
      ),
      verdict,
      if (length(obj$errors)) div(class = "alert alert-warning py-2", "Could not fit: ",
                                  paste(sprintf("%s (%s)", ml_model_names()[names(obj$errors)], unlist(obj$errors)), collapse = "; "))
    )
  })

  output$ml_perf_plot <- renderPlot({ ml_need(); plot_ml_performance(ml_res()) }, res = 96)

  output$ml_metrics_table <- renderDT({
    ml_need()
    obj <- ml_res()
    m <- obj$metrics
    out <- data.frame(Model = m$model_name, Level = m$level, `ROC AUC` = m$roc_auc, Accuracy = m$accuracy,
                      Sensitivity = m$sensitivity, Specificity = m$specificity, n = m$n, check.names = FALSE)
    out$`Tuned parameters` <- ifelse(m$level == "session", obj$tuning$parameters[match(m$model, obj$tuning$model)], "")
    datatable(out, rownames = FALSE, class = "compact small-table", options = list(dom = "t", paging = FALSE, scrollX = TRUE)) %>%
      formatRound(c("ROC AUC", "Accuracy", "Sensitivity", "Specificity"), 2)
  })

  output$ml_roc_plot <- renderPlot({ ml_need(); plot_ml_roc(ml_res(), input$ml_level) }, res = 96)
  output$ml_cm_plot <- renderPlot({ ml_need(); req(input$ml_model_cm); plot_ml_confusion(ml_res(), input$ml_model_cm, input$ml_level) }, res = 96)
  ml_time_colors <- reactive({
    g <- names(groups_ok())
    plethr_colors(g, input$palette)
  })
  output$ml_time_plot <- renderPlot({ ml_need(); req(input$ml_model_time); plot_ml_over_time(ml_res(), input$ml_model_time, ml_time_colors()) }, res = 96)
  output$ml_imp_plot <- renderPlot({
    ml_need()
    validate(need(isTRUE(nzchar(input$ml_model_imp)), "Variable importance is available for logistic regression, elastic net, random forest and boosted trees."))
    plot_ml_importance(ml_res(), input$ml_model_imp)
  }, res = 96)

  output$ml_animals_table <- renderDT({
    ml_need(); req(input$ml_model_animals)
    obj <- ml_res()
    p <- obj$predictions[obj$predictions$model == input$ml_model_animals, , drop = FALSE]
    a <- stats::aggregate(prob ~ subject + group + outcome, data = p, FUN = mean)
    a$n <- as.vector(table(p$subject)[a$subject])
    a$predicted <- ifelse(a$prob >= 0.5, obj$settings$labels[1], obj$settings$labels[2])
    a$correct <- ifelse(a$predicted == as.character(a$outcome), "yes", "no")
    a <- a[order(a$group, a$subject), c("group", "subject", "outcome", "prob", "predicted", "correct", "n")]
    names(a) <- c("Group", "Animal", "Actual", paste0("P(", obj$settings$labels[1], ")"), "Predicted", "Correct", "Sessions")
    datatable(a, rownames = FALSE, class = "compact", options = list(dom = "t", paging = FALSE)) %>%
      formatRound(4, 2) %>%
      formatStyle("Correct", color = styleEqual(c("yes", "no"), c("#2E8B57", "#B2182B")), fontWeight = "bold")
  })

  register_download("dl_ml_perf", function() "ml_performance", function() plot_ml_performance(ml_res()), function() 8, function() 5)
  register_download("dl_ml_roc", function() paste0("ml_roc_", input$ml_level), function() plot_ml_roc(ml_res(), input$ml_level),
                    function() 8, function() 6)
  register_download("dl_ml_cm", function() paste0("ml_confusion_", input$ml_model_cm),
                    function() plot_ml_confusion(ml_res(), input$ml_model_cm, input$ml_level), function() 5, function() 4.5)
  register_download("dl_ml_time", function() paste0("ml_over_time_", input$ml_model_time),
                    function() plot_ml_over_time(ml_res(), input$ml_model_time, ml_time_colors()), function() 9, function() 5.5)
  register_download("dl_ml_imp", function() paste0("ml_importance_", input$ml_model_imp),
                    function() plot_ml_importance(ml_res(), input$ml_model_imp), function() 7, function() 5.5)

  # Save / load trained models
  output$ml_save <- downloadHandler(
    filename = function() paste0("plethR_models_", format(Sys.Date(), "%Y%m%d"), ".rds"),
    content = function(file) {
      validate(need(!is.null(ml_res()), "Train models first."))
      saveRDS(ml_res(), file)
    }
  )
  observeEvent(input$ml_load, {
    obj <- tryCatch(readRDS(input$ml_load$datapath), error = function(e) NULL)
    if (!inherits(obj, "plethr_ml")) {
      showNotification("That file does not contain plethR models.", type = "error")
      return()
    }
    ml_res(obj)
    showNotification("Models loaded.", type = "message")
  })
  output$ml_loaded_info <- renderUI({
    obj <- ml_res()
    if (is.null(obj)) return(p(class = "small text-muted", "No models trained or loaded yet."))
    s <- obj$settings
    p(class = "small", sprintf("%s vs %s; %d models; trained on %s (%d animals); predictors: %s%s%s.",
                               s$labels[1], s$labels[2], length(obj$fits), obj$processing$source %||% "unknown file",
                               length(unique(obj$data$subject)), paste(s$parameters, collapse = ", "),
                               if (isTRUE(s$include_day)) ", study day" else "",
                               if (!is.null(s$covariate)) paste0(", ", s$covariate_name) else ""))
  })

  # Predict new data
  ml_new_sessions <- reactive({
    req(input$ml_new_file)
    obj <- ml_res()
    validate(need(!is.null(obj), "Train or load models first."))
    pr <- obj$processing
    raw <- withProgress(message = "Reading new file", value = 0, {
      read_wbp(input$ml_new_file$datapath, progress = function(i, n, sheet) setProgress(i / n, detail = sheet))
    })
    s <- summarize_sessions(raw, timepoint = pr$timepoint %||% "phase", stat = pr$stat %||% "median", rinx_max = pr$rinx_max)
    if (!is.null(pr$baseline) && pr$baseline != "none") s <- apply_baseline(s, pr$baseline)
    s
  })

  output$ml_new_covariate_ui <- renderUI({
    obj <- ml_res()
    req(obj, !is.null(obj$settings$covariate))
    subj <- unique(ml_new_sessions()$subject)
    lv <- sort(unique(unname(obj$settings$covariate)))
    tagList(
      p(class = "small", sprintf("These models use %s. Assign each new animal:", obj$settings$covariate_name)),
      lapply(seq_along(lv), function(i) {
        guess <- subj[grepl(lv[i], subj, ignore.case = TRUE)]
        selectizeInput(paste0("ml_newcov_", i), lv[i], choices = subj, selected = guess, multiple = TRUE,
                       options = list(plugins = list("remove_button")))
      })
    )
  })

  ml_new_pred <- reactive({
    obj <- ml_res()
    s <- ml_new_sessions()
    m <- input$ml_model_new
    validate(need(isTRUE(m %in% names(obj$fits)), "Choose a model."))
    cov <- NULL
    if (!is.null(obj$settings$covariate)) {
      lv <- sort(unique(unname(obj$settings$covariate)))
      sel <- lapply(seq_along(lv), function(i) input[[paste0("ml_newcov_", i)]])
      cov <- unlist(lapply(seq_along(lv), function(i) stats::setNames(rep(lv[i], length(sel[[i]])), sel[[i]])))
      missing <- setdiff(unique(s$subject), names(cov))
      validate(need(length(missing) == 0, paste("Assign these animals a", obj$settings$covariate_name, "level:", paste(missing, collapse = ", "))))
    }
    tryCatch(ml_predict(obj, s, m, covariate = cov), error = function(e) validate(need(FALSE, conditionMessage(e))))
  })

  output$ml_new_status <- renderUI({
    req(input$ml_new_file)
    pr <- ml_new_pred()
    lab <- ml_res()$settings$labels
    n_pos <- sum(pr$animals$predicted == lab[1])
    div(class = "alert alert-info py-2",
        sprintf("%d animals: %d predicted %s, %d predicted %s (%s). ", nrow(pr$animals), n_pos, lab[1],
                nrow(pr$animals) - n_pos, lab[2], ml_model_names()[[input$ml_model_new]]),
        "Predictions are only as reliable as the held-out performance on the Performance tab.")
  })

  output$ml_new_table <- renderDT({
    req(input$ml_new_file)
    a <- ml_new_pred()$animals
    names(a)[names(a) == "subject"] <- "Animal"
    datatable(a, rownames = FALSE, class = "compact", options = list(dom = "t", paging = FALSE)) %>% formatRound(2, 2)
  })

  ml_new_plot <- function() {
    pr <- ml_new_pred()
    lab <- ml_res()$settings$labels
    s <- pr$sessions
    s$x <- as.integer(droplevels(s$timepoint))
    lv <- levels(droplevels(s$timepoint))
    ggplot(s, aes(x = .data$x, y = .data$prob, group = .data$subject, color = .data$subject)) +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey55") +
      geom_line(linewidth = 0.7, alpha = 0.8) + geom_point(size = 1.8) +
      scale_x_continuous(breaks = seq_along(lv), labels = lv) +
      scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(x * 100, "%")) +
      labs(title = paste0("Predicted probability of \"", lab[1], "\" for each new animal"), x = NULL, y = paste0("P(", lab[1], ")"),
           color = NULL, subtitle = ml_model_names()[[input$ml_model_new]]) +
      theme_plethr() +
      theme(legend.position = "right", axis.text.x = element_text(angle = if (length(lv) > 6) 45 else 0, hjust = if (length(lv) > 6) 1 else 0.5))
  }
  output$ml_new_plot <- renderPlot({ req(input$ml_new_file); ml_new_plot() }, res = 96)

  output$ml_new_download <- downloadHandler(
    filename = function() paste0(safe_name(tools::file_path_sans_ext(input$ml_new_file$name %||% "new")), "_predictions.xlsx"),
    content = function(file) {
      pr <- ml_new_pred()
      writexl::write_xlsx(list(Animals = pr$animals, Sessions = pr$sessions), path = file)
    }
  )

  # -- 6. Export ---------------------------------------------------------------------
  all_figures <- function() {
    sel <- input$params_multi
    figs <- list()
    for (p in sel) {
      figs[[paste0("timecourse_", safe_name(p))]] <- list(plot = make_tc(p), w = 9, h = 5.5)
      figs[[paste0("comparison_", safe_name(p))]] <- list(plot = make_cmp(p), w = max(4.5, 1.2 * n_groups() + 2), h = 5.5)
      if (fac_valid()) figs[[paste0("interaction_", safe_name(p))]] <- list(plot = make_fac(p), w = 6.5, h = 5)
    }
    if (has_comparisons()) {
      figs[["heatmap"]] <- list(plot = tryCatch(make_hm(), error = function(e) NULL), w = 8,
                                h = max(4.5, 0.42 * length(sel) + 2.5))
    }
    pca <- tryCatch(pca_result()$plot, error = function(e) NULL)
    figs[["pca"]] <- list(plot = pca, w = 8.5, h = 6.5)
    obj <- ml_res()
    if (!is.null(obj)) {
      ses <- obj$metrics[obj$metrics$level == "session", ]
      best <- ses$model[which.max(ses$roc_auc)]
      figs[["ml_performance"]] <- list(plot = plot_ml_performance(obj), w = 8, h = 5)
      figs[["ml_roc"]] <- list(plot = plot_ml_roc(obj, "session"), w = 8, h = 6)
      figs[["ml_over_time"]] <- list(plot = plot_ml_over_time(obj, best, ml_time_colors()), w = 9, h = 5.5)
      if (best %in% obj$importance$model) figs[["ml_importance"]] <- list(plot = plot_ml_importance(obj, best), w = 7, h = 5.5)
    }
    figs[!vapply(figs, function(f) is.null(f$plot), logical(1))]
  }

  output$dl_pdf_all <- downloadHandler(
    filename = function() paste0(safe_name(tools::file_path_sans_ext(rv$file_name %||% "plethR")), "_figures.pdf"),
    content = function(file) {
      withProgress(message = "Building figures", value = 0, {
        figs <- all_figures()
        dev <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf
        dev(file, width = 9, height = 6.5, onefile = TRUE)
        for (i in seq_along(figs)) {
          incProgress(1 / length(figs))
          print(figs[[i]]$plot)
        }
        grDevices::dev.off()
      })
    }
  )

  output$zip_button <- renderUI({
    if (requireNamespace("zip", quietly = TRUE)) {
      downloadButton("dl_png_zip", "Download zip", class = "btn-primary", icon = NULL)
    } else {
      p(class = "text-muted small", "Install the zip package to enable this: install.packages(\"zip\")")
    }
  })

  output$dl_png_zip <- downloadHandler(
    filename = function() paste0(safe_name(tools::file_path_sans_ext(rv$file_name %||% "plethR")), "_figures.zip"),
    content = function(file) {
      dir <- file.path(tempdir(), paste0("plethR_png_", as.integer(Sys.time())))
      dir.create(dir)
      withProgress(message = "Saving figures", value = 0, {
        figs <- all_figures()
        for (n in names(figs)) {
          incProgress(1 / length(figs))
          save_plot(file.path(dir, paste0(n, ".png")), figs[[n]]$plot, figs[[n]]$w, figs[[n]]$h, "png")
        }
      })
      zip::zip(file, files = list.files(dir), root = dir)
    }
  )

  methods_text <- reactive({
    s <- sessions()
    n <- table(droplevels(s$group[!duplicated(s$subject)]))
    st <- attr(sessions_base(), "settings")
    tp_txt <- if (st$timepoint == "phase") "session (FinePointe phase label)" else "recording date"
    paste0(
      "Whole body plethysmography data were exported from DSI FinePointe and analyzed with plethR (version ", app_version, "). ",
      if (!is.null(st$rinx_max)) sprintf("Records with a rejection index (Rinx) above %s%% were excluded. ", st$rinx_max) else "",
      "For each animal, records were summarized as the ", st$stat, " of each parameter per ", tp_txt, ". ",
      switch(transform(),
        none = "",
        percent = sprintf("Values were expressed as a percentage of each animal's baseline (%s). ", input$baseline_tp),
        difference = sprintf("Values were expressed as the change from each animal's baseline (%s). ", input$baseline_tp)),
      sprintf("Data are presented as mean ± SEM with the animal as the experimental unit (n = %s animals per group; %s). ",
              if (min(n) == max(n)) min(n) else paste0(min(n), "–", max(n)),
              paste(sprintf("%s, n = %d", names(n), as.integer(n)), collapse = "; ")),
      if (input$metric == "auc") "For each animal, the area under the curve was calculated by the trapezoidal rule over study days " else "For each animal, the ",
      if (input$metric == "auc") paste0("(", window()$from, " to ", window()$to, "). ") else paste0(metric_text(), " was calculated. "),
      if (has_comparisons()) paste0(
        if (input$comp_mode == "reference") paste0("Each group was compared with the ", reference(), " group") else "All pairs of groups were compared",
        " using ", test_names[[input$test]], "s",
        if (n_groups() >= 3) paste0(", with ", omnibus_names[[input$test]], " as an omnibus test") else "",
        if (input$padj == "none") "; p-values were not adjusted for multiple comparisons. "
        else paste0("; p-values were adjusted for multiple comparisons with the ", padj_names[[input$padj]], " method within each parameter. "),
        "Differences at individual timepoints were tested with ", test_names[[input$test]], "s against the ",
        reference(), " group, ", if (input$padj == "none") "without adjustment" else paste0("adjusted across timepoints and groups (", padj_names[[input$padj]], ")"),
        ". Adjusted p < 0.05 was considered significant. "
      ) else "",
      if (fac_valid()) {
        fn <- fac_names()
        lg <- if (isTRUE(input$fac_log)) "log-transformed " else ""
        if (input$fac_model == "mixed") {
          sprintf(paste0("Effects of %s, %s and time were tested with a linear mixed model on %ssession values, with %s, %s, time and all their ",
                         "interactions as fixed effects and a random intercept for each animal (R package nlme; marginal F-tests with sum-to-zero contrasts%s). "),
                  fn[1], fn[2], lg, fn[1], fn[2],
                  if (input$padj == "none") "" else paste0("; p-values adjusted across parameters with the ", padj_names[[input$padj]], " method"))
        } else {
          sprintf("Effects of %s and %s on the per-animal %s%s were tested by two-way ANOVA with their interaction (type III F-tests%s). ",
                  fn[1], fn[2], lg, metric_text(),
                  if (input$padj == "none") "" else paste0("; p-values adjusted across parameters with the ", padj_names[[input$padj]], " method"))
        }
      } else "",
      "Principal component analysis was performed on centered and scaled per-animal values.",
      if (!is.null(ml_res())) {
        s <- ml_res()$settings
        paste0(
          " To classify ", s$labels[1], " versus ", s$labels[2], " animals, ", length(ml_res()$fits),
          " models (", paste(tolower(ml_model_names()[names(ml_res()$fits)]), collapse = ", "),
          ") were trained with tidymodels on session-level values of ", length(s$parameters), " parameters",
          if (isTRUE(s$include_day)) " and study day" else "", if (!is.null(s$covariate)) paste0(" and ", s$covariate_name) else "",
          if (length(s$exclude_timepoints)) paste0(", excluding ", paste(s$exclude_timepoints, collapse = ", ")) else "",
          ". Predictors were centered and scaled", if (isTRUE(s$upsample)) " and the smaller class was upsampled in training data" else "",
          "; hyperparameters were tuned by ROC AUC. ",
          if (s$validation == "animal") sprintf("Performance was estimated by %d-fold cross-validation in which all sessions of an animal were held out together, at the level of single sessions and of animals (mean predicted probability).", s$folds)
          else "Sessions were split at random into 80% training (tuned by 10-fold cross-validation) and 20% test data; because sessions of the same animal occur in both sets, this estimate is likely optimistic."
        )
      } else ""
    )
  })
  output$methods_text <- renderText(methods_text())

  output$dl_xlsx <- downloadHandler(
    filename = function() paste0(safe_name(tools::file_path_sans_ext(rv$file_name %||% "plethR")), "_plethR_results.xlsx"),
    content = function(file) {
      s <- sessions()
      s$start <- format(s$start, "%Y-%m-%d %H:%M")
      sel <- input$params_multi
      st <- attr(sessions_base(), "settings")
      settings <- data.frame(
        Setting = c("Source file", "plethR version", "Sessions defined by", "Session summary", "Rinx filter",
                    "Values expressed as", "Baseline timepoint", "Timepoints analyzed", "Reference group",
                    "Summary metric", "Test", "Comparisons", "Multiple comparison correction", "Exported"),
        Value = c(rv$file_name, app_version, st$timepoint, st$stat,
                  if (is.null(st$rinx_max)) "off" else paste0("Rinx <= ", st$rinx_max, "%"),
                  transform(), if (transform() == "none") "" else input$baseline_tp,
                  paste(levels(s$timepoint), collapse = ", "), reference(), metric_text(),
                  test_names[[input$test]], input$comp_mode, input$padj, format(Sys.time(), "%Y-%m-%d %H:%M"))
      )
      groups <- groups_ok()
      group_table <- data.frame(Group = rep(names(groups), lengths(groups)), Animal = unlist(groups, use.names = FALSE))
      check <- assign_groups(sessions_base(), groups)
      sheets <- list(
        Methods = data.frame(Methods = methods_text()),
        Settings = settings,
        Groups = group_table,
        Session_values = s[, c("group", "subject", "timepoint", "day", "start", "n_records", sel)],
        Group_means = group_sum()[group_sum()$parameter %in% sel, ],
        Animal_summary = subj_vals()[subj_vals()$parameter %in% sel, ],
        Data_check = as.data.frame(check[, c("group", "subject", "timepoint", "n_records")])
      )
      if (has_comparisons()) {
        sheets$Group_comparisons <- comps()[comps()$parameter %in% sel, ]
        sheets$Timepoint_tests <- tp_stats()[tp_stats()$parameter %in% sel, ]
      }
      if (!is.null(ml_res())) {
        obj <- ml_res()
        sheets$ML_metrics <- merge(obj$metrics, obj$tuning, by = "model", all.x = TRUE)
        sheets$ML_predictions <- obj$predictions
        if (!is.null(obj$importance)) sheets$ML_importance <- obj$importance
      }
      if (fac_valid()) {
        sheets$Factorial_design <- stats::setNames(fac_design(), c("Group", fac_names()))
        sheets$Factorial_model <- tryCatch(fac_results(), error = function(e) data.frame(Message = conditionMessage(e)))
      }
      writexl::write_xlsx(sheets, path = file)
    }
  )

  # -- Guide and status ---------------------------------------------------------------
  output$glossary <- renderDT({
    g <- wbp_parameter_info()
    names(g) <- c("Parameter", "Name", "Unit", "Description")
    datatable(g, rownames = FALSE, class = "compact", options = list(dom = "ft", paging = FALSE))
  })

  output$status_pill <- renderUI({
    if (is.null(rv$raw)) return(span(class = "badge text-bg-light status-pill", "No data loaded"))
    g <- tryCatch(length(groups_ok()), error = function(e) 0)
    span(class = "badge text-bg-light status-pill",
         " ", rv$file_name, " · ", length(unique(rv$raw$subject)), " animals · ", g, " groups")
  })
}

shinyApp(ui = ui, server = server)
