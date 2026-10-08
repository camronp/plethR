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
is_var_feat <- function(p) grepl(paste0("_(", paste(names(variability_feature_types()), collapse = "|"), ")$"), p)
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
  div(class = "d-flex gap-2 mt-2 align-items-center",
      downloadButton(paste0(id, "_png"), "Download image", class = "btn-sm btn-outline-secondary", icon = NULL),
      downloadButton(paste0(id, "_pdf"), "PDF (vector)", class = "btn-sm btn-outline-secondary", icon = NULL),
      span(class = "small text-muted", "Format, size and resolution: Figures in the toolbar"))
}

# Figure export settings: format (png, tiff, svg, pdf), dpi, width preset (inches; 0 = figure default), text scale.
default_fig_settings <- list(format = "png", dpi = 300, width = 0, text = 1)
fig_ext <- function(fs) switch(fs$format, tiff = "tif", fs$format)

save_plot <- function(file, plot, width, height, type, fs = default_fig_settings) {
  if (isTRUE(fs$width > 0)) {
    height <- height * fs$width / width
    width <- fs$width
  }
  m <- fs$text %||% 1
  fmt <- if (type == "pdf") "pdf" else fs$format
  w <- width / m; h <- height / m
  switch(fmt,
    pdf = ggplot2::ggsave(file, plot, width = w, height = h, device = if (capabilities("cairo")) grDevices::cairo_pdf else "pdf"),
    svg = ggplot2::ggsave(file, plot, width = w, height = h, device = svglite::svglite),
    tiff = ggplot2::ggsave(file, plot, width = w, height = h, dpi = fs$dpi * m, bg = "white", device = "tiff", compression = "lzw"),
    ggplot2::ggsave(file, plot, width = w, height = h, dpi = fs$dpi * m, bg = "white", device = "png"))
}

# Every table gets Copy / CSV / Excel buttons.
dtx <- function(data, ..., options = list(), extensions = NULL) {
  options$dom <- paste0("B", sub("^B", "", options$dom %||% "frtip"))
  options$buttons <- list("copy", list(extend = "csv", title = NULL), list(extend = "excel", title = NULL))
  DT::datatable(data, ..., extensions = unique(c(extensions, "Buttons")), options = options)
}

safe_name <- function(x) gsub("[^A-Za-z0-9_-]+", "_", x)

metric_choices <- c(
  "Area under the curve (AUC)" = "auc",
  "Time-averaged value" = "mean",
  "Peak (maximum)" = "max",
  "Minimum" = "min",
  "Value at one timepoint" = "value"
)


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
.choice-help { font-size: 0.82rem; color: #6c757d; margin-top: -0.5rem; margin-bottom: 0.9rem; }
.methods-box { background: #f8f9fa; border-left: 4px solid #1F5F8B; padding: 1rem 1.25rem; font-size: 0.95rem; }
.status-pill { font-size: 0.8rem; }
table.small-table { font-size: 0.85rem; }
.has-tip { border-bottom: 1px dotted #1F5F8B; cursor: help; }
div.dt-buttons .btn, div.dt-buttons .dt-button { font-size: 0.78rem; padding: 0.15rem 0.6rem; background: #fff; color: #1F5F8B; border: 1px solid #9DB3C6; margin-right: 0.25rem; }
div.dt-buttons .btn:hover, div.dt-buttons .dt-button:hover { background: #EEF3F7; }
.glossary td { vertical-align: top; }

/* Toolbar */
.plethr-ribbon { position: sticky; top: 0; z-index: 1020; background: #F3F6F9; border-bottom: 1px solid #D5DEE6;
  display: flex; flex-wrap: wrap; align-items: stretch; gap: 0.4rem 0; padding: 0.35rem 0.75rem; }
.ribbon-group { display: flex; flex-direction: column; padding: 0 0.9rem; border-right: 1px solid #D5DEE6; }
.ribbon-group:last-child { border-right: none; }
.ribbon-title { font-size: 0.66rem; text-transform: uppercase; letter-spacing: 0.05em; color: #7A8794; margin-bottom: 0.2rem; }
.ribbon-items { display: flex; align-items: center; gap: 0.4rem; flex-wrap: wrap; }
.ribbon-items .btn { font-size: 0.85rem; padding: 0.22rem 0.65rem; }
.ribbon-items .form-group, .ribbon-items .shiny-input-container { margin-bottom: 0 !important; }
.ribbon-field { display: flex; align-items: center; gap: 0.35rem; }
.ribbon-field-label { font-size: 0.82rem; color: #495057; white-space: nowrap; }
.ribbon-items .selectize-input { padding: 0.2rem 0.5rem; min-height: 0; font-size: 0.85rem; }
.ribbon-menu { min-width: 320px; max-width: 420px; padding: 0.9rem 1rem; max-height: 78vh; overflow-y: auto; }
.ribbon-menu .shiny-input-container { width: 100% !important; margin-bottom: 0.4rem; }
.ribbon-menu .choice-help { margin-top: 0; }
.ribbon-menu a.dropdown-item, .ribbon-menu .btn.dropdown-item { padding: 0.35rem 0.5rem; border-radius: 0.35rem; text-align: left; }
.ribbon-status { margin-left: auto; justify-content: center; }
.save-status { font-size: 0.8rem; white-space: nowrap; }
.save-status.saved { color: #2E8B57; }
.save-status.unsaved { color: #B7791F; font-weight: 600; }
.hidden-input { position: absolute; left: -9999px; width: 1px; height: 1px; overflow: hidden; }

/* Setup page */
.step-card .card-header { display: flex; align-items: center; gap: 0.6rem; font-size: 1.05rem; }
.step-num { display: inline-flex; width: 1.7rem; height: 1.7rem; border-radius: 50%; background: #1F5F8B; color: #fff;
  align-items: center; justify-content: center; font-size: 0.9rem; }
.recent-list .list-group-item { font-size: 0.88rem; padding: 0.4rem 0.75rem; }

/* Results view list */
.results-nav .nav-pills { flex-direction: column; flex-wrap: nowrap; gap: 1px; position: sticky; top: 5.2rem; max-height: calc(100vh - 6rem); overflow-y: auto; }
.results-nav .nav-pills .nav-link { padding: 0.32rem 0.75rem; font-size: 0.9rem; color: #33475B; border-radius: 0.4rem; }
.results-nav .nav-pills .nav-link.active { background: #1F5F8B; color: #fff; }
.results-nav .nav-pills .nav-link:hover:not(.active) { background: #E8EEF4; }
.nav-section { font-size: 0.68rem; text-transform: uppercase; letter-spacing: 0.05em; color: #7A8794; margin: 0.8rem 0 0.15rem 0.75rem; }
.results-nav li:first-child .nav-section { margin-top: 0; }
.scope-switch .form-check-inline { font-weight: 600; margin-right: 1.5rem; }
.view-title { font-size: 1.2rem; font-weight: 600; margin-bottom: 0.2rem; }
.section-card { margin-bottom: 1rem; }
"

app_js <- "
var plethrDirty = false;
window.addEventListener('beforeunload', function (e) { if (plethrDirty) { e.preventDefault(); e.returnValue = ''; } });
document.addEventListener('keydown', function (e) {
  if ((e.ctrlKey || e.metaKey) && (e.key === 's' || e.key === 'S')) {
    e.preventDefault();
    if (window.Shiny) Shiny.setInputValue('save_shortcut', Date.now(), {priority: 'event'});
  }
});
$(document).on('shiny:connected', function () {
  Shiny.addCustomMessageHandler('plethr-dirty', function (x) { plethrDirty = !!x; });
});
function plethrPick(id) { var el = document.getElementById(id); if (el) el.click(); return false; }
"

# A toolbar button that opens a panel of settings or actions (stays open while you use it).
ribbon_menu <- function(label, ..., end = FALSE, class = "btn-outline-secondary") {
  div(class = "dropdown",
      tags$button(class = paste("btn btn-sm dropdown-toggle", class), type = "button", `data-bs-toggle` = "dropdown",
                  `data-bs-auto-close` = "outside", `aria-expanded` = "false", label),
      div(class = paste("dropdown-menu ribbon-menu", if (end) "dropdown-menu-end"), ...))
}
ribbon_group <- function(title, ..., class = NULL) {
  div(class = paste("ribbon-group", class), div(class = "ribbon-title", title), div(class = "ribbon-items", ...))
}
menu_item_dl <- function(id, label) downloadButton(id, label, class = "btn btn-link dropdown-item", icon = NULL)

ribbon <- div(
  class = "plethr-ribbon",
  ribbon_group("File",
    ribbon_menu("Open",
      tags$a(class = "dropdown-item", href = "#", onclick = "return plethrPick('excel_file');", "Data file (.xlsx)\u2026"),
      tags$a(class = "dropdown-item", href = "#", onclick = "return plethrPick('project_file');", "Project file (.rds)\u2026"),
      tags$hr(class = "dropdown-divider"),
      h6(class = "dropdown-header px-1", "Recent projects"),
      uiOutput("recent_projects")),
    div(class = "btn-group",
        actionButton("save_project", "Save", class = "btn-primary btn-sm"),
        tags$button(class = "btn btn-primary btn-sm dropdown-toggle dropdown-toggle-split", type = "button",
                    `data-bs-toggle` = "dropdown", `aria-expanded` = "false", span(class = "visually-hidden", "Save options")),
        div(class = "dropdown-menu ribbon-menu",
            actionButton("save_as", "Save as a new project\u2026", class = "btn btn-link dropdown-item"),
            menu_item_dl("dl_project", "Download the project file (.rds)"),
            tags$hr(class = "dropdown-divider"),
            uiOutput("save_info"))),
    uiOutput("save_status", inline = TRUE),
    div(class = "hidden-input", fileInput("project_file", NULL, accept = ".rds"))),
  ribbon_group("Export",
    downloadButton("dl_everything", "Download everything", class = "btn-outline-primary btn-sm", icon = NULL),
    ribbon_menu("More",
      p(class = "small text-muted mb-2", "\"Download everything\" is one zip: the Excel workbook, every figure, a combined PDF, methods, settings and a README."),
      menu_item_dl("dl_xlsx", "Excel workbook (all tables)"),
      menu_item_dl("dl_pdf_all", "All figures as one PDF"),
      menu_item_dl("dl_png_zip", "All figures as images (.zip)"),
      menu_item_dl("dl_rscript", "R script that reproduces the analysis"),
      p(class = "small text-muted mt-2 mb-0", "Every figure and table also has its own download buttons. Image format and size: Figures."))),
  ribbon_group("Analysis",
    div(class = "ribbon-field", span(class = "ribbon-field-label", "Parameter"), selectInput("param", NULL, choices = NULL, width = "170px")),
    div(class = "ribbon-field", span(class = "ribbon-field-label", "Compare with"),
        selectInput("reference", NULL, choices = NULL, width = "190px")),
    ribbon_menu("Analysis settings",
      h6("Summary metric"),
      selectInput("metric", label_with("One value per animal",
                  "Used for group comparisons, the heatmap and PCA. AUC captures the cumulative effect over the whole time course."),
                  choices = metric_choices, selected = "auc"),
      div(class = "choice-help", tags$b("AUC is recommended"), " for overall effects. \"Time-averaged value\" is the same information in the parameter's own units (AUC divided by duration), which is easier to read."),
      uiOutput("window_ui"),
      tags$hr(),
      h6("Statistics"),
      radioButtons("test", label_with("Test"),
                   choices = c("Welch's t-test / Welch's ANOVA" = "parametric",
                               "Mann-Whitney / Kruskal-Wallis" = "nonparametric"),
                   selected = "parametric"),
      div(class = "choice-help", tags$b("Welch is recommended"), " for typical group sizes (4-10 animals). It does not assume equal variances. ",
          "Rank-based tests have very little power with small groups: with 4 vs 4 animals, the smallest possible p-value is 0.029."),
      radioButtons("comp_mode", "Compare",
                   choices = c("Each group with the control group" = "reference", "All pairs of groups" = "all"),
                   selected = "reference"),
      div(class = "choice-help", "Comparing only with the control group means fewer tests, so more power after correction."),
      selectInput("padj", label_with("Multiple comparison correction"),
                  choices = c("Holm" = "holm", "Benjamini-Hochberg (FDR)" = "BH", "Bonferroni" = "bonferroni", "None (exploratory only)" = "none"),
                  selected = "holm"),
      div(class = "choice-help", tags$b("Holm is recommended."), " It controls false positives like Bonferroni but has more power. ",
          "Benjamini-Hochberg is suited to screening many timepoints or parameters."))),
  ribbon_group("Figures",
    ribbon_menu("Figures", end = TRUE,
      selectInput("palette", "Group colors",
                  choices = c("Colorblind-safe (Okabe-Ito)" = "okabe-ito", "Set1" = "set1", "Dark2" = "dark2",
                              "Viridis" = "viridis", "Grayscale" = "grayscale"),
                  selected = "okabe-ito"),
      selectizeInput("params_multi", label_with("Parameters in overview figures and export",
                     "Used by the dashboard, heatmaps, PCA, correlations, profiles and the export. Remove parameters you do not need."),
                     choices = NULL, multiple = TRUE, options = list(plugins = list("remove_button"))),
      tags$hr(),
      h6("Downloaded images"),
      radioButtons("fig_format", "Format",
                   choices = c("PNG (slides, documents)" = "png", "TIFF (journal submission)" = "tiff",
                               "SVG (editable vector)" = "svg"), selected = "png"),
      radioButtons("fig_dpi", "Resolution (PNG, TIFF)", choices = c("150 dpi" = "150", "300 dpi" = "300", "600 dpi" = "600"),
                   selected = "300", inline = TRUE),
      selectInput("fig_width", "Width", choices = c("Figure default" = "0", "Journal single column (3.5 in / 89 mm)" = "3.5",
                                                   "Journal 1.5 column (5.5 in / 140 mm)" = "5.5", "Journal full width (7.2 in / 183 mm)" = "7.2",
                                                   "Slide (10 in)" = "10")),
      sliderInput("fig_text", "Text size", min = 0.8, max = 1.8, value = 1, step = 0.1, post = "\u00d7"),
      p(class = "choice-help", "Larger text helps in narrow journal columns and slides. Every figure also has a PDF (vector) button.")))
)

# ---- Setup: load, groups, processing on one page ----

step_card <- function(num, title, ...) {
  card(class = "step-card section-card", card_header(span(class = "step-num", num), title), ...)
}

setup_panel <- nav_panel(
  title = "Setup", value = "setup",
  div(class = "container-fluid py-3",
    p(class = "step-intro",
      "Load a FinePointe export, check the groups, and choose how sessions are summarized. ",
      "Statistics, figure and export options are always in the toolbar above. Save at any time with ", tags$b("Save"), " or Ctrl+S."),
    step_card("1", "Data",
      layout_columns(
        col_widths = c(4, 8), fill = FALSE,
        div(
          fileInput("excel_file", "FinePointe export (.xlsx)", accept = ".xlsx", buttonLabel = "Choose file", width = "100%"),
          p(class = "small text-muted", "Each sheet is one animal. Empty sheets (such as .Apnea) and FinePointe log lines are skipped. ",
            "There is no file size limit; around 100 MB takes about a minute."),
          uiOutput("load_message"),
          h6(class = "mt-3", "Or continue a saved project"),
          actionButton("open_project_btn", "Open a project file\u2026", class = "btn-sm btn-outline-secondary mb-2",
                       onclick = "plethrPick('project_file');"),
          uiOutput("recent_projects_setup")
        ),
        uiOutput("load_summary")
      )
    ),
    step_card("2", "Groups",
      layout_columns(
        col_widths = c(4, 8), fill = FALSE,
        div(
          p(class = "choice-help mt-0", "Groups were suggested from the sheet names (\"Infected WT1\", \"Infected WT2\" \u2192 \"Infected WT\"). ",
            "Edit the names, then check the animals in each group. Animals in no group are left out."),
          textAreaInput("group_names", label_with("Group names, one per line",
                        "The order here is the order groups appear in every figure and table. Rename a group by editing its line."),
                        rows = 5, width = "100%"),
          p(class = "choice-help", "Choose the control group under ", tags$b("Compare with"), " in the toolbar."),
          uiOutput("group_warnings"),
          hr(),
          h6("Study design file (optional)"),
          p(class = "choice-help mt-0", "One workbook per study with groups, sex, exclusions, body weights and bacterial burden (CFU). ",
            "Loading it sets the groups and exclusions here and adds weight and CFU analyses."),
          downloadButton("dl_design_template", "Download template (pre-filled)", class = "btn-sm btn-outline-secondary mb-2", icon = NULL),
          fileInput("design_file", NULL, accept = ".xlsx", buttonLabel = "Load design file", width = "100%"),
          uiOutput("design_status")
        ),
        div(h6("Animals in each group"), uiOutput("group_assign_ui"))
      )
    ),
    step_card("3", "Processing",
      p(class = "choice-help mt-0", "Breath records are summarized into one value per animal per session. All statistics use the animal, ",
        "not the individual breath, as the unit of analysis."),
      layout_columns(
        col_widths = c(4, 4, 4), fill = FALSE,
        div(
          radioButtons("tp_def", label_with("Define sessions (timepoints) by"),
                       choices = c("FinePointe Phase label" = "phase", "Calendar date" = "date"), selected = "phase"),
          div(class = "choice-help", "Phase labels (e.g. \"Week 2\", \"7DPE\") are the session names set in FinePointe and are recommended. ",
              "Use calendar date if you did not set phases. Timepoints are ordered by when they were recorded."),
          radioButtons("session_stat", label_with("Summarize each animal's session with the"),
                       choices = c("Median" = "median", "Mean" = "mean"), selected = "median", inline = TRUE),
          div(class = "choice-help", tags$b("Median is recommended."), " Sessions include sighs, sniffing and movement artifacts with extreme values; ",
              "the median is not pulled by these, the mean is."),
          input_switch("use_rinx", "Exclude records with a high rejection index (Rinx)", value = FALSE),
          conditionalPanel("input.use_rinx",
            sliderInput("rinx_max", "Maximum Rinx (%)", min = 10, max = 100, value = 80, step = 5)),
          div(class = "choice-help", "Optional: in typical data the median Rinx is around 50%, so strict cut-offs remove a lot of data.")
        ),
        div(
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
                         choices = NULL, multiple = TRUE, options = list(plugins = list("remove_button")), width = "100%")
        ),
        div(
          uiOutput("weight_ui"),
          input_switch("use_var", "Add breathing variability features (optional)", value = FALSE),
          div(class = "choice-help", "How each parameter fluctuates within a session: smoothness, irregularity, variability and slow/fast fluctuations. ",
              "The features become extra parameters (e.g. TVb_ac1) in every analysis."),
          conditionalPanel("input.use_var",
            selectizeInput("var_params", "Parameters", choices = c("f", "TVb", "MVb", "Penh", "PIFb", "PEFb", "EF50", "Ti", "Te", "Rpef", "EIP", "EEP"),
                           selected = c("TVb", "MVb", "PIFb", "f", "Penh"), multiple = TRUE, options = list(plugins = list("remove_button"))),
            checkboxGroupInput("var_features", "Features", choices = stats::setNames(names(variability_feature_types()), variability_feature_types()),
                               selected = c("ac1", "sampen", "slow")),
            div(class = "choice-help", tags$b("Keep the set small."), " Every feature is another test. The defaults showed the most consistent infection ",
                "differences in CP05, but that was exploratory. Records are ~2 s averages, so these describe fluctuations over seconds to minutes."))
        )
      ),
      h6(class = "mt-2", "Data check: records per animal and session"),
      p(class = "small text-muted mb-2",
        "Red cells are missing sessions; yellow cells have fewer than half the typical number of records. Consider excluding animals or timepoints with many flags."),
      withSpinner(DTOutput("check_table"), color = "#1F5F8B"),
      uiOutput("check_flags")
    ),
    div(class = "d-flex justify-content-end mb-4",
        actionButton("to_results", "See the results \u2192", class = "btn-primary btn-lg"))
  )
)

# ---- Results: one list of views ----

ex_help_text <- c(
  dashboard = "Every parameter chosen under Figures, over time, one panel each. Good for spotting which parameters respond at all.",
  heatmap = "One row per animal, one column per session, for the parameter in the toolbar. Shows which animals respond, when, and how consistently.",
  correlation = "How the parameters chosen under Figures move together. Strongly correlated parameters carry similar information; correlating animals (not sessions) avoids treating repeated sessions as independent.",
  profile = "Each group's fingerprint: % difference from the control group on every parameter chosen under Figures, using the summary metric.",
  forest = "Size and uncertainty of every effect: % difference from the control group with 95% confidence intervals. Filled points: adjusted p < 0.05.",
  distribution = "The breath records behind each session value, pooled per group, for the parameter in the toolbar. Descriptive only.",
  trajectory = "Each group's path through two parameters over time. Useful for seeing combined changes, such as faster and shallower breathing.",
  waterfall = "Each animal's average % change from its own first session over the summary-metric window, sorted, for the parameter in the toolbar.",
  cfu_overlay = "The parameter in the toolbar and bacterial burden from the design file on a shared days-post-infection axis.",
  cfu_correlation = "Breathing against bacterial burden: each CFU value paired with the nearest WBP session. With group-level CFU (separate harvest cohorts) this is an ecological correlation over time; with per-animal CFU it is a correlation between animals.")

view_head <- function(title, text = NULL) {
  tagList(div(class = "view-title", title), if (!is.null(text)) p(class = "choice-help mt-0 mb-3", text))
}

# A figure with its download buttons and, optionally, its own options on the right.
ex_block <- function(view, ...) {
  layout_columns(
    col_widths = if (length(list(...))) c(9, 3) else 12, fill = FALSE,
    div(withSpinner(plotOutput(paste0("ex_plot_", view), height = "auto"), color = "#1F5F8B"), plot_downloads(paste0("dl_ex_", view))),
    if (length(list(...))) div(...)
  )
}
ex_view_panel <- function(title, view, ...) {
  nav_panel(title, value = paste0("ex_", view), view_head(title, ex_help_text[[view]]), ex_block(view, ...))
}

nav_section <- function(title) nav_item(div(class = "nav-section", title))

results_panel <- nav_panel(
  title = "Results", value = "results",
  div(class = "container-fluid py-3 results-nav",
    navset_pill_list(
      id = "results_tabs", widths = c(2, 10), well = FALSE,
      nav_section("Overview"),
      nav_panel(
        "Key findings", value = "findings",
        view_head("Key findings"),
        uiOutput("summary_boxes"),
        uiOutput("power_note"),
        h5(class = "mt-3", "Group differences, strongest first"),
        p(class = "small text-muted", textOutput("findings_caption", inline = TRUE), " Click a row to see that parameter."),
        withSpinner(DTOutput("findings_table"), color = "#1F5F8B")
      ),
      ex_view_panel("All parameters", "dashboard"),
      nav_panel(
        "Methods text", value = "methods",
        view_head("Methods paragraph", "A description of the analysis with your current settings, for a methods section. Check and edit before use."),
        div(class = "methods-box", textOutput("methods_text"))
      ),

      nav_section("Over time"),
      nav_panel(
        "Time course", value = "tc",
        uiOutput("param_description"),
        layout_columns(
          fill = FALSE, col_widths = c(9, 3),
          div(withSpinner(plotOutput("tc_plot", height = "520px"), color = "#1F5F8B"), plot_downloads("dl_tc")),
          div(
            radioButtons("tc_method", label_with("Statistics at each timepoint",
                         "The mixed model uses every session with a random effect for each animal and can adjust for covariates; t-tests compare groups separately at each timepoint."),
                         choices = c("Mixed model" = "mixed", "t-test per timepoint" = "ttest"), selected = "mixed"),
            conditionalPanel("input.tc_method == 'mixed'",
              div(class = "choice-help", tags$b("The mixed model is recommended"), " for repeated sessions of the same animals."),
              uiOutput("tc_cov_ui"),
              input_switch("tc_mm_log", "Model log values (skewed parameters)", value = FALSE)),
            radioButtons("tc_error", label_with("Error bars", "SEM shows how precisely the group mean is known; SD shows how much animals vary."),
                         choices = c("SEM" = "sem", "SD" = "sd", "None" = "none"), selected = "sem", inline = TRUE),
            radioButtons("tc_style", "Error style", choices = c("Bars" = "bars", "Shaded band" = "band"), selected = "bars", inline = TRUE),
            radioButtons("tc_x", label_with("X axis", "Session labels are evenly spaced. Study day shows the true time between sessions."),
                         choices = c("Session labels" = "timepoint", "Study day" = "day", "Days post-infection (design file)" = "dpi"), selected = "timepoint"),
            input_switch("tc_individuals", "Show each animal", value = FALSE),
            input_switch("tc_stats", "Mark significant timepoints", value = TRUE),
            input_switch("tc_log", "Log scale (skewed parameters such as Penh)", value = FALSE),
            input_switch("tc_facet", "One panel per group", value = FALSE),
            p(class = "choice-help", "Stars: adjusted p < 0.05 vs the control group at that timepoint, colored by group. ",
              "Corrected across all timepoints and groups for this parameter.")
          )
        ),
        h6(class = "mt-3", "Tests at each timepoint"),
        DTOutput("tc_table"),
        conditionalPanel("input.tc_method == 'mixed'", h6(class = "mt-3", "Mixed model terms"), DTOutput("tc_model_table"))
      ),
      ex_view_panel("Every animal (heatmap)", "heatmap",
        radioButtons("ex_scale", "Color by", choices = c("% of first session" = "percent", "z-score" = "z", "Measured value" = "value"), selected = "percent")),
      nav_panel(
        "Animal trends", value = "trends",
        view_head("Animal trends", "Each animal's values of the parameter in the toolbar over time, with its trend and direction. Use this to spot animals that respond differently from their group."),
        layout_columns(fill = FALSE, col_widths = c(3, 3, 6),
          radioButtons("at_method", "Trend line", choices = c("Linear" = "linear", "Smooth (LOESS)" = "loess"), selected = "linear", inline = TRUE),
          selectInput("at_from", "Trend from", choices = NULL),
          div()),
        withSpinner(plotOutput("at_plot", height = "auto"), color = "#1F5F8B"), plot_downloads("dl_at"),
        h6(class = "mt-3", "Trend per animal"),
        DTOutput("at_table")
      ),
      ex_view_panel("Change from baseline", "waterfall"),
      nav_panel(
        "Bacterial burden", value = "cfu",
        view_head("Bacterial burden (CFU)", "From the study design file. Shown only when the design file has CFU values."),
        h6("CFU with breathing"),
        p(class = "choice-help mt-0", ex_help_text[["cfu_overlay"]]),
        ex_block("cfu_overlay"),
        h6(class = "mt-4", "Breathing vs CFU"),
        p(class = "choice-help mt-0", ex_help_text[["cfu_correlation"]]),
        ex_block("cfu_correlation")
      ),

      nav_section("Group differences"),
      nav_panel(
        "Group comparison", value = "cmp",
        uiOutput("param_description_cmp"),
        layout_columns(
          col_widths = c(8, 4), fill = FALSE,
          div(withSpinner(plotOutput("cmp_plot", height = "520px"), color = "#1F5F8B"), plot_downloads("dl_cmp")),
          div(
            radioButtons("cmp_style", "Plot style",
                         choices = c("Bars (mean \u00b1 SEM) with animals" = "bar",
                                     "Points with mean \u00b1 SEM" = "dot",
                                     "Box plot with animals" = "box"), selected = "bar"),
            input_switch("cmp_stats", "Show comparison brackets", value = TRUE),
            input_switch("cmp_ns", "Include non-significant (ns) brackets", value = FALSE),
            uiOutput("cmp_stats_text")
          )
        ),
        h6(class = "mt-3", "Pairwise comparisons"),
        DTOutput("cmp_table")
      ),
      ex_view_panel("Effect sizes", "forest"),
      nav_panel(
        "% difference heatmap", value = "hm",
        view_head("% difference heatmap", "Colors show the % difference between group means and the control group. Stars mark adjusted p < 0.05. Large differences are capped so small ones stay visible."),
        layout_columns(
          col_widths = c(9, 3), fill = FALSE,
          div(withSpinner(plotOutput("hm_plot", height = "640px"), color = "#1F5F8B"), plot_downloads("dl_hm")),
          div(
            radioButtons("hm_mode", "Show",
                         choices = c("Summary metric, all groups" = "metric", "Each timepoint, one group" = "time"),
                         selected = "metric"),
            conditionalPanel("input.hm_mode == 'time'", selectInput("hm_group", "Group", choices = NULL)),
            input_switch("hm_cluster", "Cluster similar parameters", value = TRUE)
          )
        )
      ),
      nav_panel(
        "Factorial model", value = "fac",
        view_head("Factorial model",
          "For designs where every group combines two factors, such as genotype \u00d7 infection. Instead of comparing groups two at a time, this tests each factor using all animals and whether the effect of one factor depends on the other (the interaction)."),
        layout_columns(
          col_widths = c(5, 7), fill = FALSE,
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
                  "The two-way ANOVA uses one value per animal (the summary metric in the toolbar)."),
              input_switch("fac_log", "Log-transform values", value = FALSE),
              div(class = "choice-help", "Useful for skewed ratio parameters such as Penh, where effects are proportional. ",
                  "Decide before looking at results, and apply the same choice to all parameters."),
              p(class = "choice-help", "p-values are corrected across the parameters in the table for each term, using the correction chosen under Statistics. ",
                "If you decided on a few key parameters in advance, keep only those under Figures.")
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

      nav_section("Patterns"),
      nav_panel(
        "PCA", value = "pca",
        view_head("PCA", "Each point is one animal, positioned by its summary metric across the parameters chosen under Figures (centered and scaled). Animals with similar respiratory profiles are close together. PCA is descriptive: it shows patterns, not significance."),
        layout_columns(
          col_widths = c(9, 3), fill = FALSE,
          div(withSpinner(plotOutput("pca_plot", height = "560px"), color = "#1F5F8B"), plot_downloads("dl_pca")),
          div(
            radioButtons("pca_shapes", "Group outlines",
                         choices = c("Outline (convex hull)" = "hull", "95% confidence ellipse" = "ellipse", "None" = "none"),
                         selected = "hull"),
            conditionalPanel("input.pca_shapes == 'ellipse'",
              p(class = "choice-help", "Ellipses need at least 3 animals per group. With few animals they are very uncertain; treat them as a visual guide only.")),
            input_switch("pca_labels", "Label animals", value = FALSE),
            sliderInput("pca_loadings", "Parameter arrows", min = 0, max = 10, value = 5, step = 1),
            uiOutput("pca_note")
          )
        ),
        h6(class = "mt-3", "Variance explained"),
        DTOutput("pca_variance")
      ),
      nav_panel(
        "More views", value = "more",
        view_head("More views"),
        radioButtons("more_view", NULL, inline = TRUE,
                     choices = c("Correlations" = "correlation", "Group profiles" = "profile",
                                 "Two-parameter paths" = "trajectory", "Breath records" = "distribution")),
        conditionalPanel("input.more_view == 'correlation'",
          p(class = "choice-help", ex_help_text[["correlation"]]),
          ex_block("correlation",
            radioButtons("ex_level", "Correlate", choices = c("Animals (recommended)" = "animal", "Animal-sessions" = "session"), selected = "animal"),
            radioButtons("ex_method", "Method", choices = c("Spearman (rank)" = "spearman", "Pearson" = "pearson"), selected = "spearman", inline = TRUE))),
        conditionalPanel("input.more_view == 'profile'",
          p(class = "choice-help", ex_help_text[["profile"]]),
          ex_block("profile",
            sliderInput("ex_limit", "Cap differences at (%)", min = 10, max = 200, value = 50, step = 10))),
        conditionalPanel("input.more_view == 'trajectory'",
          p(class = "choice-help", ex_help_text[["trajectory"]]),
          ex_block("trajectory",
            selectInput("ex_x", "Horizontal axis", choices = NULL),
            selectInput("ex_y", "Vertical axis", choices = NULL),
            sliderInput("ex_smooth", "Smooth over sessions", min = 1, max = 5, value = 3, step = 1))),
        conditionalPanel("input.more_view == 'distribution'",
          p(class = "choice-help", ex_help_text[["distribution"]]),
          ex_block("distribution",
            selectizeInput("ex_tps", "Sessions", choices = NULL, multiple = TRUE, options = list(plugins = list("remove_button"), maxItems = 6))))
      ),

      nav_section("Data"),
      nav_panel(
        "Data tables", value = "tables",
        view_head("Data tables"),
        radioButtons("table_choice", NULL, inline = TRUE,
                     choices = c("Per animal and session" = "sessions", "Group mean per timepoint" = "group",
                                 "Summary metric per animal" = "subjects", "All group comparisons" = "comparisons",
                                 "All timepoint tests" = "timepoints")),
        withSpinner(DTOutput("data_table"), color = "#1F5F8B")
      )
    )
  )
)

# ---- Prediction (machine learning): setup on the left, results on one page ----

ml_study_ui <- (
  layout_sidebar(
    sidebar = sidebar(
      width = 360, title = "Model setup",
      radioButtons("ml_goal", "What to predict",
                   choices = c("Infected vs uninfected" = "infection", "Acute vs chronic infection" = "phase",
                               "Disease severity (measured value)" = "severity", "Any two sets of groups" = "groups"),
                   selected = "infection"),
      conditionalPanel("input.ml_goal != 'severity'",
        selectizeInput("ml_pos", "Infected / positive groups", choices = NULL, multiple = TRUE,
                       options = list(plugins = list("remove_button"))),
        selectizeInput("ml_neg", "Control / negative groups", choices = NULL, multiple = TRUE,
                       options = list(plugins = list("remove_button")))),
      conditionalPanel("input.ml_goal == 'phase'",
        div(class = "choice-help", "Acute vs chronic is predicted within the infected groups. The control groups are used for the time-only check.")),
      conditionalPanel("input.ml_goal == 'infection' || input.ml_goal == 'phase'",
        selectInput("ml_inf_tp", label_with("First session after infection",
                    "Sessions before this one are pre-infection. For infected vs uninfected they count as uninfected, so each infected animal also contributes its own healthy baseline."),
                    choices = NULL)),
      conditionalPanel("input.ml_goal == 'infection'",
        radioButtons("ml_pre", "Pre-infection sessions",
                     choices = c("Leave out" = "exclude", "Count as uninfected" = "uninfected"), selected = "exclude", inline = TRUE),
        div(class = "choice-help", tags$b("Leaving them out is recommended."), " Counting them as uninfected lets a model score well by telling early sessions ",
            "from late ones (age, habituation), which happens in uninfected animals too.")),
      conditionalPanel("input.ml_goal == 'phase'",
        layout_columns(col_widths = c(6, 6),
          numericInput("ml_offset", label_with("Days from infection to that session", "Used to compute days post-infection."), value = 0, min = 0, step = 1),
          numericInput("ml_acute_days", label_with("Acute phase ends at day", "Sessions up to this many days post-infection are acute; later sessions are chronic."),
                       value = 14, min = 1, step = 1)),
        div(class = "choice-help", "Phase is tied to time, so a model can separate acute from chronic sessions by detecting age or growth.")),
      conditionalPanel("input.ml_goal == 'phase' || (input.ml_goal == 'infection' && input.ml_pre == 'uninfected')",
        input_switch("ml_phase_check", "Run the time-only check", value = TRUE),
        div(class = "choice-help", tags$b("Recommended."), " The same models are fitted to a placebo where the label only changes with time ",
            "(uninfected controls labeled as if infected, or by the same phase cutoff). If the placebo scores about as well, the model is detecting time, not infection.")),
      conditionalPanel("input.ml_goal == 'severity'",
        uiOutput("ml_sev_source_ui"),
        fileInput("ml_sev_file", label_with("Severity table (.xlsx or .csv)",
                  "One row per animal (or per animal and session) with a measured value such as lung CFU, histology score or weight loss. Animal names must match the sheet names."),
                  accept = c(".xlsx", ".csv")),
        downloadLink("ml_sev_template", "Download a template with your animal names"),
        uiOutput("ml_sev_cols_ui"),
        textInput("ml_sev_name", "Name of the measure", value = "Severity"),
        input_switch("ml_sev_log", "Use log10 of the value (recommended for CFU)", value = FALSE)),
      conditionalPanel("input.ml_goal == 'groups'",
        layout_columns(col_widths = c(6, 6),
          textInput("ml_pos_label", "Positive class name", value = "Positive"),
          textInput("ml_neg_label", "Negative class name", value = "Negative"))),
      conditionalPanel("input.ml_goal == 'groups' || input.ml_goal == 'severity'",
        selectizeInput("ml_exclude", label_with("Leave out timepoints",
                       "For example sessions before treatment, which look the same in every group."),
                       choices = NULL, multiple = TRUE, options = list(plugins = list("remove_button")))),
      selectizeInput("ml_params", label_with("Respiratory parameters used as predictors",
                     "Choose which parameters the models may use. Starts with the parameters chosen under Figures."),
                     choices = NULL, multiple = TRUE, options = list(plugins = list("remove_button"))),
      conditionalPanel("input.ml_goal != 'phase'",
        input_switch("ml_day", "Use study day as a predictor", value = TRUE)),
      uiOutput("ml_covariate_ui"),
      hr(),
      radioButtons("ml_validation", label_with("Validation"),
                   choices = c("Hold out whole animals" = "animal", "Random 80/20 split of sessions (original pipeline)" = "random"),
                   selected = "animal"),
      div(class = "choice-help", tags$b("Holding out whole animals is recommended."), " Each animal's sessions are kept together, so models are always tested on animals ",
          "they have never seen. With a random split, sessions of the same animal are in both training and test data and models can score well by recognizing individual animals."),
      checkboxGroupInput("ml_models", "Models", choices = stats::setNames(names(ml_model_names()), ml_model_names()),
                         selected = names(ml_model_names())),
      radioButtons("ml_tuning", "Tuning", choices = c("Quick" = "quick", "Thorough (original grids, slow)" = "thorough"),
                   selected = "quick", inline = TRUE),
      input_switch("ml_parallel", "Use several CPU cores", value = TRUE),
      numericInput("ml_seed", "Random seed", value = 10, min = 1, step = 1),
      actionButton("ml_run", "Train models", class = "btn-primary w-100")
    ),
    div(
      uiOutput("ml_intro"),
      uiOutput("ml_summary"),
      conditionalPanel("output.ml_ready",
      card(class = "section-card", card_header("Performance on held-out data"),
        layout_columns(
          col_widths = c(7, 5), fill = FALSE,
          div(withSpinner(plotOutput("ml_perf_plot", height = "420px"), color = "#1F5F8B"), plot_downloads("dl_ml_perf")),
          div(h6("Metrics"), DTOutput("ml_metrics_table"))
        )),
      card(class = "section-card", card_header("Accuracy details"),
        radioButtons("ml_level", NULL, inline = TRUE,
                     choices = c("Each session" = "session", "Each animal (mean of its sessions)" = "animal")),
        layout_columns(
          col_widths = c(7, 5), fill = FALSE,
          div(withSpinner(plotOutput("ml_roc_plot", height = "460px"), color = "#1F5F8B"), plot_downloads("dl_ml_roc")),
          div(selectInput("ml_model_cm", "Model", choices = NULL),
              withSpinner(plotOutput("ml_cm_plot", height = "360px"), color = "#1F5F8B"), plot_downloads("dl_ml_cm"))
        )),
      card(class = "section-card", card_header("Predictions over time"),
        p(class = "choice-help mt-0", "Held-out probability of the positive class for each group at each timepoint. ",
          "If the model detects the condition, positive groups rise above 50% after treatment and negative groups stay below."),
        selectInput("ml_model_time", "Model", choices = NULL),
        withSpinner(plotOutput("ml_time_plot", height = "480px"), color = "#1F5F8B"), plot_downloads("dl_ml_time")),
      layout_columns(
        col_widths = c(6, 6), fill = FALSE,
        card(class = "section-card", card_header("What the models use"),
          p(class = "choice-help mt-0", "Importance in the final model (trained on all data). It shows what a model uses, not whether it generalizes."),
          selectInput("ml_model_imp", "Model", choices = NULL),
          withSpinner(plotOutput("ml_imp_plot", height = "440px"), color = "#1F5F8B"), plot_downloads("dl_ml_imp")),
        card(class = "section-card", card_header("Each animal"),
          p(class = "choice-help mt-0", "Held-out predictions averaged over each animal's sessions."),
          selectInput("ml_model_animals", "Model", choices = NULL),
          DTOutput("ml_animals_table")))),
      card(class = "section-card", card_header("Save models and predict new data"),
        p(class = "choice-help mt-0", "Apply the trained models to a new FinePointe file. The new data are processed with the same settings ",
          "(sessions, summary, Rinx filter, baseline adjustment) as the training data. Trained models are also saved with the project."),
        layout_columns(
          col_widths = c(4, 8), fill = FALSE,
          div(
            downloadButton("ml_save", "Save trained models (.rds)", class = "btn-outline-secondary w-100 mb-2", icon = NULL),
            fileInput("ml_load", "Or load saved models", accept = ".rds"),
            uiOutput("ml_loaded_info"),
            hr(),
            fileInput("ml_new_file", "New FinePointe export (.xlsx)", accept = ".xlsx"),
            selectInput("ml_model_new", "Model", choices = NULL),
            uiOutput("ml_new_covariate_ui")
          ),
          div(
            uiOutput("ml_new_status"),
            DTOutput("ml_new_table"),
            withSpinner(plotOutput("ml_new_plot", height = "420px"), color = "#1F5F8B"),
            downloadButton("ml_new_download", "Download predictions (.xlsx)", class = "btn-outline-secondary mt-2", icon = NULL)
          )
        ))
    )
  )
)

# ---- Treatment efficacy ----

treatment_panel <- nav_panel(
  title = "Treatment", value = "treatment",
  layout_sidebar(
    sidebar = sidebar(
      width = 360, title = "Treatment setup",
      selectizeInput("tr_healthy", "Healthy controls", choices = NULL, multiple = TRUE, options = list(plugins = list("remove_button"))),
      selectizeInput("tr_disease", "Untreated diseased", choices = NULL, multiple = TRUE, options = list(plugins = list("remove_button"))),
      selectizeInput("tr_treated", "Treated", choices = NULL, multiple = TRUE,
                     options = list(plugins = list("remove_button"), placeholder = "Choose the treated group(s)")),
      div(class = "choice-help", "Groups must share the same disease model; the treated groups differ from the untreated groups only by treatment."),
      radioButtons("tr_score_type", label_with("Health score",
                   "The lung health score averages the chosen parameters with equal weight. The machine learning score lets a model learn how to weight and combine them."),
                   choices = c("Lung health score" = "lung", "Machine learning disease score" = "ml"), selected = "lung"),
      conditionalPanel("input.tr_score_type == 'ml'",
        div(class = "choice-help", tags$b("The lung health score is recommended for small groups."), " The machine learning score is trained on healthy vs untreated animals ",
            "(sessions from disease onset on), scores each of them with a model that never saw it, and scores treated animals with the final model. ",
            "It is the log-odds of disease, centered on healthy controls."),
        selectInput("tr_ml_model", "Model", choices = c("Elastic net (recommended)" = "en", "Logistic regression" = "log",
                                                        "Random forest" = "rf", "Gradient-boosted trees" = "bt"), selected = "en"),
        actionButton("tr_train", "Train disease model", class = "btn-primary w-100 mb-2"),
        uiOutput("tr_ml_status")),
      selectizeInput("tr_params", label_with("Parameters that define lung health",
                     "Choose the parameters you consider markers of lung health. Starts with the parameters chosen under Figures."),
                     choices = NULL, multiple = TRUE, options = list(plugins = list("remove_button"))),
      sliderInput("tr_min_effect", label_with("Use only parameters changed by disease by at least (SD)",
                  "0 uses every chosen parameter. Higher values keep only parameters that clearly differ between untreated and healthy animals."),
                  min = 0, max = 1.5, value = 0, step = 0.25),
      radioButtons("tr_reference", label_with("Compare each animal with"),
                   choices = c("Healthy controls at the same timepoint" = "healthy",
                               "Its own baseline, relative to healthy controls" = "baseline"),
                   selected = "healthy"),
      div(class = "choice-help", tags$b("Healthy controls at the same timepoint is recommended."), " Use the baseline option when animals differed a lot before ",
          "disease, or when groups were not randomized; it needs a baseline session for every animal."),
      conditionalPanel("input.tr_reference == 'baseline'", selectInput("tr_baseline_tp", "Baseline session", choices = NULL)),
      selectInput("tr_onset", label_with("Disease present from",
                  "Sessions from here on are used to learn how disease changes each parameter."), choices = NULL),
      layout_columns(col_widths = c(6, 6),
        selectInput("tr_from", label_with("Evaluate from", "For example the first session after treatment starts."), choices = NULL),
        selectInput("tr_to", "to", choices = NULL)),
      div(class = "choice-help", "Statistics use the test chosen under Statistics in the toolbar.")
    ),
    div(
      uiOutput("tr_intro"),
      uiOutput("tr_verdict"),
      layout_columns(fill = FALSE,
        col_widths = c(7, 5),
        div(withSpinner(plotOutput("tr_tc_plot", height = "440px"), color = "#1F5F8B"), plot_downloads("dl_tr_tc")),
        div(withSpinner(plotOutput("tr_cmp_plot", height = "440px"), color = "#1F5F8B"), plot_downloads("dl_tr_cmp"))
      ),
      h6(class = "mt-3", "Statistics"),
      layout_columns(col_widths = c(6, 6), fill = FALSE, DTOutput("tr_cmp_table"), DTOutput("tr_model_table")),
      card(class = "section-card mt-3", card_header("Which parameters the treatment normalizes"),
        p(class = "choice-help mt-0", "How much of the disease effect on each parameter the treatment removes. 100% = treated animals look like healthy controls on that parameter; ",
          "0% = no effect; negative = worse than untreated."),
        layout_columns(
          col_widths = c(7, 5), fill = FALSE,
          div(withSpinner(plotOutput("tr_par_plot", height = "520px"), color = "#1F5F8B"), plot_downloads("dl_tr_par")),
          DTOutput("tr_par_table")
        )),
      card(class = "section-card", card_header("Each animal over time"),
        p(class = "choice-help mt-0", "Each animal's health score over time with its trend within the evaluation window. ",
          "Down = improving (toward healthy), up = worsening."),
        radioButtons("tr_trend_method", "Trend line", choices = c("Linear" = "linear", "Smooth (LOESS)" = "loess"), selected = "linear", inline = TRUE),
        withSpinner(plotOutput("tr_trend_plot", height = "auto"), color = "#1F5F8B"), plot_downloads("dl_tr_trend"),
        layout_columns(
          col_widths = c(6, 6), fill = FALSE,
          div(h6(class = "mt-3", "Trend per animal"), DTOutput("tr_trend_table")),
          div(h6(class = "mt-3", "Average score per animal"),
              p(class = "choice-help mt-0", "Over the evaluation window. 0 = like healthy controls; higher = more disease-like."),
              DTOutput("tr_animals_table"))
        ))
    )
  )
)

# ---- Study library ----

library_ui <- (
  layout_sidebar(
    sidebar = sidebar(
      width = 340, title = "Library",
      textInput("lib_dir", label_with("Library folder", "Any folder on this computer or a shared drive. It holds one file per study and per saved model."),
                value = file.path(path.expand("~"), "plethR_library")),
      actionButton("lib_open", "Open or create library", class = "btn-primary w-100"),
      uiOutput("lib_info"),
      div(class = "choice-help mt-3", "Add each analyzed study to the library, then train models on all of them and test them on studies they have never seen ",
          "(leave-one-study-out). Saved models record which studies trained them, so you can see whether adding studies improves predictions.")
    ),
    div(
      card(class = "section-card", card_header("Add this study"),
        uiOutput("lib_add_intro"),
        layout_columns(
          col_widths = c(6, 6), fill = FALSE,
          div(
            textInput("lib_study_id", "Study id", value = ""),
            selectInput("lib_inf_tp", "First session after infection", choices = NULL),
            numericInput("lib_offset", "Days from infection to that session", value = 0, min = 0),
            textInput("lib_notes", "Notes", value = ""),
            checkboxInput("lib_overwrite", "Replace a study with the same id", value = FALSE),
            actionButton("lib_add", "Add to library", class = "btn-primary")
          ),
          div(h6("Condition of each group"),
              p(class = "choice-help mt-0", "Conditions make studies with different group names comparable. Exclude groups that should not be pooled."),
              uiOutput("lib_conditions_ui"))
        )),
      card(class = "section-card", card_header("Studies in the library"),
        DTOutput("lib_table"),
        div(actionButton("lib_remove", "Remove selected study", class = "btn-sm btn-outline-danger mt-2"))),
      card(class = "section-card", card_header("Train across studies"),
        layout_columns(
          col_widths = c(4, 8), fill = FALSE,
          div(
            radioButtons("lib_task", "What to predict", choices = c("Infected vs uninfected" = "infection", "Acute vs chronic infection" = "phase"), selected = "infection"),
            conditionalPanel("input.lib_task == 'phase'", numericInput("lib_acute", "Acute phase ends at day", value = 14, min = 1)),
            selectizeInput("lib_studies", "Studies", choices = NULL, multiple = TRUE, options = list(plugins = list("remove_button"))),
            selectizeInput("lib_params", "Predictors (parameters shared by these studies)", choices = NULL, multiple = TRUE, options = list(plugins = list("remove_button"))),
            checkboxGroupInput("lib_models", "Models", choices = stats::setNames(names(ml_model_names()), ml_model_names()), selected = c("log", "en", "rf")),
            radioButtons("lib_validation", label_with("Validation", "Leave-one-study-out tests every study with models trained only on the other studies: the honest test for future studies."),
                         choices = c("Leave one study out (recommended)" = "study", "Hold out whole animals" = "animal"), selected = "study"),
            radioButtons("lib_tuning", "Tuning", choices = c("Quick" = "quick", "Thorough" = "thorough"), selected = "quick", inline = TRUE),
            actionButton("lib_train", "Train", class = "btn-primary w-100")
          ),
          div(
            uiOutput("lib_train_summary"),
            withSpinner(plotOutput("lib_perf_plot", height = "360px"), color = "#1F5F8B"),
            h6(class = "mt-2", "Performance in each held-out study"),
            DTOutput("lib_study_table"),
            div(class = "d-flex gap-2 mt-2 align-items-end",
                textInput("lib_model_name", "Model name", value = "infection_model"),
                actionButton("lib_save_model", "Save model to library", class = "btn-outline-primary mb-3"))
          )
        )),
      card(class = "section-card", card_header("Model history"),
        withSpinner(plotOutput("lib_history_plot", height = "360px"), color = "#1F5F8B"),
        DTOutput("lib_models_table"),
        div(class = "d-flex gap-2 mt-2",
            actionButton("lib_use_model", "Use selected model to predict new files", class = "btn-sm btn-outline-primary"),
            downloadButton("lib_dl_model", "Download selected model (.rds)", class = "btn-sm btn-outline-secondary", icon = NULL)))
    )
  )
)

ml_panel <- nav_panel(
  title = "Prediction", value = "ml",
  div(class = "container-fluid pt-3 scope-switch",
      radioButtons("ml_scope", NULL, inline = TRUE,
                   choices = c("Models for this study" = "study", "Models across studies (study library)" = "library"),
                   selected = "study")),
  conditionalPanel("input.ml_scope == 'study'", ml_study_ui),
  conditionalPanel("input.ml_scope == 'library'", library_ui)
)

guide_panel <- nav_panel(
  title = "Guide", value = "guide",
  div(class = "container py-3", style = "max-width: 70rem;",
    h3("How plethR analyzes whole body plethysmography data"),
    p("FinePointe records many breath-by-breath measurements per animal per session. plethR follows the approach used in published WBP studies:"),
    tags$ol(
      tags$li(tags$b("Summarize each session per animal."), " Each animal gets one value per parameter per session (the median of its records), so outlier breaths have little effect."),
      tags$li(tags$b("Describe groups."), " Group means \u00b1 SEM at each timepoint show the time course. n is the number of animals."),
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
    h4(class = "mt-4", "Breathing variability features (optional)"),
    p("Switched on under Processing in Setup. For each animal, session and chosen parameter, plethR describes how the value fluctuates within the 20-minute session. ",
      "FinePointe records are averages over about 2 seconds, so the features capture changes over seconds to minutes, not breath-to-breath variability."),
    tags$table(class = "table table-sm glossary",
      tags$thead(tags$tr(tags$th("Feature"), tags$th("Suffix"), tags$th("Meaning"))),
      tags$tbody(
        tags$tr(tags$td("Variability"), tags$td("_cv"), tags$td("Median absolute deviation divided by the median (robust coefficient of variation).")),
        tags$tr(tags$td("Smoothness"), tags$td("_ac1"), tags$td("Lag-1 autocorrelation on a 2-second grid: higher = slower, smoother drift.")),
        tags$tr(tags$td("Irregularity"), tags$td("_sampen"), tags$td("Sample entropy (m = 2, r = 0.2 SD): lower = more regular, repetitive pattern.")),
        tags$tr(tags$td("Slow / mid / fast power"), tags$td("_slow, _mid, _fast"), tags$td("Share of fluctuation power with periods of 2-10 min, 30 s-2 min and 4-30 s.")),
        tags$tr(tags$td("Spectral slope"), tags$td("_slope"), tags$td("Slope of log power vs log frequency: more negative = slow fluctuations dominate."))
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
  fillable = FALSE,
  navbar_options = navbar_options(bg = "#1F3A5F", theme = "dark"),
  header = tagList(tags$head(tags$style(HTML(app_css)), tags$script(HTML(app_js))), ribbon),
  setup_panel,
  results_panel,
  ml_panel,
  treatment_panel,
  nav_spacer(),
  guide_panel,
  nav_item(uiOutput("status_pill"))
)

# ---- Server -----------------------------------------------------------------

server <- function(input, output, session) {

  # Figure downloads (used throughout) follow the settings on the Export page.
  fig_settings <- reactive(list(format = input$fig_format %||% "png", dpi = as.numeric(input$fig_dpi %||% 300),
                                width = as.numeric(input$fig_width %||% 0), text = input$fig_text %||% 1))
  register_download <- function(id, name, build, width, height) {
    lapply(c("png", "pdf"), function(type) {
      output[[paste0(id, "_", type)]] <- downloadHandler(
        filename = function() paste0(name(), ".", if (type == "pdf") "pdf" else fig_ext(fig_settings())),
        content = function(file) save_plot(file, build(), width(), height(), type, fig_settings())
      )
    })
  }

  # load_id makes group inputs unique per file, so selections never leak between files.
  rv <- reactiveValues(raw = NULL, file_name = NULL, saved_assign = list(), tp_levels = NULL,
                       load_id = 0, params_set = NULL)
  grp_id <- function(i) paste0("grp_", rv$load_id, "_", i)

  # -- Navigation ---------------------------------------------------------------
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
    proj_state$name <- safe_name(tools::file_path_sans_ext(f$name))
    proj_state$path <- NULL
    proj_state$saved_at <- NULL
    proj_state$saved_snapshot <- NULL
    nav_select("steps", "setup")
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
        if (length(phases)) paste(phases, collapse = " \u2192 ") else "no Phase labels found; sessions will be defined by calendar date."),
      p(tags$b("Animals: "), paste(unique(raw$subject), collapse = ", ")),
      p(tags$b("Parameters: "), paste(params_available(), collapse = ", "))
    )
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
    validate(need(length(a) >= 1, "Assign animals to at least one group (Setup, Groups)."))
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

  # -- Study design file ---------------------------------------------------------
  output$dl_design_template <- downloadHandler(
    filename = function() paste0(safe_name(tools::file_path_sans_ext(rv$file_name %||% "study")), "_design.xlsx"),
    content = function(file) {
      subj <- if (is.null(rv$raw)) character(0) else unique(rv$raw$subject)
      a <- tryCatch(assignment(), error = function(e) list())
      grp <- if (length(a)) unname(stats::setNames(rep(names(a), lengths(a)), unlist(a))[subj]) else suggest_groups(subj)
      tps <- tryCatch(levels(sessions_base()$timepoint), error = function(e) NULL)
      write_design_template(file, subj, grp, tps)
    }
  )

  observeEvent(input$design_file, {
    d <- tryCatch(read_design(input$design_file$datapath, if (is.null(rv$raw)) NULL else unique(rv$raw$subject)),
                  error = function(e) { showNotification(paste("Could not read the design file:", conditionMessage(e)), type = "error", duration = NULL); NULL })
    req(d)
    rv$design <- d
    g <- design_groups(d)
    if (length(g)) {
      rv$saved_assign <- g
      updateTextAreaInput(session, "group_names", value = paste(names(g), collapse = "\n"))
    }
    showNotification("Study design loaded.", type = "message")
  })

  output$design_status <- renderUI({
    d <- rv$design
    if (is.null(d)) return(NULL)
    a <- d$animals
    ex <- a[a$exclude, , drop = FALSE]
    n_cfu_animal <- sum(!is.na(d$cfu$animal))
    div(class = "alert alert-success py-2 small mb-0",
        tags$b(if (!is.null(d$study$study_id) && nzchar(d$study$study_id)) d$study$study_id else "Design file"), " loaded: ",
        sprintf("%d animals in %d groups", sum(!a$exclude), length(unique(a$group[!a$exclude]))),
        if (any(!is.na(a$sex))) sprintf(" (%d F, %d M)", sum(a$sex == "F", na.rm = TRUE), sum(a$sex == "M", na.rm = TRUE)), "; ",
        sprintf("%d body weights; %d CFU values%s", nrow(d$weights), nrow(d$cfu), if (n_cfu_animal) sprintf(" (%d per animal)", n_cfu_animal) else ""),
        if (nzchar(d$study$infection_session %||% "")) paste0("; infection before ", d$study$infection_session), ".",
        if (nrow(ex)) tags$div(class = "mt-1", tags$b("Excluded: "), paste(sprintf("%s (%s)", ex$animal, ifelse(is.na(ex$exclusion_reason), "no reason given", ex$exclusion_reason)), collapse = "; ")),
        if (length(d$messages)) tags$ul(class = "mb-0 mt-1 text-warning", lapply(d$messages, tags$li)))
  })

  design_dpi_r <- reactive({
    if (is.null(rv$design)) return(NULL)
    tryCatch(design_dpi(sessions(), rv$design), error = function(e) NULL)
  })

  # Use the design's infection session as the default elsewhere.
  observeEvent(list(rv$design, sessions_base()), {
    d <- rv$design
    inf <- d$study$infection_session
    lv <- levels(sessions_base()$timepoint)
    if (!is.null(inf) && inf %in% lv) {
      updateSelectInput(session, "ml_inf_tp", selected = inf)
      updateSelectInput(session, "tr_onset", selected = inf)
      off <- suppressWarnings(as.numeric(d$study$days_from_infection_to_that_session))
      if (!is.na(off)) updateNumericInput(session, "ml_offset", value = off)
    }
  }, ignoreInit = TRUE)

  output$weight_ui <- renderUI({
    d <- rv$design
    if (is.null(d) || nrow(d$weights) == 0) return(NULL)
    tagList(
      input_switch("use_weight", "Add body weight and weight-normalized volumes", value = TRUE),
      div(class = "choice-help", "From the design file: Weight (g), Weight_change (% of first weight), and TVb_per_g and MVb_per_g (mL/g). ",
          tags$b("Recommended"), " when animals gain or lose weight, because absolute volumes scale with body size."))
  })

  # -- 3. Process ---------------------------------------------------------------
  var_settings <- debounce(reactive(list(params = input$var_params, features = input$var_features)), 800)
  var_data <- reactive({
    req(rv$raw)
    vs <- var_settings()
    validate(need(length(vs$params) > 0 && length(vs$features) > 0, "Choose at least one parameter and one feature type for the variability features."))
    withProgress(message = "Computing breathing variability features", value = 0, {
      session_variability(rv$raw, vs$params, vs$features, timepoint = input$tp_def,
                          rinx_max = if (isTRUE(input$use_rinx)) input$rinx_max else NULL,
                          progress = function(i, n) if (i %% 10 == 0) setProgress(i / n))
    })
  })

  sessions_base <- reactive({
    req(rv$raw)
    s <- summarize_sessions(rv$raw, timepoint = input$tp_def, stat = input$session_stat,
                            rinx_max = if (isTRUE(input$use_rinx)) input$rinx_max else NULL)
    if (isTRUE(input$use_var)) s <- add_variability(s, var_data())
    if (isTRUE(input$use_weight) && !is.null(rv$design) && nrow(rv$design$weights) > 0) {
      s <- tryCatch(add_body_weight(s, rv$design$weights), error = function(e) { showNotification(conditionMessage(e), type = "warning"); s })
    }
    s
  })

  observeEvent(sessions_base(), {
    lv <- levels(sessions_base()$timepoint)
    if (!identical(lv, rv$tp_levels)) {
      rv$tp_levels <- lv
      updateSelectizeInput(session, "tp_include", choices = lv, selected = lv)
      updateSelectInput(session, "baseline_tp", choices = lv, selected = lv[1])
    }
  })

  params <- reactive({
    p <- attr(sessions_base(), "parameters")
    p[p %in% params_available() | is_var_feat(p) | p %in% c("Weight", "Weight_change") | grepl("_per_g$", p)]
  })

  sessions <- reactive({
    s <- assign_groups(sessions_base(), groups_ok())
    validate(need(nrow(s) > 0, "No data for the assigned animals."))
    tp <- input$tp_include
    validate(need(length(tp) > 0, "Select at least one timepoint (Setup, Processing)."))
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
    dtx(wide, rownames = FALSE, class = "compact stripe nowrap small-table",
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

  param_desc <- function() {
    if (isTRUE(is_var_feat(input$param))) {
      return(div(class = "choice-help", HTML(paste0("<b>", wbp_feature_name(input$param), "</b>. Within-session breathing variability feature; ",
                                                    "see the Guide for how it is computed."))))
    }
    info <- wbp_parameter_info()
    i <- match(input$param, info$parameter)
    if (is.na(i)) return(NULL)
    div(class = "choice-help", HTML(paste0("<b>", info$name[i], "</b>",
                                           if (nzchar(info$unit[i])) paste0(" (", info$unit[i], ")"), ". ", info$description[i])))
  }
  output$param_description <- renderUI(param_desc())
  output$param_description_cmp <- renderUI(param_desc())

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
      auc = paste0("AUC of ", lab, " \u00d7 days"),
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
    gs <- group_sum(); s <- sessions(); xa <- input$tc_x
    if (identical(xa, "dpi")) {
      dpi <- design_dpi_r()
      validate(need(!is.null(dpi), "Days post-infection needs a design file with infection_session set (Setup, Groups)."))
      gs$day <- dpi[as.character(gs$timepoint)]
      s$day <- dpi[as.character(s$timepoint)]
      xa <- "day"
    }
    st <- if (!isTRUE(input$tc_stats) || !has_comparisons()) NULL else if (identical(input$tc_method, "mixed")) tc_mm_stats(p) else tp_stats()
    g <- plot_timecourse(gs, p, s, colors(), error = input$tc_error, error_style = input$tc_style,
                         x_axis = xa, show_individuals = isTRUE(input$tc_individuals),
                         stats = st, transform = transform(),
                         log_y = isTRUE(input$tc_log), facet_groups = isTRUE(input$tc_facet))
    if (identical(input$tc_x, "dpi")) g <- g + ggplot2::labs(x = "Days post-infection") + ggplot2::geom_vline(xintercept = 0, linetype = "dotted", color = "grey50")
    g
  }
  make_cmp <- function(p) {
    plot_group_comparison(subj_vals(), p, comparisons = if (isTRUE(input$cmp_stats)) comps() else NULL,
                          colors = colors(), style = input$cmp_style, show_ns = isTRUE(input$cmp_ns),
                          y_label = metric_label(p))
  }
  heatmap_data <- reactive({
    sel <- input$params_multi
    validate(need(length(sel) >= 1, "Choose parameters under Figures in the toolbar."))
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
    validate(need(length(sel) >= 2, "PCA needs at least 2 parameters (choose them under Figures in the toolbar)."))
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
      value_box("Animals analyzed", sum(n), p(paste(sprintf("%s: %d", names(n), as.integer(n)), collapse = " \u00b7 ")),
                theme = "primary"),
      value_box("Timepoints", nlevels(s$timepoint), p(paste(levels(s$timepoint)[c(1, nlevels(s$timepoint))], collapse = " \u2192 ")),
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
      p = signif(d$p, 3),
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
    dtx(f, rownames = FALSE, selection = "single", class = "compact hover",
              options = list(dom = "ftip", pageLength = 12)) %>%
      formatStyle("% difference", color = styleInterval(0, c("#2166AC", "#B2182B")), fontWeight = "bold") %>%
      formatStyle("Adjusted p", target = "row", backgroundColor = styleInterval(0.05, c("#e3f1e8", "white")))
  })

  observeEvent(input$findings_table_rows_selected, {
    f <- findings()
    p <- f$Parameter[input$findings_table_rows_selected]
    updateSelectInput(session, "param", selected = p)
    nav_select("results_tabs", "cmp")
  })

  # -- Time course -----------------------------------------------------------------
  design_sex <- reactive({
    d <- rv$design
    if (is.null(d) || all(is.na(d$animals$sex))) return(NULL)
    stats::setNames(d$animals$sex, d$animals$animal)
  })
  output$tc_cov_ui <- renderUI({
    ch <- c("Baseline value (first session)" = "baseline")
    if (!is.null(design_sex())) ch <- c(ch, "Sex" = "sex")
    if ("Weight" %in% names(sessions())) ch <- c(ch, "Body weight" = "weight")
    tagList(checkboxGroupInput("tc_cov", label_with("Adjust for", "Covariates in the mixed model. Baseline adjustment removes differences that existed before treatment; sex and body weight come from the study design file."),
                               choices = ch, selected = intersect(isolate(input$tc_cov), ch)))
  })
  mm_fit <- function(p) {
    tryCatch(mixed_timecourse(sessions(), p, reference(), covariates = input$tc_cov %||% character(0), sex = design_sex(),
                              log_transform = isTRUE(input$tc_mm_log), p_adjust = input$padj),
             error = function(e) validate(need(FALSE, paste("Mixed model:", conditionMessage(e)))))
  }
  tc_mt <- reactive({ req(input$param); mm_fit(input$param) })
  tc_mm_stats <- function(p) {
    mt <- if (identical(p, input$param)) tc_mt() else mm_fit(p)
    data.frame(parameter = p, timepoint = mt$contrasts$timepoint, group = mt$contrasts$group,
               p = mt$contrasts$p, p_adj = mt$contrasts$p_adj, stars = mt$contrasts$stars)
  }
  output$tc_model_table <- renderDT({
    req(has_comparisons(), identical(input$tc_method, "mixed"))
    mt <- tc_mt()
    dtx(mt$anova, rownames = FALSE, class = "compact", caption = mt$settings$formula,
        options = list(dom = "t", paging = FALSE)) %>% formatSignif(c("F", "p"), 3) %>% formatRound("den_df", 0)
  })
  output$tc_plot <- renderPlot(make_tc(req(input$param)), res = 96)

  output$tc_table <- renderDT({
    req(has_comparisons(), input$param)
    gs <- group_sum()
    gs <- gs[gs$parameter == input$param, , drop = FALSE]
    gs$cell <- sprintf("%s \u00b1 %s (%d)", signif(gs$mean, 3), ifelse(is.na(gs$sem), "\u2013", signif(gs$sem, 2)), gs$n)
    wide <- tidyr::pivot_wider(gs[, c("timepoint", "group", "cell")], names_from = "group", values_from = "cell")
    st <- if (identical(input$tc_method, "mixed")) tc_mm_stats(input$param) else tp_stats()
    st <- st[st$parameter == input$param, , drop = FALSE]
    st$cell <- ifelse(is.na(st$p_adj), "", paste0(signif(st$p, 2), " (adj. ", signif(st$p_adj, 2), ")",
                                                  ifelse(st$stars == "ns", "", paste0(" ", st$stars))))
    st <- tidyr::pivot_wider(st[, c("timepoint", "group", "cell")], names_from = "group", values_from = "cell",
                             names_glue = "p (adj. p): {group}")
    out <- merge(wide, st, by = "timepoint", all.x = TRUE)
    out <- out[order(out$timepoint), , drop = FALSE]
    names(out)[1] <- "Timepoint"
    dtx(out, rownames = FALSE, class = "compact stripe", caption = sprintf(
                "Mean \u00b1 SEM (n animals) per group; p (adjusted p) vs %s.", reference()),
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
    dtx(out, rownames = FALSE, class = "compact", options = list(dom = "t", paging = FALSE)) %>%
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
    dtx(utils::head(pca_result()$variance, 6), rownames = FALSE, class = "compact",
              options = list(dom = "t")) %>% formatRound(c("Variance (%)", "Cumulative (%)"), 1)
  })
  output$pca_note <- renderUI({
    d <- pca_result()$dropped
    if (length(d) == 0) return(NULL)
    p(class = "small text-warning", "Left out (missing for some animals or constant): ", paste(d, collapse = ", "))
  })

  # -- Explore -----------------------------------------------------------------------
  ex_state <- reactiveValues(tp = NULL)
  observe({
    p <- params()
    cx <- isolate(input$ex_x); cy <- isolate(input$ex_y)
    updateSelectInput(session, "ex_x", choices = p, selected = if (isTRUE(cx %in% p)) cx else if ("f" %in% p) "f" else p[1])
    updateSelectInput(session, "ex_y", choices = p, selected = if (isTRUE(cy %in% p)) cy else if ("TVb" %in% p) "TVb" else p[min(2, length(p))])
    lv <- levels(sessions()$timepoint)
    if (!identical(lv, ex_state$tp)) {
      ex_state$tp <- lv
      updateSelectizeInput(session, "ex_tps", choices = lv, selected = lv[unique(c(1, ceiling(length(lv) / 2), length(lv)))])
    }
  })


  ex_assigned <- reactive({
    s <- assign_groups(sessions_base(), groups_ok())
    s <- s[s$timepoint %in% input$tp_include, , drop = FALSE]
    s$timepoint <- droplevels(s$timepoint)
    attr(s, "parameters") <- params()
    s
  })

  make_ex <- function(view) {
    sel <- input$params_multi
    switch(view,
      dashboard = {
        validate(need(length(sel) >= 1, "Choose parameters under Figures in the toolbar."))
        plot_dashboard(group_sum(), sel, colors(), input$tc_error %||% "sem")
      },
      heatmap = plot_animal_heatmap(sessions(), req(input$param), input$ex_scale),
      correlation = {
        validate(need(length(sel) >= 2, "Choose at least 2 parameters under Figures in the toolbar."))
        plot_correlation(sessions(), sel, input$ex_level, input$ex_method)
      },
      profile = {
        validate(need(has_comparisons(), "Profiles compare groups, so they need at least two groups."))
        validate(need(length(sel) >= 3, "Choose at least 3 parameters under Figures in the toolbar."))
        plot_group_profile(subj_vals(), reference(), sel, colors(), input$ex_limit)
      },
      forest = {
        validate(need(has_comparisons(), "Effect sizes compare groups, so they need at least two groups."))
        plot_effect_forest(comps(), sel, colors())
      },
      distribution = {
        validate(need(isTRUE(input$param %in% names(rv$raw)), "Distributions are available for respiratory parameters, not derived features."))
        validate(need(length(input$ex_tps) >= 1, "Choose at least one session."))
        plot_record_distribution(assign_groups(rv$raw, groups_ok()), input$param, input$ex_tps, input$tp_def, colors())
      },
      trajectory = {
        validate(need(!identical(input$ex_x, input$ex_y), "Choose two different parameters."))
        plot_trajectory(summarize_groups(sessions(), c(input$ex_x, input$ex_y)), input$ex_x, input$ex_y, sessions(), colors(), input$ex_smooth)
      },
      cfu_overlay = {
        d <- rv$design
        validate(need(!is.null(d) && nrow(d$cfu) > 0, "Load a study design file with CFU values (Setup, Groups)."))
        validate(need(!is.null(design_dpi_r()), "Set infection_session in the Study sheet of the design file."))
        plot_cfu_overlay(group_sum(), req(input$param), d$cfu, design_dpi_r(), colors())
      },
      cfu_correlation = {
        d <- rv$design
        validate(need(!is.null(d) && nrow(d$cfu) > 0, "Load a study design file with CFU values (Setup, Groups)."))
        validate(need(!is.null(design_dpi_r()), "Set infection_session in the Study sheet of the design file."))
        cc <- tryCatch(cfu_correlation(sessions(), d$cfu, design_dpi_r(), input$params_multi), error = function(e) validate(need(FALSE, conditionMessage(e))))
        validate(need(isTRUE(input$param %in% cc$correlations$parameter), "Choose a parameter included under Figures in the toolbar."))
        plot_cfu_correlation(cc, input$param, colors())
      },
      waterfall = {
        s <- ex_assigned()
        lv <- levels(s$timepoint)
        w <- window()
        from <- if (identical(w$from, lv[1]) && length(lv) > 1) lv[2] else w$from
        plot_waterfall(s, req(input$param), from = from, to = w$to, colors = colors())
      })
  }
  ex_size <- function(view) {
    n_an <- length(unique(sessions()$subject))
    switch(view,
      dashboard = c(12, 2.7 * ceiling(max(1, length(input$params_multi)) / 4) + 1.2),
      heatmap = c(10, 0.24 * n_an + 2.2),
      correlation = c(8.5, 7.5),
      profile = c(8.5, 7.5),
      forest = c(8.5, 0.32 * length(input$params_multi) * max(1, n_groups() - 1) / 1.5 + 2.5),
      distribution = c(10, 3.6 * ceiling(length(input$ex_tps) / 3) + 1),
      trajectory = c(8.5, 6.5),
      waterfall = c(max(8, 0.32 * n_an + 2), 5.5),
      cfu_overlay = c(9, 7),
      cfu_correlation = c(7.5, 6))
  }
  for (v in names(ex_help_text)) local({
    view <- v
    output[[paste0("ex_plot_", view)]] <- renderPlot(make_ex(view), res = 96, height = function() min(2400, 96 * ex_size(view)[2]))
    register_download(paste0("dl_ex_", view),
                      function() paste0(view, if (view %in% c("heatmap", "distribution", "waterfall", "cfu_overlay", "cfu_correlation")) paste0("_", safe_name(input$param)) else ""),
                      function() make_ex(view), function() ex_size(view)[1], function() ex_size(view)[2])
  })

  # -- Animal trends -----------------------------------------------------------------
  observe({
    lv <- levels(sessions()$timepoint)
    cur <- isolate(input$at_from)
    updateSelectInput(session, "at_from", choices = lv, selected = if (isTRUE(cur %in% lv)) cur else lv[1])
  })
  at_trends <- reactive({
    s <- sessions()
    req(input$param %in% names(s))
    lv <- levels(s$timepoint)
    from <- if (isTRUE(input$at_from %in% lv)) input$at_from else lv[1]
    animal_trends(s, input$param, from = from)
  })
  make_at <- function() {
    s <- sessions()
    plot_animal_trends(s, input$param, at_trends(), colors(), method = input$at_method,
                       reference_line = if (transform() == "percent") 100 else if (transform() == "difference") 0 else NULL,
                       y_label = wbp_axis_label(input$param, transform()),
                       title = paste0(wbp_parameter_info()$name[match(input$param, wbp_parameter_info()$parameter)] %||% input$param, " by animal"))
  }
  at_height <- function() 170 * ceiling(length(unique(sessions()$subject)) / 4) + 140
  output$at_plot <- renderPlot(make_at(), res = 96, height = function() at_height())
  output$at_table <- renderDT({
    t <- at_trends()[, c("group", "subject", "n", "slope_per_week", "change", "p", "direction")]
    names(t) <- c("Group", "Animal", "Sessions", "Slope per week", "Change over window", "p", "Direction")
    dtx(t, rownames = FALSE, class = "compact", options = list(dom = "t", paging = FALSE)) %>% formatSignif(4:6, 3)
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
    dtx(d, rownames = FALSE, filter = "top", class = "compact stripe",
              extensions = "Buttons",
              options = list(pageLength = 25, scrollX = TRUE, dom = "Bfrtip", buttons = c("copy", "csv"))) %>%
      formatSignif(num, 4)
  })

  # -- Per-figure downloads ---------------------------------------------------------------
  register_download("dl_tc", function() paste0("timecourse_", safe_name(input$param)),
                    function() make_tc(input$param), function() 9, function() 5.5)
  register_download("dl_cmp", function() paste0("comparison_", input$metric, "_", safe_name(input$param)),
                    function() make_cmp(input$param), function() max(4.5, 1.2 * n_groups() + 2), function() 5.5)
  register_download("dl_hm", function() paste0("heatmap_", input$hm_mode),
                    make_hm, function() 8, function() max(4.5, 0.42 * length(input$params_multi) + 2.5))
  register_download("dl_pca", function() paste0("pca_", input$metric),
                    function() pca_result()$plot, function() 8.5, function() 6.5)
  register_download("dl_at", function() paste0("animal_trends_", safe_name(input$param)), make_at,
                    function() 12, function() at_height() / 96)

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
        sprintf("%d \u00d7 %d design: %s (%s) \u00d7 %s (%s).", length(unique(d$A)), length(unique(d$B)),
                fac_names()[1], paste(unique(d$A), collapse = ", "), fac_names()[2], paste(unique(d$B), collapse = ", ")))
  })

  fac_results <- reactive({
    validate(need(is.null(fac_problem()), fac_problem()))
    sel <- input$params_multi
    validate(need(length(sel) >= 1, "Choose parameters under Figures in the toolbar."))
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
    long <- rbind(data.frame(parameter = r$parameter, col = paste0(r$term, ": p"), val = r$p),
                  data.frame(parameter = r$parameter, col = paste0(r$term, ": adj. p"), val = r$p_adj))
    long$col <- factor(long$col, levels = as.vector(rbind(paste0(terms, ": p"), paste0(terms, ": adj. p"))))
    long <- long[order(long$col), , drop = FALSE]
    wide <- as.data.frame(tidyr::pivot_wider(long, names_from = "col", values_from = "val"))
    names(wide)[1] <- "Parameter"
    raw_cols <- paste0(terms, ": p"); adj_cols <- paste0(terms, ": adj. p")
    dtx(wide, rownames = FALSE, selection = "single", class = "compact hover",
              caption = sprintf("%s: unadjusted p and adjusted p (%s correction across parameters) for each term. Green: adjusted p < 0.05; bold: p < 0.05. Click a row for details.",
                                if (input$fac_model == "mixed") "Mixed model" else "Two-way ANOVA", padj_names[[input$padj]]),
              options = list(dom = "t", paging = FALSE, scrollX = TRUE, ordering = FALSE)) %>%
      formatSignif(c(raw_cols, adj_cols), 3) %>%
      formatStyle(raw_cols, fontWeight = styleInterval(0.05, c("bold", "normal"))) %>%
      formatStyle(adj_cols, backgroundColor = styleInterval(0.05, c("#d4edda", "white")),
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
      n_parts <- length(strsplit(term, " \u00d7 ", fixed = TRUE)[[1]])
      if (term == "Time") return(paste0(prm, " changes over time."))
      if (n_parts == 1) return(paste0(term, " has an overall effect on ", prm, ", averaged over the other factor",
                                      if (input$fac_model == "mixed") " and time" else "", "."))
      if (n_parts == 2 && has("Time")) return(paste0("The effect of ", sub(" \u00d7 Time", "", term, fixed = TRUE), " on ", prm, " changes over time."))
      if (n_parts == 2) return(paste0("The effect of ", fn[1], " on ", prm, " depends on ", fn[2], " (interaction)."))
      paste0("The ", fn[1], " \u00d7 ", fn[2], " interaction changes over time.")
    }
    sig <- r[!is.na(r$p_adj) & r$p_adj < 0.05, , drop = FALSE]
    div(class = "mt-2",
      p(tags$b(prm, ": "), if (nrow(sig) == 0) "no term reached adjusted p < 0.05." else "significant terms:"),
      if (nrow(sig) > 0) tags$ul(lapply(seq_len(nrow(sig)), function(i)
        tags$li(sprintf("%s (F(%g, %g) = %.2f, p = %s, adjusted p = %s): %s", sig$term[i], sig$num_df[i], round(sig$den_df[i]),
                        sig$F[i], format.pval(sig$p[i], digits = 2, eps = 1e-4), format.pval(sig$p_adj[i], digits = 2, eps = 1e-4),
                        explain(sig$term[i]))))),
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
  ml_state <- reactiveValues(groups = NULL, tp = NULL, params = NULL)

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
    updateSelectInput(session, "ml_inf_tp", choices = lv, selected = lv[min(2, length(lv))])
  })

  observe({
    p <- params()
    if (identical(p, ml_state$params)) return()
    ml_state$params <- p
    updateSelectizeInput(session, "ml_params", choices = p, selected = isolate(input$params_multi) %||% p)
  })

  # Model choices depend on the outcome type.
  observeEvent(input$ml_goal, {
    nm <- ml_model_names(if (input$ml_goal == "severity") "regression" else "classification")
    updateCheckboxGroupInput(session, "ml_models", choices = stats::setNames(names(nm), nm), selected = names(nm))
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

  # Severity table
  ml_sev_raw <- reactive({
    req(input$ml_sev_file)
    f <- input$ml_sev_file
    tryCatch(
      if (grepl("\\.csv$", f$name, ignore.case = TRUE)) utils::read.csv(f$datapath, check.names = FALSE, stringsAsFactors = FALSE)
      else as.data.frame(readxl::read_excel(f$datapath)),
      error = function(e) validate(need(FALSE, paste("Could not read the severity table:", conditionMessage(e)))))
  })

  output$ml_sev_cols_ui <- renderUI({
    d <- ml_sev_raw()
    cols <- names(d)
    num <- cols[vapply(d, function(x) is.numeric(x) || !all(is.na(suppressWarnings(as.numeric(x)))), logical(1))]
    guess <- function(pattern, pool, default) { h <- pool[grepl(pattern, pool, ignore.case = TRUE)]; if (length(h)) h[1] else default }
    tagList(
      selectInput("ml_sev_subject", "Animal column", choices = cols, selected = guess("animal|subject|mouse|id|sheet", cols, cols[1])),
      selectInput("ml_sev_value", "Value column", choices = num, selected = guess("cfu|score|sever|value|weight", num, num[length(num)])),
      selectInput("ml_sev_tp", "Timepoint column (optional)", choices = c("None (one value per animal)" = "", cols),
                  selected = guess("time|phase|week|day|session", cols, "")),
      p(class = "small text-muted", sprintf("%d rows read.", nrow(d)))
    )
  })

  output$ml_sev_template <- downloadHandler(
    filename = function() "severity_template.xlsx",
    content = function(file) {
      subj <- if (is.null(rv$raw)) character(0) else unique(rv$raw$subject)
      writexl::write_xlsx(list(Severity = data.frame(animal = subj, value = NA_real_)), path = file)
    }
  )

  output$ml_sev_source_ui <- renderUI({
    d <- rv$design
    if (is.null(d) || !any(!is.na(d$cfu$animal))) return(NULL)
    radioButtons("ml_sev_source", "Severity values", inline = TRUE,
                 choices = c("Per-animal CFU from the design file" = "design", "Upload a table" = "upload"), selected = "design")
  })

  ml_severity_table <- function() {
    if (identical(input$ml_sev_source, "design") && !is.null(rv$design)) {
      cf <- rv$design$cfu[!is.na(rv$design$cfu$animal), , drop = FALSE]
      a <- stats::aggregate(log10_cfu ~ animal, data = cf, FUN = mean)
      return(data.frame(subject = a$animal, value = a$log10_cfu))
    }
    d <- ml_sev_raw()
    req(input$ml_sev_subject, input$ml_sev_value)
    out <- data.frame(subject = as.character(d[[input$ml_sev_subject]]), value = suppressWarnings(as.numeric(d[[input$ml_sev_value]])))
    if (isTRUE(nzchar(input$ml_sev_tp))) out$timepoint <- as.character(d[[input$ml_sev_tp]])
    out
  }

  observeEvent(input$ml_run, {
    goal <- input$ml_goal
    res <- tryCatch({
      ml_check_packages()
      if (length(input$ml_models) == 0) stop("Select at least one model.")
      prm <- input$ml_params
      if (length(prm) < 2) stop("Select at least 2 respiratory parameters as predictors.")
      cv <- if (isTRUE(input$ml_use_cov) && goal != "severity") ml_cov() else NULL
      cov_map <- cv$map
      cov_name <- if (is.null(cv)) "covariate" else make.names(cv$name)
      s <- sessions()
      d <- switch(goal,
        infection = ml_prepare_infection(s, input$ml_pos, input$ml_neg, infection_timepoint = input$ml_inf_tp,
                                         pre_infection = input$ml_pre, parameters = prm, include_day = isTRUE(input$ml_day),
                                         covariate = cov_map, covariate_name = cov_name),
        phase = ml_prepare_phase(s, input$ml_pos, input$ml_inf_tp, acute_days = input$ml_acute_days, offset = input$ml_offset,
                                 parameters = prm, covariate = cov_map, covariate_name = cov_name),
        severity = ml_prepare_severity(s, ml_severity_table(), outcome_name = input$ml_sev_name, log_outcome = isTRUE(input$ml_sev_log),
                                       parameters = prm, exclude_timepoints = input$ml_exclude, include_day = isTRUE(input$ml_day)),
        groups = {
          labs <- make.names(c(trimws(input$ml_pos_label) %||% "Positive", trimws(input$ml_neg_label) %||% "Negative"))
          ml_prepare(s, input$ml_pos, input$ml_neg, labels = labs, parameters = prm, exclude_timepoints = input$ml_exclude,
                     include_day = isTRUE(input$ml_day), covariate = cov_map, covariate_name = cov_name)
        })
      run <- function(data, msg) withProgress(message = msg, value = 0, {
        ml_fit(data, models = input$ml_models, validation = input$ml_validation, tuning = input$ml_tuning,
               seed = input$ml_seed, parallel = isTRUE(input$ml_parallel),
               progress = function(i, n, name) setProgress((i - 1) / n, detail = sprintf("%s (%d of %d)", name, i, n)))
      })
      out <- run(d, "Training models")
      if (goal == "infection" && input$ml_pre == "uninfected" && isTRUE(input$ml_phase_check)) {
        dc <- tryCatch(ml_prepare_infection(s, input$ml_neg, input$ml_pos, infection_timepoint = input$ml_inf_tp,
                                            pre_infection = "uninfected", parameters = prm, include_day = isTRUE(input$ml_day),
                                            covariate = cov_map, covariate_name = cov_name), error = function(e) NULL)
        if (!is.null(dc)) out$time_check <- run(dc, "Time-only check (placebo)")$metrics
      }
      if (goal == "phase" && isTRUE(input$ml_phase_check) && length(input$ml_neg)) {
        dc <- tryCatch(ml_prepare_phase(s, input$ml_neg, input$ml_inf_tp, acute_days = input$ml_acute_days, offset = input$ml_offset,
                                        parameters = prm, covariate = cov_map, covariate_name = cov_name), error = function(e) NULL)
        if (!is.null(dc)) out$time_check <- run(dc, "Time-only check on control animals")$metrics
      }
      out
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = NULL)
      NULL
    })
    if (is.null(res)) return()
    st <- attr(sessions_base(), "settings")
    res$processing <- list(timepoint = st$timepoint, stat = st$stat, rinx_max = st$rinx_max,
                           baseline = input$baseline_method, source = rv$file_name)
    ml_res(res)
    if (length(res$settings$unmatched)) {
      showNotification(paste("Severity values for these animals had no match in the data:", paste(res$settings$unmatched, collapse = ", ")),
                       type = "warning", duration = 15)
    }
    showNotification(sprintf("Trained %d model(s).", length(res$fits)), type = "message")
  })

  ml_is_reg <- reactive(!is.null(ml_res()) && ml_type(ml_res()) == "regression")
  ml_best <- function(obj) {
    ses <- obj$metrics[obj$metrics$level == "session", ]
    ses$model[which.max(if (ml_type(obj) == "regression") ses$rsq else ses$roc_auc)]
  }

  observeEvent(ml_res(), {
    obj <- ml_res()
    m <- names(obj$fits)
    nm <- stats::setNames(m, ml_model_names("all")[m])
    best <- ml_best(obj)
    for (id in c("ml_model_cm", "ml_model_time", "ml_model_animals", "ml_model_new")) updateSelectInput(session, id, choices = nm, selected = best)
    imp_models <- intersect(m, unique(obj$importance$model))
    updateSelectInput(session, "ml_model_imp", choices = stats::setNames(imp_models, ml_model_names("all")[imp_models]),
                      selected = if (best %in% imp_models) best else imp_models[1])
  })

  ml_need <- function() validate(need(!is.null(ml_res()), "Choose the settings on the left and click \"Train models\"."))

  output$ml_intro <- renderUI({
    if (!is.null(ml_res())) return(NULL)
    div(class = "step-intro mt-2",
      p("Train models that predict an animal's state from its breathing parameters:"),
      tags$ul(
        tags$li(tags$b("Infected vs uninfected:"), " the CP05 infection-prediction pipeline (logistic regression, k-nearest neighbors, linear and quadratic discriminant analysis, ",
                "elastic net, random forest and gradient-boosted trees; centered and scaled predictors, upsampling, tuning by ROC AUC)."),
        tags$li(tags$b("Acute vs chronic:"), " the same models, within infected animals, with a time-only check on controls."),
        tags$li(tags$b("Disease severity:"), " regression models (linear regression, k-nearest neighbors, elastic net, random forest, boosted trees) predicting a value you measured, ",
                "such as lung CFU or histology score. Upload a table with one value per animal."),
        tags$li(tags$b("Any two sets of groups:"), " for other comparisons.")),
      p("Each row is one animal at one session. Performance is reported for single sessions and for whole animals (averaging each animal's predictions)."),
      p(class = "text-muted", "Training takes 1 to 3 minutes with Quick tuning, and much longer with Thorough tuning."))
  })

  output$ml_summary <- renderUI({
    ml_need()
    obj <- ml_res()
    m <- obj$metrics
    reg <- ml_type(obj) == "regression"
    ses <- m[m$level == "session", ]; ani <- m[m$level == "animal", ]
    best <- ml_best(obj)
    b_s <- ses[ses$model == best, ]; b_a <- ani[ani$model == best, ]
    n_an <- length(unique(obj$data$subject))
    random <- obj$settings$validation == "random"
    score_a <- if (reg) b_a$rsq else b_a$roc_auc
    verdict <- if (random) {
      div(class = "alert alert-warning py-2",
          tags$b("Random split: likely optimistic. "), "Sessions of the same animal are in both training and test data, so models can recognize ",
          "individual animals instead of the condition. Re-run with \"Hold out whole animals\" to see how well the models work on new animals.")
    } else if (reg) {
      if (score_a >= 0.5) div(class = "alert alert-success py-2", sprintf("Breathing predicts %s well in animals the models had not seen (animal-level R\u00b2 %.2f, r = %.2f). ",
                                                                         obj$settings$outcome_name, b_a$rsq, b_a$r),
                              sprintf("With %d animals the estimate is uncertain; confirm in an independent cohort.", n_an))
      else if (score_a >= 0.2) div(class = "alert alert-info py-2", sprintf("Breathing partly predicts %s in held-out animals (animal-level R\u00b2 %.2f). Treat as preliminary.",
                                                                            obj$settings$outcome_name, b_a$rsq))
      else div(class = "alert alert-secondary py-2", tags$b(sprintf("The models did not predict %s in animals they had not seen ", obj$settings$outcome_name)),
               sprintf("(animal-level R\u00b2 %.2f; 0 means no better than guessing the average).", b_a$rsq))
    } else if (score_a >= 0.8) {
      div(class = "alert alert-success py-2", sprintf("Good separation of %s and %s in animals the models had not seen (best animal-level ROC AUC %.2f). ",
                                                       obj$settings$labels[1], obj$settings$labels[2], score_a),
          sprintf("With %d animals the estimate is uncertain; confirm in an independent cohort.", n_an))
    } else if (score_a >= 0.65) {
      div(class = "alert alert-info py-2", sprintf("Moderate separation in held-out animals (best animal-level ROC AUC %.2f). ", score_a), "Treat as preliminary.")
    } else {
      div(class = "alert alert-secondary py-2", tags$b("The models did not reliably tell the classes apart in animals they had not seen "),
          sprintf("(best animal-level ROC AUC %.2f; 0.5 is chance). ", score_a),
          "Breathing parameters may not carry a consistent signal for this outcome, or more animals are needed.")
    }
    check <- NULL
    if (!is.null(obj$time_check)) {
      tc <- obj$time_check[obj$time_check$level == "session" & obj$time_check$model == best, ]
      if (nrow(tc)) {
        same <- tc$roc_auc >= b_s$roc_auc - 0.05
        check <- div(class = paste("alert py-2", if (same) "alert-warning" else "alert-success"),
                     tags$b("Time-only check: "),
                     sprintf("%s, %s reaches ROC AUC %.2f (vs %.2f for the real labels). ",
                             if (identical(obj$settings$task, "phase")) "on control animals labeled by the same cutoff" else "on a placebo with uninfected controls labeled as infected after the same session",
                             ml_model_names("all")[[best]], tc$roc_auc, b_s$roc_auc),
                     if (same) "The placebo scores about as well, so the model is mostly detecting time (age, growth, habituation), not infection."
                     else "The placebo scores lower, so part of the signal is specific to infection.")
      }
    }
    tagList(
      layout_columns(
        fill = FALSE,
        value_box("Best model", ml_model_names("all")[[best]],
                  p(sprintf("highest session-level %s (%s)", if (reg) "R\u00b2" else "ROC AUC", if (random) "test sessions" else "held-out animals")), theme = "primary"),
        if (reg) value_box("Session R\u00b2", sprintf("%.2f", b_s$rsq), p(sprintf("r = %.2f, %d sessions", b_s$r, b_s$n)), theme = "info")
        else value_box("Session ROC AUC", sprintf("%.2f", b_s$roc_auc), p(sprintf("accuracy %.0f%%, %d sessions", 100 * b_s$accuracy, b_s$n)), theme = "info"),
        if (reg) value_box("Animal R\u00b2", sprintf("%.2f", b_a$rsq), p(sprintf("r = %.2f, %d animals", b_a$r, b_a$n)), theme = "secondary")
        else value_box("Animal ROC AUC", sprintf("%.2f", b_a$roc_auc), p(sprintf("accuracy %.0f%%, %d animals", 100 * b_a$accuracy, b_a$n)), theme = "secondary")
      ),
      verdict, check,
      if (length(obj$errors)) div(class = "alert alert-warning py-2", "Could not fit: ",
                                  paste(sprintf("%s (%s)", ml_model_names("all")[names(obj$errors)], unlist(obj$errors)), collapse = "; "))
    )
  })

  output$ml_perf_plot <- renderPlot({ ml_need(); plot_ml_performance(ml_res()) }, res = 96)

  output$ml_metrics_table <- renderDT({
    ml_need()
    obj <- ml_res()
    m <- obj$metrics
    out <- if (ml_type(obj) == "regression") {
      stats::setNames(data.frame(m$model_name, m$level, m$rsq, m$r, m$rmse, m$mae, m$n),
                      c("Model", "Level", "R\u00b2", "r", "RMSE", "MAE", "n"))
    } else {
      data.frame(Model = m$model_name, Level = m$level, `ROC AUC` = m$roc_auc, Accuracy = m$accuracy,
                 Sensitivity = m$sensitivity, Specificity = m$specificity, n = m$n, check.names = FALSE)
    }
    out$`Tuned parameters` <- ifelse(m$level == "session", obj$tuning$parameters[match(m$model, obj$tuning$model)], "")
    num <- setdiff(names(out), c("Model", "Level", "n", "Tuned parameters"))
    dtx(out, rownames = FALSE, class = "compact small-table", options = list(dom = "t", paging = FALSE, scrollX = TRUE)) %>%
      formatSignif(num, 3)
  })

  # For categories: ROC curves and confusion matrix. For severity: observed vs predicted.
  make_ml_detail <- function() {
    if (ml_type(ml_res()) == "regression") plot_ml_observed(ml_res(), req(input$ml_model_cm), input$ml_level, ml_time_colors())
    else plot_ml_roc(ml_res(), input$ml_level)
  }
  make_ml_cm <- function() {
    validate(need(ml_type(ml_res()) != "regression", "Confusion matrices are for categories. For severity, see observed vs predicted on the left."))
    plot_ml_confusion(ml_res(), req(input$ml_model_cm), input$ml_level)
  }
  output$ml_roc_plot <- renderPlot({ ml_need(); make_ml_detail() }, res = 96)
  output$ml_cm_plot <- renderPlot({ ml_need(); make_ml_cm() }, res = 96)
  ml_time_colors <- reactive({
    g <- names(groups_ok())
    plethr_colors(g, input$palette)
  })
  output$ml_time_plot <- renderPlot({ ml_need(); req(input$ml_model_time); plot_ml_over_time(ml_res(), input$ml_model_time, ml_time_colors()) }, res = 96)
  output$ml_imp_plot <- renderPlot({
    ml_need()
    validate(need(isTRUE(nzchar(input$ml_model_imp)), "Variable importance is available for linear and logistic regression, elastic net, random forest and boosted trees."))
    plot_ml_importance(ml_res(), input$ml_model_imp)
  }, res = 96)

  output$ml_animals_table <- renderDT({
    ml_need(); req(input$ml_model_animals)
    obj <- ml_res()
    p <- obj$predictions[obj$predictions$model == input$ml_model_animals, , drop = FALSE]
    if (ml_type(obj) == "regression") {
      a <- stats::aggregate(cbind(outcome, pred) ~ subject + group, data = p, FUN = mean)
      a$error <- a$pred - a$outcome
      a <- a[order(a$group, a$subject), c("group", "subject", "outcome", "pred", "error")]
      names(a) <- c("Group", "Animal", paste("Measured", obj$settings$outcome_name), paste("Predicted", obj$settings$outcome_name), "Error")
      return(dtx(a, rownames = FALSE, class = "compact", options = list(dom = "t", paging = FALSE)) %>% formatSignif(3:5, 3))
    }
    a <- stats::aggregate(prob ~ subject + group + outcome, data = p, FUN = mean)
    a$n <- as.vector(table(paste(p$subject, p$outcome))[paste(a$subject, a$outcome)])
    a$predicted <- ifelse(a$prob >= 0.5, obj$settings$labels[1], obj$settings$labels[2])
    a$correct <- ifelse(a$predicted == as.character(a$outcome), "yes", "no")
    a <- a[order(a$group, a$subject), c("group", "subject", "outcome", "prob", "predicted", "correct", "n")]
    names(a) <- c("Group", "Animal", "Actual", paste0("P(", obj$settings$labels[1], ")"), "Predicted", "Correct", "Sessions")
    dtx(a, rownames = FALSE, class = "compact", options = list(dom = "t", paging = FALSE)) %>%
      formatRound(4, 2) %>%
      formatStyle("Correct", color = styleEqual(c("yes", "no"), c("#2E8B57", "#B2182B")), fontWeight = "bold")
  })

  register_download("dl_ml_perf", function() "ml_performance", function() plot_ml_performance(ml_res()), function() 8, function() 5)
  register_download("dl_ml_roc", function() paste0("ml_detail_", input$ml_level), make_ml_detail,
                    function() 8, function() 6)
  register_download("dl_ml_cm", function() paste0("ml_confusion_", input$ml_model_cm), make_ml_cm,
                    function() 5, function() 4.5)
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
    vf <- obj$settings$parameters[is_var_feat(obj$settings$parameters)]
    if (length(vf)) {
      code <- sub("^.*_", "", vf)
      base_p <- sub("_[^_]+$", "", vf)
      vd <- withProgress(message = "Computing variability features for the new file", value = 0.5,
                         session_variability(raw, unique(base_p), unique(code), timepoint = pr$timepoint %||% "phase", rinx_max = pr$rinx_max))
      s <- add_variability(s, vd)
    }
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
    if (ml_type(ml_res()) == "regression") {
      v <- pr$animals[[2]]
      return(div(class = "alert alert-info py-2",
                 sprintf("%d animals: predicted %s from %.3g to %.3g (%s). ", nrow(pr$animals), ml_res()$settings$outcome_name,
                         min(v), max(v), ml_model_names("all")[[input$ml_model_new]]),
                 "Predictions are only as reliable as the held-out performance on the Performance tab."))
    }
    n_pos <- sum(pr$animals$predicted == lab[1])
    div(class = "alert alert-info py-2",
        sprintf("%d animals: %d predicted %s, %d predicted %s (%s). ", nrow(pr$animals), n_pos, lab[1],
                nrow(pr$animals) - n_pos, lab[2], ml_model_names("all")[[input$ml_model_new]]),
        "Predictions are only as reliable as the held-out performance on the Performance tab.")
  })

  output$ml_new_table <- renderDT({
    req(input$ml_new_file)
    a <- ml_new_pred()$animals
    names(a)[names(a) == "subject"] <- "Animal"
    dtx(a, rownames = FALSE, class = "compact", options = list(dom = "t", paging = FALSE)) %>% formatRound(2, 2)
  })

  ml_new_plot <- function() {
    pr <- ml_new_pred()
    lab <- ml_res()$settings$labels
    reg <- ml_type(ml_res()) == "regression"
    oname <- ml_res()$settings$outcome_name
    s <- pr$sessions
    s$x <- as.integer(droplevels(s$timepoint))
    lv <- levels(droplevels(s$timepoint))
    g <- ggplot(s, aes(x = .data$x, y = .data$prob, group = .data$subject, color = .data$subject))
    if (!reg) g <- g + geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey55")
    g <- g + geom_line(linewidth = 0.7, alpha = 0.8) + geom_point(size = 1.8) +
      scale_x_continuous(breaks = seq_along(lv), labels = lv)
    if (!reg) g <- g + scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(x * 100, "%"))
    g +
      labs(title = if (reg) paste("Predicted", oname, "for each new animal") else paste0("Predicted probability of \"", lab[1], "\" for each new animal"),
           x = NULL, y = if (reg) paste("Predicted", oname) else paste0("P(", lab[1], ")"),
           color = NULL, subtitle = ml_model_names("all")[[input$ml_model_new]]) +
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

  # -- 6. Treatment efficacy -----------------------------------------------------------
  tr_state <- reactiveValues(groups = NULL, tp = NULL, params = NULL)
  role_colors <- c(Healthy = "#0072B2", Untreated = "#D55E00", Treated = "#009E73")

  observe({
    g <- names(groups_ok())
    if (identical(g, tr_state$groups)) return()
    tr_state$groups <- g
    is_treat <- grepl("treat|drug|therap|antibiot|compound|dose", g, ignore.case = TRUE) & !grepl("untreat", g, ignore.case = TRUE)
    is_dis <- grepl("infect|disease|untreat|vehicle|sick|expos", g, ignore.case = TRUE) &
      !grepl("uninfect|naive|healthy|sham|control", g, ignore.case = TRUE) & !is_treat
    healthy <- reference()
    updateSelectizeInput(session, "tr_healthy", choices = g, selected = healthy)
    updateSelectizeInput(session, "tr_disease", choices = g, selected = setdiff(g[is_dis], healthy))
    updateSelectizeInput(session, "tr_treated", choices = g, selected = setdiff(g[is_treat], healthy))
  })

  observe({
    lv <- levels(sessions()$timepoint)
    if (identical(lv, tr_state$tp)) return()
    tr_state$tp <- lv
    second <- lv[min(2, length(lv))]
    updateSelectInput(session, "tr_baseline_tp", choices = lv, selected = lv[1])
    updateSelectInput(session, "tr_onset", choices = lv, selected = second)
    updateSelectInput(session, "tr_from", choices = lv, selected = second)
    updateSelectInput(session, "tr_to", choices = lv, selected = lv[length(lv)])
  })

  observe({
    p <- params()
    if (identical(p, tr_state$params)) return()
    tr_state$params <- p
    updateSelectizeInput(session, "tr_params", choices = p, selected = isolate(input$params_multi) %||% p)
  })

  tr_lung <- reactive({
    validate(need(length(input$tr_healthy) > 0 && length(input$tr_disease) > 0, "Choose the healthy control and untreated diseased groups on the left."))
    validate(need(length(input$tr_params) >= 1, "Choose at least one lung health parameter."))
    tryCatch(lung_health_score(sessions(), input$tr_healthy, input$tr_disease, parameters = input$tr_params,
                               reference = input$tr_reference, baseline_timepoint = input$tr_baseline_tp,
                               onset_timepoint = input$tr_onset, min_effect = input$tr_min_effect),
             error = function(e) validate(need(FALSE, conditionMessage(e))))
  })

  # Machine learning disease score: trained on request, because it takes a while.
  tr_ml <- reactiveVal(NULL)
  tr_ml_key <- reactive(list(sessions = sessions(), healthy = input$tr_healthy, disease = input$tr_disease,
                             params = input$tr_params, onset = input$tr_onset, model = input$tr_ml_model))
  observeEvent(input$tr_train, {
    key <- tr_ml_key()
    res <- tryCatch({
      if (length(key$healthy) == 0 || length(key$disease) == 0) stop("Choose the healthy control and untreated diseased groups first.")
      if (length(key$params) < 2) stop("Choose at least 2 parameters for the machine learning score.")
      withProgress(message = "Training disease model", value = 0.3, {
        ml_disease_score(key$sessions, key$healthy, key$disease, parameters = key$params, onset_timepoint = key$onset,
                         model = key$model, seed = 10, parallel = FALSE)
      })
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = NULL)
      NULL
    })
    if (!is.null(res)) tr_ml(list(scored = res, key = key))
  })

  output$tr_ml_status <- renderUI({
    obj <- tr_ml()
    if (is.null(obj)) return(p(class = "small text-muted", "Not trained yet."))
    m <- attr(obj$scored, "ml_disease")$metrics
    a <- m[m$level == "animal", ]
    stale <- !identical(obj$key, tr_ml_key())
    tagList(
      p(class = "small", sprintf("%s trained. Telling untreated from healthy animals it had not seen: ROC AUC %.2f (animal level, %d animals).",
                                 ml_model_names("all")[[attr(obj$scored, "ml_disease")$model]], a$roc_auc, a$n)),
      if (a$roc_auc < 0.7) p(class = "small text-warning", "The model separates the groups weakly, so the score is noisy; the lung health score may be more reliable."),
      if (stale) p(class = "small text-danger", "Settings changed since training: click \"Train disease model\" again.")
    )
  })

  tr_scored <- reactive({
    if (identical(input$tr_score_type, "ml")) {
      obj <- tr_ml()
      validate(need(!is.null(obj), "Click \"Train disease model\" on the left to compute the machine learning score."))
      validate(need(identical(obj$key, tr_ml_key()), "Settings changed since the disease model was trained. Click \"Train disease model\" again."))
      obj$scored
    } else {
      tr_lung()
    }
  })
  tr_score_label <- reactive(if (identical(input$tr_score_type, "ml")) "Disease score (log-odds vs healthy)" else "Lung health score (0 = healthy)")

  tr_trends <- reactive({
    sc <- tr_scored()
    sc <- sc[as.character(sc$group) %in% tr_groups_used(), , drop = FALSE]
    sc$group <- droplevels(sc$group)
    lv <- levels(sc$timepoint)
    from <- if (isTRUE(input$tr_from %in% lv)) input$tr_from else lv[1]
    to <- if (isTRUE(input$tr_to %in% lv) && match(input$tr_to, lv) >= match(from, lv)) input$tr_to else lv[length(lv)]
    list(sc = sc, trends = animal_trends(sc, "lung_score", from, to, higher_is = "worse"))
  })
  make_tr_trend <- function() {
    tt <- tr_trends()
    plot_animal_trends(tt$sc, "lung_score", tt$trends, plethr_colors(names(groups_ok()), input$palette)[levels(tt$sc$group)],
                       method = input$tr_trend_method, higher_is = "worse", reference_line = 0,
                       y_label = tr_score_label(), title = "Health score of each animal")
  }
  tr_trend_height <- function() 170 * ceiling(length(unique(tr_trends()$sc$subject)) / 4) + 140
  output$tr_trend_plot <- renderPlot(make_tr_trend(), res = 96, height = function() tr_trend_height())
  output$tr_trend_table <- renderDT({
    t <- tr_trends()$trends
    t <- t[, c("group", "subject", "n", "slope_per_week", "change", "p", "direction")]
    names(t) <- c("Group", "Animal", "Sessions", "Slope per week", "Change over window", "p", "Direction")
    dtx(t, rownames = FALSE, class = "compact", options = list(dom = "t", paging = FALSE)) %>%
      formatSignif(4:6, 3) %>%
      formatStyle("Direction", color = styleEqual(c("improving", "worsening"), c("#2E8B57", "#B2182B")), fontWeight = "bold")
  })
  register_download("dl_tr_trend", function() "health_score_by_animal", make_tr_trend,
                    function() 12, function() tr_trend_height() / 96)

  tr_eff <- reactive({
    validate(need(length(input$tr_treated) > 0, "Choose the treated group(s) on the left to evaluate the treatment."))
    lv <- levels(sessions()$timepoint)
    req(input$tr_from %in% lv, input$tr_to %in% lv)
    to <- if (match(input$tr_to, lv) < match(input$tr_from, lv)) input$tr_from else input$tr_to
    tryCatch(treatment_efficacy(tr_scored(), input$tr_treated, from = input$tr_from, to = to, test = input$test),
             error = function(e) validate(need(FALSE, conditionMessage(e))))
  })

  output$tr_intro <- renderUI({
    if (length(input$tr_treated) > 0) return(NULL)
    div(class = "step-intro mt-2",
      p("Is the treatment working? This step builds a ", tags$b("lung health score"), " from the parameters you choose and asks whether treated animals are closer to healthy controls than untreated diseased animals are."),
      tags$ol(
        tags$li("Each parameter is standardized against healthy controls at the same timepoint, so 0 means \"like a healthy animal\"."),
        tags$li("The direction in which disease moves each parameter is learned from the untreated diseased animals (for example Penh up, TVb down)."),
        tags$li("The score is the average of the signed values: 0 = healthy-like, higher = more disease-like."),
        tags$li("Treated animals are compared with untreated and healthy animals over the evaluation window. ", tags$b("Rescue"), " is the share of the disease effect the treatment removes (100% = back to healthy).")),
      p(class = "text-muted", "Treated animals are never used to build the score, and each untreated animal is scored with directions learned from the other untreated animals, so the comparison is not biased in favor of the treatment."))
  })

  output$tr_verdict <- renderUI({
    ef <- tr_eff()
    cls <- switch(ef$verdict$level, full = "alert-success", partial = "alert-info", none = "alert-secondary", worse = "alert-danger", "alert-warning")
    pm <- if (!is.null(ef$model)) ef$model$p[ef$model$term == "Treatment"] else NA
    sm <- ef$summary
    tagList(
      div(class = paste("alert py-2", cls), tags$b("Verdict: "), ef$verdict$text),
      layout_columns(
        fill = FALSE,
        value_box("Rescue", if (is.na(ef$rescue)) "\u2013" else sprintf("%.0f%%", ef$rescue),
                  p("of the disease effect removed (100% = like healthy)"), theme = "primary"),
        value_box("Treated vs untreated", format.pval(ef$comparisons$p[1], digits = 2, eps = 1e-4),
                  p(sprintf("%s, per-animal mean score", test_names[[ef$test]])), theme = "info"),
        value_box("Mixed model", if (length(pm) && !is.na(pm)) format.pval(pm, digits = 2, eps = 1e-4) else "\u2013",
                  p("treatment effect over time, all sessions"), theme = "secondary")
      ),
      p(class = "small text-muted", sprintf("Window: %s to %s. Animals: %s.", ef$window[1], ef$window[length(ef$window)],
                                            paste(sprintf("%s n = %d", sm$role, sm$n), collapse = ", ")))
    )
  })

  tr_groups_used <- reactive(c(input$tr_healthy, input$tr_disease, input$tr_treated))

  make_tr_tc <- function() {
    sc <- tr_scored()
    sc <- sc[as.character(sc$group) %in% tr_groups_used(), , drop = FALSE]
    sc$group <- droplevels(sc$group)
    attr(sc, "parameters") <- "lung_score"
    gs <- summarize_groups(sc, "lung_score")
    cols <- plethr_colors(names(groups_ok()), input$palette)[levels(sc$group)]
    lv <- levels(sc$timepoint)
    p <- plot_timecourse(gs, "lung_score", sc, cols, title = if (identical(input$tr_score_type, "ml")) "Machine learning disease score" else "Lung health score")
    if (length(input$tr_treated) && isTRUE(input$tr_from %in% lv) && isTRUE(input$tr_to %in% lv)) {
      p <- p + annotate("rect", xmin = match(input$tr_from, lv) - 0.4, xmax = match(input$tr_to, lv) + 0.4,
                        ymin = -Inf, ymax = Inf, alpha = 0.06, fill = "#1F5F8B")
    }
    p + geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      labs(y = tr_score_label(), caption = "Mean \u00b1 SEM; shaded = evaluation window; dashed line = healthy controls")
  }

  make_tr_cmp <- function() {
    ef <- tr_eff()
    a <- ef$animals
    vals <- data.frame(subject = a$subject, group = factor(a$role, levels = c("Healthy", "Untreated", "Treated")),
                       parameter = "lung_score", value = a$lung_score)
    vals <- vals[!is.na(vals$group), , drop = FALSE]
    vals$group <- droplevels(vals$group)
    cm <- ef$comparisons
    pairs <- list(c("Untreated", "Treated"), c("Healthy", "Treated"), c("Healthy", "Untreated"))
    cmp <- data.frame(parameter = "lung_score", group1 = vapply(pairs, `[`, "", 1), group2 = vapply(pairs, `[`, "", 2),
                      p_adj = stats::p.adjust(cm$p, method = "holm"))
    cmp$stars <- ifelse(is.na(cmp$p_adj), "", ifelse(cmp$p_adj < 0.001, "***", ifelse(cmp$p_adj < 0.01, "**", ifelse(cmp$p_adj < 0.05, "*", "ns"))))
    plot_group_comparison(vals, "lung_score", cmp, role_colors[levels(vals$group)], style = "bar", show_ns = TRUE,
                          y_label = "Lung health score (window mean)", title = "Lung health by treatment") +
      labs(caption = "Mean \u00b1 SEM; each point is one animal; Holm-adjusted p")
  }

  output$tr_tc_plot <- renderPlot(make_tr_tc(), res = 96)
  output$tr_cmp_plot <- renderPlot(make_tr_cmp(), res = 96)
  output$tr_par_plot <- renderPlot(plot_treatment_parameters(tr_eff()), res = 96)

  output$tr_cmp_table <- renderDT({
    ef <- tr_eff()
    d <- data.frame(Comparison = ef$comparisons$comparison, `Score difference` = ef$comparisons$difference,
                    p = ef$comparisons$p, check.names = FALSE)
    dtx(d, rownames = FALSE, class = "compact", caption = "Per-animal mean lung health score over the window",
              options = list(dom = "t", paging = FALSE)) %>% formatSignif(2:3, 3)
  })
  output$tr_model_table <- renderDT({
    ef <- tr_eff()
    validate(need(!is.null(ef$model), "The mixed model could not be fitted."))
    dtx(ef$model, rownames = FALSE, class = "compact", caption = "Mixed model, treated vs untreated: score ~ treatment \u00d7 time + (1 | animal)",
              options = list(dom = "t", paging = FALSE)) %>% formatSignif(2:3, 3)
  })
  output$tr_par_table <- renderDT({
    d <- tr_eff()$per_parameter
    names(d) <- c("Parameter", "Disease effect (SD)", "Used in score", "Rescue (%)", "p treated vs untreated")
    dtx(d, rownames = FALSE, class = "compact", options = list(dom = "t", paging = FALSE)) %>%
      formatSignif(c(2, 5), 3) %>% formatRound(4, 0)
  })
  output$tr_animals_table <- renderDT({
    a <- tr_eff()$animals
    a <- a[order(a$role, a$group, a$subject), c("role", "group", "subject", "lung_score")]
    names(a) <- c("Role", "Group", "Animal", "Lung health score")
    dtx(a, rownames = FALSE, class = "compact", options = list(dom = "t", paging = FALSE)) %>% formatRound(4, 2)
  })

  register_download("dl_tr_tc", function() "lung_health_timecourse", make_tr_tc, function() 9, function() 5.5)
  register_download("dl_tr_cmp", function() "lung_health_by_treatment", make_tr_cmp, function() 5.5, function() 5.5)
  register_download("dl_tr_par", function() "treatment_by_parameter", function() plot_treatment_parameters(tr_eff()), function() 7, function() 6)

  tr_valid <- function() !inherits(tryCatch(tr_eff(), error = function(e) e), "error")

  # The Bacterial burden view appears only when the design file has CFU values.
  observe({
    has_cfu <- !is.null(rv$design) && nrow(rv$design$cfu) > 0
    if (has_cfu) nav_show("results_tabs", "cfu") else nav_hide("results_tabs", "cfu")
  })

  output$ml_ready <- reactive(!is.null(ml_res()))
  outputOptions(output, "ml_ready", suspendWhenHidden = FALSE)

  # -- Projects: save, autosave, open ------------------------------------------------
  # Save writes the project straight to a folder on this computer (default Documents/plethR_projects),
  # asking for a name the first time. Ctrl+S saves too. Unsaved work is autosaved every 3 minutes
  # to <name>_autosave.rds, and the browser warns before closing a page with unsaved changes.
  proj_state <- reactiveValues(dir = file.path(path.expand("~"), "plethR_projects"), name = NULL, path = NULL,
                               saved_at = NULL, saved_snapshot = NULL, saved_changes = 0, changes = 0,
                               recent_version = 0, settle = 0)
  restore_state <- reactiveValues(inputs = NULL, passes = 0, tries = list())

  restorable <- function(id) {
    !grepl("^(excel_file|design_file|ml_new_file|ml_sev_file|ml_load|project_file|steps|results_tabs|grp_|proj_|save_|open_)", id) &&
      !grepl("_(rows_|cell_|search|state|columns_|rows_all|rows_current|rows_selected)", id) &&
      !id %in% c("to_results", "ml_run", "tr_train", "lib_open", "lib_add", "lib_remove", "lib_train",
                 "lib_save_model", "lib_use_model", "group_names")
  }
  project_inputs <- function() {
    inp <- reactiveValuesToList(input)
    inp <- inp[vapply(names(inp), restorable, logical(1))]
    inp <- inp[!vapply(inp, function(v) is.null(v) || is.list(v), logical(1))]
    inp[order(names(inp))]
  }
  default_name <- function() proj_state$name %||% safe_name(tools::file_path_sans_ext(rv$file_name %||% "plethR_project"))
  project_bundle <- function() {
    list(plethr_project = TRUE, version = app_version, saved = format(Sys.time(), "%Y-%m-%d %H:%M"),
         project_name = default_name(), file_name = rv$file_name, raw = rv$raw, design = rv$design,
         groups = tryCatch(assignment(), error = function(e) list()), group_names = input$group_names,
         inputs = project_inputs(), ml = ml_res(), tr_ml = tr_ml())
  }
  # Trained models count as changes too.
  observeEvent(ml_res(), proj_state$changes <- isolate(proj_state$changes) + 1, ignoreInit = TRUE)
  observeEvent(tr_ml(), proj_state$changes <- isolate(proj_state$changes) + 1, ignoreInit = TRUE)

  current_snapshot <- function() list(load_id = rv$load_id, groups = input$group_names, inputs = project_inputs())
  mark_saved <- function() {
    proj_state$saved_snapshot <- isolate(current_snapshot())
    proj_state$saved_changes <- isolate(proj_state$changes)
  }
  dirty <- reactive({
    if (is.null(rv$raw)) return(FALSE)
    is.null(proj_state$saved_snapshot) || proj_state$changes != proj_state$saved_changes ||
      !identical(current_snapshot(), proj_state$saved_snapshot)
  })
  dirty_t <- throttle(dirty, 1500)
  observe(session$sendCustomMessage("plethr-dirty", isTRUE(dirty_t()) && restore_state$passes <= 0))

  write_project <- function(path) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(isolate(project_bundle()), path)
  }
  do_save <- function(path) {
    ok <- tryCatch({ write_project(path); TRUE },
                   error = function(e) { showNotification(paste("Could not save:", conditionMessage(e)), type = "error", duration = NULL); FALSE })
    if (!ok) return(invisible(FALSE))
    proj_state$path <- path
    proj_state$dir <- dirname(path)
    proj_state$name <- tools::file_path_sans_ext(basename(path))
    proj_state$saved_at <- Sys.time()
    mark_saved()
    unlink(file.path(dirname(path), paste0(proj_state$name, "_autosave.rds")))
    proj_state$recent_version <- isolate(proj_state$recent_version) + 1
    showNotification(paste("Saved", basename(path), "in", dirname(path)), type = "message", duration = 4)
    invisible(TRUE)
  }
  show_save_as <- function() {
    showModal(modalDialog(
      title = "Save project",
      textInput("proj_name_in", "Project name", value = default_name(), width = "100%"),
      textInput("proj_dir_in", "Folder", value = proj_state$dir, width = "100%"),
      checkboxInput("proj_replace_in", "Replace a project with the same name", value = FALSE),
      p(class = "small text-muted", "Saved as <name>.rds: the data, groups, design file, every setting and any trained models. ",
        "After this, Save (or Ctrl+S) updates the same file."),
      footer = tagList(modalButton("Cancel"), actionButton("save_as_go", "Save", class = "btn-primary")),
      easyClose = TRUE))
  }
  save_now <- function() {
    if (is.null(rv$raw)) { showNotification("Load a data file first.", type = "warning"); return() }
    if (is.null(proj_state$path)) show_save_as() else do_save(proj_state$path)
  }
  observeEvent(input$save_project, save_now())
  observeEvent(input$save_shortcut, save_now())
  observeEvent(input$save_as, {
    if (is.null(rv$raw)) { showNotification("Load a data file first.", type = "warning"); return() }
    show_save_as()
  })
  observeEvent(input$save_as_go, {
    name <- safe_name(trimws(input$proj_name_in %||% ""))
    dir <- path.expand(trimws(input$proj_dir_in %||% ""))
    if (!nzchar(name) || !nzchar(dir)) { showNotification("Give the project a name and a folder.", type = "warning"); return() }
    path <- file.path(dir, paste0(name, ".rds"))
    if (file.exists(path) && !identical(normalizePath(path, mustWork = FALSE), normalizePath(proj_state$path %||% "", mustWork = FALSE)) &&
        !isTRUE(input$proj_replace_in)) {
      showNotification(paste0("A project called '", name, "' already exists in that folder. Choose another name or tick Replace."), type = "warning", duration = 8)
      return()
    }
    removeModal()
    do_save(path)
  })
  # Autosave unsaved work every 3 minutes (never overwrites the saved project).
  observe({
    invalidateLater(3 * 60 * 1000)
    isolate({
      if (is.null(rv$raw) || !isTRUE(dirty()) || restore_state$passes > 0) return()
      path <- file.path(proj_state$dir, paste0(default_name(), "_autosave.rds"))
      ok <- tryCatch({ write_project(path); TRUE }, error = function(e) FALSE)
      if (ok) proj_state$recent_version <- proj_state$recent_version + 1
    })
  })
  output$dl_project <- downloadHandler(
    filename = function() paste0(default_name(), ".rds"),
    content = function(file) {
      validate(need(!is.null(rv$raw), "Load data first."))
      saveRDS(project_bundle(), file)
    }
  )

  output$save_status <- renderUI({
    if (is.null(rv$raw)) return(NULL)
    if (restore_state$passes > 0) return(span(class = "save-status text-muted", "Opening\u2026"))
    if (is.null(proj_state$saved_at) && is.null(proj_state$path)) return(span(class = "save-status unsaved", "Not saved yet"))
    if (isTRUE(dirty_t())) return(span(class = "save-status unsaved", "Unsaved changes"))
    span(class = "save-status saved", if (!is.null(proj_state$saved_at)) paste("Saved", format(proj_state$saved_at, "%H:%M")) else "Saved")
  })
  output$save_info <- renderUI({
    p(class = "small text-muted mb-0 px-1",
      if (!is.null(proj_state$path)) tagList("Saving to ", tags$code(proj_state$path)) else tagList("Projects folder: ", tags$code(proj_state$dir)),
      br(), "Ctrl+S saves. Unsaved work is autosaved every 3 minutes.")
  })

  # Recent projects in the projects folder (newest first).
  recent_files <- reactive({
    proj_state$recent_version
    invalidateLater(60000)
    d <- proj_state$dir
    if (!dir.exists(d)) return(data.frame(path = character(0), mtime = as.POSIXct(character(0))))
    f <- list.files(d, pattern = "\\.rds$", full.names = TRUE)
    if (!length(f)) return(data.frame(path = character(0), mtime = as.POSIXct(character(0))))
    i <- file.info(f)
    out <- data.frame(path = normalizePath(f, winslash = "/"), mtime = i$mtime, stringsAsFactors = FALSE)
    utils::head(out[order(out$mtime, decreasing = TRUE), , drop = FALSE], 8)
  })
  recent_links <- function(item_class) {
    r <- recent_files()
    if (!nrow(r)) return(p(class = "small text-muted mb-0 px-1", "No saved projects yet."))
    lapply(seq_len(nrow(r)), function(i) {
      nm <- tools::file_path_sans_ext(basename(r$path[i]))
      tags$a(class = item_class, href = "#",
             onclick = sprintf("Shiny.setInputValue('open_recent', %s, {priority: 'event'}); return false;",
                               jsonlite::toJSON(r$path[i], auto_unbox = TRUE)),
             span(nm), span(class = "small text-muted ms-2", format(r$mtime[i], "%d %b %H:%M")),
             if (grepl("_autosave$", nm)) span(class = "badge text-bg-light ms-1", "autosave"))
    })
  }
  output$recent_projects <- renderUI(tagList(recent_links("dropdown-item d-flex justify-content-between")))
  output$recent_projects_setup <- renderUI({
    r <- recent_files()
    if (!nrow(r)) return(NULL)
    div(class = "recent-list", p(class = "small text-muted mb-1", "Recent projects"),
        div(class = "list-group", recent_links("list-group-item list-group-item-action d-flex justify-content-between")))
  })

  open_project <- function(p, path = NULL, fallback_name = NULL) {
    if (!isTRUE(p$plethr_project)) {
      showNotification("That file is not a plethR project.", type = "error")
      return(invisible(FALSE))
    }
    rv$load_id <- rv$load_id + 1
    rv$raw <- p$raw
    rv$file_name <- p$file_name
    rv$design <- p$design
    rv$saved_assign <- p$groups
    updateTextAreaInput(session, "group_names", value = p$group_names %||% paste(names(p$groups), collapse = "\n"))
    if (!is.null(p$ml)) ml_res(p$ml)
    if (!is.null(p$tr_ml)) tr_ml(p$tr_ml)
    restore_state$inputs <- p$inputs
    restore_state$tries <- list()
    restore_state$passes <- 600
    nm <- p$project_name %||% safe_name(fallback_name %||% tools::file_path_sans_ext(p$file_name %||% "plethR_project"))
    proj_state$name <- sub("_autosave$", "", nm)
    is_autosave <- !is.null(path) && grepl("_autosave\\.rds$", path)
    proj_state$path <- if (!is.null(path) && !is_autosave) path else NULL
    if (!is.null(path)) proj_state$dir <- dirname(path)
    proj_state$saved_at <- if (!is.null(path) && !is_autosave) file.info(path)$mtime else NULL
    proj_state$saved_snapshot <- NULL
    nav_select("steps", if (length(p$groups) >= 1) "results" else "setup")
    showNotification(sprintf("Opened %s (saved %s with plethR %s).", proj_state$name, p$saved, p$version), type = "message")
    invisible(TRUE)
  }
  observeEvent(input$project_file, {
    p <- tryCatch(readRDS(input$project_file$datapath), error = function(e) NULL)
    open_project(p, NULL, tools::file_path_sans_ext(input$project_file$name))
  })
  observeEvent(input$open_recent, {
    path <- input$open_recent
    p <- tryCatch(readRDS(path), error = function(e) NULL)
    if (is.null(p)) { showNotification("Could not read that project file.", type = "error"); return() }
    open_project(p, path)
  })

  # Settings are re-sent until each one matches the saved value: some controls only exist after the
  # data are processed. Once everything matches (and a few seconds have passed for the app to settle),
  # the opened project counts as saved.
  observe({
    invalidateLater(1000)
    isolate({
      if (proj_state$settle > 0) {
        proj_state$settle <- proj_state$settle - 1
        if (proj_state$settle == 0) mark_saved()
      }
      if (restore_state$passes <= 0 || !length(restore_state$inputs)) return()
      same <- function(a, b) identical(as.character(unlist(a)), as.character(unlist(b)))
      pending <- restore_state$inputs
      pending <- pending[!vapply(names(pending), function(id) same(input[[id]], pending[[id]]), logical(1))]
      # A control that exists but still differs after 3 tries cannot take the saved value; leave it to the user.
      tries <- restore_state$tries %||% list()
      for (id in names(pending)) if (!is.null(input[[id]])) tries[[id]] <- (tries[[id]] %||% 0) + 1
      pending <- pending[vapply(names(pending), function(id) (tries[[id]] %||% 0) <= 3, logical(1))]
      for (id in names(pending)) session$sendInputMessage(id, list(value = pending[[id]]))
      restore_state$tries <- tries
      restore_state$inputs <- pending
      restore_state$passes <- if (length(pending)) restore_state$passes - 1 else 0
      if (restore_state$passes == 0) proj_state$settle <- 3
    })
  })

  # -- R script -------------------------------------------------------------------------
  r_script <- function() {
    dq <- function(x) paste0("\"", gsub("\"", "\\\\\"", x), "\"")
    vec <- function(x) paste0("c(", paste(vapply(x, dq, ""), collapse = ", "), ")")
    st <- attr(sessions_base(), "settings")
    g <- groups_ok()
    sel <- input$params_multi
    w <- window()
    lines <- c(
      paste0("# plethR analysis script, generated ", format(Sys.time(), "%Y-%m-%d %H:%M"), " with plethR ", app_version),
      "# Reproduces the analysis set up in the app. Edit the file paths below if the files have moved.",
      "library(plethR)", "",
      paste0("data_file <- ", dq(rv$file_name %||% "data.xlsx")),
      "wbp <- read_wbp(data_file)", "",
      "groups <- list(",
      paste0("  ", vapply(names(g), function(n) paste0(dq(n), " = ", vec(g[[n]])), ""), c(rep(",", length(g) - 1), "")),
      ")",
      paste0("sessions <- summarize_sessions(assign_groups(wbp, groups), timepoint = ", dq(st$timepoint), ", stat = ", dq(st$stat),
             if (!is.null(st$rinx_max)) paste0(", rinx_max = ", st$rinx_max) else "", ")"),
      if (!is.null(st$variability)) c(paste0("variability <- session_variability(wbp, ", vec(st$variability$parameters), ", ", vec(st$variability$features),
                                             ", timepoint = ", dq(st$timepoint), ")"), "sessions <- add_variability(sessions, variability)"),
      if (!is.null(rv$design)) c(paste0("design <- read_design(", dq(input$design_file$name %||% "study_design.xlsx"), ")"),
                                 if (isTRUE(input$use_weight)) "sessions <- add_body_weight(sessions, design$weights)"),
      if (transform() != "none") paste0("sessions <- apply_baseline(sessions, ", dq(transform()), ", ", dq(input$baseline_tp), ")"),
      paste0("keep <- ", vec(input$tp_include)),
      "params <- attr(sessions, \"parameters\")",
      "sessions <- sessions[sessions$timepoint %in% keep, ]",
      "sessions$timepoint <- droplevels(sessions$timepoint)",
      "attr(sessions, \"parameters\") <- params", "",
      paste0("parameters <- ", vec(sel)),
      paste0("reference <- ", dq(reference())),
      "group_means <- summarize_groups(sessions, parameters)",
      paste0("values <- summarize_subjects(sessions, parameters, metric = ", dq(input$metric), ", from = ", dq(w$from), ", to = ", dq(w$to), ")"),
      paste0("comparisons <- compare_groups(values, reference = ", if (input$comp_mode == "reference") "reference" else "NULL",
             ", test = ", dq(input$test), ", p_adjust = ", dq(input$padj), ")"),
      paste0("timepoint_tests <- compare_timepoints(sessions, parameters, reference = reference, test = ", dq(input$test), ", p_adjust = ", dq(input$padj), ")"),
      paste0("colors <- plethr_colors(names(groups), ", dq(input$palette), ")"), "",
      "# Figures for one parameter (change \"p\" to plot another)",
      paste0("p <- ", dq(input$param)),
      "plot_timecourse(group_means, p, sessions, colors, stats = timepoint_tests)",
      paste0("plot_group_comparison(values, p, comparisons, colors, style = ", dq(input$cmp_style %||% "bar"), ")"),
      if (identical(input$tc_method, "mixed")) c(
        paste0("mt <- mixed_timecourse(sessions, p, reference, covariates = ", vec(input$tc_cov %||% character(0)),
               if ("sex" %in% input$tc_cov) ", sex = setNames(design$animals$sex, design$animals$animal)" else "",
               ", log_transform = ", isTRUE(input$tc_mm_log), ", p_adjust = ", dq(input$padj), ")"),
        "plot_mixed_timecourse(mt, colors)"),
      "plot_effect_forest(comparisons, parameters, colors)",
      "plot_dashboard(group_means, parameters, colors)", "",
      "# Tables",
      "writexl::write_xlsx(list(Session_values = sessions, Group_means = group_means, Animal_summary = values,",
      "                         Group_comparisons = comparisons, Timepoint_tests = timepoint_tests), \"plethR_results.xlsx\")")
    paste(unlist(lines), collapse = "\n")
  }
  output$dl_rscript <- downloadHandler(
    filename = function() paste0(safe_name(tools::file_path_sans_ext(rv$file_name %||% "plethR")), "_analysis.R"),
    content = function(file) writeLines(r_script(), file)
  )

  # -- Study library ---------------------------------------------------------------------
  rv_lib <- reactiveValues(dir = NULL, version = 0, fit = NULL)
  observeEvent(input$lib_open, {
    dir <- path.expand(trimws(input$lib_dir))
    ok <- tryCatch({ library_init(dir); TRUE }, error = function(e) { showNotification(conditionMessage(e), type = "error"); FALSE })
    if (ok) { rv_lib$dir <- dir; rv_lib$version <- rv_lib$version + 1 }
  })
  lib_index <- reactive({ rv_lib$version; req(rv_lib$dir); library_list(rv_lib$dir) })
  lib_models_r <- reactive({ rv_lib$version; req(rv_lib$dir); library_models(rv_lib$dir) })
  lib_need <- function() validate(need(!is.null(rv_lib$dir), "Open or create a library first (left)."))
  output$lib_info <- renderUI({
    if (is.null(rv_lib$dir)) return(p(class = "small text-muted mt-2", "No library open."))
    p(class = "small mt-2", tags$b("Open: "), rv_lib$dir, br(), sprintf("%d studies, %d saved models", nrow(lib_index()), nrow(lib_models_r())))
  })

  lib_state <- reactiveValues(groups = NULL)
  observe({
    lv <- levels(sessions_base()$timepoint)
    d <- rv$design
    inf <- d$study$infection_session %||% ""
    updateSelectInput(session, "lib_inf_tp", choices = lv, selected = if (inf %in% lv) inf else isolate(input$ml_inf_tp) %||% lv[min(2, length(lv))])
    off <- suppressWarnings(as.numeric(d$study$days_from_infection_to_that_session %||% NA))
    if (!is.na(off)) updateNumericInput(session, "lib_offset", value = off)
    id <- d$study$study_id %||% ""
    if (!nzchar(id)) id <- tools::file_path_sans_ext(rv$file_name %||% "")
    if (!nzchar(isolate(input$lib_study_id) %||% "")) updateTextInput(session, "lib_study_id", value = safe_name(id))
  })
  output$lib_add_intro <- renderUI({
    if (is.null(rv$raw)) return(p(class = "text-muted mt-2", "Load and process a study first (steps 1 to 3)."))
    p(class = "choice-help mt-2", sprintf("Saves the session values of %s (%d animals, %d parameters, the timepoints and processing chosen in Setup) to the library%s.",
                                         rv$file_name, length(unique(sessions()$subject)), length(params()),
                                         if (!is.null(rv$design)) ", with the study design file" else ""))
  })
  output$lib_conditions_ui <- renderUI({
    g <- names(groups_ok())
    guess <- function(x) if (grepl("uninfect|naive|healthy|sham|control", x, ignore.case = TRUE)) "Uninfected"
                         else if (grepl("treat|drug|therap|antibiot", x, ignore.case = TRUE) && !grepl("untreat", x, ignore.case = TRUE)) "Treated"
                         else if (grepl("infect|expos|disease", x, ignore.case = TRUE)) "Infected" else "Uninfected"
    lapply(g, function(x) selectInput(paste0("lib_cond_", safe_name(x)), x, choices = c("Infected", "Uninfected", "Treated", "Exclude"), selected = guess(x)))
  })
  observeEvent(input$lib_add, {
    res <- tryCatch({
      lib_need()
      g <- names(groups_ok())
      cond <- stats::setNames(vapply(g, function(x) input[[paste0("lib_cond_", safe_name(x))]] %||% "Uninfected", ""), g)
      s <- ex_assigned()
      library_add_study(rv_lib$dir, s, input$lib_study_id, cond, infection_timepoint = input$lib_inf_tp, offset = input$lib_offset %||% 0,
                        design = rv$design, source_file = rv$file_name, notes = input$lib_notes, overwrite = isTRUE(input$lib_overwrite))
      TRUE
    }, error = function(e) { showNotification(conditionMessage(e), type = "error", duration = NULL); FALSE })
    if (isTRUE(res)) {
      rv_lib$version <- rv_lib$version + 1
      showNotification(paste("Added", input$lib_study_id, "to the library."), type = "message")
    }
  })
  output$lib_table <- renderDT({
    lib_need()
    dtx(lib_index(), rownames = FALSE, selection = "single", class = "compact", options = list(dom = "t", paging = FALSE, scrollX = TRUE))
  })
  observeEvent(input$lib_remove, {
    i <- input$lib_table_rows_selected
    if (!length(i)) { showNotification("Select a study in the table first.", type = "warning"); return() }
    id <- lib_index()$study_id[i]
    library_remove(rv_lib$dir, id)
    rv_lib$version <- rv_lib$version + 1
    showNotification(paste("Removed", id), type = "message")
  })
  observe({
    idx <- lib_index()
    updateSelectizeInput(session, "lib_studies", choices = idx$study_id, selected = idx$study_id)
  })
  observe({
    req(length(input$lib_studies) > 0)
    common <- tryCatch(attr(library_load(rv_lib$dir, input$lib_studies), "parameters"), error = function(e) character(0))
    core <- intersect(c("f", "TVb", "MVb", "Penh", "PIFb", "PEFb", "EF50", "Ti", "Te", "Rpef", "EIP", "EEP"), common)
    updateSelectizeInput(session, "lib_params", choices = common, selected = if (length(core)) core else common)
  })
  observeEvent(input$lib_train, {
    res <- tryCatch({
      lib_need()
      ml_check_packages()
      d <- ml_prepare_library(library_load(rv_lib$dir, input$lib_studies), input$lib_task, input$lib_params, acute_days = input$lib_acute %||% 14)
      withProgress(message = "Training across studies", value = 0, {
        ml_fit(d, models = input$lib_models, validation = input$lib_validation, tuning = input$lib_tuning, seed = 10,
               progress = function(i, n, name) setProgress((i - 1) / n, detail = sprintf("%s (%d of %d)", name, i, n)))
      })
    }, error = function(e) { showNotification(conditionMessage(e), type = "error", duration = NULL); NULL })
    if (!is.null(res)) {
      rv_lib$fit <- res
      updateTextInput(session, "lib_model_name", value = paste0(input$lib_task, "_", length(input$lib_studies), "studies"))
    }
  })
  lib_fit_need <- function() validate(need(!is.null(rv_lib$fit), "Choose studies and click Train."))
  output$lib_train_summary <- renderUI({
    lib_fit_need()
    f <- rv_lib$fit
    m <- f$metrics[f$metrics$level == "animal", ]
    b <- m[which.max(m$roc_auc), ]
    div(class = paste("alert py-2", if (b$roc_auc >= 0.75) "alert-success" else if (b$roc_auc >= 0.65) "alert-info" else "alert-secondary"),
        sprintf("%s trained on %d studies (%d animals); best animal-level ROC AUC %.2f (%s) %s.",
                ml_outcome_label(f), length(unique(f$data$study)), length(unique(f$data$subject)), b$roc_auc, b$model_name,
                if (f$settings$validation == "study") "on studies the models had not seen" else "on held-out animals"))
  })
  output$lib_perf_plot <- renderPlot({ lib_fit_need(); plot_ml_performance(rv_lib$fit) }, res = 96)
  output$lib_study_table <- renderDT({
    lib_fit_need()
    d <- ml_study_metrics(rv_lib$fit)
    d$model <- unname(ml_model_names("all")[d$model])
    dtx(d, rownames = FALSE, class = "compact", options = list(dom = "t", paging = FALSE)) %>%
      formatRound(c("session_roc_auc", "animal_roc_auc", "animal_accuracy"), 2)
  })
  observeEvent(input$lib_save_model, {
    if (is.null(rv_lib$fit)) { showNotification("Train a model first.", type = "warning"); return() }
    id <- library_save_model(rv_lib$dir, rv_lib$fit, input$lib_model_name)
    rv_lib$version <- rv_lib$version + 1
    showNotification(paste("Saved model", id), type = "message")
  })
  output$lib_history_plot <- renderPlot({
    lib_need()
    validate(need(nrow(lib_models_r()) > 0, "No saved models yet."))
    plot_model_history(lib_models_r())
  }, res = 96)
  output$lib_models_table <- renderDT({
    lib_need()
    dtx(lib_models_r(), rownames = FALSE, selection = "single", class = "compact", options = list(dom = "t", paging = FALSE, scrollX = TRUE)) %>%
      formatRound(c("session_score", "animal_score"), 2)
  })
  lib_selected_model <- function() {
    i <- input$lib_models_table_rows_selected
    validate(need(length(i) == 1, "Select a model in the table first."))
    lib_models_r()$model_id[i]
  }
  observeEvent(input$lib_use_model, {
    id <- tryCatch(lib_selected_model(), error = function(e) NULL)
    if (is.null(id)) { showNotification("Select a model in the table first.", type = "warning"); return() }
    obj <- library_load_model(rv_lib$dir, id)
    first <- readRDS(file.path(rv_lib$dir, "studies", paste0(obj$settings$studies[1], ".rds")))
    s <- first$info$settings
    obj$processing <- list(timepoint = s$timepoint %||% "phase", stat = s$stat %||% "median", rinx_max = s$rinx_max, baseline = "none",
                           source = paste("library:", paste(obj$settings$studies, collapse = ", ")))
    ml_res(obj)
    updateRadioButtons(session, "ml_scope", selected = "study")
    showNotification("Model loaded. Apply it to a new file under \"Save models and predict new data\" below.", type = "message", duration = 8)
  })
  output$lib_dl_model <- downloadHandler(
    filename = function() paste0(lib_selected_model(), ".rds"),
    content = function(file) file.copy(file.path(rv_lib$dir, "models", paste0(lib_selected_model(), ".rds")), file)
  )

  # Controls built on the server keep rendering on unopened tabs, so their settings exist
  # (and can be restored from a project) before the user visits that tab.
  for (id in c("weight_ui", "window_ui", "tc_cov_ui", "ml_covariate_ui", "ml_sev_cols_ui", "ml_sev_source_ui", "lib_conditions_ui"))
    outputOptions(output, id, suspendWhenHidden = FALSE)

  # -- 7. Export ---------------------------------------------------------------------
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
    figs[["dashboard"]] <- list(plot = tryCatch(plot_dashboard(group_sum(), sel, colors(), input$tc_error %||% "sem"), error = function(e) NULL),
                                w = 12, h = 2.7 * ceiling(max(1, length(sel)) / 4) + 1.2)
    if (length(sel) >= 2) figs[["correlations"]] <- list(plot = tryCatch(plot_correlation(sessions(), sel), error = function(e) NULL), w = 8.5, h = 7.5)
    if (has_comparisons()) {
      figs[["effect_sizes"]] <- list(plot = tryCatch(plot_effect_forest(comps(), sel, colors()), error = function(e) NULL),
                                     w = 8.5, h = 0.32 * length(sel) * max(1, n_groups() - 1) / 1.5 + 2.5)
      if (length(sel) >= 3) figs[["group_profiles"]] <- list(plot = tryCatch(plot_group_profile(subj_vals(), reference(), sel, colors()), error = function(e) NULL), w = 8.5, h = 7.5)
    }
    for (p in sel) {
      figs[[paste0("animals_heatmap_", safe_name(p))]] <- list(plot = tryCatch(plot_animal_heatmap(sessions(), p, "percent"), error = function(e) NULL),
                                                               w = 10, h = 0.24 * length(unique(sessions()$subject)) + 2.2)
    }
    obj <- ml_res()
    if (!is.null(obj)) {
      ses <- obj$metrics[obj$metrics$level == "session", ]
      best <- ml_best(obj)
      figs[["ml_performance"]] <- list(plot = plot_ml_performance(obj), w = 8, h = 5)
      figs[["ml_detail"]] <- list(plot = if (ml_type(obj) == "regression") plot_ml_observed(obj, best, "animal", ml_time_colors()) else plot_ml_roc(obj, "session"), w = 8, h = 6)
      figs[["ml_over_time"]] <- list(plot = plot_ml_over_time(obj, best, ml_time_colors()), w = 9, h = 5.5)
      if (best %in% obj$importance$model) figs[["ml_importance"]] <- list(plot = plot_ml_importance(obj, best), w = 7, h = 5.5)
    }
    if (tr_valid()) {
      figs[["lung_health_timecourse"]] <- list(plot = make_tr_tc(), w = 9, h = 5.5)
      figs[["lung_health_by_treatment"]] <- list(plot = make_tr_cmp(), w = 5.5, h = 5.5)
      figs[["treatment_by_parameter"]] <- list(plot = tryCatch(plot_treatment_parameters(tr_eff()), error = function(e) NULL), w = 7, h = 6)
      figs[["health_score_by_animal"]] <- list(plot = tryCatch(make_tr_trend(), error = function(e) NULL), w = 12, h = tryCatch(tr_trend_height() / 96, error = function(e) 8))
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


  output$dl_everything <- downloadHandler(
    filename = function() paste0(safe_name(tools::file_path_sans_ext(rv$file_name %||% "plethR")), "_plethR_", format(Sys.Date(), "%Y%m%d"), ".zip"),
    content = function(file) {
      dir <- file.path(tempdir(), paste0("plethR_all_", as.integer(Sys.time())))
      dir.create(file.path(dir, "figures"), recursive = TRUE)
      fs <- fig_settings()
      withProgress(message = "Preparing everything", value = 0, {
        setProgress(0.05, detail = "workbook")
        write_workbook(file.path(dir, "results.xlsx"))
        figs <- all_figures()
        for (i in seq_along(figs)) {
          n <- names(figs)[i]
          setProgress(0.1 + 0.75 * i / length(figs), detail = n)
          tryCatch(save_plot(file.path(dir, "figures", paste0(n, ".", fig_ext(fs))), figs[[n]]$plot, figs[[n]]$w, figs[[n]]$h, "img", fs),
                   error = function(e) NULL)
        }
        setProgress(0.9, detail = "PDF")
        dev <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf
        dev(file.path(dir, "all_figures.pdf"), width = 9, height = 6.5, onefile = TRUE)
        for (fg in figs) tryCatch(print(fg$plot), error = function(e) NULL)
        grDevices::dev.off()
        writeLines(methods_text(), file.path(dir, "methods.txt"))
        utils::write.csv(settings_table(), file.path(dir, "settings.csv"), row.names = FALSE)
        writeLines(c(
          paste0("plethR ", app_version, " results for ", rv$file_name %||% "", " (", format(Sys.time(), "%Y-%m-%d %H:%M"), ")"), "",
          "results.xlsx     Every table: Methods, Settings, Groups, Session_values (one row per animal and session),",
          "                 Group_means, Animal_summary (summary metric per animal), Group_comparisons and",
          "                 Timepoint_tests (p and adjusted p), Data_check, and when run: ML_*, Treatment_*, Factorial_*.",
          paste0("figures/         Every figure as ", toupper(fs$format), if (fs$format %in% c("png", "tiff")) paste0(" at ", fs$dpi, " dpi") else "", "."),
          "all_figures.pdf  The same figures as one vector PDF (editable in Illustrator or Inkscape).",
          "methods.txt      A methods paragraph describing the analysis with these settings. Check before use.",
          "settings.csv     Every setting used.", "",
          "Statistics treat the animal as the experimental unit; n = animals."), file.path(dir, "README.txt"))
      })
      zip::zip(file, files = list.files(dir, recursive = TRUE), root = dir)
    }
  )

  output$dl_png_zip <- downloadHandler(
    filename = function() paste0(safe_name(tools::file_path_sans_ext(rv$file_name %||% "plethR")), "_figures.zip"),
    content = function(file) {
      dir <- file.path(tempdir(), paste0("plethR_png_", as.integer(Sys.time())))
      dir.create(dir)
      withProgress(message = "Saving figures", value = 0, {
        figs <- all_figures()
        for (n in names(figs)) {
          incProgress(1 / length(figs))
          save_plot(file.path(dir, paste0(n, ".", fig_ext(fig_settings()))), figs[[n]]$plot, figs[[n]]$w, figs[[n]]$h, "img", fig_settings())
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
      if (!is.null(rv$design) && any(rv$design$animals$exclude)) {
        ex <- rv$design$animals[rv$design$animals$exclude, ]
        sprintf("%d animal%s %s excluded (%s). ", nrow(ex), if (nrow(ex) > 1) "s" else "", if (nrow(ex) > 1) "were" else "was",
                paste(sprintf("%s: %s", ex$animal, ifelse(is.na(ex$exclusion_reason), "reason not given", ex$exclusion_reason)), collapse = "; "))
      } else "",
      if (isTRUE(input$use_weight) && !is.null(rv$design) && nrow(rv$design$weights)) paste0(
        "Body weight was recorded at the WBP sessions (interpolated linearly within each animal between weigh-ins); tidal and minute volume ",
        "were also expressed per gram of body weight, and weight change as a percentage of each animal's first weight. ") else "",
      if (!is.null(st$variability)) sprintf(paste0("Within-session breathing variability of %s was described by %s, computed from records resampled to a regular ",
                                                  "2-second grid (fluctuation power from a tapered periodogram; bands 2-10 min, 30 s-2 min and 4-30 s). "),
                                           paste(st$variability$parameters, collapse = ", "),
                                           paste(variability_feature_types()[st$variability$features], collapse = ", ")) else "",
      switch(transform(),
        none = "",
        percent = sprintf("Values were expressed as a percentage of each animal's baseline (%s). ", input$baseline_tp),
        difference = sprintf("Values were expressed as the change from each animal's baseline (%s). ", input$baseline_tp)),
      sprintf("Data are presented as mean \u00b1 SEM with the animal as the experimental unit (n = %s animals per group; %s). ",
              if (min(n) == max(n)) min(n) else paste0(min(n), "\u2013", max(n)),
              paste(sprintf("%s, n = %d", names(n), as.integer(n)), collapse = "; ")),
      if (input$metric == "auc") "For each animal, the area under the curve was calculated by the trapezoidal rule over study days " else "For each animal, the ",
      if (input$metric == "auc") paste0("(", window()$from, " to ", window()$to, "). ") else paste0(metric_text(), " was calculated. "),
      if (has_comparisons()) paste0(
        if (input$comp_mode == "reference") paste0("Each group was compared with the ", reference(), " group") else "All pairs of groups were compared",
        " using ", test_names[[input$test]], "s",
        if (n_groups() >= 3) paste0(", with ", omnibus_names[[input$test]], " as an omnibus test") else "",
        if (input$padj == "none") "; p-values were not adjusted for multiple comparisons. "
        else paste0("; p-values were adjusted for multiple comparisons with the ", padj_names[[input$padj]], " method within each parameter. "),
        if (identical(input$tc_method, "mixed")) paste0("Differences at individual timepoints were estimated with a linear mixed model (group \u00d7 time",
            if (length(input$tc_cov)) paste0(", adjusted for ", paste(c(baseline = "baseline value", sex = "sex", weight = "body weight")[input$tc_cov], collapse = ", ")) else "",
            if (isTRUE(input$tc_mm_log)) ", log-transformed values" else "", ", random intercept per animal; R packages nlme and emmeans) against the ")
        else paste0("Differences at individual timepoints were tested with ", test_names[[input$test]], "s against the "),
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
      if (tr_valid()) {
        st <- attr(tr_scored(), "lung_settings"); ef <- tr_eff()
        if (identical(st$score_type, "ml")) {
          a <- attr(tr_scored(), "ml_disease")$metrics
          a <- a[a$level == "animal", ]
          sprintf(paste0(" Treatment efficacy was assessed with a machine learning disease score: a %s model was trained to distinguish untreated (%s) from healthy (%s) animals ",
                         "using %d parameters from %s onward (tuned by ROC AUC; animal-held-out ROC AUC %.2f). Each healthy and untreated animal was scored by a model trained without it, ",
                         "treated animals by the final model, and the log-odds of disease were centered on healthy animals at each timepoint. ",
                         "Per-animal mean scores from %s to %s were compared between treated (%s), untreated and healthy groups with %ss, and treated and untreated animals ",
                         "were compared over time with a linear mixed model (treatment \u00d7 time, random intercept per animal)."),
                  tolower(ml_model_names("all")[[st$ml_model]]), paste(st$disease, collapse = ", "), paste(st$healthy, collapse = ", "),
                  length(st$parameters), st$onset_timepoint, a$roc_auc, ef$window[1], ef$window[length(ef$window)],
                  paste(input$tr_treated, collapse = ", "), test_names[[ef$test]])
        } else sprintf(paste0(" Treatment efficacy was assessed with a lung health score: %d parameters (%s) were standardized against healthy controls (%s)%s, ",
                       "signed by the direction of the disease effect in untreated animals (%s; leave-one-animal-out for untreated animals), and averaged. ",
                       "Per-animal mean scores from %s to %s were compared between treated (%s), untreated and healthy groups with %ss, and treated and untreated animals ",
                       "were compared over time with a linear mixed model (treatment \u00d7 time, random intercept per animal). Rescue was defined as (untreated \u2212 treated) / (untreated \u2212 healthy) \u00d7 100%%."),
                length(st$parameters), paste(st$parameters, collapse = ", "), paste(st$healthy, collapse = ", "),
                if (st$reference == "baseline") paste0(" using each animal's change from baseline (", st$baseline_timepoint, ")") else " at each timepoint",
                paste(st$disease, collapse = ", "), ef$window[1], ef$window[length(ef$window)], paste(input$tr_treated, collapse = ", "),
                test_names[[ef$test]])
      } else "",
      if (!is.null(ml_res())) {
        s <- ml_res()$settings
        paste0(
          switch(s$task %||% "groups",
            infection = sprintf(" To predict infection, sessions of %s from %s onward were labeled infected and sessions of %s%s were labeled uninfected; ",
                                paste(s$positive, collapse = " and "), s$infection_timepoint, paste(s$negative, collapse = " and "),
                                if (identical(s$pre_infection, "uninfected")) ", plus pre-infection sessions of infected animals," else ""),
            phase = sprintf(" To predict infection phase, post-infection sessions of %s were labeled acute up to day %s post-infection and chronic afterwards (study day was not used as a predictor); the same models were fitted to control animals labeled by the same cutoff to check for time effects unrelated to infection; ",
                            paste(s$positive, collapse = " and "), s$acute_days),
            severity = sprintf(" To predict %s, each session was paired with the animal's measured value; ", s$outcome_name),
            sprintf(" To classify %s versus %s animals, ", s$labels[1], s$labels[2])),
          length(ml_res()$fits),
          " models (", paste(tolower(ml_model_names("all")[names(ml_res()$fits)]), collapse = ", "),
          ") were trained with tidymodels on session-level values of ", length(s$parameters), " parameters",
          if (isTRUE(s$include_day)) " and study day" else "", if (!is.null(s$covariate)) paste0(" and ", s$covariate_name) else "",
          if (length(s$exclude_timepoints)) paste0(", excluding ", paste(s$exclude_timepoints, collapse = ", ")) else "",
          ". Predictors were centered and scaled", if (isTRUE(s$upsample)) " and the smaller class was upsampled in training data" else "",
          "; hyperparameters were tuned by ", if (ml_type(ml_res()) == "regression") "RMSE. " else "ROC AUC. ",
          if (s$validation == "animal") sprintf("Performance was estimated by %d-fold cross-validation in which all sessions of an animal were held out together, at the level of single sessions and of animals (mean of its predictions).", s$folds)
          else "Sessions were split at random into 80% training (tuned by 10-fold cross-validation) and 20% test data; because sessions of the same animal occur in both sets, this estimate is likely optimistic."
        )
      } else ""
    )
  })
  output$methods_text <- renderText(methods_text())

  settings_table <- function() {
    s <- sessions()
    st <- attr(sessions_base(), "settings")
    fs <- fig_settings()
    data.frame(
      Setting = c("Source file", "plethR version", "Sessions defined by", "Session summary", "Rinx filter",
                  "Values expressed as", "Baseline timepoint", "Timepoints analyzed", "Reference group",
                  "Summary metric", "Test", "Comparisons", "Multiple comparison correction", "Parameters exported",
                  "Variability features", "Figure format", "Exported"),
      Value = c(rv$file_name, app_version, st$timepoint, st$stat,
                if (is.null(st$rinx_max)) "off" else paste0("Rinx <= ", st$rinx_max, "%"),
                transform(), if (transform() == "none") "" else input$baseline_tp,
                paste(levels(s$timepoint), collapse = ", "), reference(), metric_text(),
                test_names[[input$test]], input$comp_mode, input$padj, paste(input$params_multi, collapse = ", "),
                if (is.null(st$variability)) "off" else paste(paste(st$variability$parameters, collapse = ", "), "x",
                                                             paste(st$variability$features, collapse = ", ")),
                paste0(toupper(fs$format), if (fs$format %in% c("png", "tiff")) paste0(", ", fs$dpi, " dpi") else ""),
                format(Sys.time(), "%Y-%m-%d %H:%M"))
    )
  }

  write_workbook <- function(file) {
      s <- sessions()
      s$start <- format(s$start, "%Y-%m-%d %H:%M")
      sel <- input$params_multi
      settings <- settings_table()
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
      if (identical(input$tc_method, "mixed") && has_comparisons()) {
        mm <- lapply(sel, function(p) tryCatch(mixed_timecourse(sessions(), p, reference(), input$tc_cov %||% character(0), sex = design_sex(),
                                                                log_transform = isTRUE(input$tc_mm_log), p_adjust = input$padj), error = function(e) NULL))
        names(mm) <- sel
        mm <- mm[!vapply(mm, is.null, logical(1))]
        if (length(mm)) {
          sheets$Mixed_model_terms <- do.call(rbind, lapply(names(mm), function(p) cbind(parameter = p, mm[[p]]$anova)))
          sheets$Mixed_model_timepoints <- do.call(rbind, lapply(names(mm), function(p) cbind(parameter = p, mm[[p]]$contrasts)))
        }
      }
      if (!is.null(rv$design)) {
        d <- rv$design
        sheets$Design_animals <- d$animals
        if (nrow(d$weights)) sheets$Design_weights <- d$weights
        if (nrow(d$cfu)) {
          sheets$Design_CFU <- d$cfu
          cc <- tryCatch(cfu_correlation(sessions(), d$cfu, design_dpi_r(), sel), error = function(e) NULL)
          if (!is.null(cc)) { sheets$CFU_correlations <- cc$correlations; sheets$CFU_breathing_pairs <- cc$pairs }
        }
      }
      if (!is.null(ml_res())) {
        obj <- ml_res()
        sheets$ML_metrics <- merge(obj$metrics, obj$tuning, by = "model", all.x = TRUE)
        sheets$ML_predictions <- obj$predictions
        if (!is.null(obj$importance)) sheets$ML_importance <- obj$importance
      }
      if (tr_valid()) {
        ef <- tr_eff()
        sheets$Treatment_summary <- cbind(ef$summary, rescue_percent = c(NA, NA, ef$rescue)[seq_len(nrow(ef$summary))])
        sheets$Treatment_tests <- ef$comparisons
        if (!is.null(ef$model)) sheets$Treatment_mixed_model <- ef$model
        sheets$Treatment_by_parameter <- ef$per_parameter
        sheets$Lung_health_animals <- ef$animals
        sheets$Lung_health_sessions <- as.data.frame(tr_scored()[, c("group", "subject", "timepoint", "lung_score")])
      }
      if (fac_valid()) {
        sheets$Factorial_design <- stats::setNames(fac_design(), c("Group", fac_names()))
        sheets$Factorial_model <- tryCatch(fac_results(), error = function(e) data.frame(Message = conditionMessage(e)))
      }
      writexl::write_xlsx(sheets, path = file)
  }

  output$dl_xlsx <- downloadHandler(
    filename = function() paste0(safe_name(tools::file_path_sans_ext(rv$file_name %||% "plethR")), "_plethR_results.xlsx"),
    content = function(file) write_workbook(file)
  )

  # -- Guide and status ---------------------------------------------------------------
  output$glossary <- renderDT({
    g <- wbp_parameter_info()
    names(g) <- c("Parameter", "Name", "Unit", "Description")
    dtx(g, rownames = FALSE, class = "compact", options = list(dom = "ft", paging = FALSE))
  })

  output$status_pill <- renderUI({
    if (is.null(rv$raw)) return(span(class = "badge text-bg-light status-pill", "No data loaded"))
    g <- tryCatch(length(groups_ok()), error = function(e) 0)
    span(class = "badge text-bg-light status-pill",
         " ", rv$file_name, " \u00b7 ", length(unique(rv$raw$subject)), " animals \u00b7 ", g, " groups")
  })
}

shinyApp(ui = ui, server = server)
