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
} else if (requireNamespace("plethR", quietly = TRUE)) {
  library(plethR)
} else {
  stop("Could not find the plethR package. Install it with devtools::install() or run this app with plethR::run_plethR_app().")
}

summary_fun_map <- list(
  "Mean" = base::mean,
  "Median" = stats::median,
  "Standard deviation" = stats::sd,
  "Minimum" = base::min,
  "Maximum" = base::max
)

palette_choices <- c("Set1", "Set2", "Dark2", "viridis", "plasma", "magma")
metadata_cols <- c("Time", "time", "Tbody", "Subject", "Phase", "Rinx",
                    "RH", "Tc", "Recording", "Alarms", "BFCF",
                    "group", "subject_id", "sample_id", "n")

numeric_param_choices <- function(df_list) {
  if (is.null(df_list) || length(df_list) == 0) return(character(0))
  cols <- unique(unlist(lapply(df_list, function(df) names(df)[sapply(df, is.numeric)])))
  sort(setdiff(cols, metadata_cols))
}

save_plot_to_tempfile <- function(plot_obj, width = 8, height = 6, dpi = 300) {
  tmp <- tempfile(fileext = ".png")
  ggplot2::ggsave(tmp, plot = plot_obj, width = width, height = height, dpi = dpi, bg = "white")
  tmp
}

no_data_message <- function(step_name) {
  sprintf("No data available yet. Complete the \"%s\" step first.", step_name)
}

# ---- UI ----------------------------------------------------------------

system_font_stack <- font_collection(
  "-apple-system", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif"
)

app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#2C5F7C",
  base_font = system_font_stack,
  heading_font = system_font_stack
)

import_panel <- nav_panel(
  "Import Data",
  layout_sidebar(
    sidebar = sidebar(
      width = 360,
      h5("Load an Excel file"),
      p(class = "text-muted small",
        "Upload a multi-sheet Excel export from a DSI whole body plethysmography system. Each sheet should represent one subject."),
      fileInput("excel_file", "Excel file (.xlsx)", accept = c(".xlsx")),
      checkboxInput("opt_clean_time", "Standardize the Time column to a date format", value = TRUE),
      checkboxInput("opt_remove_apnea", "Exclude sheets with \"Apnea\" in the name", value = FALSE),
      checkboxInput("opt_remove_na", "Remove rows with missing values", value = FALSE),
      checkboxInput("opt_date_average", "Average repeated measurements by date", value = FALSE),
      p(class = "text-muted small",
        "Averaging by date requires the Time column to be standardized first."),
      actionButton("btn_import", "Import data", class = "btn-primary w-100"),
      hr(),
      actionButton("btn_reset_all", "Start a new session", class = "btn-outline-secondary w-100")
    ),
    h4("Imported sheets"),
    p(class = "text-muted",
      "This table lists every sheet found in the uploaded file. Confirm the counts look correct before moving on to group assignment."),
    shinycssloaders::withSpinner(DTOutput("import_summary_table"), color = "#2C5F7C")
  )
)

groups_panel <- nav_panel(
  "Define Groups",
  layout_sidebar(
    sidebar = sidebar(
      width = 360,
      h5("Step 1: name your groups"),
      numericInput("n_groups", "Number of experimental groups", value = 2, min = 1, max = 12, step = 1),
      actionButton("btn_make_name_inputs", "Create name fields", class = "btn-outline-primary w-100"),
      uiOutput("group_name_inputs"),
      actionButton("btn_confirm_names", "Confirm group names", class = "btn-primary w-100"),
      hr(),
      h5("Step 2: assign sheets to groups"),
      p(class = "text-muted small",
        "Choose which sheets belong to each group. Every sheet should be assigned to exactly one group."),
      uiOutput("group_assignment_inputs"),
      actionButton("btn_build_mapping", "Save group assignments", class = "btn-primary w-100")
    ),
    h4("Group assignment summary"),
    uiOutput("group_mapping_status"),
    DTOutput("group_mapping_table")
  )
)

averages_panel <- nav_panel(
  "Group Averages",
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      h5("Compute averages across time"),
      selectInput("avg_time_var", "Time column", choices = c("Time"), selected = "Time"),
      selectInput("avg_summary_fun", "Summary statistic", choices = names(summary_fun_map), selected = "Mean"),
      checkboxInput("avg_keep_n", "Include sample size (n) per time point", value = TRUE),
      actionButton("btn_compute_averages", "Calculate group averages", class = "btn-primary w-100")
    ),
    h4("Averaged data by group"),
    uiOutput("averages_status"),
    uiOutput("averages_group_selector"),
    shinycssloaders::withSpinner(DTOutput("averages_table"), color = "#2C5F7C")
  )
)

auc_panel <- nav_panel(
  "AUC Analysis",
  layout_sidebar(
    sidebar = sidebar(
      width = 340,
      h5("Area under the curve"),
      uiOutput("auc_parameter_selector"),
      selectInput("auc_normalize_to", "Normalize to group (optional)", choices = c("None" = "")),
      checkboxInput("auc_baseline_correct", "Baseline correct (subtract first time point)", value = FALSE),
      radioButtons("auc_method", "Calculation method",
                   choices = c("Trapezoidal rule" = "trapezoid", "Simple sum" = "sum"),
                   selected = "trapezoid"),
      checkboxInput("auc_exclude_na", "Exclude missing values", value = TRUE),
      actionButton("btn_compute_auc", "Calculate AUC", class = "btn-primary w-100"),
      hr(),
      downloadButton("dl_auc_table", "Download AUC results (.xlsx)", class = "btn-outline-secondary w-100")
    ),
    h4("AUC results"),
    uiOutput("auc_status"),
    shinycssloaders::withSpinner(DTOutput("auc_table"), color = "#2C5F7C")
  )
)

timeseries_panel <- nav_panel(
  "Time Series",
  layout_sidebar(
    sidebar = sidebar(
      width = 340,
      h5("Time series plots"),
      radioButtons("ts_plot_type", "Layout",
                   choices = c("One plot per parameter" = "by_parameter",
                               "One plot per group" = "by_group",
                               "Single combined plot" = "combined"),
                   selected = "by_parameter"),
      uiOutput("ts_parameter_selector"),
      radioButtons("ts_smooth_method", "Smoothing",
                   choices = c("None" = "none", "Rolling average" = "rolling", "LOESS" = "loess"),
                   selected = "none"),
      conditionalPanel("input.ts_smooth_method == 'rolling'",
                        numericInput("ts_smooth_window", "Rolling window size", value = 5, min = 2, max = 30)),
      conditionalPanel("input.ts_smooth_method == 'loess'",
                        sliderInput("ts_smooth_span", "LOESS span", value = 0.3, min = 0.05, max = 1, step = 0.05),
                        checkboxInput("ts_show_se", "Show confidence band", value = FALSE)),
      selectInput("ts_palette", "Color palette", choices = palette_choices, selected = "Set1"),
      sliderInput("ts_line_size", "Line thickness", value = 0.8, min = 0.2, max = 2, step = 0.1),
      checkboxInput("ts_show_points", "Show data points", value = FALSE),
      actionButton("btn_generate_ts", "Generate plots", class = "btn-primary w-100")
    ),
    h4("Time series"),
    uiOutput("ts_status"),
    uiOutput("ts_plot_selector"),
    shinycssloaders::withSpinner(plotOutput("ts_plot_preview", height = "520px"), color = "#2C5F7C"),
    downloadButton("dl_ts_plot", "Download this plot (.png)", class = "btn-outline-secondary mt-2")
  )
)

aucbars_panel <- nav_panel(
  "AUC Bar Plots",
  layout_sidebar(
    sidebar = sidebar(
      width = 340,
      h5("AUC bar plots"),
      uiOutput("bar_parameter_selector"),
      uiOutput("bar_group_order_selector"),
      radioButtons("bar_plot_type", "Layout",
                   choices = c("Separate plots" = "separate", "Combined multi-panel plot" = "combined"),
                   selected = "separate"),
      radioButtons("bar_value_type", "Values to plot",
                   choices = c("Raw AUC" = "auc", "Normalized (fold change)" = "normalized"),
                   selected = "auc"),
      radioButtons("bar_y_axis_start", "Y-axis starting point",
                   choices = c("Smart (emphasize differences)" = "smart", "Zero" = "zero"),
                   selected = "smart"),
      checkboxInput("bar_show_values", "Show values above bars", value = TRUE),
      checkboxInput("bar_show_stats", "Show significance indicators", value = FALSE),
      conditionalPanel("input.bar_show_stats == true",
                        uiOutput("bar_reference_group_selector")),
      selectInput("bar_palette", "Color palette", choices = c(palette_choices, "grayscale"), selected = "Set1"),
      actionButton("btn_generate_bars", "Generate plots", class = "btn-primary w-100")
    ),
    h4("AUC bar plots"),
    uiOutput("bar_status"),
    uiOutput("bar_plot_selector"),
    shinycssloaders::withSpinner(plotOutput("bar_plot_preview", height = "520px"), color = "#2C5F7C"),
    downloadButton("dl_bar_plot", "Download this plot (.png)", class = "btn-outline-secondary mt-2")
  )
)

heatmap_panel <- nav_panel(
  "Heatmap",
  layout_sidebar(
    sidebar = sidebar(
      width = 340,
      h5("AUC heatmap"),
      radioButtons("hm_value_type", "Values to plot",
                   choices = c("Normalized (fold change)" = "normalized", "Raw AUC" = "auc"),
                   selected = "normalized"),
      uiOutput("hm_exclude_groups_selector"),
      uiOutput("hm_include_parameters_selector"),
      checkboxInput("hm_cluster_rows", "Cluster parameters", value = TRUE),
      checkboxInput("hm_cluster_cols", "Cluster groups", value = TRUE),
      selectInput("hm_color_scheme", "Color scheme",
                  choices = c("RdYlBu", "RdBu", "PRGn", "BrBG", "viridis", "plasma", "magma"),
                  selected = "RdYlBu"),
      checkboxInput("hm_show_values", "Show values in cells", value = FALSE),
      actionButton("btn_generate_heatmap", "Generate heatmap", class = "btn-primary w-100"),
      hr(),
      downloadButton("dl_heatmap", "Download heatmap (.png)", class = "btn-outline-secondary w-100")
    ),
    h4("AUC heatmap"),
    uiOutput("hm_status"),
    shinycssloaders::withSpinner(plotOutput("hm_plot_preview", height = "560px"), color = "#2C5F7C")
  )
)

pca_panel <- nav_panel(
  "PCA",
  layout_sidebar(
    sidebar = sidebar(
      width = 340,
      h5("Principal component analysis"),
      uiOutput("pca_parameter_selector"),
      checkboxInput("pca_use_clustering", "Apply k-means clustering", value = FALSE),
      conditionalPanel("input.pca_use_clustering == true",
                        numericInput("pca_num_clusters", "Number of clusters", value = 4, min = 2, max = 10)),
      radioButtons("pca_color_by", "Color points by",
                   choices = c("Experimental group" = "group", "Cluster" = "cluster"), selected = "group"),
      checkboxInput("pca_show_ellipses", "Show confidence ellipses", value = TRUE),
      checkboxInput("pca_show_labels", "Show subject labels", value = TRUE),
      checkboxInput("pca_show_loadings", "Show loading vectors", value = FALSE),
      conditionalPanel("input.pca_show_loadings == true",
                        numericInput("pca_n_loadings", "Number of loading vectors", value = 5, min = 1, max = 15)),
      selectInput("pca_palette", "Color palette", choices = palette_choices, selected = "Set1"),
      actionButton("btn_generate_pca", "Run PCA", class = "btn-primary w-100"),
      hr(),
      downloadButton("dl_pca_plot", "Download PCA plot (.png)", class = "btn-outline-secondary w-100")
    ),
    h4("PCA results"),
    uiOutput("pca_status"),
    shinycssloaders::withSpinner(plotOutput("pca_plot_preview", height = "520px"), color = "#2C5F7C"),
    h5("Variance explained", class = "mt-4"),
    DTOutput("pca_variance_table")
  )
)

export_panel <- nav_panel(
  "Export",
  layout_sidebar(
    sidebar = sidebar(
      width = 340,
      h5("Export results"),
      p(class = "text-muted small", "Select the datasets to include and download a single Excel workbook."),
      checkboxGroupInput("export_choices", NULL,
                         choices = c("Group averages" = "averages",
                                     "AUC results" = "auc",
                                     "Combined raw subject data" = "raw")),
      textInput("export_filename", "File name", value = "plethR_results"),
      downloadButton("dl_export_workbook", "Download Excel workbook", class = "btn-primary w-100")
    ),
    h4("Export"),
    p("Build and download a workbook containing the analysis results currently available in this session."),
    uiOutput("export_status")
  )
)

about_panel <- nav_panel(
  "Overview",
  div(
    class = "container py-4",
    h3("plethR: Whole Body Plethysmography Analysis"),
    p("This application walks through the full plethR workflow for analyzing respiratory data from DSI whole body plethysmography systems."),
    tags$ol(
      tags$li(tags$b("Import Data:"), " upload a multi-sheet Excel file and choose cleaning options."),
      tags$li(tags$b("Define Groups:"), " name your experimental groups and assign sheets to each one."),
      tags$li(tags$b("Group Averages:"), " summarize each group across time."),
      tags$li(tags$b("AUC Analysis:"), " calculate area under the curve, with optional normalization and baseline correction."),
      tags$li(tags$b("Time Series, AUC Bar Plots, Heatmap, PCA:"), " generate publication-quality figures."),
      tags$li(tags$b("Export:"), " download results as an Excel workbook.")
    ),
    p(class = "text-muted", "Work through the tabs in order. Each step builds on the previous one, and status messages will indicate if a prior step still needs to be completed.")
  )
)

ui <- page_navbar(
  title = "plethR",
  theme = app_theme,
  fillable = TRUE,
  about_panel,
  import_panel,
  groups_panel,
  averages_panel,
  auc_panel,
  timeseries_panel,
  aucbars_panel,
  heatmap_panel,
  pca_panel,
  export_panel
)

# ---- Server -------------------------------------------------------------

server <- function(input, output, session) {

  rv <- reactiveValues(
    df_list = NULL,
    sheet_summary = NULL,
    group_names = NULL,
    group_mapping = NULL,
    group_averages = NULL,
    auc_results = NULL,
    ts_plots = NULL,
    bar_plots = NULL,
    heatmap_result = NULL,
    pca_result = NULL
  )

  reset_session <- function() {
    rv$df_list <- NULL
    rv$sheet_summary <- NULL
    rv$group_names <- NULL
    rv$group_mapping <- NULL
    rv$group_averages <- NULL
    rv$auc_results <- NULL
    rv$ts_plots <- NULL
    rv$bar_plots <- NULL
    rv$heatmap_result <- NULL
    rv$pca_result <- NULL
  }

  observeEvent(input$btn_reset_all, {
    reset_session()
    showNotification("Session cleared. Upload a new file to begin.", type = "message")
  })

  # -- Import ------------------------------------------------------------

  observeEvent(input$btn_import, {
    req(input$excel_file)

    result <- tryCatch({
      sheets_into_list(
        input$excel_file$datapath,
        remove_apnea = input$opt_remove_apnea,
        clean_time = input$opt_clean_time,
        date_average = input$opt_date_average,
        remove_na = input$opt_remove_na
      )
    }, error = function(e) {
      showNotification(paste("Import failed:", conditionMessage(e)), type = "error", duration = 10)
      NULL
    })

    if (is.null(result) || length(result) == 0) return()

    rv$df_list <- result
    rv$group_mapping <- NULL
    rv$group_averages <- NULL
    rv$auc_results <- NULL

    rv$sheet_summary <- data.frame(
      Index = seq_along(result),
      Sheet = names(result),
      Rows = sapply(result, nrow),
      Columns = sapply(result, ncol),
      `Has Time Column` = sapply(result, function(df) "Time" %in% names(df)),
      check.names = FALSE
    )

    time_candidates <- unique(unlist(lapply(result, function(df) names(df)[grepl("time", names(df), ignore.case = TRUE)])))
    if (length(time_candidates) == 0) time_candidates <- "Time"
    updateSelectInput(session, "avg_time_var", choices = time_candidates, selected = time_candidates[1])

    showNotification(sprintf("Imported %d sheet(s) successfully.", length(result)), type = "message")
  })

  output$import_summary_table <- renderDT({
    validate(need(rv$sheet_summary, "Upload an Excel file and click \"Import data\" to see a summary of its sheets here."))
    datatable(rv$sheet_summary, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })

  # -- Group definition ----------------------------------------------------

  group_count <- reactiveVal(0)

  observeEvent(input$btn_make_name_inputs, {
    req(input$n_groups)
    group_count(input$n_groups)
  })

  output$group_name_inputs <- renderUI({
    n <- group_count()
    if (n == 0) return(NULL)
    tagList(
      lapply(seq_len(n), function(i) {
        textInput(paste0("group_name_", i), paste("Group", i, "name"), value = "")
      })
    )
  })

  observeEvent(input$btn_confirm_names, {
    n <- group_count()
    req(n > 0)

    names_raw <- vapply(seq_len(n), function(i) {
      val <- input[[paste0("group_name_", i)]]
      if (is.null(val)) "" else trimws(val)
    }, character(1))

    if (any(names_raw == "")) {
      showNotification("Every group needs a name before continuing.", type = "error")
      return()
    }

    result <- tryCatch(set_group_names(names_raw), error = function(e) {
      showNotification(paste("Group names error:", conditionMessage(e)), type = "error")
      NULL
    })

    if (is.null(result)) return()

    rv$group_names <- result
    rv$group_mapping <- NULL
    showNotification("Group names confirmed. Now assign sheets to each group.", type = "message")
  })

  output$group_assignment_inputs <- renderUI({
    req(rv$group_names, rv$df_list)
    sheet_choices <- setNames(seq_along(rv$df_list), names(rv$df_list))

    tagList(
      lapply(seq_along(rv$group_names), function(i) {
        selectizeInput(
          paste0("group_sheets_", i),
          label = sprintf("Sheets for \"%s\"", rv$group_names[i]),
          choices = sheet_choices,
          multiple = TRUE
        )
      })
    )
  })

  observeEvent(input$btn_build_mapping, {
    req(rv$group_names, rv$df_list)

    mapping <- list()
    for (i in seq_along(rv$group_names)) {
      sel <- input[[paste0("group_sheets_", i)]]
      mapping[[rv$group_names[i]]] <- if (is.null(sel)) integer(0) else as.integer(sel)
    }

    all_indices <- unlist(mapping)
    if (length(all_indices) == 0) {
      showNotification("Assign at least one sheet to a group before continuing.", type = "error")
      return()
    }

    if (length(all_indices) != length(unique(all_indices))) {
      showNotification("Each sheet can only belong to one group. Remove duplicate assignments.", type = "error")
      return()
    }

    rv$group_mapping <- mapping
    rv$group_averages <- NULL
    rv$auc_results <- NULL

    updateSelectInput(session, "auc_normalize_to",
                       choices = c("None" = "", setNames(rv$group_names, rv$group_names)))

    showNotification("Group assignments saved.", type = "message")
  })

  output$group_mapping_status <- renderUI({
    if (is.null(rv$group_mapping)) {
      return(p(class = "text-muted", no_data_message("Define Groups")))
    }
    unassigned <- length(rv$df_list) - length(unlist(rv$group_mapping))
    if (unassigned > 0) {
      tagList(p(class = "text-warning", sprintf("%d sheet(s) are not yet assigned to a group.", unassigned)))
    } else {
      p(class = "text-success", "All sheets are assigned.")
    }
  })

  output$group_mapping_table <- renderDT({
    validate(need(rv$group_mapping, ""))
    rows <- lapply(names(rv$group_mapping), function(g) {
      idx <- rv$group_mapping[[g]]
      data.frame(
        Group = g,
        `Number of Sheets` = length(idx),
        Sheets = paste(names(rv$df_list)[idx], collapse = ", "),
        check.names = FALSE
      )
    })
    datatable(do.call(rbind, rows), rownames = FALSE, options = list(dom = "t"))
  })

  # -- Group averages -------------------------------------------------------

  observeEvent(input$btn_compute_averages, {
    req(rv$df_list, rv$group_mapping)

    fun <- summary_fun_map[[input$avg_summary_fun]]

    result <- tryCatch({
      calculate_group_averages(
        rv$df_list, rv$group_mapping,
        time_var = input$avg_time_var,
        summary_fun = fun,
        keep_n = input$avg_keep_n
      )
    }, error = function(e) {
      showNotification(paste("Calculation failed:", conditionMessage(e)), type = "error", duration = 10)
      NULL
    })

    if (is.null(result)) return()

    rv$group_averages <- result
    rv$auc_results <- NULL

    params <- numeric_param_choices(result)
    updateSelectInput(session, "ts_palette", selected = input$ts_palette)

    showNotification("Group averages calculated.", type = "message")
  })

  output$averages_status <- renderUI({
    if (is.null(rv$group_averages)) p(class = "text-muted", no_data_message("Define Groups"))
  })

  output$averages_group_selector <- renderUI({
    req(rv$group_averages)
    selectInput("averages_selected_group", "Group to display", choices = names(rv$group_averages))
  })

  output$averages_table <- renderDT({
    validate(need(rv$group_averages, ""))
    sel <- input$averages_selected_group
    validate(need(sel %in% names(rv$group_averages), "Select a group to preview its averaged data."))
    datatable(rv$group_averages[[sel]], rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })

  # -- AUC -----------------------------------------------------------------

  output$auc_parameter_selector <- renderUI({
    req(rv$group_averages)
    choices <- numeric_param_choices(rv$group_averages)
    selectizeInput("auc_parameters", "Parameters (leave empty for all)", choices = choices, multiple = TRUE)
  })

  observeEvent(input$btn_compute_auc, {
    req(rv$group_averages)

    params <- if (length(input$auc_parameters) == 0) NULL else input$auc_parameters
    normalize_to <- if (identical(input$auc_normalize_to, "")) NULL else input$auc_normalize_to

    result <- tryCatch({
      calculate_auc(
        rv$group_averages,
        time_var = input$avg_time_var,
        parameters = params,
        normalize_to = normalize_to,
        baseline_correct = input$auc_baseline_correct,
        method = input$auc_method,
        exclude_na = input$auc_exclude_na
      )
    }, error = function(e) {
      showNotification(paste("AUC calculation failed:", conditionMessage(e)), type = "error", duration = 10)
      NULL
    })

    if (is.null(result)) return()

    rv$auc_results <- result
    showNotification("AUC calculated.", type = "message")
  })

  output$auc_status <- renderUI({
    if (is.null(rv$group_averages)) p(class = "text-muted", no_data_message("Group Averages"))
  })

  output$auc_table <- renderDT({
    validate(need(rv$auc_results, "Calculate AUC to see results here."))
    dt <- datatable(rv$auc_results, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
    formatRound(dt, columns = intersect(c("auc", "auc_normalized", "fold_change"), names(rv$auc_results)), digits = 3)
  })

  output$dl_auc_table <- downloadHandler(
    filename = function() "auc_results.xlsx",
    content = function(file) {
      req(rv$auc_results)
      writexl::write_xlsx(list(AUC_Results = rv$auc_results), path = file)
    }
  )

  # -- Time series -----------------------------------------------------------

  output$ts_parameter_selector <- renderUI({
    req(rv$group_averages)
    choices <- numeric_param_choices(rv$group_averages)
    selectizeInput("ts_parameters", "Parameters (leave empty for all)", choices = choices, multiple = TRUE)
  })

  observeEvent(input$btn_generate_ts, {
    req(rv$group_averages)

    params <- if (length(input$ts_parameters) == 0) NULL else input$ts_parameters

    result <- tryCatch({
      plot_wbp_timeseries(
        rv$group_averages,
        time_var = input$avg_time_var,
        plot_type = input$ts_plot_type,
        parameters = params,
        smooth_method = input$ts_smooth_method,
        smooth_window = input$ts_smooth_window,
        smooth_span = input$ts_smooth_span,
        color_palette = input$ts_palette,
        line_size = input$ts_line_size,
        show_points = input$ts_show_points,
        show_se = isTRUE(input$ts_show_se)
      )
    }, error = function(e) {
      showNotification(paste("Plot generation failed:", conditionMessage(e)), type = "error", duration = 10)
      NULL
    })

    if (is.null(result)) return()

    rv$ts_plots <- result
    updateSelectInput(session, "ts_selected_plot", choices = names(result))
    showNotification(sprintf("Generated %d plot(s).", length(result)), type = "message")
  })

  output$ts_status <- renderUI({
    if (is.null(rv$group_averages)) p(class = "text-muted", no_data_message("Group Averages"))
  })

  output$ts_plot_selector <- renderUI({
    req(rv$ts_plots)
    selectInput("ts_selected_plot", "Plot to preview", choices = names(rv$ts_plots))
  })

  output$ts_plot_preview <- renderPlot({
    validate(need(rv$ts_plots, "Generate plots to see a preview here."))
    sel <- input$ts_selected_plot
    validate(need(sel %in% names(rv$ts_plots), ""))
    rv$ts_plots[[sel]]
  })

  output$dl_ts_plot <- downloadHandler(
    filename = function() paste0("timeseries_", input$ts_selected_plot, ".png"),
    content = function(file) {
      req(rv$ts_plots, input$ts_selected_plot)
      ggplot2::ggsave(file, plot = rv$ts_plots[[input$ts_selected_plot]], width = 10, height = 6, dpi = 300, bg = "white")
    }
  )

  # -- AUC bar plots --------------------------------------------------------

  output$bar_parameter_selector <- renderUI({
    req(rv$auc_results)
    selectizeInput("bar_parameters", "Parameters (leave empty for all)",
                    choices = sort(unique(rv$auc_results$parameter)), multiple = TRUE)
  })

  output$bar_group_order_selector <- renderUI({
    req(rv$auc_results)
    selectizeInput("bar_group_order", "Group order (click groups in the order you want them displayed)",
                    choices = sort(unique(rv$auc_results$group)), multiple = TRUE)
  })

  output$bar_reference_group_selector <- renderUI({
    req(rv$auc_results)
    selectInput("bar_reference_group", "Reference group", choices = sort(unique(rv$auc_results$group)))
  })

  observeEvent(input$btn_generate_bars, {
    req(rv$auc_results)

    params <- if (length(input$bar_parameters) == 0) NULL else input$bar_parameters
    group_order <- if (length(input$bar_group_order) == 0) NULL else input$bar_group_order
    reference_group <- if (isTRUE(input$bar_show_stats)) input$bar_reference_group else NULL

    result <- tryCatch({
      plot_auc_bars(
        rv$auc_results,
        parameters = params,
        group_order = group_order,
        plot_type = input$bar_plot_type,
        value_type = input$bar_value_type,
        y_axis_start = input$bar_y_axis_start,
        show_values = input$bar_show_values,
        show_stats = isTRUE(input$bar_show_stats),
        reference_group = reference_group,
        color_palette = input$bar_palette
      )
    }, error = function(e) {
      showNotification(paste("Plot generation failed:", conditionMessage(e)), type = "error", duration = 10)
      NULL
    })

    if (is.null(result)) return()

    rv$bar_plots <- result
    updateSelectInput(session, "bar_selected_plot", choices = names(result))
    showNotification(sprintf("Generated %d plot(s).", length(result)), type = "message")
  })

  output$bar_status <- renderUI({
    if (is.null(rv$auc_results)) p(class = "text-muted", no_data_message("AUC Analysis"))
  })

  output$bar_plot_selector <- renderUI({
    req(rv$bar_plots)
    selectInput("bar_selected_plot", "Plot to preview", choices = names(rv$bar_plots))
  })

  output$bar_plot_preview <- renderPlot({
    validate(need(rv$bar_plots, "Generate plots to see a preview here."))
    sel <- input$bar_selected_plot
    validate(need(sel %in% names(rv$bar_plots), ""))
    rv$bar_plots[[sel]]
  })

  output$dl_bar_plot <- downloadHandler(
    filename = function() paste0("auc_bars_", input$bar_selected_plot, ".png"),
    content = function(file) {
      req(rv$bar_plots, input$bar_selected_plot)
      ggplot2::ggsave(file, plot = rv$bar_plots[[input$bar_selected_plot]], width = 6, height = 4.5, dpi = 300, bg = "white")
    }
  )

  # -- Heatmap ---------------------------------------------------------------

  output$hm_exclude_groups_selector <- renderUI({
    req(rv$auc_results)
    selectizeInput("hm_exclude_groups", "Exclude groups (optional)",
                    choices = sort(unique(rv$auc_results$group)), multiple = TRUE)
  })

  output$hm_include_parameters_selector <- renderUI({
    req(rv$auc_results)
    selectizeInput("hm_include_parameters", "Include only these parameters (optional)",
                    choices = sort(unique(rv$auc_results$parameter)), multiple = TRUE)
  })

  observeEvent(input$btn_generate_heatmap, {
    req(rv$auc_results)

    value_type <- input$hm_value_type
    if (value_type == "normalized" && !"auc_normalized" %in% names(rv$auc_results)) {
      showNotification("Normalized AUC is not available. Recalculate AUC with a normalization group selected, or choose raw AUC here.",
                        type = "error", duration = 10)
      return()
    }

    exclude_groups <- if (length(input$hm_exclude_groups) == 0) NULL else input$hm_exclude_groups
    include_parameters <- if (length(input$hm_include_parameters) == 0) NULL else input$hm_include_parameters

    rv$heatmap_result <- list(
      auc_results = rv$auc_results,
      value_type = value_type,
      exclude_groups = exclude_groups,
      include_parameters = include_parameters,
      cluster_rows = input$hm_cluster_rows,
      cluster_cols = input$hm_cluster_cols,
      color_scheme = input$hm_color_scheme,
      show_values = input$hm_show_values
    )
    showNotification("Heatmap generated.", type = "message")
  })

  output$hm_status <- renderUI({
    if (is.null(rv$auc_results)) p(class = "text-muted", no_data_message("AUC Analysis"))
  })

  render_heatmap_from_spec <- function(spec) {
    plot_auc_heatmap(
      spec$auc_results,
      value_type = spec$value_type,
      exclude_groups = spec$exclude_groups,
      include_parameters = spec$include_parameters,
      cluster_rows = spec$cluster_rows,
      cluster_cols = spec$cluster_cols,
      color_scheme = spec$color_scheme,
      show_values = spec$show_values
    )
  }

  output$hm_plot_preview <- renderPlot({
    validate(need(rv$heatmap_result, "Generate a heatmap to see a preview here."))
    render_heatmap_from_spec(rv$heatmap_result)
  })

  output$dl_heatmap <- downloadHandler(
    filename = function() "auc_heatmap.png",
    content = function(file) {
      req(rv$heatmap_result)
      grDevices::png(file, width = 8, height = 6, units = "in", res = 300)
      render_heatmap_from_spec(rv$heatmap_result)
      grDevices::dev.off()
    }
  )

  # -- PCA -------------------------------------------------------------------

  output$pca_parameter_selector <- renderUI({
    req(rv$df_list)
    choices <- numeric_param_choices(rv$df_list)
    selectizeInput("pca_parameters", "Parameters (leave empty for all)", choices = choices, multiple = TRUE)
  })

  observeEvent(input$btn_generate_pca, {
    req(rv$df_list)

    params <- if (length(input$pca_parameters) == 0) NULL else input$pca_parameters
    color_by <- if (input$pca_color_by == "cluster" && !isTRUE(input$pca_use_clustering)) "group" else input$pca_color_by

    result <- tryCatch({
      plot_pca(
        rv$df_list,
        group_mapping = rv$group_mapping,
        parameters = params,
        use_clustering = input$pca_use_clustering,
        num_clusters = input$pca_num_clusters,
        color_by = color_by,
        show_ellipses = input$pca_show_ellipses,
        show_labels = input$pca_show_labels,
        show_loadings = input$pca_show_loadings,
        n_loadings = input$pca_n_loadings,
        color_palette = input$pca_palette
      )
    }, error = function(e) {
      showNotification(paste("PCA failed:", conditionMessage(e)), type = "error", duration = 10)
      NULL
    })

    if (is.null(result)) return()

    rv$pca_result <- result
    showNotification("PCA complete.", type = "message")
  })

  output$pca_status <- renderUI({
    if (is.null(rv$df_list)) p(class = "text-muted", no_data_message("Import Data"))
  })

  output$pca_plot_preview <- renderPlot({
    validate(need(rv$pca_result, "Run PCA to see a preview here."))
    rv$pca_result$plot
  })

  output$pca_variance_table <- renderDT({
    validate(need(rv$pca_result, ""))
    dt <- datatable(rv$pca_result$variance, rownames = FALSE, options = list(pageLength = 10, dom = "t"))
    formatRound(dt, columns = c("Variance", "Cumulative"), digits = 1)
  })

  output$dl_pca_plot <- downloadHandler(
    filename = function() "pca_plot.png",
    content = function(file) {
      req(rv$pca_result)
      ggplot2::ggsave(file, plot = rv$pca_result$plot, width = 8, height = 6, dpi = 300, bg = "white")
    }
  )

  # -- Export ----------------------------------------------------------------

  output$export_status <- renderUI({
    available <- c(
      if (!is.null(rv$group_averages)) "Group averages",
      if (!is.null(rv$auc_results)) "AUC results",
      if (!is.null(rv$group_mapping)) "Combined raw subject data"
    )
    if (length(available) == 0) {
      p(class = "text-muted", "No results are available yet. Complete earlier steps before exporting.")
    } else {
      tagList(
        p("Available for export:"),
        tags$ul(lapply(available, tags$li))
      )
    }
  })

  output$dl_export_workbook <- downloadHandler(
    filename = function() paste0(gsub("[^A-Za-z0-9_-]", "_", input$export_filename), ".xlsx"),
    content = function(file) {
      sheets <- list()

      if ("averages" %in% input$export_choices && !is.null(rv$group_averages)) {
        for (g in names(rv$group_averages)) {
          sheets[[substr(paste0("Avg_", g), 1, 31)]] <- rv$group_averages[[g]]
        }
      }

      if ("auc" %in% input$export_choices && !is.null(rv$auc_results)) {
        sheets[["AUC_Results"]] <- rv$auc_results
      }

      if ("raw" %in% input$export_choices && !is.null(rv$group_mapping)) {
        combined <- tryCatch({
          organize_by_groups(rv$df_list, rv$group_mapping,
                             combine_groups = TRUE, combine_all = TRUE, add_subject_id = TRUE)
        }, error = function(e) NULL)
        if (!is.null(combined)) sheets[["Raw_Combined"]] <- combined
      }

      if (length(sheets) == 0) {
        sheets[["Info"]] <- data.frame(Message = "No datasets were selected for export.")
      }

      writexl::write_xlsx(sheets, path = file)
    }
  )
}

shinyApp(ui = ui, server = server)
