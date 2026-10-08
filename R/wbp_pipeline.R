# Subject-level WBP analysis pipeline:
# read_wbp() -> assign_groups() -> summarize_sessions() -> apply_baseline()
#   -> summarize_groups() / summarize_subjects() -> compare_groups() / compare_timepoints()

# Columns FinePointe records for quality control rather than respiratory outcomes.
wbp_qc_columns <- c("Tbody", "Tc", "RH", "Rinx", "Comp")

#' DSI WBP Parameter Descriptions
#'
#' Returns a table describing the respiratory parameters exported by DSI
#' FinePointe whole body plethysmography, with units. Used for axis labels and
#' the parameter glossary in the Shiny app.
#'
#' @return A data frame with columns `parameter`, `name`, `unit` and `description`.
#' @examples
#' wbp_parameter_info()
#' @export
wbp_parameter_info <- function() {
  data.frame(
    parameter = c("f", "TVb", "MVb", "Penh", "PAU", "Rpef", "PIFb", "PEFb",
                  "Ti", "Te", "EF50", "EIP", "EEP", "Tr", "Comp", "TB", "TP",
                  "Tbody", "Tc", "RH", "Rinx"),
    name = c("Respiratory frequency", "Tidal volume", "Minute volume",
             "Enhanced pause", "Pause", "Ratio of time to peak expiratory flow",
             "Peak inspiratory flow", "Peak expiratory flow",
             "Inspiratory time", "Expiratory time",
             "Mid-expiratory flow", "End-inspiratory pause",
             "End-expiratory pause", "Relaxation time", "Compensation",
             "Time of brake", "Time of pause",
             "Body temperature", "Chamber temperature", "Relative humidity",
             "Rejection index"),
    unit = c("breaths/min", "mL", "mL/min", "", "", "", "mL/s", "mL/s",
             "s", "s", "mL/s", "ms", "ms", "s", "", "", "",
             "\u00b0C", "\u00b0C", "%", "%"),
    description = c(
      "Breaths per minute.",
      "Volume of air per breath.",
      "Total ventilation per minute (f \u00d7 TVb).",
      "Unitless index of breathing pattern; often used as an indicator of airway obstruction, but it reflects timing and flow shape, not resistance directly.",
      "(Te \u2212 Tr) / Tr; component of Penh.",
      "Time to peak expiratory flow divided by Te; decreases with airway obstruction.",
      "Maximum flow during inspiration.",
      "Maximum flow during expiration.",
      "Duration of inspiration.",
      "Duration of expiration.",
      "Expiratory flow when 50% of the tidal volume has been exhaled; decreases with airway obstruction.",
      "Pause between inspiration and expiration.",
      "Pause between expiration and the next inspiration (apneic pause).",
      "Time to exhale a set fraction of tidal volume.",
      "FinePointe signal compensation value.",
      "FinePointe timing parameter.",
      "FinePointe timing parameter.",
      "Body temperature used for volume correction (quality control).",
      "Chamber temperature (quality control).",
      "Chamber humidity (quality control).",
      "Percentage of breaths rejected by FinePointe in this record (quality control)."
    ),
    stringsAsFactors = FALSE
  )
}

#' Axis Label for a WBP Parameter
#'
#' @param parameter Parameter name, e.g. `"Penh"`.
#' @param transform Baseline transform applied to the values: `"none"`,
#'   `"percent"` or `"difference"` (see [apply_baseline()]).
#' @return A character label such as `"TVb (mL)"`.
#' @export
wbp_axis_label <- function(parameter, transform = "none") {
  info <- wbp_parameter_info()
  if (is_variability_feature(parameter)) {
    nm <- wbp_feature_name(parameter)
    return(switch(transform, percent = paste0(nm, " (% of baseline)"), difference = paste0("\u0394 ", nm), nm))
  }
  unit <- info$unit[match(parameter, info$parameter)]
  if (identical(transform, "percent")) return(paste0(parameter, " (% of baseline)"))
  if (identical(transform, "difference")) {
    return(paste0("\u0394 ", parameter, if (!is.na(unit) && nzchar(unit)) paste0(" (", unit, ")") else " from baseline"))
  }
  if (is.na(unit) || !nzchar(unit)) parameter else paste0(parameter, " (", unit, ")")
}

#' Read a DSI FinePointe Excel Export
#'
#' Reads every whole body plethysmography sheet of a FinePointe Excel export into
#' one data frame with one row per record. Unlike [sheets_into_list()], it keeps
#' the `Phase` column (the session label), drops the FinePointe log lines at the
#' top of each sheet, and skips empty sheets (such as `.Apnea` sheets with no data).
#'
#' @param file Path to the `.xlsx` file. Each sheet is one animal.
#' @param sheet_pattern Regular expression selecting which sheets to read. If no
#'   sheet matches, all sheets are read. Default `"WBP"` keeps `.WBPth` sheets.
#' @param progress Optional function called as `progress(i, n, sheet)` before each
#'   sheet is read, e.g. to update a progress bar.
#'
#' @return A data frame with columns `subject`, `sheet`, `Time`, `Phase` and one
#'   numeric column per parameter. Attribute `"parameters"` lists the respiratory
#'   parameters (QC columns such as `Rinx` are kept but not listed).
#' @examples
#' \dontrun{
#' wbp <- read_wbp("experiment.xlsx")
#' }
#' @export
read_wbp <- function(file, sheet_pattern = "WBP", progress = NULL) {
  sheets <- readxl::excel_sheets(file)
  if (!is.null(sheet_pattern) && any(grepl(sheet_pattern, sheets))) {
    sheets <- sheets[grepl(sheet_pattern, sheets)]
  }

  out <- list()
  for (i in seq_along(sheets)) {
    if (is.function(progress)) progress(i, length(sheets), sheets[i])
    df <- readxl::read_excel(file, sheet = sheets[i], guess_max = 5000)
    if (nrow(df) == 0 || !"Time" %in% names(df)) next

    numeric_cols <- setdiff(names(df)[vapply(df, is.numeric, logical(1))], "Time")
    if (length(numeric_cols) == 0) next
    # FinePointe log lines ("Measurement ...", "Create measurement ...") have no values.
    df <- df[rowSums(!is.na(df[numeric_cols])) > 0, , drop = FALSE]
    if (nrow(df) == 0) next

    subject <- sub("\\.(WBPth|WBP|Apnea)$", "", sheets[i], ignore.case = TRUE)
    if (subject %in% names(out)) subject <- sheets[i]

    out[[subject]] <- data.frame(
      subject = subject,
      sheet = sheets[i],
      Time = as.POSIXct(df$Time),
      Phase = if ("Phase" %in% names(df)) as.character(df$Phase) else NA_character_,
      df[numeric_cols],
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }

  if (length(out) == 0) stop("No sheets with plethysmography data were found in this file.")

  data <- dplyr::bind_rows(out)
  numeric_cols <- setdiff(names(data), c("subject", "sheet", "Time", "Phase"))
  attr(data, "parameters") <- setdiff(numeric_cols, wbp_qc_columns)
  data
}

#' Suggest Experimental Groups from Subject Names
#'
#' Strips trailing animal numbers from subject names, so
#' `"Infected WT1"` and `"Infected WT2"` both suggest group `"Infected WT"`.
#' Subjects whose names are only numbers get no suggestion.
#'
#' @param subjects Character vector of subject names.
#' @return A character vector of suggested group names (`NA` when no suggestion).
#' @examples
#' suggest_groups(c("Infected WT1", "Infected WT2", "Control 1", "7"))
#' @export
suggest_groups <- function(subjects) {
  groups <- trimws(sub("[-_ #.]*[0-9]+$", "", subjects))
  groups[groups == "" | groups == subjects] <- NA_character_
  groups
}

#' Assign Animals to Experimental Groups
#'
#' @param data Output of [read_wbp()] or [summarize_sessions()] (any data frame
#'   with a `subject` column).
#' @param groups Named list mapping each group name to a character vector of
#'   subjects, e.g. `list(Control = c("A1", "A2"), Treated = c("B1", "B2"))`.
#'   The order of the list sets the display order. Subjects not listed are dropped.
#' @return `data` with a factor column `group`, keeping only assigned subjects.
#' @export
assign_groups <- function(data, groups) {
  if (!is.list(groups) || is.null(names(groups))) stop("groups must be a named list")
  lookup <- stats::setNames(rep(names(groups), lengths(groups)), unlist(groups, use.names = FALSE))
  if (anyDuplicated(names(lookup))) {
    stop("Each subject can belong to only one group. Duplicated: ",
         paste(unique(names(lookup)[duplicated(names(lookup))]), collapse = ", "))
  }
  params <- attr(data, "parameters")
  data$group <- factor(unname(lookup[data$subject]), levels = names(groups))
  data <- data[!is.na(data$group), , drop = FALSE]
  attr(data, "parameters") <- params
  data
}

#' Summarize Each Animal per Session
#'
#' Collapses the breath-by-breath records of each animal into one value per
#' session (timepoint). Downstream statistics then treat the animal, not the
#' individual breath, as the experimental unit.
#'
#' @param data Output of [read_wbp()], optionally passed through [assign_groups()].
#' @param timepoint How to define sessions: `"phase"` uses the FinePointe Phase
#'   label (recommended); `"date"` uses the calendar date. Falls back to `"date"`
#'   when there are no Phase labels.
#' @param stat Per-session summary: `"median"` (recommended; robust to outlier
#'   breaths) or `"mean"`.
#' @param rinx_max Optional quality filter: drop records whose rejection index
#'   (`Rinx`, percent) is above this value. `NULL` keeps all records.
#'
#' @return A data frame with one row per animal and timepoint: `subject`, `group`
#'   (if assigned), `timepoint` (factor in chronological order), `day` (days since
#'   that animal's first session), `start`, `n_records`, and one column per parameter.
#' @export
summarize_sessions <- function(data, timepoint = c("phase", "date"),
                               stat = c("median", "mean"), rinx_max = NULL) {
  timepoint <- match.arg(timepoint)
  stat <- match.arg(stat)
  params <- attr(data, "parameters")
  if (is.null(params)) params <- setdiff(names(data)[vapply(data, is.numeric, logical(1))], wbp_qc_columns)
  qc <- setdiff(intersect(wbp_qc_columns, names(data)), params)

  if (timepoint == "phase" && all(is.na(data$Phase))) timepoint <- "date"
  data$timepoint <- if (timepoint == "phase") data$Phase else format(as.Date(data$Time), "%Y-%m-%d")
  data <- data[!is.na(data$timepoint), , drop = FALSE]

  if (!is.null(rinx_max) && "Rinx" %in% names(data)) {
    data <- data[is.na(data$Rinx) | data$Rinx <= rinx_max, , drop = FALSE]
  }

  f <- if (stat == "median") stats::median else base::mean
  by <- intersect(c("subject", "group", "timepoint"), names(data))
  sessions <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dplyr::summarize(
      start = min(.data$Time),
      n_records = dplyr::n(),
      dplyr::across(dplyr::all_of(c(params, qc)), ~ {
        v <- f(.x, na.rm = TRUE)
        if (is.nan(v)) NA_real_ else v
      }),
      .groups = "drop"
    )

  # Chronological order of timepoints (by typical start time across animals).
  order_time <- tapply(as.numeric(sessions$start), sessions$timepoint, stats::median)
  sessions$timepoint <- factor(sessions$timepoint, levels = names(sort(order_time)))

  first <- tapply(as.numeric(as.Date(sessions$start)), sessions$subject, min)
  sessions$day <- as.numeric(as.Date(sessions$start)) - unname(first[sessions$subject])

  sessions <- sessions[order(sessions$subject, sessions$timepoint), c(by, "day", "start", "n_records", params, qc)]
  sessions <- as.data.frame(sessions)
  attr(sessions, "parameters") <- params
  attr(sessions, "settings") <- list(timepoint = timepoint, stat = stat, rinx_max = rinx_max)
  sessions
}

#' Express Session Values Relative to Baseline
#'
#' @param sessions Output of [summarize_sessions()].
#' @param method `"none"`, `"percent"` (value as a percentage of the animal's own baseline,
#'   so 100 = no change) or `"difference"` (value minus baseline).
#' @param baseline Timepoint used as baseline. Defaults to the first timepoint.
#' @return `sessions` with transformed parameter columns. Animals without a
#'   baseline value get `NA`.
#' @export
apply_baseline <- function(sessions, method = c("none", "percent", "difference"), baseline = NULL) {
  method <- match.arg(method)
  if (method == "none") return(sessions)
  params <- attr(sessions, "parameters")
  if (is.null(baseline)) baseline <- levels(sessions$timepoint)[1]

  base_rows <- sessions[sessions$timepoint == baseline, c("subject", params), drop = FALSE]
  idx <- match(sessions$subject, base_rows$subject)
  for (p in params) {
    b <- base_rows[[p]][idx]
    sessions[[p]] <- if (method == "percent") {
      ifelse(is.na(b) | b == 0, NA_real_, sessions[[p]] / b * 100)
    } else {
      sessions[[p]] - b
    }
  }
  attr(sessions, "parameters") <- params
  attr(sessions, "baseline") <- list(method = method, timepoint = baseline)
  sessions
}

#' Group Mean, SD and SEM at Each Timepoint
#'
#' @param sessions Output of [summarize_sessions()] with groups assigned.
#' @param parameters Parameters to include. Defaults to all.
#' @return A long data frame: `group`, `timepoint`, `day` (mean across animals),
#'   `parameter`, `mean`, `sd`, `sem`, `n` (animals with a value).
#' @export
summarize_groups <- function(sessions, parameters = NULL) {
  if (is.null(parameters)) parameters <- attr(sessions, "parameters")
  long <- tidyr::pivot_longer(sessions, dplyr::all_of(parameters),
                              names_to = "parameter", values_to = "value")
  long %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::group_by(.data$group, .data$timepoint, .data$parameter) %>%
    dplyr::summarize(
      day = mean(.data$day),
      mean = mean(.data$value),
      sd = if (dplyr::n() > 1) stats::sd(.data$value) else NA_real_,
      n = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(sem = .data$sd / sqrt(.data$n)) %>%
    dplyr::select("group", "timepoint", "day", "parameter", "mean", "sd", "sem", "n") %>%
    as.data.frame()
}

#' One Summary Value per Animal
#'
#' Reduces each animal's time course to a single number per parameter, for
#' group comparisons and PCA.
#'
#' @param sessions Output of [summarize_sessions()] with groups assigned.
#' @param parameters Parameters to include. Defaults to all.
#' @param metric `"auc"` (area under the curve over study days, trapezoidal rule),
#'   `"mean"` (time-weighted average over the window, i.e. AUC / duration, in the
#'   parameter's own units), `"max"`, `"min"`, or `"value"` (the value at a single
#'   timepoint, given by `from`).
#' @param from,to First and last timepoint of the window. Default: all timepoints.
#'
#' @return A long data frame: `subject`, `group`, `parameter`, `value`,
#'   `n_timepoints` (timepoints with data in the window).
#' @export
summarize_subjects <- function(sessions, parameters = NULL,
                               metric = c("auc", "mean", "max", "min", "value"),
                               from = NULL, to = NULL) {
  metric <- match.arg(metric)
  if (is.null(parameters)) parameters <- attr(sessions, "parameters")
  lv <- levels(sessions$timepoint)
  if (is.null(from)) from <- lv[1]
  if (is.null(to)) to <- lv[length(lv)]
  if (metric == "value") to <- from
  window <- lv[match(from, lv):match(to, lv)]

  sub <- sessions[sessions$timepoint %in% window, , drop = FALSE]
  long <- tidyr::pivot_longer(sub, dplyr::all_of(parameters),
                              names_to = "parameter", values_to = "value")
  long <- long[!is.na(long$value), , drop = FALSE]

  trapezoid <- function(x, y) {
    o <- order(x)
    x <- x[o]
    y <- y[o]
    if (length(x) < 2) return(NA_real_)
    sum(diff(x) * (y[-1] + y[-length(y)]) / 2)
  }

  long %>%
    dplyr::group_by(.data$subject, .data$group, .data$parameter) %>%
    dplyr::summarize(
      value = switch(metric,
        auc = trapezoid(.data$day, .data$value),
        mean = {
          span <- diff(range(.data$day))
          if (span > 0) trapezoid(.data$day, .data$value) / span else mean(.data$value)
        },
        max = max(.data$value),
        min = min(.data$value),
        value = .data$value[1]
      ),
      n_timepoints = dplyr::n(),
      .groups = "drop"
    ) %>%
    as.data.frame()
}

# Significance stars for adjusted p-values.
p_stars <- function(p) {
  out <- ifelse(p < 0.0001, "****", ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "ns"))))
  out[is.na(p)] <- ""
  out
}

# Pairwise test between two numeric vectors; NA when either group has < 2 values.
# 95% confidence interval of (b - a): Welch t interval, or the Hodges-Lehmann shift for rank tests.
pair_ci <- function(a, b, test) {
  a <- a[!is.na(a)]
  b <- b[!is.na(b)]
  if (length(a) < 2 || length(b) < 2 || stats::sd(c(a, b)) == 0) return(c(NA_real_, NA_real_))
  ci <- tryCatch(suppressWarnings(
    if (test == "parametric") stats::t.test(b, a)$conf.int
    else stats::wilcox.test(b, a, conf.int = TRUE, exact = !anyDuplicated(c(a, b)))$conf.int
  ), error = function(e) c(NA_real_, NA_real_))
  as.numeric(ci[1:2])
}

pair_test <- function(a, b, test) {
  a <- a[!is.na(a)]
  b <- b[!is.na(b)]
  if (length(a) < 2 || length(b) < 2) return(NA_real_)
  if (stats::sd(c(a, b)) == 0) return(NA_real_)
  suppressWarnings(
    if (test == "parametric") stats::t.test(a, b)$p.value
    else stats::wilcox.test(a, b, exact = !anyDuplicated(c(a, b)))$p.value
  )
}

#' Compare Groups on One Value per Animal
#'
#' Runs an omnibus test across groups and pairwise comparisons, separately for
#' each parameter, with the animal as the unit of analysis.
#'
#' @param values Long data frame with columns `group`, `parameter`, `value`
#'   (e.g. output of [summarize_subjects()]).
#' @param reference Optional reference (control) group. If given, every other
#'   group is compared with it; otherwise all pairs are compared.
#' @param test `"parametric"`: Welch's ANOVA and Welch's t-tests (do not assume
#'   equal variances; recommended for small groups). `"nonparametric"`:
#'   Kruskal-Wallis and Mann-Whitney tests.
#' @param p_adjust Multiple comparison correction applied to the pairwise tests
#'   within each parameter; any method of [stats::p.adjust()]. Default `"holm"`.
#'
#' @return A data frame with one row per parameter and pair: `parameter`,
#'   `group1`, `group2`, `n1`, `n2`, `mean1`, `mean2`, `difference`,
#'   `ci_low`, `ci_high` (95% CI of the difference: Welch interval, or Hodges-Lehmann
#'   for rank tests), `pct_difference`, `pct_ci_low`, `pct_ci_high` (relative to
#'   group1's mean), `p`, `p_adj`, `stars`, and `p_omnibus`.
#' @export
compare_groups <- function(values, reference = NULL,
                           test = c("parametric", "nonparametric"), p_adjust = "holm") {
  test <- match.arg(test)
  values$group <- droplevels(as.factor(values$group))
  groups <- levels(values$group)
  if (length(groups) < 2) stop("At least two groups are needed for comparisons.")
  if (!is.null(reference) && !reference %in% groups) reference <- NULL

  pairs <- if (is.null(reference)) {
    utils::combn(groups, 2, simplify = FALSE)
  } else {
    lapply(setdiff(groups, reference), function(g) c(reference, g))
  }

  res <- lapply(split(values, values$parameter), function(d) {
    d <- d[!is.na(d$value), , drop = FALSE]
    present <- names(which(table(d$group) >= 2))
    p_omnibus <- NA_real_
    if (length(present) >= 3) {
      dd <- d[d$group %in% present, , drop = FALSE]
      dd$group <- droplevels(dd$group)
      p_omnibus <- tryCatch(suppressWarnings(
        if (test == "parametric") stats::oneway.test(value ~ group, data = dd)$p.value
        else stats::kruskal.test(value ~ group, data = dd)$p.value
      ), error = function(e) NA_real_)
    }
    rows <- lapply(pairs, function(pr) {
      a <- d$value[d$group == pr[1]]
      b <- d$value[d$group == pr[2]]
      data.frame(
        parameter = d$parameter[1], group1 = pr[1], group2 = pr[2],
        n1 = length(a), n2 = length(b),
        mean1 = if (length(a)) mean(a) else NA_real_,
        mean2 = if (length(b)) mean(b) else NA_real_,
        p = pair_test(a, b, test),
        ci_low = pair_ci(a, b, test)[1],
        ci_high = pair_ci(a, b, test)[2],
        stringsAsFactors = FALSE
      )
    })
    out <- do.call(rbind, rows)
    out$p_adj <- stats::p.adjust(out$p, method = p_adjust)
    out$p_omnibus <- p_omnibus
    out
  })

  out <- do.call(rbind, res)
  rownames(out) <- NULL
  out$difference <- out$mean2 - out$mean1
  out$pct_difference <- ifelse(out$mean1 == 0, NA_real_, out$difference / abs(out$mean1) * 100)
  out$pct_ci_low <- ifelse(out$mean1 == 0, NA_real_, out$ci_low / abs(out$mean1) * 100)
  out$pct_ci_high <- ifelse(out$mean1 == 0, NA_real_, out$ci_high / abs(out$mean1) * 100)
  out$stars <- p_stars(out$p_adj)
  out[, c("parameter", "group1", "group2", "n1", "n2", "mean1", "mean2",
          "difference", "ci_low", "ci_high", "pct_difference", "pct_ci_low", "pct_ci_high",
          "p", "p_adj", "stars", "p_omnibus")]
}

#' Compare Groups with a Reference Group at Each Timepoint
#'
#' @param sessions Output of [summarize_sessions()] with groups assigned.
#' @param parameters Parameters to test. Defaults to all.
#' @param reference Reference (control) group.
#' @param test `"parametric"` (Welch's t-test) or `"nonparametric"` (Mann-Whitney).
#' @param p_adjust Correction applied across all timepoints and groups within each
#'   parameter. Default `"holm"`.
#' @return A data frame: `parameter`, `timepoint`, `group`, `p`, `p_adj`, `stars`.
#' @export
compare_timepoints <- function(sessions, parameters = NULL, reference,
                               test = c("parametric", "nonparametric"), p_adjust = "holm") {
  test <- match.arg(test)
  if (is.null(parameters)) parameters <- attr(sessions, "parameters")
  others <- setdiff(levels(droplevels(as.factor(sessions$group))), reference)

  out <- lapply(parameters, function(p) {
    rows <- list()
    for (tp in levels(sessions$timepoint)) {
      s <- sessions[sessions$timepoint == tp, , drop = FALSE]
      ref <- s[[p]][s$group == reference]
      for (g in others) {
        rows[[length(rows) + 1]] <- data.frame(
          parameter = p, timepoint = tp, group = g,
          p = pair_test(ref, s[[p]][s$group == g], test),
          stringsAsFactors = FALSE
        )
      }
    }
    r <- do.call(rbind, rows)
    if (is.null(r)) return(NULL)
    r$p_adj <- stats::p.adjust(r$p, method = p_adjust)
    r
  })
  out <- do.call(rbind, out)
  if (is.null(out)) return(data.frame())
  out$timepoint <- factor(out$timepoint, levels = levels(sessions$timepoint))
  out$stars <- p_stars(out$p_adj)
  out
}
