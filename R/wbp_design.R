# Study design file: animals, groups, exclusions, body weights and bacterial
# burden (CFU) for a study, in one Excel workbook that travels with the data.

design_sheets <- c("Study", "Animals", "Weights", "CFU")

#' Write a Study Design Template
#'
#' Creates an Excel workbook to fill in for a study: study details, one row per
#' animal (group, sex, chamber, exclusions), body weights per session, and
#' bacterial burden (CFU). Pass the animals, groups and sessions of a loaded data
#' set to pre-fill it.
#'
#' @param file Path of the `.xlsx` file to write.
#' @param subjects Animal names (sheet names without `.WBPth`).
#' @param groups Optional group of each animal, same length as `subjects`.
#' @param timepoints Optional session labels, used to pre-fill the Weights sheet.
#' @return `file`, invisibly.
#' @export
write_design_template <- function(file, subjects = character(0), groups = NULL, timepoints = NULL) {
  if (is.null(groups)) groups <- rep("", length(subjects))
  groups[is.na(groups)] <- ""
  study <- data.frame(
    field = c("study_id", "pathogen", "dose", "infection_session", "days_from_infection_to_that_session",
              "treatment_start_session", "notes"),
    value = c("", "", "", if (length(timepoints) > 1) timepoints[2] else "", "0", "", ""),
    description = c("Short name for the study, e.g. CP05",
                    "Organism, e.g. M. abscessus 103",
                    "Dose, e.g. 1e6 CFU/lung",
                    "First WBP session after infection (a Phase label as in the data)",
                    "Days between infection and that session (0 if measured the same day)",
                    "First session after treatment started (leave empty if no treatment)",
                    "Anything else"),
    stringsAsFactors = FALSE)
  animals <- data.frame(animal = subjects, group = groups, sex = "", chamber = "", exclude = "", exclusion_reason = "",
                        notes = "", stringsAsFactors = FALSE)
  weights <- if (length(subjects) && length(timepoints)) {
    expand.grid(animal = subjects, session = timepoints, stringsAsFactors = FALSE)[, c("animal", "session")]
  } else data.frame(animal = character(0), session = character(0))
  weights$weight_g <- NA_real_
  cfu <- data.frame(group = c("Infected WT", "Infected WT"), animal = c("", ""), days_post_infection = c(0, 5),
                    log10_cfu = c(NA_real_, NA_real_), organ = c("lung", "lung"), stringsAsFactors = FALSE)
  instructions <- data.frame(Instructions = c(
    "Fill in the sheets and load this file in step 2 (Groups) of the plethR app, or with read_design() in R.",
    "Study: the infection session and the days between infection and it define days post-infection.",
    "Animals: one row per WBP animal. 'animal' must match the sheet name in the FinePointe export (without .WBPth).",
    "  group: the experimental group. sex: F or M. chamber: optional chamber or box ID.",
    "  exclude: Y to leave the animal out of every analysis; give the reason in exclusion_reason.",
    "Weights: body weight in grams per animal and session (session = Phase label or date YYYY-MM-DD).",
    "  Missing weigh-ins are interpolated within each animal. Used for TVb and MVb per gram and % weight change.",
    "CFU: bacterial burden. One row per measurement with days_post_infection and log10_cfu (or a column 'cfu' with raw counts).",
    "  Fill 'group' for group-level burden from separate harvest cohorts (one row per group and day, or one per harvested animal).",
    "  Fill 'animal' when the CFU belongs to a WBP animal (e.g. terminal burden); it can then be used as a severity outcome.",
    "Delete the example CFU rows before use."))
  writexl::write_xlsx(list(Instructions = instructions, Study = study, Animals = animals, Weights = weights, CFU = cfu), file)
  invisible(file)
}

#' Read a Study Design File
#'
#' Reads a workbook created with [write_design_template()] and checks it against
#' the loaded data.
#'
#' @param file Path of the filled-in `.xlsx` file.
#' @param subjects Optional animal names in the data, used to report mismatches.
#' @return A list of class `plethr_design` with `study` (named list), `animals`,
#'   `weights`, `cfu` (data frames, possibly empty) and `messages` (character).
#' @export
read_design <- function(file, subjects = NULL) {
  sheets <- readxl::excel_sheets(file)
  rd <- function(s) if (s %in% sheets) as.data.frame(readxl::read_excel(file, sheet = s)) else NULL
  norm <- function(d) { if (is.null(d)) return(NULL); names(d) <- tolower(trimws(gsub("[^A-Za-z0-9]+", "_", names(d)))); d }
  msgs <- character(0)

  st <- norm(rd("Study"))
  study <- if (!is.null(st) && all(c("field", "value") %in% names(st))) {
    v <- as.character(st$value); v[is.na(v)] <- ""
    stats::setNames(as.list(trimws(v)), trimws(st$field))
  } else list()

  an <- norm(rd("Animals"))
  if (is.null(an) || !all(c("animal", "group") %in% names(an))) stop("The Animals sheet needs columns 'animal' and 'group'.")
  for (col in c("sex", "chamber", "exclude", "exclusion_reason", "notes")) if (!col %in% names(an)) an[[col]] <- NA
  an$animal <- trimws(as.character(an$animal))
  an$group <- trimws(as.character(an$group))
  an <- an[!is.na(an$animal) & nzchar(an$animal), , drop = FALSE]
  an$exclude <- toupper(substr(trimws(as.character(an$exclude)), 1, 1)) %in% c("Y", "T", "1", "X")
  an$sex <- toupper(substr(trimws(as.character(an$sex)), 1, 1))
  an$sex[!an$sex %in% c("F", "M")] <- NA
  if (any(duplicated(an$animal))) msgs <- c(msgs, paste("Duplicated animals in the Animals sheet:", paste(unique(an$animal[duplicated(an$animal)]), collapse = ", ")))
  if (any(!an$exclude & (is.na(an$group) | !nzchar(an$group)))) msgs <- c(msgs, "Some animals have no group; they are left out.")
  if (!is.null(subjects)) {
    miss <- setdiff(subjects, an$animal)
    extra <- setdiff(an$animal, subjects)
    if (length(miss)) msgs <- c(msgs, paste("In the data but not in the Animals sheet (left out):", paste(miss, collapse = ", ")))
    if (length(extra)) msgs <- c(msgs, paste("In the Animals sheet but not in the data:", paste(extra, collapse = ", ")))
  }

  wt <- norm(rd("Weights"))
  weights <- if (!is.null(wt) && all(c("animal", "weight_g") %in% names(wt))) {
    wt$animal <- trimws(as.character(wt$animal))
    wt$session <- if ("session" %in% names(wt)) trimws(as.character(wt$session)) else NA_character_
    wt$weight_g <- suppressWarnings(as.numeric(wt$weight_g))
    wt[!is.na(wt$weight_g), c("animal", "session", "weight_g"), drop = FALSE]
  } else data.frame(animal = character(0), session = character(0), weight_g = numeric(0))

  cf <- norm(rd("CFU"))
  cfu <- data.frame(group = character(0), animal = character(0), days_post_infection = numeric(0), log10_cfu = numeric(0))
  if (!is.null(cf) && "days_post_infection" %in% names(cf) && any(c("log10_cfu", "cfu") %in% names(cf))) {
    val <- if ("log10_cfu" %in% names(cf)) suppressWarnings(as.numeric(cf$log10_cfu)) else rep(NA_real_, nrow(cf))
    if ("cfu" %in% names(cf)) {
      raw <- suppressWarnings(as.numeric(cf$cfu))
      # Values above 15 cannot be log10 CFU, so treat them as counts.
      is_raw <- is.na(val) & !is.na(raw)
      val[is_raw] <- ifelse(raw[is_raw] > 15, log10(pmax(raw[is_raw], 1)), raw[is_raw])
    }
    cfu <- data.frame(group = if ("group" %in% names(cf)) trimws(as.character(cf$group)) else NA_character_,
                      animal = if ("animal" %in% names(cf)) trimws(as.character(cf$animal)) else NA_character_,
                      days_post_infection = suppressWarnings(as.numeric(cf$days_post_infection)),
                      log10_cfu = val, stringsAsFactors = FALSE)
    cfu$animal[!is.na(cfu$animal) & !nzchar(cfu$animal)] <- NA
    cfu$group[!is.na(cfu$group) & !nzchar(cfu$group)] <- NA
    # Per-animal rows inherit the animal's group.
    cfu$group[is.na(cfu$group) & !is.na(cfu$animal)] <- an$group[match(cfu$animal[is.na(cfu$group) & !is.na(cfu$animal)], an$animal)]
    cfu <- cfu[!is.na(cfu$log10_cfu) & !is.na(cfu$days_post_infection), , drop = FALSE]
  }
  structure(list(study = study, animals = an, weights = weights, cfu = cfu, messages = msgs), class = "plethr_design")
}

#' @export
print.plethr_design <- function(x, ...) {
  cat("plethR study design", if (!is.null(x$study$study_id) && nzchar(x$study$study_id)) paste0("(", x$study$study_id, ")"), "\n")
  a <- x$animals
  cat(sprintf("Animals: %d in %d groups (%d excluded)\n", sum(!a$exclude), length(unique(a$group[!a$exclude])), sum(a$exclude)))
  cat(sprintf("Weights: %d measurements; CFU: %d values (%d per animal)\n", nrow(x$weights), nrow(x$cfu), sum(!is.na(x$cfu$animal))))
  if (length(x$messages)) cat(paste("-", x$messages), sep = "\n")
  invisible(x)
}

#' Groups from a Study Design
#'
#' @param design Output of [read_design()].
#' @return A named list of animals per group (excluded animals left out), in the
#'   order groups first appear in the Animals sheet; usable with [assign_groups()].
#' @export
design_groups <- function(design) {
  a <- design$animals
  a <- a[!a$exclude & !is.na(a$group) & nzchar(a$group), , drop = FALSE]
  split(a$animal, factor(a$group, levels = unique(a$group)))
}

#' Add Body Weight and Weight-Normalized Volumes
#'
#' Adds `Weight` (g), `Weight_change` (% of the animal's first weight) and, for the
#' chosen volume parameters, per-gram versions (e.g. `TVb_per_g`, mL/g). Weights
#' are matched to sessions by Phase label or date, and interpolated linearly in
#' time within each animal for sessions without a weigh-in.
#'
#' @param sessions Output of [summarize_sessions()].
#' @param weights The `weights` element of [read_design()], or a data frame with
#'   `animal`, `session` and `weight_g`.
#' @param parameters Volume parameters to normalize.
#' @return `sessions` with the new columns, added to attribute `"parameters"`.
#' @export
add_body_weight <- function(sessions, weights, parameters = c("TVb", "MVb")) {
  w <- weights[!is.na(weights$weight_g), , drop = FALSE]
  if (nrow(w) == 0) stop("No body weights given.")
  s_date <- format(as.Date(sessions$start), "%Y-%m-%d")
  key_tp <- paste(sessions$subject, as.character(sessions$timepoint))
  key_dt <- paste(sessions$subject, s_date)
  wk <- paste(w$animal, w$session)
  matched <- w$weight_g[match(key_tp, wk)]
  by_date <- w$weight_g[match(key_dt, wk)]
  matched[is.na(matched)] <- by_date[is.na(matched)]
  sessions$Weight <- NA_real_
  for (a in unique(sessions$subject)) {
    i <- which(sessions$subject == a)
    known <- !is.na(matched[i])
    if (sum(known) == 0) next
    sessions$Weight[i] <- if (sum(known) == 1) matched[i][known] else
      stats::approx(sessions$day[i][known], matched[i][known], xout = sessions$day[i], rule = 2, ties = mean)$y
  }
  first <- tapply(seq_len(nrow(sessions)), sessions$subject, function(i) sessions$Weight[i][order(sessions$day[i])][1])
  sessions$Weight_change <- (sessions$Weight / first[sessions$subject] - 1) * 100
  new <- c("Weight", "Weight_change")
  for (p in intersect(parameters, names(sessions))) {
    nm <- paste0(p, "_per_g")
    sessions[[nm]] <- sessions[[p]] / sessions$Weight
    new <- c(new, nm)
  }
  attr(sessions, "parameters") <- unique(c(attr(sessions, "parameters"), new))
  sessions
}

#' Days Post-Infection of Each Session
#'
#' @param sessions Output of [summarize_sessions()].
#' @param design Output of [read_design()] with `infection_session` set.
#' @return Named numeric vector: mean days post-infection of each timepoint.
#' @export
design_dpi <- function(sessions, design) {
  inf <- design$study$infection_session
  if (is.null(inf) || !nzchar(inf) || !inf %in% levels(sessions$timepoint)) return(NULL)
  off <- suppressWarnings(as.numeric(design$study$days_from_infection_to_that_session))
  if (is.na(off)) off <- 0
  s <- add_dpi(sessions, inf, off)
  tapply(s$dpi, s$timepoint, mean)
}

#' Plot Bacterial Burden Alongside Breathing
#'
#' Two panels sharing a days-post-infection axis: the group time course of a
#' breathing parameter, and the group bacterial burden (log10 CFU).
#'
#' @param group_summary Output of [summarize_groups()].
#' @param parameter Breathing parameter.
#' @param cfu The `cfu` element of [read_design()].
#' @param dpi Output of [design_dpi()].
#' @param colors Named group colors.
#' @return A ggplot object.
#' @export
plot_cfu_overlay <- function(group_summary, parameter, cfu, dpi, colors = NULL) {
  if (is.null(dpi)) stop("Set infection_session in the Study sheet to align CFU with breathing.")
  g <- group_summary[group_summary$parameter == parameter, , drop = FALSE]
  g$x <- dpi[as.character(g$timepoint)]
  b <- data.frame(group = as.character(g$group), x = g$x, y = g$mean, lo = g$mean - g$sem, hi = g$mean + g$sem,
                  panel = wbp_axis_label(parameter))
  cs <- stats::aggregate(log10_cfu ~ group + days_post_infection, data = cfu,
                         FUN = function(v) c(m = mean(v), se = if (length(v) > 1) stats::sd(v) / sqrt(length(v)) else 0))
  c2 <- data.frame(group = cs$group, x = cs$days_post_infection, y = cs$log10_cfu[, "m"],
                   lo = cs$log10_cfu[, "m"] - cs$log10_cfu[, "se"], hi = cs$log10_cfu[, "m"] + cs$log10_cfu[, "se"],
                   panel = "Bacterial burden (log10 CFU)")
  d <- rbind(b, c2)
  d$panel <- factor(d$panel, levels = c(wbp_axis_label(parameter), "Bacterial burden (log10 CFU)"))
  if (is.null(colors)) colors <- plethr_colors(unique(d$group))
  d$group <- factor(d$group, levels = unique(c(intersect(names(colors), d$group), d$group)))
  missing_cols <- setdiff(unique(d$group), names(colors))
  if (length(missing_cols)) colors <- c(colors, stats::setNames(rep("grey40", length(missing_cols)), missing_cols))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$y, color = .data$group, fill = .data$group)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lo, ymax = .data$hi), alpha = 0.15, color = NA) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::facet_grid(panel ~ ., scales = "free_y", switch = "y") +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_fill_manual(values = colors, guide = "none") +
    ggplot2::labs(title = paste(wbp_feature_name(parameter), "and bacterial burden"), x = "Days post-infection", y = NULL,
                  caption = "Mean \u00b1 SEM; CFU from the study design file (often separate harvest cohorts); dotted line = infection") +
    theme_plethr(11) +
    ggplot2::theme(strip.placement = "outside", strip.text.y.left = ggplot2::element_text(angle = 90, face = "bold"))
}

#' Correlate Breathing with Bacterial Burden
#'
#' Pairs each CFU measurement (group mean at a day post-infection, or a single
#' animal) with breathing at the nearest session and computes Spearman
#' correlations for every parameter.
#'
#' Group-level pairs (separate harvest cohorts) give an ecological correlation:
#' they show whether breathing and burden change together over time, not whether
#' individual animals with more bacteria breathe differently. Per-animal CFU
#' (CFU from WBP animals) gives an animal-level correlation.
#'
#' @param sessions Output of [summarize_sessions()] with groups assigned.
#' @param cfu The `cfu` element of [read_design()].
#' @param dpi Output of [design_dpi()].
#' @param parameters Parameters to correlate. Defaults to all.
#' @param max_gap Largest allowed gap (days) between a CFU day and a WBP session.
#' @return A list with `pairs` (matched values) and `correlations` (one row per
#'   parameter: rho, p, n, level).
#' @export
cfu_correlation <- function(sessions, cfu, dpi, parameters = NULL, max_gap = 3) {
  if (is.null(dpi)) stop("Set infection_session in the Study sheet to align CFU with breathing.")
  if (is.null(parameters)) parameters <- attr(sessions, "parameters")
  per_animal <- !is.na(cfu$animal)
  level <- if (any(per_animal)) "animal" else "group"
  near_tp <- function(x) {
    k <- which.min(abs(dpi - x))
    if (length(k) && abs(dpi[k] - x) <= max_gap) names(dpi)[k] else NA_character_
  }
  cf <- if (level == "animal") cfu[per_animal, , drop = FALSE] else
    stats::aggregate(log10_cfu ~ group + days_post_infection, data = cfu, FUN = mean)
  cf$timepoint <- vapply(cf$days_post_infection, near_tp, character(1))
  cf <- cf[!is.na(cf$timepoint), , drop = FALSE]
  if (nrow(cf) < 4) stop("Fewer than 4 CFU values fall within ", max_gap, " days of a WBP session.")
  pairs <- do.call(rbind, lapply(parameters, function(p) {
    v <- if (level == "animal") {
      sessions[[p]][match(paste(cf$animal, cf$timepoint), paste(sessions$subject, as.character(sessions$timepoint)))]
    } else {
      m <- stats::aggregate(sessions[[p]], by = list(group = as.character(sessions$group), timepoint = as.character(sessions$timepoint)),
                            FUN = mean, na.rm = TRUE)
      m$x[match(paste(cf$group, cf$timepoint), paste(m$group, m$timepoint))]
    }
    data.frame(parameter = p, group = cf$group, animal = if (level == "animal") cf$animal else NA_character_,
               timepoint = cf$timepoint, days_post_infection = cf$days_post_infection, log10_cfu = cf$log10_cfu, value = v)
  }))
  pairs <- pairs[!is.na(pairs$value), , drop = FALSE]
  cors <- do.call(rbind, lapply(split(pairs, pairs$parameter), function(d) {
    ct <- if (nrow(d) >= 4 && stats::sd(d$value) > 0) suppressWarnings(stats::cor.test(d$value, d$log10_cfu, method = "spearman", exact = FALSE)) else NULL
    data.frame(parameter = d$parameter[1], rho = if (is.null(ct)) NA_real_ else unname(ct$estimate),
               p = if (is.null(ct)) NA_real_ else ct$p.value, n = nrow(d), level = level)
  }))
  cors$p_adj <- stats::p.adjust(cors$p, "BH")
  cors <- cors[order(-abs(cors$rho)), , drop = FALSE]
  rownames(cors) <- NULL
  list(pairs = pairs, correlations = cors, level = level)
}

#' Plot Breathing against Bacterial Burden
#'
#' @param correlation Output of [cfu_correlation()].
#' @param parameter Parameter to plot.
#' @param colors Named group colors.
#' @return A ggplot object.
#' @export
plot_cfu_correlation <- function(correlation, parameter, colors = NULL) {
  d <- correlation$pairs[correlation$pairs$parameter == parameter, , drop = FALSE]
  if (nrow(d) == 0) stop("No matched values for ", parameter, ".")
  r <- correlation$correlations[correlation$correlations$parameter == parameter, ]
  if (is.null(colors)) colors <- plethr_colors(unique(d$group))
  d$group <- factor(d$group, levels = unique(c(intersect(names(colors), d$group), d$group)))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$log10_cfu, y = .data$value, color = .data$group)) +
    ggplot2::geom_smooth(ggplot2::aes(group = 1), method = "lm", formula = y ~ x, se = TRUE, color = "grey45", fill = "grey85", linewidth = 0.7) +
    ggplot2::geom_point(size = 3) +
    ggrepel::geom_text_repel(ggplot2::aes(label = paste0(round(.data$days_post_infection), " d")), size = 3, show.legend = FALSE, seed = 1) +
    ggplot2::scale_color_manual(values = colors[names(colors) %in% d$group]) +
    ggplot2::labs(title = paste(wbp_feature_name(parameter), "vs bacterial burden"), x = "log10 CFU", y = wbp_axis_label(parameter),
                  caption = sprintf("Spearman rho = %.2f, p = %s (n = %d %s); labels = days post-infection%s", r$rho,
                                    format.pval(r$p, digits = 2), r$n, if (correlation$level == "animal") "animals" else "group-days",
                                    if (correlation$level == "group") "; group means, so this is an ecological correlation" else "")) +
    theme_plethr(11)
}
