# Within-session breathing variability and spectral features.
#
# FinePointe records are averages over ~2 s, so these features describe how
# breathing drifts and fluctuates over seconds to minutes within a session (not
# breath-to-breath variability). Each feature is computed per animal and session
# and can be analyzed like any respiratory parameter.

variability_features <- c(cv = "variability (robust CV)", ac1 = "smoothness (lag-1 autocorrelation)",
                          sampen = "irregularity (sample entropy)", slow = "slow-fluctuation power (2-10 min)",
                          mid = "mid-fluctuation power (30 s-2 min)", fast = "fast-fluctuation power (4-30 s)",
                          slope = "spectral slope")

#' Breathing Variability Feature Types
#'
#' @return A named character vector: feature codes and their descriptions.
#' @examples
#' variability_feature_types()
#' @export
variability_feature_types <- function() variability_features

#' Readable Name for a Parameter or Variability Feature
#'
#' @param parameter A parameter (`"TVb"`) or variability feature (`"TVb_ac1"`).
#' @return A character string, e.g. `"Tidal volume"` or `"TVb: smoothness (lag-1 autocorrelation)"`.
#' @export
wbp_feature_name <- function(parameter) {
  info <- wbp_parameter_info()
  vapply(parameter, function(p) {
    m <- regmatches(p, regexec(paste0("^(.+)_(", paste(names(variability_features), collapse = "|"), ")$"), p))[[1]]
    if (length(m) == 3) return(paste0(m[2], ": ", variability_features[[m[3]]]))
    nm <- info$name[match(p, info$parameter)]
    if (is.na(nm)) p else nm
  }, character(1), USE.NAMES = FALSE)
}

is_variability_feature <- function(parameter) {
  grepl(paste0("_(", paste(names(variability_features), collapse = "|"), ")$"), parameter)
}

sample_entropy <- function(x, m = 2, r = 0.2 * stats::sd(x)) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 50 || !is.finite(r) || r == 0) return(NA_real_)
  count <- function(mm) {
    emb <- stats::embed(x, mm)[, mm:1, drop = FALSE]
    emb <- emb[seq_len(n - m), , drop = FALSE]
    d <- as.matrix(stats::dist(emb, method = "maximum"))
    (sum(d <= r) - nrow(emb)) / 2
  }
  B <- count(m)
  A <- count(m + 1)
  if (A == 0 || B == 0) return(NA_real_)
  -log(A / B)
}

one_series_features <- function(t, y, features) {
  ok <- !is.na(y) & !is.na(t)
  t <- t[ok]; y <- y[ok]
  out <- stats::setNames(rep(NA_real_, length(features)), features)
  if (length(y) < 100 || stats::sd(y) == 0) return(out)
  grid <- seq(min(t), max(t), by = 2)
  yy <- stats::approx(t, y, xout = grid, ties = mean)$y
  if ("cv" %in% features) out["cv"] <- stats::mad(y) / abs(stats::median(y))
  if ("ac1" %in% features) out["ac1"] <- stats::acf(yy, lag.max = 1, plot = FALSE)$acf[2]
  if ("sampen" %in% features) out["sampen"] <- sample_entropy(yy[seq_len(min(600, length(yy)))])
  if (any(c("slow", "mid", "fast", "slope") %in% features)) {
    sp <- stats::spec.pgram(stats::ts(yy, frequency = 0.5), taper = 0.1, detrend = TRUE, plot = FALSE)
    fr <- sp$freq; pw <- sp$spec
    band <- function(lo, hi) sum(pw[fr >= lo & fr < hi])
    tot <- band(1 / 600, 0.25)
    if (tot > 0) {
      if ("slow" %in% features) out["slow"] <- band(1 / 600, 1 / 120) / tot
      if ("mid" %in% features) out["mid"] <- band(1 / 120, 1 / 30) / tot
      if ("fast" %in% features) out["fast"] <- band(1 / 30, 0.25) / tot
    }
    keep <- fr >= 1 / 600 & pw > 0
    if ("slope" %in% features && sum(keep) > 5) out["slope"] <- unname(stats::coef(stats::lm(log10(pw[keep]) ~ log10(fr[keep])))[2])
  }
  out
}

#' Breathing Variability Features per Session
#'
#' Computes, for each animal and session, how each chosen parameter fluctuates
#' within the session: robust coefficient of variation, lag-1 autocorrelation
#' (smoothness), sample entropy (irregularity), the share of fluctuation power in
#' slow (2-10 min), mid (30 s-2 min) and fast (4-30 s) bands, and the spectral
#' slope. Records are placed on a regular 2-second grid (short gaps interpolated).
#'
#' Sessions are defined as in [summarize_sessions()], so the result can be joined
#' with [add_variability()].
#'
#' @param data Output of [read_wbp()].
#' @param parameters Parameters to describe.
#' @param features Feature codes from [variability_feature_types()].
#' @param timepoint,rinx_max As in [summarize_sessions()].
#' @param progress Optional function called as `progress(i, n)`.
#' @return A data frame with `subject`, `timepoint` and one column per parameter and
#'   feature, named like `TVb_ac1`.
#' @export
session_variability <- function(data, parameters = c("TVb", "MVb", "PIFb", "f", "Penh"),
                                features = c("ac1", "sampen", "slow"), timepoint = c("phase", "date"),
                                rinx_max = NULL, progress = NULL) {
  timepoint <- match.arg(timepoint)
  features <- intersect(features, names(variability_features))
  parameters <- intersect(parameters, names(data))
  if (!length(features) || !length(parameters)) stop("Choose at least one parameter and one feature type.")
  if (timepoint == "phase" && all(is.na(data$Phase))) timepoint <- "date"
  data$timepoint <- if (timepoint == "phase") data$Phase else format(as.Date(data$Time), "%Y-%m-%d")
  data <- data[!is.na(data$timepoint), , drop = FALSE]
  if (!is.null(rinx_max) && "Rinx" %in% names(data)) data <- data[is.na(data$Rinx) | data$Rinx <= rinx_max, , drop = FALSE]
  keys <- unique(data[, c("subject", "timepoint")])
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    if (is.function(progress)) progress(i, nrow(keys))
    a <- data[data$subject == keys$subject[i] & data$timepoint == keys$timepoint[i], , drop = FALSE]
    a <- a[order(a$Time), , drop = FALSE]
    tt <- as.numeric(a$Time)
    v <- unlist(lapply(parameters, function(p) {
      f <- one_series_features(tt, a[[p]], features)
      stats::setNames(f, paste(p, names(f), sep = "_"))
    }))
    data.frame(subject = keys$subject[i], timepoint = keys$timepoint[i], t(v), check.names = FALSE, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  attr(out, "variability") <- list(parameters = parameters, features = features)
  out
}

#' Add Variability Features to Session Data
#'
#' Joins the output of [session_variability()] to [summarize_sessions()] output so
#' the features can be analyzed like respiratory parameters.
#'
#' @param sessions Output of [summarize_sessions()].
#' @param variability Output of [session_variability()].
#' @return `sessions` with the feature columns added and listed in attribute `"parameters"`.
#' @export
add_variability <- function(sessions, variability) {
  feats <- setdiff(names(variability), c("subject", "timepoint"))
  idx <- match(paste(sessions$subject, as.character(sessions$timepoint)), paste(variability$subject, variability$timepoint))
  params <- attr(sessions, "parameters")
  settings <- attr(sessions, "settings")
  for (f in feats) sessions[[f]] <- variability[[f]][idx]
  attr(sessions, "parameters") <- c(params, feats)
  attr(sessions, "settings") <- c(settings, list(variability = attr(variability, "variability")))
  sessions
}
