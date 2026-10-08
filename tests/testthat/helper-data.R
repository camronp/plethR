# Synthetic FinePointe-like data with known structure, in the format of read_wbp():
# 4 groups x n animals, sessions "Baseline", "Day 3", "Day 7", "Day 14" on known dates,
# 60 records per session. Infected animals have higher Penh and lower TVb after baseline.
make_wbp <- function(n = 5, effect = 0.5, seed = 1, records = 60) {
  set.seed(seed)
  groups <- c("Control WT", "Control KO", "Infected WT", "Infected KO")
  phases <- c("Baseline", "Day 3", "Day 7", "Day 14")
  days <- c(0, 3, 7, 14)
  rows <- list()
  for (g in groups) for (i in seq_len(n)) {
    subj <- paste0(g, i)
    inf <- grepl("^Infected", g)
    animal_offset <- stats::rnorm(1, 0, 0.05)
    for (k in seq_along(phases)) {
      t0 <- as.POSIXct("2024-01-01 10:00:00", tz = "UTC") + days[k] * 86400 + i * 1500
      post <- days[k] > 0
      penh <- 0.6 + animal_offset + if (inf && post) effect else 0
      tvb <- 0.30 - if (inf && post) 0.05 else 0
      rows[[length(rows) + 1]] <- data.frame(
        subject = subj, sheet = paste0(subj, ".WBPth"), Time = t0 + seq(0, by = 2, length.out = records),
        Phase = phases[k], f = stats::rnorm(records, 400, 20), TVb = stats::rnorm(records, tvb, 0.01),
        Penh = stats::rnorm(records, penh, 0.05), Rinx = stats::runif(records, 0, 100), stringsAsFactors = FALSE)
    }
  }
  d <- do.call(rbind, rows)
  attr(d, "parameters") <- c("f", "TVb", "Penh")
  d
}

make_groups <- function(d) {
  s <- unique(d$subject)
  split(s, factor(sub("[0-9]+$", "", s), levels = c("Control WT", "Control KO", "Infected WT", "Infected KO")))
}
