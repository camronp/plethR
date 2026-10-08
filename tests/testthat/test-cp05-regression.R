# Regression tests on the real CP05 data. They run only when CP05.xlsx is in the
# package folder (it is not distributed), and protect published numbers from
# silent changes in future versions.

cp05_path <- function() {
  for (p in c(file.path("..", "..", "CP05.xlsx"), file.path("..", "..", "..", "CP05.xlsx"))) if (file.exists(p)) return(p)
  NULL
}

test_that("CP05 is read and summarized as before", {
  f <- cp05_path()
  skip_if(is.null(f), "CP05.xlsx not available")
  w <- read_wbp(f)
  expect_equal(length(unique(w$subject)), 16)
  expect_equal(nrow(w), 157766)
  g <- split(unique(w$subject), factor(suggest_groups(unique(w$subject)), levels = unique(suggest_groups(unique(w$subject)))))
  s <- summarize_sessions(assign_groups(w, g))
  expect_equal(nlevels(s$timepoint), 17)
  expect_equal(levels(s$timepoint)[c(1, 2, 17)], c("Pre-Infection", "Week 1", "Week 8.5"))
  v <- summarize_subjects(s, "Penh", "auc")
  cg <- compare_groups(v, reference = "Uninfected WT")
  row <- cg[cg$group2 == "Uninfected BENaC", ]
  expect_equal(row$p, 0.01822, tolerance = 1e-3)
  expect_equal(row$pct_difference, 23.99, tolerance = 1e-3)
  # Published descriptive change (mean summary): Uninfected WT TVb +28.4% first to last session
  sm <- summarize_sessions(assign_groups(w, g), stat = "mean")
  gm <- summarize_groups(sm, "TVb")
  x <- gm[gm$group == "Uninfected WT", ]
  x <- x[order(x$timepoint), ]
  expect_equal(round(100 * (x$mean[nrow(x)] / x$mean[1] - 1), 1), 28.4)
})
