test_that("suggest_groups strips animal numbers", {
  expect_equal(suggest_groups(c("Infected WT1", "Infected WT2", "Control 3", "7")), c("Infected WT", "Infected WT", "Control", NA))
})

test_that("sessions are summarized per animal in chronological order", {
  d <- make_wbp()
  s <- summarize_sessions(assign_groups(d, make_groups(d)))
  expect_equal(levels(s$timepoint), c("Baseline", "Day 3", "Day 7", "Day 14"))
  expect_equal(nrow(s), 20 * 4)
  expect_true(all(s$n_records == 60))
  one <- d[d$subject == "Infected WT1" & d$Phase == "Day 7", ]
  expect_equal(s$Penh[s$subject == "Infected WT1" & s$timepoint == "Day 7"], stats::median(one$Penh))
  expect_equal(as.numeric(unique(s$day[s$timepoint == "Day 14"])), 14)
})

test_that("assign_groups drops unassigned animals and rejects duplicates", {
  d <- make_wbp()
  g <- make_groups(d)
  g$`Control WT` <- g$`Control WT`[-1]
  a <- assign_groups(d, g)
  expect_false("Control WT1" %in% a$subject)
  expect_error(assign_groups(d, list(A = "Control WT1", B = "Control WT1")), "only one group")
})

test_that("Rinx filter removes records", {
  d <- make_wbp()
  s_all <- summarize_sessions(d)
  s_f <- summarize_sessions(d, rinx_max = 50)
  expect_true(all(s_f$n_records < s_all$n_records))
})

test_that("baseline percent is 100 at baseline", {
  d <- make_wbp()
  s <- apply_baseline(summarize_sessions(assign_groups(d, make_groups(d))), "percent")
  expect_true(all(abs(s$Penh[s$timepoint == "Baseline"] - 100) < 1e-9))
})

test_that("AUC uses the trapezoidal rule over study days", {
  s <- data.frame(subject = "A", group = factor("G"), timepoint = factor(c("t1", "t2", "t3"), levels = c("t1", "t2", "t3")),
                  day = c(0, 2, 6), Penh = c(1, 3, 3))
  attr(s, "parameters") <- "Penh"
  v <- summarize_subjects(s, "Penh", "auc")
  expect_equal(v$value, (1 + 3) / 2 * 2 + 3 * 4)
  expect_equal(summarize_subjects(s, "Penh", "mean")$value, ((1 + 3) / 2 * 2 + 3 * 4) / 6)
  expect_equal(summarize_subjects(s, "Penh", "max")$value, 3)
  expect_equal(summarize_subjects(s, "Penh", "value", from = "t2")$value, 3)
})

test_that("compare_groups matches Welch's t-test and its confidence interval", {
  d <- make_wbp()
  s <- summarize_sessions(assign_groups(d, make_groups(d)))
  v <- summarize_subjects(s, "Penh", "auc")
  cg <- compare_groups(v, reference = "Control WT", p_adjust = "none")
  a <- v$value[v$group == "Control WT"]; b <- v$value[v$group == "Infected WT"]
  tt <- stats::t.test(b, a)
  row <- cg[cg$group2 == "Infected WT", ]
  expect_equal(row$p, tt$p.value)
  expect_equal(c(row$ci_low, row$ci_high), as.numeric(tt$conf.int))
  expect_lt(row$p, 0.001)
  expect_equal(nrow(cg), 3)
  expect_true(all(c("pct_ci_low", "pct_ci_high", "p_adj", "stars") %in% names(cg)))
})

test_that("compare_timepoints finds the infection effect after baseline only", {
  d <- make_wbp()
  s <- summarize_sessions(assign_groups(d, make_groups(d)))
  tp <- compare_timepoints(s, "Penh", reference = "Control WT")
  expect_equal(nrow(tp), 4 * 3)
  inf <- tp[tp$group == "Infected WT", ]
  expect_gt(inf$p[inf$timepoint == "Baseline"], 0.01)
  expect_true(all(inf$p_adj[inf$timepoint != "Baseline"] < 0.05))
})

test_that("plots build without error", {
  d <- make_wbp()
  s <- summarize_sessions(assign_groups(d, make_groups(d)))
  gs <- summarize_groups(s)
  v <- summarize_subjects(s)
  cg <- compare_groups(v, reference = "Control WT")
  cols <- plethr_colors(names(make_groups(d)))
  for (p in list(plot_timecourse(gs, "Penh", s, cols, show_individuals = TRUE, stats = compare_timepoints(s, reference = "Control WT")),
                 plot_group_comparison(v, "Penh", cg, cols), plot_difference_heatmap(cg), plot_subject_pca(v, cols)$plot,
                 plot_dashboard(gs, colors = cols), plot_animal_heatmap(s, "Penh"), plot_correlation(s),
                 plot_group_profile(v, "Control WT", colors = cols), plot_effect_forest(cg, colors = cols),
                 plot_trajectory(gs, "f", "TVb", s, cols), plot_waterfall(s, "Penh", colors = cols),
                 plot_animal_trends(s, "Penh", colors = cols))) {
    expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  }
})
