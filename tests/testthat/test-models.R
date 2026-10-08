test_that("factorial models detect the infection effect", {
  d <- make_wbp()
  s <- summarize_sessions(assign_groups(d, make_groups(d)))
  des <- suggest_factorial_design(names(make_groups(d)))
  expect_false(any(is.na(des$A)))
  v <- summarize_subjects(s, "Penh", "auc")
  a <- factorial_anova(v, des, factor_names = c("Infection", "Genotype"))
  expect_lt(a$p[a$term == "Infection"], 0.001)
  m <- factorial_mixed_model(s, des, "Penh", c("Infection", "Genotype"))
  expect_lt(m$p[m$term == "Infection \u00d7 Time"], 0.001)
})

test_that("mixed-model time course compares groups at each timepoint", {
  skip_if_not_installed("emmeans")
  d <- make_wbp()
  s <- summarize_sessions(assign_groups(d, make_groups(d)))
  mt <- mixed_timecourse(s, "Penh", "Control WT", covariates = "baseline")
  expect_true(all(c("anova", "means", "contrasts") %in% names(mt)))
  inf <- mt$contrasts[mt$contrasts$group == "Infected WT", ]
  expect_equal(nrow(inf), 3)
  expect_true(all(inf$p_adj < 0.05))
  expect_true(all(inf$pct_lower > 0))
})

test_that("sample size planning agrees with power.t.test", {
  v <- data.frame(subject = paste0("a", 1:8), group = rep(c("Ref", "Trt"), each = 4), parameter = "Penh",
                  value = c(10, 11, 9, 10, 13, 14, 12, 13))
  pl <- sample_size_plan(v, "Ref", power = 0.8)
  sdp <- sqrt((stats::var(v$value[1:4]) + stats::var(v$value[5:8])) / 2)
  expect_equal(pl$n_per_group_needed, ceiling(stats::power.t.test(delta = 3, sd = sdp, power = 0.8)$n))
  pl2 <- sample_size_plan(v, "Ref", effect_pct = 10, n_comparisons = 2)
  expect_equal(pl2$planned_pct, 10)
  expect_gt(pl2$n_per_group_needed, pl$n_per_group_needed)
})

test_that("lung health score is higher in diseased animals and treatment efficacy runs", {
  d <- make_wbp()
  s <- summarize_sessions(assign_groups(d, make_groups(d)))
  sc <- lung_health_score(s, healthy = "Control WT", disease = "Infected WT", onset_timepoint = "Day 3")
  post <- sc$timepoint != "Baseline"
  expect_gt(mean(sc$lung_score[post & sc$group == "Infected WT"]), mean(sc$lung_score[post & sc$group == "Control WT"]) + 1)
  ef <- treatment_efficacy(sc, treated = "Control KO", from = "Day 3")
  expect_true(ef$rescue > 80)
  expect_true(ef$verdict$level %in% c("full", "partial"))
})

test_that("animal trends find slopes", {
  s <- data.frame(subject = rep(c("A", "B"), each = 5), group = factor("G"), timepoint = factor(rep(paste0("t", 1:5), 2), levels = paste0("t", 1:5)),
                  day = rep(c(0, 7, 14, 21, 28), 2), y = c(1, 2, 3, 4, 5, 5, 5.01, 4.99, 5, 5))
  tr <- suppressWarnings(animal_trends(s, "y", higher_is = "worse"))
  expect_equal(tr$slope_per_week[tr$subject == "A"], 1)
  expect_equal(tr$direction[tr$subject == "A"], "worsening")
  expect_equal(tr$direction[tr$subject == "B"], "no clear trend")
})

test_that("variability features separate slow from fast fluctuations", {
  t <- seq(0, 1200, by = 2)
  set.seed(1)
  slow <- one_series_features(t, sin(2 * pi * t / 300) + stats::rnorm(length(t), 0, 0.1), c("slow", "fast", "ac1"))
  fast <- one_series_features(t, sin(2 * pi * t / 10) + stats::rnorm(length(t), 0, 0.1), c("slow", "fast", "ac1"))
  expect_gt(slow[["slow"]], 0.5)
  expect_gt(fast[["fast"]], 0.5)
  expect_gt(slow[["ac1"]], fast[["ac1"]])
})
