test_that("study design template round-trips and sets groups, exclusions and weights", {
  d <- make_wbp()
  s0 <- summarize_sessions(d)
  subj <- unique(d$subject)
  f <- tempfile(fileext = ".xlsx")
  write_design_template(f, subj, suggest_groups(subj), levels(s0$timepoint))
  sh <- lapply(stats::setNames(readxl::excel_sheets(f), readxl::excel_sheets(f)), function(x) as.data.frame(readxl::read_excel(f, x)))
  sh$Study$value[sh$Study$field == "infection_session"] <- "Day 3"
  sh$Study$value[sh$Study$field == "days_from_infection_to_that_session"] <- "3"
  sh$Animals$exclude[1] <- "Y"
  sh$Animals$exclusion_reason[1] <- "test"
  sh$Animals$sex <- rep(c("F", "M"), length.out = nrow(sh$Animals))
  w <- sh$Weights
  w$weight_g <- ifelse(w$session == "Day 3", NA, ifelse(w$session == "Baseline", 20, 22))   # Day 3 missing
  sh$Weights <- w
  sh$CFU <- data.frame(group = c("Infected WT", "Infected WT"), animal = c("", ""), days_post_infection = c(3, 7), cfu = c(1e6, 1e4))
  f2 <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(sh, f2)
  des <- read_design(f2, subj)
  expect_s3_class(des, "plethr_design")
  expect_true(des$animals$exclude[1])
  g <- design_groups(des)
  expect_false(subj[1] %in% unlist(g))
  expect_equal(des$cfu$log10_cfu, c(6, 4))
  s <- add_body_weight(summarize_sessions(assign_groups(d, g)), des$weights)
  expect_true(all(c("Weight", "Weight_change", "TVb_per_g") %in% attr(s, "parameters")))
  # Day 3 is interpolated between day 0 (20 g) and day 7 (22 g)
  expect_equal(unique(round(s$Weight[s$timepoint == "Day 3"], 6)), round(20 + 2 * 3 / 7, 6))
  dpi <- design_dpi(s, des)
  expect_equal(unname(dpi["Day 3"]), 3)
  expect_equal(unname(dpi["Baseline"]), 0)
})

test_that("machine learning prepares infection, phase and severity data", {
  d <- make_wbp()
  s <- summarize_sessions(assign_groups(d, make_groups(d)))
  inf <- c("Infected WT", "Infected KO"); ctl <- c("Control WT", "Control KO")
  di <- ml_prepare_infection(s, inf, ctl, "Day 3")
  expect_false("Baseline" %in% di$timepoint)
  expect_setequal(levels(di$outcome), c("Infected", "Uninfected"))
  dp <- ml_prepare_phase(s, inf, "Day 3", acute_days = 4)
  expect_equal(sort(unique(as.character(dp$outcome[dp$timepoint == "Day 3"]))), "Acute")
  sev <- data.frame(subject = unique(s$subject), value = seq_along(unique(s$subject)))
  ds <- ml_prepare_severity(s, sev)
  expect_true(is.numeric(ds$outcome))
})

test_that("models separate a strong effect when animals are held out", {
  skip_on_cran()
  skip_if_not_installed("parsnip"); skip_if_not_installed("tune"); skip_if_not_installed("themis")
  d <- make_wbp(n = 6, effect = 1)
  s <- summarize_sessions(assign_groups(d, make_groups(d)))
  f <- suppressWarnings(ml_fit(ml_prepare_infection(s, c("Infected WT", "Infected KO"), c("Control WT", "Control KO"), "Day 3"),
              models = "log", seed = 1))
  expect_gt(f$metrics$roc_auc[f$metrics$level == "animal"], 0.9)
})

test_that("study library stores studies and validates across them", {
  skip_on_cran()
  skip_if_not_installed("parsnip"); skip_if_not_installed("tune"); skip_if_not_installed("themis")
  lib <- tempfile("plethr_lib")
  conds <- c("Control WT" = "Uninfected", "Control KO" = "Uninfected", "Infected WT" = "Infected", "Infected KO" = "Infected")
  for (k in 1:2) {
    d <- make_wbp(n = 5, effect = 1, seed = k)
    s <- summarize_sessions(assign_groups(d, make_groups(d)))
    library_add_study(lib, s, paste0("S", k), conds, infection_timepoint = "Day 3")
  }
  expect_equal(nrow(library_list(lib)), 2)
  expect_error(library_add_study(lib, s, "S1", conds), "already in the library")
  ld <- library_load(lib)
  expect_equal(length(unique(ld$subject)), 40)
  f <- suppressWarnings(ml_fit(ml_prepare_library(ld, "infection"), models = "log", validation = "study", seed = 1))
  sm <- ml_study_metrics(f)
  expect_equal(sort(sm$study), c("S1", "S2"))
  expect_true(all(sm$animal_roc_auc > 0.9))
  id <- library_save_model(lib, f, "test")
  expect_equal(nrow(library_models(lib)), 1)
  expect_s3_class(library_load_model(lib, id), "plethr_ml")
  library_remove(lib, "S2")
  expect_equal(library_list(lib)$study_id, "S1")
})
