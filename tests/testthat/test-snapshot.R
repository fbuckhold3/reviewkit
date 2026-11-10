snap <- function() build_review_snapshot("101", make_forms(), make_activity(),
                                         fixture_window(),
                                         built_at = as.POSIXct("2026-10-15 09:00:00"))

test_that("duty counts follow amiontools duty_status", {
  d <- snap()$duty
  expect_equal(d$days_scheduled, 10)
  expect_equal(d$days_verified, 8)
  expect_equal(d$days_amion_default, 1)
  expect_equal(d$days_needs_entry, 1)
  expect_equal(d$days_off, 4)
  expect_equal(d$pct_verified, 80)
  expect_equal(d$total_hours, 90)
})

test_that("conference counts separate attended, did-not-attend and unlogged", {
  c <- snap()$conference
  expect_equal(c$expected_days, 10)
  expect_equal(c$expected_sluh, 10)
  expect_equal(c$attended_days, 6)
  expect_equal(c$not_attended_days, 1)
  expect_equal(c$unlogged_days, 3)
  expect_equal(c$pct_logged, 70)
})

test_that("MKSAP uses the latest import on or before the window end", {
  m <- snap()$mksap
  expect_equal(m$export_date, "2026-10-10")
  expect_equal(m$n_complete, 196)
})

test_that("peer: written by the resident vs received, 9 excluded from means", {
  p <- snap()$peer
  expect_equal(p$completed_by_resident, 1)
  expect_equal(p$received, 2)
  expect_equal(p$received_item_means$item1, 4)
  expect_equal(p$received_item_means$item2, 5)
})

test_that("assessments, faculty evals and ITE", {
  s <- snap()
  expect_equal(s$assessments$received, 3)        # Aug 1 counts: evaluations run from Jul 1
  expect_equal(s$assessments$with_plus_delta, 2)
  expect_equal(s$faculty_evals_submitted, 1)
  expect_equal(s$ite, list(pgy = 2, total_percentile = 70))
})

test_that("only the window counts", {
  s <- build_review_snapshot("101", make_forms(), make_activity(),
                             review_window(as.Date("2026-10-06")))
  expect_equal(s$duty$days_scheduled, 3)   # Oct 1, 2, 5
})

test_that("a resident with no data gives zeros and NAs, not an error", {
  s <- build_review_snapshot("999", make_forms(), make_activity(), fixture_window())
  expect_equal(s$duty$days_scheduled, 0)
  expect_true(is.na(s$duty$pct_verified))
  expect_true(is.na(s$mksap$n_complete))
  expect_equal(s$peer$received, 0)
})

test_that("several snapshots flatten to one row each", {
  ss <- build_review_snapshots(make_forms(), make_activity(), fixture_window(),
                               record_ids = c("101", "999"))
  tb <- snapshots_to_tibble(ss)
  expect_equal(nrow(tb), 2)
  expect_equal(tb$duty_days_verified, c(8, 0))
  expect_type(tb$record_id, "character")
})

test_that("evaluations count from Jul 1, duty and conference from the window start", {
  f <- make_forms()
  f$faculty_evaluation <- data.frame(record_id = "101", redcap_repeat_instrument = "faculty_evaluation",
    redcap_repeat_instance = 1:3, fac_eval_date = c("2026-08-01", "2026-10-05", "2026-06-30"),
    stringsAsFactors = FALSE)
  s <- build_review_snapshot("101", f, make_activity(), fixture_window())
  expect_equal(s$faculty_evals_submitted, 2)   # Aug 1 and Oct 5, not Jun 30
  expect_equal(s$window$ay_start, "2026-07-01")
  expect_equal(s$window$start, "2026-10-01")
})

