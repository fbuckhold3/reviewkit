test_that("strict gate fails with plain reasons", {
  s <- build_review_snapshot("101", make_forms(), make_activity(), fixture_window())
  g <- evaluate_gate(s)
  expect_false(g$pass)
  expect_false(g$duty$pass)
  expect_match(g$duty$message, "1 day needing hours entered, 1 day still to confirm")
  expect_false(g$conference$pass)
  expect_match(g$conference$message, "3 expected days with no entry")
})

test_that("gate passes once everything is accounted for", {
  a <- make_activity()
  a$duty_status[a$duty_status %in% c("amion_default", "needs_entry")] <- "confirmed"
  a$conf_attended[a$conf_expected != "None"] <- TRUE
  s <- build_review_snapshot("101", make_forms(), a, fixture_window())
  g <- evaluate_gate(s)
  expect_true(g$pass)
  expect_match(g$duty$message, "10 days verified")
})

test_that("tolerances loosen the rule without code changes", {
  s <- build_review_snapshot("101", make_forms(), make_activity(), fixture_window())
  g <- evaluate_gate(s, gate_rules(duty_max_unverified = 2, conf_max_unlogged = 3))
  expect_true(g$pass)
})

test_that("nudges never block and carry counts", {
  s <- build_review_snapshot("101", make_forms(), make_activity(), fixture_window())
  n <- evaluate_gate(s)$nudges
  expect_equal(n$value, c(196, 1, 1))
  expect_match(n$detail[1], "2026-10-10 import")
})

test_that("no schedule fails the gate instead of passing with nothing to check", {
  s <- build_review_snapshot("999", make_forms(), make_activity(), fixture_window())
  g <- evaluate_gate(s)
  expect_false(g$pass)
  expect_false(g$duty$pass)
  expect_false(g$conference$pass)
  expect_match(g$duty$message, "can't find your schedule")
  expect_true(evaluate_gate(s, gate_rules(require_schedule = FALSE))$pass)
})
