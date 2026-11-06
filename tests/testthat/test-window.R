test_that("mid-year window starts Oct 1 and ends the day before", {
  w <- review_window(as.Date("2026-12-01"))
  expect_equal(w$start, as.Date("2026-10-01"))
  expect_equal(w$end, as.Date("2026-11-30"))
  expect_equal(w$label, "mid")
})

test_that("January belongs to the window that started the previous Oct", {
  w <- review_window(as.Date("2027-01-10"))
  expect_equal(w$start, as.Date("2026-10-01"))
})

test_that("end-of-year and entering windows", {
  expect_equal(review_window(as.Date("2027-04-15"))$start, as.Date("2027-02-01"))
  expect_equal(review_window(as.Date("2027-04-15"))$label, "end")
  expect_equal(review_window(as.Date("2026-08-15"))$label, "entering")
})

test_that("lookback_start overrides the period start", {
  w <- review_window(as.Date("2026-12-01"), lookback_start = "2026-07-01")
  expect_equal(w$start, as.Date("2026-07-01"))
})

test_that("submitting on the first day gives an empty window", {
  w <- review_window(as.Date("2026-10-01"))
  expect_true(w$end < w$start)
})
