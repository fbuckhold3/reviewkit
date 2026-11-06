test_that("JSON round trip keeps every field, NAs included", {
  s <- build_review_snapshot("101", make_forms(), make_activity(), fixture_window(),
                             built_at = as.POSIXct("2026-10-15 09:00:00"))
  back <- snapshot_from_json(snapshot_to_json(s))
  expect_equal(back$duty, s$duty)
  expect_equal(back$conference, s$conference)
  expect_equal(back$peer$received_item_means$item1, 4)
  expect_equal(back$window, s$window)

  empty <- build_review_snapshot("999", make_forms(), make_activity(), fixture_window())
  back2 <- snapshot_from_json(snapshot_to_json(empty))
  expect_true(is.na(back2$mksap$export_date))
  expect_true(is.na(back2$duty$pct_verified))
})

test_that("empty values read as NULL; newer versions are refused", {
  expect_null(snapshot_from_json(""))
  expect_null(snapshot_from_json(NA))
  expect_error(snapshot_from_json('{"schema_version": 99}'), "newer")
})
