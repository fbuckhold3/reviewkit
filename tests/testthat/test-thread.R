thread_forms <- function() {
  list(
    coach_interim = data.frame(
      record_id = c("101", "101", "202"), redcap_repeat_instrument = "coach_interim",
      redcap_repeat_instance = 1:3,
      ci_date = c("2026-11-20", "2026-10-05", "2026-11-01"),
      ci_coach = "Dr. Coach", ci_reason = c("4", "3", "1"),
      ci_reason_other = c("Schedule change", NA, NA),
      ci_notes = c("Talked about nights.", "Missed two shifts.", "Other resident"),
      ci_ccc_instance = NA, ci_ccc_flag = c("0", "1", NA),
      ci_followup_date = c(NA, "2026-12-01", NA), stringsAsFactors = FALSE),
    ccc_review = data.frame(
      record_id = "101", redcap_repeat_instrument = "ccc_review",
      redcap_repeat_instance = 1:2,
      ccc_date = c("2026-10-10", NA), ccc_rev_type = c("2", "1"),
      ccc_session = c(NA, "3"), ccc_interim = c("Attendance concern", NA),
      ccc_concern = c("1", "0"),
      ccc_action___1 = "0", ccc_action___4 = c("1", "0"), ccc_action___5 = c("1", "0"),
      ccc_action_status___2 = c("1", "0"),
      ccc_competency___5 = c("1", "0"),
      ccc_fu_resp = c("Coach", NA), ccc_ilp = NA, ccc_mile_notes = NA,
      ccc_issues_follow_up = NA, ccc_comments = c(NA, "On track"),
      stringsAsFactors = FALSE),
    coach_rev = data.frame(
      record_id = "101", redcap_repeat_instrument = "coach_rev",
      redcap_repeat_instance = 1:2,
      coach_date = c("2026-06-01", "2026-01-10"), coach_period = c("2", "1"),
      coach_summary = c("Strong year.", ""), stringsAsFactors = FALSE)
  )
}

test_that("thread merges the three forms for one resident, newest first", {
  th <- build_resident_thread(thread_forms(), "101")
  expect_equal(th$source, c("coach_interim", "ccc_review", "coach_interim",
                            "coach_rev", "ccc_review"))
  expect_equal(th$date[1:4], as.Date(c("2026-11-20", "2026-10-10", "2026-10-05", "2026-06-01")))
  expect_true(is.na(th$date[5]))            # undated sorts last
  expect_false(any(th$text[th$source == "coach_interim"][[1]] == "Other resident"))
})

test_that("labels come from codes", {
  th <- build_resident_thread(thread_forms(), "101")
  expect_equal(th$title[1], "Coach interim note: Other: Schedule change")
  expect_equal(th$title[3], "Coach interim note: Concern")
  expect_equal(th$tags[[3]], "Flagged for CCC")
  expect_equal(th$followup[3], as.Date("2026-12-01"))
  expect_equal(th$title[2], "CCC interim review")
  expect_true("Concern" %in% th$tags[[2]])
  expect_true("Actions: Coach follow up; Meet with PD and/or CCC Chair" %in% th$tags[[2]])
  expect_true("Status: Ongoing" %in% th$tags[[2]])
  expect_true("Competency: Professionalism" %in% th$tags[[2]])
  expect_equal(th$title[5], "CCC review: Mid PGY2")
  expect_equal(th$title[4], "Coach review: End Intern")
})

test_that("empty coach summaries are dropped and labels pass through", {
  f <- thread_forms(); f$coach_rev$coach_period <- c("End Intern", "Mid Intern")
  th <- build_resident_thread(f, "101")
  expect_equal(sum(th$source == "coach_rev"), 1)
  expect_equal(th$title[th$source == "coach_rev"], "Coach review: End Intern")
})

test_that("missing forms give an empty thread", {
  th <- build_resident_thread(list(), "101")
  expect_equal(nrow(th), 0)
  expect_named(th, c("date", "source", "title", "author", "text", "tags", "followup", "instance"))
  expect_error(build_resident_thread(list(), c("1", "2")))
})

test_that("snapshot_rows lists the panel rows", {
  snap <- build_review_snapshot("101", make_forms(), make_activity(),
                                review_window(as.Date("2026-10-15")))
  r <- snapshot_rows(snap)
  expect_equal(r$value[r$item == "Days verified"], "8 of 10")
  expect_equal(r$value[r$item == "Days logged"], "7 of 10")
  expect_equal(nrow(snapshot_rows(NULL)), 0)
  # Round trip through JSON gives the same rows.
  expect_equal(snapshot_rows(snapshot_from_json(snapshot_to_json(snap))), r)
})

test_that("panels render", {
  snap <- build_review_snapshot("101", make_forms(), make_activity(),
                                review_window(as.Date("2026-10-15")))
  shiny::testServer(mod_snapshot_panel_server, args = list(
    snapshot_r = shiny::reactive(snap),
    override_r = shiny::reactive(list(reason = "Leave", by = "PD", date = "2026-11-12")),
    live_status_r = shiny::reactive("idle")), {
      html <- output$panel$html
      expect_match(html, "Data gate cleared by override")
      expect_match(html, "Leave")
      expect_match(html, "Compare with today")
      expect_match(html, "8 of 10")
    })
  shiny::testServer(mod_snapshot_panel_server, args = list(
    snapshot_r = shiny::reactive(NULL)), {
      expect_match(output$panel$html, "been submitted for this period")
    })
  th <- build_resident_thread(thread_forms(), "101")
  shiny::testServer(mod_thread_server, args = list(thread_r = shiny::reactive(th)), {
    expect_match(output$thread$html, "Missed two shifts")
    expect_match(output$thread$html, "Follow up by Dec")
  })
})
