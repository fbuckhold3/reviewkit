# Synthetic inputs, no REDCap calls. Resident "101": Oct 1-14 2026.
#   Duty: 8 weekdays verified, 1 on the Amion default, 1 needing entry,
#         weekends off.
#   Conference: SLUH expected on the 10 weekdays; attended 6, one
#         "did not attend" entry, 3 with nothing.
make_activity <- function(rid = "101") {
  dates <- seq(as.Date("2026-10-01"), as.Date("2026-10-14"), by = "day")
  wkend <- format(dates, "%u") %in% c("6", "7")
  wd <- which(!wkend)
  duty <- ifelse(wkend, "off", "confirmed")
  duty[wd[9]]  <- "amion_default"
  duty[wd[10]] <- "needs_entry"
  attended <- rep(FALSE, length(dates)); attended[wd[1:6]] <- TRUE
  tibble::tibble(
    record_id = rid, name = "Test Resident", Level = "PGY2",
    Date = dates,
    week_start = dates - as.integer(format(dates, "%w")),
    is_future = FALSE,
    conf_expected = ifelse(wkend, "None", "SLUH"),
    conf_logged = attended,
    conf_attended = attended,
    conf_not_attended = seq_along(dates) == wd[7],
    afternoon_school = FALSE,
    duty_status = duty,
    duty_hours = ifelse(wkend, 0, ifelse(duty == "needs_entry", NA, 10)),
    home_hours = 0,
    is_vacation = FALSE
  )
}

make_forms <- function(rid = "101") {
  wd <- seq(as.Date("2026-10-01"), as.Date("2026-10-14"), by = "day")
  wd <- wd[!format(wd, "%u") %in% c("6", "7")]
  list(
    questions = data.frame(
      record_id = rid, redcap_repeat_instrument = "questions",
      redcap_repeat_instance = 1:2,
      q_date = format(c(wd[7], as.Date("2026-09-15"))),
      stringsAsFactors = FALSE),
    peer_eval = data.frame(
      record_id = c(rid, rid, "202"), redcap_repeat_instrument = "peer_eval",
      redcap_repeat_instance = 1:3,
      peer_date = c("2026-10-05", "2026-10-06", "2026-10-07"),
      peer_evaluator_id = c("202", "203", rid),
      peer_item1 = c("4", "9", "5"), peer_item2 = "5", peer_item3 = "3",
      peer_item4 = "4", peer_item5 = "5", stringsAsFactors = FALSE),
    mksap_progress = data.frame(
      record_id = rid, redcap_repeat_instrument = "mksap_progress",
      redcap_repeat_instance = 1:2,
      mk_export_date = c("2026-09-01", "2026-10-10"),
      mk_total_questions = "2375", mk_n_complete = c("50", "196"),
      mk_pct_complete = c("2", "8"), mk_pct_correct = c("60", "66"),
      stringsAsFactors = FALSE),
    assessment = data.frame(
      record_id = rid, redcap_repeat_instrument = "assessment",
      redcap_repeat_instance = 1:3,
      ass_date = c("2026-10-02", "2026-10-09", "2026-08-01"),
      ass_plus = c("Good work", "", "Old"), ass_delta = c("", NA, ""),
      stringsAsFactors = FALSE),
    faculty_evaluation = data.frame(
      record_id = rid, redcap_repeat_instrument = "faculty_evaluation",
      redcap_repeat_instance = 1:2,
      fac_eval_date = c("2026-10-03", "2026-06-10"), stringsAsFactors = FALSE),
    test_data = data.frame(
      record_id = rid, redcap_repeat_instrument = NA,
      pgy1_total_ile = "77", pgy2_total_ile = "70", pgy3_total_ile = "",
      stringsAsFactors = FALSE)
  )
}

fixture_window <- function() review_window(as.Date("2026-10-15"))
