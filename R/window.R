# =============================================================================
# Review windows: the date range the snapshot and the gate look at.
#
# Self-evaluation periods follow gmed (Oct 1 - Jan 31 "mid", Feb 1 - Jun 30
# "end", Jul - Sep Entering Residency). Both ranges run through the day
# BEFORE the self-eval is submitted, so a resident can't fail the gate on a
# day they haven't had a chance to log yet.
#
# Two ranges (Fred, 2026-11-10):
#   - Duty hours and conference logging (the gate) count from the start of
#     the academic year, Jul 1. Exception for AY 2026-27 only: the gate
#     started mid-year, so it counts from Oct 1, 2026 (FIRST_GATE_START).
#   - Evaluations (peer, assessments, faculty evaluations written) count
#     from Jul 1 of the academic year, every year.
#   - Faculty evaluations the resident writes count through `as_of` itself
#     (2026-11-18): they're done in the app, often right before submitting,
#     so there's no "hasn't had a chance to log it" day to allow for.
#
# "Today" is the program's date (gmed::program_today(), Central time), not
# the server's: Posit Connect runs on UTC, which is already tomorrow after
# 6 pm Central.
# =============================================================================

#' First day the gate ever counted (AY 2026-27 started mid-year).
#' @export
FIRST_GATE_START <- as.Date("2026-10-01")

#' Review window for a self-evaluation submitted on `as_of`.
#'
#' @param as_of Submission (or "today") date, in program time. The window
#'   ends the day before (faculty evaluations count through `as_of`).
#' @param lookback_start Optional first day of the duty/conference range,
#'   overriding the default.
#' @return A list: `start` (first day for duty hours and conference),
#'   `ay_start` (Jul 1 of the academic year, first day for evaluations),
#'   `end`, `as_of` (Dates) and `label` ("mid", "end" or "entering").
#' @examples
#' review_window(as.Date("2026-12-01"))  # start 2026-10-01, ay_start 2026-07-01
#' review_window(as.Date("2028-03-01"))  # start and ay_start 2027-07-01
#' @export
review_window <- function(as_of = gmed::program_today(), lookback_start = NULL) {
  as_of <- as.Date(as_of)
  if (is.na(as_of)) stop("review_window: as_of is not a valid date")

  end   <- as_of - 1L
  month <- as.integer(format(as_of, "%m"))
  label <- if (month >= 10 || month == 1) "mid" else if (month <= 6) "end" else "entering"

  # Academic year of the last day counted (a Jul 1 submission closes out
  # the previous year).
  ey <- as.integer(format(end, "%Y"))
  ay_start <- as.Date(sprintf("%d-07-01", if (as.integer(format(end, "%m")) >= 7) ey else ey - 1L))

  start <- max(ay_start, FIRST_GATE_START)
  if (!is.null(lookback_start)) start <- as.Date(lookback_start)

  list(start = start, ay_start = ay_start, end = end, as_of = as_of, label = label)
}
