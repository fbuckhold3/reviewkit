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
# =============================================================================

#' First day the gate ever counted (AY 2026-27 started mid-year).
#' @export
FIRST_GATE_START <- as.Date("2026-10-01")

#' Review window for a self-evaluation submitted on `as_of`.
#'
#' @param as_of Submission (or "today") date. The window ends the day before.
#' @param lookback_start Optional first day of the duty/conference range,
#'   overriding the default.
#' @return A list: `start` (first day for duty hours and conference),
#'   `ay_start` (Jul 1 of the academic year, first day for evaluations),
#'   `end` (Dates) and `label` ("mid", "end" or "entering").
#' @examples
#' review_window(as.Date("2026-12-01"))  # start 2026-10-01, ay_start 2026-07-01
#' review_window(as.Date("2028-03-01"))  # start and ay_start 2027-07-01
#' @export
review_window <- function(as_of = Sys.Date(), lookback_start = NULL) {
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

  list(start = start, ay_start = ay_start, end = end, label = label)
}
