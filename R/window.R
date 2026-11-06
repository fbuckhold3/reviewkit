# =============================================================================
# Review windows: the date range the snapshot and the gate look at.
#
# Self-evaluation periods follow gmed (Oct 1 - Jan 31 "mid", Feb 1 - Jun 30
# "end"). By default the lookback starts where the current period starts and
# runs through the day BEFORE the self-eval is submitted, so a resident can't
# fail the gate on a day they haven't had a chance to log yet.
#
# Fred, 2026-11-06: the first gated window (Oct 2026 - Jan 2027) looks back
# to Oct 1, not Jul 1, to keep the catch-up doable. That is the default
# below; pass lookback_start to widen it for later windows.
# =============================================================================

#' Review window for a self-evaluation submitted on `as_of`.
#'
#' @param as_of Submission (or "today") date. The window ends the day before.
#' @param lookback_start Optional first day of the window. Default: the start
#'   of the self-evaluation period `as_of` falls in (Oct 1 for mid-year,
#'   Feb 1 for end-of-year, Jul 1 for Entering Residency).
#' @return A list: `start`, `end` (Dates) and `label` ("mid", "end" or
#'   "entering").
#' @examples
#' review_window(as.Date("2026-12-01"))  # 2026-10-01 to 2026-11-30, "mid"
#' @export
review_window <- function(as_of = Sys.Date(), lookback_start = NULL) {
  as_of <- as.Date(as_of)
  if (is.na(as_of)) stop("review_window: as_of is not a valid date")

  month <- as.integer(format(as_of, "%m"))
  year  <- as.integer(format(as_of, "%Y"))

  # Period start for the date: Oct-Jan = mid (Oct 1), Feb-Jun = end (Feb 1),
  # Jul-Sep = Entering Residency (Jul 1).
  if (month >= 10) {
    start <- as.Date(sprintf("%d-10-01", year)); label <- "mid"
  } else if (month == 1) {
    start <- as.Date(sprintf("%d-10-01", year - 1L)); label <- "mid"
  } else if (month <= 6) {
    start <- as.Date(sprintf("%d-02-01", year)); label <- "end"
  } else {
    start <- as.Date(sprintf("%d-07-01", year)); label <- "entering"
  }

  if (!is.null(lookback_start)) start <- as.Date(lookback_start)
  # Submitted on the period's first day -> end < start, an empty window.
  end <- as_of - 1L

  list(start = start, end = end, label = label)
}
