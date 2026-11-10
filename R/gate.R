# =============================================================================
# Data-readiness gate (Fred, 2026-11-04 / 11-06).
#
# STRICT on duty hours and conference logging: the self-eval cannot be
# submitted until both are current for the window. Everything else (MKSAP,
# peer reviews, faculty evals) is a NUDGE: shown with counts, never blocking.
#
# Duty: every scheduled day in the window is confirmed or entered by the
#   resident (none left on the Amion default, none needing entry).
# Conference: every expected conference day (Amion says SLUH or VA) has an
#   entry: attended, or "did not attend" with a reason.
# =============================================================================

#' Gate thresholds.
#'
#' Defaults are the strict rule. The tolerances exist so the rule can be
#' loosened without code changes (e.g. a soft launch), not as the plan.
#'
#' @param duty_max_unverified Scheduled days allowed to be left unverified.
#' @param conf_max_unlogged Expected conference days allowed to have no entry.
#' @param require_schedule Fail the gate when the window has no scheduled
#'   days. With nothing to check, every count is zero and the gate would
#'   otherwise pass (e.g. a resident not linked to Amion).
#' @return A list of thresholds.
#' @export
gate_rules <- function(duty_max_unverified = 0L, conf_max_unlogged = 0L,
                       require_schedule = TRUE) {
  list(duty_max_unverified = duty_max_unverified,
       conf_max_unlogged   = conf_max_unlogged,
       require_schedule    = require_schedule)
}

.month_day <- function(d) trimws(format(as.Date(d), "%B %e"))

.plural <- function(n, word) sprintf("%d %s%s", n, word, if (n == 1) "" else "s")

#' Check a snapshot against the gate.
#'
#' @param snapshot A `build_review_snapshot()` result.
#' @param rules `gate_rules()`.
#' @return A list: `pass` (logical, both strict checks), `duty` and
#'   `conference` (each `pass` + `message`), and `nudges`, a tibble with
#'   `item`, `value` and `detail` for the non-blocking checklist.
#' @export
evaluate_gate <- function(snapshot, rules = gate_rules()) {
  d <- snapshot$duty
  c <- snapshot$conference

  no_schedule <- isTRUE(rules$require_schedule) && d$days_scheduled == 0
  unverified <- d$days_amion_default + d$days_needs_entry
  duty_pass  <- !no_schedule && unverified <= rules$duty_max_unverified
  duty_msg <- if (no_schedule) {
    "We can't find your schedule for this period, so your data can't be checked. Please contact the program office."
  } else if (duty_pass) {
    sprintf("Duty hours current: %s verified.", .plural(d$days_verified, "day"))
  } else {
    parts <- c(
      if (d$days_needs_entry > 0) sprintf("%s needing hours entered", .plural(d$days_needs_entry, "day")),
      if (d$days_amion_default > 0) sprintf("%s still to confirm", .plural(d$days_amion_default, "day"))
    )
    sprintf("Duty hours: %s.", paste(parts, collapse = ", "))
  }

  conf_pass <- !no_schedule && c$unlogged_days <= rules$conf_max_unlogged
  conf_msg <- if (no_schedule) {
    "Conference: can't be checked without a schedule."
  } else if (conf_pass) {
    sprintf("Conference log current: %d of %d expected days logged.",
            c$attended_days + c$not_attended_days, c$expected_days)
  } else {
    sprintf("Conference: %s with no entry. Log each as attended, or did not attend with a reason.",
            .plural(c$unlogged_days, "expected day"))
  }

  m <- snapshot$mksap
  nudges <- tibble::tibble(
    item = c("MKSAP questions completed", "Peer reviews completed",
             "Faculty evaluations submitted"),
    value = c(m$n_complete, snapshot$peer$completed_by_resident,
              snapshot$faculty_evals_submitted),
    detail = c(
      if (is.na(m$export_date)) "No MKSAP import yet" else
        sprintf("As of the %s import (%s%% of questions)", m$export_date,
                format(m$pct_complete)),
      sprintf("Since %s", .month_day(snapshot$window$ay_start %||% snapshot$window$start)),
      sprintf("Since %s", .month_day(snapshot$window$ay_start %||% snapshot$window$start))
    )
  )

  list(
    pass       = duty_pass && conf_pass,
    duty       = list(pass = duty_pass, message = duty_msg),
    conference = list(pass = conf_pass, message = conf_msg),
    nudges     = nudges
  )
}
