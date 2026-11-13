# =============================================================================
# Snapshot panel: the self-eval's frozen numbers, the gate result, any
# override, and (optionally) today's numbers beside them.
#
# Display only. The app loads the snapshot (s_e_snapshot_json) and, if it
# offers "Compare with today", builds the live snapshot itself (it takes
# ~30 s: all of RDM plus Amion), so this module never fetches.
# Styling is Bootstrap classes only, so it picks up each app's bslib theme.
# =============================================================================

.fmt_n <- function(x, digits = 0) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x)) return("—")
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

.of <- function(n, d) {
  if (is.na(suppressWarnings(as.numeric(d)))) return("—")
  sprintf("%s of %s", .fmt_n(n), .fmt_n(d))
}

#' Snapshot as display rows.
#'
#' The rows the snapshot panel shows, for apps that want the same numbers in
#' a table of their own (CCC, program report).
#'
#' @param snapshot A `build_review_snapshot()` or `snapshot_from_json()`
#'   result, or `NULL`.
#' @return A tibble with `section`, `item` and `value` (character). Empty
#'   for `NULL`.
#' @export
snapshot_rows <- function(snapshot) {
  if (is.null(snapshot)) {
    return(tibble::tibble(section = character(), item = character(), value = character()))
  }
  d <- snapshot$duty; c <- snapshot$conference; m <- snapshot$mksap
  p <- snapshot$peer; a <- snapshot$assessments; t <- snapshot$ite
  rows <- list(
    c("Duty hours", "Days verified", .of(d$days_verified, d$days_scheduled)),
    c("Duty hours", "Needing hours entered", .fmt_n(d$days_needs_entry)),
    c("Duty hours", "Still to confirm", .fmt_n(d$days_amion_default)),
    c("Duty hours", "Average weekly hours", .fmt_n(d$avg_weekly_hours, 1)),
    c("Duty hours", "Highest week", .fmt_n(d$max_week_hours, 1)),
    c("Duty hours", "Weeks over 80 h (4-week average)", .fmt_n(d$weeks_flag_80h)),
    c("Conference", "Days logged", .of(c$attended_days + c$not_attended_days, c$expected_days)),
    c("Conference", "Attended", .of(c$attended_days, c$expected_days)),
    c("Conference", "Did not attend (with reason)", .fmt_n(c$not_attended_days)),
    c("Conference", "No entry", .fmt_n(c$unlogged_days)),
    c("MKSAP", "Questions completed",
      if (is.na(m$n_complete)) "No import yet" else
        sprintf("%s (%s%%), as of %s", .fmt_n(m$n_complete), .fmt_n(m$pct_complete), m$export_date)),
    c("MKSAP", "Percent correct", if (is.na(m$pct_correct)) "—" else paste0(.fmt_n(m$pct_correct), "%")),
    c("Evaluations", "Peer reviews completed", .fmt_n(p$completed_by_resident)),
    c("Evaluations", "Peer reviews received", .fmt_n(p$received)),
    c("Evaluations", "Faculty assessments received", .fmt_n(a$received)),
    c("Evaluations", "Faculty evaluations submitted", .fmt_n(snapshot$faculty_evals_submitted)),
    c("ITE", "Latest total percentile",
      if (is.na(t$total_percentile)) "—" else sprintf("%s (PGY%s)", .fmt_n(t$total_percentile), t$pgy))
  )
  m2 <- do.call(rbind, rows)
  tibble::tibble(section = m2[, 1], item = m2[, 2], value = m2[, 3])
}

.month_day_year <- function(x) {
  d <- suppressWarnings(as.Date(substr(as.character(x), 1, 10)))
  if (length(d) == 0 || is.na(d)) return(as.character(x))
  trimws(format(d, "%B %e, %Y"))
}

.gate_alert <- function(snapshot, override) {
  g <- evaluate_gate(snapshot)
  cls <- if (g$pass) "alert-success" else if (!is.null(override)) "alert-info" else "alert-warning"
  head <- if (g$pass) "Data gate passed" else if (!is.null(override)) "Data gate cleared by override" else "Data gate not passed"
  shiny::div(
    class = paste("alert mb-2", cls),
    shiny::tags$strong(head),
    shiny::tags$ul(class = "mb-0 mt-1",
      shiny::tags$li(g$duty$message),
      shiny::tags$li(g$conference$message),
      shiny::tags$li(g$faculty$message)
    )
  )
}

.override_note <- function(override) {
  if (is.null(override)) return(NULL)
  bits <- c(
    if (!is.na(override$by %||% NA) && nzchar(override$by)) paste0("by ", override$by),
    if (!is.na(override$date %||% NA) && nzchar(override$date)) paste0("on ", .month_day_year(override$date))
  )
  shiny::div(
    class = "alert alert-info mb-2",
    shiny::tags$strong("Gate override"),
    if (length(bits)) paste0(" (", paste(bits, collapse = " "), ")"),
    if (!is.na(override$reason %||% NA) && nzchar(override$reason))
      shiny::tags$p(class = "mb-0 mt-1", override$reason)
  )
}

.snapshot_table <- function(snap, live) {
  s_rows <- snapshot_rows(snap)
  l_rows <- snapshot_rows(live)
  base <- if (nrow(s_rows)) s_rows else l_rows
  has_s <- nrow(s_rows) > 0; has_l <- nrow(l_rows) > 0
  head <- shiny::tags$tr(
    shiny::tags$th(""),
    if (has_s) shiny::tags$th("At submission"),
    if (has_l) shiny::tags$th("Today")
  )
  body <- lapply(seq_len(nrow(base)), function(i) {
    sec_start <- i == 1 || base$section[i] != base$section[i - 1]
    list(
      if (sec_start) shiny::tags$tr(class = "table-light",
        shiny::tags$th(colspan = 1 + has_s + has_l, base$section[i])),
      shiny::tags$tr(
        shiny::tags$td(base$item[i]),
        if (has_s) shiny::tags$td(s_rows$value[i]),
        if (has_l) shiny::tags$td(
          class = if (has_s && l_rows$value[i] != s_rows$value[i]) "fw-semibold",
          l_rows$value[i])
      )
    )
  })
  shiny::tags$table(class = "table table-sm mb-0",
                    shiny::tags$thead(head), shiny::tags$tbody(body))
}

#' Snapshot panel UI.
#' @param id Module id.
#' @export
mod_snapshot_panel_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("panel"))
}

#' Snapshot panel server.
#'
#' Shows the self-eval's stored snapshot (or "not submitted yet"), the gate
#' result, any override, and the live snapshot beside it when the app
#' supplies one.
#'
#' @param id Module id.
#' @param snapshot_r Reactive: the stored snapshot (`snapshot_from_json()`)
#'   or `NULL` when the self-eval isn't submitted.
#' @param live_r Reactive: a live `build_review_snapshot()` for today, or
#'   `NULL`.
#' @param override_r Reactive: `NULL`, or a list with `reason`, `by`, `date`
#'   when the period's gate was cleared by override.
#' @param live_status_r Reactive: `"idle"`, `"running"` or an error message,
#'   for the "Compare with today" button. `NULL` hides the button.
#' @return A list with `compare`, a reactive that fires when the "Compare
#'   with today" button is clicked.
#' @export
mod_snapshot_panel_server <- function(id, snapshot_r,
                                      live_r = shiny::reactive(NULL),
                                      override_r = shiny::reactive(NULL),
                                      live_status_r = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$panel <- shiny::renderUI({
      snap <- snapshot_r(); live <- live_r(); ov <- override_r()
      status <- if (is.null(live_status_r)) NULL else live_status_r()

      lead <- if (!is.null(snap)) {
        shiny::p(class = "mb-2",
          sprintf("Submitted %s. Duty hours and conference count from %s; evaluations from %s, through %s.",
                  .month_day_year(snap$built_at), .month_day_year(snap$window$start),
                  .month_day_year(snap$window$ay_start %||% snap$window$start),
                  .month_day_year(snap$window$end)))
      } else {
        shiny::div(class = "alert alert-secondary mb-2",
          "The self-evaluation hasn't been submitted for this period, so there are no frozen numbers yet.")
      }

      button <- if (!is.null(status)) {
        running <- identical(status, "running")
        shiny::div(class = "mt-2 d-flex align-items-center gap-2",
          shiny::actionButton(ns("compare"),
            if (is.null(snap)) "Check today's data" else "Compare with today",
            class = "btn-outline-primary btn-sm", disabled = if (running) "disabled"),
          if (running) shiny::span(class = "text-muted small",
                                   "Loading schedule and logs… about 30 seconds."),
          if (!running && !identical(status, "idle"))
            shiny::span(class = "text-danger small", status)
        )
      }

      shiny::tagList(
        lead,
        if (!is.null(snap)) .gate_alert(snap, ov) else if (!is.null(live)) .gate_alert(live, ov),
        .override_note(ov),
        if (!is.null(snap) || !is.null(live)) .snapshot_table(snap, live),
        button
      )
    })
    list(compare = shiny::reactive(input$compare))
  })
}
