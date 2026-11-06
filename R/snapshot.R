# =============================================================================
# Review snapshot: one resident's numbers for one review window.
#
# Built once when the self-eval is submitted and stored with it
# (snapshot_to_json()), so the coach, second reviewer and CCC all see what
# was true at submission. Apps can show live numbers beside it.
#
# build_review_snapshot() fetches nothing: pass the RDM forms and the
# amiontools activity table the app already has (or fetch_review_inputs()).
# No duty-hour, schedule or attendance rules live here: those come from
# amiontools::build_resident_activity(). This file only counts, inside the
# window.
#
# Milestones are deliberately NOT in the snapshot: the coach, second
# reviewer and CCC change them during the review, so they are read live.
# =============================================================================

#' Snapshot format version, stored in every snapshot. Bump it when a field
#' is added, renamed or redefined, so stored snapshots can be read back.
#' @export
SNAPSHOT_SCHEMA_VERSION <- 1L

# Conference "did not attend (reason)" response on the questions form.
# Proposed field (not in RDM yet, Phase 1 schema change): until it exists,
# not_attended_days is always 0.
.NOT_ATTENDED_FIELD <- "q_attended"
.NOT_ATTENDED_VALUE <- "0"

# Duty statuses that count as a scheduled (non-off) day, and the subset the
# resident has verified.
.DUTY_SCHEDULED <- c("confirmed", "entered", "amion_default", "needs_entry")
.DUTY_VERIFIED  <- c("confirmed", "entered")

.as_date <- function(x) suppressWarnings(as.Date(as.character(x)))

.in_window <- function(dates, window) {
  d <- .as_date(dates)
  !is.na(d) & d >= window$start & d <= window$end
}

.num <- function(x) suppressWarnings(as.numeric(as.character(x)))

# Rows of a repeating form for one resident (or all rows when rid is NULL).
.form_rows <- function(forms, form, rid = NULL) {
  x <- forms[[form]]
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) return(NULL)
  if ("redcap_repeat_instrument" %in% names(x)) {
    x <- x[!is.na(x$redcap_repeat_instrument) & x$redcap_repeat_instrument == form, , drop = FALSE]
  }
  if (!is.null(rid)) x <- x[as.character(x$record_id) == rid, , drop = FALSE]
  x
}

.pct <- function(n, d) if (is.na(d) || d == 0) NA_real_ else round(100 * n / d, 1)

.duty_block <- function(act, window) {
  sched <- act[act$duty_status %in% .DUTY_SCHEDULED, , drop = FALSE]
  n_sched    <- nrow(sched)
  n_verified <- sum(sched$duty_status %in% .DUTY_VERIFIED)

  weekly <- if (nrow(act) > 0) {
    amiontools::summarise_activity(act, period = "week", by = "resident",
                                   through = window$end)
  } else NULL
  has_weeks <- !is.null(weekly) && nrow(weekly) > 0

  list(
    days_scheduled     = n_sched,
    days_verified      = n_verified,
    days_amion_default = sum(sched$duty_status == "amion_default"),
    days_needs_entry   = sum(sched$duty_status == "needs_entry"),
    days_off           = sum(act$duty_status == "off"),
    pct_verified       = .pct(n_verified, n_sched),
    total_hours        = round(sum(act$duty_hours, na.rm = TRUE), 1),
    weeks              = if (has_weeks) nrow(weekly) else 0L,
    avg_weekly_hours   = if (has_weeks) round(mean(weekly$duty_hours), 1) else NA_real_,
    max_week_hours     = if (has_weeks) round(max(weekly$duty_hours), 1) else NA_real_,
    # The 4-week rolling average only sees weeks inside the window.
    weeks_flag_80h     = if (has_weeks) sum(weekly$flag_80h) else 0L
  )
}

.conference_block <- function(act, forms, rid, window) {
  exp_days <- act[act$conf_expected != "None", , drop = FALSE]

  q <- .form_rows(forms, "questions", rid)
  not_att_dates <- if (!is.null(q) && .NOT_ATTENDED_FIELD %in% names(q)) {
    unique(.as_date(q$q_date[!is.na(q[[.NOT_ATTENDED_FIELD]]) &
                             as.character(q[[.NOT_ATTENDED_FIELD]]) == .NOT_ATTENDED_VALUE]))
  } else as.Date(character())

  not_attended <- exp_days$Date %in% not_att_dates
  attended     <- exp_days$conf_attended & !not_attended
  n_expected   <- nrow(exp_days)
  n_accounted  <- sum(attended | not_attended)

  list(
    expected_days         = n_expected,
    expected_sluh         = sum(exp_days$conf_expected == "SLUH"),
    expected_va           = sum(exp_days$conf_expected == "VA"),
    attended_days         = sum(attended),
    not_attended_days     = sum(not_attended),
    unlogged_days         = n_expected - n_accounted,
    pct_logged            = .pct(n_accounted, n_expected),
    pct_attended          = .pct(sum(attended), n_expected),
    afternoon_school_days = sum(act$afternoon_school)
  )
}

.mksap_block <- function(forms, rid, window) {
  m <- .form_rows(forms, "mksap_progress", rid)
  empty <- list(export_date = NA_character_, total_questions = NA_real_,
                n_complete = NA_real_, pct_complete = NA_real_,
                pct_correct = NA_real_)
  if (is.null(m) || nrow(m) == 0) return(empty)
  # Latest import on or before the window end.
  d <- .as_date(m$mk_export_date)
  keep <- !is.na(d) & d <= window$end
  if (!any(keep)) return(empty)
  m <- m[keep, , drop = FALSE]; d <- d[keep]
  r <- m[which.max(d), , drop = FALSE]
  list(
    export_date     = format(max(d)),
    total_questions = .num(r$mk_total_questions),
    n_complete      = .num(r$mk_n_complete),
    pct_complete    = .num(r$mk_pct_complete),
    pct_correct     = .num(r$mk_pct_correct)
  )
}

.peer_block <- function(forms, rid, window) {
  p <- .form_rows(forms, "peer_eval")
  if (is.null(p)) {
    return(list(completed_by_resident = 0L, received = 0L,
                received_item_means = as.list(stats::setNames(rep(NA_real_, 5), paste0("item", 1:5)))))
  }
  p <- p[.in_window(p$peer_date, window), , drop = FALSE]
  written  <- p[as.character(p$peer_evaluator_id) == rid, , drop = FALSE]
  received <- p[as.character(p$record_id) == rid, , drop = FALSE]

  means <- vapply(1:5, function(i) {
    v <- .num(received[[paste0("peer_item", i)]])
    v <- v[!is.na(v) & v != 9]   # 9 = not observed
    if (length(v)) round(mean(v), 2) else NA_real_
  }, numeric(1))
  names(means) <- paste0("item", 1:5)

  list(completed_by_resident = nrow(written),
       received              = nrow(received),
       received_item_means   = as.list(means))
}

.assessment_block <- function(forms, rid, window) {
  a <- .form_rows(forms, "assessment", rid)
  if (is.null(a)) return(list(received = 0L, with_plus_delta = 0L))
  a <- a[.in_window(a$ass_date, window), , drop = FALSE]
  has_text <- function(x) !is.na(x) & nzchar(trimws(as.character(x)))
  list(received        = nrow(a),
       with_plus_delta = sum(has_text(a$ass_plus) | has_text(a$ass_delta)))
}

.faculty_eval_count <- function(forms, rid, window) {
  f <- .form_rows(forms, "faculty_evaluation", rid)
  if (is.null(f)) return(0L)
  sum(.in_window(f$fac_eval_date, window))
}

.ite_block <- function(forms, rid) {
  t <- forms[["test_data"]]
  if (is.null(t) || !"record_id" %in% names(t)) return(list(pgy = NA_integer_, total_percentile = NA_real_))
  t <- t[as.character(t$record_id) == rid, , drop = FALSE]
  for (y in 3:1) {
    col <- paste0("pgy", y, "_total_ile")
    if (col %in% names(t)) {
      v <- .num(t[[col]]); v <- v[!is.na(v)]
      if (length(v)) return(list(pgy = y, total_percentile = v[1]))
    }
  }
  list(pgy = NA_integer_, total_percentile = NA_real_)
}

#' Build one resident's review snapshot.
#'
#' @param record_id RDM record_id.
#' @param forms Named list of RDM forms with raw codes, as in
#'   `gmed::load_rdm_complete(raw_or_label = "raw")$all_forms`.
#' @param activity `amiontools::build_resident_activity()` (may cover more
#'   residents and dates; it is filtered here).
#' @param window `review_window()` result.
#' @param built_at Timestamp recorded in the snapshot.
#' @return A list (see the package README for every field): `schema_version`,
#'   `record_id`, `name`, `level`, `window`, `built_at`, `duty`,
#'   `conference`, `mksap`, `peer`, `assessments`, `faculty_evals_submitted`,
#'   `ite`.
#' @export
build_review_snapshot <- function(record_id, forms, activity,
                                  window = review_window(),
                                  built_at = Sys.time()) {
  rid <- as.character(record_id)
  if (length(rid) != 1 || is.na(rid) || !nzchar(rid)) {
    stop("build_review_snapshot: record_id must be a single value")
  }
  if (!is.list(forms)) stop("build_review_snapshot: forms must be a named list of data frames")

  act <- activity[as.character(activity$record_id) == rid &
                    activity$Date >= window$start & activity$Date <= window$end,
                  , drop = FALSE]

  name  <- if (nrow(act)) act$name[1] else NA_character_
  level <- if (nrow(act)) as.character(act$Level[1]) else NA_character_
  if (is.na(name)) {
    rd <- forms[["resident_data"]]
    if (!is.null(rd) && "name" %in% names(rd)) {
      n <- rd$name[as.character(rd$record_id) == rid & !is.na(rd$name)]
      if (length(n)) name <- n[1]
    }
  }

  list(
    schema_version          = SNAPSHOT_SCHEMA_VERSION,
    record_id               = rid,
    name                    = name,
    level                   = level,
    window                  = list(start = format(window$start),
                                   end   = format(window$end),
                                   label = window$label),
    built_at                = format(built_at, "%Y-%m-%d %H:%M:%S"),
    duty                    = .duty_block(act, window),
    conference              = .conference_block(act, forms, rid, window),
    mksap                   = .mksap_block(forms, rid, window),
    peer                    = .peer_block(forms, rid, window),
    assessments             = .assessment_block(forms, rid, window),
    faculty_evals_submitted = .faculty_eval_count(forms, rid, window),
    ite                     = .ite_block(forms, rid)
  )
}

#' Build snapshots for several residents.
#'
#' @inheritParams build_review_snapshot
#' @param record_ids Character vector of record_ids. Default: every resident
#'   in `activity`.
#' @return A named list of snapshots (names are record_ids).
#' @export
build_review_snapshots <- function(forms, activity, window = review_window(),
                                   record_ids = unique(activity$record_id),
                                   built_at = Sys.time()) {
  ids <- as.character(record_ids)
  out <- lapply(ids, build_review_snapshot, forms = forms, activity = activity,
                window = window, built_at = built_at)
  stats::setNames(out, ids)
}

#' Flatten snapshots to one row per resident.
#'
#' For tables (CCC, program report) and checks. Nested fields become
#' `section_field` columns, e.g. `duty_pct_verified`.
#'
#' @param snapshots One snapshot or a list from `build_review_snapshots()`.
#' @return A tibble.
#' @export
snapshots_to_tibble <- function(snapshots) {
  if (!is.null(snapshots$schema_version)) snapshots <- list(snapshots)
  rows <- lapply(snapshots, function(s) {
    flat <- unlist(s)
    names(flat) <- gsub(".", "_", names(flat), fixed = TRUE)
    as.list(flat)
  })
  out <- dplyr::bind_rows(rows)
  num <- vapply(out, function(col) all(is.na(col) | !is.na(suppressWarnings(as.numeric(col)))), logical(1))
  keep_chr <- c("record_id", "name", "level", "window_start", "window_end",
                "window_label", "built_at", "mksap_export_date")
  num[names(num) %in% keep_chr] <- FALSE
  out[num] <- lapply(out[num], as.numeric)
  tibble::as_tibble(out)
}
