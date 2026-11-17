# =============================================================================
# Resident thread: one dated history per resident, newest first.
#
# Merges three RDM forms (plan doc, "Concerns thread"):
#   - coach_interim: coach notes outside the semi-annual review
#   - ccc_review:    CCC reviews, scheduled and interim, with concern,
#                    actions and action status
#   - coach_rev:     each coach review's takeaway (summary and/or finalized
#                    ILP) and, after a semi-annual review, the touch-base
#                    ILP check-in (source "coach_touch")
#
# The thread is not tied to a review period. coach.dash, second review and
# the CCC all read it, so they show the same entries.
#
# Forms come with raw codes (gmed::load_rdm_complete(raw_or_label = "raw"));
# period fields that an app has already turned into labels are kept as-is.
# =============================================================================

.CI_REASON <- c("1" = "Resident-initiated", "2" = "CCC follow-up",
                "3" = "Concern", "4" = "Other")

.CCC_ACTION <- c("1" = "Remediation plan", "2" = "Probation",
                 "3" = "Referral for professionalism", "4" = "Coach follow up",
                 "5" = "Meet with PD and/or CCC Chair", "6" = "Meet with Chiefs",
                 "7" = "Other (see notes)", "8" = "Referral to behavioral health")

.CCC_STATUS <- c("1" = "Initiation", "2" = "Ongoing", "3" = "Resolved",
                 "4" = "Recurring")

.CCC_COMPETENCY <- c("1" = "Patient Care", "2" = "Medical Knowledge",
                     "3" = "Systems-based Practice",
                     "4" = "Practice-based Learning and Improvement",
                     "5" = "Professionalism",
                     "6" = "Interpersonal Communication Skills",
                     "7" = "Not a competence concern")

# ccc_session and coach_period share codes 1-7; coach_period adds 8.
.PERIOD_LABEL <- c("1" = "Mid Intern", "2" = "End Intern", "3" = "Mid PGY2",
                   "4" = "End PGY2", "5" = "Mid PGY3", "6" = "Graduation",
                   "7" = "Entering Residency", "8" = "Interim")

.txt <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x) | !nzchar(trimws(x)), NA_character_, trimws(x))
}

.label <- function(x, map) {
  x <- .txt(x)
  out <- unname(map[x])
  ifelse(is.na(out), x, out)
}

# Labels of the checked boxes of one checkbox field, one string per row.
.checked <- function(df, field, map) {
  cols <- paste0(field, "___", names(map))
  vapply(seq_len(nrow(df)), function(i) {
    on <- vapply(cols, function(cl) cl %in% names(df) && identical(as.character(df[[cl]][i]), "1"),
                 logical(1))
    if (any(on)) paste(unname(map[on]), collapse = "; ") else NA_character_
  }, character(1))
}

# Labelled text sections for one row: drops the empty ones.
.sections <- function(...) {
  s <- c(...)
  s[!is.na(s)]
}

.empty_thread <- function() {
  tibble::tibble(date = as.Date(character()), source = character(),
                 title = character(), author = character(),
                 text = list(), tags = list(), followup = as.Date(character()),
                 instance = integer())
}

#' One resident's dated thread: coach interim notes, CCC reviews, coach
#' review takeaways and touch-base check-ins, newest first.
#'
#' @param forms Named list of RDM forms with raw codes, as in
#'   `gmed::load_rdm_complete(raw_or_label = "raw")$all_forms`. Uses
#'   `coach_interim`, `ccc_review` and `coach_rev` when present.
#' @param record_id RDM record_id.
#' @return A tibble, one row per entry: `date`, `source` (`coach_interim`,
#'   `ccc_review`, `coach_rev` or `coach_touch`), `title`, `author`, `text` (list of named
#'   character vectors: section label -> text), `tags` (list of character
#'   vectors, e.g. "Concern", "Flagged for CCC", actions), `followup`
#'   (Date) and `instance`. Entries with no date sort last.
#' @export
build_resident_thread <- function(forms, record_id) {
  rid <- as.character(record_id)
  if (length(rid) != 1 || is.na(rid) || !nzchar(rid)) {
    stop("build_resident_thread: record_id must be a single value")
  }
  parts <- list(.empty_thread())

  ci <- .form_rows(forms, "coach_interim", rid)
  if (!is.null(ci) && nrow(ci) > 0) {
    reason <- .label(ci$ci_reason, .CI_REASON)
    other  <- .txt(ci$ci_reason_other)
    reason <- ifelse(!is.na(other) & reason %in% "Other", paste0("Other: ", other), reason)
    parts[[length(parts) + 1]] <- tibble::tibble(
      date     = .as_date(ci$ci_date),
      source   = "coach_interim",
      title    = ifelse(is.na(reason), "Coach interim note", paste0("Coach interim note: ", reason)),
      author   = .txt(ci$ci_coach),
      text     = lapply(seq_len(nrow(ci)), function(i) .sections(Notes = .txt(ci$ci_notes[i]))),
      tags     = lapply(seq_len(nrow(ci)), function(i) {
        if (identical(as.character(ci$ci_ccc_flag[i]), "1")) "Flagged for CCC" else character()
      }),
      followup = .as_date(ci$ci_followup_date),
      instance = as.integer(ci$redcap_repeat_instance)
    )
  }

  cc <- .form_rows(forms, "ccc_review", rid)
  if (!is.null(cc) && nrow(cc) > 0) {
    interim <- as.character(cc$ccc_rev_type) %in% c("2", "Interim")
    session <- .label(cc$ccc_session, .PERIOD_LABEL)
    actions <- .checked(cc, "ccc_action", .CCC_ACTION)
    status  <- .checked(cc, "ccc_action_status", .CCC_STATUS)
    comp    <- .checked(cc, "ccc_competency", .CCC_COMPETENCY)
    parts[[length(parts) + 1]] <- tibble::tibble(
      date     = .as_date(cc$ccc_date),
      source   = "ccc_review",
      title    = ifelse(interim, "CCC interim review",
                        ifelse(is.na(session), "CCC review", paste0("CCC review: ", session))),
      author   = "CCC",
      text     = lapply(seq_len(nrow(cc)), function(i) .sections(
        `Interim note`        = .txt(cc$ccc_interim[i]),
        `Learning plan`       = .txt(cc$ccc_ilp[i]),
        `Milestone notes`     = .txt(cc$ccc_mile_notes[i]),
        `Issues to follow up` = .txt(cc$ccc_issues_follow_up[i]),
        Comments              = .txt(cc$ccc_comments[i]),
        `Follow-up by`        = .txt(cc$ccc_fu_resp[i])
      )),
      tags     = lapply(seq_len(nrow(cc)), function(i) {
        t <- c(if (identical(as.character(cc$ccc_concern[i]), "1")) "Concern",
               if (!is.na(comp[i])) paste0("Competency: ", comp[i]),
               if (!is.na(actions[i])) paste0("Actions: ", actions[i]),
               if (!is.na(status[i])) paste0("Status: ", status[i]))
        if (is.null(t)) character() else t
      }),
      followup = as.Date(NA),
      instance = as.integer(cc$redcap_repeat_instance)
    )
  }

  cr <- .form_rows(forms, "coach_rev", rid)
  if (!is.null(cr) && nrow(cr) > 0) {
    period <- .label(cr$coach_period, .PERIOD_LABEL)
    col <- function(f) if (f %in% names(cr)) .txt(cr[[f]]) else rep(NA_character_, nrow(cr))

    # The review itself: its takeaway is the finalized ILP (semi-annual and
    # graduation reviews) and/or the summary (Entering Residency, interim).
    rev_text <- lapply(seq_len(nrow(cr)), function(i) .sections(
      Summary         = col("coach_summary")[i],
      `Finalized ILP` = col("coach_ilp_final")[i]
    ))
    keep <- lengths(rev_text) > 0
    if (any(keep)) {
      parts[[length(parts) + 1]] <- tibble::tibble(
        date     = .as_date(cr$coach_date[keep]),
        source   = "coach_rev",
        title    = ifelse(is.na(period[keep]), "Coach review", paste0("Coach review: ", period[keep])),
        author   = "Coach",
        text     = rev_text[keep],
        tags     = lapply(which(keep), function(i) character()),
        followup = as.Date(NA),
        instance = as.integer(cr$redcap_repeat_instance[keep])
      )
    }

    # The touch-base after a semi-annual review: same coach_rev instance,
    # its own date, its takeaway is the ILP check-in.
    chk <- col("coach_ilp_checkin")
    keep <- !is.na(chk)
    if (any(keep)) {
      parts[[length(parts) + 1]] <- tibble::tibble(
        date     = .as_date(col("coach_touch_date")[keep]),
        source   = "coach_touch",
        title    = ifelse(is.na(period[keep]), "Coach touch-base", paste0("Coach touch-base: ", period[keep])),
        author   = "Coach",
        text     = lapply(chk[keep], function(s) c(`ILP check-in` = s)),
        tags     = lapply(which(keep), function(i) character()),
        followup = as.Date(NA),
        instance = as.integer(cr$redcap_repeat_instance[keep])
      )
    }
  }

  out <- dplyr::bind_rows(parts)
  # Same day: later instance (saved later) first.
  out[order(is.na(out$date), -as.numeric(out$date), out$source, -out$instance), , drop = FALSE]
}
