#' Fetch everything `build_review_snapshot()` reads, in one call.
#'
#' For scripts and batch jobs. Apps that have already loaded RDM (gmed) and
#' the activity table (amiontools) should pass those in directly instead of
#' calling this: it re-fetches both, and the Amion part alone takes
#' ~10-20 s.
#'
#' @param rdm_token,redcap_url RDM REDCap credentials (test or prod: the
#'   caller's choice). Use `Sys.getenv()`; never hardcode a token.
#' @param record_ids Optional character vector. Filters the activity table
#'   only; REDCap forms are always loaded whole (peer reviews a resident
#'   wrote live on other residents' records).
#' @return A list: `forms` (named list of RDM forms, raw codes, as in
#'   `gmed::load_rdm_complete()$all_forms`) and `activity`
#'   (`amiontools::build_resident_activity()`).
#' @export
fetch_review_inputs <- function(rdm_token, redcap_url, record_ids = NULL) {
  if (!nzchar(rdm_token)) stop("fetch_review_inputs: rdm_token is empty")

  rdm <- gmed::load_rdm_complete(
    rdm_token    = rdm_token,
    redcap_url   = redcap_url,
    raw_or_label = "raw",
    per_form     = TRUE,
    verbose      = FALSE
  )
  activity <- amiontools::build_resident_activity(
    rdm_token, redcap_url,
    record_ids = record_ids
  )

  list(forms = rdm$all_forms, activity = activity)
}
