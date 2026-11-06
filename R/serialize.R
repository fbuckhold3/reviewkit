#' Store a snapshot as JSON (for a REDCap notes field).
#'
#' @param snapshot A `build_review_snapshot()` result.
#' @return A single JSON string.
#' @export
snapshot_to_json <- function(snapshot) {
  as.character(jsonlite::toJSON(snapshot, auto_unbox = TRUE, null = "null",
                                na = "null", digits = NA))
}

#' Read a stored snapshot back.
#'
#' @param json A string from `snapshot_to_json()`.
#' @return The snapshot list, or `NULL` for an empty or missing value.
#'   Errors on a snapshot newer than this package can read.
#' @export
snapshot_from_json <- function(json) {
  if (is.null(json) || length(json) == 0 || is.na(json) || !nzchar(trimws(json))) return(NULL)
  s <- jsonlite::fromJSON(json, simplifyVector = TRUE)
  v <- s$schema_version
  if (is.null(v) || v > SNAPSHOT_SCHEMA_VERSION) {
    stop(sprintf("snapshot_from_json: schema version %s is newer than this reviewkit reads (%d)",
                 format(v), SNAPSHOT_SCHEMA_VERSION))
  }
  # JSON null -> NULL; restore NA so fields keep their shape.
  fix <- function(x) {
    if (is.null(x)) return(NA)
    if (is.list(x)) return(lapply(x, fix))
    x
  }
  lapply(s, fix)
}
