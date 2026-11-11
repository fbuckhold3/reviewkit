# =============================================================================
# Thread display: build_resident_thread() as a dated list, newest first.
# Display only; the app builds the thread and owns any "add note" button.
# =============================================================================

.SOURCE_BADGE <- c(coach_interim = "bg-primary", ccc_review = "bg-danger",
                   coach_rev = "bg-secondary")
.SOURCE_LABEL <- c(coach_interim = "Coach", ccc_review = "CCC",
                   coach_rev = "Review")

.thread_entry <- function(row) {
  date <- if (is.na(row$date)) "No date" else trimws(format(row$date, "%b %e, %Y"))
  text <- row$text[[1]]; tags <- row$tags[[1]]
  shiny::div(
    class = "border-start border-3 ps-3 mb-3",
    shiny::div(
      class = "d-flex flex-wrap align-items-baseline gap-2",
      shiny::span(class = paste("badge", .SOURCE_BADGE[[row$source]]), .SOURCE_LABEL[[row$source]]),
      shiny::tags$strong(row$title),
      shiny::span(class = "text-muted small",
                  paste(c(date, if (!is.na(row$author) && !row$author %in% c("CCC", "Coach")) row$author),
                        collapse = " · "))
    ),
    if (length(tags)) shiny::div(class = "mt-1",
      lapply(tags, function(t) shiny::span(
        class = paste("badge me-1", if (t %in% c("Concern", "Flagged for CCC")) "bg-warning text-dark" else "bg-light text-dark border"),
        t))),
    lapply(names(text), function(nm) shiny::div(class = "mt-1",
      if (length(text) > 1 || !nm %in% c("Notes", "Summary")) shiny::tags$em(paste0(nm, ": ")),
      shiny::span(style = "white-space: pre-wrap;", text[[nm]]))),
    if (!is.na(row$followup)) shiny::div(class = "text-muted small mt-1",
      paste0("Follow up by ", trimws(format(row$followup, "%b %e, %Y"))))
  )
}

#' Thread UI.
#' @param id Module id.
#' @export
mod_thread_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("thread"))
}

#' Thread server.
#'
#' @param id Module id.
#' @param thread_r Reactive returning `build_resident_thread()`, or `NULL`
#'   while loading.
#' @param empty_text Shown when the thread has no entries.
#' @export
mod_thread_server <- function(id, thread_r,
                              empty_text = "No interim notes or CCC reviews yet.") {
  shiny::moduleServer(id, function(input, output, session) {
    output$thread <- shiny::renderUI({
      th <- thread_r()
      if (is.null(th)) return(shiny::p(class = "text-muted mb-0", "Loading…"))
      if (nrow(th) == 0) return(shiny::p(class = "text-muted mb-0", empty_text))
      shiny::tagList(lapply(seq_len(nrow(th)), function(i) .thread_entry(th[i, ])))
    })
  })
}
