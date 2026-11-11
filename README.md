# reviewkit

Shared data snapshot and readiness gate for the semi-annual resident review
(self-eval → coach → second review → CCC).

The snapshot is built once, when the resident submits the self-evaluation,
and stored with it. Every later step reads the same numbers, so the coach,
second reviewer and CCC see what was true at submission. Apps can show live
numbers beside it.

```r
inputs <- fetch_review_inputs(Sys.getenv("RDM_TOKEN"), Sys.getenv("REDCAP_URL"))
w      <- review_window()                       # duty/conference from Oct 1 (AY 2026-27) or Jul 1; evaluations from Jul 1
snap   <- build_review_snapshot("142", inputs$forms, inputs$activity, w)
gate   <- evaluate_gate(snap)                    # strict: duty + conference
gate$pass; gate$duty$message; gate$nudges

json <- snapshot_to_json(snap)                   # stored on s_eval at submission
snapshot_from_json(json)
```

Apps that already load RDM (`gmed::load_rdm_complete(raw_or_label = "raw")`)
and the activity table (`amiontools::build_resident_activity()`) pass those
in instead of calling `fetch_review_inputs()`. `build_review_snapshot()`
fetches nothing.

## The window

`review_window(as_of)` ends the day before `as_of` and has two starts:

- `start`, for duty hours and conference logging (the gate): Jul 1 of the
  academic year. AY 2026-27 is the exception: the gate began mid-year, so
  it counts from Oct 1, 2026 (`FIRST_GATE_START`).
- `ay_start`, for evaluations (peer reviews, assessments, faculty
  evaluations written): always Jul 1 of the academic year.

Pass `lookback_start` to override `start`.

## The gate

Strict (blocks submission):

- **Duty hours:** every scheduled day in the window is confirmed or entered
  by the resident. None left on the Amion default, none needing entry.
- **Conference:** every expected conference day (Amion: SLUH or VA) has an
  entry, either attended or "did not attend" with a reason.

A resident with no scheduled days in the window fails both checks: with
nothing to verify, the gate would otherwise pass.

Nudges (shown with counts, never blocking): MKSAP questions completed (latest
import), peer reviews completed, faculty evaluations submitted.

`gate_rules()` takes tolerances so the rule can be loosened without code
changes.

## Snapshot fields (schema version 2)

| Section | Fields |
|---|---|
| top level | `schema_version`, `record_id`, `name`, `level`, `window` (`start`, `ay_start`, `end`, `label`), `built_at` |
| `duty` | `days_scheduled`, `days_verified`, `days_amion_default`, `days_needs_entry`, `days_off`, `pct_verified`, `total_hours`, `weeks`, `avg_weekly_hours`, `max_week_hours`, `weeks_flag_80h` |
| `conference` | `expected_days`, `expected_sluh`, `expected_va`, `attended_days`, `not_attended_days`, `unlogged_days`, `pct_logged`, `pct_attended`, `afternoon_school_days` |
| `mksap` | `export_date`, `total_questions`, `n_complete`, `pct_complete`, `pct_correct` (latest import on or before the window end) |
| `peer` | `completed_by_resident`, `received`, `received_item_means` (`item1`-`item5`, 9 = not observed excluded) |
| `assessments` | `received`, `with_plus_delta` (faculty assessments of the resident) |
| top level | `faculty_evals_submitted` (evaluations the resident wrote) |
| `ite` | `pgy`, `total_percentile` (latest year with a score) |

## Shared panels (0.3.0)

Display-only Shiny modules, so coach.dash, second review and the CCC show
the same thing. They use Bootstrap classes only and pick up each app's
theme. Apps load the data and own every REDCap write.

- `mod_snapshot_panel_ui/server(id, snapshot_r, live_r, override_r, live_status_r)`:
  the stored snapshot, gate result, any override, and optionally today's
  numbers beside it. Returns `compare`, the "Compare with today" click;
  the app builds the live snapshot (about 30 s, so off the main process).
- `snapshot_rows(snapshot)`: the same rows as a tibble.
- `build_resident_thread(forms, record_id)`: one dated thread from
  `coach_interim`, `ccc_review` and each `coach_rev` summary, newest first.
- `mod_thread_ui/server(id, thread_r)`: shows it.

Milestones are not in the snapshot: the coach, second reviewer and CCC
change them during the review, so they are read live.

Counting rules come from amiontools; this package only counts inside the
window. `avg_weekly_hours` and the 80-hour flag follow
`amiontools::summarise_activity()` over the window's weeks, so partial
first and last weeks count as weeks.
