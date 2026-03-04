# Crossfilter special values demo
#
# Tests handling of "", " ", NA, NaN, Inf, -Inf in the crossfilter.
# - Character columns: "", " ", NA should all be filterable
# - Numeric columns: NA, NaN, Inf, -Inf should not break the range slider
pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.dag")

library(dm)

patients <- data.frame(
  USUBJID = paste0("SUBJ-", 1:10),
  SEX = c("M", "F", "", " ", NA, "M", "F", "", NA, "M"),
  AGE = c(25, 30, NA, NaN, Inf, -Inf, 45, 50, 55, 60),
  stringsAsFactors = FALSE
)

events <- data.frame(
  USUBJID = rep(paste0("SUBJ-", 1:10), each = 2),
  AEDECOD = c("Headache", "", NA, "Nausea", " ", "Headache", "", NA, "Rash", "Nausea",
               "Headache", "", NA, "Nausea", " ", "Headache", "", NA, "Rash", "Nausea"),
  AESEV = c("MILD", "MODERATE", "SEVERE", "", NA, "MILD", " ", "MODERATE", "SEVERE", "MILD",
            "MODERATE", "SEVERE", "MILD", "", NA, "MILD", " ", "MODERATE", "SEVERE", "MILD"),
  VALUE = c(1.2, 3.5, NA, 7.8, NaN, 12.1, Inf, 0.3, -Inf, 5.5,
            2.4, NA, 9.1, 4.6, NaN, 11.0, 6.7, Inf, 8.3, -Inf),
  stringsAsFactors = FALSE
)

test_dm <- dm(patients = patients, events = events) |>
  dm_add_pk(patients, USUBJID) |>
  dm_add_fk(events, USUBJID, patients)

board <- new_dock_board(
  blocks = c(
    data = new_static_block(data = test_dm),
    crossfilter = new_dm_crossfilter_block()
  ),
  links = c(
    new_link("data", "crossfilter", "data")
  ),
  extensions = list(
    blockr.dag::new_dag_extension()
  )
)

serve(board)
