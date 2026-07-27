# blockr.dm:::encode_crossfilter_payload(): dictionary encoding of the lookup payload.
#
# The client (crossfilter-block.js) keeps the integer codes in its rows and
# decodes to label strings only at its edges, so everything asserted here is
# load-bearing for the JS side: global per-column codes (sibling-key sets are
# intersected across lookups), sentinels as ordinary levels (they round-trip
# to R as strings), na = "null" for numerics (filterRange breaks on "NA"
# strings), and arrays that stay arrays.

decode_lookup <- function(lookup_json, levels_json) {
  cols <- jsonlite::fromJSON(as.character(lookup_json))
  lvls <- jsonlite::fromJSON(as.character(levels_json))
  for (cn in names(cols)) {
    if (cn %in% names(lvls)) {
      cols[[cn]] <- lvls[[cn]][cols[[cn]] + 1L]
    }
  }
  cols
}

sentinelize <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- "__NA__"
  x[x == ""] <- "__EMPTY__"
  x
}

make_star_dm <- function() {
  adsl <- data.frame(
    USUBJID = c("S1", "S2", "S3"),
    ARM = c("A", NA, ""),
    SEX = c("F", "M", "F"),
    AGE = c(50, NA, 60),
    stringsAsFactors = FALSE
  )
  adae <- data.frame(
    USUBJID = c("S1", "S1", "S2"),
    AESEV = factor(c("MILD", "SEVERE", "MILD")),
    AVAL = c(1.5, NA, 2),
    stringsAsFactors = FALSE
  )
  dm_obj <- dm::dm(adsl = adsl, adae = adae)
  dm_obj <- dm::dm_add_pk(dm_obj, adsl, USUBJID)
  dm::dm_add_fk(dm_obj, adae, USUBJID, adsl)
}

star_lookups <- function() {
  dm_obj <- make_star_dm()
  blockr.dm:::build_crossfilter_lookups(
    tables = lapply(dm::dm_get_tables(dm_obj), as.data.frame),
    active_dims = list(adsl = c("ARM", "SEX"), adae = "AESEV"),
    pks = dm::dm_get_all_pks(dm_obj),
    fks = dm::dm_get_all_fks(dm_obj)
  )
}

test_that("encode round-trips characters and factors through codes + levels", {
  lookups <- list(
    t1 = data.frame(
      chr = c("b", "a", "b", NA, ""),
      fct = factor(c("x", "y", "x", "y", "x")),
      num = c(1.5, NA, 3, 4, 5),
      int = c(1L, 2L, NA, 4L, 5L),
      lgl = c(TRUE, FALSE, NA, TRUE, FALSE),
      stringsAsFactors = FALSE
    )
  )
  enc <- blockr.dm:::encode_crossfilter_payload(lookups)
  dec <- decode_lookup(enc$lookups$t1, enc$levels)

  expect_identical(dec$chr, sentinelize(lookups$t1$chr))
  expect_identical(dec$fct, as.character(lookups$t1$fct))
  # Non-character columns pass through untouched (null -> NA on re-parse)
  expect_identical(dec$num, lookups$t1$num)
  expect_identical(dec$int, lookups$t1$int)
  expect_identical(dec$lgl, lookups$t1$lgl)

  # Encoded columns are pure int codes on the wire: no NA/null anywhere
  raw_cols <- jsonlite::fromJSON(as.character(enc$lookups$t1))
  expect_true(is.integer(raw_cols$chr) || is.numeric(raw_cols$chr))
  expect_false(anyNA(raw_cols$chr))
  expect_false(anyNA(raw_cols$fct))
})

test_that("sentinels are ordinary levels, distinct from literal strings", {
  lookups <- list(
    t1 = data.frame(
      v = c("NA", NA, "", "x"),
      stringsAsFactors = FALSE
    )
  )
  enc <- blockr.dm:::encode_crossfilter_payload(lookups)
  lvls <- jsonlite::fromJSON(as.character(enc$levels))

  # Real NA and the literal string "NA" stay distinguishable
  expect_setequal(lvls$v, c("NA", "__NA__", "__EMPTY__", "x"))
  dec <- decode_lookup(enc$lookups$t1, enc$levels)
  expect_identical(dec$v, c("NA", "__NA__", "__EMPTY__", "x"))
})

test_that("numeric NA serializes as JSON null, never the string NA", {
  lookups <- list(t1 = data.frame(num = c(1, NA, 3)))
  enc <- blockr.dm:::encode_crossfilter_payload(lookups)
  json <- as.character(enc$lookups$t1)
  expect_match(json, "null", fixed = TRUE)
  expect_no_match(json, '"NA"', fixed = TRUE)
})

test_that("single-level and single-row columns stay JSON arrays", {
  lookups <- list(t1 = data.frame(v = "only", n = 1))
  enc <- blockr.dm:::encode_crossfilter_payload(lookups)
  # Column arrays must not collapse to scalars (JS iterates them)
  expect_match(as.character(enc$lookups$t1), '"v":[0]', fixed = TRUE)
  expect_match(as.character(enc$lookups$t1), '"n":[1]', fixed = TRUE)
  expect_match(as.character(enc$levels), '"v":["only"]', fixed = TRUE)
})

test_that("unused factor levels are dropped from the shipped levels", {
  lookups <- list(
    t1 = data.frame(v = factor("a", levels = c("a", "unused")))
  )
  enc <- blockr.dm:::encode_crossfilter_payload(lookups)
  lvls <- jsonlite::fromJSON(as.character(enc$levels))
  expect_identical(lvls$v, "a")
})

test_that("levels is an empty JSON object when nothing is encoded", {
  lookups <- list(t1 = data.frame(num = c(1, 2), lgl = c(TRUE, FALSE)))
  enc <- blockr.dm:::encode_crossfilter_payload(lookups)
  expect_identical(as.character(enc$levels), "{}")
  # Lookup itself is untouched numeric/logical columnar JSON (fromJSON
  # simplifies whole numbers to integer; equal is the right comparison)
  dec <- jsonlite::fromJSON(as.character(enc$lookups$t1))
  expect_equal(dec$num, c(1, 2))
})

test_that("codes are globally consistent across lookups (star builder)", {
  info <- star_lookups()
  # Parent dims + the key are joined into the child lookup AND emitted as a
  # parent-keyed lookup: same value must get the same code in both.
  expect_setequal(names(info$lookups), c("adae", "adsl"))
  enc <- blockr.dm:::encode_crossfilter_payload(info$lookups)
  adae <- jsonlite::fromJSON(as.character(enc$lookups$adae))
  adsl <- jsonlite::fromJSON(as.character(enc$lookups$adsl))
  lvls <- jsonlite::fromJSON(as.character(enc$levels))

  # One shared levels entry per column name
  expect_true(all(c("USUBJID", "ARM", "SEX", "AESEV") %in% names(lvls)))

  # Decode both lookups and check the FK/dim values agree row-wise with the
  # raw builder output
  dec_adae <- decode_lookup(enc$lookups$adae, enc$levels)
  dec_adsl <- decode_lookup(enc$lookups$adsl, enc$levels)
  expect_identical(dec_adae$USUBJID, sentinelize(info$lookups$adae$USUBJID))
  expect_identical(dec_adsl$USUBJID, sentinelize(info$lookups$adsl$USUBJID))

  # Same value -> same code across lookups: intersecting coded key sets
  # (what _syncSiblingKeys does) matches intersecting the raw strings
  shared_raw <- intersect(info$lookups$adae$USUBJID, info$lookups$adsl$USUBJID)
  shared_codes <- intersect(adae$USUBJID, adsl$USUBJID)
  expect_identical(
    sort(lvls$USUBJID[shared_codes + 1L]),
    sort(shared_raw)
  )
})

test_that("encoding covers the flat builder's output", {
  dm_obj <- make_star_dm()
  info <- blockr.dm:::build_lookups_flat(
    dm_obj,
    active_dims = list(adsl = c("ARM", "SEX"), adae = "AESEV")
  )
  skip_if(is.null(info), "flat builder returned NULL on this dm")
  enc <- blockr.dm:::encode_crossfilter_payload(info$lookups)
  for (nm in names(info$lookups)) {
    dec <- decode_lookup(enc$lookups[[nm]], enc$levels)
    for (cn in names(info$lookups[[nm]])) {
      col <- info$lookups[[nm]][[cn]]
      if (is.character(col) || is.factor(col)) {
        expect_identical(dec[[cn]], sentinelize(col))
      }
    }
  }
})

test_that("encoding covers the independent builder's output", {
  tables <- list(
    a = data.frame(x = c("p", NA, "q"), n = 1:3, stringsAsFactors = FALSE),
    b = data.frame(y = factor(c("u", "v", "u")))
  )
  info <- blockr.dm:::build_lookups_independent(
    tables, active_dims = list(a = "x", b = "y")
  )
  enc <- blockr.dm:::encode_crossfilter_payload(info$lookups)
  expect_identical(
    decode_lookup(enc$lookups$a, enc$levels)$x,
    sentinelize(tables$a$x)
  )
  expect_identical(
    decode_lookup(enc$lookups$b, enc$levels)$y,
    as.character(tables$b$y)
  )
})

test_that("Date columns ship as epoch-day ints, NA as null", {
  lookups <- list(t1 = data.frame(
    ADT = as.Date(c("2020-01-01", NA, "2022-01-01")),
    n = 1:3
  ))
  enc <- blockr.dm:::encode_crossfilter_payload(lookups)
  json <- as.character(enc$lookups$t1)
  # Numbers, not ISO strings; NA is a JSON null (toEpochDay -> NaN -> dropped
  # from crossfilter's sorted index)
  expect_match(json, sprintf('"ADT":[%d,null,%d]',
                             as.integer(as.Date("2020-01-01")),
                             as.integer(as.Date("2022-01-01"))),
               fixed = TRUE)
  # Dates are not dictionary-encoded
  expect_identical(as.character(enc$levels), "{}")
})

test_that("POSIXct columns pass through unconverted (fractional-day trap)", {
  lookups <- list(t1 = data.frame(
    ts = as.POSIXct("2020-01-01 12:00:00", tz = "UTC")
  ))
  enc <- blockr.dm:::encode_crossfilter_payload(lookups)
  # Stays a string on the wire; the client's toEpochDay parses it and keeps
  # the fractional day (no flooring on either side)
  expect_match(as.character(enc$lookups$t1), '"ts":\\["2020-01-01')
})

test_that("gzip wire compression round-trips and stays single-line", {
  info <- star_lookups()
  enc <- blockr.dm:::encode_crossfilter_payload(info$lookups)
  gz <- blockr.dm:::compress_crossfilter_lookups(enc$lookups)
  expect_named(gz, names(enc$lookups))
  for (nm in names(gz)) {
    # atob() in the browser rejects newlines
    expect_false(grepl("[\r\n]", gz[[nm]]))
    # base64 -> gzip -> the exact JSON that went in (what the client's
    # DecompressionStream reproduces)
    round <- rawToChar(memDecompress(
      jsonlite::base64_dec(gz[[nm]]), type = "gzip"
    ))
    expect_identical(round, as.character(enc$lookups[[nm]]))
  }
})

test_that("encoding identical input twice is byte-identical", {
  info <- star_lookups()
  enc1 <- blockr.dm:::encode_crossfilter_payload(info$lookups)
  enc2 <- blockr.dm:::encode_crossfilter_payload(info$lookups)
  expect_identical(
    vapply(enc1$lookups, as.character, character(1)),
    vapply(enc2$lookups, as.character, character(1))
  )
  expect_identical(as.character(enc1$levels), as.character(enc2$levels))
})
