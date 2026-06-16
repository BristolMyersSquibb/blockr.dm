# Behavioural lock for the Key-lines data-model viewer (R/dm-keylines.R).
#
# These tests pin the *derivation* (FK edges -> key lines / self-refs / depth /
# table order) and the *rendered structure* (element classes, data-attributes,
# HTML escaping) so the render internals can be refactored — e.g. from
# hand-assembled strings to htmltools tag builders — without behaviour drift.
# Assertions are deliberately structural (counts, classes, attrs, escaped text)
# rather than byte-for-byte, so they survive a faithful markup rewrite.

library(dm)

# --- fixtures ----------------------------------------------------------------

# rich model: a shared parent key (did) with three members, a second key (eid)
# with two members, a self-referential FK (emp.mgr -> emp.eid) and three
# renamed FKs (child column name differs from the parent key it points at).
kl_dm_rich <- function() {
  emp  <- data.frame(eid = 1:4, mgr = c(NA, 1, 1, 2), dept = c(1, 1, 2, 2))
  dept <- data.frame(did = 1:2, dname = c("X", "Y"))
  attr(dept, "label") <- "Dept <b>&</b> co"   # HTML specials -> escape probe
  proj <- data.frame(pid = 1:3, lead_eid = c(1, 2, 3), dept = c(1, 2, 2))
  d <- dm(emp = emp, dept = dept, proj = proj)
  d <- dm_add_pk(d, emp, eid)
  d <- dm_add_pk(d, dept, did)
  d <- dm_add_fk(d, emp, mgr, emp)            # self-ref
  d <- dm_add_fk(d, emp, dept, dept, did)     # renamed: emp.dept -> dept.did
  d <- dm_add_fk(d, proj, dept, dept, did)    # renamed: proj.dept -> dept.did
  d <- dm_add_fk(d, proj, lead_eid, emp, eid) # renamed: proj.lead_eid -> emp.eid
  d
}

# composite parent key (region + code), one referencing child
kl_dm_composite <- function() {
  ord  <- data.frame(region = c(1, 1, 2), code = c("a", "b", "a"), q = 1:3)
  item <- data.frame(region = c(1, 2), code = c("a", "a"), n = 5:6)
  d <- dm(ord = ord, item = item)
  d <- dm_add_pk(d, ord, c(region, code))
  d <- dm_add_fk(d, item, c(region, code), ord)
  d
}

# two FKs from the same child to the same parent key -> "xN" multiplicity badge
kl_dm_multi <- function() {
  usr  <- data.frame(uid = 1:3)
  msg  <- data.frame(mid = 1:2, sender = c(1, 2), recipient = c(2, 3))
  d <- dm(usr = usr, msg = msg)
  d <- dm_add_pk(d, usr, uid)
  d <- dm_add_fk(d, msg, sender, usr, uid)
  d <- dm_add_fk(d, msg, recipient, usr, uid)
  d
}

# no foreign keys at all
kl_dm_nolines <- function() {
  dm(a = data.frame(x = 1:2), b = data.frame(y = 1:2))
}

# parse helper
kl_doc <- function(meta, root = "ROOT") {
  rvest::read_html(as.character(dm_keylines_html(meta, root)))
}

# --- derivation: edges -------------------------------------------------------

test_that("dm_keylines_edges reads every FK edge with column tuples", {
  edges <- dm_keylines_edges(kl_dm_rich())
  expect_length(edges, 4)
  for (e in edges) {
    expect_named(e, c("child", "child_cols", "parent", "parent_cols"))
    expect_type(e$child_cols, "character")
    expect_type(e$parent_cols, "character")
  }
  # the renamed proj.lead_eid -> emp.eid edge is present with differing cols
  ren <- Filter(function(e) e$child == "proj" && identical(e$child_cols, "lead_eid"), edges)
  expect_length(ren, 1)
  expect_identical(ren[[1]]$parent, "emp")
  expect_identical(ren[[1]]$parent_cols, "eid")
})

test_that("dm_keylines_edges returns empty list when there are no FKs", {
  expect_identical(dm_keylines_edges(kl_dm_nolines()), list())
})

# --- derivation: lines -------------------------------------------------------

test_that("dm_keylines_lines groups edges by parent key and drops singletons", {
  lines <- dm_keylines_lines(dm_keylines_edges(kl_dm_rich()))
  # self-ref is NOT a line; two real keys remain (did, eid)
  expect_length(lines, 2)
  names <- vapply(lines, function(L) L$name, character(1))
  expect_setequal(names, c("did", "eid"))

  by_name <- stats::setNames(lines, names)
  expect_identical(by_name[["did"]]$parent, "dept")
  expect_setequal(by_name[["did"]]$members, c("dept", "emp", "proj"))
  expect_identical(by_name[["eid"]]$parent, "emp")
  expect_setequal(by_name[["eid"]]$members, c("emp", "proj"))
})

test_that("dm_keylines_lines orders by descending member count and assigns palette + lid", {
  lines <- dm_keylines_lines(dm_keylines_edges(kl_dm_rich()))
  pal <- dm_keylines_palette()
  # widest key (did, 3 members) is first -> L0 + first palette colour
  expect_identical(lines[[1]]$name, "did")
  expect_identical(lines[[1]]$lid, "L0")
  expect_identical(lines[[1]]$color, pal[[1]])
  expect_identical(lines[[2]]$lid, "L1")
  expect_identical(lines[[2]]$color, pal[[2]])
})

test_that("dm_keylines_lines flags composite keys", {
  lines <- dm_keylines_lines(dm_keylines_edges(kl_dm_composite()))
  expect_length(lines, 1)
  expect_true(lines[[1]]$composite)
  expect_identical(lines[[1]]$name, "region + code")
})

# --- derivation: self-refs, depth --------------------------------------------

test_that("dm_keylines_selfrefs surfaces self-referential FKs only", {
  sr <- dm_keylines_selfrefs(dm_keylines_edges(kl_dm_rich()))
  expect_named(sr, "emp")
  expect_length(sr[["emp"]], 1)
  expect_identical(sr[["emp"]][[1]]$child_cols, "mgr")
  expect_identical(sr[["emp"]][[1]]$parent_cols, "eid")
})

test_that("dm_keylines_depths ranks tables by FK hierarchy (parents above children)", {
  d <- kl_dm_rich()
  edges <- dm_keylines_edges(d)
  depth <- dm_keylines_depths(names(dm::dm_get_tables(d)), edges)
  expect_equal(depth[["dept"]], 0L) # root key owner
  expect_equal(depth[["emp"]], 1L)  # references dept
  expect_equal(depth[["proj"]], 2L) # references emp (which references dept)
})

# --- derivation: meta --------------------------------------------------------

test_that("dm_keylines_meta orders rows + lanes by depth, widest key leftmost", {
  meta <- dm_keylines_meta(kl_dm_rich())
  expect_identical(meta$order, c("dept", "emp", "proj"))
  # lane order: depth of owner asc, then member count desc
  expect_identical(unname(vapply(meta$lane_order, function(L) L$name, character(1))),
                   c("did", "eid"))
})

test_that("dm_keylines_meta handles a model with no foreign keys", {
  meta <- dm_keylines_meta(kl_dm_nolines())
  expect_length(meta$lines, 0)
  expect_setequal(meta$order, c("a", "b"))
  expect_length(meta$self_refs, 0)
})

# --- rendering: structure ----------------------------------------------------

test_that("dm_keylines_html renders one root, one row per table, one cap per line", {
  meta <- dm_keylines_meta(kl_dm_rich())
  doc <- kl_doc(meta)

  root <- rvest::html_element(doc, "#ROOT")
  expect_false(is.na(root))
  expect_true(grepl("dmv-rails2", rvest::html_attr(root, "class")))

  expect_length(rvest::html_elements(doc, ".r2row"), 3)   # tables
  expect_length(rvest::html_elements(doc, ".r2key"), 2)   # lines

  # every row carries its table id; the set matches the tables
  ids <- rvest::html_attr(rvest::html_elements(doc, ".r2row"), "data-table")
  expect_setequal(ids, c("dept", "emp", "proj"))
})

test_that("dm_keylines_html draws a node per line membership, owner nodes filled", {
  meta <- dm_keylines_meta(kl_dm_rich())
  doc <- kl_doc(meta)
  # members: did{dept,emp,proj}=3 + eid{emp,proj}=2 = 5 nodes
  expect_length(rvest::html_elements(doc, ".r2node"), 5)
  # one filled (owner / PK) node per line
  expect_length(rvest::html_elements(doc, ".r2node--src"), 2)
  # routed (linked) layout draws at least one interchange jog
  expect_gte(length(rvest::html_elements(doc, ".r2jog")), 1)
  # one coloured spine per line
  expect_length(rvest::html_elements(doc, ".r2line"), 2)
})

test_that("dm_keylines_html node tooltips carry PK/FK meaning", {
  meta <- dm_keylines_meta(kl_dm_rich())
  doc <- kl_doc(meta)
  tips <- rvest::html_text(rvest::html_elements(doc, ".r2node title"))
  expect_true(any(grepl("primary key", tips)))
  expect_true(any(grepl("foreign key", tips)))
})

test_that("dm_keylines_html tags self-refs and renamed FKs, hides plain ones", {
  meta <- dm_keylines_meta(kl_dm_rich())
  doc <- kl_doc(meta)
  expect_length(rvest::html_elements(doc, ".r2tag--self"), 1) # emp.mgr -> emp.eid
  # three renamed FKs get a mapping tag (emp.dept, proj.dept, proj.lead_eid)
  expect_length(rvest::html_elements(doc, ".r2tag--map"), 3)
})

test_that("dm_keylines_html escapes user-supplied text (labels)", {
  meta <- dm_keylines_meta(kl_dm_rich())
  html <- as.character(dm_keylines_html(meta, "ROOT"))
  expect_false(grepl("Dept <b>", html, fixed = TRUE)) # no raw markup leak
  expect_true(grepl("&lt;b&gt;", html))               # escaped instead
  # parsed text round-trips to the original
  doc <- rvest::read_html(html)
  descs <- rvest::html_text(rvest::html_elements(doc, ".r2row__desc"))
  expect_true(any(descs == "Dept <b>&</b> co"))
})

test_that("dm_keylines_html marks composite key caps", {
  meta <- dm_keylines_meta(kl_dm_composite())
  doc <- kl_doc(meta)
  expect_length(rvest::html_elements(doc, ".r2key--comp"), 1)
})

test_that("dm_keylines_html badges multiplicity when a child has >1 FK to a key", {
  meta <- dm_keylines_meta(kl_dm_multi())
  doc <- kl_doc(meta)
  expect_length(rvest::html_elements(doc, ".r2node--multi"), 1)
  expect_equal(rvest::html_text(rvest::html_element(doc, ".r2nodect")), "2")
})

test_that("dm_keylines_html shows the no-FK banner and no lines", {
  meta <- dm_keylines_meta(kl_dm_nolines())
  doc <- kl_doc(meta)
  expect_length(rvest::html_elements(doc, ".r2key"), 0)
  expect_length(rvest::html_elements(doc, ".r2node"), 0)
  banner <- rvest::html_element(doc, ".rails2__banner")
  expect_false(is.na(banner))
  expect_match(rvest::html_text(banner), "No foreign keys")
  expect_true(grepl("dmv-rails2--nolines",
                    rvest::html_attr(rvest::html_element(doc, ".dmv-rails2"), "class")))
})
