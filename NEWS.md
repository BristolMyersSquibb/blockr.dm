# blockr.dm 0.0.104

* None in a split's Columns list works again: it empties the list, which
  turns amber and says "Pick at least one column". The split keeps its
  columns until one is picked, so a few columns can be picked afresh instead
  of unticking the rest one by one.

* The crossfilter's gear has a "Group by offers (none)" switch, the new
  `group_none` argument, off by default. On, the `Group by` select lists
  "(none)" last, which unpins the group.

# blockr.dm 0.0.103

* The crossfilter's group columns and pool levels and the value filter's
  multi values use blockr.ui's `Blockr.Select` in place of `Blockr.Pick`,
  the copy that was on trial in blockr.dplyr. They look and behave the
  same.

# blockr.dm 0.0.102

* The crossfilter's categorical cards are drawn as the multi-select's list:
  a tick on the left, the count and a thin bar on each row, ticked values
  under "Selected" at the top. Above eight values the card has a filter
  box, the count and All / None, and the list scrolls. Levels the other
  filters leave no rows for stay in the list at 0. Numeric columns of ten
  or fewer values keep their value order. Categorical cards lose their
  reset button.
* "Reset all" reads the row count, then an x.
* Range cards keep a filter as the user set it while other filters change
  the card's scale, so range filters no longer drift. With one value or no
  rows in reach a card shows the column's whole range. Columns with
  missing values get the right scale. A handle stops at the other one.

# blockr.dm 0.0.101

* `trail_key()`, `trail_expr()` and `provenance()` are exported again, for
  boards with blocks written against blockr.dm 0.0.95. They are deprecated
  and will stop being exported: each logs a warning through
  `blockr.core::log_warn()` the first time it is called in a session.
  `trail_expr()` takes the 0.0.95 arguments (`table`, `data`, `count`).
  `dataset_name()` and `filter_trail_counts()` stay removed.
