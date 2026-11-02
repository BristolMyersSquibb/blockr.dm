# blockr.dm 0.0.101

* `trail_key()`, `trail_expr()` and `provenance()` are exported again, for
  boards with blocks written against blockr.dm 0.0.95. They are deprecated
  and will stop being exported: each logs a warning through
  `blockr.core::log_warn()` the first time it is called in a session.
  `trail_expr()` takes the 0.0.95 arguments (`table`, `data`, `count`).
  `dataset_name()` and `filter_trail_counts()` stay removed.
