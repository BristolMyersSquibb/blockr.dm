# blockr.dm Development Ideas
#
# This file documents potential features and improvements for blockr.dm
# based on functionality available in the dm package.

# =============================================================================
# CURRENT STATE
# =============================================================================

# blockr.dm currently exposes these dm features:
# - new_dm_block()          -> dm::dm() + auto-inference
# - new_dm_block2()         -> dm::dm() with relationship config UI
# - new_dm_add_keys_block() -> dm_add_pk() + dm_add_fk()
# - new_dm_filter_block()   -> dm_filter() (cascading)
# - new_dm_flatten_block()  -> dm_flatten_to_tbl()
# - new_dm_pluck_block()    -> pull_tbl()

# =============================================================================
# IDEA 1: Data Quality / Validation Block
# =============================================================================

# dm has dm_examine_constraints() which checks if all PK/FK constraints are
# satisfied. This would be great for data QC.
#
# Implementation:
# - new_dm_examine_block() -> dm_examine_constraints()
# - Output: table showing which constraints pass/fail
# - Use case: "Are all USUBJIDs in ADAE also in ADSL?"
#
# Example:
# dm_nycflights13() |>
#   dm_examine_constraints()

# =============================================================================
# IDEA 2: Database Integration Block
# =============================================================================

# dm can connect to databases and auto-discover schema:
# - dm_from_con() - load dm from database connection
# - dm_sql() - generate DDL statements
#
# Implementation:
# - new_dm_from_db_block() - connect to Postgres/SQLite/etc
# - Autodiscover tables and relationships from DB metadata
# - Use case: Connect to clinical data warehouse

# =============================================================================
# IDEA 3: Alternative Join Blocks
# =============================================================================

# dm_join_to_tbl() is more flexible than flatten:
# - Join specific tables, not just flatten all
# - More control over which relationships to follow
#
# dm_squash_to_tbl() squashes with column selection
#
# Implementation:
# - new_dm_join_block() - select which tables to join
# - Use case: Join ADAE with ADSL only, not ADLB

# =============================================================================
# IDEA 4: Nested Data Blocks
# =============================================================================

# dm has powerful nested data features:
# - dm_nest_tbl() - nest child tables into parent
# - dm_pack_tbl() - pack related tables into list-columns
# - dm_unnest_tbl() / dm_unpack_tbl() - reverse operations
#
# Implementation:
# - new_dm_nest_block() - nest children into parent
# - Use case: Nest all AEs inside each subject for hierarchical views
#
# Example:
# dm_nycflights13() |>
#   dm_nest_tbl(flights, .into = airlines)

# =============================================================================
# IDEA 5: Zooming Block
# =============================================================================

# dm_zoom_to() allows dplyr operations on a single table within dm context:
# - Zoom into one table
# - Apply dplyr verbs (mutate, filter, select, etc.)
# - Update back to dm with dm_update_zoomed()
#
# Implementation:
# - new_dm_zoom_block() - zoom into table, show dplyr interface
# - Use case: Add calculated columns to one table while preserving relationships

# =============================================================================
# IDEA 6: Enhanced Diagram with Interactive Relationship Editing
# =============================================================================

# PARKED - Too complex for now
#
# dm has dm_gui() with an interactive SVG diagram:
# - dmSVG widget - click tables to select
# - Click edges to select relationships
# - Conditional panels for 1 vs 2 table selection
#
# This is overly complex. The current dm_block2 approach of adding
# one relationship at a time through simple dropdowns is simpler,
# more maintainable, and follows blockr's style better.
#
# If we revisit this later, the dm_gui code is in:
# - dm/R/gui-dmSVG.R - the widget
# - dm/R/gui-gui_ui.R - the UI
# - dm/R/gui-gui_server.R - the server logic

# =============================================================================
# IDEA 7: Cardinality Analysis Block
# =============================================================================

# dm_examine_cardinalities() shows relationship types:
# - 1:1, 1:n, n:m relationships
# - Useful for understanding data structure
#
# Implementation:
# - new_dm_cardinality_block() - visualize relationship types
# - Use case: "Is USUBJID truly 1:1 between ADSL and ADAE?"

# =============================================================================
# IDEA 8: Code Generation / Export Block
# =============================================================================

# dm has code generation features:
# - dm_paste() - generate R code to recreate dm
# - dm_deconstruct() - show how to recreate programmatically
#
# Implementation:
# - new_dm_export_block() - export dm definition as code
# - Use case: Save workflow as reproducible R script

# =============================================================================
# IDEA 9: Table Description Block
# =============================================================================

# dm supports table descriptions:
# - dm_set_table_description() - add descriptions to tables
# - dm_get_table_description() - retrieve descriptions
#
# Implementation:
# - Add description field to dm_block UI
# - Show descriptions in diagram tooltips
# - Use case: Document what each table contains

# =============================================================================
# IDEA 10: Color Coding Block
# =============================================================================

# dm_set_colors() allows coloring tables in diagrams:
# - Color by category (e.g., ADaM domains)
# - Visual organization
#
# Implementation:
# - Add color picker to dm_block2 UI
# - Use case: Color ADSL green, ADAE red, ADLB blue

# =============================================================================
# PRIORITY RANKING
# =============================================================================

# High priority (most useful for pharma):
# 1. Data Quality Block (dm_examine_constraints)
# 2. Nested Data Block (dm_nest_tbl)

# Medium priority:
# 3. Database Integration (dm_from_con)
# 4. Cardinality Analysis (dm_examine_cardinalities)
# 5. Alternative Join Block (dm_join_to_tbl)

# Lower priority:
# 6. Zooming Block (dm_zoom_to)
# 8. Table Descriptions
# 9. Color Coding

# PARKED:
# - Enhanced Diagram with Interactive Editing (dm_gui) - too complex
# - Code Generation (dm_paste)
