## BLOCKR.DM improvemnt plan 

use blue 'data-structure' category for all dm blocks

new_dm_read
- similar structure than new_read block from ../blockr.io, with both browser and server support.
- select a directory, or upload a zip filer, or excel file
- In an excel file, each sheet is a data.frame

new_dm_write
- same as new_dm_read, but for write, also look at new_write from ../blockr.io

new_dm_select
- select a subset fo tables from a dm
- UI: a single slectize
- Is there a funciton in dm? I think so

new_dm_pull
- already existts but unsure if name an implementation in line with dm or other functioins

new_dm
- make shure it can take data frames AND existing dms. If a dt already exists, it will be updated. the first element will be updated by the second. this also works with multiple dms: if tables exits in both dm, the ones in the first one will be overwritten with the second one.
- challenge: how to get the name of a datafram? how do we handle it now?


new_dm_flatten alternatives
- dm_join_to_tbl() is more flexible than flatten:
  - Join specific tables, not just flatten all
  - More control over which relationships to follow
- Use case: Join ADAE with ADSL only, not ADLB
- I wonder if in the current examples, this is what new_dm_flatten does...

dm_squash_to_tbl() 
- squashes with column selection
- what does that mean? Usecase
- could we combine these functions in a single block? This would avoid the confusion a user has when they need to deal with all of these function.


other functions?
- are we missing impotrant or useful functionality from dm? is there related functionality that is not in dm. do comprehensive web research.

dm output method
- current diagrammeR based output is ugly, the .js library that it uses it outdated.
- brainstorm about alternatives
  - re-implementation using g6r - we load the library anyway in order to run bockr.dag. 
  - Other ideas? a select box to pick an individual table and then use the df output method?
  - Do comprehensive research on how other softwware handles this.
  - Perhaps: keep current output method, unless we find a good alternative. If we really need somehting, a proper re-implementation in dm could be done later on.
