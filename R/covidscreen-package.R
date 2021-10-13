#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom magrittr %>%
## usethis namespace: end
NULL

# Make generics data.table aware
.datatable.aware <- TRUE

# Get rid of `.` R CMD CHECK NOTE
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' @importFrom data.table
#'   `%between%` `%chin%` `%flike%` `%ilike%` `%like%` `%inrange%` `:=`
#'   `.SD` `.BY` `.N` `.I` `.GRP` `.NGRP` `.EACHI`
#'   data.table merge.data.table set setcolorder setkey setkeyv setindex
#'   setindexv setorder setorderv nafill setnafill fifelse CJ
#'
#'
#' @importFrom reactable
#'   reactable colDef colFormat colGroup renderReactable reactableOutput
#'
#'
#' @import shinymaterial
#'
#' @importFrom sortable
#'   rank_list add_rank_list bucket_list render_sortable sortable_output
NULL
